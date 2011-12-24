#!/usr/bin/perl -w
=head1 NAME

mwimport -- quick and dirty mediawiki importer

=head1 SYNOPSIS

cat pages.xml | mwimport [-s N|--skip=N]

=cut

use strict;
use Getopt::Long;
use Pod::Usage;

my ($cnt_page, $cnt_rev, %namespace, $ns_pattern);
my $committed = 0;
my $skip = 0;

## set this to 1 to match "mwdumper --format=sql:1.5" as close as possible
sub Compat() { 0 }

# 512kB is what mwdumper uses, but 4MB gives much better performance here
my $Buffer_Size = Compat ? 512*1024 : 4*1024*1024;

sub textify($)
{
  my $l;
  for ($_[0]) {
    if (defined $_) {
      s/&quot;/"/ig;
      s/&lt;/</ig;
      s/&gt;/>/ig;
      /&(?!amp;)(.*?;)/ and die "textify: does not know &$1";
      s/&amp;/&/ig;
      $l = length $_;
      s/\\/\\\\/g;
      s/\n/\\n/g;
      s/'/\\'/ig;
      Compat and s/"/\\"/ig;
      $_ = "'$_'";
    } else {
      $l = 0;
      $_ = "''";
    }
  }
  return $l;
}

sub getline()
{
  $_ = <>;
  defined $_ or die "eof at line $.\n";
}

sub ignore_elt($)
{
  m|^\s*<$_[0]>.*?</$_[0]>\n$| or die "expected $_[0] element in line $.\n";
  getline;
}

sub simple_elt($$)
{
  if (m|^\s*<$_[0]\s*/>\n$|) {
    $_[1]{$_[0]} = '';
  } elsif (m|^\s*<$_[0]>(.*?)</$_[0]>\n$|) {
    $_[1]{$_[0]} = $1;
  } else {
    die "expected $_[0] element in line $.\n";
  }
  getline;
}

sub simple_opt_elt($$)
{
  if (m|^\s*<$_[0]\s*/>\n$|) {
    $_[1]{$_[0]} = '';
  } elsif (m|^\s*<$_[0]>(.*?)</$_[0]>\n$|) {
    $_[1]{$_[0]} = $1;
  } else {
    return;
  }
  getline;
}

sub opening_tag($)
{
  m|^\s*<$_[0]>\n$| or die "expected $_[0] element in line $.\n";
  getline;
}

sub closing_tag($)
{
  m|^\s*</$_[0]>\n$| or die "$_[0]: expected closing tag in line $.\n";
  getline;
}

sub si_nss_namespace()
{
  m|^\s*<namespace key="(-?\d+)"\s*/>()\n|
    or m|^\s*<namespace key="(-?\d+)">(.*?)</namespace>\n|
    or die "expected namespace element in line $.\n";
  $namespace{$2} = $1;
  getline;
}

sub si_namespaces()
{
  opening_tag("namespaces");
  eval {
    while (1) {
      si_nss_namespace;
    }
  };
  # note: $@ is always defined
  $@ =~ /^expected namespace element / or die "namespaces: $@";
  $ns_pattern = '^('.join('|',map { quotemeta } keys %namespace).'):';
  closing_tag("namespaces");
}

sub siteinfo()
{
  opening_tag("siteinfo");
  eval {
    my %site;
    simple_elt sitename => \%site;
    simple_elt base => \%site;
    simple_elt generator => \%site;
    $site{generator} =~ /^MediaWiki 1.(9|10)alpha$/
      or warn("siteinfo: untested generator '$site{generator}',",
              " expect trouble ahead\n");
    simple_elt case => \%site;
    si_namespaces;
    print "-- MediaWiki XML dump converted to SQL by mwimport
BEGIN;

-- Site: $site{sitename}
-- URL: $site{base}
-- Generator: $site{generator}
-- Case: $site{case}
--
-- Namespaces:
",map { "-- $namespace{$_}: $_\n" }
  sort { $namespace{$a} <=> $namespace{$b} } keys %namespace;
  };
  $@ and die "siteinfo: $@";
  closing_tag("siteinfo");
}

sub pg_rv_contributor($)
{
  opening_tag "contributor";
  my %c;
  eval {
    simple_elt username => \%c;
    simple_elt id => \%c;
    $_[0]{contrib_user} = $c{username};
    $_[0]{contrib_id}   = $c{id};
  };
  if ($@) {
    $@ =~ /^expected username element / or die "contributor: $@";
    eval {
      simple_elt ip => \%c;
      $_[0]{contrib_user} = $c{ip};
    };
    $@ and die "contributor: $@";
  }
  closing_tag "contributor";
}

sub pg_rv_comment($)
{
  if (m|^\s*<comment\s*/>\s*\n|) {
    getline;
  } elsif (s|^\s*<comment>([^<]*)||g) {
    while (1) {
      $_[0]{comment} .= $1;
      last if $_;
      getline;
      s|^([^<]*)||;
    }
    closing_tag "comment";
  } else {
    return;
  }
}

sub pg_rv_text($)
{
  if (m|^\s*<text xml:space="preserve"\s*/>\s*\n|) {
    $_[0]{text} = '';
    getline;
  } elsif (s|^\s*<text xml:space="preserve">([^<]*)||g) {
    while (1) {
      $_[0]{text} .= $1;
      last if $_;
      getline;
      s|^([^<]*)||;
    }
    closing_tag "text";
  } else {
    die "expected text element in line $.\n";
  }
}

my $start = time;

sub stats()
{
  my $s = time - $start;
  $s ||= 1;
  printf STDERR "%9d pages (%7.3f/s), %9d revisions (%7.3f/s) in %d seconds\n",
    $cnt_page, $cnt_page/$s, $cnt_rev, $cnt_rev/$s, $s;
}

### flush_rev($text, $rev, $page)
sub flush_rev($$$)
{
  $_[0] or return;
  for my $i (0,1,2) {
    $_[$i] =~ s/,\n?$//;
  }
  print "INSERT INTO text(old_id,old_text,old_flags) VALUES $_[0];\n";
  $_[2] and print "INSERT INTO page(page_id,page_namespace,page_title,page_restrictions,page_counter,page_is_redirect,page_is_new,page_random,page_touched,page_latest,page_len) VALUES $_[2];\n";
  print "INSERT INTO revision(rev_id,rev_page,rev_text_id,rev_comment,rev_user,rev_user_text,rev_timestamp,rev_minor_edit,rev_deleted) VALUES $_[1];\n";
  for my $i (0,1,2) {
    $_[$i] = '';
  }
}

### flush($text, $rev, $page)
sub flush($$$)
{
  flush_rev $_[0], $_[1], $_[2];
  print "COMMIT;\n";
  $committed = $cnt_page;
}

### pg_revision(\%page, $skip, $text, $rev, $page)
sub pg_revision($$$$$)
{
  my $rev = {};
  opening_tag "revision";
  eval {
    my %revision;
    simple_elt id => $rev;
    simple_elt timestamp => $rev;
    pg_rv_contributor $rev;
    simple_opt_elt minor => $rev;
    pg_rv_comment $rev;
    pg_rv_text $rev;
  };
  $@ and die "revision: $@";
  closing_tag "revision";
  $_[1] and return;
  $$rev{id} =~ /^\d+$/ or return
    warn("page '$_[0]{title}': ignoring bogus revision id '$$rev{id}'\n");
  $_[0]{latest_len} = textify $$rev{text};
  for my $f qw(comment contrib_user) {
    textify $$rev{$f};
  }
  $$rev{timestamp} =~
    s/^(\d\d\d\d)-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)Z$/'$1$2$3$4$5$6'/
      or return warn("page '$_[0]{title}' rev $$rev{id}: ",
                     "bogus timestamp '$$rev{timestamp}'\n");
  $_[2] .= "($$rev{id},$$rev{text},'utf-8'),\n";
  $$rev{minor} = defined $$rev{minor} ? 1 : 0;
  $_[3] .= "($$rev{id},$_[0]{id},$$rev{id},$$rev{comment},"
    .($$rev{contrib_id}||0)
    .",$$rev{contrib_user},$$rev{timestamp},$$rev{minor},0),\n";
  $_[0]{latest} = $$rev{id};
  $_[0]{latest_start} = substr $$rev{text}, 0, 20;
  if (length $_[2] > $Buffer_Size) {
    flush_rev $_[2], $_[3], $_[4];
    $_[0]{do_commit} = 1;
  }
  ++$cnt_rev % 1000 == 0 and stats;
}

### page($text, $rev, $page)
sub page($$$)
{
  opening_tag "page";
  my %page;
  ++$cnt_page;
  eval {
    simple_elt title => \%page;
    simple_elt id => \%page;
    simple_opt_elt restrictions => \%page;
    while (1) {
      pg_revision \%page, $skip, $_[0], $_[1], $_[2];
    }
  };
  # note: $@ is always defined
  $@ =~ /^expected revision element / or die "page: $@";
  closing_tag "page";
  if ($skip) {
    --$skip;
  } else {
    $page{id} =~ /^\d+$/
      or warn("page '$page{title}': bogus id '$page{id}'\n");
    my $ns;
    if ($page{title} =~ s/$ns_pattern//o) {
      $ns = $namespace{$1};
    } else {
      $ns = 0;
    }
    for my $f qw(title restrictions) {
      textify $page{$f};
    }
    if (Compat) {
      $page{redirect} = $page{latest_start} =~ /^'#(?:REDIRECT|redirect) / ?
        1 : 0;
    } else {
      $page{redirect} = $page{latest_start} =~ /^'#REDIRECT /i ? 1 : 0;
    }
    $page{title} =~ y/ /_/;
    if (Compat) {
      $_[2] .= "($page{id},$ns,$page{title},$page{restrictions},0,"
        ."$page{redirect},0,RAND(),"
          ."DATE_ADD('1970-01-01', INTERVAL UNIX_TIMESTAMP() SECOND)+0,"
            ."$page{latest},$page{latest_len}),\n";
    } else {
      $_[2] .= "($page{id},$ns,$page{title},$page{restrictions},0,"
        ."$page{redirect},0,RAND(),NOW(),$page{latest},$page{latest_len}),\n";
    }
    if ($page{do_commit}) {
      flush $_[0], $_[1], $_[2];
      print "BEGIN;\n";
    }
  }
}

sub terminate
{
  die "terminated by SIG$_[0]\n";
}

my $SchemaVer = '0.3';
my $SchemaLoc = "http://www.mediawiki.org/xml/export-$SchemaVer/";
my $Schema    = "http://www.mediawiki.org/xml/export-$SchemaVer.xsd";

my $help;
GetOptions("skip=i"           => \$skip,
           "help"             => \$help) or pod2usage(2);
$help and pod2usage(1);

getline;
m|^<mediawiki \Qxmlns="$SchemaLoc" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="$SchemaLoc $Schema" version="$SchemaVer"\E xml:lang="..">$|
  or die "unknown schema or invalid first line\n";
getline;
$SIG{TERM} = $SIG{INT} = \&terminate;
siteinfo;
my ($text, $rev, $page) = ('', '', '');
eval {
  while (1) {
    page $text, $rev, $page;
  }
};
$@ =~ /^expected page element / or die "$@ (committed $committed pages)\n";
flush $text, $rev, $page;
stats;
m|</mediawiki>| or die "mediawiki: expected closing tag in line $.\n";

=head1 COPYRIGHT

Copyright 2007 by Robert Bihlmeyer

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

You may also redistribute and/or modify this software under the terms
of the GNU Free Documentation License without invariant sections, and
without front-cover or back-cover texts.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
