#!/usr/bin/perl -w
use strict;

my($input_file, $page_title, $page_id, $output_file, $quantity) = @ARGV;

open(INPUT, $input_file) || die "Could not open input file $input_file";
open(OUTPUT, ">$output_file") || die "Could not open output file $output_file";
binmode INPUT, ":utf8";
binmode OUTPUT, ":utf8";

my $page_count = 0;
my $pages = 0;
my $max_id = 0;
my $start_output = 0;
while(<INPUT>)
{
    if(!/^\s*<title>([^<]+)<\/title>.*/)
    {
        next;
    }
    
    my $title = $1;
    if((++$pages % 10000) == 0)
    {
        print $pages . " pages, max ID is $max_id\n";
    }

    my $idStr = <INPUT>;

    if($idStr =~ /^\s*<id>([^<]+)<\/id>.*/)
    {
        my $tmpID = $1;
        if($tmpID <= $max_id)
        {
            die "Non-increasing IDs:  compare current ($tmpID) to previous ($max_id)";
        }

        $max_id = $tmpID;

        if($max_id >= $page_id &&
           !$start_output)
        {
            $start_output = 1;
            print "Printing pages, started at $title (ID=$max_id, file position=" . tell(INPUT) . "\n";
        }
    }
    else
    {
        die "Invalid ID line:  $idStr";
    }

    if(($title =~ /^$page_title$/) ||
       ($start_output))
    {
        print OUTPUT "<page>\n";
        print OUTPUT "  <title>$title</title>\n";
        print OUTPUT "    <id>$max_id</id>\n";

        while(<INPUT>)
        {
            if(/.*<\/page>.*/)
            {
                print OUTPUT $_;
                last;
            }

            print OUTPUT $_;
        }

        if(++$page_count == $quantity)
        {
            last;
        }
    }
}

print OUTPUT "</mediawiki>";
close(INPUT);
close(OUTPUT);

system("cat header.xml $output_file > missing_pages.xml");
