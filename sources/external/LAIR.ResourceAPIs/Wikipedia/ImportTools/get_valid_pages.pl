#!/usr/bin/perl -w
use Encode;
use strict;

if(scalar@ARGV != 5)
{
    print "Usage:  ./get_valid_pages.pl\n" .
          "  <dump XML file>         :  raw XML dump file from Wikipedia\n" .
          "  <start ID>              :  page ID at which to start getting pages\n" .
          "  <output XML file>       :  where to write output to\n" .
          "  <# of pages to get>     :  number of pages to get\n" .
          "  <starting file position>:  position within input file to start looking for <start ID>\n\n";
    exit;
}

my($input_file, $page_id, $output_file, $num_to_get, $start_pos) = @ARGV;
my $temp_file = ".tempout";
my $skip_dump = ".skipped";

system("rm -f $temp_file");
system("cat header.xml > $temp_file");

open(INPUT, $input_file) || die "Could not open input file $input_file";
seek(INPUT, $start_pos, 0);
open(TMPOUT, ">>$temp_file") || die "Could not open output file $temp_file";
open(SKIP, ">$skip_dump") || die "Could not open skip dump file $skip_dump";

binmode TMPOUT, ":utf8";
binmode INPUT, ":utf8";
binmode SKIP, ":utf8";

my $max_id = 0;
my $start_output = 0;
my $pages_scanned = 0;
my $pages_output = 0;
my $fpos = tell(INPUT);
my $pages_skipped = 0;
my $page_element;
while(<INPUT>)
{
    if(!utf8::is_utf8($_))
    {
        die "Input string isn't in UTF8:  $_\n";
    }

    if($start_output)
    {
        # skip pages with gothic and cuneiform characters (BAD)
        if(/^\s*<title>([^<]+)<\/title>.*/)
        {
            my $title = $1;
            if($title =~ /.*[\x{10330}-\x{1034A}\x{12000}-\x{123FF}].*/)
            {
                print SKIP $page_element;
                print SKIP $_;

                # skip rest of page
                ++$pages_skipped;
                print "Skipping page $pages_skipped\n";
                while(<INPUT>)
                {
                    print SKIP $_;
                    if(/.*<\/page>.*/)
                    {
                        last;
                    }
                }
                next;                
            }
            else
            {
                print TMPOUT $page_element;
                print TMPOUT $_;
                next;
            }
        }

        # check for start of page so we can check title for bad chars
        if(/.*<page>.*/)
        {
            $page_element = $_;
        }
        # otherwise, write line
        else
        {
            print TMPOUT $_;
        }

        # check for end of page
        if(/.*<\/page>.*/)
        {
            --$num_to_get;
            if((++$pages_output % 10000) == 0)
            {
                print "$pages_output pages output\n";
            }
        }

        if($num_to_get == 0)
        {
            print TMPOUT "</mediawiki>\n";
            last;
        }

        next;
    }

    # check for start page
    if(!/^\s*<title>([^<]+)<\/title>.*/)
    {
        $fpos = tell(INPUT);
        next;
    }

    my $title = $1;
    if((++$pages_scanned % 10000) == 0)
    {
        print $pages_scanned . " pages scanned, max ID is $max_id\n";
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

        if($max_id >= $page_id)
        {
            # print start of page
            print TMPOUT "  <page>\n";
            print TMPOUT "    <title>$title</title>\n";
            print TMPOUT "      <id>$max_id</id>\n";

            # print rest of page
            while(<INPUT>)
            {
                if(/.*<\/page>.*/)
                {
                    print TMPOUT $_;
                    last;                    
                }
                print TMPOUT $_;
            }
            --$num_to_get;

            # set flag to output remaining pages
            $start_output = 1;
            print "Printing pages to $output_file, started at $title, ID=$max_id, file position=$fpos\n";
        }
    }
    else
    {
        die "Invalid ID line:  $idStr";
    }

    $fpos = tell(INPUT);
}

close(INPUT);
close(TMPOUT);
close(SKIP);
system("mv $temp_file $output_file");
