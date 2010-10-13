#!/usr/bin/perl -w

use strict;

my($input, $line, $output) = @ARGV;

open(INPUT, $input);
open(OUTPUT, ">$output");
binmode INPUT, ":utf8";
binmode OUTPUT, ":utf8";

while(<INPUT>)
{
    if(--$line != 0)
    {
        next;
    }

    my @inserts = split(/\),\(/, $_);

    for(my $i = 0; $i < scalar(@inserts); ++$i)
    {
        my $curr_insert = $inserts[$i];

        if($curr_insert =~ /^[^,]+,[^,]+,([^,]+),.*/)
        {
            print OUTPUT "$1\n";
        }
    }
    last;
}

close(INPUT);
close(OUTPUT);
