#!/usr/bin/perl
use strict;
use warnings;
use Getopt::Std;

my $status = 0;

# Need perl command-line argument variables to get a filename.
# Set flags based on command-line flags.
open my $file "<filename" or die "$0:$filename:$!\n"
while( defined(my $line = <$file>)) {
# Do stuff with the line.
}
exit $status