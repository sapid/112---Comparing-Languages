#!/usr/bin/perl
use strict;
use warnings;

$0 =~ s|^(.*/)?([^/]+)/*$|$2|; # Get the basename.
my $EXITCODE = 0;
END { exit $EXITCODE; } # Return exit code on exit.
sub note(@) { print STDERR "$0: @_"; };
$SIG{'__WARN__'} = sub { note @_; $EXITCODE = 1; };
$SIG{'__DIE__'} = sub { warn @_; exit; };

(my $USAGE = <<__END_USAGE__) =~ s/^#[ ]?//gm;
#
# NAME
#    $0 - $0 Perform Makefile tasks with Perl
#
__END_USAGE__

use Getopt::Std;
my %OPTIONS;
getopts ("chnstv", \%OPTIONS); # These options are wrong.
print $USAGE and exit if $OPTIONS{'h'};

# Need perl command-line argument variables to get a filename.
# Set flags based on command-line flags.
open my $file "<filename" or die "$0:$filename:$!\n"
while( defined(my $line = <$file>)) {
# Do stuff with the line.
}