#!/usr/bin/perl
use strict;
use warnings;
use POSIX qw(locale_h);

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
getopts ("hdnf:", \%OPTIONS);
print $USAGE and exit if $OPTIONS{'h'};

# Need perl command-line argument variables to get a filename.
# Set flags based on command-line flags.
open my $file, "<filename" or die "$0:$filename:$!\n";
while (my($line) = <$file>){
# Do stuff with the line.

#Checks to see if the line is a macro. If it is macro, it adds it to the macro
#hash
   if ($line =~ /(\w+)\s=\s(.+)/){
        my($macro) = $1;
        my($value) = $2;
        # $macro_hash{$macro} = $value;
        print "Added! macro\n";
    }
#Checks to see if the line is a target. If it is, it adds it to the target
#hash
    elsif ($line =~ /(\w+)\s:\s(.+)/){
        my($target) = $1;
        my($pre) = $2;
        # $target_hash{$target} = $pre;
        print "Added! target\n";
    }
#Checks to see if the line is a command. If it is, it adds it to the cmd list
    elsif ($line =~ /\s(.+)/){
        my($cmd) = $1;
        #push(my(@cmd_hash), $cmd);
    }

}
