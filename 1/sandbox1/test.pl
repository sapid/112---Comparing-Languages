#!/usr/bin/perl -w
use strict;
use warnings;
my $RCSID = '$Id: getopts.perl,v 358.4 2005-11-14 18:44:54-08 - - $';

$0 =~ s|^(.*/)?([^/]+)/*$|$2|;
my $EXITCODE = 0;
END{ exit $EXITCODE; }
sub note(@) { print STDERR "$0: @_"; };
$SIG{'__WARN__'} = sub { note @_; $EXITCODE = 1; };
$SIG{'__DIE__'} = sub { warn @_; exit; };

(my $USAGE = <<__END_USAGE__) =~ s/^#[ ]?//gm;
#
# NAME
#    $0 - getopts example
#
# SYNOPSIS
#    $0 [-abcopq] [file...]
#
# DESCRIPTION
#    Illustrates the use of getopts.
#
# OPTIONS
#    -h    print help and exit
#    -abc  flags not requiring options
#    -opq  flags requiring arguments
#
# $RCSID
__END_USAGE__

use Getopt::Std;
my %OPTS;
getopts ("abcho:p:q:", \%OPTS);
print $USAGE and exit if $OPTS{'h'};

print "$0: -$_ = $OPTS{$_}\n" for sort keys %OPTS;
print "$0: ARGV[$_]=$ARGV[$_]\n" for 0 .. $#ARGV;
print "-o = $OPTS{'o'}\n"
