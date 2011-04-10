#!/usr/bin/perl -w
use strict;
use warnings;
my $RCSID = '$Id: cat.perl,v 358.1 2005-10-28 18:34:27-07 - - $';

$0 =~ s|^(.*/)?([^/]+)/*$|$2|;
my $EXITCODE = 0;
END { exit $EXITCODE; }
sub note(@) { print STDERR "$0: @_"; };
$SIG{'__WARN__'} = sub { note @_; $EXITCODE = 1; };
$SIG{'__DIE__'} = sub { warn @_; exit; };

(my $USAGE = <<__END_USAGE__) =~ s/^#[ ]?//gm;
#
# NAME
#    $0 - $0 concatenate and display files
#
# SYNOPSIS
#    $0 [-chnst] [file...]
#
# DESCRIPTION
#    Displays each file in sequence.  The filename `-'
#    causes STDIN to be read
#
# OPTIONS
#    -c  comment lines beginning with `#' are ignored
#    -h  displays help man page
#    -n  each line of output is numbered
#    -s  sequences of empty lines are suppressed
#    -t  filenames are printed ahead of files
#
# $RCSID
__END_USAGE__

use POSIX qw(locale_h);
setlocale LC_CTYPE, "iso_8859_1";

use Getopt::Std;
my %OPTIONS;
getopts ("chnstv", \%OPTIONS);
print $USAGE and exit if $OPTIONS{'h'};

my $eqline = ":" x 64 . "\n";
push @ARGV, "-" unless @ARGV;
for my $filename (@ARGV) {
   open my $infile, "<$filename"
           or warn "<$filename: $!\n" and next;
   print "\n$eqline$filename\n$eqline\n" if $OPTIONS{'t'};
   my $lastempty = 0;
   my $thisempty;
   while (defined (my $line = <$infile>)) {
      chomp $line;
      next if $OPTIONS{'c'} and $line =~ m/^\s*#/;
      $thisempty = $line =~ m/^\s*$/;
      next if $OPTIONS{'s'} and $lastempty and $thisempty;
      printf "%6d  ", $. if $OPTIONS{'n'};
      printf "%s\n", $line;
   }continue {
      $lastempty = $thisempty;
   };
   close $infile;
};

