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
my %OPTIONS = ();
getopts ("hdnf:", \%OPTIONS);
print $USAGE and exit if $OPTIONS{'h'};

# Get filename.
my $filename = 'Makefile';
$filename = $OPTIONS{'f'} if $OPTIONS{'f'};
# Get target.
my $myTarget = $ARGV[0] if $ARGV[0];

# Need perl command-line argument variables to get a filename.
# Set flags based on command-line flags.
open my $file, "<$filename" or die "$0:$filename:$!\n";
my %macro_hash = ();
my %target_hash = ();
my %cmd_hash = ();
my $target = ();
#my $line = ();
while (my $line = <$file>){
	chomp($line);
# Do stuff with the line.

#Checks to see if the line is a macro. If it is macro, it adds it to the macro
#hash
   if ($line =~ /\s*(\S+)\s*=\s+(.+)/){
        my($macro) = $1;
        my($value) = $2;
		  #die "Macro $1 assigned to null string!" if ($2 == undef);
        $macro_hash{$macro} = $value;
        print "Added! macro. $1 = $2\n";

    }
#Checks to see if the line is a target. If it is, it adds it to the target
#hash
    elsif ($line =~ /\s*(\S+)\s*:.*/){
    	$target = $1;
		$cmd_hash{$target} = ();
    	if($line =~ /.+:\s+(.+)/){
    		$target_hash{$target} = $1;
    	}
    	else {$target_hash{$target} = "";}
        print "Target found; $target : $target_hash{$target}\n";
        print "Added! target: $target\n";
    }
#Checks to see if the line is a command. If it is, it adds it to the cmd list
    elsif ($line =~ /\t\s*(.+)/){
        my($cmd) = $1;
        push(@{$cmd_hash{$target}}, $cmd);
		  print "Command found: $1";
    }

}
while (my ($k, my $v) = each %macro_hash) {print "$k -- $v\n"};

close $file;
