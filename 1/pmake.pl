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
# OPTIONS
#         -h: This usage information.
#
#         -d: Displays the reasons why make chooses to rebuild a target. (Debug information.)
#
#         -n: Only print commands; do not execute.
#
#         -f: Specify a Makefile to use.
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
my $myTarget = "";
$myTarget = $ARGV[0] if $ARGV[0];

my %macro_hash = ();
my %target_hash = ();
my @has_pre = ();
my %cmd_hash = ();
my $previous_target = "";

# Need perl command-line argument variables to get a filename.
# Set flags based on command-line flags.
open my $file, "<$filename" or die "$0:$filename:$!\n";
while (my $line = <$file>){
    chomp($line);
    $previous_target = &check_line($line,\%macro_hash,\%target_hash,\%cmd_hash,\                                   $previous_target);
}
foreach my $myMacro (keys %macro_hash){
    my @check_list = @{$macro_hash{$myMacro}};
    @check_list = &replace_macro(\@check_list,\%macro_hash);
    $macro_hash{$myMacro} = [@check_list];
}
foreach my $array (keys %macro_hash){
    my @print_list = @{$macro_hash{$array}};
    print "Macro: $array ";
    foreach my $str (@print_list){
      print "$str ";
    }
    print "\n";
}

foreach my $tar (@has_pre){
    if ($tar =~ /\$\{([^\}]+)\}/){
        my @replace_target = @{$macro_hash{$1}};
        my $replace = "";
        foreach my $str (@replace_target){$replace = $str;}
        my @replace_list = @{$target_hash{$tar}};
        delete $target_hash{$tar};
        @{$target_hash{$replace}} = @replace_list;
        $tar = $replace;
    }
    print "Target: $tar\n";
    my @check_list = @{$target_hash{$tar}};
    if (@check_list > 0){
        @check_list = &replace_macro(\@check_list,\%macro_hash);
        $target_hash{$tar} = [@check_list];
    }
}
foreach my $array (@has_pre){
    my @print_list = @{$target_hash{$array}};
    print "Target: $array ";
    foreach my $str (@print_list){
      print "$str ";
    }
    print "\n";
}
#Checks a line for a macro, target or cmd. Places corresponding value into
#the correct hash
sub check_line {
    my $line = $_[0];
    my $macro_hash = $_[1];
    my $target_hash = $_[2];
    my $cmd_hash = $_[3];
    my $previous_target = $_[4];

    if ($line !~ /#.+/){

        #Checks to see if line is a macro
        #If a macro, places in the macro hash
        if ($line =~ /\s*(\S+)\s*=\s+(.+)/){
            my $macro = $1;
            my $value = $2;
            my @value_split = ();
            @value_split = split(" ", $value);
            $macro_hash->{$macro} = [@value_split];
        }


        #Checks to see if the line is a target
        #If a target, palces in the target hash
        elsif ($line =~ /\s*(\S+)\s*:.*/ and $line !~ /\t\s*.+/){
            my $target = $1;
            if ($myTarget eq "") {$myTarget = $target;}
            $previous_target = $target;
            if ($line =~ /.+:\s+(.+)/){
                my @value_split = ();
                @value_split = split(" ", $1);
                $target_hash->{$target} = [@value_split];
                push(@has_pre,$target);
            }
            else {
                $target_hash->{$target} = "";
            }
        }

        #Checks to see if the line is a command.
        #If cmd, places in the cmd hash
        elsif ($line =~ /\t\s*(.+)/){
            my $cmd = $1;
            my @value_split = ();
            if (exists $cmd_hash->{$previous_target}){
               @value_split = split( " ", $cmd);
               push(@{$cmd_hash->{$previous_target}}, @value_split);
            }
            else {
                $cmd_hash->{$previous_target} = ();
                @value_split = split( " ", $cmd);
                push(@{$cmd_hash->{$previous_target}}, @value_split);
                push(@{$cmd_hash->{$previous_target}}, "\n");
            }
        }
    }
   return $previous_target
}

sub replace_macro {
    my @line = @{$_[0]};
    my $macro_hash = $_[1];
    my $done_string = "";
    for(my $count = 0; $count < @line; $count++){
       my $value = $line[$count];
       if ($value =~ /\$\{([^\}]+)\}/){
          my @replace_list = @{$macro_hash->{$1}};
          splice @line, $count, 1, @replace_list;
       }
    }
    return @line;
} 
