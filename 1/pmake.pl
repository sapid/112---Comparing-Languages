#!/usr/bin/perl
use strict;
use warnings;
use POSIX qw(locale_h);
my $fullname = $0;
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

my %macro_hash= ();
my %target_hash = ();
my @has_pre = ();
my %cmd_hash = ();

my $previous_target = "";
my $include;
my $include_string;

# Need perl command-line argument variables to get a filename.
# Set flags based on command-line flags.
open my $file, "<$filename" or die "$0:$filename:$!\n";
while (my $line = <$file>){
    chomp($line);
    $previous_target = &check_line($line,\%macro_hash,\%target_hash,\%cmd_hash,\                                   \$previous_target);
}
foreach my $myMacro (keys %macro_hash){
    my @check_list = @{$macro_hash{$myMacro}};
    @check_list = &replace_macro(\@check_list,\%macro_hash);
    $macro_hash{$myMacro} = [@check_list];
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
    my @check_list = @{$target_hash{$tar}};
    if (@check_list > 0){
        @check_list = &replace_macro(\@check_list,\%macro_hash);
        $target_hash{$tar} = [@check_list];
    }
}

foreach my $tar (keys %cmd_hash){
    if (exists($cmd_hash{$tar})){
        if ($tar =~ /\$\{([^\}]+)\}/){
            my @replace_target = @{$macro_hash{$1}};
            my $replace = "";
            foreach my $str (@replace_target){$replace = $str;}
            my @replace_list = @{$cmd_hash{$tar}};
            delete $cmd_hash{$tar};
            @{$cmd_hash{$replace}} = @replace_list;
            $tar = $replace;
        }
        my @check_list = @{$cmd_hash{$tar}};
        @check_list = &replace_macro(\@check_list,\%macro_hash);
        $cmd_hash{$tar} = [@check_list];
    }
}

if ($include){
    my $finish = "";
    my @include_split = split(" ",$include_string);
    @include_split = &replace_macro(\@include_split,\%macro_hash);
    my @cmd_list = @{$cmd_hash{"deps"}};
    print "@cmd_list";
    foreach my $cmd (@cmd_list){
        print "$cmd";    
    }
}
#Checks a line for a macro, target or cmd. Places corresponding value into
#the correct hash
sub check_line {
    my $line = $_[0];
    my $macro_hash = $_[1];
    my $target_hash = $_[2];
    my $cmd_hash = $_[3];
    my $prev_target = $_[4];

    if ($line !~ /^#.+/){

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
        #If a target, places in the target hash
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

        #Checks to see if the line is an include
        #If include, sets include string = to that line, and sets include = 1
        elsif ($line =~ /\s*include.+/) {
            $include = 1;
            $include_string = $line;
        }
    }
   return $previous_target;
}

sub replace_macro {
    my @line = @{$_[0]};
    my $macro_hash = $_[1];
    my $done_string = "";
    for(my $count = 0; $count < @line; $count++){
       my $value = $line[$count];
       if ($value =~ /\$\{([^\}]+)\}/){
          if ($1 eq "MAKE"){
              my @make_list = ("pmake");
              splice @line, $count, 1, @make_list;
          }
          else{
              my @replace_list = @{$macro_hash->{$1}};
              splice @line, $count, 1, @replace_list;
          }
       }
    }
    return @line;
}
