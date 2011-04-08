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
my @macro_list = ();
my %target_hash = ();
my @has_pre = ();
my %cmd_hash = ();
my $target = ();
#my $line = ();
while (my $line = <$file>){
	chomp($line);
# Do stuff with the line.

#Checks to see if the line is a macro. If it is macro, it adds it to the macro
#hash
   if ($line !~ /#.+/){
   if ($line =~ /\s*(\S+)\s*=\s+(.+)/){
        my($macro) = $1;
        my($value) = $2;
		  #die "Macro $1 assigned to null string!" if ($2 == undef);
        my @value_split = ();
        @value_split = split(" ", $value);
        $macro_hash{$macro} = [@value_split];
        push (@macro_list, $1);
        my @test = @{$macro_hash{$macro}};
    }
#Checks to see if the line is a target. If it is, it adds it to the target
#hash
    elsif ($line =~ /\s*(\S+)\s*:.*/ and $line !~ /\t\s*.+/){
    	$target = $1;
    	if($line =~ /.+:\s+(.+)/){
            my @value_split = ();
            @value_split = split(" ", $1);
    	      $target_hash{$target} = [@value_split];
            push(@has_pre,$target);
				push(@has_pre,';');
    	}
    	else {
            $target_hash{$target} = "";
        }
    }
#Checks to see if the line is a command. If it is, it adds it to the cmd list
    elsif ($line =~ /\t\s*(.+)/){
        my($cmd) = $1;
        my @value_split = ();
        if (exists $cmd_hash{$target}){
            @value_split = split(" ",$cmd);
            push(@{$cmd_hash{$target}}, @value_split);
        }else{
            $cmd_hash{$target} = ();
            @value_split = split(" ",$cmd);
            push(@{$cmd_hash{$target}}, @value_split);
        }
    }
  }
}
#Replaces all of the macros with their definitions. The previous macros are
#used to do this
my @macro_back = reverse(@macro_list);
foreach my $myMacro (@macro_list){
        my @check_list = @{$macro_hash{$myMacro}};
        my $done_string = "";
        for (my $count = 0; $count < @check_list; $count++){
            my $value = $check_list[$count];
            my $replace_string = "";
            if ($value =~ /\$\{([^\}]+)\}/){
                my @replace_list = @{$macro_hash{$1}};
                foreach my $str (@replace_list){
                    $replace_string = $replace_string . $str . " ";
                }
                chop($replace_string);
                $check_list[$count] = $replace_string;
            }
       }
       $macro_hash{$myMacro} = [@check_list];
}
#Takes the lists from the macro hash and converts them to strings. This is done
#so we can easily call them from the command line
foreach my $myMacro (@macro_list){
    my @check_list = @{$macro_hash{$myMacro}};
    my $done_string = "";
    for(my $count = 0; $count < @check_list; $count++){
        $done_string = $done_string . $check_list[$count] . " ";
    }
    $macro_hash{$myMacro} = $done_string;
}
#Replaces all of the macros in targets with their definitions.
foreach my $Tar (@has_pre){
    my @check_list = @{$target_hash{$Tar}};
    for (my $count = 0; $count < @check_list; $count++){
    my $value = $check_list[$count];
    my $replace_string = "";
    if ($value =~ /\$\{([^\}]+)\}/){
        $replace_string = $macro_hash{$1};
        $check_list[$count] = $replace_string;
        }
    }
}
#Converts the list the target has to strings. This is used so we can
#easily call them from the command line
foreach my $Tar (@has_pre){
    my @check_list = @{$target_hash{$Tar}};
    my $done_string = "";
    for (my $count = 0; $count < @check_list; $count++){
        $done_string = $done_string . $check_list[$count] . " ";
    }
    $target_hash{$Tar} = $done_string;
}

#Replaces all of the macros in commands with their values
foreach my $Tar (keys %cmd_hash){
    if (exists($cmd_hash{$Tar})){
        my @check_list = @{$cmd_hash{$Tar}};
        for (my $count = 0; $count < @check_list; $count++){
            my $value = $check_list[$count];
            my $replace_string = "";
            if ($value =~  /\$\{([^\}]+)\}/){
                $replace_string = $macro_hash{$1};
                $check_list[$count] = $replace_string;
            }
        }
    }
}
#Converts all of the commands to strings with macro definitions complete
foreach my $Tar (keys %cmd_hash){
    my @check_list = @{$cmd_hash{$Tar}};
    my $done_string = "";
    for (my $count = 0; $count < @check_list; $count++){
        $done_string = $done_string . $check_list[$count] . " ";
    }
    $cmd_hash{$Tar} = $done_string;
}

# Now we just have to run the target.

close $file;
