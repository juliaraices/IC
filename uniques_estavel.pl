# March, 2015
# Júlia Raíces
# Program intended to cross 2 tables to see the repetitions.
# list of duplicated genes, gives us a table with the duplicates dn, ds and dn/ds
#!/usr/bin/perl
##use warnings ; # let's try it... let's not... lol
use strict ; #doesn't let me use variables if i have not declared them

my ($i, $s4a, $s4b); # files i'll use
my ($name, $output, $lines, $line, $lin, $j, $k, $l, $size); # counters, strings and other stuff i'll use, j, l and k in this case are counters
my (%s4a_data, %s4b_data, %dnds, %dnds_); # where i'll store the datas (note that the list does not need a hash, for it can be accessed by one coordinate alone
my (@s4a, @s4b); # raw data that i have not yet transformed
undef %s4a_data ;
undef %dnds ;
undef %dnds_ ;
undef @s4a ;
undef @s4b ; #undef so that things will be empty when i use them
undef %s4b_data;
for($i=0;$i<=$#ARGV;$i+=2){ # the vector ARGV contains what was writen after the program. Here it's used to read the files that will be used.
	if($ARGV[$i] eq "-s4a"){
		$s4a = $ARGV[$i+1]; #saves the retro file in the $retro variable (or whatever apears after -r)
	}
	if($ARGV[$i] eq "-s4b"){
		$s4b = $ARGV[$i+1]; #saves the retro file in the $retro variable (or whatever apears after -r)
	}
} #(do that until all ARGV is read, and both -t and -l are stored
if(($s4a eq "" || $s4b eq "")){# a quick guide to how to use this program, in case you got it wrong....
	print "Program usage: perl uniques.pl -s4a S4A_TABLE_WITH_DN_DS_DATA -s4b S4B_TABLE_WITH_DN_DS_DATA\n";
	print "\t-s4a: S4a Table of genes's dn and ds data.\n";
	print "\t-s4b: S4b Table of genes's dn and ds data.\n";
	exit(0);
}
open (LOG, ">>uniques.log"); #opens the log file and adds things to it. (while keeps the old things)
print LOG "\n".(localtime)."\n"; #print the time in the log file
print LOG "Table S4a data: $s4a\n" ; #prints the table data file in the log...
print LOG "Table S4b data: $s4b\n" ; #prints the table data file in the log...
print "Give new name to output? (Y/N)\n"; # checks if you want a new name to be given to the output
$name = <STDIN>;
chomp $name;
if ($name eq "Y" || $name eq "y" || $name eq "YES" || $name eq "Yes" || $name eq "yes"){ #if you want a new name, asks for it
	print "what name should be given?\n";
	$output = <STDIN>;
	chomp $output; #gets what the new name is
	open (OUTPUT, ">$output"); #opens the new output using the new name!
	print LOG "Output file: $output\n"; # ...and also the list file
}
else{ # if you don't want a new name calls the standard output name
	open (OUTPUT, ">uniques_output.txt"); #opens a new output file with standard name
	print LOG "Output file: uniques_output.txt\n"; # ...and also the list file
}
# ok, so here is where the program actually start doing things to your files, almost everything before was just geting ready for this part ^^
unless(open(S4A, $s4a) && open(S4B, $s4b)){ # if not all files are open (unless all are open), kills the program with an error message
	print STDERR "Couldn't open files, please check if files exist and if you have permission to read them.\n";
	print LOG "Error opening files. \n";
	exit 0;
}
# if all files were actually opened
print LOG "General results:\n";
print OUTPUT "id\tdn1\tds1\tdn1/ds1\tdn2\tds2\tdn2/ds2\tequal?\n"; # what will be the header of the new file
while (<S4A>){# for the equivalence data (all of it)
	$line = $_;
	chomp $line; #gets all lines, without the "break line" sign
	@s4a = split(/\t/,$line);#stores each column (separated by tabs) into a space in to a  vector
	if($s4a[2] != 0){
		$dnds{$s4a[13]} = $s4a[4]/$s4a[2] ;
	}
	else{
		$dnds{$s4a[13]} = "NaN";
	}
	$s4a_data{$s4a[13]}="$s4a[13]\t$s4a[4]\t$s4a[2]\t$dnds{$s4a[13]}"; #first i'll try only with primary id and anotation, if it doesn't work, i'll try the other...
}
while (<S4B>){ # while the table file is open do:
	$lines = $_;
	chomp $lines;  #gets all lines, without the "break line" sign
	@s4b = split(/\t/,$lines) ; #stores each column (separated by tabs) into a space in to a vector
	if($s4b[2] != 0){
		$dnds_{$s4b[13]} = $s4b[4]/$s4b[2] ;
	}
	else{
		$dnds_{$s4b[13]} = "NaN";
	}
	$s4b_data{$s4b[13]}="$s4b[4]\t$s4b[2]\t$dnds_{$s4b[13]}"; #first i'll try only with primary id and anotation, if it doesn't work, i'll try the other...
	if($s4a_data{$s4b[13]} ne ""){#if there is a fb gene with the same identifier as a gene in the table file
		$j++; # increases the counter, because a new match was found fo FB notation
		if($dnds{$s4b[13]} == $dnds_{$s4b[13]}){
			print OUTPUT "$s4a_data{$s4b[13]}\t$s4b_data{$s4b[13]}\tY\n"; # archives both table and equivalence data in a new hash for the $equi[3] gene
			$k++;
		}
		else{
			print OUTPUT "$s4a_data{$s4b[13]}\t$s4b_data{$s4b[13]}\tN\n"; # archives both table and equivalence data in a new hash for the $equi[3] gene
			$l++;
		}
	}
}
print LOG "\t$j genes crossed between the tables.\n"; # tells how many genes where in both files
print LOG "\t$k genes with same dn/ds.\n"; # tells how many genes where in both files
print LOG "\t$l genes with different dn/ds.\n"; # tells how many genes where in both files
close LOG; #closes Log, Output and all other files...
close OUTPUT ;
close S4B;
close S4A;
exit;
