#!/usr/bin/perl
#use warnings ;
#use strict ;

# Feburary 21th, 2013
# Created by Jœlia Raices - USP
# Crosslink tables of gene's age and expression data.

for($i=0;$i<=$#ARGV;$i+=2){ # the vector ARGV contains what was writen after the program. Here it's used to read the files that will be used.
   
    if($ARGV[$i] eq "-a"){
        $age = $ARGV[$i+1];
    }
    if($ARGV[$i] eq "-e"){
        $expression = $ARGV[$i+1];
    }
}

if(($age eq "") || ($expression eq "")){ # a quick guide to how to use this program.
    print "Program usage: perl crosslink_age_expression_2013.pl -a AGE -e EXPRESSION\n";
    print "\t-a: Age data.\n";
    print "\t-e: Expression data.\n";
    exit(0);
}

unless (open (AGE , $age) && open (EXPRESSION , $expression)){
    die ("My programming is bad, and I should feel bad.\n");
} # if the files are not openned it will die and print it.

if (open (AGE , $age) && open (EXPRESSION , $expression)){ # assures the files are opened, before running the code.
    my ($i, $b, $c) = 0x3;
    open (LOG , ">>crosslink_age_expression_2013.log");
    open (OUTPUT, ">crosslink_age_expression_table.txt");
    print LOG "\n".(localtime)."\n";
    print LOG "Age data: $age\n" ;
    print LOG "Expression data: $expression\n" ;
    undef @not_found;
    undef %flag;
    while (<AGE>){ # stores the data from the age file.
	$lines = $_;
	chomp $lines;
	undef @age ;
	@age = split(/\t/,$lines) ;
	#$age_data{$age[0]} = "\t$age[1]\t$age[2]\t$age[3]";
	if ($age[2] == "0"){
	    $age_data{$age[0]} = "\t$age[1]\t$age[2]\t$age[3]\told";
	}
	else {
	    $age_data{$age[0]} = "\t$age[1]\t$age[2]\t$age[3]\tnew";
	} 
	if ($lines=~/Symbol/){}
	else {
	    $flag{$age[0]} = 0 ;
	}
    }
    while (<EXPRESSION>){
	$line = $_;
	chomp $line;
	$line=~s/\|/\t/g;
	
	undef @expression;
	@expression = split (/\t/, $line);
	undef @expression_2;
	undef @expression_3;
	undef @expression_4;
	@expression_2 = split (/-/ , $expression[1]);
	if($expression[3]=~/,/){
		@expression_3 = split (/,/ , $expression[3]);
	}
	else{
	    $expression_3[0]=$expression[3];
	}
	@expression_4 = split (/,/ , $expression[2]);
	if($expression[2]=~/,/){
	    @expression_4 = split (/,/ , $expression[2]);
	}
	else{
	    $expression_4[0] = $expression[2];
	}
	$newline="";
	$newline="$expression[0]";	
	for($k=1; $k<=11; $k++){
	    $newline.="\t$expression[$k]";	
        }
	
	if ($line =~ /Symbol/){
	    print OUTPUT "$newline";
	    print OUTPUT ("\tCG_Zhang\tchrom\tbranch\tbias\tage\n");
	}
	else{
	    if ($age_data{$expression_2[0]} ne ""){ # compares the gene name in both tables and prints the data.
		print OUTPUT ("$newline", "\t$expression_2[0]" , "$age_data{$expression_2[0]}", "\n");
		$b =$b + 1;
		$flag{$expression_2[0]} = 1;
	    }
	    elsif ($age_data{$expression_4[0]} ne ""){ # if the gene was not found in the first column, it tries in the first part of the second one.
		print OUTPUT ("$newline", "\t$expression_4[0]" , "$age_data{$expression_4[0]}", "\n");
		$b =$b + 1;
		$flag{$expression_4[0]} = 1;
	    }
	    elsif ($age_data{$expression_4[1]} ne ""){ # if the gene was not found in the first column, it tries in the second part of the second one.
		print OUTPUT ("$newline", "\t$expression_4[1]" , "$age_data{$expression_4[1]}", "\n");
		$b =$b + 1;
		$flag{$expression_4[1]} = 1;
	    }
	    elsif ($age_data{$expression_3[0]} ne ""){ # if the gene was not found in the first column, it tries in the  first part of the third one.
		print OUTPUT ("$newline", "\t$expression_3[0]" , "$age_data{$expression_3[0]}", "\n");
		$b =$b + 1;
		$flag{$expression_3[0]} = 1;
	    }
	    elsif ($age_data{$expression_3[1]} ne ""){ # if the gene was not found in the first column, it tries in the second part of the third one.
		print OUTPUT ("$newline", "\t$expression_3[1]" , "$age_data{$expression_3[1]}", "\n");
		$b =$b + 1;
		$flag{$expression_3[1]} = 1;
	    }
	    else { # if the program does not find a match for a given entrance, it goes here.
		$not_found[$i] = "$expression[1]\t$expression[2]\t$expression[3]"; # stores the elements that were not found in the array @not_found
		$i = $i + 1;
	    }
	}
    }
    foreach $key (keys %flag){
	if ($flag{$key} == 0){
	    $c++;
	}
	
    }
    
    print LOG "General results:\n";
    print LOG "\tThe total amount of entrances found and crosslinked was: $b.\n";
    print LOG "\tThe total number of entrances not found on Zhang was: $#not_found.\n";
    print LOG "\tThe total number of entrances not found from Zhang was: $c \n";    
    print LOG "\tCould not find the following entrances in Zhang's file:\n";
    for ($i=0; $i<=$#not_found; $i++){
	print LOG ($not_found[$i] , "\n");
    }
    print LOG "\tCould not find the following entrances from Zhang's file:\n";    
    foreach $key (keys %flag){
	if ($flag{$key}==0){
	print LOG ("$key\n");
	}
	else {}
    }
}

close (AGE);
close (EXPRESSION);
close (OUTPUT);
close (LOG);