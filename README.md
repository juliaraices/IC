# IC pipelines and scripts

This scripts were made to analyse the proportion of new and old genes in different phases of spermatogenesis. The scripts were developed during my undergrad research project around 2015. The perl script needs two inputs: one with the expression of genes in different spermatogenesis stages and another with the genes' age as the branch from _Drosophila_ phylogeny in which the gene appears.

## Getting Started

To get the  scripts running you need to just download the scripts and run them according to the instructions. You can get the instructions for each script by reading bellow, or by running them without any inputs or parameters. For how I used the scripts and with which inputs check [pipelines.txt](https://github.com/juliaraices/IC/blob/master/pipelines.txt).

### Prerequisites

To be able to run the scripts you need Perl version 5.18 and R version 3.2. Please follow the developers' directions for download and installation of the programs.

R: https://cloud.r-project.org/
Perl: https://www.perl.org/get.html


## Usage

For using the *crosslink_age_expression_2013.pl* with an input with the age data and one input with spermatogenesis expression:

`
perl crosslink_age_expression_2013.pl -a AgeFile.txt -e ExpressionFile.txt
`

The AgeFile.txt must be a tab separated file, in which first column is a gene identifier, and the 3rd column is the branch in which the gene first appeared.

The ExpressionFile.txt must be tab separated. The first column has the gene identifier, the second, third, and fourth should have expression in different spermatogenesis phases.

After running the script, two files will be created: a log file, and an output file. The log is called *crosslink_age_expression_2013.log*, and the output is *crosslink_age_expression_table.txt*. The log file will have a compilation of all the runs for the script, and any errors or problems with the run.

Once the output is created, there's still some tinkering to be done with the output table. Here they are:

First we define if each gene is autosomic or on the X-chromosome from the AgeFile.txt and ExpressionFile.txt:

`
  awk '{if($14 == "chrX") XorA="X"; else if($14 == "chrom") XorA="XorAZ"; else XorA="A"; print $0, "\t", XorA}' crosslink_age_expression_table.txt > cross_test.txt

awk '{if($5 == "arm_X") XorA="X"; else if($5 == "Chr") XorA="XorAV"; else XorA="A"; print $0,"\t", XorA}' cross_test.txt > cross_test2.txt
`

After this it is necessary to make sure the identification of the gene's location is the same between the two files:

`
awk '{if($19 == $18) XorA = "ok"; else XorA = "not_ok";  print $0,"\t", XorA}' cross_test2.txt >cross_test3.txt
`

Finally, we create a new column for the expression pattern of each gene during spermmatogenesis:

`
awk '{if($12=="1" || $12=="2" || $12=="3") new_cat = "PostMeiotic"; else if($12=="13") new_cat = "Equal"; else if($12=="6" || $12=="8" || $12=="10") new_cat = "Mitotic";  else if($12=="9") new_cat = "Mitotic-Meiotic"; else if($12=="4" || $12=="7" || $12=="12") new_cat = "Meiotic";  else if($12=="5") new_cat = "Meiotic-PostMeiotic";  else if($12=="11") new_cat = "TheV";  else if($12=="14" || $12=="15" || $12=="16" || $12=="17" || $12=="18" || $12=="19") new_cat = "Impossible"; else new_cat= "New_Class"; print $0, "\t", new_cat}' cross_test3.txt > table_allClasses.txt
`

Once all of that is done, it is possible to run the R script to get statiscal analysis and plots.

`
R source("complete_analysis.R")
`


## License

This project is licensed under copyleft agreement. You may use it and modify it but should cite this project and author when publishing.

## Acknowledgments

* Prof. Dr. Maria Vibranovski (advisor)

