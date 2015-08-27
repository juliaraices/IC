#############################################################################################################
######### Program to make all the analysis I did during my undergrad work, last version (I hope)	#########
######### Júlia Raíces, July 2015																	#########
#############################################################################################################

# libraries I will use
library(ggplot2)
library(gridExtra)

alls <- read.table("table_allClasses.txt", header=T) #reading table

#getting the table as I want it:
table2 <- subset(alls, alls$XorAV =='A')
table2$CLASS <- factor(table2$CLASS, levels=c(8, 10, 6, 9, 12, 4, 7, 5, 1, 3, 2, 11, 13, 14, 15, 16, 17, 18, 19))

####################### making tables to use later ###############################
tabs <- subset(table2, table2$New_Class == "Meiotic" | table2$New_Class == "Mitotic" | table2$New_Class == "PostMeiotic")
tabs$CLASS <- factor(tabs$CLASS, levels=c(8, 10, 6, 12, 4, 7, 1, 3, 2))
tabs$age <- factor(tabs$age, levels=c("new","old"))
tabs$New_Class <- factor(tabs$New_Class, levels=c("Mitotic","Meiotic","PostMeiotic"))

tabe <- subset(table2, table2$New_Class=="Meiotic" | table2$New_Class=="PostMeiotic" | table2$New_Class == "Mitotic" | table2$New_Class =="Meiotic-PostMeiotic" | table2$New_Class =="Mitotic-Meiotic") # creating a subset of the big table, with only the groups listed above (correspondent to classes 1 to 10 + 12)
# making the columns i will use into factors
tabe$CLASS <- factor(tabe$CLASS, levels=c(8, 10, 6, 9, 12, 4, 7, 5, 1, 3, 2))
tabe$age <- factor(tabe$age, levels=c("new", "old"))
tabe$New_Class <- factor(tabe$New_Class, levels=c("Mitotic", "Mitotic-Meiotic", "Meiotic", "Meiotic-PostMeiotic", "PostMeiotic"))
news <- subset(tabe, tabe$age =="new")
old <- subset(tabe, tabe$age =="old")
tabe$CLASS <- factor(tabe$CLASS, levels=c(8, 10, 6, 9, 12, 4, 7, 5, 1, 3, 2, 13, 11))
tabe$age <- factor(tabe$age, levels=c("new", "old")) 
tabe$New_Class <- factor(tabe$New_Class, levels=c("Mitotic", "Mitotic-Meiotic", "Meiotic", "Meiotic-PostMeiotic", "PostMeiotic"))

tabe$bias <- factor(tabe$bias, levels=c("male", "female", "unbiased"))
tabe$branch <- factor(tabe$branch, levels=c(0, 1, 2, 3, 4, 5, 6))

mei.pos.mit.meipos <-subset(tabe, New_Class== 'Mitotic' | New_Class =='Meiotic-PostMeiotic' | New_Class== 'Meiotic' |  New_Class =='PostMeiotic')
new.mei.pos.mit.meipos <- subset(mei.pos.mit.meipos, age =="new")
old.mei.pos.mit.meipos <- subset(mei.pos.mit.meipos, age =="old")

mei.pos.mit <-subset(tabe, New_Class== 'Mitotic' | New_Class== 'Meiotic' |  New_Class =='PostMeiotic')
mei.pos.mit$New_Class <-factor(mei.pos.mit$New_Class, levels=c('Mitotic', 'Meiotic','PostMeiotic'))
new.mei.pos.mit <- subset(mei.pos.mit, age =="new")
old.mei.pos.mit <- subset(mei.pos.mit, age =="old")

mit.meipos <- subset(tabe, New_Class == 'Mitotic' | New_Class =='Meiotic-PostMeiotic')
mit.meipos$New_Class <- factor(mit.meipos$New_Class, levels=c("Mitotic", "Meiotic-PostMeiotic"))

mit.mei <- subset(tabe, New_Class== 'Mitotic' |  New_Class =='Meiotic')
mit.mei$New_Class <- factor(mit.mei$New_Class, levels=c("Mitotic", "Meiotic"))

mit.pos <- subset(tabe, New_Class== "Mitotic" | New_Class =="PostMeiotic")
mit.pos$New_Class <- factor(mit.pos$New_Class, levels=c("Mitotic", "PostMeiotic"))

mei.pos <- subset(tabe, subset=(tabe$New_Class== "Meiotic" |  tabe$New_Class =="PostMeiotic"))
mei.pos$New_Class <- factor(mei.pos$New_Class, levels=c("Meiotic", "PostMeiotic"))

mei.meipos <- subset(tabe, subset=(tabe$New_Class== "Meiotic" |  tabe$New_Class =="Meiotic-PostMeiotic"))
mei.meipos$New_Class <- factor(mei.meipos$New_Class, levels=c("Meiotic", "Meiotic-PostMeiotic"))

meipos.pos <- subset(tabe, subset=(tabe$New_Class== "PostMeiotic" |  tabe$New_Class =="Meiotic-PostMeiotic"))
meipos.pos$New_Class <- factor(meipos.pos$New_Class, levels=c("Meiotic-PostMeiotic", "PostMeiotic"))

tabe2 <- subset(table2, table2$New_Class=="Meiotic" | table2$New_Class=="PostMeiotic" | table2$New_Class == "Mitotic" | table2$New_Class =="Meiotic-PostMeiotic" | table2$New_Class =="Mitotic-Meiotic" | table2$New_Class=="Equal")
# making things into factors
tabe2$CLASS <- factor(tabe2$CLASS, levels=c(8, 10, 6, 9, 12, 4, 7, 5, 1, 3, 2, 13))
tabe2$age <- factor(tabe2$age, levels=c("new", "old"))
tabe2$New_Class <- factor(tabe2$New_Class, levels=c("Mitotic", "Mitotic-Meiotic", "Meiotic", "Meiotic-PostMeiotic", "PostMeiotic", "Equal"))

clas.14.15 <- subset(table2, table2$CLASS == "14" | table2$CLASS=="15")
clas.14.15$CLASS <- as.factor(clas.14.15$CLASS)

##### Class 1 #####
clas.1 <- subset(table2, table2$CLASS == "1")
clas.1.old <- subset(table2, table2$CLASS == "1" & table2$age == "old")
clas.1.new <- subset(table2, table2$CLASS == "1" & table2$age == "new")
clas.1$age <- as.factor(clas.1$age)
clas.1$CLASS <- as.factor(clas.1$CLASS)
##### Class 2 #####
clas.2 <- subset(table2, table2$CLASS == "2")
clas.2.old <- subset(table2, table2$CLASS == "2" & table2$age == "old")
clas.2.new <- subset(table2, table2$CLASS == "2" & table2$age == "new")
clas.2$age <- factor(clas.2$age)
clas.2$CLASS <- factor(clas.2$CLASS)
##### Class 3 #####
clas.3 <- subset(table2, table2$CLASS == "3")
clas.3.old <- subset(table2, table2$CLASS == "3" & table2$age == "old")
clas.3.new <- subset(table2, table2$CLASS == "3" & table2$age == "new")
clas.3$age <- factor(clas.3$age)
clas.3$CLASS <- factor(clas.3$CLASS)
##### Class 4 #####
clas.4 <- subset(table2, table2$CLASS == "4")
clas.4.old <- subset(table2, table2$CLASS == "4" & table2$age == "old")
clas.4.new <- subset(table2, table2$CLASS == "4" & table2$age == "new")
clas.4$age <- factor(clas.4$age)
clas.4$CLASS <- factor(clas.4$CLASS)
##### Class 5 #####
clas.5 <- subset(table2, table2$CLASS == "5")
clas.5.old <- subset(table2, table2$CLASS == "5" & table2$age == "old")
clas.5.new <- subset(table2, table2$CLASS == "5" & table2$age == "new")
clas.5$age <- factor(clas.5$age)
clas.5$CLASS <- factor(clas.5$CLASS)
##### Class 6 #####
clas.6 <- subset(table2, table2$CLASS == "6")
clas.6.old <- subset(table2, table2$CLASS == "6" & table2$age == "old")
clas.6.new <- subset(table2, table2$CLASS == "6" & table2$age == "new")
clas.6$age <- factor(clas.6$age)
clas.6$CLASS <- factor(clas.6$CLASS)
##### Class 7 #####
clas.7 <- subset(table2, table2$CLASS == "7")
clas.7.old <- subset(table2, table2$CLASS == "7" & table2$age == "old")
clas.7.new <- subset(table2, table2$CLASS == "7" & table2$age == "new")
clas.7$age <- factor(clas.7$age)
clas.7$CLASS <- factor(clas.7$CLASS)
##### Class 8 #####
clas.8 <- subset(table2, table2$CLASS == "8")
clas.8.old <- subset(table2, table2$CLASS == "8" & table2$age == "old")
clas.8.new <- subset(table2, table2$CLASS == "8" & table2$age == "new")
clas.8$age <- factor(clas.8$age)
clas.8$CLASS <- factor(clas.8$CLASS)
##### Class 9 #####
clas.9 <- subset(table2, table2$CLASS == "9")
clas.9.old <- subset(table2, table2$CLASS == "9" & table2$age == "old")
clas.9.new <- subset(table2, table2$CLASS == "9" & table2$age == "new")
clas.9$age <- factor(clas.9$age)
clas.9$CLASS <- factor(clas.9$CLASS)
##### Class 10 #####
clas.10 <- subset(table2, table2$CLASS == "10")
clas.10.old <- subset(table2, table2$CLASS == "10" & table2$age == "old")
clas.10.new <- subset(table2, table2$CLASS == "10" & table2$age == "new")
clas.10$age <- factor(clas.10$age)
clas.10$CLASS <- factor(clas.10$CLASS)
##### Class 11 #####
clas.11 <- subset(table2, table2$CLASS == "11")
clas.11.old <- subset(table2, table2$CLASS == "11" & table2$age == "old")
clas.11.new <- subset(table2, table2$CLASS == "11" & table2$age == "new")
clas.11$age <- factor(clas.11$age)
clas.11$CLASS <- factor(clas.11$CLASS)
##### Class 12 #####
clas.12 <- subset(table2, table2$CLASS == "12")
clas.12.old <- subset(table2, table2$CLASS == "12" & table2$age == "old")
clas.12.new <- subset(table2, table2$CLASS == "12" & table2$age == "new")
clas.12$age <- factor(clas.12$age)
clas.12$CLASS <- factor(clas.12$CLASS)
##### Class 13 #####
clas.13 <- subset(table2, table2$CLASS == "13")
clas.13.old <- subset(table2, table2$CLASS == "13" & table2$age == "old")
clas.13.new <- subset(table2, table2$CLASS == "13" & table2$age == "new")
clas.13$age <- factor(clas.13$age)
clas.13$CLASS <- factor(clas.13$CLASS)
##### Class 14 #####
clas.14 <- subset(table2, table2$CLASS == "14")
clas.14.old <- subset(table2, table2$CLASS == "14" & table2$age == "old")
clas.14.new <- subset(table2, table2$CLASS == "14" & table2$age == "new")
clas.14$age <- factor(clas.14$age)
clas.14$CLASS <- factor(clas.14$CLASS)
##### Class 15 #####
clas.15 <- subset(table2, table2$CLASS == "15")
clas.15.old <- subset(table2, table2$CLASS == "15" & table2$age == "old")
clas.15.new <- subset(table2, table2$CLASS == "15" & table2$age == "new")
clas.15$age <- factor(clas.15$age)
clas.15$CLASS <- factor(clas.15$CLASS)

classes1.4 <- subset(tabe2, tabe2$CLASS == "1" | tabe2$CLASS == "4")
classes1.4$age <- factor(classes1.4$age)
classes1.4$CLASS <- factor(classes1.4$CLASS)

classes1.6 <- subset(tabe2, tabe2$CLASS == "1" | tabe2$CLASS == "6")
classes1.6$age <- factor(classes1.6$age)
classes1.6$CLASS <- factor(classes1.6$CLASS)

classes1.8 <- subset(tabe2, tabe2$CLASS == "1" | tabe2$CLASS == "8")
classes1.8$age <- factor(classes1.8$age)
classes1.8$CLASS <- factor(classes1.8$CLASS)

classes1.9 <- subset(tabe2, tabe2$CLASS == "1" | tabe2$CLASS == "9")
classes1.9$age <- factor(classes1.9$age)
classes1.9$CLASS <- factor(classes1.9$CLASS)

classes1.10 <- subset(tabe2, tabe2$CLASS == "1" | tabe2$CLASS == "10")
classes1.10$age <- factor(classes1.10$age)
classes1.10$CLASS <- factor(classes1.10$CLASS)

classes1.6 <- subset(tabe2, tabe2$CLASS == "1" | tabe2$CLASS == "6")
classes1.6$age <- factor(classes1.6$age)
classes1.6$CLASS <- factor(classes1.6$CLASS)

classes3.8 <- subset(tabe2, tabe2$CLASS == "3" | tabe2$CLASS == "8")
classes3.8$age <- factor(classes3.8$age)
classes3.8$CLASS <- factor(classes3.8$CLASS)

classes3.10 <- subset(tabe2, tabe2$CLASS == "3" | tabe2$CLASS == "10")
classes3.10$age <- factor(classes3.10$age)
classes3.10$CLASS <- factor(classes3.10$CLASS)

classes3.6 <- subset(tabe2, tabe2$CLASS == "3" | tabe2$CLASS == "6")
classes3.6$age <- factor(classes3.6$age)
classes3.6$CLASS <- factor(classes3.6$CLASS)

classes8.4 <- subset(tabe2, tabe2$CLASS == "4" | tabe2$CLASS == "8")
classes8.4$age <- factor(classes8.4$age)
classes8.4$CLASS <- factor(classes8.4$CLASS)

classes8.5 <- subset(tabe2, tabe2$CLASS == "5" | tabe2$CLASS == "8")
classes8.5$age <- factor(classes8.5$age)
classes8.5$CLASS <- factor(classes8.5$CLASS)

classes8.7 <- subset(tabe2, tabe2$CLASS == "7" | tabe2$CLASS == "8")
classes8.7$age <- factor(classes8.7$age)
classes8.7$CLASS <- factor(classes8.7$CLASS)

classes8.12 <- subset(tabe2, tabe2$CLASS == "12" | tabe2$CLASS == "8")
classes8.12$age <- factor(classes8.12$age)
classes8.12$CLASS <- factor(classes8.12$CLASS)

classes9.4 <- subset(tabe2, tabe2$CLASS == "4" | tabe2$CLASS == "9")
classes9.4$age <- factor(classes9.4$age)
classes9.4$CLASS <- factor(classes9.4$CLASS)

classes9.5 <- subset(tabe2, tabe2$CLASS == "5" | tabe2$CLASS == "9")
classes9.5$age <- factor(classes9.5$age)
classes9.5$CLASS <- factor(classes9.5$CLASS)

classes9.7 <- subset(tabe2, tabe2$CLASS == "7" | tabe2$CLASS == "9")
classes9.7$age <- factor(classes9.7$age)
classes9.7$CLASS <- factor(classes9.7$CLASS)

classes9.12 <- subset(tabe2, tabe2$CLASS == "12" | tabe2$CLASS == "9")
classes9.12$age <- factor(classes9.12$age)
classes9.12$CLASS <- factor(classes9.12$CLASS)
### all of this was just making tables to use later ###

######################## Here the actual program starts ###########################
sink("complete_analysis.txt")
cat("São paulo, July, 2015\n\n
	  During this project our main goal is to verify if new genes are differentially expressed in relation to old genes during all 3 spermatogenesis phases, and to understand the dynamics and evolutionary explanations that can account for this difference. To do so spermatogenesis was separated in 3 phases: Mitosis, Meiosis and Post-Meiosis.\n
	  \n
	  1) Proportion of new and old genes in each group\n
	  \t (I) Proportion of new and old genes in Mitotic (genes with higher expression during Mitosis, classes 6, 8 and 10) , Meiotic (genes with higher expression during Meiosis, classes 4, 7 and 12) and Post-Meiotic (genes with higher expression during Post-Meiosis, classes 1, 2 and 3) groups:")

print(table(mei.pos.mit$New_Class, mei.pos.mit$age))
print(prop.table(table(mei.pos.mit$New_Class, mei.pos.mit$age), 2)*100)
cat("\n\t\t a) The proportion of old and new genes is different between groups Mitotic and Meiotic, Meiotic group has more new genes:")
print(fisher.test(mit.mei$New_Class, mit.mei$age))
cat("\n\t\t b) The proportion of old and new genes is different between groups Mitotic and Post-Meiotic, Post-Meiotic groups has more new genes:")
print(fisher.test(mit.pos$New_Class, mit.pos$age))
cat("\n\t\t c) The proportion of old and new genes is different between groups Meiotic and Post-Meiotic, Meiotic group has more genes:")
print(fisher.test(mei.pos$New_Class, mei.pos$age))

cat("\n\n\t (II) Proportion of new and old genes in Mitotic, Meiotic, Post-Meiotic and Meiotic-Post.Meiotic groups\n
	\t Here we decided to add the Meiotic-PostMeiotic group because the PostMeiotic group has a decrease in new genes, and we were wondering whether this intermediate phase (Meiotic-PostMeiotic) behaves more like Meiotic or PostMeiotic group, so we could consider if we would add it to our analysis or not. Since the group Mitotic is already our control and very different from the other phases, there was no reason to analyse the Mitotic-Meiotic group.\n")
mei.pos.mit.meipos$New_Class <- factor(mei.pos.mit.meipos$New_Class, levels=c("Mitotic", "Meiotic", "Meiotic-PostMeiotic", "PostMeiotic"))
tabela2 <- table(mei.pos.mit.meipos$New_Class, mei.pos.mit.meipos$age)
print(tabela2)
print(prop.table(tabela2, 2) * 100)
cat("\n\t\t d) The proportion of old and new genes is different between groups Mitotic and Meiotic-Post.Meiotic, Meiotic-Post.Meiotic has more new genes:")
print(fisher.test(mit.meipos$age, mit.meipos$New_Class)) # gente!!!!! deu certoooo!!!!!
cat("\n\t\t e) The proportion of old and new genes is different between groups Meiotic and Meiotic-Post.Meiotic, Meiotic has more new genes:")
print(fisher.test(mei.meipos$age, mei.meipos$New_Class)) # gente!!!!! deu certoooo!!!!!
#nao-significatico para autossomos
cat("\n\t\t f) The proportion of old and new genes is different between groups Meitotic-Post.Meiotic and Post.Meiotic, PostMeiotic has more new genes:")
print(fisher.test(meipos.pos$age, meipos.pos$New_Class)) # gente!!!!! deu certoooo!!!!!
#significativo para autossomos
cat("\n We can see with this that the intermediate phase is more similar to Meiotic group then to PostMeiotic one.")

cat("\n\n\t (III) Proportion of new and old genes in classes 14 and 15:")
clas.14.15$CLASS <- factor(clas.14.15$CLASS, levels=c(14, 15))
print(table(clas.14.15$CLASS, clas.14.15$age))
print(prop.table(table(clas.14.15$CLASS, clas.14.15$age), 1))
cat("\n\t\t g) The proportion of new and old genes is different betwen classes 14 and 15, class 15 has more new genes:")
print(fisher.test(clas.14.15$CLASS, clas.14.15$age))


cat("\n\n\n2) Here we analyse the expression profile of the groups, to see if they also differ in expression according to their age.\n
	\t (I) All autossomic genes from groups Mitotic, Meiotic, Post-Meiotic, Mitotic-Meiotic and Meiotic-Post.Meiotic, used in the boxplot graphic of proportion_and_expression_1to10and12.pdf:\n
	\t\t a) Expression during Mitosis is not different for new and old genes:")
print(wilcox.test(tabe$Mitosis~tabe$age))
cat("\t\t\t Median expression of new genes during Mitosis:")
print(median(news$Mitosis))
cat("\t\t\t Median expression of old genes during Mitosis:")
print(median(old$Mitosis))
cat("\n\t\t b) Expression during Meiosis has higher expression of new genes:")
print(wilcox.test(tabe$Meiosis~tabe$age))
cat("\t\t\t Median expression of new genes during Meiosis:")
print(median(news$Meiosis))
cat("\t\t\t Median expression of old genes during Meiosis:")
print(median(old$Meiosis))
cat("\n\t\t c) Expression during Post-Meiosis has a higher expression of new genes:")
print(wilcox.test(tabe$Post.meiosis~tabe$age))
cat("\n\t\t\t Median expression of new genes during Post-Meiosis:")
print(median(news$Post.meiosis))
cat("\n\t\t\t Median expression of old genes during Post-Meiosis:")
print(median(old$Post.meiosis))

cat("\n\t (II) All autossomic genes from groups Mitotic, Meiotic and Post-Meiotic (Main conclusions remain the same). Used in the boxplot graphic of proportion_and_expression_mit_mei_pos.pdf:\n
	\t\t d) Expression during Mitosis is not different between old and new genes:")
print(wilcox.test(mei.pos.mit$Mitosis~mei.pos.mit$age))
cat("\t\t\t Median expression of new genes during Mitosis:")
print(median(new.mei.pos.mit$Mitosis))
cat("\t\t\t Median expression of old genes during Mitosis:")
print(median(old.mei.pos.mit$Mitosis))
cat("\t\t e) Expression during Meiosis has a higher expression of new genes:")
print(wilcox.test(mei.pos.mit$Meiosis~mei.pos.mit$age))
cat("\t\t\t Median expression of new genes during Meiosis:")
print(median(new.mei.pos.mit$Meiosis))
cat("\t\t\t Median expression of old genes during Meiosis:")
print(median(old.mei.pos.mit$Meiosis))
cat("\t\t f) Expression during Post-Meiosis has a higher expression of new genes:")
print(wilcox.test(mei.pos.mit$Post.meiosis~mei.pos.mit$age))
cat("\t\t\t Median expression of new genes during Post-Meiosis:")
print(median(new.mei.pos.mit$Post.meiosis))
cat("\t\t\t Median expression of old genes during Post-Meiosis:")
print(median(old.mei.pos.mit$Post.meiosis))

cat("\n\t (III) All autossomic genes from groups Mitotic, Meiotic, Post-Meiotic and Meiotic-Post.Meiotic  (also does not change main conclusions):\n
	\t\t g) Expression during Mitosis is not different between new and old genes:")
print(wilcox.test(mei.pos.mit.meipos$Mitosis~mei.pos.mit.meipos$age))
cat("\t\t\t Median expression of new genes during Mitosis:")
print(median(new.mei.pos.mit.meipos$Mitosis))
cat("\t\t\t Median expression of old genes during Mitosis:")
print(median(new.mei.pos.mit.meipos$Mitosis))
cat("\t\t h) Expression during Meiosis has a higher expression of new genes:")
print(wilcox.test(mei.pos.mit.meipos$Meiosis~mei.pos.mit.meipos$age))
cat("\t\t\t Median expression of new genes during Meiosis:")
print(median(new.mei.pos.mit.meipos$Meiosis))
cat("\t\t\t Median expression of old genes during Meiosis:")
print(median(old.mei.pos.mit.meipos$Meiosis))
cat("\t\t i) Expression during Post-Meiosis has a higher expression of new genes:")
print(wilcox.test(mei.pos.mit.meipos$Post.meiosis~mei.pos.mit.meipos$age))
cat("\t\t\t Median expression of new genes during Post-Meiosis:")
print(median(new.mei.pos.mit.meipos$Post.meiosis))
cat("\t\t\t Median expression of old genes during Post-Meiosis:")
print(median(old.mei.pos.mit.meipos$Post.meiosis))

cat("\n\t (IV) Autossomic genes from groups Mitotic, Mitotic-Meiotic, Meiotic, Meiotic-PostMeiotic, Post-Meiotic and Equal. Used in the boxplots of Mitosis, Meiosis and Post-Meiosis expression in the proportion_and_expression_1to10and12and13.pdf file. The addition of the Equal group to the expression analysis improves the role of ld genes, which are now always more expressed then new genes.\n
	\t j) Expression during Mitosis has a higher expression of old genes:")
print(wilcox.test(tabe2$Mitosis~tabe2$age))
cat("\t\t\t Median expression of new genes during Mitosis:")
print(median(subset(tabe2$Mitosis, tabe2$age == "new")))
cat("\t\t\t Median expression of old genes during Mitosis:")
print(median(subset(tabe2$Mitosis, tabe2$age == "old")))
cat("\t\t k) Expression during Meiosis has a higher expression of old genes:")
print(wilcox.test(tabe2$Meiosis~tabe2$age))
cat("\t\t\t Median expression of new genes during Meiosis:")
print(median(subset(tabe2$Meiosis, tabe2$age=="new")))
cat("\t\t\t Median expression of old genes during Meiosis:")
print(median(subset(tabe2$Meiosis, tabe2$age=="old")))
cat("\t\t l) Expression during Post-Meiosis has a higher expression of old genes:")
print(wilcox.test(tabe2$Post.meiosis~tabe2$age))
cat("\t\t\t Median expression of new genes during Post-Meiosis:")
print(median(subset(tabe2$PostMeiosis, tabe2$age=="new")))
cat("\t\t\t Median expression of old genes during Post-Meiosis:")
print(median(subset(tabe2$PostMeiosis, tabe2$age=="old")))

cat("\n\t (V) Autossomic classes 14 and 15 (from Impossible group).
	\n\t Those classes where chosen because they show a change in expression from mitosis to meiosis. Class 14 has equal expression between Mitosis and PostMeiosis and betwen Meiosis and PostMeiosis, but has a higher expression during meiosis when compared to mitosis. Class 15 has equal expression between Mitosis and PostMeiosis and between Meiosis and PostMeiosis, but has a higher expression during mitosis when compared to meiosis. The ideia behind analysing this classes is that with it we can see the behaviour of genes with distinct expression only when comparing Mitosis (control) with Meiosis (our experimental group with more interesting aspects). Yet the results show these classes behave just like Equal or Impossible groups, and therefore are not interesting for us.
	\n\t Used in the boxplots of the proportion_expression_14and15.pdf file:\n
	\t\t Expression during Mitosis has higher expression of old genes:")
print(wilcox.test(clas.14.15$Mitosis~clas.14.15$age))
cat("\t\t\t Median expression of new genes during Mitosis:")
print(median(subset(clas.14.15$Mitosis, clas.14.15$age=="new")))
cat("\t\t\t Median expression of old genes during Mitosis:")
print(median(subset(clas.14.15$Mitosis, clas.14.15$age=="old")))
cat("\t\t Expression during Meiosis has higher expression of old genes:")
print(wilcox.test(clas.14.15$Meiosis~clas.14.15$age))
cat("\t\t\t Median expression of new genes during Meiosis:")
print(median(subset(clas.14.15$Meiosis, clas.14.15$age=="new")))
cat("\t\t\t Median expression of old genes during Meiosis:")
print(median(subset(clas.14.15$Meiosis, clas.14.15$age=="old")))
cat("\t\t Expression during Post-Meiosis has higher expression of old genes:")
print(wilcox.test(clas.14.15$Post.meiosis~clas.14.15$age))
cat("\t\t\t Median expression of new genes during Post-Meiosis:")
print(median(subset(clas.14.15$PostMeiosis, clas.14.15$age=="new")))
cat("\t\t\t Median expression of old genes during Post-Meiosis:")
print(median(subset(clas.14.15$PostMeiosis, clas.14.15$age=="old")))


### doing graphs:
### only mit mei pos
gcc <- ggplot(data.frame(tabs), aes(x=New_Class, group=age))
gccnew <- data.frame(New_Class=1:3, y=c(0.22,0.46,0.32), lab="Text", age=factor("new", levels=c("new","old")))
gccold <- data.frame(New_Class=1:3, y=c(0.61,0.16,0.22), lab="Text", age=factor("old", levels=c("new","old")))
gcc1 <- gcc + geom_bar(aes(y=..density.., fill=age)) + labs(x="New expression classes", y="Density") + facet_grid(~age) + theme(legend.position = "none") + ggtitle("") + geom_text(data=gccnew, label=c("66","124","88"), y=c(0.22,0.46,0.32), size=4, angle=45) + geom_text(data=gccold, label=c("3286","927","1377"), y=c(0.61,0.16,0.22), size=4, angle=45)
# graph with proportions of new and old genes for each class (old genes = 100% and new genes = 100%, 2 graphs!)
gcc2 <- gcc + geom_histogram(aes(y=..count../sum(..count..), fill=age)) + labs(x="New expression classes", y="Density") + ggtitle("") # one graph in wich 100% = new + old genes, and old and new genes are differentiated by collor, for genes in each group
# graph with the proportion of new and old genes for the classes
gcg <- ggplot(data.frame(tabs),aes(x=CLASS, group=age))
gcgnew <- data.frame(CLASS=1:9, y=c(0.16,0.06,0.01,0.12,0.08,0.25,0.21,0.03,0.06), lab="Text", age = factor("new",levels = c("new","old")))
gcgold <- data.frame(CLASS=1:9, y=c(0.45,0.13,0.02,0.6,0.01,0.08,0.1,0.06,0.08), lab="Text", age=factor("old", levels=c("new","old")))
gcg1 <- gcg + geom_bar(aes(y=..density.., fill=age)) + labs(x="New expression classes", y="Density") + facet_grid(~age) + theme(legend.position = "none") + ggtitle("") + geom_text(data=gcgnew, label=c(46,18,2,34,23,67,60,11,17), y=c(0.16,0.06,0.01,0.12,0.08,0.25,0.21,0.03,0.06), size=4, angle=45) + geom_text(data=gcgold, label=c(2359,738,189,338,121,468,560,343,474), y=c(0.45,0.13,0.02,0.06,0.01,0.08,0.1,0.06,0.08), size=4, angle=45)# graph with proportions of new and old genes in the groups above mentioned (old genes = 100% and new genes = 100%, 2 graphs!)
gcg2 <- gcg + geom_histogram(aes(y=..count../sum(..count..), fill=age)) + labs(x="New expression classes", y="Density") + ggtitle("") # one graph in wich 100% = new + old genes, and old and new genes are differentiated by collor, for genes in groups Mit, Mei, Mit-Mei, Mei-Pos and Post
#boxplots

gcb1 <- ggplot(data.frame(tabs), aes(x=tabs$age, y=tabs$Mitosis, color=age)) + geom_boxplot(notch = T) + labs(x="Genes' age", y="Expression during Mitosis") + theme(legend.position = "none") + annotate("text", x=1.5, y=13.5, label="p = 0.1254") +ggtitle("") + scale_y_continuous(limits = c(3, 15)) # expression during mitosis for genes from classes 1 to 10 + 12
gcb2 <- ggplot(data.frame(tabs), aes(x=tabs$age, y=tabs$Meiosis, color=age)) + geom_boxplot(notch = T) + labs(x="Genes' age", y="Expression during Meiosis") + theme(legend.position = "none") + annotate("text", x=1.5, y=13.5, label="p < 0.01") +ggtitle("") + scale_y_continuous(limits = c(3, 15)) # expression during meiosis for genes in classes 1 to 10 + 12
gcb3 <- ggplot(data.frame(tabs), aes(x=tabs$age, y=tabs$Post.meiosis, color=age)) + geom_boxplot(notch = T) + labs(x="Genes' age", y="Expression during Post-Meiosis") + theme(legend.position = "none") + annotate("text", x=1.5, y=13.5, label="p < 0.01") +ggtitle("") + scale_y_continuous(limits = c(3, 15)) # expression during post-meiosis for genes in classes 1 to 10 + 12

pdf("proportion_and_expression_mit_mei_pos.pdf", width=9, height=18)
grid.arrange(arrangeGrob(gcg1, gcg2, nrow=1, ncol=2), arrangeGrob(gcc1, gcc2, nrow=1, ncol=2), arrangeGrob(gcb1, gcb2, gcb3, nrow=1, ncol=3))
grid.text("Genes' age for classes 1 to 4, 6 to 8, 10 and 12 toghether", just="centre",  y = unit(0.985, "npc"))
grid.text("Genes' age for groups toghether", just="centre",  y = unit(0.65, "npc"))
grid.text("Genes Expression in each spermatogenesis phase by age", just="centre",  y = unit(0.31, "npc"))
grid.text("Old genes: \n\t 5590\n New genes: \n\t 278", just="centre",  y = unit(0.65, "npc"), x=unit(0.94, "npc"), gp=gpar(fontsize=10))
dev.off() # graphs: proportion of new and old genes in classes 1 to 1 and 12; proportion of new and old genes in groups mitotic, mitotic-meiotic, meiotic, meiotic-postmeiotic and postmeiotic; expression of those genes in mitosis, meiosis and postmeiosis.


########## Getting all numerical classes from 1 to 10, 12 and 13 expression and proportion of new and old genes ##########
#### Mesmos gráficos com a classe equal ####

# doing graphs and stats no equal
ggp11 <- ggplot(data.frame(tabe2),aes(x=CLASS, group=age))
b1new <- data.frame(CLASS=1:12, y=c(0.08,0.03,0.01,0.03,0.5,0.04,0.1,0.12,0.095,0.02,0.03,0.38), lab="Text", age=factor("new", levels=c("new","old")))
b1old <- data.frame(CLASS=1:12, y=c(0.042, 0.08,0.02,0.06,0.04,0.01,0.05,0.06,0.06,0.04,0.05,0.28), lab="Text", age=factor("old", levels=c("new","old")))
b1 <- ggp11 + geom_bar(aes(y=..density.., fill=age)) + labs(x="New expression classes", y="Density") + facet_grid(~age) + ggtitle("") + theme(legend.position = "none") + geom_text(data=b1new, label=c("46","18","2","19","34","23","67","83","60","11","17","254"), y=c(0.08,0.03,0.01,0.03,0.5,0.04,0.1,0.12,0.095    ,0.02,0.03,0.38), size=4, angle=45) + geom_text(data=b1old, label=c("2359","738","189","557","338","23","468","559","560","343","474","2365"), y=c(0.042, 0.08,0.02,0.06,0.04,0.01,0.05,0.06,0.06,0.04,0.05,0.28), size=4, angle=45)

b3 <- ggp11 + geom_histogram(aes(y=..count../sum(..count..), fill=age)) + labs(x="New expression classes", y="Density") + ggtitle("")

group2 <- ggplot(data.frame(tabe2), aes(x=New_Class, group=age))
g3new <- data.frame(New_Class=c("Mitotic","Mitotic-Meiotic","Meiotic","Meiotic-PostMeiotic","PostMeiotic","Equal"),y =c(0.08,0.02,0.18,0.1,0.1,0.4), lab = "Text", age = factor("new",levels = c("new","old")))
g3old <- data.frame(New_Class=1:6, y=c(0.38,0.05,0.11,0.062,0.16,0.28), lab="Text", age=factor("old", levels=c("new","old")))
g3 <- group2 + geom_bar(aes(y=..density.., fill=age)) + labs(x="New expression classes", y="Density") + facet_grid(~age) + theme(legend.position = "none") + ggtitle("") + geom_text(data=g3new, label=c("66","19","124","83","88","254"), y =c(0.08,0.02,0.18,0.1,0.1,0.4), size=4, angle=45) + geom_text(data=g3old, label=c("3286","557","927","559","1377","2365"),  y=c(0.38,0.05,0.11,0.062,0.16,0.28), size=4, angle=45)# graph with proportions of new and old genes in the groups above mentioned (old genes = 100% and new genes = 100%, 2 graphs!)
g4 <- group2 + geom_histogram(aes(y=..count../sum(..count..), fill=age)) + labs(x="New expression classes", y="Density") + ggtitle("") # one graph in wich 100% = new + old genes, and old and new genes are differentiate by collor, for genes in groups Mit, Mei, Mit-Mei, Mei-Pos and Post

#boxplots
ggp12 <- ggplot(data.frame(tabe2), aes(x=tabe2$age, y=tabe2$Mitosis, color=age)) + geom_boxplot(notch = T) + labs(x="Genes' age", y="Expression during Mitosis") + theme(legend.position = "none") + annotate("text", x=1.5, y=13.5, label="p < 0.01") + scale_y_continuous(limits = c(3, 15))
ggp13 <- ggplot(data.frame(tabe2), aes(x=tabe2$age, y=tabe2$Meiosis, color=age)) + geom_boxplot(notch = T) + labs(x="Genes' age", y="Expression during Meiosis") + theme(legend.position = "none") + annotate("text", x=1.5, y=13.5, label="p < 0.01") + scale_y_continuous(limits = c(3, 15))
ggp14 <- ggplot(data.frame(tabe2), aes(x=tabe2$age, y=tabe2$Post.meiosis, color=age)) + geom_boxplot(notch = T) + labs(x="Genes' age", y="Expression during Post-Meiosis") + theme(legend.position = "none") + annotate("text", x=1.5, y=13.5, label="p < 0.01") + scale_y_continuous(limits = c(3, 15))

pdf("proportion_expression_1to10and12and13.pdf", width=16, height=12)
	grid.arrange(arrangeGrob(b1, b3, ncol=2, nrow=1), arrangeGrob(g3, g4, ncol=2, nrow=1), arrangeGrob(ggp12, ggp13, ggp14, nrow=1, ncol=3))
	grid.text("Genes' age for all classes (1 to 10 + 12 and 13) toghether", just="centre",  y = unit(0.97, "npc"))
	grid.text("Genes' age for all groups", just="centre", y=unit(0.65, "npc"))
	grid.text("Genes Expression in each spermatogenesis phase by age", just="centre",  y = unit(0.33, "npc"))
	grid.text("Old genes: \n 10788\n New genes: \n 746", just="centre",  y = unit(0.63, "npc"), x=unit(0.95, "npc"), gp=gpar(fontsize=10))
dev.off()


# proportoin classes
ggp3 <- ggplot(data.frame(tabe),aes(x=CLASS, group=age)) 
ann_text <- data.frame(CLASS = 1:11,y =c(0.1,0.03,0.01,0.03,0.06,0.05,0.15,0.2,0.13,0.02,0.03), lab = "Text", age = factor("new",levels = c("new","old")))
a1old <- data.frame(CLASS=1:11, y=c(0.3,0.1,0.02,0.08,0.05,0.01,0.07,0.08,0.08,0.05,0.07), lab="Text", age=factor("old", levels=c("new","old")))
a1 <- ggp3 + geom_bar(aes(y=..density.., fill=age)) + labs(x="New expression classes", y="Density") + facet_grid(~age) + theme(legend.position = "none") + ggtitle("") + geom_text(data=ann_text, label=c("46","18","2","19","34","23","67","83","60","11","17"), y=c(0.1,0.03,0.01,0.03,0.06,0.05,0.15,0.2,0.13,0.02,0.03), size=4, angle=45) + geom_text(data=a1old, label=c("2359","738","189","557","338","121","468","559","560","343","474"), y=c(0.3,0.1,0.02,0.08,0.05,0.01,0.07,0.08,0.08,0.05,0.07), size=4, angle=45)
# graph with proportions of new and old genes for each numerical class (old genes = 100% and new genes = 100%, 2 graphs!)
a3 <- ggp3 + geom_histogram(aes(y=..count../sum(..count..), fill=age)) + labs(x="New expression classes", y="Density") + ggtitle("") # one graph in wich 100% = new + old genes, and old and new genes are differentiated by collor, for genes in numerical classes from 1 to 10 + 12
# graph with the proportion of new and old genes for the categorical groups
group1 <- ggplot(data.frame(tabe),aes(x=New_Class, group=age))
g1new <- data.frame(New_Class=c("Mitotic","Mitotic-Meiotic","Meiotic","Meiotic-PostMeiotic","PostMeiotic"),y =c(0.17,0.4,0.3,0.2,0.21), lab = "Text", age = factor("new",levels = c("new","old")))
g1old <- data.frame(New_Class=1:5, y=c(0.5,0.08,0.12,0.08,0.21), lab="Text", age=factor("old", levels=c("new","old")))
g1 <- group1 + geom_bar(aes(y=..density.., fill=age)) + labs(x="New expression classes", y="Density") + facet_grid(~age) + theme(legend.position = "none") + ggtitle("") + geom_text(data=g1new, label=c("66","19","124","83","88"), y =c(0.17,0.04,0.3,0.2,0.21), size=4, angle=45) + geom_text(data=g1old, label=c("3286","557","927","559","1377"), y=c(0.5,0.08,0.12,0.08,0.21), size=4, angle=45)# graph with proportions of new and old genes in the groups above mentioned (old genes = 100% and new genes = 100%, 2 graphs!)
g2 <- group1 + geom_histogram(aes(y=..count../sum(..count..), fill=age)) + labs(x="New expression classes", y="Density") + ggtitle("") # one graph in wich 100% = new + old genes, and old and new genes are differentiated by collor, for genes in groups Mit, Mei, Mit-Mei, Mei-Pos and Post
#boxplots
ggp4 <- ggplot(data.frame(tabe), aes(x=tabe$age, y=tabe$Mitosis, color=age)) + geom_boxplot(notch = T) + labs(x="Genes' age", y="Expression during Mitosis") + theme(legend.position = "none") + annotate("text", x=1.5, y=13.5, label="p = 0.249") +ggtitle("") + scale_y_continuous(limits = c(3, 15)) # expression during mitosis for genes from classes 1 to 10 + 12
ggp5 <- ggplot(data.frame(tabe), aes(x=tabe$age, y=tabe$Meiosis, color=age)) + geom_boxplot(notch = T) + labs(x="Genes' age", y="Expression during Meiosis") + theme(legend.position = "none") + annotate("text", x=1.5, y=13.5, label="p < 0.01") +ggtitle("") + scale_y_continuous(limits = c(3, 15)) # expression during meiosis for genes in classes 1 to 10 + 12
ggp6 <- ggplot(data.frame(tabe), aes(x=tabe$age, y=tabe$Post.meiosis, color=age)) + geom_boxplot(notch = T) + labs(x="Genes' age", y="Expression during Post-Meiosis") + theme(legend.position = "none") + annotate("text", x=1.5, y=13.5, label="p < 0.01") +ggtitle("") + scale_y_continuous(limits = c(3, 15)) # expression during post-meiosis for genes in classes 1 to 10 + 12

pdf("proportion_and_expression_1to10and12.pdf", width=9, height=18)
	grid.arrange(arrangeGrob(a1, a3, nrow=1, ncol=2), arrangeGrob(g1, g2, nrow=1, ncol=2), arrangeGrob(ggp4, ggp5, ggp6, nrow=1, ncol=3))
	grid.text("Genes' age for all classes (1 to 10 + 12) toghether", just="centre",  y = unit(0.985, "npc"))
	grid.text("Genes' age for all groups toghether", just="centre",  y = unit(0.65, "npc"))
	grid.text("Genes Expression in each spermatogenesis phase by age", just="centre",  y = unit(0.31, "npc"))
	grid.text("Old genes: \n\t 6706\n New genes: \n\t 380", just="centre",  y = unit(0.65, "npc"), x=unit(0.94, "npc"), gp=gpar(fontsize=10))
dev.off() # graphs: proportion of new and old genes in classes 1 to 1 and 12; proportion of new and old genes in groups mitotic, mitotic-meiotic, meiotic, meiotic-postmeiotic and postmeiotic; expression of those genes in mitosis, meiosis and postmeiosis.

########## Getting old/new genes proportion and expression for the impossible numerical classes 14 and 15 ##########
#### gráficos de expressão das classes 14 e 15 com % de old e new e estatísticas wilcos e fisher
# doing graphs and stats
ggp7 <- ggplot(data.frame(clas.14.15),aes(x=CLASS, group=age))
c1new <- data.frame(CLASS=1:2, y=c(0.2,0.2), lab="Text", age=factor("new", levels=c("new","old")))
c1old <- data.frame(CLASS=1:2, y=c(0.2,0.2), lab="Text", age=factor("old", levels=c("new", "old")))
c1 <- ggp7 + geom_bar(aes(y=..density.., fill=age)) + labs(x="New expression classes", y="Density") + ggtitle("") + facet_grid(~age) + theme(legend.position = "none") #+ geom_text(data=c1new, label=c("32","11"), y=(0.2, 0.2), size=4, angle=45) + geom_text(data=c1old, label=c("321","231"), y=(0.2,0.2), size=4, angle=45)
c3 <- ggp7 + geom_histogram(aes(y=..count../sum(..count..), fill=age)) + labs(x="New expression classes", y="Density") + ggtitle("")

#boxplots
ggp8 <- ggplot(data.frame(clas.14.15), aes(x=clas.14.15$age, y=clas.14.15$Mitosis, color=age)) + geom_boxplot(notch = T) + labs(x="Genes' age", y="Expression during Mitosis") + theme(legend.position = "none") + annotate("text", x=1.5, y=13.5, label="p < 0.01") + scale_y_continuous(limits = c(3, 15))
ggp9 <- ggplot(data.frame(clas.14.15), aes(x=clas.14.15$age, y=clas.14.15$Meiosis, color=age)) + geom_boxplot(notch = T) + labs(x="Genes' age", y="Expression during Meiosis") + theme(legend.position = "none") + annotate("text", x=1.5, y=13.5, label="p < 0.01") + scale_y_continuous(limits = c(3, 15))
ggp10 <- ggplot(data.frame(clas.14.15), aes(x=clas.14.15$age, y=clas.14.15$Post.meiosis, color=age)) + geom_boxplot(notch = T) + labs(x="Genes' age", y="Expression during Post-Meiosis") + theme(legend.position = "none") + annotate("text", x=1.5, y=13.5, label="p < 0.01") + scale_y_continuous(limits = c(3, 15))

pdf("proportion_and_expression_14and15.pdf", width=9, height=8)
	grid.arrange(arrangeGrob(c1, c3, nrow=1, ncol=2), arrangeGrob(ggp8, ggp9, ggp10, ncol=3, nrow=1))
	grid.text("Genes' age for impossible classes 14 and 15", just="centre",  y = unit(0.96, "npc"))
	grid.text("Genes Expression in each spermatogenesis phase by age", just="centre",  y = unit(0.49, "npc"))
	grid.text("Old genes: 654\n New genes: 53", just="centre",  y = unit(0.5, "npc"), x=unit(0.9, "npc"), gp=gpar(fontsize=10))
	grid.text("32", y=unit(0.72, "npc"), x=unit(0.12, "npc"), gp=gpar(fontsize=9))
	grid.text("11", y=unit(0.65, "npc"), x=unit(0.21, "npc"), gp=gpar(fontsize=9))
	grid.text("321", y=unit(0.73, "npc"), x=unit(0.33, "npc"), gp=gpar(fontsize=9))
	grid.text("231", y=unit(0.7, "npc"), x=unit(0.42, "npc"), gp=gpar(fontsize=9))
dev.off()

############### Writing things down ################

x.axis <- c("Class 8", "Class 10", "Class 6", "Class 9", "Class 12", "Class 4", "Class 7", "Class 5", "Class 1", "Class 3", "Class 2", "Class 11", "Class 13", "Class 14", "Class 15")
pdf("expression_classes1_15.pdf", width=11, height = 10)
par(mfrow = c(3,1))
boxplot(clas.8.new$Mitosis, clas.8.old$Mitosis, clas.10.new$Mitosis, clas.10.old$Mitosis, clas.6.new$Mitosis, clas.6.old$Mitosis, clas.9.new$Mitosis, clas.9.old$Mitosis, clas.12.new$Mitosis, clas.12.old$Mitosis, clas.4.new$Mitosis, clas.4.old$Mitosis, clas.7.new$Mitosis, clas.7.old$Mitosis, clas.5.new$Mitosis, clas.5.old$Mitosis, clas.1.new$Mitosis, clas.1.old$Mitosis, clas.3.new$Mitosis, clas.3.old$Mitosis, clas.2.new$Mitosis, clas.2.old$Mitosis, clas.11.new$Mitosis, clas.11.old$Mitosis, clas.13.new$Mitosis, clas.13.old$Mitosis, clas.14.new$Mitosis, clas.14.old$Mitosis, clas.15.new$Mitosis, clas.15.old$Mitosis, col=c("lightpink", "lightblue"), main="Expression during Mitosis", xlab="Gene class and age (new genes are pink, old genes are blue)\n*** - p < 0.01 | ** - p < 0.05 | * - p < 0.1", ylab="Expression",  xaxt = "n", outline = F)
#legend(0, 4, c("*** - p < 0.01", "** - p < 0.05", "* - p < 0.1"))
axis(1, at=seq(1.5, 30, by=2), lab = x.axis)
text(x=seq(1.5, 30, by=2), y=rep(13, 15), labels=c("0.7162", "***", "0.4216", "*","0.2352","0.9956","0.1653","0.33","*","0.8528","0.7207","*","****","***","*")) #mit
# 8 10 6 9 12 4 7 5 1 3 2 11 13 14 15
boxplot(clas.8.new$Meiosis, clas.8.old$Meiosis, clas.10.new$Meiosis, clas.10.old$Meiosis, clas.6.new$Meiosis, clas.6.old$Meiosis, clas.9.new$Meiosis, clas.9.old$Meiosis, clas.12.new$Meiosis, clas.12.old$Meiosis, clas.4.new$Meiosis, clas.4.old$Meiosis, clas.7.new$Meiosis, clas.7.old$Meiosis, clas.5.new$Meiosis, clas.5.old$Meiosis, clas.1.new$Meiosis, clas.1.old$Meiosis, clas.3.new$Meiosis, clas.3.old$Meiosis, clas.2.new$Meiosis, clas.2.old$Meiosis, clas.11.new$Meiosis, clas.11.old$Meiosis, clas.13.new$Meiosis, clas.13.old$Meiosis, clas.14.new$Meiosis, clas.14.old$Meiosis, clas.15.new$Meiosis, clas.15.old$Meiosis, col=c("lightpink", "lightblue"), main="Expression during Meiosis", xlab="Gene class and age (new genes are pink, olde genes are blue)\n*** - p < 0.01 | ** - p < 0.05 | * - p < 0.1", ylab="Expression",  xaxt = "n", outline = F)
#legend(x, y, c("*** - p < 0.01", "** - p < 0.05", "* - p < 0.1"))
text(x=seq(1.5, 30, by=2), y=rep(13, 15), labels=c("0.8791", "**", "0.3512", "0.1164","0.4621","0.9956","***","0.1392","**","0.8411","*","0.2739","***","***","*")) #mei
axis(1, at=seq(1.5, 30, by=2), lab = x.axis)
boxplot(clas.8.new$Post.meiosis, clas.8.old$Post.meiosis, clas.10.new$Post.meiosis, clas.10.old$Post.meiosis, clas.6.new$Post.meiosis, clas.6.old$Post.meiosis, clas.9.new$Post.meiosis, clas.9.old$Post.meiosis, clas.12.new$Post.meiosis, clas.12.old$Post.meiosis, clas.4.new$Post.meiosis, clas.4.old$Post.meiosis, clas.7.new$Post.meiosis, clas.7.old$Post.meiosis, clas.5.new$Post.meiosis, clas.5.old$Post.meiosis, clas.1.new$Post.meiosis, clas.1.old$Post.meiosis, clas.3.new$Post.meiosis, clas.3.old$Post.meiosis, clas.2.new$Post.meiosis, clas.2.old$Post.meiosis, clas.11.new$Post.meiosis, clas.11.old$Post.meiosis, clas.13.new$Post.meiosis, clas.13.old$Post.meiosis, clas.14.new$Post.meiosis, clas.14.old$Post.meiosis, clas.15.new$Post.meiosis, clas.15.old$Post.meiosis, col=c("lightpink", "lightblue"), main="Expression during PostMeiosis", xlab="Gene class and age (new genes are pink, olde genes are blue)\n*** - p < 0.01 | ** - p < 0.05 | * - p < 0.1", ylab="Expression",  xaxt = "n", outline = F)
#legend(x, y, c("*** - p < 0.01", "** - p < 0.05", "* - p < 0.1"))
text(x=seq(1.5, 30, by=2), y=rep(13, 15), labels=c("***", "**", "0.5244", "***","0.2185","0.9956","0.7613","*","***","0.8598","0.7614","*","***","***","**")) #pos
axis(1, at=seq(1.5, 30, by=2), lab = x.axis)
dev.off()

labes = c("Class 8", "Class 10", "Class 6", "Class 9", "Class 12", "Class 4", "Class 7", "Class 5", "Class 1", "Class 3", "Class 2", "Class 11", "Class 13", "Class 14", "Class 15", "Class 16", "Class 17", "Class 18", "Class 19")

#### new graphs ####
all.news = subset(table2, table2$age == "new")
all.new = subset(all.news, all.news$CLASS == 1 | all.news$CLASS == 2 | all.news$CLASS == 3 | all.news$CLASS == 4 | all.news$CLASS == 5 | all.news$CLASS == 6 | all.news$CLASS == 7 | all.news$CLASS == 8 | all.news$CLASS == 9 | all.news$CLASS == 10 | all.news$CLASS == 11 | all.news$CLASS == 12 | all.news$CLASS == 13 | all.news$CLASS == 14 | all.news$CLASS == 15)

all.olds = subset(table2, table2$age == "old")
all.old = subset(all.olds, all.olds$CLASS == 1 | all.olds$CLASS == 2 | all.olds$CLASS == 3 | all.olds$CLASS == 4 | all.olds$CLASS == 5 | all.olds$CLASS == 6 | all.olds$CLASS == 7 | all.olds$CLASS == 8 | all.olds$CLASS == 9 | all.olds$CLASS == 10 | all.olds$CLASS == 11 | all.olds$CLASS == 12 | all.olds$CLASS == 13 | all.olds$CLASS == 14 | all.olds$CLASS == 15)

all.old$CLASS <- factor(all.old$CLASS, levels=c(8, 10, 6, 9, 12, 4, 7, 5, 1, 3, 2, 11, 13, 14, 15))
all.new$CLASS <- factor(all.new$CLASS, levels=c(8, 10, 6, 9, 12, 4, 7, 5, 1, 3, 2, 11, 13, 14, 15))

mit.new.mean =  by(all.new$Mitosis, all.new$CLASS, mean)
mei.new.mean =  by(all.new$Meiosis, all.new$CLASS, mean)
pos.new.mean =  by(all.new$Post.meiosis, all.new$CLASS, mean)

mit.old.mean =  by(all.old$Mitosis, all.old$CLASS, mean)
mei.old.mean =  by(all.old$Meiosis, all.old$CLASS, mean)
pos.old.mean =  by(all.old$Post.meiosis, all.old$CLASS, mean)

mit.new.median =  by(all.new$Mitosis, all.new$CLASS, median)
mei.new.median =  by(all.new$Meiosis, all.new$CLASS, median)
pos.new.median =  by(all.new$Post.meiosis, all.new$CLASS, median)

mit.old.median =  by(all.old$Mitosis, all.old$CLASS, median)
mei.old.median =  by(all.old$Meiosis, all.old$CLASS, median)
pos.old.median =  by(all.old$Post.meiosis, all.old$CLASS, median)

pdf("median.pdf", width = 15, height = 20)
par(mfrow = c(3,1))
plot(mit.old.median, xaxt="n", col="green",, ylim=c(3, 15), main="Expression of Old genes during spermatogenesis", ylab="Expression", xlab="Gene Class")
lines(mit.old.median, col="green")
points(mei.old.median, col="cornflowerblue")
lines(mei.old.median, col="cornflowerblue")
points(pos.old.median, col="cyan")
lines(pos.old.median, col="cyan")
axis(1, at=c(seq(1, 19)), labels=labes, las=0)
legend(12.5, 14.5, c("Mitosis", "Meiosis", "Post-Meiosis"), text.col=c("green", "cornflowerblue", "cyan"))

plot(mit.new.median, xaxt="n", col="magenta",, ylim=c(3, 15), main="Expression of New genes during spermatogenesis", ylab="Expression", xlab="Gene Class")
lines(mit.new.median, col="magenta")
points(mei.new.median, col="red")
lines(mei.new.median, col="red")
points(pos.new.median, col="plum")
lines(pos.new.median, col="plum")
axis(1, at=c(seq(1, 19)), labels=labes, las=0)
legend(12.5, 14.5, c("Mitosis", "Meiosis", "Post-Meiosis"), text.col=c("magenta", "red", "plum"))

plot(mit.old.median, xaxt="n", col="green",, ylim=c(3, 15), main="Expression of Old and New genes during spermatogenesis", ylab="Expression", xlab="Gene Class")
lines(mit.old.median, col="green")
points(mei.old.median, col="cornflowerblue")
lines(mei.old.median, col="cornflowerblue")
points(pos.old.median, col="cyan")
lines(pos.old.median, col="cyan")
points(mit.new.median, col="magenta")
lines(mit.new.median, col="magenta")
points(mei.new.median, col="red")
lines(mei.new.median, col="red")
points(pos.new.median, col="plum")
lines(pos.new.median, col="plum")
axis(1, at=c(seq(1, 19)), labels=labes, las=0)
legend(12.5, 14.5, c("Old Genes Mitosis", "Old Genes Meiosis", "Old Genes Post-Meiosis", "New Genes Mitosis", "New Genes Meiosis", "New Genes Post-Meiosis"), text.col=c("green", "cornflowerblue", "cyan", "magenta", "red", "plum"))
dev.off()


########## Expression for each class from 1 to 15 in old and new genes for each spermatogenesis phase and wilcox-mann-whitney statistics ##########
## Total expression for every and each class (from the not impossible ones)
# stats
cat("\n\n 3) Statistic for all Numerical Classes used in the boxplots of Mitosis, Meiosis and Post-Meiosis expression in the expression_classes1_15.pdf file
	\n\tClass 1:
	\t\t Expression during Mitosis versus age:")
print(wilcox.test(clas.1$Mitosis~clas.1$age))
cat("\t\t\tMedian expression of new genes during Mitosis:")
print(median(subset(clas.1$Mitosis, clas.1$age == "new")))
cat("\t\t\tMedian expression of old genes during Mitosis:")
print(median(subset(clas.1$Mitosis, clas.1$age == "old")))
cat("\t\t Expression during Meiosis versus age:")
print(wilcox.test(clas.1$Meiosis~clas.1$age))
cat("\t\t\tMedian expression of new genes during Meiosis:")
print(median(subset(clas.1$Meiosis, clas.1$age == "new")))
cat("\t\t\tMedian expression of old genes during Meiosis:")
print(median(subset(clas.1$Meiosis, clas.1$age == "old")))
cat("\t\t Expression during Post-Meiosis versus age:")
print(wilcox.test(clas.1$Post.meiosis~clas.1$age))
cat("\t\t\tMedian expression of new genes during Post-Meiosis:")
print(median(subset(clas.1$Post.meiosis, clas.1$age == "new")))
cat("\t\t\tMedian expression of old genes during Post-Meiosis:")
print(median(subset(clas.1$Post.meiosis, clas.1$age == "old")))
cat("\n\t Class 2:
	\n\t\t Expression during Mitosis versus age:")
print(wilcox.test(clas.2$Mitosis~clas.2$age))
cat("\t\t\tMedian expression of new genes during Mitosis:")
print(median(subset(clas.2$Mitosis, clas.2$age == "new")))
cat("\t\t\tMedian expression of old genes during Mitosis:")
print(median(subset(clas.2$Mitosis, clas.2$age == "old")))
cat("\t\t Expression during Meiosis versus age:")
print(wilcox.test(clas.2$Meiosis~clas.2$age))
cat("\t\t\tMedian expression of new genes during Meiosis:")
print(median(subset(clas.2$Meiosis, clas.2$age == "new")))
cat("\t\t\tMedian expression of old genes during Meiosis:")
print(median(subset(clas.2$Meiosis, clas.2$age == "old")))
cat("\t\t Expression during Post-Meiosis versus age:")
print(wilcox.test(clas.2$Post.meiosis~clas.2$age))
cat("\t\t\tMedian expression of new genes during Post-Meiosis:")
print(median(subset(clas.2$Post.meiosis, clas.2$age == "new")))
cat("\t\t\tMedian expression of old genes during Post-Meiosis:")
print(median(subset(clas.2$Post.meiosis, clas.2$age == "old")))
cat("\n\t Class 3:
	\n\t\t Expression during Mitosis versus age:")
print(wilcox.test(clas.3$Mitosis~clas.3$age))
cat("\t\t\tMedian expression of new genes during Mitosis:")
print(median(subset(clas.3$Mitosis, clas.3$age == "new")))
cat("\t\t\tMedian expression of old genes during Mitosis:")
print(median(subset(clas.3$Mitosis, clas.3$age == "old")))
cat("\t\t Expression during Meiosis versus age:")
print(wilcox.test(clas.3$Meiosis~clas.3$age))
cat("\t\t\tMedian expression of new genes during Meiosis:")
print(median(subset(clas.3$Meiosis, clas.3$age == "new")))
cat("\t\t\tMedian expression of old genes during Meiosis:")
print(median(subset(clas.3$Meiosis, clas.3$age == "old")))
cat("\t\t Expression during Post-Meiosis versus age:")
print(wilcox.test(clas.3$Post.meiosis~clas.3$age))
cat("\t\t\tMedian expression of new genes during Post-Meiosis:")
print(median(subset(clas.3$Post.meiosis, clas.3$age == "new")))
cat("\t\t\tMedian expression of old genes during Post-Meiosis:")
print(median(subset(clas.3$Post.meiosis, clas.3$age == "old")))
cat("\n\t Class 4:
	\n\t\t Expression during Mitosis versus age:")
print(wilcox.test(clas.4$Mitosis~clas.4$age))
cat("\t\t\tMedian expression of new genes during Mitosis:")
print(median(subset(clas.4$Mitosis, clas.4$age == "new")))
cat("\t\t\tMedian expression of old genes during Mitosis:")
print(median(subset(clas.4$Mitosis, clas.4$age == "old")))
cat("\t\t Expression during Meiosis versus age:")
print(wilcox.test(clas.4$Meiosis~clas.4$age))
cat("\t\t\tMedian expression of new genes during Meiosis:")
print(median(subset(clas.4$Meiosis, clas.4$age == "new")))
cat("\t\t\tMedian expression of old genes during Meiosis:")
print(median(subset(clas.4$Meiosis, clas.4$age == "old")))
cat("\t\t Expression during Post-Meiosis versus age:")
print(wilcox.test(clas.4$Post.meiosis~clas.4$age))
cat("\t\t\tMedian expression of new genes during Post-Meiosis:")
print(median(subset(clas.4$Post.meiosis, clas.4$age == "new")))
cat("\t\t\tMedian expression of old genes during Post-Meiosis:")
print(median(subset(clas.4$Post.meiosis, clas.4$age == "old")))
cat("\n\t Class 5:
	\n\t\t Expression during Mitosis versus age:")
print(wilcox.test(clas.5$Mitosis~clas.5$age))
cat("\t\t\tMedian expression of new genes during Mitosis:")
print(median(subset(clas.5$Mitosis, clas.5$age == "new")))
cat("\t\t\tMedian expression of old genes during Mitosis:")
print(median(subset(clas.5$Mitosis, clas.5$age == "old")))
cat("\t\t Expression during Meiosis versus age:")
print(wilcox.test(clas.5$Meiosis~clas.5$age))
cat("\t\t\tMedian expression of new genes during Meiosis:")
print(median(subset(clas.5$Meiosis, clas.5$age == "new")))
cat("\t\t\tMedian expression of old genes during Meiosis:")
print(median(subset(clas.5$Meiosis, clas.5$age == "old")))
cat("\t\t Expression during Post-Meiosis versus age:")
print(wilcox.test(clas.5$Post.meiosis~clas.5$age))
cat("\t\t\tMedian expression of new genes during Post-Meiosis:")
print(median(subset(clas.5$Post.meiosis, clas.5$age == "new")))
cat("\t\t\tMedian expression of old genes during Post-Meiosis:")
print(median(subset(clas.5$Post.meiosis, clas.5$age == "old")))
cat("\n\t Class 6:
	\n\t\t Expression during Mitosis versus age:")
print(wilcox.test(clas.6$Mitosis~clas.6$age))
cat("\t\t\tMedian expression of new genes during Mitosis:")
print(median(subset(clas.6$Mitosis, clas.6$age == "new")))
cat("\t\t\tMedian expression of old genes during Mitosis:")
print(median(subset(clas.6$Mitosis, clas.6$age == "old")))
cat("\t\t Expression during Meiosis versus age:")
print(wilcox.test(clas.6$Meiosis~clas.6$age))
cat("\t\t\tMedian expression of new genes during Meiosis:")
print(median(subset(clas.6$Meiosis, clas.6$age == "new")))
cat("\t\t\tMedian expression of old genes during Mitosis:")
print(median(subset(clas.6$Meiosis, clas.6$age == "old")))
cat("\t\t Expression during Post-Meiosis versus age:")
print(wilcox.test(clas.6$Post.meiosis~clas.6$age))
cat("\t\t\tMedian expression of new genes during Post-Meiosis:")
print(median(subset(clas.6$Post.meiosis, clas.6$age == "new")))
cat("\t\t\tMedian expression of old genes during Post-Meiosis:")
print(median(subset(clas.6$Post.meiosis, clas.6$age == "old")))
cat("\n\t Class 7:
	\n\t\t Expression during Mitosis versus age:")
print(wilcox.test(clas.7$Mitosis~clas.7$age))
cat("\t\t\tMedian expression of new genes during Mitosis:")
print(median(subset(clas.7$Mitosis, clas.7$age == "new")))
cat("\t\t\tMedian expression of old genes during Mitosis:")
print(median(subset(clas.7$Mitosis, clas.7$age == "old")))
cat("\t\t Expression during Meiosis versus age:")
print(wilcox.test(clas.7$Meiosis~clas.7$age))
cat("\t\t\tMedian expression of new genes during Meiosis:")
print(median(subset(clas.7$Meiosis, clas.7$age == "new")))
cat("\t\t\tMedian expression of old genes during Meiosis:")
print(median(subset(clas.7$Meiosis, clas.7$age == "old")))
cat("\t\t Expression during Post-Meiosis versus age:")
print(wilcox.test(clas.7$Post.meiosis~clas.7$age))
cat("\t\t\tMedian expression of new genes during Post-Meiosis:")
print(median(subset(clas.7$Post.meiosis, clas.7$age == "new")))
cat("\t\t\tMedian expression of old genes during Post-Meiosis:")
print(median(subset(clas.7$Post.meiosis, clas.7$age == "old")))
cat("\n\t Class 8:
	\n\t\t Expression during Mitosis versus age:")
print(wilcox.test(clas.8$Mitosis~clas.8$age))
cat("\t\t\tMedian expression of new genes during Mitosis:")
print(median(subset(clas.8$Mitosis, clas.8$age == "new")))
cat("\t\t\tMedian expression of old genes during Mitosis:")
print(median(subset(clas.8$Mitosis, clas.8$age == "old")))
cat("\t\t Expression during Meiosis versus age:")
print(wilcox.test(clas.8$Meiosis~clas.8$age))
cat("\t\t\tMedian expression of new genes during Meiosis:")
print(median(subset(clas.8$Meiosis, clas.8$age == "new")))
cat("\t\t\tMedian expression of old genes during Meiosis:")
print(median(subset(clas.8$Meiosis, clas.8$age == "old")))
cat("\t\t Expression during Post-Meiosis versus age:")
print(wilcox.test(clas.8$Post.meiosis~clas.8$age))
cat("\t\t\tMedian expression of new genes during Post-Meiosis:")
print(median(subset(clas.8$Post.meiosis, clas.8$age == "new")))
cat("\t\t\tMedian expression of old genes during Post-Meiosis:")
print(median(subset(clas.8$Post.meiosis, clas.8$age == "old")))
cat("\n\t Class 9:
	\n\t\t Expression during Mitosis versus age:")
print(wilcox.test(clas.9$Mitosis~clas.9$age))
cat("\t\t\tMedian expression of new genes during Mitosis:")
print(median(subset(clas.9$Mitosis, clas.9$age == "new")))
cat("\t\t\tMedian expression of old genes during Mitosis:")
print(median(subset(clas.9$Mitosis, clas.9$age == "old")))
cat("\t\t Expression during Meiosis versus age:")
print(wilcox.test(clas.9$Meiosis~clas.9$age))
cat("\t\t\tMedian expression of new genes during Meiosis:")
print(median(subset(clas.9$Meiosis, clas.9$age == "new")))
cat("\t\t\tMedian expression of old genes during Meiosis:")
print(median(subset(clas.9$Meiosis, clas.9$age == "old")))
cat("\t\t Expression during Post-Meiosis versus age:")
print(wilcox.test(clas.9$Post.meiosis~clas.9$age))
cat("\t\t\tMedian expression of new genes during Post-Meiosis:")
print(median(subset(clas.9$Post.meiosis, clas.9$age == "new")))
cat("\t\t\tMedian expression of old genes during Post-Meiosis:")
print(median(subset(clas.9$Post.meiosis, clas.9$age == "old")))
cat("\n\t Class 10:
	\n\t\t Expression during Mitosis versus age:")
print(wilcox.test(clas.10$Mitosis~clas.10$age))
cat("\t\t\tMedian expression of new genes during Mitosis:")
print(median(subset(clas.10$Mitosis, clas.10$age == "new")))
cat("\t\t\tMedian expression of old genes during Mitosis:")
print(median(subset(clas.10$Mitosis, clas.10$age == "old")))
cat("\t\t Expression during Meiosis versus age:")
print(wilcox.test(clas.10$Meiosis~clas.10$age))
cat("\t\t\tMedian expression of new genes during Meiosis:")
print(median(subset(clas.10$Meiosis, clas.10$age == "new")))
cat("\t\t\tMedian expression of old genes during Meiosis:")
print(median(subset(clas.10$Meiosis, clas.10$age == "old")))
cat("\t\t Expression during Post-Meiosis versus age:")
print(wilcox.test(clas.10$Post.meiosis~clas.10$age))
cat("\t\t\tMedian expression of new genes during Post-Meiosis:")
print(median(subset(clas.10$Post.meiosis, clas.10$age == "new")))
cat("\t\t\tMedian expression of old genes during Post-Meiosis:")
print(median(subset(clas.10$Post.meiosis, clas.10$age == "old")))
cat("\n\t Class 11:
	\n\t\t Expression during Mitosis versus age:")
print(wilcox.test(clas.11$Mitosis~clas.11$age))
cat("\t\t\tMedian expression of new genes during Mitosis:")
print(median(subset(clas.11$Mitosis, clas.11$age == "new")))
cat("\t\t\tMedian expression of old genes during Mitosis:")
print(median(subset(clas.11$Mitosis, clas.11$age == "old")))
cat("\t\t Expression during Meiosis versus age:")
print(wilcox.test(clas.11$Meiosis~clas.11$age))
cat("\t\t\tMedian expression of new genes during Meiosis:")
print(median(subset(clas.11$Meiosis, clas.11$age == "new")))
cat("\t\t\tMedian expression of old genes during Meiosis:")
print(median(subset(clas.11$Meiosis, clas.11$age == "old")))
cat("\t\t Expression during Post-Meiosis versus age:")
print(wilcox.test(clas.11$Post.meiosis~clas.11$age))
cat("\t\t\tMedian expression of new genes during Post-Meiosis:")
print(median(subset(clas.11$Post.meiosis, clas.11$age == "new")))
cat("\t\t\tMedian expression of old genes during Post-Meiosis:")
print(median(subset(clas.11$Post.meiosis, clas.11$age == "old")))
cat("\n\t Class 12:
	\n\t\t Expression during Mitosis versus age:")
print(wilcox.test(clas.12$Mitosis~clas.12$age))
cat("\t\t\tMedian expression of new genes during Mitosis:")
print(median(subset(clas.12$Mitosis, clas.12$age == "new")))
cat("\t\t\tMedian expression of old genes during Mitosis:")
print(median(subset(clas.12$Mitosis, clas.12$age == "old")))
cat("\t\t Expression during Meiosis versus age:")
print(wilcox.test(clas.12$Meiosis~clas.12$age))
cat("\t\t\tMedian expression of new genes during Meiosis:")
print(median(subset(clas.12$Meiosis, clas.12$age == "new")))
cat("\t\t\tMedian expression of old genes during Meiosis:")
print(median(subset(clas.12$Meiosis, clas.12$age == "old")))
cat("\t\t Expression during Post-Meiosis versus age:")
print(wilcox.test(clas.12$Post.meiosis~clas.12$age))
cat("\t\t\tMedian expression of new genes during Post-Meiosis:")
print(median(subset(clas.12$Post.meiosis, clas.12$age == "new")))
cat("\t\t\tMedian expression of old genes during Post-Meiosis:")
print(median(subset(clas.12$Post.meiosis, clas.12$age == "old")))
cat("\n\t Class 13:
	\n\t\t Expression during Mitosis versus age:")
print(wilcox.test(clas.13$Mitosis~clas.13$age))
cat("\t\t\tMedian expression of new genes during Mitosis:")
print(median(subset(clas.13$Mitosis, clas.13$age == "new")))
cat("\t\t\tMedian expression of old genes during Mitosis:")
print(median(subset(clas.13$Mitosis, clas.13$age == "old")))
cat("\t\t Expression during Meiosis versus age:")
print(wilcox.test(clas.13$Meiosis~clas.13$age))
cat("\t\t\tMedian expression of new genes during Meiosis:")
print(median(subset(clas.13$Meiosis, clas.13$age == "new")))
cat("\t\t\tMedian expression of old genes during Meiosis:")
print(median(subset(clas.13$Meiosis, clas.13$age == "old")))
cat("\t\t Expression during Post-Meiosis versus age:")
print(wilcox.test(clas.13$Post.meiosis~clas.13$age))
cat("\t\t\tMedian expression of new genes during Post-Meiosis:")
print(median(subset(clas.13$Post.meiosis, clas.13$age == "new")))
cat("\t\t\tMedian expression of old genes during Post-Meiosis:")
print(median(subset(clas.13$Post.meiosis, clas.13$age == "old")))
cat("\n\t Class 14:
	\n\t\t Expression during Mitosis versus age:")
print(wilcox.test(clas.14$Mitosis~clas.14$age))
cat("\t\t\tMedian expression of new genes during Mitosis:")
print(median(subset(clas.14$Mitosis, clas.14$age == "new")))
cat("\t\t\tMedian expression of old genes during Mitosis:")
print(median(subset(clas.14$Mitosis, clas.14$age == "old")))
cat("\t\t Expression during Meiosis versus age:")
print(wilcox.test(clas.14$Meiosis~clas.14$age))
cat("\t\t\tMedian expression of new genes during Meiosis:")
print(median(subset(clas.14$Meiosis, clas.14$age == "new")))
cat("\t\t\tMedian expression of old genes during Meiosis:")
print(median(subset(clas.14$Meiosis, clas.14$age == "old")))
cat("\t\t Expression during Post-Meiosis versus age:")
print(wilcox.test(clas.14$Post.meiosis~clas.14$age))
cat("\t\t\tMedian expression of new genes during Post-Meiosis:")
print(median(subset(clas.14$Post.meiosis, clas.14$age == "new")))
cat("\t\t\tMedian expression of old genes during Post-Meiosis:")
print(median(subset(clas.14$Post.meiosis, clas.14$age == "old")))
cat("\n\t Class 15:
	\n\t\t Expression during Mitosis versus age:")
print(wilcox.test(clas.15$Mitosis~clas.15$age))
cat("\t\t\tMedian expression of new genes during Mitosis:")
print(median(subset(clas.15$Mitosis, clas.15$age == "new")))
cat("\t\t\tMedian expression of old genes during Mitosis:")
print(median(subset(clas.15$Mitosis, clas.15$age == "old")))
cat("\t\t Expression during Meiosis versus age:")
print(wilcox.test(clas.15$Meiosis~clas.15$age))
cat("\t\t\tMedian expression of new genes during Meiosis:")
print(median(subset(clas.15$Meiosis, clas.15$age == "new")))
cat("\t\t\tMedian expression of old genes during Meiosis:")
print(median(subset(clas.15$Meiosis, clas.15$age == "old")))
cat("\t\t Expression during Post-Meiosis versus age:")
print(wilcox.test(clas.15$Post.meiosis~clas.15$age))
cat("\t\t\tMedian expression of new genes during Post-Meiosis:")
print(median(subset(clas.15$Post.meiosis, clas.15$age == "new")))
cat("\t\t\tMedian expression of old genes during Post-Meiosis:")
print(median(subset(clas.15$Post.meiosis, clas.15$age == "old")))
# 8 10 6 9 12 4 7 5 1 3 2 11 13 14 15

cat("\t Conclusions:
	\t\t During Mitosis there is differential expression of new and old genes in classes: 1, 9, 10, 11, 13, 14 and 15.
	\t\t From those, classes 1 has higher new genes expression.
	\n\t\t During Meiosis there is differential expression of new and old genes in classes: 1, 2, 7, 10, 11, 13, 14 and 15.
	\t\t From those, classes 1 and 7 have higher new gene expression.
	\n\t\t During Post-Meiosis there is differential expression of new and old genes in classes 1, 5, 8, 9, 10, 11, 13, 14 and 15.
	From those, classes 1 and 5 have higher new gene expression.\n")

########## Resume of comparison between classes to see if post-meiosis gets in the way of new genes (decreases the % of new genes in the classes where its expression is high) ##########
cat("\n\n4) Resume of comparison between classes to see if post-meiosis gets in the way of new genes (decreases the % of new genes in the classes where its expression is high). This analysis show that although post-meiotic classes do have a decrease in the percentage of new genes, they still have more new genes then the mitotic phase, which means the decrease is not sufficient to vanish the effect of new genes increaase that occurs during the meiosis.\n
	\t Table of absolute number of genes in each Class and age:")
print(table(tabe2$age, tabe2$CLASS))
cat("Table of proportion of genes in each Class and age:")
print(prop.table(table(tabe2$age, tabe2$CLASS), 2)*100)

cat("\t a) Classes 1 and 4 have different proportions of old and new genes, class 4 has more new genes:")
print(fisher.test(classes1.4$age, classes1.4$CLASS))
cat("\t b) Classes 3 and 8 do not have a different proportion of old and new genes:")
print(fisher.test(classes3.8$age, classes3.8$CLASS))
cat("\t c) Classes 3 and 6 do not have a different proportion of old and new genes:")
print(fisher.test(classes3.6$age, classes3.6$CLASS))
cat("\t d) Classes 3 and 10 do not have a different proportion of old and new genes:")
print(fisher.test(classes3.10$age, classes3.10$CLASS))
cat("\t e) Classes 8 and 5 have different proportions of old and new genes, class 5 has more new genes:")
print(fisher.test(classes8.5$age, classes8.5$CLASS))
cat("\t f) Classes 8 and 7 have different proportions of old and new genes, class 7 has more new genes:")
print(fisher.test(classes8.7$age, classes8.7$CLASS))
cat("\t g) Classes 8 and 12 have different proportions of old and new genes, class 12 has more new genes:")
print(fisher.test(classes8.12$age, classes8.12$CLASS))
cat("\t h) Classes 8 and 4 have different proportions of old and new genes, class 4 has more new genes:")
print(fisher.test(classes8.4$age, classes8.4$CLASS))
cat("\t i) Classes 9 and 5 have different proportions of old and new genes, class 5 has more new genes:")
print(fisher.test(classes9.5$age, classes9.5$CLASS))
cat("\t j) Classes 9 and 7 have different proportions of old and new genes, class 7 has more new genes:")
print(fisher.test(classes9.7$age, classes9.7$CLASS))
cat("\t k) Classes 9 and 12 have different proportions of old and new genes, class 12 has more new genes:")
print(fisher.test(classes9.12$age, classes9.12$CLASS))
cat("\t l) Classes 9 and 4 have different proportions of old and new genes, class 4 has more new genes:")
print(fisher.test(classes9.4$age, classes9.4$CLASS))
cat("\t m) Classes 1 and 8 have different proportions of old and new genes, class 1 has more new genes:")
print(fisher.test(classes1.8$age, classes1.8$CLASS))
cat("\t n) Classes 1 and 9 have different proportions of old and new genes, class 1 has more new genes:")
print(fisher.test(classes1.9$age, classes1.9$CLASS))
cat("\t o) Classes 1 and 10 have different proportions of old and new genes, class 1 has more new genes:")
print(fisher.test(classes1.10$age, classes1.10$CLASS))
cat("\t p) Classes 1 and 6 have different proportions of old and new genes, class 1 has more new genes:")
print(fisher.test(classes1.6$age, classes1.6$CLASS))

cat("\n\nFor information on X vs Atossomes on classes and groups numbers, proportions, and distribuition across ages (old vs new) please contact me for the XAanalysis.R and the table number_and_proportions_XandA.xls.\n\n")

cat("\n Conclusion about graphics median.pdf and expression_classes1_15.pdf: \n
    1) the median.pdf shows that for classes 3, 2 and 11 (post-meiotic), 13 (equal), 14 and 15 (impossibles) have lower expression during post-meiosis, showing evidence that genes that are up-regulated in post-meiosis tend to be lower expressed in general. This is evidence in favor of how hard is to detect post-meiotic transcription. Those genes that are post-meioticly transcribed (more RNA in post meiosis than other phases), they are lower transcribed and they may not pass the detection threshold as in situ hybridization or radioactive uridine. That is why maybe in the past, they did not detect those class of genes.
	This is also evidence that equal expressed genes are enriched with genes that have no expression at all in spermatogenesis. Therefore their expression is low and not differential among phases. This is important evidence to exclude the equal class from our analyses as they probably have no matter in spermatogenesis.\n
    2) from the expressionclasses1_15.pdf and the proportion graphics (proportion_and_expression_1to10and12.pdf and proportion_and_expression_1to10and12and13.pdf) we can see that the classes 7, 5 and 1 have highest expression of new genes. \n
    3) from the same graphics we can see that:
    \t a) no pattern of general lower new genes expression is seen;
    \t b) in other words, there is no fixed pattern for genes' expression. Sometimes new genes have higher expressin, sometimes they have lower expression and sometimes they are equally expressed then old genes;
    \t c) for instance, in some classes new genes have high mitotic expression, and in other classes new genes have low mitotic expression, when if this expression was due solely to chromatin remodeling we would have all new genes having the same pattern in all 3 phases, which does not occur. This means that there is no general rule for expression characteristics according to genes age.\n
    This shows that probably the changes in new and old genes expression is not due solely to chromatin remodeling, but there probably also is a selection and functional factor involved.\n
	4) From the comparison between classes to see if post-meiosis gets in the way of new genes (decreases the % of new genes in the classes where its expression is high):
	\t a) The analysis of 1 x 4 shows that the Post-Meiosis higher expression does decrease the number of new genes from 12-15% to 9%.
	\t b) Post-meioses alone does not increases the number of new genes 3 x (8 or 6 or 10) is not significant different.
	\t c) Meiosis is the most relevant phase for account to the increase of new genes as 8 or 9 x (5 or 7 or 12 or 4).  
	\t d) However, the presence of higher pos-meiosis does not remove completely the effect of meiosis 1 x (8 or 9 or 10 or 6) 9% x 1-3%. There are still more new genes.\n ")

sink()
