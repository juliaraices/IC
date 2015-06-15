## minha segunda tentativa de fazer o R fazer tudo por mim, enquanto eu tento obter:
## Porcentagens das proporcoes de genes novos e antigos
##	- Comparacao das porcentagens do grupo Meiotic-PostMeiotic (classe 5)  com o grupo Mitotic (qui-quadrado ou fisher)
##	- entender o nao aumeioticto da proporcao do grupo PostMeiotic
##	- separar cada classe do grupo PostMeiotic para ver como elas se comportam em relação a genes novos e antigos

all <- read.table("input/table_allClasses.txt", header=T)

table2 <- subset(all, all$XorAV =='A')
tabe <- subset(table2, table2$New_Class=="Meiotic" | table2$New_Class=="PostMeiotic" | table2$New_Class == "Mitotic" | table2$New_Class =="Meiotic-PostMeiotic" | table2$New_Class =="Mitotic-Meiotic")

news <- subset(tabe, tabe$age =="new")
old <- subset(tabe, tabe$age =="old")
tabe$CLASS <- factor(tabe$CLASS, levels=c(8, 10, 6, 9, 12, 4, 7, 5, 1, 3, 2, 13, 11))
tabe$age <- factor(tabe$age, levels=c("new", "old"))
tabe$New_Class <- factor(tabe$New_Class, levels=c("Mitotic", "Mitotic-Meiotic", "Meiotic", "Meiotic-PostMeiotic", "PostMeiotic"))

#table$bias <- factor(table$bias, levels=c("male", "female", "unbiased"))
#table$branch <- factor(table$branch, levels=c(0, 1, 2, 3, 4, 5, 6))

mei.pos.mit.meipos <-subset(tabe, New_Class== 'Mitotic' | New_Class =='Meiotic-PostMeiotic' | New_Class== 'Meiotic' |  New_Class =='PostMeiotic')
new.mei.pos.mit.meipos <- subset(mei.pos.mit.meipos, age =="new")
old.mei.pos.mit.meipos <- subset(mei.pos.mit.meipos, age =="old")

mei.pos.mit <-subset(tabe, New_Class== 'Mitotic' | New_Class== 'Meiotic' |  New_Class =='PostMeiotic')
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

sink("output/chi-quadrados_A.txt")
tabela <- table(mei.pos.mit$New_Class, mei.pos.mit$age)
tabela2 <- table(mei.pos.mit.meipos$New_Class, mei.pos.mit.meipos$age)
tabela
prop.table(tabela, 1) * 100
tabela2
prop.table(tabela2, 1) * 100

fisher.test(mit.mei$age, mit.mei$New_Class) # gente!!!!! deu certoooo!!!!!
(tabela <- table(mit.mei$New_Class, mit.mei$age))
prop.table(tabela, 1) * 100
#significativo para autossomos+X
#significativo para autossomos
fisher.test(mit.pos$age, mit.pos$New_Class) # gente!!!!! deu certoooo!!!!!
(tabela <- table(mit.pos$New_Class, mit.pos$age))
prop.table(tabela, 1) * 100
#significativo para autossomos+X
#significativo para autossomos
fisher.test(mit.meipos$age, mit.meipos$New_Class) # gente!!!!! deu certoooo!!!!!
(tabela <- table(mit.meipos$New_Class, mit.meipos$age))
prop.table(tabela, 1) * 100
#significativo para autossomos+X
#significativo para autossomos
fisher.test(mei.meipos$age, mei.meipos$New_Class) # gente!!!!! deu certoooo!!!!!
(tabela <- table(mei.meipos$New_Class, mei.meipos$age))
prop.table(tabela, 1) * 100
#nao-significativo para autossomos+X
#nao-significatico para autossomos
fisher.test(mei.pos$age, mei.pos$New_Class) # gente!!!!! deu certoooo!!!!!
(tabela <- table(mei.pos$New_Class, mei.pos$age))
prop.table(tabela, 1) * 100
#significativo para autossomos+X
#significativo para autossomos
fisher.test(meipos.pos$age, meipos.pos$New_Class) # gente!!!!! deu certoooo!!!!!
(tabela <- table(meipos.pos$New_Class, meipos.pos$age))
prop.table(tabela, 1) * 100
#significativo para autossomos+X
#significativo para autossomos

cat("Expression mesuares")
cat("All genes:")
wilcox.test(tabe$Mitosis~tabe$age)
cat("means new and old:")
mean(news$Mitosis)
mean(old$Mitosis)
wilcox.test(tabe$Meiosis~tabe$age)
mean(news$Meiosis)
mean(old$Meiosis)
wilcox.test(tabe$Post.meiosis~tabe$age)
mean(news$Post.meiosis)
mean(old$Post.meiosis)

cat("Only classes Mitotic, Meiotic and PostMeiotic")
wilcox.test(mei.pos.mit$Mitosis~mei.pos.mit$age)
mean(new.mei.pos.mit$Mitosis)
mean(old.mei.pos.mit$Mitosis)
wilcox.test(mei.pos.mit$Meiosis~mei.pos.mit$age)
mean(new.mei.pos.mit$Meiosis)
mean(old.mei.pos.mit$Meiosis)
wilcox.test(mei.pos.mit$Post.meiosis~mei.pos.mit$age)
mean(new.mei.pos.mit$Post.meiosis)
mean(old.mei.pos.mit$Post.meiosis)

cat("Only classes Mitotic, Meiotic, Meiotic-PostMeiotic and PostMeiotic")
wilcox.test(mei.pos.mit.meipos$Mitosis~mei.pos.mit.meipos$age)
mean(new.mei.pos.mit.meipos$Mitosis)
mean(new.mei.pos.mit.meipos$Mitosis)
wilcox.test(mei.pos.mit.meipos$Meiosis~mei.pos.mit.meipos$age)
mean(new.mei.pos.mit.meipos$Meiosis)
mean(old.mei.pos.mit.meipos$Meiosis)
wilcox.test(mei.pos.mit.meipos$Post.meiosis~mei.pos.mit.meipos$age)
mean(new.mei.pos.mit.meipos$Post.meiosis)
mean(old.mei.pos.mit.meipos$Post.meiosis)
sink()


library(ggplot2)

ggp2 <- ggplot(data.frame(tabe),aes(x=tabe$New_Class, group=tabe$age, color=tabe$age))
pdf("graphs/genes_interestclasses.pdf", width=9, height=8)
ggp2 + geom_histogram(aes(fill=tabe$age)) + labs(x="New expression classes", y="Number of genes") + ggtitle("Genes in each Expression Class during Spermatogenesis")
dev.off()

pdf("graphs/genes_interestclasses_proportion.pdf", width=9, height=8)
ggp2 + geom_histogram(aes(y=..count../sum(..count..), fill=tabe$age)) + labs(x="New expression classes", y="Frequency") + ggtitle("Frequency of each Expression Class during Spermatogenesis")
dev.off()


ggp9 <- ggplot(data.frame(tabe),aes(x=tabe$CLASS, group=tabe$age, color=tabe$age))
pdf("graphs/genes_interestclasses.pdf", width=9, height=8)
ggp9 + geom_histogram(aes(fill=tabe$age)) + labs(x="New expression classes", y="Number of genes") + ggtitle("Genes in each Expression Class during Spermatogenesis")
dev.off()

pdf("graphs/genes_interestclasses_proportion.pdf", width=9, height=8)
ggp9 + geom_histogram(aes(y=..count../sum(..count..), fill=tabe$age)) + labs(x="New expression classes", y="Frequency") + ggtitle("Frequency of each Expression Class during Spermatogenesis")
dev.off()


ggp2 <- ggplot(data.frame(tabe),aes(x=tabe$CLASS, group=tabe$age, color=tabe$age))
pdf("graphs/genes_numericclasses.pdf", width=9, height=8)
ggp2 + geom_histogram(aes(fill=tabe$age)) + labs(x="New expression classes", y="Number of genes") + ggtitle("Genes in each Expression Class during Spermatogenesis")
dev.off()

pdf("graphs/genes_numericclasses_proportion.pdf", width=9, height=8)
ggp2 + geom_histogram(aes(y=..count../sum(..count..), fill=tabe$age)) + labs(x="New expression classes", y="Frequency") + ggtitle("Frequency of each Expression Class during Spermatogenesis")
dev.off()


#boxplots
w3 <- wilcox.test(tabe$Mitosis~tabe$age)
ggp3 <- ggplot(data.frame(tabe), aes(x=tabe$age, y=tabe$Mitosis, color=age)) + geom_boxplot(notch = T) + labs(x="Genes' age", y="Expression during Mitosis") + theme(legend.position = "none") + annotate("text", x=1.5, y=13.5, label=round(w3[[3]], 4)) + ggtitle("") + theme(legend.position = "none")


w4 <- wilcox.test(tabe$Meiosis~tabe$age)
ggp4 <- ggplot(data.frame(tabe), aes(x=tabe$age, y=tabe$Meiosis, color=age)) + geom_boxplot(notch = T) + labs(x="Genes' age", y="Expression during Meiosis") + theme(legend.position = "none") + annotate("text", x=1.5, y=13.5, label=round(w4[[3]], 4)) + ggtitle("") + theme(legend.position = "none") 

w5 <- wilcox.test(tabe$Post.meiosis~tabe$age)
ggp5 <- ggplot(data.frame(tabe), aes(x=tabe$age, y=tabe$Meiosis, color=age)) + geom_boxplot(notch = T) + labs(x="Genes' age", y="Expression during Post-Meiosis") + theme(legend.position = "none") + annotate("text", x=1.5, y=13.5, label=round(w5[[3]], 4)) + ggtitle("") + theme(legend.position = "none") 


library(gridExtra)
pdf("graphs/expression_possible_interest_classes.pdf", width=9, height=8)
grid.arrange(ggp3, ggp4, ggp5, ncol=3) 
grid.text("Genes Expression in each spermatogenesis phase by age", just="centre", y=unit(0.95, "npc"), gp = gpar(fontsize = 18, fontface = "bold")) 
dev.off()

w6 <- wilcox.test(mei.pos.mit$Mitosis~mei.pos.mit$age)
ggp6 <- ggplot(data.frame(mei.pos.mit), aes(x=mei.pos.mit$age, y=mei.pos.mit$Mitosis, color=age)) + geom_boxplot(notch = T) + labs(x="Genes' age", y="Expression during Mitosis") + theme(legend.position = "none") + annotate("text", x=1.5, y=13.5, label=round(w6[[3]], 4)) + ggtitle("") + theme(legend.position = "none") 


w7 <- wilcox.test(mei.pos.mit$Meiosis~mei.pos.mit$age)
ggp7 <- ggplot(data.frame(mei.pos.mit), aes(x=mei.pos.mit$age, y=mei.pos.mit$Meiosis, color=age)) + geom_boxplot(notch = T) + labs(x="Genes' age", y="Expression during Meiosis") + theme(legend.position = "none") + annotate("text", x=1.5, y=13.5, label=round(w7[[3]], 4)) + ggtitle("") + theme(legend.position = "none") 

w8 <- wilcox.test(mei.pos.mit$Post.meiosis~mei.pos.mit$age)
ggp8 <- ggplot(data.frame(mei.pos.mit), aes(x=mei.pos.mit$age, y=mei.pos.mit$Meiosis, color=age)) + geom_boxplot(notch = T) + labs(x="Genes' age", y="Expression during Post-Meiosis") + theme(legend.position = "none") + annotate("text", x=1.5, y=13.5, label=round(w8[[3]],4)) + ggtitle("") + theme(legend.position = "none") 


pdf("graphs/expression_interest_classes.pdf", width=9, height=8)
grid.arrange(ggp6, ggp7, ggp8, ncol=3)
grid.text("Genes Expression in each spermatogenesis phase by age", just="centre", y=unit(0.95, "npc"), gp = gpar(fontsize = 18, fontface="bold"))
dev.off()

w19 <- wilcox.test(table2$Mitosis~table2$age)
ggp19 <- ggplot(data.frame(table2), aes(x=table2$age, y=table2$Mitosis, color=table2$age)) + geom_boxplot(notch = T, aes(color=table2$age)) + labs(x="Genes' age", y="Expression during Mitosis") + annotate("text", x=1.5, y=13.5, label= round(w19[[3]],4)) + ggtitle("") + theme(legend.position = "none") 

w18 <- wilcox.test(table2$Meiosis~table2$age)
ggp18 <- ggplot(data.frame(table2), aes(x=table2$age, y=table2$Meiosis, color=table2$age)) + geom_boxplot(notch = T, aes(color=table2$age)) + labs(x="Genes' age", y="Expression during Meiosis") + annotate("text", x=1.5, y=13.5, label=round(w18[[3]],4)) + ggtitle("") + theme(legend.position = "none") 

w20 <- wilcox.test(table2$Post.meiosis~table2$age)
ggp20 <- ggplot(data.frame(table2), aes(x=table2$age, y=table2$Post.meiosis, color=table2$age)) + geom_boxplot(notch = T, aes(color=table2$age)) + labs(x="Genes' age", y="Expression during Post-Meiosis") + annotate("text", x=1.5, y=13.5, label=round(w20[[3]],4)) + ggtitle("") + theme(legend.position = "none") 

pdf("graphs/expression_all_classes.pdf", width=9, height=8)
grid.arrange(ggp19, ggp18, ggp20, ncol=3)
grid.text("Genes Expression in each spermatogenesis phase by age", just="centre", y=unit(0.95, "npc"), gp = gpar(fontsize = 18, fontface = "bold"))
dev.off()




# 1, 2, 3 -> post-meiotic
# 6, 8, 10 -> mitotic
# 4, 7, 12 -> meiotic
# 5 -> meiotic-postmeiotic
# 13 -> equal
# 11 -> the V

