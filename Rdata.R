setwd("/Users/liuzhe/Desktop/FengLab/figures")
data<-read.csv("Ax.GO.top20.csv",header=T)
library(dplyr)
library("ggplot2")
data2<-data[order(-data$count),]
data2$Description <- factor(data2$Description,levels=rev(c("regulation of angiogenesis","response to wounding",
                                          "regulation of ossification","gliogenesis",
                                          "embryonic organ development","renal system development",
                                          "regulation of mitotic cell cycle",
                                          "regulation of actin filament-based process",
                                          "protein kinase B signaling","epithelial tube morphogenesis",
                                          "learning or memory","Wnt signaling pathway",
                                          "cell-substrate adhesion","negative regulation of phosphorylation",
                                          "muscle tissue development","regulation of neurotransmitter levels",
                                          "glial cell differentiation","forebrain development",
                                          "negative regulation of neurogenesis",
                                          "morphogenesis of a branching structure")))
ggplot(data2, aes(x=Description, y=count, fill=p.adjust)) + 
  geom_bar(stat = "identity") +
  scale_fill_gradient2(midpoint=mean(data2$p.adjust), 
                       low ="#E3170D",mid = "#FA8072", high = "#FFC0CB",
                       space ="Lab") +
  coord_flip()


data<-read.csv("38_genes.venn.csv",header=T)
#newdata<-subset(data, data$p_val_adj < 0.05)
#data<-newdata
data2<-data[order(data$avg_log2FC),]
dotchart(data2$avg_log2FC, labels = data$gene, pch = 19, cex =.7, pt.cex = data$pct.3) 
genelist<-data2$gene
data2$gene<-factor(data2$gene,levels=data2$gene)
f <- ggplot(data2, aes(avg_log2FC, gene))+ geom_point(aes(size = pct.3)) + geom_point(aes(color=p_val_adj)) 
f + scale_colour_gradient(low = "red", high = "green")
