
#code for stack bar plot for phyum level distribution of the genomes
library(readxl)
library(ggplot2)
library(dplyr)
dat<-read_excel("/Users/arijitmukherjee/Downloads/phytobiomes_taxonomy_stack_barplot.xlsx",sheet = "counts",
                col_names = T,skip = 0)
head(dat)
dat$Source<-as.factor(dat$Source)
dat$Phylum<-as.factor(dat$Phylum)
library(ggplot2)


stack<-ggplot(dat,aes(x=Source,y=count,fill=Phylum))+
  geom_bar(stat = "identity",position = "fill")+
  theme_classic()+
  xlab("")+
  ylab("Proportion")+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(expand = expansion(mult = c(0, 0)))

stack
ggsave(
  "phytobiomes_taxa_dist.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 6,
  height = 6,
  units = "in",
  dpi = 400,
)
dev.off()


















