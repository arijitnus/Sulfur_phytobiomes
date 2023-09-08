library(readxl)
library(ggplot2)
dat<-read_excel("/Users/arijitmukherjee/Downloads/quality_info.xlsx",sheet = "Sheet1",col_names = T,skip = 0)
dat$Sphere<-as.factor(dat$Sphere)

level_order<-c("Soil","Rhizosphere","Phyllosphere")
cols=c("#4C9900","#CC6600",'#663300')
#boxplot of total number of BGCs per genome from isolates and MAGs
box_gc<-ggplot(dat,aes(x=factor(Sphere,levels = level_order),y=GC*100))+
  geom_boxplot(aes(col=Sphere)) +
  geom_jitter(aes(col=Sphere), size=0.2, alpha=0.2) +
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic() +
  xlab("")+
  ylab("GC (%)")+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(20, 80), expand = expansion(mult = c(0, 0)))
box_gc
ggsave(
  "GC_Content_spheres.tiff",
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

shapiro.test(dat$GC)
#Not normal. So fit a wilcox test for the differences
library(dunn.test)
kruskal.test(GC~Sphere,data =dat)
stat_sphere<-dunn.test(dat$GC,dat$Sphere,wrap = TRUE, method = "bh")
head(stat_sphere$P.adjusted)

#Only Soil vs rhizosphere is different

dat$Size

box_gs<-ggplot(dat,aes(x=factor(Sphere,levels = level_order),y=Size/10^6))+
  geom_boxplot(aes(col=Sphere)) +
  geom_jitter(aes(col=Sphere), size=0.2, alpha=0.2) +
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic() +
  xlab("")+
  ylab("Genome size (Mb)")+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(0,15), expand = expansion(mult = c(0, 0)))
box_gs
ggsave(
  "Genome_size_spheres.tiff",
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

shapiro.test(dat$Size)
#Not normal. So fit a wilcox test for the differences
library(dunn.test)
kruskal.test(Size~Sphere,data =dat)
stat_sphere<-dunn.test(dat$Size,dat$Sphere,wrap = TRUE, method = "bh")
head(stat_sphere$P.adjusted)
#all are significantly different for all niches



box_ng<-ggplot(dat,aes(x=factor(Sphere,levels = level_order),y=Number_of_genes))+
  geom_boxplot(aes(col=Sphere)) +
  geom_jitter(aes(col=Sphere), size=0.2, alpha=0.2) +
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic() +
  xlab("")+
  ylab("Number of predicted genes per genome")+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(0,15000), expand = expansion(mult = c(0, 0)))
box_ng
ggsave(
  "Gene_number_spheres.tiff",
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


library(dunn.test)
kruskal.test(Number_of_genes~Sphere,data =dat)
stat_sphere<-dunn.test(dat$Number_of_genes,dat$Sphere,wrap = TRUE, method = "bh")
head(stat_sphere$P.adjusted)
#all are significantly different for all niches










