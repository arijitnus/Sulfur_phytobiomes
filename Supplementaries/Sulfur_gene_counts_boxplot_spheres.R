#boxplot for three sphereslibrary(ggplot2)
library(viridis)
library(hrbrthemes)
library(readxl)
library(ggplot2)
df<-read_excel("/Users/arijitmukherjee/Downloads/boxplot_sulfur_count.xlsx",sheet = "box",col_names = T,skip = 0)
names(df)
df$sphere<-as.factor(df$sphere)
level_order<-c("Soil","Rhizosphere","Phyllosphere")
cols=c("#4C9900","#CC6600",'#663300')
#boxplot of total number of BGCs per genome from isolates and MAGs
box<-ggplot(df,aes(x=factor(sphere,levels = level_order),y=unique_count))+
  geom_boxplot(aes(col=sphere)) +
  geom_jitter(aes(col=sphere), size=0.2, alpha=0.2) +
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic() +
  xlab("")+
  ylab("Number of sulfur metabolic genes per genome")+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(0, 120), expand = expansion(mult = c(0, 0)))
box

shapiro.test(df$unique_count)
#Not normal. So fit a wilcox test for the differences
library(dunn.test)
kruskal.test(unique_count~sphere,data =df)
stat_sphere<-dunn.test(df$unique_count,df$sphere,wrap = TRUE, method = "bh")
head(stat_rescue_biomass$P.adjusted)

#Result Rhizo vs Phyllo <0.0001, Rhizos vs soil <0 Soil vs phyllo =0.12
#Col Mean-|
  #Row Mean |   Phyllosp   Rhizosph

ggsave(
  "Sulfur_gene_count_spheres.tiff",
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


