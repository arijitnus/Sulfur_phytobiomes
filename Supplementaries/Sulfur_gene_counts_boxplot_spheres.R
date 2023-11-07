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
box<-ggplot(df,aes(x=factor(sphere,levels = level_order),y=`count/Mb`))+
  geom_boxplot(aes(col=sphere)) +
  geom_jitter(aes(col=sphere), size=0.2, alpha=0.2) +
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  theme_classic() +
  xlab("")+
  ylab("Number of unique sulfur metabolic genes per Mb genome")+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x=element_text(size=14),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(0, 40), expand = expansion(mult = c(0, 0)))
box

shapiro.test(df$`count/Mb`)
#Not normal. So fit a wilcox test for the differences
library(dunn.test)
kruskal.test(`count/Mb`~sphere,data =df)
stat_sphere<-dunn.test(df$`count/Mb`,df$sphere,wrap = TRUE, method = "bh")
stat_sphere$comparisons
stat_sphere$P.adjusted
library(dplyr)
#mean count per Mb genome is :
df%>%group_by(sphere)%>%summarise(avg=mean(`count/Mb`))
# A tibble: 3 × 2
#sphere         avg
#<fct>        <dbl>
 # 1 Phyllosphere  12.9
#2 Rhizosphere   12.3
#3 Soil          10.8




