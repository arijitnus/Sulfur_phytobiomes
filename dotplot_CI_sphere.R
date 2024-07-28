df<-read_excel("/Users/arijitmukherjee/Downloads/forest_plot_input.xlsx",sheet = "Sheet1",col_names = T,skip = 0)
dim(df)
#forest plot for Genus
library(readxl)
library(ggplot2)
library(dplyr)
df$Process
df_thiosulfate<-df%>%filter(Process=='Thiosulfate oxidation')
df$Process

p = ggplot(data=df_thiosulfate,
           aes(x = Niche,y = log_count, ymin = Lower, ymax = Upper ))+
  geom_pointrange(size=1)+
  #geom_hline(aes(fill=Phylum),yintercept =1, linetype=2)+
  xlab('')+ ylab(expression("Log"[10]*" mean count per genome at 95% confidence interval"))+
  geom_errorbar(aes(ymin=Lower, ymax=Upper),width=0.4,cex=0.6)+
  scale_x_discrete(limits = c("Soil", "Rhizosphere", "Phyllosphere")) +
  theme_classic()+
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.title.x=element_text(size=16),
        axis.line = element_line(colour="black", size = 0.7),
        strip.background = element_blank())+
  scale_y_continuous(limits = c(-3, -0.5), expand = expansion(mult = c(0, 0)))
p
ggsave(
  "thisoulfate_oxidation_final.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 7,
  height = 7,
  units = "in",
  dpi = 300,
)
dev.off()