#Selecting appropriate model; we tested four different models based on count data
#We tested Poisson, NB, ZINB or ZIP model and tested for each of the BGC category 
#This was done for isolates dataset since the variation explained was higher in isolates dataset 
#This was done at the genus level since higher taxonomic resolution explained better variance as shown In Fig.3A
library(readxl)
library(dplyr)
library(ggplot2)
library(MASS)
library(pscl)
##
data<-read_excel("/Users/arijitmukherjee/Downloads/Submod_Neel.xlsx",
                 sheet = "Master",col_names = T,skip = 0)

head(data)
View(data)
dim(data)
data$`H2S production`
data$sphere<-as.factor(data$sphere)
data<-within(data,sphere<-relevel(sphere,ref = 'Soil'))



#H2S production
H2S_poisson<-glm(`H2S production`~sphere,data = data)
H2S_nb<-glm.nb(`H2S production`~sphere,data = data)
H2S_zinb<-zeroinfl(`H2S production`~sphere,data = data,dist = "negbin")
H2S_zip<-zeroinfl(`H2S production`~sphere,data = data,dist = "poisson")
AIC(H2S_poisson,H2S_nb,H2S_zip,H2S_zinb)#H2S Zinb

data$`DMS production`
#DMS production
DMS_poisson<-glm(`DMS production`~sphere,data = data)
DMS_nb<-glm.nb(`DMS production`~sphere,data = data)
DMS_zinb<-zeroinfl(`DMS production`~sphere,data = data,dist = "negbin")
DMS_zip<-zeroinfl(`DMS production`~sphere,data = data,dist = "poisson")
AIC(DMS_poisson,DMS_nb,DMS_zip,DMS_zinb)#DMS Zinb

data$`Sulfate-thiosulfate transporter`

#Sulfthio
Sulfthio_poisson<-glm(`Sulfate-thiosulfate transporter`~sphere,data = data)
Sulfthio_nb<-glm.nb(`Sulfate-thiosulfate transporter`~sphere,data = data)
Sulfthio_zinb<-zeroinfl(`Sulfate-thiosulfate transporter`~sphere,data = data,dist = "negbin")
Sulfthio_zip<-zeroinfl(`Sulfate-thiosulfate transporter`~sphere,data = data,dist = "poisson")
AIC(Sulfthio_poisson,Sulfthio_nb,Sulfthio_zip,Sulfthio_zinb)#zip 

#Taurine transporter
Taurine_poisson<-glm(`Taurine transporter`~sphere,data = data)
Taurine_nb<-glm.nb(`Taurine transporter`~sphere,data = data)
Taurine_zinb<-zeroinfl(`Taurine transporter`~sphere,data = data,dist = "negbin")
Taurine_zip<-zeroinfl(`Taurine transporter`~sphere,data = data,dist = "poisson")
AIC(Taurine_poisson,Taurine_nb,Taurine_zip,Taurine_zinb)#zip 


#`Sulfonate transporter`
Sulfonate_poisson<-glm(`Sulfonate transporter`~sphere,data = data)
Sulfonate_nb<-glm.nb(`Sulfonate transporter`~sphere,data = data)
Sulfonate_zinb<-zeroinfl(`Sulfonate transporter`~sphere,data = data,dist = "negbin")
Sulfonate_zip<-zeroinfl(`Sulfonate transporter`~sphere,data = data,dist = "poisson")
AIC(Sulfonate_poisson,Sulfonate_nb,Sulfonate_zip,Sulfonate_zinb)#zinb

#Sulfatase
Sulfatase_poisson<-glm(Sulfutase~sphere,data = data)
Sulfatase_nb<-glm.nb(Sulfutase~sphere,data = data)
Sulfatase_zinb<-zeroinfl(Sulfutase~sphere,data = data,dist = "negbin")
Sulfatase_zip<-zeroinfl(Sulfutase~sphere,data = data,dist = "poisson")
AIC(Sulfatase_poisson,Sulfatase_nb,Sulfatase_zip,Sulfatase_zinb)#zinb


#data$`Thiosulfate oxidation`
sox_poisson<-glm(`Thiosulfate oxidation`~sphere,data = data)
sox_nb<-glm.nb(`Thiosulfate oxidation`~sphere,data = data)
sox_zinb<-zeroinfl(`Thiosulfate oxidation`~sphere,data = data,dist = "negbin")
sox_zip<-zeroinfl(`Thiosulfate oxidation`~sphere,data = data,dist = "poisson")
AIC(sox_poisson,sox_nb,sox_zip,sox_zinb)#nb

summary(H2S_zinb)
summary(DMS_zinb)
summary(Sulfthio_zip)
summary(Taurine_zip)
summary(Sulfonate_zinb)
summary(Sulfatase_zinb)
summary(sox_nb)


df<-read_excel("/Users/arijitmukherjee/Downloads/forest_plot_input.xlsx",sheet = "Sheet1",col_names = T,skip = 0)
dim(df)
#forest plot for Genus
library(readxl)
library(ggplot2)
cols=c("#4C9900","#CC6600","#663300")
df


p = ggplot(data=df,
           aes(x = Niche,y = log_count, ymin = Lower, ymax = Upper ))+
  geom_pointrange(aes(col=Niche))+
  scale_color_manual(values = cols)+
  #geom_hline(aes(fill=Phylum),yintercept =1, linetype=2)+
  xlab('')+ ylab(expression("Log"[10]*" mean count per genome at 95% confidence interval"))+
  geom_errorbar(aes(ymin=Lower, ymax=Upper,col=Niche),width=0.3,cex=0.6)+ 
  facet_wrap(~Process,strip.position="left",nrow=3,scales = "free_y") +
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.y=element_text(size = 14),
        axis.ticks.y=element_text(size = 14),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size=12,hjust=0,vjust = 1,angle=180,face="bold"))
q<-p+theme_bw()+
  coord_flip()
q


saveRDS(q,"Forest_plot_S_proceses.rds")
ggsave(
  "fores_S_processes_final.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 10,
  height = 6,
  units = "in",
  dpi = 300,
)
dev.off()
