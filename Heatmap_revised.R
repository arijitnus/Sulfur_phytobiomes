library(readxl)
library(dplyr)
library(ggplot2)
library(ComplexHeatmap)

Dat<-read_excel("/Users/arijitmukherjee/Downloads/calc.xlsx",sheet = "total_mat",col_names = T,skip = 0)
Dat<-as.data.frame(Dat)

rownames(Dat)<-Dat$gene
Dat<-Dat[,-1]
Dat

ha=rowAnnotation(category=c(rep("Assimilatory sulfate reduction",5),rep("Cysteine metabolism",18),
                            rep("Glutathione metabolism",18),rep("Methionine metabolism",25),
                            rep("Sulfate/thiosulfate transporter",3),rep("Sulfonate transporter",3),
                            rep("Sulfur compounds reduction/oxidation",14),rep("Sulfur disproportionation",2),
                            rep("Taurine metabolism",5),rep("Taurine transporter",3),
                            rep("Thiosulfate oxidation",2)))
library(RColorBrewer)
YlOrBr <- c("#000000","#CC6600","#4C9900","#E0E0E0")


hm<-Heatmap(as.matrix(Dat[,-c(1,2)]),cluster_rows = F,cluster_columns = F,row_names_gp = gpar(fontsize=4),
            row_split = c(rep("Assimilatory sulfate reduction",5),rep("Cysteine metabolism",18),
                          rep("Glutathione metabolism",18),rep("Methionine metabolism",25),
                          rep("Sulfate/thiosulfate transporter",3),rep("Sulfonate transporter",3),
                          rep("Sulfur compounds reduction/oxidation",14),rep("Sulfur disproportionation",2),
                          rep("Taurine metabolism",5),rep("Taurine transporter",3),
                          rep("Thiosulfate oxidation",2)),
            col=YlOrBr ,
            left_annotation = ha
            )
hm


dpi <- 300

# Set the filename and dimensions of the plot
filename <- "HM_niche_enrichment.tiff"
width <- 6  # Width in inches
height <- 9 # Height in inches

# Set the pointsize based on the DPI
pointsize <- dpi / 72

# Open the TIFF device with specified DPI and dimensions
tiff(file = filename, width = width, height = height, units = "in", res = dpi, pointsize = pointsize)

# Print the ggplot object
print(hm)

# Close the TIFF device
dev.off()









