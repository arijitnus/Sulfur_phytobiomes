library(ComplexHeatmap)
library(dplyr)
library(ggplot2)
library(readxl)
input_rhizo<-read_excel("/Users/arijitmukherjee/Downloads/HM_rhizo_phyllo_plot_input.xlsx",sheet = "rhizosphere",col_names = T,skip = 0)
head(input_rhizo)
input_rhizo<-as.data.frame(input_rhizo)
rownames(input_rhizo)<-input_rhizo$genes
input2<-input_rhizo[,4:ncol(input_rhizo)]
head(input2)

ha=rowAnnotation(category=c(rep("Cys/Met metabolism",32),rep("Glutathione metabolism",15),
                            rep("Sulfur metabolism",26),rep("Sulfur oxidation/reduction",7),
                            rep("Sulfur transport",7),rep("Taurine metabolism",7)))

column_ha = HeatmapAnnotation (genes = anno_barplot(c(24,57,57,5)))

hm<-Heatmap(as.matrix(input2),cluster_rows = F,cluster_columns = F,row_names_gp = gpar(fontsize=4),
            row_split = c(rep("Cys/Met metabolism",32),rep("Glutathione metabolism",15),
                          rep("Sulfur metabolism",26),rep("Sulfur oxidation/reduction",7),
                          rep("Sulfur transport",7),rep("Taurine metabolism",7)),
            left_annotation = ha,
            top_annotation = column_ha)
hm

dpi <- 300

# Set the filename and dimensions of the plot
filename <- "HM_rhizo_sulfur.tiff"
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


#code for phyllosphere microbiome enrichments

library(ComplexHeatmap)
library(dplyr)
library(ggplot2)
library(readxl)
input_phyllo<-read_excel("/Users/arijitmukherjee/Downloads/HM_rhizo_phyllo_plot_input.xlsx",sheet = "phyllosphere",col_names = T,skip = 0)
head(input_phyllo)
input_phyllo<-as.data.frame(input_phyllo)
rownames(input_phyllo)<-input_phyllo$genes
input2<-input_phyllo[,2:4]
head(input2)
nrow(input2)

ha=rowAnnotation(category=c(rep("Sulfur metabolism",4),rep("Cys/Met metabolism",1),
                            rep("Glutathione metabolism",1),rep("Sulfur oxidation/reduction",2)))


column_ha = HeatmapAnnotation(genomes = anno_barplot(matrix(nc = 2, c(111,677,88,318,91,182)), 
                                          beside = TRUE, attach = TRUE))


hm<-Heatmap(as.matrix(input2),cluster_rows = F,cluster_columns = F,row_names_gp = gpar(fontsize=14),
            row_split = c(rep("Sulfur metabolism",4),rep("Cys/Met metabolism",1),
                          rep("Glutathione metabolism",1),rep("Sulfur oxidation/reduction",2)),
            left_annotation = ha,
            top_annotation = column_ha)
hm
dpi <- 300

# Set the filename and dimensions of the plot
filename <- "HM_phyllo_sulfur.tiff"
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




