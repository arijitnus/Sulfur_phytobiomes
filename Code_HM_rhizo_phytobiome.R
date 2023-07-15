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
#input2 <- input2 %>% mutate(across(everything(), ~ifelse(. > 1, 1, .)))
Heatmap(input2,cluster_rows = F,cluster_columns = F,na_col = "grey")

ha=rowAnnotation(category=c(rep("Cys/Met metabolism",32),rep("Glutathione metabolism",15),
                            rep("Sulfur metabolism",26),rep("Sulfur oxidation/reduction",7),
                            rep("Sulfur transport",7),rep("Taurine metabolism",7)))

column_ha = HeatmapAnnotation (genes = anno_barplot(c(24,57,57,5)))

hm<-Heatmap(as.matrix(input2),cluster_rows = F,cluster_columns = F,row_names_gp = gpar(fontsize=4),
            column_names_gp = gpar(fontsize=4),
            row_split = c(rep("Cys/Met metabolism",32),rep("Glutathione metabolism",15),
                          rep("Sulfur metabolism",26),rep("Sulfur oxidation/reduction",7),
                          rep("Sulfur transport",7),rep("Taurine metabolism",7)),
            left_annotation = rowAnnotation(foo = anno_block(gp = gpar(fill = 2:7),
                                            labels = c("Cys/Met metabolism", "Glutathione metabolism", "Sulfur metabolism",
                                                        "Sulfur oxidation/reduction","Sulfur transport","Taurine metabolism"),
            top_annotation = column_ha)
hm









