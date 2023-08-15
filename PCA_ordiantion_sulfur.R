#plotting PcoA plot for sulfur metabolic genes in plant and soil microbial genomes
#Date: 15.08.2023
library(readxl)
library(ggplot2)
library(dplyr)
plant<-read_excel("/Users/arijitmukherjee/Downloads/plant_soil_matrix.xlsx",sheet = "plant",col_names = T,skip = 0)
soil<-read_excel("/Users/arijitmukherjee/Downloads/plant_soil_matrix.xlsx",sheet = "soil",col_names = T,skip = 0)
plant<-as.data.frame(plant)
soil<-as.data.frame(soil)
dim(soil)#2558
dim(plant)#4022
merged_df<-rbind(plant,soil)#stack these two df
dim(merged_df)
head(merged_df)
class(merged_df)
rownames(merged_df)<-merged_df$genomes
merged_df<-merged_df[,-1]

head(merged_df)#convert it to presence absence data
head(merged_df)
threshold<-1
merged_df_bin<-ifelse(merged_df>threshold,1,0)
#perform PCA for this binary dataframe
# Perform PCA
pca_result <- prcomp(merged_df_bin, scale. = FALSE)

# Get PCA scores
pca_scores <- pca_result$x

# Create a data frame with PCA scores
pca_df <- data.frame(PC1 = pca_scores[, 1], PC2 = pca_scores[, 2],source=
                       c(rep("plant",4022),rep("soil",2558)))

# Create an ordination plot using ggplot2
ggplot(pca_df, aes(x = PC1, y = PC2,col=as.factor(pca_df$source))) +
  geom_point() +
  labs(x = "PC1", y = "PC2", title = "Ordination Plot - PCA")


##Based on raw counts=====
#===========
#===========

#Now try to create PCA based on raw counts
pca_result <- prcomp(input, scale. = TRUE)

# Get PCA scores
pca_scores <- pca_result$x

# Create a data frame with PCA scores
pca_df <- data.frame(PC1 = pca_scores[, 1], PC2 = pca_scores[, 2],source=
                       c(rep("plant",4022),rep("soil",2558)))

# Create an ordination plot using ggplot2
ggplot(pca_df, aes(x = PC1, y = PC2,col=as.factor(pca_df$source))) +
  geom_point() +
  labs(x = "PC1", y = "PC2", title = "Ordination Plot - PCA")


dim(merged_df)
rownames(merged_df)<-merged_df$genomes
merged_df<-merged_df[,-1]
merged_df$source<-c(rep("Plant",4022),rep("Soil",2558))
merged_df$sums<-rowSums(merged_df)
merged_df_filt<-merged_df%>%filter(sums!=0)
dim(merged_df_filt)#6556
names(merged_df_filt)
input<-merged_df_filt[,-c(292,293)]



asv_tab10_ra<-sweep(input,1,rowSums(input),"/")
asv_tab10_ra



cs_rhizo10<-capscale(asv_tab10_ra~source,
                     data = merged_df_filt,add=F,sqrt.dist=T,distance="bray")
anova.cca(cs_rhizo10,permutations = 999)
























