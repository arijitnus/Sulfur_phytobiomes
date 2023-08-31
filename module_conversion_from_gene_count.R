library(readxl)
library(dplyr)
library(tidyr)
sub_mods<-read_excel("/Users/arijitmukherjee/Downloads/gene_distribution.xlsx",sheet = "Soilplant_submodules",
                        col_names = T,skip = 0,.name_repair = "none")
names(sub_mods)
dim(sub_mods)
names(sub_mods)
#remove columns without any names
# Identify columns with empty names
columns_with_empty_names <- sapply(names(sub_mods), function(col_name) col_name == "")

# Remove columns with empty names
cleaned_data <- sub_mods[, !columns_with_empty_names]
names(cleaned_data)

#Remove columns with NA names
na_cols<-sapply(names(cleaned_data), function(col_name) col_name == "NA")
cleaned_data <- cleaned_data[, !na_cols]

#check if all columns are numeric
numeric_columns <- sapply(cleaned_data, is.numeric)
sum(numeric_columns)

#Keep only columns that are numeric
data<-cleaned_data[,numeric_columns]
ncol(data)
#We want to collapse the columns with similar names into one colums
#extract unique module names
class(data)
data<-as.data.frame(data,check.names=F)
data
# Identify columns with identical names
identical_column_names <- unique(names(data))
identical_column_names
identical_column_names<-identical_column_names[-15]
# Create a new dataframe to store collapsed columns
collapsed_data <- data.frame(matrix(0, nrow = nrow(data), ncol = length(identical_column_names)))
colnames(collapsed_data) <- identical_column_names
collapsed_data

for (col_name in identical_column_names) {
  identical_cols <- data[, names(data) == col_name]
  collapsed_data[, col_name] <- apply(identical_cols, 1, function(row) ifelse(any(row > 0), 1, 0))
}

# Remove duplicated columns
collapsed_data <- collapsed_data[, !duplicated(colnames(collapsed_data))]
collapsed_data$genomes<-cleaned_data$Gene
collapsed_data$sphere<-cleaned_data$sphere

# Print the collapsed dataframe

collapsed_data$genomes <- gsub("G", "", collapsed_data$genomes)#Remov G from the genomes names
collapsed_data#This is the sub-module distribution data

write.table(collapsed_data,"sub_mod_collapsed_plant_soil_genomes.tsv",sep = "\t",row.names = F)































