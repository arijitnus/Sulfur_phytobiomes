#Calculate D-stats for all higher mods traits in total genomes of 3358 
#This is the script for calculating D-values 

##########################################Plant isolates##############
library(readxl)
library(dplyr)
library(caper)
dat<-read_excel("/Users/arijitmukherjee/Downloads/higher_mods_classification_revised.xlsx",
                col_names = T,skip = 0,sheet = "Sheet1")


#Read trees
tree<-read.tree("plant_soil_genomes.tree")
names(dat)

####for the 98 ANI data
dat_bin<-dat[,1:11]%>% mutate_if(is.numeric, ~1 * (. > 0))
head(dat_bin)
dat_bin$genomes<-dat$genomes
dat_bin<-as.data.frame(dat_bin)
length(tree$tip.label)
tree$node.label<-NULL
length(dat_bin$genomes)
genome_ord<-tree$tip.label
dat_bin_ord<-dat_bin[match(genome_ord,dat_bin$genomes),]

sum(dat_bin_ord$genomes==tree$tip.label)#all arranged perfect

dat_bin_ord$`Sulfur compound reduction/oxidation`
#calculate D-value for all columns in data
#calculate D-values
dat_phy_signal<-comparative.data(tree,dat_bin_ord,genomes)
S_red_ox<-phylo.d(dat_phy_signal,binvar = `Sulfur compound reduction/oxidation`)
S_red_ox#NA

names(dat_bin_ord)

transporters<-phylo.d(dat_phy_signal,binvar = Transporters)
transporters#

dso<-phylo.d(dat_phy_signal,binvar = `Dissimilatory sulfur oxidation`)
dso

met<-phylo.d(dat_phy_signal,binvar =`Methionine metabolism` )
met#

cys<-phylo.d(dat_phy_signal,binvar =`Cysteine metabolism` )
cys

gsh<-phylo.d(dat_phy_signal,binvar =`Glutathione metabolism` )
gsh

taurine<-phylo.d(dat_phy_signal,binvar =`Taurine metabolism` )
taurine


asr<-phylo.d(dat_phy_signal,binvar =`Assimilatory sulfur reduction` )
asr

sdis<-phylo.d(dat_phy_signal,binvar = `Sulfur disproportionation`)
sdis

dsr<-phylo.d(dat_phy_signal,binvar = `Dissimilatory sulfur reduction`)
dsr

sox<-phylo.d(dat_phy_signal,binvar = `Thiosulfate oxidation by SOX complex, thiosulfate => sulfate`)
sox



dsr

sox<-phylo.d(dat_phy_signal,binvar = )
sox

res<-list(transporters,dso,met, cys, gsh, taurine, asr, sdis, dsr,sox)
length(res)

d_vals<-numeric(length = length(res))
trait<-character(length = length(res))
pval_br<-numeric(length = length(res))
pval_rand<-numeric(length = length(res))

for (i in 1:length(res)) {
  trait[i]=res[[i]]$binvar
  d_vals[i]=res[[i]]$DEstimate
  pval_br[i]=res[[i]]$Pval0
  pval_rand[i]=res[[i]]$Pval1
}
res_df<-cbind(trait,d_vals,pval_br,pval_rand)
res_df
class(res_df)
res_df<-as.data.frame(res_df)

write.table(res_df,"D_stats_higher_mods.tsv",sep = "\t")






