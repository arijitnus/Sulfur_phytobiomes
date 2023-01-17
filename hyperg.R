#try hypergeometric test based on isai scripts
#For phyper, q is number of genomes in PA category with their count more than 0
#q = nrow(subset(Map, Classification == "PA" & gene > 0)) - 1
#m is number of genomes that are plant associated, m = sum(Map$gene > 0)
#n is number of genoems that are soil associated n = nrow(subset(Map,gene == 0))
#k is k = nrow(subset(Map, Classification == "PA")),lower.tail=FALSE)
library(readxl)
library(ggplot2)
library(dplyr)
PA<-read_excel("/Users/arijitmukherjee/Downloads/plant_soil_matrix.xlsx",sheet = "plant",col_names = T,skip = 0)
soil<-read_excel("/Users/arijitmukherjee/Downloads/plant_soil_matrix.xlsx",sheet = "soil",col_names = T,skip = 0)
head(PA)
df<-rbind(PA,soil)
dim(df)
head(df)
names(df)

Map<-read_excel("/Users/arijitmukherjee/Downloads/plant_soil_matrix.xlsx",sheet = "combined_map",col_names = T,skip = 0)
head(Map)
sum(df$genome_ID==Map$genome_ID)#orders are same

df<-as.data.frame(df)
Map<-as.data.frame(Map)


rownames(df)<-df$genome_ID
rownames(Map)<-Map$genome_ID
df<-df[,-1]
Map<-Map[,-1]
head(df)
df<-df[,-1]
head(df)


N.pa<-sum(df[Map$source=="PA",])
N.npa<-sum(df[Map$source=="soil",])






allgenes_hyp <- function(genes,Map){
  #tree <- Dat$tree
  #Map <- Dat$Map
  #genes <- Dat$genes >=1
  
  N.pa <- sum(genes[ Map$source == "PA", ])
  N.npa <- sum(genes[ Map$source == "soil", ])
  #N.genomes <- nrow(Map)
  Res <- NULL
  for(gene in 1:ncol(genes)){
    #gene <- 1
    print (gene)
    gene.id <- colnames(genes)[gene]
    Map$gene <- genes[,gene]
    
    #genes.per.class <- aggregate(gene ~ Classification, data = Map,FUN = sum)
    #genomes.per.class <- aggregate(gene ~ Classification, data = Map,FUN = length)
    #N.genes <- sum(Map$gene)
    
    #ftable(gene ~ Classification , Map)
    
    #dhyper(x = 0, m = 2, n = 2, k = 2)
    #dhyper(x = 1, m = 2, n = 2, k = 2)
    #dhyper(x = 2, m = 2, n = 2, k = 2)
    #phyper(q = 0, m = 2, n = 2, k = 2)
    #phyper(q = 1, m = 2, n = 2, k = 2)
    #phyper(q = 2, m = 2, n = 2, k = 2)
    # Following is wrong, counts genomes for zero and genes for 1+
    # pval <- phyper(q = sum(subset(Map, Classification == "PA" & gene > 0)$gene) - 1,
    #                m = sum(Map$gene),
    #                n = nrow(subset(Map,gene == 0)),
    #                k = sum(subset(Map, Classification == "PA")$gene))
    #pval <- 1 - pval
    
    # Binary version
    pval <- phyper(q = nrow(subset(Map, source == "PA" & gene > 0)) - 1,
                   m = sum(Map$gene > 0),
                   n = nrow(subset(Map,gene == 0)),
                   k = nrow(subset(Map, source == "PA")),lower.tail=FALSE)
    #pval <- 1 - pval
    #pval[ pval == 0 ] <- 1e-16
    score <- -log10(pval)
    
    pval2 <- phyper(q = sum(subset(Map, source == "PA")$gene) - 1, 
                    m = N.pa, n = N.npa, k = sum(Map$gene),lower.tail=FALSE)
    #pval2 <- 1 - pval2
    #pval2[ pval2 == 0 ] <- 1e-16 
    
    
    #Test for depletion binary version 
    pval_dep <- phyper(q = nrow(subset(Map, source == "PA" & gene > 0)),
                       m = sum(Map$gene > 0),
                       n = nrow(subset(Map,gene == 0)),
                       k = nrow(subset(Map, source == "PA")),
                       lower.tail=TRUE)
    #pval_dep[ pval_dep == 0 ] <- 1e-16
    score_dep<--log10(pval_dep)
    
    #Test for depletion Raw counts version 
    pval2_dep <- phyper(q = sum(subset(Map, source == "PA")$gene),
                        m = N.pa, n = N.npa, k = sum(Map$gene),lower.tail=TRUE)
    #pval2_dep[ pval2_dep == 0 ] <- 1e-16
    score2_dep<--log10(pval2_dep)
    
    
    
    #res <- data.frame(gene.id = gene.id, score = score, p.value = pval,
    #                  full.score = -log10(pval2), full.p.value = pval2)
    res <- data.frame(gene.id = gene.id, score_enriched_binary = score, p.value_enriched_binary = pval,
                      score_depletion_binary=score_dep,p.value_depletion_binary=pval_dep,
                      score_enriched_rawcounts = -log10(pval2), p.value_enriched_rawcounts = pval2,
                      score_depletion_rawcounts=score2_dep,p.value_depletion_rawcounts=pval2_dep)
    
    Map$gene <- NULL
    Res <- rbind(Res,res)
  }
  Res<-data.frame(gene.id=Res$gene.id,score_enriched_binary=Res$score_enriched_binary,
                  z.score_enriched_binary= (Res$score_enriched_binary - mean(Res$score_enriched_binary)) / sd(Res$score_enriched_binary),
                  p.value_enriched_binary=Res$p.value_enriched_binary,
                  score_depletion_binary=Res$score_depletion_binary,
                  z.score_depletion_binary=(Res$score_depletion_binary - mean(Res$score_depletion_binary)) / sd(Res$score_depletion_binary),
                  p.value_depletion_binary=Res$p.value_depletion_binary,
                  score_enriched_rawcounts=Res$score_enriched_rawcounts,
                  z.score_enriched_rawcounts=(Res$score_enriched_rawcounts - mean(Res$score_enriched_rawcounts)) / sd(Res$score_enriched_rawcounts),
                  p.value_enriched_rawcounts=Res$p.value_enriched_rawcounts,
                  score_depletion_rawcounts=Res$score_depletion_rawcounts,
                  z.score_depletion_rawcounts=(Res$score_depletion_rawcounts - mean(Res$score_depletion_rawcounts)) / sd(Res$score_depletion_rawcounts),
                  p.value_depletion_rawcounts=Res$p.value_depletion_rawcounts)
  
  return(Res)
}

df<-allgenes_hyp(df,Map)
df

#correct them for pvalue corrections
df$adj_p.value_enriched_binary<-p.adjust(df$p.value_enriched_binary,method = "fdr")
df$adj_p.value_depletion_binary<-p.adjust(df$p.value_depletion_binary,method = "fdr")
df$adj_p.value_enriched_rawcounts<-p.adjust(df$p.value_enriched_rawcounts,method = "fdr")
df$adj_p.value_depletion_rawcounts<-p.adjust(df$p.value_depletion_rawcounts,method = "fdr")

write.table(df,"final_res_hyperg.tsv",sep = "\t")







































