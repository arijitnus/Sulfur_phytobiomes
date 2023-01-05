##Trying out phyloGLM for BGC MAGs data
library(phylolm)
df<-read.table('/Users/arijitmukherjee/Documents/Phytobiome/BGC_isolates/New_final_data/MAGs/Phylo-D/MAGs_final_tree.tsv',sep = "\t")
head(df)
rownames(df)<-df$Assembly
names(df)
df<-df[,-12]
tree<-read.tree('rooted828mags.tre')
tree$tip.label
dim(df)
df$niche<-rep(c(0,1),c(414,414))
df$niche
y<-df[,12]
names(y)<-rownames(df)
Res<-NULL
subtree<-drop.tip(phy = tree,tip = which(!(tree$tip.label%in%rownames(df))))
head(df)
x<-sample(c(0,1),828,replace = TRUE)# a dummy x value

names(x)<-rownames(df)
x
dat<-as.data.frame(cbind(y,x))
for (column in 1:11) {
  print(column)
  x<-df[,column]
  orthogroup<-colnames(df)[column]
  names(x)<-rownames(df)
  dat<-as.data.frame(cbind(y,x))
  m1<-tryCatch(phyloglm(formula = y~x,data = dat,phy = subtree,method = "logistic_IG10"),error = function(e) list(coefficients = NA))
  if(is.na(coef(m1)[1])){
    res<-data.frame(orthogroup.id = orthogroup,
                    Estimate = NA,
                    SE = NA,
                    z.value = NA,
                    p.value = NA)
  
  }else{
    m1.sum<-summary(m1)
    res<-data.frame(orthogroup.id = orthogroup,
                    Estimate = m1.sum$coefficients["x",1],
                    SE = m1.sum$coefficients["x",2],
                    z.value = m1.sum$coefficients["x",3],
                    p.value = m1.sum$coefficients["x",4])
  }
  rm(m1,m1.sum)
  Res<-rbind(Res,res)
}

#The following works,  but fails to converge for dummy data
m<-phyloglm(x~y,data = dat,phy = subtree,method = "logistic_IG10")

















