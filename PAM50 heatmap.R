source("./leash2.0.7.R")
source("./VarGuid20240626.R")
#source("./organized code:data/functions.R")
library(glmnet)
library(tidyverse)
library(MASS)
library(xtable)
library(scales)
library(faux)
library(olsrr)
library(caTools)
library(caret)
library(pheatmap)
library(dendsort)
library(egg)
library(gridExtra)
sort_hclust <- function(...) as.hclust(dendsort(as.dendrogram(...)))
breaks=seq(5,20,by=0.25)

#genes=load("~/Documents/Dissertation/varguid/PAM50.RData")

#### The heatmap
p50=as.data.frame(cbind(topgene,outcome)) %>% drop_na(outcome) %>% arrange(desc(outcome))
p20133=as.data.frame(cbind(genes,outcome)) %>% drop_na(outcome)

######

c1=sort_hclust(hclust(dist(t(p50[,-51])))) ## col order
r1=sort_hclust(hclust(dist(p50[,-51]))) ## row order

#q_vec=quantile(outcome,na.rm= T)

# cut1=p50 %>% filter(outcome<=q_vec[2]) %>% dplyr::select(-outcome)
# cut2=p50 %>% filter(q_vec[2]<outcome & outcome<=q_vec[3])%>% dplyr::select(-outcome)
# cut3=p50 %>% filter(q_vec[3]<outcome & outcome<=q_vec[4])%>% dplyr::select(-outcome)
# cut4=p50 %>% filter(q_vec[4]<outcome & outcome<=q_vec[5])%>% dplyr::select(-outcome)


#plot(cut1_cluster_gene, main = "Sorted Dendrogram in outcome 1st quantile", xlab = "", sub = "",ylab = "")

p_heatmap=function(data,title,col_order){
  r=NULL
  for(i in 1:4){
  p1=pheatmap(data[[i]] ,
           fontsize_col = 10,
           fontsize_row = 10,
           display_numbers = F,
           number_color = "black", 
           cluster_cols = col_order,
           breaks = breaks,
           cluster_rows = F,
           color=colorRampPalette(c("green", "black", "red"))(length(breaks)),
           fontsize_number = 6,#
           border_color = "black",
           show_colnames = T, show_rownames = F,
           main = title[i])
  r[[i]]=p1[[4]]
  }
  return(r)
}
d1=list(p50[r1[["order"]][1:12],-51],p50[r1[["order"]][13:24],-51],
        p50[r1[["order"]][25:36],-51],p50[r1[["order"]][37:49],-51])
t1=paste0("Gene expression in ",1:4," quantile,based on hclust row order")
res1=p_heatmap(data=d1,title=t1,col_order=c1)
do.call(grid.arrange,res1)

d1_1=list(p50[1:12,-51],p50[13:24,-51],
        p50[25:36,-51],p50[37:49,-51])
t1_1=paste0("Gene expression in ",1:4," quantile,based on outcome decresing row order")
res1_1=p_heatmap(data=d1_1,title=t1_1,col_order=c1)
do.call(grid.arrange,res1_1)


########### in p20133 data,use varguid lasso to select genes
set.seed(2024)
o_p20133 <- lmv(X =as.matrix(p20133[,1:20133]) , Y = unlist(p20133$outcome), lasso = TRUE)
m=as.data.frame(as.matrix(o_p20133$beta)) %>% filter(s0>0)
select_gene=rownames(m) # 34

p34=p20133 %>% dplyr::select(select_gene,outcome)%>% arrange(desc(outcome))
c2=sort_hclust(hclust(dist(t(p34[,-35]))))
###################
d2=list(p34[r1[["order"]][1:12],-35],p34[r1[["order"]][13:24],-35],
        p34[r1[["order"]][25:36],-35],p34[r1[["order"]][37:49],-35])
t2=paste0("34 Varguild-lasso selected GE in ",1:4," quantile based on hclust row order")
res2=p_heatmap(data=d2,title=t2,col_order=c2)
do.call(grid.arrange,res2)
##################################

d2_2=list(p34[1:12,-35],p34[13:24,-35],
        p34[25:36,-35],p34[37:49,-35])
t2_2=paste0("34 Varguild-lasso selected GE in ",1:4," quantile based on outcome decresing row order")
res2_2=p_heatmap(data=d2_2,title=t2_2,col_order=c2)
do.call(grid.arrange,res2_2)

  ########### in p20133 data,use traditional lasso to select genes
  set.seed(2024)
  o_p20133_2 <- cv.glmnet(x =as.matrix(p20133[,1:20133]) , y = p20133$outcome,alpha = 1,lambda =exp(seq(-1,1,length=100) ))
  plot(o_p20133_2)
  m2=as.data.frame(as.matrix(coef(o_p20133_2, s = "lambda.min"))) %>% filter(s1>0)
  select_gene2=rownames(m2) # 16

  intersect(select_gene,select_gene2)
  
  p16=p20133 %>% dplyr::select(select_gene2,outcome) %>%arrange(desc(outcome))
  c3=sort_hclust(hclust(dist(t(p16[,-17]))))
  
  d3=list(p16[r1[["order"]][1:12],-17],p16[r1[["order"]][13:24],-17],
          p16[r1[["order"]][25:36],-17],p16[r1[["order"]][37:49],-17])
  t3=paste0("16 Lasso selected genesexpression in ",1:4," quantile based on hclust row order")
  res3=p_heatmap(data=d3,title=t3,col_order=c3)
  do.call(grid.arrange,res3)
  
  d3_3=list(p16[1:12,-17],p16[13:24,-17],
            p16[25:36,-17],p16[37:49,-17])
  t3_3=paste0("16 Lasso selected GE in ",1:4," quantile based on outcome decresing row order")
  res3_3=p_heatmap(data=d3_3,title=t3_3,col_order=c3)
  do.call(grid.arrange,res3)
 