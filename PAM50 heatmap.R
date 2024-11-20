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
sort_hclust <- function(...) as.hclust(dendsort(as.dendrogram(...)))


#genes=load("~/Documents/Dissertation/varguid/PAM50.RData")

#### The heatmap
p50=as.data.frame(cbind(topgene,outcome)) %>% drop_na(outcome)
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

pheatmap(p50[r1[["labels"]][1:12],-51] ,
         fontsize_col = 10,
         fontsize_row = 10,
         display_numbers = F,
         number_color = "black", 
         cluster_cols = c1,
         cluster_rows = F,
         color=colorRampPalette(c("green", "black", "red"))(50),
         fontsize_number = 6,#
         border_color = "black",
         show_colnames = T, show_rownames = T,
         main = "Gene expression in outcome 1st quantile")

#plot(cut2_cluster_gene, main = "Sorted Dendrogram in outcome 2nd quantile", xlab = "", sub = "",ylab = "")

pheatmap(p50[r1[["labels"]][13:24],-51] ,
         fontsize_col = 10,
         fontsize_row = 10,
         display_numbers = F,
         number_color = "black", 
         cluster_cols = c1,
         cluster_rows = F,
         color=colorRampPalette(c("green", "black", "red"))(50),
         fontsize_number = 6,#
         border_color = "black",
         show_colnames = T, show_rownames = T,
         main = "Gene expression in outcome 2nd quantile")


#plot(cut3_cluster_gene, main = "Sorted Dendrogram in outcome 3rd quantile", xlab = "", sub = "",ylab = "")

pheatmap(p50[r1[["labels"]][25:36],-51],
         fontsize_col = 10,
         fontsize_row = 10,
         display_numbers = F,
         number_color = "black", 
         cluster_cols = c1,
         cluster_rows = F,
         color=colorRampPalette(c("green", "black", "red"))(50),
         fontsize_number = 6,#
         border_color = "black",
         show_colnames = T, show_rownames = T,
         main = "Gene expression in outcome 3rd quantile")


#plot(cut4_cluster_gene, main = "Sorted Dendrogram in outcome 4th quantile", xlab = "", sub = "",ylab = "")

pheatmap(p50[r1[["labels"]][37:49],-51],
         fontsize_col = 10,
         fontsize_row = 10,
         display_numbers = F,
         number_color = "black", 
         cluster_cols = c1,
         cluster_rows = F,
         color=colorRampPalette(c("green", "black", "red"))(50),
         fontsize_number = 6,#
         border_color = "black",
         show_colnames = T, show_rownames = T,
         main = "Gene expression in outcome 4th quantile")

#ggarrange(p1, p2, p3,p4, ncol = 2, nrow = 2)

########### in p20133 data,use lmv to select genes
set.seed(2024)
o_p20133 <- lmv(X =as.matrix(p20133[,1:20133]) , Y = unlist(p20133$outcome), lasso = TRUE)
m=as.data.frame(as.matrix(o_p20133$beta)) %>% filter(s0>0)
select_gene=rownames(m) # 34

p34=p20133 %>% dplyr::select(select_gene)
c2=sort_hclust(hclust(dist(t(p34))))

# cut1_2=p20133 %>% dplyr::select(select_gene,outcome) %>% filter(outcome<=q_vec[2])%>% dplyr::select(-outcome)
# cut2_2=p20133 %>%  dplyr::select(select_gene,outcome) %>% filter(q_vec[2]<outcome & outcome<=q_vec[3])%>% dplyr::select(-outcome)
# cut3_2=p20133 %>% dplyr::select(select_gene,outcome) %>%filter(q_vec[3]<outcome & outcome<=q_vec[4])%>% dplyr::select(-outcome)
# cut4_2=p20133 %>% dplyr::select(select_gene,outcome) %>%filter(q_vec[4]<outcome & outcome<=q_vec[5])%>% dplyr::select(-outcome)


  pheatmap(p34[r1[["labels"]][1:12],],
           fontsize_col = 10,
           fontsize_row = 10,
           display_numbers = F,
           number_color = "black", 
           cluster_cols = c2,
           cluster_rows = F,
           color=colorRampPalette(c("green", "black", "red"))(50),
           fontsize_number = 6,#
           border_color = "black",
           show_colnames = T, show_rownames = T,
           main = "34 selected gene expression in outcome 1st quantile")


  pheatmap(p34[r1[["labels"]][13:24],],
           fontsize_col = 10,
           fontsize_row = 10,
           display_numbers = F,
           number_color = "black", 
           cluster_cols = c2,
           cluster_rows = F,
           color=colorRampPalette(c("green", "black", "red"))(50),
           fontsize_number = 6,#
           border_color = "black",
           show_colnames = T, show_rownames = T,
           main = "34 selected gene expression in outcome 2nd quantile")
  
  pheatmap(p34[r1[["labels"]][25:36],],
           fontsize_col = 10,
           fontsize_row = 10,
           display_numbers = F,
           number_color = "black", 
           cluster_cols = c2,
           cluster_rows = F,
           color=colorRampPalette(c("green", "black", "red"))(50),
           fontsize_number = 6,#
           border_color = "black",
           show_colnames = T, show_rownames = T,
           main = "34 selected gene expression in outcome 3rd quantile")
  
  pheatmap(p34[r1[["labels"]][37:49],],
           fontsize_col = 10,
           fontsize_row = 10,
           display_numbers = F,
           number_color = "black", 
           cluster_cols = c2,
           cluster_rows = F,
           color=colorRampPalette(c("green", "black", "red"))(50),
           fontsize_number = 6,#
           border_color = "black",
           show_colnames = T, show_rownames = T,
           main = "34 selected gene expression in outcome 4th quantile")
  

