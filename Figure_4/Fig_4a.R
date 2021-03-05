# 11.6.19
# perform danaher method for estimating immune infiltration and make a heatmap of the results

# import packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(ComplexHeatmap)
library(ggsci)
library(rapportools)
library(colorRamps)
library(stringi)
library(RColorBrewer)

get_gbm_order <- function(vector) {
  re_samples <- unique(vector[grepl("Re", vector)])
  not_re_samples <- unique(vector[!grepl("Re", vector)])
  sample_order <- c(not_re_samples, re_samples)
  tumor_order <- unique(sapply(sample_order, function(x) return(substr(x, 1, nchar(x)-2))))
  order_list <- list(sample_order, tumor_order)
  return(sample_order)
}

# set wd
setwd("~/Desktop/GBM/immune_infiltration/")
## for expression: log2(fpkm+0.001) / log2(tpm+0.001)

# 9.21.20
######### KALLISTO GENE EXPRESSION EXCEL FILE TO MATRIX FOR XCELL AND CIBERSORT ###########
excel_file <- read_excel("kallisto_abundance_matrix_strandfix.xlsx")
samples <- names(excel_file[,2:ncol(excel_file)])
# filter kallisto file by danaher gene set
dgene <- read_excel("danaher_analysis/danaher_gene_set.xlsx")
excel_file <- filter(excel_file, gene_name %in% dgene$Gene)
# merge two files to include Cell Types
final_df <- merge(dgene, excel_file, by.x = "Gene", by.y = "gene_name")
# which gene not present in kallisto output?
not_included_sample <- filter(dgene, !Gene %in% excel_file$gene_name)
# log2 + 1 transform final_df
final_df_norm <- data.frame(final_df[,c(1:2)], sapply(final_df[samples], function(x) log2(x + 1)))
# gather samples to average expression per cell type
final_gather <- gather(final_df_norm, key="Samples", value="Norm. Expression", -Gene, -Cell.Type)
# get correct sample order
final_gather$Samples <- str_replace(final_gather$Samples, 'Re.', 'Re-')
final_gather$Samples[!grepl("Re", final_gather$Samples)] <- str_replace(final_gather$Samples[!grepl("Re", final_gather$Samples)], '\\.', '-')
final_gather$Samples <- gsub("-", "  ", final_gather$Samples)
final_gather$Samples <- factor(final_gather$Samples, levels = get_gbm_order(final_gather$Samples))
final_gather <- final_gather[order(final_gather$Samples), ]
# cell type order 
final_gather$Cell.Type <- factor(final_gather$Cell.Type, levels = rev(c("B-cells", "CD45", "CD8 T cells", "Cytotoxic cells", "Exhausted CD8", "T-cells", "Th1 cells", "Treg", "DC", "Macrophages", "Mast cells", "Neutrophils", "NK cells", "NK CD56dim cells")))
final_gather <- final_gather[order(final_gather$Cell.Type), ]
# change colname for legend title
names(final_gather) <- c("Gene", "Cell Type", "Samples", "log2(TPM + 1)")
# final heatmap approach
danaher_heatmap <- ggplot(final_gather, aes(x=Samples, y=`Cell Type`, fill=`log2(TPM + 1)`)) + geom_tile() + geom_vline(xintercept=c(4.5,7.5,10.5,13.5,16.5,19.5,22.5,24.5,27.5,30.5,31.5,33.5,36.5,39.5,41.5,42.5,45.5,46.5,49.5,52.5,55.5,58.5,60.5,63.5,66.5,69.5,72.5,76.5,78.5)) + theme_bw() + theme(axis.text.x=element_text(angle=65, hjust=1), axis.title.y=element_blank(), axis.title.x.bottom=element_blank(), axis.ticks.y=element_blank(), legend.position = "right", text = element_text(size=8)) + scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd"), limit = c(0,12))

