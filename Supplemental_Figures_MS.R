
## Code needed to generate each figure is indicated in the sections below. 
## This is only for a subset of the figures. See additional files in repository for remainder of data.

## library(ggplot2), library(ggsci), library(gridExtra), library(ggsignif)
## library(RColorBrewer) library(colorRamps) library(patchwork)
## library(forcats) library(dplyr) library(ggrepel)
## library(png) library(ggpubr)
## library(immunarch)
## scale_fill_npg()
##

#6-color palette#
## mypal6<-pal_npg("nrc")(5)
## mypal6[6]="#7E6148FF"

mypal6<-c("#E64B35FF", "#7E6148FF", "#00A087FF", "#4DBBD5FF", "#3C5488FF", "#F39B7FFF")
mypal4<-c("#E64B35FF", "#7E6148FF", "#00A087FF", "#4DBBD5FF")
mypal2<-c("#3C5488FF", "#F39B7FFF")
immarch_pal<-c("#FF4B20", "#FF9F2F", "#DCDFA2", "#A7E6F3", "#62ABED", "#0348A6")

# Positive-negative color palette:
scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu")), limit = c(-12, 16))

#Positive-zero color palette:
scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd"), limit = c(-12, 16))

### Supplemental Figure 1 ###
# Table of clinical characteristics and general stats.

### Supplemental Figure 2 ###
# Mutational spectrum and mutational signatures. Should be all Megan.

### Supplemental Figure 3 ###
# CNV heat maps.

### Supplemental Figure 4 ###
# Distribution of Class II neoantigens. Can be all Megan. Just similar clonality plot and example UpSet plots.

### Supplemental Figure 5 ###
# UpSet plots for variants, class I, and class II neoantigens.

### Supplemental Figure 6 ###
# CT Antigen scores and descriptions of heterogeneity/similarity.

alt_names<-c("1", "BrMET008 2", "3", "4", "1", "BrMET009 2", "3", "1", "BrMET010 2", "3", "1", "BrMET018 2", "3", "1", "BrMET019 2", "3", "1", "BrMET023 2", "3", "1", "BrMET024 2", "3", "BrMET025 1", "2", "1", "BrMET027 2", "3", "1", "BrMET028 2", "3", "BrMET058 2")
Supp_Fig_6a<-ggplot(Met_Histology_CT_Scores, aes(x=Met_Histology_CT_Scores[,3], y=Met_Histology_CT_Scores[,2], fill=Met_Histology_CT_Scores[,1]))+geom_tile()+xlim(levels(fct_reorder(Met_Histology_CT_Scores$Tumor,Met_Histology_CT_Scores$Position, min)))+ylim(rev(levels(Met_Histology_CT_Scores[,2])))+theme(plot.title=element_text(hjust=0.5),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(angle=65, hjust=1), axis.title.x.bottom=element_blank(), axis.ticks.y=element_blank(), legend.position = "right", text = element_text(size=14), legend.title = element_blank())+scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu")), limit = c(-12, 16))+labs(title="BrMET")+geom_vline(xintercept=line_vector_mets)+scale_x_discrete(labels=alt_names)

Supp_Fig_6b<-ggplot(CT_Similarities, aes(x=CT_Similarities[,2], y=CT_Similarities[,1]))+theme(plot.title = element_text(hjust=0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13), axis.title.x = element_blank(), legend.title = element_blank())+geom_boxplot(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.2),aes(color=CT_Similarities[,3]), size=2.5)+scale_color_manual(breaks=c("Breast Cancer", "Melanoma", "NSCLC", "SCLC", "Primary GBM", "Recurrent GBM"), values=mypal6)+scale_y_continuous(breaks=c(0.2, 0.4, 0.6, 0.8, 1.0), limits=c(0,1.1))+labs(y="CT Similarity")+geom_signif(comparisons=list(c("BrMET", "GBM")), map_signif_level = TRUE, y_position=1.05, annotations="N.S.")

### Supplemental Figure 7 ###
# CIBERSORT description & Danaher similarities.

# SF 7A -- CIBERSORT plot from Megan.

# SF 7B -- Danaher ITH and similarities. Originally figure 4F.

# Danaher Similarity
SF7b_R<-ggplot(Similarity_Metrics, aes(x=Similarity_Metrics[,5], y=Similarity_Metrics[,1]))+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13),axis.title.x = element_blank())+geom_boxplot(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.2),aes(color=Similarity_Metrics[,6]), size=2.5)+scale_color_manual(breaks=c("Breast Cancer", "Melanoma", "NSCLC", "SCLC", "Primary GBM", "Recurrent GBM"), values=mypal6, "Tumor Type")+labs(y="Cosine Similarity", title="Intratumoral Danaher Similarity")

# Greater than 3 Triangle Areas
SF7b_L<-ggplot(Similarity_Metrics, aes(x=Similarity_Metrics[,5], y=Similarity_Metrics[,4]))+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13),axis.title.x = element_blank())+geom_boxplot(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.2),aes(color=Similarity_Metrics[,6]), size=2.5)+scale_color_manual(breaks=c("Breast Cancer", "Melanoma", "NSCLC", "SCLC", "Primary GBM", "Recurrent GBM"), values=mypal6, "Tumor Type")+labs(y="Area", title="Danaher ITH")+geom_signif(comparisons=list(c("BrMET", "GBM")), test="t.test", annotations = "N.S.")


### Supplemental Figure 8 ### Data is contained within Figure 5 R object.
# SF 8A -- Occupied repertoire space by clones grouped.
Supp_Fig_8a<-vis(imm_top_alternate, .by="Histology", .meta=tcr_data$meta)+theme(legend.title=element_blank(), plot.title = element_blank(), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13),axis.title.x = element_blank())

# SF 8B -- Cosine similarities maps.
Cosine_GBMs<-vis(Cosine_Similarity_GBMs, .text.size=0)+theme(plot.title=element_text(size=16, hjust=0.5), axis.title.x = element_blank(), axis.title.y=element_blank())+labs(title="GBM")
Cosine_Mets<-vis(Cosine_Similarity_Mets, .text.size=0)+theme(plot.title=element_text(size=16, hjust=0.5), axis.title.x = element_blank(), axis.title.y=element_blank())+labs(title="BrMET")

Supp_Fig_8b<-Cosine_GBMs+Cosine_Mets+plot_layout(guides='collect')

# SF 8C -- Quantifications of similarities (original Fig 5E)
# Overlap Quantification
# Morisita
Fig5e_L<-ggplot(TCR_Overlaps, aes(x=TCR_Overlaps[,3], y=TCR_Overlaps[,1]))+theme(legend.title=element_blank(), plot.title = element_blank(), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13),axis.title.x = element_blank())+geom_boxplot(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.2),aes(color=TCR_Overlaps[,5]), size=2.5)+scale_color_manual(breaks=c("Breast Cancer", "Melanoma", "NSCLC", "SCLC", "Primary GBM", "Recurrent GBM"), values=mypal6)+labs(y="Intratumoral Morisita Overlap")+geom_signif(comparisons=list(c("BrMET", "GBM")), map_signif_level = TRUE, test="t.test")+scale_y_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1.0),limits=c(0, 1.1))
#Cosine
Fig5e_R<-ggplot(TCR_Overlaps, aes(x=TCR_Overlaps[,3], y=TCR_Overlaps[,2]))+theme(legend.title=element_blank(), plot.title = element_blank(), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13),axis.title.x = element_blank())+geom_boxplot(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.2),aes(color=TCR_Overlaps[,5]), size=2.5)+scale_color_manual(breaks=c("Breast Cancer", "Melanoma", "NSCLC", "SCLC", "Primary GBM", "Recurrent GBM"), values=mypal6)+labs(y="Intratumoral Cosine Similarity")+geom_signif(comparisons=list(c("BrMET", "GBM")), map_signif_level = TRUE, test="t.test")+scale_y_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1.0),limits=c(0, 1.1))

Fig5e<-Fig5e_L+Fig5e_R+plot_layout(guides='collect')

### Supplemental Figure 9 ###
# ClonEvol for GBM065.Re.

### Supplemental Figure 10 ###
# Extra scRNA-seq for GBM065.Re.
