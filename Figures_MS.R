
## Code needed to generate each figure is indicated in the sections below. 
## This is only for a subset of the figures. See additional files in repository for remainder of data.

## library(ggplot2), library(ggsci), library(gridExtra), library(ggsignif)
## library(RColorBrewer) library(colorRamps) library(patchwork)
## library(forcats) library(dplyr) library(ggrepel)
## library(png) library(ggpubr)
## library(immunarch)
## theme_bw()
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

### Figure 1 ###
#1A#
#Image describing the study protocol
#1B#
#Variant counts
#1C#
#Waterfall plots for GBM & BrMET
#1D#
#CNV plots for tumors combined.

### Figure 2 ###

#2A#
#Variant clonality plots
#2B#
#Variant proportions

#2C#
Fig2c<-ggplot(single_site, aes(x=single_site[,2], y=single_site[,1]))+theme(plot.title = element_text(hjust=0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13), axis.title.x = element_blank(), legend.title = element_blank())+geom_boxplot(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.2),aes(color=single_site[,3]), size=2.5)+scale_color_manual(breaks=c("Breast Cancer", "Melanoma", "NSCLC", "SCLC", "Primary GBM", "Recurrent GBM"), values=mypal6)+scale_y_continuous(breaks=c(0.2, 0.4, 0.6, 0.8, 1.0), limits=c(0,1.2))+labs(y="Fraction Variants Identified", title="Single Site Sequencing")+geom_signif(comparisons=list(c(1,2), c(1,3)), map_signif_level = TRUE, y_position=c(1.05, 1.15))

#2D#
# Mets
Fig2dL<-ggplot(Multi_Region_Mets, aes(x=Multi_Region_Mets[,2], y=Multi_Region_Mets[,1]))+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13),axis.title.x = element_blank(), legend.title=element_blank())+geom_violin(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.2),aes(color=Multi_Region_Mets[,3]), size=2.5)+scale_color_manual(breaks=c("Breast Cancer", "Melanoma", "NSCLC", "SCLC"), values=mypal4)+scale_y_continuous(breaks=c(250, 500, 750), limits=c(100,1000))+labs(y="Total Variants Identified", title="BrMET")+geom_signif(annotations="N.S.", test=t.test, comparisons=list(c(1,3), y_position=950), map_signif_level = TRUE)+scale_x_discrete(labels=c("1 Site", "2 Sites", "3 Sites"))
# GBMs
Fig2dR<-ggplot(Multi_Region_GBMs, aes(x=Multi_Region_GBMs[,2], y=Multi_Region_GBMs[,1]))+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13),axis.title.x = element_blank(), legend.title=element_blank())+geom_violin(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.2),aes(color=Multi_Region_GBMs[,3]), size=2.5)+scale_color_manual(breaks=c("Primary GBM", "Recurrent GBM"), values=mypal2)+scale_y_continuous(breaks=c(50,100,150,200), limits=c(0,250))+labs(y="Total Variants Identified", title="GBM")+geom_signif(test=t.test, comparisons=list(c(1,3)), map_signif_level = TRUE, y_position=235)+scale_x_discrete(labels=c("1 Site", "2 Sites", "3 Sites"))

Fig2d<-Fig2dL+Fig2dR+plot_layout(guides='collect')

# Violin Options
Fig2dL_violin<-ggplot(Multi_Region_Mets, aes(x=Multi_Region_Mets[,2], y=Multi_Region_Mets[,1]))+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13),axis.title.x = element_blank(), legend.title=element_blank())+geom_violin(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.2),aes(color=Multi_Region_Mets[,3]), size=2.5)+scale_color_manual(breaks=c("Breast Cancer", "Melanoma", "NSCLC", "SCLC"), values=mypal4)+scale_y_continuous(breaks=c(250, 500, 750), limits=c(100,1000))+labs(y="Total Variants Identified", title="BrMET")+geom_signif(annotations="N.S.", test=t.test, comparisons=list(c(1,3), y_position=950), map_signif_level = TRUE)+scale_x_discrete(labels=c("1 Site", "2 Sites", "3 Sites"))

Fig2dR_violin<-ggplot(Multi_Region_GBMs, aes(x=Multi_Region_GBMs[,2], y=Multi_Region_GBMs[,1]))+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13),axis.title.x = element_blank(), legend.title=element_blank())+geom_violin(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.2),aes(color=Multi_Region_GBMs[,3]), size=2.5)+scale_color_manual(breaks=c("Primary GBM", "Recurrent GBM"), values=mypal2)+scale_y_continuous(breaks=c(50,100,150,200), limits=c(0,250))+labs(y="Total Variants Identified", title="GBM")+geom_signif(test=t.test, comparisons=list(c(1,3)), map_signif_level = TRUE, y_position=235)+scale_x_discrete(labels=c("1 Site", "2 Sites", "3 Sites"))

Fig2d_violin<-Fig2dR_violin+Fig2dL_violin+plot_layout(guides='collect')

#2E#
#CNV quantification

### Figure 3 ###

#3A#
# Class I Neoantigen clonality plots

#3B#
# Class I Neoantigen proportions

# 3C #
# Mets Class I
Fig3c_Met<-ggplot(multi_regions_neoantigens_mets, aes(x=multi_regions_neoantigens_mets[,3], y=multi_regions_neoantigens_mets[,1]))+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13),axis.title.x = element_blank(), legend.title=element_blank())+geom_violin(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.1),aes(color=multi_regions_neoantigens_mets[,4]), size=2.5)+scale_color_manual(breaks=c("Breast Cancer", "Melanoma", "NSCLC", "SCLC"), values=mypal4, "Tumor Type")+scale_y_continuous(breaks=c(100, 200, 300), limits=c(0,400))+labs(y="Total Class I Neoantigens Identified", title="BrMET")+geom_signif(test=t.test, comparisons=list(c(1,3)), map_signif_level = TRUE, y_position=375, annotations="N.S.")+scale_x_discrete(labels=c("1 Site", "2 Sites", "3 Sites"))

# GBMs Class I
Fig3c_GBM<-ggplot(multi_regions_neoantigens_gbms, aes(x=multi_regions_neoantigens_gbms[,3], y=multi_regions_neoantigens_gbms[,1]))+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13),axis.title.x = element_blank(), legend.title=element_blank())+geom_violin(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.1),aes(color=multi_regions_neoantigens_gbms[,4]), size=2.5)+scale_color_manual(breaks=c("Primary GBM", "Recurrent GBM"), values=mypal2, "Tumor Type")+scale_y_continuous(breaks=c(20,40,60,80), limits=c(0,100))+labs(y="Total Class I Neoantigens Identified", title="GBM")+geom_signif(test=t.test, comparisons=list(c(1,3)), map_signif_level = TRUE)+scale_x_discrete(labels=c("1 Site", "2 Sites", "3 Sites"))

Fig3c<-Fig3c_GBM+Fig3c_Met+plot_layout(guides='collect')

# 3D #
# CT Antigen Score Heat Maps
Fig3d_GBM<-ggplot(GBM_CT_Scores, aes(x=GBM_CT_Scores[,3], y=GBM_CT_Scores[,2], fill=GBM_CT_Scores[,1]))+geom_tile()+xlim(levels(fct_reorder(GBM_CT_Scores$Tumor,GBM_CT_Scores$Position, min)))+ylim(rev(levels(GBM_CT_Scores[,2])))+theme(plot.title=element_text(hjust=0.5),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(angle=65, hjust=1), axis.title.x.bottom=element_blank(), axis.ticks.y=element_blank(), legend.position = "right", text = element_text(size=14), legend.title = element_blank())+scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu")), limit = c(-12, 16))+geom_vline(xintercept = line_vector_GBM)+labs(title="GBM")

Fig3d_BrMET<-ggplot(Met_CT_Scores, aes(x=Met_CT_Scores[,3], y=Met_CT_Scores[,2], fill=Met_CT_Scores[,1]))+geom_tile()+xlim(levels(fct_reorder(Met_CT_Scores$Tumor,Met_CT_Scores$Position, min)))+ylim(rev(levels(Met_CT_Scores[,2])))+theme(plot.title=element_text(hjust=0.5),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(angle=65, hjust=1), axis.title.x.bottom=element_blank(),
axis.ticks.y=element_blank(), legend.position = "right", text = element_text(size=14), legend.title = element_blank())+scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu")), limit = c(-12, 16))+geom_vline(xintercept = line_vector_Mets)+labs(title="BrMET")

Fig3d<-Fig3d_GBM+Fig3d_BrMET+plot_layout(widths=c(49,31), guides='collect')

# 3E #
# CT Antigen Scatter Plot -- Make one combined.
Fig3e<-ggplot(Combined_CT_Scatter, aes(x=Combined_CT_Scatter[,2], y=Combined_CT_Scatter[,3], shape=Combined_CT_Scatter[,4], color=Combined_CT_Scatter[,4]))+geom_point()+geom_text_repel(label=Combined_CT_Scatter[,1], size=3.2, show.legend=FALSE)+scale_color_manual(values=CT_pal)+theme_light()+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=10), legend.title=element_blank())+labs(x="CT Antigen Score", y="Intratumoral Variance")+guides(shape=guide_legend(override.aes=list(size=3.5)))

### Figure 4 ###
# 4A #
# Heat map of Danaher Scores #


# 4B #
# PD-L1 #
TPM_PDL1<-ggplot(transformed_TPM, aes(x=transformed_TPM[,5], y=transformed_TPM[,1]))+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13),axis.title.x = element_blank())+geom_violin(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.2),aes(color=transformed_TPM[,6]), size=2.5)+scale_color_manual(breaks=c("Breast Cancer", "Melanoma", "NSCLC", "SCLC", "Primary GBM", "Recurrent GBM"), values=mypal6, "Tumor Type")+labs(y="Log(TPM)", title="PD-L1")+geom_signif(comparisons=list(c("BrMET", "GBM")), map_signif_level = TRUE, y_position=5.2)+scale_y_continuous(breaks=c(1,2,3,4,5), limits=c(0,5.5))
# CXCL9 #
TPM_CXCL9<-ggplot(transformed_TPM, aes(x=transformed_TPM[,5], y=transformed_TPM[,2]))+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13),axis.title.x = element_blank())+geom_violin(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.2),aes(color=transformed_TPM[,6]), size=2.5)+scale_color_manual(breaks=c("Breast Cancer", "Melanoma", "NSCLC", "SCLC", "Primary GBM", "Recurrent GBM"), values=mypal6, "Tumor Type")+labs(y="Log(TPM)", title="CXCL9")+geom_signif(comparisons=list(c("BrMET", "GBM")), map_signif_level = TRUE, y_position=4.2)+scale_y_continuous(breaks=c(1,2,3,4), limits=c(0,4.5))
# P2RY12 #
ggplot(transformed_TPM, aes(x=transformed_TPM[,5], y=transformed_TPM[,4]))+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13),axis.title.x = element_blank())+geom_violin(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.2),aes(color=transformed_TPM[,6]), size=1.5)+scale_color_manual(breaks=c("Breast Cancer", "Melanoma", "NSCLC", "SCLC", "Primary GBM", "Recurrent GBM"), values=mypal6, "Tumor Type")+labs(y="Log(TPM)", title="P2RY12")+geom_signif(comparisons=list(c("BrMET", "GBM")), map_signif_level = TRUE, y_position=6.2)+scale_y_continuous(breaks=c(1,2,3,4,5,6), limits=c(0,6.5))
# TMEM119 #
ggplot(transformed_TPM, aes(x=transformed_TPM[,5], y=transformed_TPM[,3]))+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13),axis.title.x = element_blank())+geom_violin(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.2),aes(color=transformed_TPM[,6]), size=1.5)+scale_color_manual(breaks=c("Breast Cancer", "Melanoma", "NSCLC", "SCLC", "Primary GBM", "Recurrent GBM"), values=mypal6, "Tumor Type")+labs(y="Log(TPM)", title="TMEM119")+geom_signif(comparisons=list(c("BrMET", "GBM")), map_signif_level = TRUE, y_position=6.2)+scale_y_continuous(breaks=c(1,2,3,4,5,6), limits=c(0,6.5))

Averaged_TPM<-TPM_PDL1+TPM_CXCL9+plot_layout(guides='collect')

### Regional ###
# PD-L1 #
Regional_TPM_PDL1<-ggplot(transformed_regional_TPM, aes(x=transformed_regional_TPM[,5], y=transformed_regional_TPM[,1]))+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13),axis.title.x = element_blank())+geom_violin(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.2),aes(color=transformed_regional_TPM[,6]), size=2.5)+scale_color_manual(breaks=c("Breast Cancer", "Melanoma", "NSCLC", "SCLC", "Primary GBM", "Recurrent GBM"), values=mypal6, "Tumor Type")+labs(y="Log(TPM)", title="PD-L1")+geom_signif(comparisons=list(c("BrMET", "GBM")), map_signif_level = TRUE, y_position=5.45, annotations="**")+scale_y_continuous(breaks=c(1,2,3,4,5), limits=c(0,5.6))
# CXCL9 #
Regional_TPM_CXCL9<-ggplot(transformed_regional_TPM, aes(x=transformed_regional_TPM[,5], y=transformed_regional_TPM[,2]))+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13),axis.title.x = element_blank())+geom_violin(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.2),aes(color=transformed_regional_TPM[,6]), size=2.5)+scale_color_manual(breaks=c("Breast Cancer", "Melanoma", "NSCLC", "SCLC", "Primary GBM", "Recurrent GBM"), values=mypal6, "Tumor Type")+labs(y="Log(TPM)", title="CXCL9")+geom_signif(comparisons=list(c("BrMET", "GBM")), map_signif_level = TRUE, y_position=4.5, annotations="**")+scale_y_continuous(breaks=c(1,2,3,4), limits=c(0,4.5))


# 4C #
# Macrophage ontogeny #
#M2-M1 Skewing
Fig4d_L<-ggplot(Macrophage_Analysis, aes(x=Macrophage_Analysis[,5], y=Macrophage_Analysis[,2]-Macrophage_Analysis[,1]))+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13),axis.title.x = element_blank(), legend.title=element_blank())+geom_boxplot(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.2),aes(color=Macrophage_Analysis[,6]), size=2.5)+scale_color_manual(breaks=c("Breast Cancer", "Melanoma", "NSCLC", "SCLC", "Primary GBM", "Recurrent GBM"), values=mypal6)+labs(y="M2-M1 Skew")
#MG-MDM Skewing
Fig4d_R<-ggplot(Macrophage_Analysis, aes(x=Macrophage_Analysis[,5], y=Macrophage_Analysis[,4]-Macrophage_Analysis[,3]))+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13),axis.title.x = element_blank(), legend.title=element_blank())+geom_boxplot(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.2),aes(color=Macrophage_Analysis[,6]), size=2.5)+scale_color_manual(breaks=c("Breast Cancer", "Melanoma", "NSCLC", "SCLC", "Primary GBM", "Recurrent GBM"), values=mypal6)+labs(y="MDM-Microglia Skew")+geom_signif(comparisons=list(c("BrMET", "GBM")), map_signif_level = TRUE)+scale_y_continuous(breaks=c(0,2,4), limits=c(-2,5.5))

Fig4d<-Fig4d_L+Fig4d_R+plot_layout(guides='collect')

## By region ##

Fig4d_L<-ggplot(Macrophage_Analysis_by_Region, aes(x=Macrophage_Analysis_by_Region[,5], y=Macrophage_Analysis_by_Region[,2]-Macrophage_Analysis_by_Region[,1]))+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13),axis.title.x = element_blank(), legend.title=element_blank())+geom_boxplot(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.2),aes(color=Macrophage_Analysis_by_Region[,6]), size=2.5)+scale_color_manual(breaks=c("Breast Cancer", "Melanoma", "NSCLC", "SCLC", "Primary GBM", "Recurrent GBM"), values=mypal6)+labs(y="M2-M1 Skew")+geom_signif(comparisons=list(c("BrMET", "GBM")), map_signif_level=TRUE, test="t.test")

Fig4d_R<-ggplot(Macrophage_Analysis_by_Region, aes(x=Macrophage_Analysis_by_Region[,5], y=Macrophage_Analysis_by_Region[,4]-Macrophage_Analysis_by_Region[,3]))+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13),axis.title.x = element_blank(), legend.title=element_blank())+geom_boxplot(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.2),aes(color=Macrophage_Analysis_by_Region[,6]), size=2.5)+scale_color_manual(breaks=c("Breast Cancer", "Melanoma", "NSCLC", "SCLC", "Primary GBM", "Recurrent GBM"), values=mypal6)+labs(y="M2-M1 Skew")+geom_signif(comparisons=list(c("BrMET", "GBM")), map_signif_level=TRUE, test="t.test")

Fig4d<-Fig4d_L+Fig4d_R+plot_layout(guides='collect')

# 4D #
# Regional Danaher PCA #
# Construct PCA for all samples.
regional_pca<-prcomp(Danaher_by_Region[,1:15], center=TRUE, scale.=TRUE)
regional_pca_graph<-ggbiplot(regional_pca, ellipse=TRUE, groups=Danaher_by_Region$Patient, var.axes=FALSE)+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), text = element_text(size=13))

# Plot only GBMs/Mets onto original PCA.
regional_pca_mets_graph<-ggbiplot(pca_only_mets, ellipse=TRUE, groups=Danaher_by_Region[Danaher_by_Region$`Tumor Type`=="BrMET",]$Patient, var.axes=FALSE)+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), text = element_text(size=13))
regional_pca_gbms_graph<-ggbiplot(pca_only_gbms, ellipse=TRUE, groups=Danaher_by_Region[Danaher_by_Region$`Tumor Type`!="BrMET",]$Patient, var.axes=FALSE)+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), text = element_text(size=13))

# PCA with only samples >3 regions.
regional_pca_greater3<-prcomp(Danaher_by_Region[Danaher_by_Region$Regions>2,1:15], center=TRUE, scale.=TRUE)
regional_pca_greater3_graph<-ggbiplot(regional_pca_greater3, ellipse=TRUE, groups=Danaher_by_Region[Danaher_by_Region$Regions>2,]$Patient, var.axes=FALSE)+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), text = element_text(size=13))
regional_pca_greater3_mets_graph<-ggbiplot(regional_pca_greater3_only_mets, ellipse=TRUE, groups=Danaher_by_Region[Danaher_by_Region$`Tumor Type`=="BrMET" & Danaher_by_Region$Regions>2,]$Patient, var.axes=FALSE)+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), text = element_text(size=13))+scale_color_manual(values=brewer.pal(12,"Paired"))+xlim(-2,4.2)+ylim(-3,3)
regional_pca_greater3_gbms_graph<-ggbiplot(regional_pca_greater3_only_gbms, ellipse=TRUE, groups=Danaher_by_Region[Danaher_by_Region$`Tumor Type`!="BrMET" & Danaher_by_Region$Regions>2,]$Patient, var.axes=FALSE)+theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), text = element_text(size=13))+scale_color_manual(values=c(recurrent_pal, brewer.pal(10,"Paired")))+xlim(-2,4.2)+ylim(-3,3)

### Figure 5 ###
# Analysis done in Immunarch

# 5A #
# Clonotype Repertoire Space
# imm_top is the data frame
line_vector_immarch<-c(3.5, 6.5, 9.5, 12.5, 15.5, 18.5, 21.5, 25.5, 28.5, 31.5, 34.5, 37.5, 40.5, 42.5, 44.5, 46.5, 49.5, 52.5)
Fig5b_base<-vis(imm_top)

Fig5b<-Fig5b_base+scale_fill_manual(values=immarch_pal, labels=c("Clone 1", "Clones 2-5", "Clones 6-20", "Clones 21-100", "Clones 101-1000", "Clones 1000+"))+theme(legend.title=element_blank(), plot.subtitle = element_blank(), plot.title = element_blank(), axis.title.x = element_blank())+labs(y="Percentage of Repertoire")+geom_vline(xintercept=line_vector_immarch, color="darkslategray")

# 5B #
# T cell clonality & Fraction
# T cell fraction
Fig5a_L<-ggplot(Adaptive_Analyzer, aes(x=Adaptive_Analyzer[,1], y=Adaptive_Analyzer[,6]))+theme(plot.title = element_blank(), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13),axis.title.x = element_blank(),legend.title=element_blank())+geom_boxplot(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.2),aes(color=Adaptive_Analyzer[,2]), size=2.5)+scale_color_manual(breaks=c("Breast Cancer", "Melanoma", "NSCLC", "SCLC", "Primary GBM", "Recurrent GBM"), values=mypal6, "Tumor Type")+labs(y="T Cell Fraction", title="T Cell Fraction")+geom_signif(comparisons=list(c("BrMET", "GBM")), map_signif_level = TRUE, test="t.test")+scale_y_continuous(limits=c(0, 0.125))
# T cell Clonality
Fig_5a_R<-ggplot(Adaptive_Analyzer, aes(x=Adaptive_Analyzer[,1], y=Adaptive_Analyzer[,3]))+theme(plot.title = element_blank(), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13),axis.title.x = element_blank(),legend.title=element_blank())+geom_boxplot(fill="grey90", outlier.shape=NA)+geom_jitter(position=position_jitter(0.2),aes(color=Adaptive_Analyzer[,2]), size=2.5)+scale_color_manual(breaks=c("Breast Cancer", "Melanoma", "NSCLC", "SCLC", "Primary GBM", "Recurrent GBM"), values=mypal6, "Tumor Type")+labs(y="Simpson Clonality", title="Simpson Clonality")+geom_signif(comparisons=list(c("BrMET", "GBM")), map_signif_level = TRUE, test="t.test")+scale_y_continuous(limits=c(0, 0.35))

Fig5a<-Fig5a_L+Fig5a_R+plot_layout(guides='collect')


# 5C #
# Clonotype Heat Maps #
BrMET009<-ggplot(BrMET009_Clonotypes, aes(x=BrMET009_Clonotypes[,3], y=reorder(BrMET009_Clonotypes[,2], BrMET009_Clonotypes[,1]), fill=BrMET009_Clonotypes[,1]))+geom_tile()+theme_bw()+theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_text(angle=65, hjust=1), axis.title.x.bottom=element_blank(), axis.ticks.y=element_blank(), legend.position = "right", text = element_text(size=14), legend.title = element_blank())+scale_fill_gradientn(colors = (brewer.pal(9, "YlOrRd")), limit = c(0, 0.165))+labs(title="BrMET009", y="Clonotype")
BrMET018<-ggplot(BrMET018_Clonotypes, aes(x=BrMET018_Clonotypes[,3], y=reorder(BrMET018_Clonotypes[,2], BrMET018_Clonotypes[,1]), fill=BrMET018_Clonotypes[,1]))+geom_tile()+theme_bw()+theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_text(angle=65, hjust=1), axis.text.y.left = element_blank(), axis.title.y=element_blank(), axis.title.x.bottom=element_blank(), axis.ticks.y=element_blank(), legend.position = "right", text = element_text(size=14), legend.title = element_blank())+scale_fill_gradientn(colors = (brewer.pal(9, "YlOrRd")), limit = c(0, 0.165))+labs(title="BrMET018")
GBM074<-ggplot(GBM074_Clonotypes, aes(x=GBM074_Clonotypes[,3], y=reorder(GBM074_Clonotypes[,2], GBM074_Clonotypes[,1]), fill=GBM074_Clonotypes[,1]))+geom_tile()+theme_bw()+theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_text(angle=65, hjust=1), axis.text.y.left = element_blank(), axis.title.y=element_blank(), axis.title.x.bottom=element_blank(), axis.ticks.y=element_blank(), legend.position = "right", text = element_text(size=14), legend.title = element_blank())+scale_fill_gradientn(colors = (brewer.pal(9, "YlOrRd")), limit = c(0, 0.165))+labs(title="GBM074")
GBM079<-ggplot(GBM079_Clonotypes, aes(x=GBM079_Clonotypes[,3], y=reorder(GBM079_Clonotypes[,2], GBM079_Clonotypes[,1]), fill=GBM079_Clonotypes[,1]))+geom_tile()+theme_bw()+theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_text(angle=65, hjust=1), axis.text.y.left = element_blank(), axis.title.y=element_blank(), axis.title.x.bottom=element_blank(), axis.ticks.y=element_blank(), legend.position = "right", text = element_text(size=14), legend.title = element_blank())+scale_fill_gradientn(colors = (brewer.pal(9, "YlOrRd")), limit = c(0, 0.165))+labs(title="GBM079")
GBM055<-ggplot(GBM055_Clonotypes, aes(x=GBM055_Clonotypes[,3], y=reorder(GBM055_Clonotypes[,2], GBM055_Clonotypes[,1]), fill=GBM055_Clonotypes[,1]))+geom_tile()+theme_bw()+theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_text(angle=65, hjust=1), axis.text.y.left = element_blank(), axis.title.y=element_blank(), axis.title.x.bottom=element_blank(), axis.ticks.y=element_blank(), legend.position = "right", text = element_text(size=14), legend.title = element_blank())+scale_fill_gradientn(colors = (brewer.pal(9, "YlOrRd")), limit = c(0, 0.18))+labs(title="GBM055")

 Fig5c<-BrMET009+BrMET018+GBM074+GBM079+plot_layout(guides='collect', nrow=1)

# 5D #
Fig5d_GBM<-vis(Morisita_Overlap_GBMs, .text.size=0)+theme(plot.title=element_text(size=16, hjust=0.5), axis.title.x = element_blank(), axis.title.y=element_blank())+labs(title="GBM")
Fig5d_BrMET<-vis(Morisita_Overlap_Mets, .text.size=0)+theme(legend.position="none", plot.title=element_text(size=16, hjust=0.5), axis.title.x = element_blank(), axis.title.y=element_blank())+labs(title="BrMET")

Fig5d<-Fig5d_GBM+Fig5d_BrMET+plot_layout(guides='collect')

### Figure 6 ###

#6A -- Description of clinical course

#6B -- Variant counts per tumor
Fig6b<-ggplot(GBM065_Variants, aes(x=GBM065_Variants[,2], y=GBM065_Variants[,1], fill=GBM065_Variants[,3]))+geom_bar(stat="identity")+theme(plot.title = element_text(hjust=0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=13), axis.title.x = element_blank(), legend.position = "none")+labs(y="Variant Count")+scale_fill_manual(values=mypal2)

#6C -- Variant UpSet plot

#6D -- Heat Map for GBM065.Re TCR_Overlaps
Fig6d<-ggplot(GBM065_Clonotypes, aes(x=GBM065_Clonotypes[,3], y=reorder(GBM065_Clonotypes[,2], GBM065_Clonotypes[,1]), fill=GBM065_Clonotypes[,1]))+geom_tile()+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_text(angle=65, hjust=1), axis.text.y=element_text(), axis.title.x.bottom=element_blank(), axis.ticks.y=element_blank(), legend.position = "right", text = element_text(size=14), legend.title = element_blank())+scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd"), limit = c(0, 0.20))+labs(y="Clonotype")

#6E -- Single-cell UMAP.

#6F -- Volcano Plot#
#Use the definition of hyperexpanded that encompasses top 3 clones. Then compare differential expression of genes in 'hyperexpanded' to 'rest'
#Only plot genes that are at 1% frequency in each
#To correspond to a p-value-adjusted of 0.05, place a cut-off of 5*10^-6. Works for our data set as the p-value cut-off.
library(ggrepel)
Fig6f<-ggplot(Volcano_Data, aes(x=Volcano_Data[,2], y=-log10(Volcano_Data[,1]), col=Volcano_Data[,6], label=Volcano_Data[,7]))+geom_point(alpha=Volcano_Data[,8])+geom_text_repel(size=2.2, box.padding=0.4)+theme(legend.position="none",plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white", colour = "black", size=1), panel.grid.major = element_line(colour = "grey90"), text = element_text(size=13))+labs(y=expression('-log'[10]('P-Value')), x=expression('log'[2]('Fold Change')))+scale_color_manual(values=c("dodgerblue1", "gray20", "firebrick3"))+scale_x_continuous(limits=c(-2.7,2.7))
