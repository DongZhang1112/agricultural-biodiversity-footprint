# Recreate Supplementary Figures S1-S3 from archived derived datasets.
# Dependencies: ggplot2. Run from the archive root.
root <- Sys.getenv('NF_DATA_DIR','.')
out <- file.path(root,'reproduced_figures');dir.create(out,showWarnings=FALSE,recursive=TRUE)
if(!requireNamespace('ggplot2',quietly=TRUE)) stop('Install ggplot2')
library(ggplot2)
th <- theme_classic(base_size=11)
s1 <- read.csv(file.path(root,'data','05_country_BFI_period_trends.csv'))
s1$Period <- factor(s1$Period,levels=c('2000-2005','2005-2010','2010-2015','2015-2020'))
p1 <- ggplot(s1,aes(Period,Annual_trend_pct_yr)) + geom_boxplot(width=.55,outlier.shape=NA,fill='grey85',colour='grey30') + geom_jitter(width=.13,height=0,alpha=.22,size=.6,colour='#3E91A3') + geom_hline(yintercept=0,linetype='dashed',colour='grey60') + labs(x='Period',y='Annual change in BFI (% yr^-1)') + th
# Plot all observed values; a long tail may compress the boxes, but no values are hidden.
ggsave(file.path(out,'FigS1_reproduced.pdf'),p1,width=7.5,height=5)
s2 <- read.csv(file.path(root,'data','11_missing_BF_treatment_sensitivity.csv'),check.names=FALSE)
cols <- c('Quantity_effect_km2eq','Within_effect_km2eq','Between_effect_km2eq')
long <- do.call(rbind,lapply(cols,function(n) data.frame(Commodity=s2$Commodity,Treatment=s2$Treatment,Component=n,Value=s2[[n]]/1000)))
p2<-ggplot(long,aes(x=Commodity,y=Value,fill=Treatment))+geom_col(position=position_dodge(width=.75),width=.65)+facet_wrap(~Component,scales='free_y')+geom_hline(yintercept=0,colour='grey55')+labs(x=NULL,y='Contribution (10^3 km2-equivalent)',fill='BF-record treatment')+th+theme(axis.text.x=element_text(angle=25,hjust=1))
ggsave(file.path(out,'FigS2_reproduced.pdf'),p2,width=11,height=5.5)
s3<-read.csv(file.path(root,'data','12_leave_one_country_out_decomposition.csv'))
cols3<-c('quantity_effect','within_country_intensity_effect','between_country_production_share_effect')
lo <- do.call(rbind,lapply(cols3,function(n) data.frame(Commodity=s3$Commodity,Component=n,Value=s3[[n]]/1000)))
p3<-ggplot(lo,aes(x=Commodity,y=Value))+geom_boxplot(fill='grey85',outlier.shape=NA,width=.56)+geom_jitter(width=.1,alpha=.12,size=.35,colour='#3E91A3')+facet_wrap(~Component,scales='free_y')+geom_hline(yintercept=0,colour='grey55')+labs(x=NULL,y='Leave-one-country-out contribution (10^3 km2-equivalent)')+th+theme(axis.text.x=element_text(angle=25,hjust=1))
ggsave(file.path(out,'FigS3_reproduced.pdf'),p3,width=11,height=5.5)
message('Supplementary figures regenerated in ',out)
