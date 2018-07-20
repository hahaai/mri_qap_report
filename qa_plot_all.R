setwd('/Users/lei.ai/Documents/projects/MRI_QAP_report')
source("qa_plot_functions.R")
library(gridExtra)

# some settings
average_san_type = TRUE
rm_outlier= TRUE
release_num='All'

# load Subject list for individusl release
#sublist_RU_R1	<- read.csv("Sublist_RU_R1.txt",header = FALSE,col.names = 'Participant') # anatomical
sublist_RU_R2	<- read.csv("Sublist_RU_R2.txt",header = FALSE,col.names = 'Participant') # anatomical
sublist_RU_R2$Participant=sub('-','_',sublist_RU_R2$Participant)

sublist_RU_R3	<- read.csv("Sublist_RU_R3.txt",header = FALSE,col.names = 'Participant') # anatomical
sublist_RU_R3$Participant=sub('-','_',sublist_RU_R3$Participant)

sublist_RU_R4	<- read.csv("Sublist_RU_R4.txt",header = FALSE,col.names = 'Participant') # anatomical
sublist_RU_R4$Participant=sub('-','_',sublist_RU_R4$Participant)

sublist_CBIC_R3	<- read.csv("Sublist_CBIC_R3.txt",header = FALSE,col.names = 'Participant') # anatomical
sublist_CBIC_R3$Participant=sub('-','_',sublist_CBIC_R3$Participant)

sublist_CBIC_R4	<- read.csv("Sublist_CBIC_R4.txt",header = FALSE,col.names = 'Participant') # anatomical
sublist_CBIC_R4$Participant=sub('-','_',sublist_CBIC_R4$Participant)


#' ## Read in Data
#' Along with reading the data, we setup descriptions that will be associated
#' with each column and used as the label for the y-axis.
#+ anat-read
df1 			<- read.csv("RU_qap_anatomical_spatial.csv") # anatomical
#df <- subset(df,site =='Rutgers'| site == 'HBN-SS'| site =='CoRR')
#df <- subset(df,site == 'HBN-SS'| site =='CoRR')

# handling subject- release number
if (release_num=='R3'){
  sublist=sublist_RU_R3
}
if (release_num=='R4'){
  sublist=sublist_RU_R4
}
if (release_num=='All'){
  sublist=as.data.frame(df1$Participant)
  colnames(sublist)='Participant'
}
df1=subset(df1, Participant %in% sublist$Participant)

# delete anat_t1
df1=df1[! df1$Series=='anat_t1',]
df1=df1[! df1$Series=='anat_t2',]

df1$site[df1$Series=='T1w_HCP']='HCP_RU'

df_Rutgers=data.frame(df1$CNR)
colnames(df_Rutgers)='anat_cnr'
df_Rutgers$anat_efc=df1$EFC
df_Rutgers$anat_fber=df1$FBER
df_Rutgers$anat_fwhm=df1$FWHM
df_Rutgers$anat_qi1=df1$Qi1
df_Rutgers$anat_snr=df1$SNR
df_Rutgers$site=df1$site
#df_Rutgers <- subset(df_Rutgers,site == 'HCP_RU')
df_Rutgers$site     <- factor(sub("_", " ", as.character(df_Rutgers$site)))

# read CBIC data
df1 			<- read.csv("CBIC_qap_anatomical_spatial.csv") # anatomical

# handling subject- release number
if (release_num=='R3'){
  sublist=sublist_CBIC_R3
}
if (release_num=='R4'){
  sublist=sublist_CBIC_R4
}
if (release_num=='All'){
  sublist=as.data.frame(df1$Participant)
  colnames(sublist)='Participant'
}
df1=subset(df1, Participant %in% sublist$Participant)

df1$site=as.character(df1$Site)
# delete anat_t1
df1=df1[! df1$Series=='anat_t1',]
df1$site[grepl('T1w_HCP',df1$Series)]='HCP_CBIC'
df1$site[grepl('T1w_VNav',df1$Series)]='VNav_CBIC'
df1$site[grepl('T1w_VNavNorm',df1$Series)]='VNavNorm_CBIC'


#df <- subset(df,site =='Rutgers'| site == 'HBN-SS'| site =='CoRR')
#df <- subset(df,site == 'HBN-SS'| site =='CoRR')
df_CBIC=data.frame(df1$CNR)
colnames(df_CBIC)='anat_cnr'
df_CBIC$anat_efc=df1$EFC
df_CBIC$anat_fber=df1$FBER
df_CBIC$anat_fwhm=df1$FWHM
df_CBIC$anat_qi1=df1$Qi1
df_CBIC$anat_snr=df1$SNR
df_CBIC$site=df1$site
#df_CBIC <- subset(df_CBIC,site == 'CBIC')
df_CBIC$site     <- factor(sub("_", " ", as.character(df_CBIC$site)))

df = rbind(df_Rutgers,df_CBIC)
#levels(df$site) = c("CBIC","RU")

qa.measures <- colnames(df)[grep("^anat_", colnames(df))]

qa.descs    <- list(
  anat_cnr  = "CNR", 
  anat_efc  = "EFC", 
  anat_fber = "FBER", 
  anat_fwhm = "FWHM", 
  anat_qi1  = "QI1",
  anat_snr = "SNR",
  func_efc  = "Entropy Focus Criterion", 
  func_fber = "FBER", 
  func_fwhm = "Smoothness of Voxels", 
  func_gsr  = "Ghost-to-Signal Ratio", 
  func_dvars    = "Standardized DVARS", 
  func_outlier  = "Fraction of Outlier Voxels", 
  func_quality  = "Mean Distance to Median Volume", 
  func_mean_fd  = "Mean FD",
  func_gcor = "Global Correlation",
  Quality_Mean = "Quality",
  Fraction_of_Outliers = "Outliers Detection",
  FWHM = "FWHM"
)


#' ## Plot each measure
#' Now we plot the data. Note that here we are removing outliers with values
#' greater than 3 times the IQR relative to the 25% or 75% mark.
#+ anat-plot, fig.width=8, fig.height=5, dpi=100
#for (measure in qa.measures)
i=1
p = list()
for (measure in qa.measures) {
  desc <- qa.descs[[measure]]
  p[[i]]=plot_measure(df, measure, desc, site.col="site", plot=FALSE, 
                      outfile=NULL, rm.outlier=rm_outlier)
  i=i+1
}






############################# functional data


#' ## Read in Data
#' Along with reading the data, we setup descriptions that will be associated
#' with each column and used as the label for the y-axis.
#+ func-spat-read
tmp1=read.csv("RU_qap_functional_spatial.csv")
#tmp1=tmp1[! (grepl('func_rest',tmp1$Series) & ! grepl('func_resting',tmp1$Series)) ,]
tmp1=tmp1[! grepl('func_resting',tmp1$Series) ,]

tmp1$mark=paste(tmp1$Participant,tmp1$Series)


tmp2=read.csv("RU_qap_functional_temporal.csv")
#tmp2=tmp2[! (grepl('func_rest',tmp2$Series) & ! grepl('func_resting',tmp2$Series)) ,]
tmp2=tmp2[! grepl('func_resting',tmp2$Series) ,]

#tmp2=tmp2[! tmp2$Series=='func_rest_run_1',]
#tmp2=tmp2[! tmp2$Series=='func_rest_run_2',]
tmp2$mark=paste(tmp2$Participant,tmp2$Series)

# merge both temporal and spatial together
df1=merge(tmp1,tmp2,by='mark')
df1$Scanning_type=df1$Series.x
df1$Participant=as.character(df1$Participant.x)
#df1 			<- read.csv("RU_func_qap_all.csv")

# handling subject- release number
if (release_num=='R3'){
  sublist=sublist_RU_R3
}
if (release_num=='R4'){
  sublist=sublist_RU_R4
}
if (release_num=='All'){
  sublist=as.data.frame(df1$Participant.x)
  colnames(sublist)='Participant'
}
df1=subset(df1, Participant %in% sublist$Participant)


df_Rutgers=data.frame(df1$EFC)
colnames(df_Rutgers)='func_efc'
df_Rutgers$func_gsr=df1$Ghost_y
df_Rutgers$func_dvars=df1$Std..DVARS..Mean.
df_Rutgers$func_fber=df1$FBER
df_Rutgers$func_mean_fd=df1$RMSD..Mean.
df_Rutgers$func_gcor=df1$GCOR
df_Rutgers$Quality_Mean=df1$Quality..Mean.
df_Rutgers$Fraction_of_Outliers=df1$Fraction.of.Outliers..Mean.
df_Rutgers$FWHM=df1$FWHM
df_Rutgers$site='RU'
df_Rutgers$Scan_type = as.character(df1$Scanning_type)
df_Rutgers$Participant=df1$Participant.x

# manage site information (put site and scantype together). They are soem subejct has 10 minute resting.

df_Rutgers$site[which(df_Rutgers$Scan_type=='func_resting')]="rest_RU"
df_Rutgers$site[grepl('func_resting_run_1',df_Rutgers$Scan_type)]="rest_RU1"
df_Rutgers$site[grepl('func_resting_run_2',df_Rutgers$Scan_type)]="rest_RU2"
df_Rutgers$site[grepl('func_movieDM',df_Rutgers$Scan_type)]="DM_RU"
df_Rutgers$site[grepl('func_movieTP',df_Rutgers$Scan_type)]="TP_RU"
df_Rutgers$site[grepl('func_peer_run_1',df_Rutgers$Scan_type)]="peer_RU1"
df_Rutgers$site[grepl('func_peer_run_2',df_Rutgers$Scan_type)]="peer_RU2"
df_Rutgers$site[grepl('func_peer_run_3',df_Rutgers$Scan_type)]="peer_RU3"
df_Rutgers=df_Rutgers[complete.cases(df_Rutgers),]


if (average_san_type){
  rm('df_Rutgers_tmp')
  for (sub in unique(df_Rutgers$Participant)){
    #print(sub)
    # here the index 1:9 is hard coded.
    xx=as.data.frame(t(colMeans(df_Rutgers[which(as.character(df_Rutgers$Participant)==sub & grepl('rest',df_Rutgers$Scan_type)),1:9])))
    xx$site='rest_RU'
    if (exists('df_Rutgers_tmp')){
      df_Rutgers_tmp=rbind(df_Rutgers_tmp,xx)
    }else{
      df_Rutgers_tmp=xx
    }
    
    xx=as.data.frame(t(colMeans(df_Rutgers[which(as.character(df_Rutgers$Participant)==sub & grepl('peer',df_Rutgers$Scan_type)),1:9])))
    xx$site='peer_RU'
    if (exists('df_Rutgers_tmp')){
      df_Rutgers_tmp=rbind(df_Rutgers_tmp,xx)
    }else{
      df_Rutgers_tmp=xx
    }
    
    xx=as.data.frame(t(colMeans(df_Rutgers[which(as.character(df_Rutgers$Participant)==sub & grepl('movieDM',df_Rutgers$Scan_type)),1:9])))
    xx$site='DM_RU'
    if (exists('df_Rutgers_tmp')){
      df_Rutgers_tmp=rbind(df_Rutgers_tmp,xx)
    }else{
      df_Rutgers_tmp=xx
    }
    
    xx=as.data.frame(t(colMeans(df_Rutgers[which(as.character(df_Rutgers$Participant)==sub & grepl('movieTP',df_Rutgers$Scan_type)),1:9])))
    xx$site='TP_RU'
    if (exists('df_Rutgers_tmp')){
      df_Rutgers_tmp=rbind(df_Rutgers_tmp,xx)
    }else{
      df_Rutgers_tmp=xx
    }
    
    
    
  }
  df_Rutgers_tmp=df_Rutgers_tmp[complete.cases(df_Rutgers_tmp),]
  
  df_Rutgers=df_Rutgers_tmp
}


df_Rutgers$Scan_type=NULL

#Rutgers_Rest <- subset(df_Rutgers,Scan_type =='REST' | Scan_type=='REST1'| Scan_type=='REST2')
#Rutgers_Movie <- subset(df_Rutgers,Scan_type =='MOVIE')
#Rutgers_Movie2 <- subset(df_Rutgers,Scan_type =='MOVIE_2')


#df_Rutgers <- subset(df_Rutgers,site =='Rutgers')
#df <- subset(df,site == 'HBN-SS'| site =='CoRR')
df_Rutgers$site     <- factor(sub("_", " ", as.character(df_Rutgers$site)))




###### SI funcitonal data
tmp1=read.csv("CBIC_qap_functional_spatial.csv")
#tmp1=tmp1[! (grepl('func_rest',tmp1$Series) & ! grepl('func_resting',tmp1$Series)) ,]

tmp1$mark=paste(tmp1$Participant,tmp1$Series)


tmp2=read.csv("CBIC_qap_functional_temporal.csv")
#tmp2=tmp2[! (grepl('func_rest',tmp2$Series) & ! grepl('func_resting',tmp2$Series)) ,]

#tmp2=tmp2[! tmp2$Series=='func_rest_run_1',]
#tmp2=tmp2[! tmp2$Series=='func_rest_run_2',]
tmp2$mark=paste(tmp2$Participant,tmp2$Series)

# merge both temporal and spatial together
df1=merge(tmp1,tmp2,by='mark')
df1$Participant=as.character(df1$Participant.x)
df1$Scanning_type=df1$Series.x
#df1 			<- read.csv("RU_func_qap_all.csv")

# handling subject- release number
if (release_num=='R3'){
  sublist=sublist_CBIC_R3
}
if (release_num=='R4'){
  sublist=sublist_CBIC_R4
}
if (release_num=='All'){
  sublist=as.data.frame(df1$Participant)
  colnames(sublist)='Participant'
}
df1=subset(df1, Participant %in% sublist$Participant)


df_CBIC=data.frame(df1$EFC)
colnames(df_CBIC)='func_efc'
df_CBIC$func_gsr=df1$Ghost_y
df_CBIC$func_dvars=df1$Std..DVARS..Mean.
df_CBIC$func_fber=df1$FBER
df_CBIC$func_mean_fd=df1$RMSD..Mean.
df_CBIC$func_gcor=df1$GCOR
df_CBIC$Quality_Mean=df1$Quality..Mean.
df_CBIC$Fraction_of_Outliers=df1$Fraction.of.Outliers..Mean.
df_CBIC$FWHM=df1$FWHM
df_CBIC$site='RU'
df_CBIC$Scan_type = as.character(df1$Scanning_type)
df_CBIC$Participant=df1$Participant.x
# manage site information (put site and scantype together). They are soem subejct has 10 minute resting.

df_CBIC$site[which(df_CBIC$Scan_type=='func_rest')]="rest_CBIC"
df_CBIC$site[grepl('func_rest_run_1',df_CBIC$Scan_type)]="rest_CBIC1"
df_CBIC$site[grepl('func_rest_run_2',df_CBIC$Scan_type)]="rest_CBIC2"
df_CBIC$site[grepl('func_movieDM',df_CBIC$Scan_type)]="DM_CBIC"
df_CBIC$site[grepl('func_movieTP',df_CBIC$Scan_type)]="TP_CBIC"
df_CBIC$site[grepl('func_peer_run_1',df_CBIC$Scan_type)]="peer_CBIC1"
df_CBIC$site[grepl('func_peer_run_2',df_CBIC$Scan_type)]="peer_CBIC2"
df_CBIC$site[grepl('func_peer_run_3',df_CBIC$Scan_type)]="peer_CBIC3"


if (average_san_type){
  rm('df_CBIC_tmp')
  for (sub in unique(df_CBIC$Participant)){
    #print(sub)
    # here the index 1:9 is hard coded.
    xx=as.data.frame(t(colMeans(df_CBIC[which(as.character(df_CBIC$Participant)==sub & grepl('rest',df_CBIC$Scan_type)),1:9])))
    xx$site='rest_CBIC'
    if (exists('df_CBIC_tmp')){
      df_CBIC_tmp=rbind(df_CBIC_tmp,xx)
    }else{
      df_CBIC_tmp=xx
    }
    
    xx=as.data.frame(t(colMeans(df_CBIC[which(as.character(df_CBIC$Participant)==sub & grepl('peer',df_CBIC$Scan_type)),1:9])))
    xx$site='peer_CBIC'
    if (exists('df_CBIC_tmp')){
      df_CBIC_tmp=rbind(df_CBIC_tmp,xx)
    }else{
      df_CBIC_tmp=xx
    }
    
    xx=as.data.frame(t(colMeans(df_CBIC[which(as.character(df_CBIC$Participant)==sub & grepl('movieDM',df_CBIC$Scan_type)),1:9])))
    xx$site='DM_CBIC'
    if (exists('df_CBIC_tmp')){
      df_CBIC_tmp=rbind(df_CBIC_tmp,xx)
    }else{
      df_CBIC_tmp=xx
    }
    
    xx=as.data.frame(t(colMeans(df_CBIC[which(as.character(df_CBIC$Participant)==sub & grepl('movieTP',df_CBIC$Scan_type)),1:9])))
    xx$site='TP_CBIC'
    if (exists('df_CBIC_tmp')){
      df_CBIC_tmp=rbind(df_CBIC_tmp,xx)
    }else{
      df_CBIC_tmp=xx
    }
    
    
    
  }
  df_CBIC_tmp=df_CBIC_tmp[complete.cases(df_CBIC_tmp),]
  
  df_CBIC=df_CBIC_tmp
}

df_CBIC=df_CBIC[complete.cases(df_CBIC),]
df_CBIC$Scan_type=NULL

df_CBIC$site     <- factor(sub("_", " ", as.character(df_CBIC$site)))





df = rbind(df_Rutgers,df_CBIC)
# plot orders
levels(df$site) = c("DM RU", "DM CBIC", "TP RU" ,"TP CBIC", "rest RU","rest CBIC","peer RU","peer CBIC")
# not plot mean_fd > 2
#df$func_mean_fd[df$func_mean_fd>2]=NA


qa.measures <- c(
  "func_efc", "func_gsr",
  "func_dvars", "func_fber", "func_mean_fd", "func_gcor",
  "Quality_Mean","Fraction_of_Outliers","FWHM"
)


#' ## Plot each measure
#' Now we plot the data. Note that here we are removing outliers with values
#' greater than 3 times the IQR relative to the 25% or 75% mark.
#+ func-spat-plot, fig.width=8, fig.height=5, dpi=100


for (measure in qa.measures) {
  desc <- qa.descs[[measure]]
  p[[i]]=plot_measure(df, measure, desc, site.col="site", plot=FALSE, 
                      outfile=NULL, rm.outlier=rm_outlier)
  i=i+1
}


#quartz(title="QA plot",width=180/25.4,height=7)
#do.call("grid.arrange",c(p,ncol=3))

outputname=paste0("qaplots_HBN_release#_",release_num,'_AverageScanType_',average_san_type,"_RmOutlier_",rm_outlier,".pdf")

pdf(file=outputname,title="QA plot",width=180/25,height=10,family="ArialMT",paper="special")

grid.newpage()
layout = grid.layout(7, 6,heights=unit(c(2,5,5,2,5,5,5),"null"))

pushViewport(viewport(layout=layout))
grid.text('Morphometry', vp=viewport(layout.pos.row=1,layout.pos.col=1:6), gp=gpar(fontsize=12,fontfamily="ArialMT",fontface="bold"))
grid.text('fMRI', vp=viewport(layout.pos.row=4,layout.pos.col=1:6), gp=gpar(fontsize=12,fontfamily="ArialMT",fontface="bold"))

print(p[[1]], vp = viewport(layout.pos.row = 2, layout.pos.col = 1:2))
print(p[[2]], vp = viewport(layout.pos.row = 2, layout.pos.col = 3:4))
print(p[[3]], vp = viewport(layout.pos.row = 2, layout.pos.col = 5:6))
print(p[[4]], vp = viewport(layout.pos.row = 3, layout.pos.col = 1:2))
print(p[[5]], vp = viewport(layout.pos.row = 3, layout.pos.col = 3:4))
print(p[[6]], vp = viewport(layout.pos.row = 3, layout.pos.col = 5:6))

print(p[[7]], vp = viewport(layout.pos.row = 5, layout.pos.col = 1:2))
print(p[[8]], vp = viewport(layout.pos.row = 5, layout.pos.col = 3:4))
print(p[[9]], vp = viewport(layout.pos.row = 5, layout.pos.col = 5:6))
print(p[[10]], vp = viewport(layout.pos.row = 6, layout.pos.col = 1:2))
print(p[[11]], vp = viewport(layout.pos.row = 6, layout.pos.col = 3:4))
print(p[[12]], vp = viewport(layout.pos.row = 6, layout.pos.col = 5:6))
print(p[[13]], vp = viewport(layout.pos.row = 7, layout.pos.col = 1:2))
print(p[[14]], vp = viewport(layout.pos.row = 7, layout.pos.col = 3:4))
print(p[[15]], vp = viewport(layout.pos.row = 7, layout.pos.col = 5:6))
dev.off()

