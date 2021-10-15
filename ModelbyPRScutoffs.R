################################################################
################################################################
###Analysis for one site
#fit the linear regression model:
#volume ~ PRS + age + age^2+ gender + ICV + PC1-4 
#scale all continuous variables including outcomes
################################################################
################################################################


cat("Start:\n",file="R_log.txt",sep="\n")

cat("Load package tableone\n",file="R_log.txt",append=TRUE)
library(tableone)

cat("Load Data files\n",file="R_log.txt",append=TRUE)

################################################################
# Analysis using all PRS cut-off for each outcome
# Output all results
################################################################
options(warn=2) #set warning as errors

####PRS data
dt_prs1 <- read.csv("dt_prs_avghippo.csv",stringsAsFactors=FALSE,header=TRUE)
dt_prs2 <- read.csv("dt_prs_avghippo2.csv",stringsAsFactors=FALSE,header=TRUE)
dt_prs3 <- read.csv("dt_prs_avgthal.csv",stringsAsFactors=FALSE,header=TRUE)
dt_prs4 <- read.csv("dt_prs_avgcaud.csv",stringsAsFactors=FALSE,header=TRUE)
dt_prs5 <- read.csv("dt_prs_avgput.csv",stringsAsFactors=FALSE,header=TRUE)
dt_prs6 <- read.csv("dt_prs_avgpal.csv",stringsAsFactors=FALSE,header=TRUE)
dt_prs7 <- read.csv("dt_prs_avgamyg.csv",stringsAsFactors=FALSE,header=TRUE)
dt_prs8 <- read.csv("dt_prs_avgaccumb.csv",stringsAsFactors=FALSE,header=TRUE)

dt_prs_all <- list(dt_prs1, dt_prs2, dt_prs3, dt_prs4, dt_prs5, dt_prs6, dt_prs7, dt_prs8)
dt_prs_all_org<-dt_prs_all

####other data
dt_otherthanPRS <- read.csv("dt_otherthanPRS.csv",stringsAsFactors=FALSE,header=TRUE)
dt_otherthanPRS_org<-dt_otherthanPRS

###outcome names
outcomenames <- c("Avg_hippo", "Avg_hippo2", "Avg_thal", "Avg_caud", 
"Avg_put", "Avg_pal", "Avg_amyg", "Avg_accumb")

###PRS cutoff names
prs_cutoff <- seq(from=0.0001, to=1.0001, by=0.001)
prs_cutoff2 <- paste0('X',format(prs_cutoff[1:length(prs_cutoff)],scientific=F))
 
###scale all continuous variables including outcomes
dt_otherthanPRS$agesq<-dt_otherthanPRS$age^2
dt_otherthanPRS$current_sx_org<-dt_otherthanPRS$current_sx
scale_var<-c(outcomenames,"current_sx","age","agesq","ICV","PC1","PC2","PC3","PC4")
dt_otherthanPRS[,scale_var] <-scale(dt_otherthanPRS[,scale_var])

for(i in 1:8){dt_prs_all[[i]][, prs_cutoff2]<-scale(dt_prs_all[[i]][, prs_cutoff2])}

#summary(dt_otherthanPRS)

################################################################
#Analysis
################################################################
cat("Start Analysis\n",file="R_log.txt",append=TRUE)

time1<-Sys.time()

analysisbyPRS<-list()

for(j in 1:length(prs_cutoff)){

results_all<-c()
results1_all<-c()
results2_all<-c()
results3_all<-c()
results4_all<-c()
results5_all<-c()
results6_all<-c()
results7_all<-c()
results8_all<-c()
results9_all<-c()
results10_all<-c()
results11_all<-c()


for(i in 1:length(outcomenames)){

prs_dat<-dt_prs_all[[i]][, c(prs_cutoff2,"IID")]
datforanalysis<-merge(dt_otherthanPRS,prs_dat,by=c("IID"))
#check missingness
#summary(datforanalysis[,1:25])

##m1: prs only without PTSD
############################################################
mod_prs_noPTSD <-lm(datforanalysis[,paste(outcomenames[i])]~datforanalysis[,paste(prs_cutoff2[j])]+ age+agesq+gender+ICV+PC1+PC2+PC3+PC4,
data=datforanalysis)

#compute total numbers
varlist<-c(outcomenames[i], prs_cutoff2[j],"age","gender","ICV",
"PC1","PC2","PC3","PC4")
temp<-datforanalysis[, varlist]
temp<-na.omit(temp)#remove any rows contains missing data
ntotal<-nrow(temp)

results<-summary(mod_prs_noPTSD)$coef[2,c("Estimate","Std. Error","t value","Pr(>|t|)")]
results<-c(results, ntotal)
names(results)<-c("m1_beta_prs","m1_se_prs","m1_t_prs","m1_p_prs","m1_total_npts")
results1_all<-rbind(results1_all, results)


#m2: prs with PTSD
################################################################
mod_prs_PTSD<-lm(datforanalysis[,paste(outcomenames[i])]~datforanalysis[,paste(prs_cutoff2[j])]+current_dx+age+agesq+gender+ICV+PC1+PC2+PC3+PC4,data=datforanalysis)

results_prs<-summary(mod_prs_PTSD)$coef[2,c("Estimate","Std. Error","t value","Pr(>|t|)")]
results_ptsd<-summary(mod_prs_PTSD)$coef[3,c("Estimate","Std. Error","t value","Pr(>|t|)")]

#combine all results
results<-c(results_prs,results_ptsd)
names(results)<-c("m2_beta_prs","m2_se_prs","m2_t_prs","m2_p_prs","m2_beta_ptsd","m2_se_ptsd","m2_t_ptsd","m2_p_ptsd")
results2_all<-rbind(results2_all, results)


#m3: prs with PTSD and interaction
################################################################
mod_prs_PTSD_inter <-lm(datforanalysis[,paste(outcomenames[i])]
~datforanalysis[,paste(prs_cutoff2[j])]*current_dx+age+agesq+gender+ICV+PC1+PC2+PC3+PC4,data= datforanalysis)

results_prs<-summary(mod_prs_PTSD_inter)$coef[2,c("Estimate","Std. Error","t value","Pr(>|t|)")]
results_ptsd<-summary(mod_prs_PTSD_inter)$coef[3,c("Estimate","Std. Error","t value","Pr(>|t|)")]
results_inter<-summary(mod_prs_PTSD_inter)$coef["datforanalysis[, paste(prs_cutoff2[j])]:current_dx",c("Estimate","Std. Error","t value","Pr(>|t|)")]

#compute number of cases and controls
varlist<-c(outcomenames[i], prs_cutoff2[j],"current_dx","age","gender","ICV",
"PC1","PC2","PC3","PC4")
temp<-datforanalysis[, varlist]
temp<-na.omit(temp)#remove any rows contains missing data
cases<-sum(as.numeric(temp$current_dx==1))
controls<-sum(as.numeric(temp$current_dx==0))
df<-cases+controls-nrow(summary(mod_prs_PTSD_inter)$coef)
numobs<-c(cases,controls,df) 

#combine all results
results<-c(results_prs, results_ptsd, results_inter, numobs)
names(results)<-c("m3_beta_prs","m3_se_prs","m3_t_prs","m3_p_prs","m3_beta_ptsd","m3_se_ptsd","m3_t_ptsd","m3_p_ptsd","m3_beta_inter","m3_se_inter","m3_t_inter","m3_p_inter","m3_cases","m3_controls","m3_df")

results3_all<-rbind(results3_all, results)

#m4: prs with Childhood Trauma
#Childhood Trauma: 0,1,2 scores
################################################################
mod_prs_childtra<-lm(datforanalysis[,paste(outcomenames[i])]~datforanalysis[,paste(prs_cutoff2[j])]+child_trauma+age+agesq+gender+ICV+PC1+PC2+PC3+PC4,data=datforanalysis)

results_prs<-summary(mod_prs_childtra)$coef[2,c("Estimate","Std. Error","t value","Pr(>|t|)")]
results_childtra<-summary(mod_prs_childtra)$coef[3,c("Estimate","Std. Error","t value","Pr(>|t|)")] 

#combine all results
results<-c(results_prs, results_childtra)
names(results)<-c("m4_beta_prs","m4_se_prs","m4_t_prs","m4_p_prs","m4_beta_childtra","m4_se_childtra","m4_t_childtra","m4_p_childtra")
results4_all<-rbind(results4_all, results)


#m5: prs with Childhood Trauma and interaction
#Childhood Trauma : 0,1,2 scores
################################################################
mod_prs_childtra_inter <-lm(datforanalysis[,paste(outcomenames[i])]
~datforanalysis[,paste(prs_cutoff2[j])]*child_trauma+age+agesq+gender+ICV+PC1+PC2+PC3+PC4,data= datforanalysis)

results_prs<-summary(mod_prs_childtra_inter)$coef[2,c("Estimate","Std. Error","t value","Pr(>|t|)")]
results_childtra<-summary(mod_prs_childtra_inter)$coef[3,c("Estimate","Std. Error","t value","Pr(>|t|)")]
results_inter<-summary(mod_prs_childtra_inter)$coef["datforanalysis[, paste(prs_cutoff2[j])]:child_trauma",c("Estimate","Std. Error","t value","Pr(>|t|)")]

#compute number of cases and controls
varlist<-c(outcomenames[i], prs_cutoff2[j],"child_trauma","age","gender","ICV",
"PC1","PC2","PC3","PC4")
temp<-datforanalysis[, varlist]
temp<-na.omit(temp)#remove any rows contains missing data
cases2<-sum(as.numeric(temp$child_trauma==2))
cases1<-sum(as.numeric(temp$child_trauma==1))
controls<-sum(as.numeric(temp$child_trauma==0))
df<-cases1+cases2+controls-nrow(summary(mod_prs_childtra_inter)$coef)
numobs<-c(cases2,cases1,controls,df) 

#combine all results
results<-c(results_prs, results_childtra, results_inter, numobs)
names(results)<-c("m5_beta_prs","m5_se_prs","m5_t_prs","m5_p_prs","m5_beta_childtra","m5_se_childtra","m5_t_childtra","m5_p_childtra","m5_beta_inter","m5_se_inter","m5_t_inter","m5_p_inter","m5_cases2","m5_cases1","m5_controls","m5_df")

results5_all<-rbind(results5_all, results)


#m6: prs with Childhood Trauma
#Childhood Trauma: 0,1 scores
################################################################
mod_prs_childtra2<-lm(datforanalysis[,paste(outcomenames[i])]~datforanalysis[,paste(prs_cutoff2[j])]+child_trauma2+age+agesq+gender+ICV+PC1+PC2+PC3+PC4,data=datforanalysis)

results_prs<-summary(mod_prs_childtra2)$coef[2,c("Estimate","Std. Error","t value","Pr(>|t|)")]
results_childtra2<-summary(mod_prs_childtra2)$coef[3,c("Estimate","Std. Error","t value","Pr(>|t|)")] 

#combine all results
results<-c(results_prs, results_childtra2)
names(results)<-c("m6_beta_prs","m6_se_prs","m6_t_prs","m6_p_prs","m6_beta_childtra2","m6_se_childtra2","m6_t_childtra2","m6_p_childtra2")
results6_all<-rbind(results6_all, results)


#m7: prs with Childhood Trauma and interaction
#Childhood Trauma : 0,1 scores
################################################################
mod_prs_childtra2_inter <-lm(datforanalysis[,paste(outcomenames[i])]
~datforanalysis[,paste(prs_cutoff2[j])]*child_trauma2+age+agesq+gender+ICV+PC1+PC2+PC3+PC4, data=datforanalysis)

results_prs<-summary(mod_prs_childtra2_inter)$coef[2,c("Estimate","Std. Error","t value","Pr(>|t|)")]
results_childtra2<-summary(mod_prs_childtra2_inter)$coef[3,c("Estimate","Std. Error","t value","Pr(>|t|)")]
results_inter<-summary(mod_prs_childtra2_inter)$coef["datforanalysis[, paste(prs_cutoff2[j])]:child_trauma2",c("Estimate","Std. Error","t value","Pr(>|t|)")]

#compute number of cases and controls
varlist<-c(outcomenames[i], prs_cutoff2[j],"child_trauma2","age","gender","ICV",
"PC1","PC2","PC3","PC4")
temp<-datforanalysis[, varlist]
temp<-na.omit(temp)#remove any rows contains missing data
cases<-sum(as.numeric(temp$child_trauma2==1))
controls<-sum(as.numeric(temp$child_trauma2==0))
df<-cases+controls-nrow(summary(mod_prs_childtra2_inter)$coef)
numobs<-c(cases,controls,df) 

#combine all results
results<-c(results_prs, results_childtra2, results_inter, numobs)
names(results)<-c("m7_beta_prs","m7_se_prs","m7_t_prs","m7_p_prs","m7_beta_childtra2","m7_se_childtra2","m7_t_childtra2","m7_p_childtra2","m7_beta_inter","m7_se_inter","m7_t_inter","m7_p_inter","m7_cases","m7_controls","m7_df")

results7_all<-rbind(results7_all, results)


#m8: prs with PTSD severity
################################################################
mod_prs_PTSDs<-lm(datforanalysis[,paste(outcomenames[i])]~datforanalysis[,paste(prs_cutoff2[j])]+current_sx+age+agesq+gender+ICV+PC1+PC2+PC3+PC4,data=datforanalysis)

results_prs<-summary(mod_prs_PTSDs)$coef[2,c("Estimate","Std. Error","t value","Pr(>|t|)")]
results_ptsds<-summary(mod_prs_PTSDs)$coef[3,c("Estimate","Std. Error","t value","Pr(>|t|)")]

#combine all results
results<-c(results_prs, results_ptsds)
names(results)<-c("m8_beta_prs","m8_se_prs","m8_t_prs","m8_p_prs","m8_beta_ptsds","m8_se_ptsds","m8_t_ptsds","m8_p_ptsds")
results8_all<-rbind(results8_all, results)


#m9: prs with PTSD severity and interaction
################################################################
mod_prs_PTSDs_inter <-lm(datforanalysis[,paste(outcomenames[i])]
~datforanalysis[,paste(prs_cutoff2[j])]*current_sx+age+agesq+gender+ICV+PC1+PC2+PC3+PC4, data=datforanalysis)

results_prs<-summary(mod_prs_PTSDs_inter)$coef[2,c("Estimate","Std. Error","t value","Pr(>|t|)")]
results_ptsds<-summary(mod_prs_PTSDs_inter)$coef[3,c("Estimate","Std. Error","t value","Pr(>|t|)")]
results_inter<-summary(mod_prs_PTSDs_inter)$coef["datforanalysis[, paste(prs_cutoff2[j])]:current_sx",c("Estimate","Std. Error","t value","Pr(>|t|)")]

#compute total numbers
varlist<-c(outcomenames[i], prs_cutoff2[j],"age","gender","ICV","current_sx",
"PC1","PC2","PC3","PC4")
temp<-datforanalysis[, varlist]
temp<-na.omit(temp)#remove any rows contains missing data
ntotal<-nrow(temp)

#combine all results
results<-c(results_prs, results_ptsds, results_inter, ntotal)
names(results)<-c("m9_beta_prs","m9_se_prs","m9_t_prs","m9_p_prs","m9_beta_ptsds","m9_se_ptsds","m9_t_ptsds","m9_p_ptsds","m9_beta_inter","m9_se_inter","m9_t_inter","m9_p_inter","m9_total_npts")

results9_all<-rbind(results9_all, results)


#m10: prs with PTSD severity - unscaled
################################################################
mod_prs_PTSDs_unscaled<-lm(datforanalysis[,paste(outcomenames[i])]~datforanalysis[,paste(prs_cutoff2[j])]+current_sx_org+age+agesq+gender+ICV+PC1+PC2+PC3+PC4,data=datforanalysis)

results_prs_unscaled<-summary(mod_prs_PTSDs_unscaled)$coef[2,c("Estimate","Std. Error","t value","Pr(>|t|)")]
results_ptsds_unscaled<-summary(mod_prs_PTSDs_unscaled)$coef[3,c("Estimate","Std. Error","t value","Pr(>|t|)")]

#combine all results
results<-c(results_prs_unscaled, results_ptsds_unscaled)
names(results)<-c("m10_beta_prs","m10_se_prs","m10_t_prs","m10_p_prs","m10_beta_ptsds","m10_se_ptsds","m10_t_ptsds","m10_p_ptsds")
results10_all<-rbind(results10_all, results)


#m11: prs with PTSD severity and interaction - unscaled
################################################################
mod_prs_PTSDs_inter_unscaled <-lm(datforanalysis[,paste(outcomenames[i])]
                         ~datforanalysis[,paste(prs_cutoff2[j])]*current_sx_org+age+agesq+gender+ICV+PC1+PC2+PC3+PC4, data=datforanalysis)

results_prs_unscaled<-summary(mod_prs_PTSDs_inter_unscaled)$coef[2,c("Estimate","Std. Error","t value","Pr(>|t|)")]
results_ptsds_unscaled<-summary(mod_prs_PTSDs_inter_unscaled)$coef[3,c("Estimate","Std. Error","t value","Pr(>|t|)")]
results_inter_unscaled<-summary(mod_prs_PTSDs_inter_unscaled)$coef["datforanalysis[, paste(prs_cutoff2[j])]:current_sx_org",c("Estimate","Std. Error","t value","Pr(>|t|)")]

#combine all results
results<-c(results_prs_unscaled, results_ptsds_unscaled, results_inter_unscaled)
names(results)<-c("m11_beta_prs","m11_se_prs","m11_t_prs","m11_p_prs","m11_beta_ptsds","m11_se_ptsds","m11_t_ptsds","m11_p_ptsds","m11_beta_inter","m11_se_inter","m11_t_inter","m11_p_inter")

results11_all<-rbind(results11_all, results)

}


#combine results
################################################################
results_all<-cbind(results1_all, results2_all, results3_all, results4_all, results5_all, results6_all, results7_all, results8_all, results9_all, results10_all, results11_all)
row.names(results_all)<-outcomenames

analysisbyPRS[[j]]<-as.data.frame(results_all)
names(analysisbyPRS)[j]<-paste0("PRS: ", prs_cutoff2[j])
}

time2<-Sys.time()
time2-time1

cat("Time used for the loop:",time2-time1,"\n",file="R_log.txt",append=TRUE)


################################################################
################################################################
###Get Summary Statistics (not scaled, use original scales):
#summary statistics for all predictors and outcomes 
#                  (for freq, report missing values)
################################################################
################################################################

################################################################
### PRS by outcomes: mean, std, ranges
################################################################
cat("Get summary statistics for PRS by outcome\n",file="R_log.txt",append=TRUE)

SummaryStat_PRS<-list()

for(i in 1:length(outcomenames)){

prs_dat<-dt_prs_all_org[[i]][, c(prs_cutoff2)]

summary_prs <- lapply(prs_dat, function(x) rbind( 
                                  mean = mean(x) ,
                                  sd = sd(x) ,
                                  median = median(x),
                                  minimum = min(x),
                                  maximum = max(x),
                                  nmiss = sum(is.na(x)) ))
summary_prs <- as.data.frame(summary_prs)
SummaryStat_PRS[[i]]<-t(summary_prs)
names(SummaryStat_PRS)[i]<-outcomenames[i]
}
#SummaryStat_PRS[[1]]

################################################################
### summary statistics at subject levels
### for all predictors and outcomes 
### for freq: report missing values  
################################################################
cat("Get summary statistics at subject level\n",file="R_log.txt",append=TRUE)

names(dt_otherthanPRS_org)
summary(dt_otherthanPRS_org)

var_cont<-c(
"Avg_hippo", "Avg_hippo2", "Avg_thal", "Avg_caud",  
"Avg_put", "Avg_pal", "Avg_amyg", "Avg_accumb",
"current_sx", "ICV", "age", "PC1", "PC2", "PC3", "PC4")

var_cat<-c("gender", "current_dx", "child_trauma", "child_trauma2")

var_all<-c(var_cont, var_cat)

tab1<-CreateTableOne(var= var_all,data=dt_otherthanPRS_org,factorVars=var_cat)

table_sub1<-print(tab1$ContTable) 
table_sub2<-print(tab1$ContTable, nonnormal = var_cont, minMax=TRUE)
table_sub3<-print(tab1$CatTable, showAllLevels = TRUE, 
missing=TRUE, dropEqual=TRUE)

#Continuous - Mean(Std) 
SummaryStat_pts1<-table_sub1

#Continuous - Median(Range)
SummaryStat_pts2<-table_sub2

#Categorical - Freq(Percent) and missingness
SummaryStat_pts3<-table_sub3

cat("Output summary statistics for subjects as a separate file\n",file="R_log.txt",append=TRUE)

#sart a file
sink("summary_statistics.csv")

cat('Continuous - Mean(Std)')
write.csv(table_sub1)
cat('____________________________')

cat('\n')
cat('\n')

cat('Continuous - Median(Range)')
write.csv(table_sub2)
cat('____________________________')

cat('\n')
cat('\n')

cat('Categorical - Freq(Percent) and missingness')
write.csv(table_sub3)
cat('____________________________')

sink()
#close a file



################################################################
##Interactions Plot: PRS x PTSD
##Avg_hippo2: X0.8891
################################################################

cat("Get stratified plots by PTSD for the best meta PTSD and PRS interaction\n",file="R_log.txt",append=TRUE)

i=2 
j=890
outcomenames[i] #Avg_hippo2
prs_cutoff[j]  #0.8891

prs_dat<-dt_prs_all[[i]][, c(prs_cutoff2,"IID")]
datforanalysis<-merge(dt_otherthanPRS,prs_dat,by=c("IID"))
varkeep <- c("Avg_hippo2", "X0.8891", "current_dx", "age", "agesq", "gender", "ICV", "PC1", "PC2", "PC3", "PC4")
datforanalysis<-datforanalysis[,varkeep]
dim(datforanalysis)  
datforanalysis<-na.omit(datforanalysis)
dim(datforanalysis)  

datforanalysis_case <- datforanalysis[datforanalysis$current_dx==1,]
datforanalysis_control <- datforanalysis[datforanalysis$current_dx==0,]

#original model
################################################################
mod1<-lm(Avg_hippo2~X0.8891+age+agesq+gender+ICV+PC1+PC2+PC3+PC4,data= datforanalysis_case)
mod2<-lm(Avg_hippo2~X0.8891+age+agesq+gender+ICV+PC1+PC2+PC3+PC4,data= datforanalysis_control)

mod1_int <- summary(mod1)$coef[1,1]
mod1_slope <- summary(mod1)$coef[2,1]

mod2_int <- summary(mod2)$coef[1,1]
mod2_slope <- summary(mod2)$coef[2,1]

### save for meta-analysis
formeta_mod1 <- summary(mod1)$coef[1:2,]
formeta_mod2 <- summary(mod2)$coef[1:2,]

#residual model
################################################################

##regress without PRS and PTSD
resid_mod1<-lm(Avg_hippo2~age+agesq+gender+ICV+PC1+PC2+PC3+PC4,data=datforanalysis)
yadj<-resid_mod1$residuals
datforanalysis$yadj<-yadj

datforanalysis_case <- datforanalysis[datforanalysis$current_dx==1,]
datforanalysis_control <- datforanalysis[datforanalysis$current_dx==0,]

##regress on PRS using subsets data with and without PTSD
resid_mod2 <-lm(yadj~X0.8891, dat= datforanalysis_case)
resid_mod3 <-lm(yadj~X0.8891, dat= datforanalysis_control)

### save for meta-analysis
formeta_resid_mod2 <- summary(resid_mod2)$coef
formeta_resid_mod3 <- summary(resid_mod3)$coef

### output data for meta-analysis
################################################################

formeta_mod1 <- data.frame(formeta_mod1)
names(formeta_mod1) <- c("est","se","t","pval")
formeta_mod1$model <- "case"
formeta_mod1$var <- row.names(formeta_mod1)

formeta_mod2 <- data.frame(formeta_mod2)
names(formeta_mod2) <- c("est","se","t","pval")
formeta_mod2$model <- "control"
formeta_mod2$var <- row.names(formeta_mod2)

formeta_resid_mod2 <- data.frame(formeta_resid_mod2)
names(formeta_resid_mod2) <- c("est","se","t","pval")
formeta_resid_mod2$model <- "case_resid"
formeta_resid_mod2$var <- row.names(formeta_resid_mod2)

formeta_resid_mod3 <- data.frame(formeta_resid_mod3)
names(formeta_resid_mod3) <- c("est","se","t","pval")
formeta_resid_mod3$model <- "control_resid"
formeta_resid_mod3$var <- row.names(formeta_resid_mod3)


plotdata_PTSDinter_formeta <- rbind(formeta_mod1, formeta_mod2, formeta_resid_mod2, formeta_resid_mod3)

plotdata_PTSDinter_formeta_Duke <- plotdata_PTSDinter_formeta


### get data from other studies  
################################################################

cat("Interaction Plot - Get data from other studies\n",file="R_log.txt",append=TRUE)

load("ptsdinter_plotdat_main.Rdat")
plotdata_PTSDinter_formeta_main <- plotdata_PTSDinter_formeta

load("ptsdinter_plotdat_TRACTS.Rdat")
plotdata_PTSDinter_formeta_TRACTS <- plotdata_PTSDinter_formeta

load("ptsdinter_plotdat_VETSA.Rdat")
plotdata_PTSDinter_formeta_VETSA <- plotdata_PTSDinter_formeta

load("ptsdinter_plotdat_UKBB.Rdat")
plotdata_PTSDinter_formeta_UKBB <- plotdata_PTSDinter_formeta


### run meta-analyses 
################################################################
cat("Interaction Plot - Run meta-analyses\n",file="R_log.txt",append=TRUE)

library(metafor) #for mixed effect model/EB
library(plyr)

plotdata_PTSDinter_formeta_meta <- c()

for(i in 1:nrow(plotdata_PTSDinter_formeta_main)){
datatemp <- rbind(plotdata_PTSDinter_formeta_main[i,], plotdata_PTSDinter_formeta_TRACTS[i,], plotdata_PTSDinter_formeta_VETSA[i,], plotdata_PTSDinter_formeta_UKBB[i,],plotdata_PTSDinter_formeta_Duke[i,])
modi <-rma(yi=datatemp[,"est"],sei=datatemp[,"se"],method="EB")  
plotdata_PTSDinter_formeta_meta <-rbind(plotdata_PTSDinter_formeta_meta, unlist(summary(modi)[c("beta","se","zval","pval")]))
}

plotdata_PTSDinter_formeta_meta <-  data.frame(plotdata_PTSDinter_formeta_meta)

plotdata_PTSDinter_formeta_meta$model <- plotdata_PTSDinter_formeta_main$model

plotdata_PTSDinter_formeta_meta$var <- plotdata_PTSDinter_formeta_main$var
plotdata_PTSDinter_formeta_meta$var2 <- rep(c("Intercept","Slope"),4)

# intercept, slope for each of below
# formeta_mod1: org model - case
# formeta_mod2: org model - control
# formeta_resid_mod2: resin model - case
# formeta_resid_mod3: resin model - control


### Plots
################################################################
cat("Interaction Plot - Make Plots\n",file="R_log.txt",append=TRUE)

## Org Plot
filename<-paste0("Interaction_PTSD_Hippo2_Duke.png")
png(filename, width = 500, height = 500)

#color: case=2, control=1
plot(datforanalysis$X0.8891, datforanalysis$Avg_hippo2, col=datforanalysis$current_dx+1,
xlab="PRS Cutoff 0.8891",ylab="Hippo2 (scaled)",ylim=c(-3, 3),xlim=c(-3,3))
title("Duke")

abline(mod1_int, mod1_slope, col=2)
abline(mod2_int, mod2_slope, col=1)

tmp <- plotdata_PTSDinter_formeta_meta
mod1_int_meta   <- tmp[tmp$model=="case" & tmp$var2=="Intercept",]$beta 
mod1_slope_meta <- tmp[tmp$model=="case" & tmp$var2=="Slope",]$beta 
mod2_int_meta   <- tmp[tmp$model=="control" & tmp$var2=="Intercept",]$beta 
mod2_slope_meta <- tmp[tmp$model=="control" & tmp$var2=="Slope",]$beta 

#meta: case
abline(mod1_int_meta, mod1_slope_meta, col=2, lty=2)
#meta: control
abline(mod2_int_meta, mod2_slope_meta, col=1, lty=2)

legend(-3, 2.5, legend=c("PTSD", "No PTSD", "PTSD: meta","No PTSD: meta"),
       col=c(2,1,2,1), pch=c(NA,NA,NA,NA), cex=1, bty = "n", lty=c(1,1,2,2))
dev.off()


## Resid Plot
filename<-paste0("Interaction_PTSD_Hippo2_Duke_residual.png")
png(filename, width = 500, height = 500)

plot(datforanalysis$X0.8891, datforanalysis$yadj, col=datforanalysis$current_dx+1,
xlab="PRS Cutoff 0.8891",ylab="Partial Residuals in Hippo2",ylim=c(-3, 3),xlim=c(-3,3))
title("Duke: Residual Plot")

abline(resid_mod2, col=2)
abline(resid_mod3, col=1)

tmp <- plotdata_PTSDinter_formeta_meta
resid_mod2_int_meta   <- tmp[tmp$model=="case_resid" & tmp$var2=="Intercept",]$beta 
resid_mod2_slope_meta <- tmp[tmp$model=="case_resid" & tmp$var2=="Slope",]$beta 
resid_mod3_int_meta   <- tmp[tmp$model=="control_resid" & tmp$var2=="Intercept",]$beta 
resid_mod3_slope_meta <- tmp[tmp$model=="control_resid" & tmp$var2=="Slope",]$beta 

#meta: case
abline(resid_mod2_int_meta, resid_mod2_slope_meta, col=2, lty=2)
#meta: control
abline(resid_mod3_int_meta, resid_mod3_slope_meta, col=1, lty=2)

legend(-3, 2.5, legend=c("PTSD", "No PTSD", "PTSD: meta","No PTSD: meta"),
       col=c(2,1,2,1), pch=c(NA,NA,NA,NA), cex=1, bty = "n", lty=c(1,1,2,2))
dev.off()


################################################################
################################################################
### Output all datasets
################################################################
################################################################
cat("Save final output\n",file="R_log.txt",append=TRUE)

save(list=c("analysisbyPRS", "SummaryStat_PRS", "SummaryStat_pts1", "SummaryStat_pts2", "SummaryStat_pts3", "plotdata_PTSDinter_formeta_Duke", "plotdata_PTSDinter_formeta_meta"),file="finaloutput_onesite.RData")

cat("The End",file="R_log.txt",append=TRUE)


