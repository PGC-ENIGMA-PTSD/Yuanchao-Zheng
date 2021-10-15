# Yuanchao-Zheng

Instructions


After preparing your data ready, please 

Step 1: 
save all of your input data files under the same folder together with the R code, “ModelbyPRScutoffs.R”. 

Step 2: 
Run R code given below to get the final output file.

setwd("your location")
source("ModelbyPRScutoffs.R")

Step 3: 
Send us the final output file, “finaloutput_onesite.Rdat”.

Step 4 (Optional):
Check output summary statistics (“summary_statistics.csv”) and output log file (“R_log.txt”).




R Software Info
R packages needed	tableone


Input and Output Files
	Filename 
(case sensitivity)	Description
Input Files	dt_otherthanPRS.csv	A csv data file includes all variables EXCEPT PRS. 
Only European subjects are included.
	dt_prs_avghippo.csv	A csv data file includes PRS scores for the outcome of avghippo.
	dt_prs_avghippo2.csv	A csv data file includes PRS scores for the outcome of avghippo2.
	dt_prs_avgthal.csv	A csv data file includes PRS scores for the outcome of avgthal.
	dt_prs_avgcaud.csv	A csv data file includes PRS scores for the outcome of avgcaud.
	dt_prs_avgput.csv	A csv data file includes PRS scores for the outcome of avgput.
	dt_prs_avgpal.csv	A csv data file includes PRS scores for the outcome of avgpal.
	dt_prs_avgamyg.csv	A csv data file includes PRS scores for the outcome of avgamyg.
	dt_prs_avgaccumb.csv	A csv data file includes PRS scores for the outcome of avgaccumb.
Output Files 
 
 	finaloutput_onesite.Rdat	A R dataset includes all output datasets.
	summary_statistics.csv	A csv file contains summary statistics of subjects in the dt_otherthanPRS.csv file.
	R_log.txt	A text file helps track R code running completeness.



Input Data file: each element in dt_prs_* files
Variable 	Type	Description	Variable Name in R 
(case sensitive)
Unique Identifier	Char	the unique identifier in input data files	IID
Polygenic risk scores (PRS)	Num	PRS from using 1001 different p-value cut-offs as output by Adam’s PRSice code. 	0.0001, 0.0011, 0.0021, …, 0.9991, 1.0001 


Input Data file: dt_otherthanPRS
Variable 	Type	Description	Variable Name in R 
(case sensitive)
Unique Identifier	Char	the unique identifier in dt_otherthanPRS and dt_prs1- dt_prs8	IID
Age	Num		age
Gender	Num	values: 0, 1. 0=F and 1=M.	gender
Current PTSD	Num	values: 0, 1.	current_dx
Current PTSD severity score	Num		current_sx
Childhood trauma	Num	values: 0, 1, 2.	child_trauma
Childhood trauma	Num	values: 0, 1.	child_trauma2
1st Principle component 	Num	Principle component computed using European subjects.	PC1 
2nd Principle component 	Num		PC2
3rd Principle component 	Num		PC3
4th Principle component 	Num		PC4
ICV	Num		ICV
Average hippo 	Num		Avg_hippo 
Average hippo2	Num	Identical to average hippo.	Avg_hippo2
Average thal	Num		Avg_thal
Average caud	Num		Avg_caud
Average put	Num		Avg_put
Average pal 	Num		Avg_pal 
Average amyg	Num		Avg_amyg
Average accumb	Num		Avg_accumb



