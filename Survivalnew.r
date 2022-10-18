#Import the required libraries
library('survival')
library('survminer')
library('tidyr')
#Import the dataset
data=read.csv("C:/Users/arkaj/OneDrive/Documents/pancancerInfo.csv",header=T,row.names=1)
data
data = data %>% drop_na() #Drop na values
colnames(data)[2] = "age" #Convert the name of age column
colnames(data)[9] = 'OS' #Convert the name of Delay column to OS

#Replace male and female to 1 and 0
data$gender[data$gender == 'MALE'] = 1
data$gender[data$gender == 'FEMALE'] = 0

#Select the data that includes BLCA type of cancer
df = data[data$type == 'BLCA',] 
#Remove Column values Unknown and others from race and treatment column
df = df[df$race != '[Unknown]' & df$race != '[Not Evaluated]' & df$race != '[Not Available]',] 
df = df[df$treatment_outcome_first_course != '[Unknown]' & df$race != '[Not Applicable]' & df$treatment_outcome_first_course != '[Not Available]',] 
#age column to less than and greater than 50
df$age = ifelse(df$age>50,1,0) 

#Now plot the survival plot based on different Attributes

#AGE
fit=survfit(Surv(OS,Event)~age,data=df)
fit
ggsurvplot(fit,data=data)
ggsurvplot(fit,data=data,surv.median.line='hv')
ggsurvplot(fit,data=data,surv.median.line='hv',pval = T)
ggsurvplot(fit,data=data,surv.median.line='hv',pval = T,risk.table = T)


#RACE
fit=survfit(Surv(OS,Event)~race,data=df)
fit
ggsurvplot(fit,data=data)
ggsurvplot(fit,data=data,surv.median.line='hv')
ggsurvplot(fit,data=data,surv.median.line='hv',pval = T)
ggsurvplot(fit,data=data,surv.median.line='hv',pval = T,risk.table = T)

#AGE AND RACE
fit=survfit(Surv(OS,Event)~race + age,data=df)
fit
ggsurvplot(fit,data=data)
ggsurvplot(fit,data=data,surv.median.line='hv')
ggsurvplot(fit,data=data,surv.median.line='hv',pval = T)
ggsurvplot(fit,data=data,surv.median.line='hv',pval = T,risk.table = T)

#GENDER
fit=survfit(Surv(OS,Event)~gender,data=df)
fit
ggsurvplot(fit,data=data)
ggsurvplot(fit,data=data,surv.median.line='hv')
ggsurvplot(fit,data=data,surv.median.line='hv',pval = T)
ggsurvplot(fit,data=data,surv.median.line='hv',pval = T,risk.table = T)


#GENDER AND RACE
fit=survfit(Surv(OS,Event)~gender + race,data=df)
fit
ggsurvplot(fit,data=data)
ggsurvplot(fit,data=data,surv.median.line='hv')
ggsurvplot(fit,data=data,surv.median.line='hv',pval = T)
ggsurvplot(fit,data=data,surv.median.line='hv',pval = T,risk.table = T)

#Treatment 
fit=survfit(Surv(OS,Event)~treatment_outcome_first_course,data=df)
fit
ggsurvplot(fit,data=data)
ggsurvplot(fit,data=data,surv.median.line='hv')
ggsurvplot(fit,data=data,surv.median.line='hv',pval = T)
ggsurvplot(fit,data=data,surv.median.line='hv',pval = T,risk.table = T)

