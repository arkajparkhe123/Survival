library('survival')
library('survminer')
data=read.csv("C:/Users/arkaj/OneDrive/Documents/pancancerInfo.csv",header=T,row.names=1)
data
library(tidyr)
data = data %>% drop_na()
colnames(data)[2] = "age"
colnames(data)[9] = 'OS'
data$gender[data$gender == 'MALE'] = 1

data$gender[data$gender == 'FEMALE'] = 0

df = data[data$type == 'BLCA',]
#f = subset(data,race != '[Not Available]' | race != '[Unknown]' | race != '[Not Evaluated]')

df = df[df$race != '[Unknown]' & df$race != '[Not Evaluated]' & df$race != '[Not Available]',] 

df = df[df$treatment_outcome_first_course != '[Unknown]' & df$race != '[Not Applicable]' & df$treatment_outcome_first_course != '[Not Available]',] 
df$age = ifelse(df$age>50,1,0)
# library(CatEncoders)
# labs = LabelEncoder.fit(data$vital_status)
# data$vital_status = transform(labs,data$vital_status)



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

