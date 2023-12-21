#UCO3001 setting data
newdata<-merge(table_DM_3001,table_PRE_3001,by="USUBJID",all.x = TRUE)
newdata<-merge(newdata,table_OUTCOME_3001,by="USUBJID",all.x = TRUE)
table_analy<-newdata

table_analy<-subset(table_analy,select = c(USUBJID,reason_for_stop,reason_for_stop_induc,
                                           biologics,
                                           SEX,AGE,ACTARM,ARM,DISEASE_DURATION,BMI,
                                           CORTICOSTEROID,ASA,MERCAPTOPURINE,AZATHIOPRINE,METHOTREXATE,
                                           TNF_expo,VDZ_expo,
                                           pMAYO_baseline_2,eMAYO_baseline_2,CRP_baseline,FC_baseline,LTF_baseline,
                                           time_CR,time_SF,time_RB,CR_WEEK2,SF_WEEK2,RB_WEEK2,
                                           pMAYO_WEEK_2,CRP_WEEK_2,FC_WEEK_2,
                                           stool_WEEK_2,bleeding_WEEK_2,assessment_WEEK_2,
                                           ER_WEEK8,EI_WEEK8,HR_WEEK8,HEI_WEEK8,ERes_WEEK8,
                                           CR_WEEK52,ER_WEEK52,HR_WEEK52,HEI_WEEK52,ERes_WEEK52))

table_analy<-subset(table_analy,biologics=="induction"|biologics=="miantenance"|biologics=="biologics_PNR")

#disease duration (transform to the mediate data)
#newdata<-subset(MH,MHCAT=="DIAGNOSIS",select = c(USUBJID,MHDTC,MHSTDTC))
#newdata$MHSTDTC<-ifelse(nchar(newdata$MHSTDTC)==10,newdata$MHSTDTC,
#                        ifelse(nchar(newdata$MHSTDTC)==4,
#                        paste(newdata$MHSTDTC,"07-01",sep = "-"),
#                        paste(newdata$MHSTDTC,"15",sep = "-")))
#newdata$MHSTDTC<-as.Date(newdata$MHSTDTC)
#newdata$DISEASE_DURATION<-time_length(interval(newdata$MHSTDTC,newdata$MHDTC),'years')
#newdata<-subset(newdata,select = c(USUBJID,DISEASE_DURATION))
#table_analy<-merge(table_analy,newdata,by="USUBJID",all.x = TRUE)

#treatment group
##induction
attach(table_analy)
table_analy$treatment_induc<-ifelse(substring(ARM,1,11)=="Ust 6 (390)","Ust 6 (390)",
                                   ifelse(substring(ARM,1,11)=="Ust 6 (260)","Ust 6 (260)",
                                  ifelse(substring(ARM,1,11)=="Ust 6 (520)","Ust 6 (520)",
                                ifelse(substring(ARM,1,7)=="Ust 130","Ust 130","XXXX"))))
detach(table_analy)
##maintanence
table_analy$treatment<-table_analy$ACTARM

table_analy$treatment<-ifelse(table_analy$treatment=="Ust 130-R8-Ust 90 Q8W"|
                                table_analy$treatment=="Ust 130-Ust 90-DR16-Ust 90 Q8W",
                              "Ust 130-R8-Ust 90 Q8W", table_analy$treatment)
table_analy$treatment<-ifelse(table_analy$treatment=="Ust 6 (260)-R8-Ust 90 Q8W"|
                                table_analy$treatment=="Ust 6 (260)-Ust 90-DR16-Ust 90 Q8W",
                              "Ust 6 (260)-R8-Ust 90 Q8W", table_analy$treatment)
table_analy$treatment<-ifelse(table_analy$treatment=="Ust 6 (390)-R8-Ust 90 Q8W"|
                              table_analy$treatment=="Ust 6 (390)-Ust 90-DR16-Ust 90 Q8W",
                              "Ust 6 (390)-R8-Ust 90 Q8W", table_analy$treatment)
table_analy$treatment<-ifelse(table_analy$treatment=="Ust 6 (520)-R8-Ust 90 Q8W"|
                             table_analy$treatment=="Ust 6 (520)-Ust 90-DR16-Ust 90 Q8W",
                              "Ust 6 (520)-R8-Ust 90 Q8W", table_analy$treatment)

#biologics exposure
table_analy$TNF_expo<-ifelse(table_analy$TNF_expo=="NEVER USED","0","1")
table_analy$VDZ_expo<-ifelse(table_analy$VDZ_expo=="NEVER USED","0","1")

#loss follow-up=failure
##keep original outcomes
table_analy$CR_WEEK52_OR<-as.factor(table_analy$CR_WEEK52)
#table_analy$EI_WEEK52_OR<-as.factor(table_analy$EI_WEEK52)
table_analy$ER_WEEK52_OR<-as.factor(table_analy$ER_WEEK52)
table_analy$HR_WEEK52_OR<-as.factor(table_analy$HR_WEEK52)
#table_analy$HI_WEEK52_OR<-as.factor(table_analy$HI_WEEK52)
table_analy$HEI_WEEK52_OR<-as.factor(table_analy$HEI_WEEK52)

##week-8 outcomes
failure<-function(x){ifelse(is.na(table_analy$reason_for_stop_induc),x,
                            ifelse(is.na(x),"1",x))}
#table_analy$CR_WEEK8<-as.factor(failure(table_analy$CR_WEEK8))
table_analy$ER_WEEK8<-as.factor(failure(table_analy$ER_WEEK8))
table_analy$HR_WEEK8<-as.factor(failure(table_analy$HR_WEEK8))
table_analy$HEI_WEEK8<-as.factor(failure(table_analy$HEI_WEEK8))
#table_analy$CRes_WEEK8<-as.factor(failure(table_analy$CRes_WEEK8))
table_analy$ERes_WEEK8<-as.factor(failure(table_analy$ERes_WEEK8))
#table_analy$EI_WEEK8<-as.factor(failure(table_analy$EI_WEEK8))

##one-year outcomes
failure<-function(x){ifelse(is.na(table_analy$reason_for_stop),x,
                            ifelse(is.na(x),"1",x))}
#table_analy$CR_WEEK52<-as.factor(failure(table_analy$CR_WEEK52))
#table_analy$EI_WEEK52<-as.factor(failure(table_analy$EI_WEEK52))
table_analy$ER_WEEK52<-as.factor(failure(table_analy$ER_WEEK52))
table_analy$HR_WEEK52<-as.factor(failure(table_analy$HR_WEEK52))
#table_analy$HI_WEEK52<-as.factor(failure(table_analy$HI_WEEK52))
table_analy$HEI_WEEK52<-as.factor(failure(table_analy$HEI_WEEK52))

#patients for one-year outcomes analysis (N=2)
table_analy$long_outcome<-ifelse(table_analy$biologics=="miantenance",
                                 ifelse(!is.na(table_analy$ER_WEEK52)|
                                 !is.na(table_analy$HR_WEEK52),"1","0"),"0")

#add new avraibles
attach(table_analy)
table_analy$CRes_WEEK_2<-ifelse((pMAYO_baseline_2- pMAYO_WEEK_2)>2,"1","0")
table_analy$FCR_WEEK_2<-ifelse(FC_WEEK_2<250,"1","0")
table_analy$CRPR_WEEK_2<-ifelse(CRP_WEEK_2<5,"1","0")
table_analy$biologics_expo<-ifelse(TNF_expo=="1","1",ifelse(VDZ_expo=="1","1","0"))
detach(table_analy)

attach(table_analy)
table_analy$CR_CRes_WEEK2<-ifelse(CR_WEEK2=="1","1",
                                  ifelse(CRes_WEEK_2=="1","2","3"))
table_analy$CR_FCR_WEEK2<-ifelse(CR_WEEK2=="1","1",
                                 ifelse(FCR_WEEK_2=="1","2","3"))
table_analy$CR_CRPR_WEEK2<-ifelse(CR_WEEK2=="1","1",
                                  ifelse(CRPR_WEEK_2=="1","2","3"))
table_analy$CR_SF_WEEK2<-ifelse(CR_WEEK2=="1","1",
                                ifelse(SF_WEEK2=="1","2","3"))
table_analy$CR_RB_WEEK2<-ifelse(CR_WEEK2=="1","1",
                                ifelse(RB_WEEK2=="1","2","3"))
detach(table_analy)   

#excluding patients without disease activity assessment at week 8 (n=1)
table_analy<-subset(table_analy,!is.na(table_analy$ER_WEEK8)|
                            !is.na(table_analy$HR_WEEK8))

#baseline characteristics
a<-c("SEX","AGE","DISEASE_DURATION", 
     "BMI","smoking_history",
     "CORTICOSTEROID","ASA","MERCAPTOPURINE","AZATHIOPRINE","METHOTREXATE",
     "TNF_expo",
     "pMAYO_baseline_2","eMAYO_baseline_2","CRP_baseline","FC_baseline","LTF_baseline",
     "time_CR","time_SF","time_RB","CR_WEEK2","SF_WEEK2","RB_WEEK2","CRPR_WEEK_2",
     "CR_CRes_WEEK2","CR_FCR_WEEK2","CR_CRPR_WEEK2","CR_SF_WEEK2","CR_RB_WEEK2",
     "ER_WEEK8","HR_WEEK8","HEI_WEEK8","BR_WEEK8","ERes_WEEK8",
      "CR_WEEK52","ER_WEEK52","HR_WEEK52","HEI_WEEK52","ERes_WEEK52")

newdata<-CreateTableOne(vars = a,data=table_analy)
baseline_ALL<-as.data.frame(print(newdata,nonnormal = a))

newdata<-CreateTableOne(vars = a,data=table_analy,strata = "long_outcome")
baseline_biologics<-as.data.frame(print(newdata,nonnormal = a))

newdata<-CreateTableOne(vars = a,data=table_analy,strata = "CR_WEEK2")
baseline_CRWEEK2<-as.data.frame(print(newdata,nonnormal = a))

newdata_2<-subset(table_analy,long_outcome=="1")
newdata<-CreateTableOne(vars = a,data=newdata_2,strata = "time_CR")
baseline_timeCR<-as.data.frame(print(newdata,nonnormal = a))

baseline_characteristics<-bind_rows(baseline_ALL,baseline_biologics,baseline_CRWEEK2,
                                    baseline_timeCR)
write.csv(baseline_characteristics,"X:/Workspace/baseline_characteristics.csv")

##3.select adjusted factors
newdata_analy<-subset(table_analy,long_outcome=="1",select = c(SEX,AGE,ACTARM,DISEASE_DURATION,TNF_expo,BMI,
                                                  CORTICOSTEROID,ASA,MERCAPTOPURINE,AZATHIOPRINE,METHOTREXATE,
                                                  pMAYO_baseline_2,CRP_baseline,FC_baseline,eMAYO_baseline_2,
                                                  ER_WEEK52,HR_WEEK52))
a<-c("ER_WEEK52","HR_WEEK52")
b<-colnames(newdata_analy[1:(ncol(newdata_analy)-2)])
newdata_3<-data.frame(matrix(ncol = 1,nrow=length(b)))
newdata_3[,1]<-b
for(i in 1:length(a)){
  outcome<-newdata_analy[,which(names(newdata_analy)==a[i])]
  newdata_2<-as.data.frame(c())
  for(j in 1:length(b)){
    d<-b[j]
    variables<-subset(newdata_analy,select = d)
    newdata<-data.frame(outcome,variables)
    newdata<-na.omit(newdata)
    logistic<-glm(outcome~.,data=newdata,family =binomial())
    e<-ShowRegTable(logistic)[2,]
    newdata_2<-rbind(newdata_2,e)
  }
  newdata_3<-cbind(newdata_3,newdata_2)
  colnames(newdata_3)[2*i]<-a[i]
  colnames(newdata_3)[2*i+1]<-"p_value"
}
adjusted_factors<-newdata_3
write.csv(adjusted_factors,"X:/Workspace/adjusted_factors.csv")

#OR: WEEK-8 outcomes
OR_analy<-subset(table_analy,select = c(SEX,AGE,TNF_expo,VDZ_expo,treatment_induc,treatment,eMAYO_baseline_2,pMAYO_baseline_2,
                                        CORTICOSTEROID,Immunomodulators,DISEASE_DURATION,biologics_expo,
                                        CR_WEEK2,SF_WEEK2,RB_WEEK2,FCR_WEEK_2,
                                        CR_CRes_WEEK2,CR_FCR_WEEK2,CR_CRPR_WEEK2,CRPR_WEEK_2,
                                        CR_SF_WEEK2,CR_RB_WEEK2,
                                        ER_WEEK8,EI_WEEK8,HR_WEEK8,HEI_WEEK8,ERes_WEEK8))
##number of cases
a<-c("HEI_WEEK8","ERes_WEEK8")
b<-c("CR_WEEK2","CR_CRes_WEEK2","CR_FCR_WEEK2","CR_CRPR_WEEK2","CRPR_WEEK_2","FCR_WEEK_2")
NUM_WEEK8<-data.frame(matrix(ncol = 0,nrow=1))
for(i in 1:length(a)){
  outcome<-OR_analy[,which(names(OR_analy)==a[i])]
  for(j in 1:length(b)){
variables<-subset(OR_analy,select = b[j])
newdata<-data.frame(variables,outcome)
newdata<-na.omit(newdata)
newdata_2<-data.frame(table(newdata))
colnames(newdata_2)<-c("variables","outcome","number")
newdata_3<-aggregate(number~variables,data = newdata_2,FUN = sum)
colnames(newdata_3)<-c("variables","total")
newdata_2<-subset(newdata_2,outcome=="2")
newdata_2$outcome<-a[i]
newdata_2$group<-b[j]
newdata_2<-merge(newdata_2,newdata_3,by="variables")
NUM_WEEK8<-rbind(NUM_WEEK8,newdata_2)
}
}

##Uni_OR
a<-c("HEI_WEEK8","ERes_WEEK8")
b<-c("CR_WEEK2","CR_CRes_WEEK2","CR_FCR_WEEK2","CR_CRPR_WEEK2","CRPR_WEEK_2","FCR_WEEK_2")
uni_OR<-data.frame(matrix(ncol = 0,nrow=1))
for(i in 1:length(a)){
  outcome<-OR_analy[,which(names(OR_analy)==a[i])]
  newdata_2<-as.data.frame(c())
  for(j in 1:length(b)){
    d<-b[j]
    variables<-subset(OR_analy,select = d)
    newdata<-data.frame(outcome,variables)
    newdata<-na.omit(newdata)
    logistic<-glm(outcome~.,data=newdata,family =binomial())
    e<-ShowRegTable(logistic)
    newdata_2<-rbind(newdata_2,e)
  }
  uni_OR<-cbind(uni_OR,newdata_2)
  colnames(uni_OR)[2*i-1]<-a[i]
}

#multi OR (age,TNF-expo, pMAYO_baseline)
a<-c("HEI_WEEK8","ERes_WEEK8")
b<-c("CR_WEEK2","CR_CRes_WEEK2","CR_FCR_WEEK2","CR_CRPR_WEEK2","CRPR_WEEK_2","FCR_WEEK_2")
c<-c("AGE","TNF_expo","pMAYO_baseline_2")
model_2<-data.frame(matrix(ncol = 0,nrow=1))
for(i in 1:length(a)){
  outcome<-OR_analy[,which(names(OR_analy)==a[i])]
  newdata_2<-as.data.frame(c())
  for(j in 1:length(b)){
    d<-c(c,b[j])
    variables<-subset(OR_analy,select = d)
    newdata<-data.frame(outcome,variables)
    newdata<-na.omit(newdata)
    logistic<-glm(outcome~.,data=newdata,family =binomial(),control = list(maxit=25))
    e<-ShowRegTable(logistic)
    f<-e[(nrow(e)-1):nrow(e),]
    newdata_2<-rbind(newdata_2,f)
  }
  model_2<-cbind(model_2,newdata_2)
  colnames(model_2)[2*i-1]<-a[i]
}

OR_WEEK8<-rbind(uni_OR,model_2)
write.csv(OR_WEEK8,"X:/Workspace/OR_WEEK8.csv")

#OR: WEEK-52 outcomes
OR_analy<-subset(table_analy,long_outcome=="1",
                 select = c(SEX,AGE,TNF_expo,VDZ_expo,treatment_induc,treatment,eMAYO_baseline_2,pMAYO_baseline_2,
                            CORTICOSTEROID,Immunomodulators,DISEASE_DURATION,biologics_expo,
                            time_CR,CR_WEEK2,SF_WEEK2,RB_WEEK2,FCR_WEEK_2,
                            CR_CRes_WEEK2,CR_FCR_WEEK2,CR_CRPR_WEEK2,CRPR_WEEK_2,
                            CR_SF_WEEK2,CR_RB_WEEK2,
                            ER_WEEK52,HR_WEEK52,HEI_WEEK52,ERes_WEEK52))

##number of cases
a<-c("ER_WEEK52","HR_WEEK52","HEI_WEEK52")
b<-c("time_CR","CR_WEEK2","CRPR_WEEK_2","CR_FCR_WEEK2","CR_CRPR_WEEK2","FCR_WEEK_2")
NUM_WEEK52<-data.frame(matrix(ncol = 0,nrow=1))
for(i in 1:length(a)){
  outcome<-OR_analy[,which(names(OR_analy)==a[i])]
  for(j in 1:length(b)){
    variables<-subset(OR_analy,select = b[j])
    newdata<-data.frame(variables,outcome)
    newdata<-na.omit(newdata)
    newdata_2<-data.frame(table(newdata))
    colnames(newdata_2)<-c("variables","outcome","number")
    newdata_3<-aggregate(number~variables,data = newdata_2,FUN = sum)
    colnames(newdata_3)<-c("variables","total")
    newdata_2<-subset(newdata_2,outcome=="2")
    newdata_2$outcome<-a[i]
    newdata_2$group<-b[j]
    newdata_2<-merge(newdata_2,newdata_3,by="variables")
    NUM_WEEK52<-rbind(NUM_WEEK52,newdata_2)
  }
}

NUM_case<-rbind(NUM_WEEK8,NUM_WEEK52)
write.csv(NUM_case,"X:/Workspace/NUM_case.csv")

##Uni_OR
a<-c("ER_WEEK52","HR_WEEK52","HEI_WEEK52")
b<-c("time_CR","CR_WEEK2","CRPR_WEEK_2","CR_FCR_WEEK2","CR_CRPR_WEEK2","FCR_WEEK_2")
uni_OR<-data.frame(matrix(ncol = 0,nrow=1))
for(i in 1:length(a)){
  outcome<-OR_analy[,which(names(OR_analy)==a[i])]
  newdata_2<-as.data.frame(c())
  for(j in 1:length(b)){
    d<-b[j]
    variables<-subset(OR_analy,select = d)
    newdata<-data.frame(outcome,variables)
    newdata<-na.omit(newdata)
    logistic<-glm(outcome~.,data=newdata,family =binomial())
    e<-ShowRegTable(logistic)
    newdata_2<-rbind(newdata_2,e)
  }
  uni_OR<-cbind(uni_OR,newdata_2)
  colnames(uni_OR)[2*i-1]<-a[i]
}

#multi OR (age,TNF-expo, pMAYO_baseline)
a<-c("ER_WEEK52","HR_WEEK52","HEI_WEEK52")
b<-c("time_CR","CR_WEEK2","CRPR_WEEK_2","CR_FCR_WEEK2","CR_CRPR_WEEK2","FCR_WEEK_2")
c<-c("AGE","TNF_expo","pMAYO_baseline_2")
model_2<-data.frame(matrix(ncol = 0,nrow=1))
for(i in 1:length(a)){
  outcome<-OR_analy[,which(names(OR_analy)==a[i])]
  newdata_2<-as.data.frame(c())
  for(j in 1:length(b)){
    d<-c(c,b[j])
    variables<-subset(OR_analy,select = d)
    newdata<-data.frame(outcome,variables)
    newdata<-na.omit(newdata)
    logistic<-glm(outcome~.,data=newdata,family =binomial(),control = list(maxit=25))
    e<-ShowRegTable(logistic)
    f<-e[(nrow(e)-2):nrow(e),]
    newdata_2<-rbind(newdata_2,f)
  }
  model_2<-cbind(model_2,newdata_2)
  colnames(model_2)[2*i-1]<-a[i]
}

OR_WEEK52<-rbind(uni_OR,model_2)
write.csv(OR_WEEK52,"X:/Workspace/OR_WEEK52.csv")

#5.p for trend
##short-term outcomes
newdata_analy<-subset(table_analy,
                      select = c(DISEASE_DURATION,SEX,AGE,ACTARM,pMAYO_baseline_2,TNF_expo,
                                 time_CR,time_SF,time_RB,CR_FCR_WEEK2,CR_CRPR_WEEK2,
                                 CR_SF_WEEK2,CR_RB_WEEK2,
                                 HEI_WEEK8,ERes_WEEK8))
##numeric
attach(newdata_analy)
newdata_analy$CR_FCR_WEEK2<-as.numeric(CR_FCR_WEEK2)
newdata_analy$CR_CRPR_WEEK2<-as.numeric(CR_CRPR_WEEK2)
detach(newdata_analy)
##uni OR
a<-c("HEI_WEEK8","ERes_WEEK8")
b<-c("CR_FCR_WEEK2","CR_CRPR_WEEK2")
newdata_3<-data.frame(matrix(ncol = 0,nrow=1))

for(i in 1:length(a)){
  outcome<-newdata_analy[,which(names(newdata_analy)==a[i])]
  newdata_2<-as.data.frame(c())
  for(j in 1:length(b)){
    d<-b[j]
    variables<-subset(newdata_analy,select = d)
    newdata<-data.frame(outcome,variables)
    newdata<-na.omit(newdata)
    logistic<-glm(outcome~.,data=newdata,family =binomial())
    e<-ShowRegTable(logistic)[2,]
    newdata_2<-rbind(newdata_2,e)
  }
  newdata_3<-cbind(newdata_3,newdata_2)
  colnames(newdata_3)[2*i-1]<-a[i]
  colnames(newdata_3)[2*i]<-"p_value"
}
newdata_3$variables<-b
newdata_3$OR<-"uni"
uni_OR_trend<-newdata_3

##multi OR 
a<-c("HEI_WEEK8","ERes_WEEK8")
b<-c("CR_FCR_WEEK2","CR_CRPR_WEEK2")
newdata_3<-data.frame(matrix(ncol = 0,nrow=1))
c<-c("AGE","TNF_expo","pMAYO_baseline_2")
for(i in 1:length(a)){
  outcome<-newdata_analy[,which(names(newdata_analy)==a[i])]
  newdata_2<-as.data.frame(c())
  for(j in 1:length(b)){
    d<-c(c,b[j])
    variables<-subset(newdata_analy,select = d)
    newdata<-data.frame(outcome,variables)
    newdata<-na.omit(newdata)
    logistic<-glm(outcome~.,data=newdata,family =binomial())
    e<-ShowRegTable(logistic)
    e<-e[nrow(e),]
    newdata_2<-rbind(newdata_2,e)
  }
  newdata_3<-cbind(newdata_3,newdata_2)
  colnames(newdata_3)[2*i-1]<-a[i]
  colnames(newdata_3)[2*i]<-"p_value"
}
newdata_3$variables<-b
newdata_3$OR<-"multi"
multi_OR_trend<-newdata_3

p_trend_short<-rbind(uni_OR_trend,multi_OR_trend)
write.csv(p_trend_short,"X:/Workspace/p_trend_short.csv")
##long-term outcomes
newdata_analy<-subset(table_analy,long_outcome=="1",
                      select = c(DISEASE_DURATION,SEX,AGE,ACTARM,pMAYO_baseline_2,TNF_expo,
                      time_CR,time_SF,time_RB,CR_FCR_WEEK2,CR_CRPR_WEEK2,
                      CR_SF_WEEK2,CR_RB_WEEK2,
                      HEI_WEEK8,ERes_WEEK8,
                      ER_WEEK52,HR_WEEK52,HEI_WEEK52,
                      ER_WEEK52_OR,HR_WEEK52_OR,HEI_WEEK52_OR))
##numeric
attach(newdata_analy)
newdata_analy$time_CR<-as.numeric(time_CR)
newdata_analy$CR_FCR_WEEK2<-as.numeric(CR_FCR_WEEK2)
newdata_analy$CR_CRPR_WEEK2<-as.numeric(CR_CRPR_WEEK2)
detach(newdata_analy)
##uni OR
a<-c("ER_WEEK52","HR_WEEK52","HEI_WEEK52","ER_WEEK52_OR","HR_WEEK52_OR","HEI_WEEK52_OR")
b<-c("time_CR","CR_FCR_WEEK2","CR_CRPR_WEEK2")
newdata_3<-data.frame(matrix(ncol = 0,nrow=1))

for(i in 1:length(a)){
  outcome<-newdata_analy[,which(names(newdata_analy)==a[i])]
  newdata_2<-as.data.frame(c())
  for(j in 1:length(b)){
    d<-b[j]
    variables<-subset(newdata_analy,select = d)
    newdata<-data.frame(outcome,variables)
    newdata<-na.omit(newdata)
    logistic<-glm(outcome~.,data=newdata,family =binomial())
    e<-ShowRegTable(logistic)[2,]
    newdata_2<-rbind(newdata_2,e)
  }
  newdata_3<-cbind(newdata_3,newdata_2)
  colnames(newdata_3)[2*i-1]<-a[i]
  colnames(newdata_3)[2*i]<-"p_value"
}
newdata_3$variables<-b
newdata_3$OR<-"uni"
uni_OR_trend<-newdata_3

##multi OR 
b<-c("ER_WEEK52","HR_WEEK52","HEI_WEEK52","ER_WEEK52_OR","HR_WEEK52_OR","HEI_WEEK52_OR")
b<-c("time_CR","CR_FCR_WEEK2","CR_CRPR_WEEK2")
newdata_3<-data.frame(matrix(ncol = 0,nrow=1))
c<-c("AGE","TNF_expo","pMAYO_baseline_2")
for(i in 1:length(a)){
  outcome<-newdata_analy[,which(names(newdata_analy)==a[i])]
  newdata_2<-as.data.frame(c())
  for(j in 1:length(b)){
    d<-c(c,b[j])
    variables<-subset(newdata_analy,select = d)
    newdata<-data.frame(outcome,variables)
    newdata<-na.omit(newdata)
    logistic<-glm(outcome~.,data=newdata,family =binomial())
    e<-ShowRegTable(logistic)
    e<-e[nrow(e),]
    newdata_2<-rbind(newdata_2,e)
  }
  newdata_3<-cbind(newdata_3,newdata_2)
  colnames(newdata_3)[2*i-1]<-a[i]
  colnames(newdata_3)[2*i]<-"p_value"
}
newdata_3$variables<-b
newdata_3$OR<-"multi"
multi_OR_trend<-newdata_3

p_trend_long<-rbind(uni_OR_trend,multi_OR_trend)
write.csv(p_trend_long,"X:/Workspace/p_trend_long.csv")


#subgroup analyses
##short-term outcomes
subg_analy<-subset(table_analy,
                   select = c(SEX,AGE,TNF_expo,VDZ_expo,treatment_induc,treatment,eMAYO_baseline_2,pMAYO_baseline_2,
                              CORTICOSTEROID,Immunomodulators,DISEASE_DURATION,biologics_expo,
                              time_CR,CR_WEEK2,SF_WEEK2,RB_WEEK2,FCR_WEEK_2,
                              CR_CRes_WEEK2,CR_FCR_WEEK2,CR_CRPR_WEEK2,CRPR_WEEK_2,
                              CR_SF_WEEK2,CR_RB_WEEK2,
                              ER_WEEK8,EI_WEEK8,HR_WEEK8,HEI_WEEK8,ERes_WEEK8))
subg_analy$AGE_2<-ifelse(subg_analy$AGE<40,"yong","old")


##
a<-c("SEX","AGE_2","TNF_expo")
b<-c("ERes_WEEK8","HEI_WEEK8")
h<-c("CR_WEEK2","CR_CRPR_WEEK2")
newdata_3<-data.frame(matrix(ncol = 0,nrow=1))
for(i in 1:length(b)){
  group_1<-c()
  group_2<-c()
  p_inter<-c()
  for(k in 1:length(h)){
    for(j in 1:length(a)){
      outcome<-subg_analy[,which(names(subg_analy)==b[i])]
      group<-subg_analy[,which(names(subg_analy)==a[j])]
      variables<-subg_analy[,which(names(subg_analy)==h[k])]
      newdata<-data.frame(group,outcome,variables)
      newdata<-na.omit(newdata)
      logistic<-glm(outcome~group+variables+group*variables,data=newdata,family =binomial())
      m<-ShowRegTable(logistic)
      p_inter<-m[(nrow(m)-2):nrow(m),2]
      
      c<-group[!duplicated(group)][1]
      newdata_2<-subset(newdata,group==c)
      logistic<-glm(outcome~variables,data=newdata_2,family =binomial())
      m<-ShowRegTable(logistic)
      variable_1<-m[(nrow(m)-2):nrow(m),]
      e<-cbind(variable_1,p_inter)
      e<-as.data.frame(e)
      e$group<-paste(a[j],c,sep = "_")
      
      d<-group[!duplicated(group)][2]
      newdata_2<-subset(newdata,group==d)
      logistic<-glm(outcome~variables,data=newdata_2,family =binomial())
      m<-ShowRegTable(logistic)
      variable_2<-m[(nrow(m)-2):nrow(m),]
      f<-cbind(variable_2,p_inter)
      f<-as.data.frame(f)
      f$group<-paste(a[j],d,sep = "_")
      
      f<-rbind(e,f)
      f$outcome<-b[i]
      f$variable<-h[k]
    }
    newdata_3<-rbind(newdata_3,f)
  }}
subgroup_week8<-newdata_3
##long-term outcomes
subg_analy<-subset(table_analy,long_outcome=="1",
                 select = c(SEX,AGE,TNF_expo,VDZ_expo,treatment_induc,treatment,eMAYO_baseline_2,pMAYO_baseline_2,
                            CORTICOSTEROID,Immunomodulators,DISEASE_DURATION,biologics_expo,
                            time_CR,CR_WEEK2,SF_WEEK2,RB_WEEK2,FCR_WEEK_2,
                            CR_CRes_WEEK2,CR_FCR_WEEK2,CR_CRPR_WEEK2,CRPR_WEEK_2,
                            CR_SF_WEEK2,CR_RB_WEEK2,
                            ER_WEEK52,HR_WEEK52,HEI_WEEK52,ERes_WEEK52))
subg_analy$AGE_2<-ifelse(subg_analy$AGE<40,"yong","old")


##
a<-c("SEX","AGE_2","TNF_expo")
b<-c("HR_WEEK52","ER_WEEK52","HEI_WEEK52")
h<-c("time_CR","CR_WEEK2","CR_CRPR_WEEK2")
newdata_3<-data.frame(matrix(ncol = 0,nrow=1))
for(i in 1:length(b)){
  group_1<-c()
  group_2<-c()
  p_inter<-c()
  for(k in 1:length(h)){
  for(j in 1:length(a)){
    outcome<-subg_analy[,which(names(subg_analy)==b[i])]
    group<-subg_analy[,which(names(subg_analy)==a[j])]
    variables<-subg_analy[,which(names(subg_analy)==h[k])]
    newdata<-data.frame(group,outcome,variables)
    newdata<-na.omit(newdata)
    logistic<-glm(outcome~group+variables+group*variables,data=newdata,family =binomial())
    m<-ShowRegTable(logistic)
    p_inter<-m[(nrow(m)-2):nrow(m),2]
    
    c<-group[!duplicated(group)][1]
    newdata_2<-subset(newdata,group==c)
    logistic<-glm(outcome~variables,data=newdata_2,family =binomial())
    m<-ShowRegTable(logistic)
    variable_1<-m[(nrow(m)-2):nrow(m),]
    e<-cbind(variable_1,p_inter)
    e<-as.data.frame(e)
    e$group<-paste(a[j],c,sep = "_")
    
    d<-group[!duplicated(group)][2]
    newdata_2<-subset(newdata,group==d)
    logistic<-glm(outcome~variables,data=newdata_2,family =binomial())
    m<-ShowRegTable(logistic)
    variable_2<-m[(nrow(m)-2):nrow(m),]
    f<-cbind(variable_2,p_inter)
    f<-as.data.frame(f)
    f$group<-paste(a[j],d,sep = "_")
    
    f<-rbind(e,f)
    f$outcome<-b[i]
    f$variable<-h[k]
  }
  newdata_3<-rbind(newdata_3,f)
}}
subgroup_week52<-newdata_3

subgroup<-rbind(subgroup_week8,subgroup_week52)
write.csv(subgroup,"X:/Workspace/subgroup.csv")

#Sensitivity analyses
##only including patient complete the trails 
OR_analy<-subset(table_analy,long_outcome=="1",
                 select = c(SEX,AGE,TNF_expo,VDZ_expo,treatment_induc,treatment,eMAYO_baseline_2,pMAYO_baseline_2,
                            CORTICOSTEROID,Immunomodulators,DISEASE_DURATION,biologics_expo,
                            time_CR,CR_WEEK2,SF_WEEK2,RB_WEEK2,FCR_WEEK_2,
                            CR_CRes_WEEK2,CR_FCR_WEEK2,CR_CRPR_WEEK2,CRPR_WEEK_2,
                            CR_SF_WEEK2,CR_RB_WEEK2,
                            ER_WEEK52_OR,HR_WEEK52_OR,HEI_WEEK52_OR))

##number of cases
a<-c("ER_WEEK52_OR","HR_WEEK52_OR","HEI_WEEK52_OR")
b<-c("time_CR","CR_WEEK2","CR_CRPR_WEEK2")
NUM_SENSITIVITY<-data.frame(matrix(ncol = 0,nrow=1))
for(i in 1:length(a)){
  outcome<-OR_analy[,which(names(OR_analy)==a[i])]
  for(j in 1:length(b)){
    variables<-subset(OR_analy,select = b[j])
    newdata<-data.frame(variables,outcome)
    newdata<-na.omit(newdata)
    newdata_2<-data.frame(table(newdata))
    colnames(newdata_2)<-c("variables","outcome","number")
    newdata_3<-aggregate(number~variables,data = newdata_2,FUN = sum)
    colnames(newdata_3)<-c("variables","total")
    newdata_2<-subset(newdata_2,outcome=="1")
    newdata_2$outcome<-a[i]
    newdata_2$group<-b[j]
    newdata_2<-merge(newdata_2,newdata_3,by="variables")
    NUM_SENSITIVITY<-rbind(NUM_SENSITIVITY,newdata_2)
  }
}

NUM_case<-rbind(NUM_case,NUM_SENSITIVITY)
write.csv(NUM_case,"X:/Workspace/NUM_case.csv")




