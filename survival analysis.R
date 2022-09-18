 
library(survminer)
library(survival)
library(ggpubr)
library(ggplot2) 
library(survminer)
ctcar<-read.csv("CTCarcinoma.csv")
str(ctcar)
ctcar$TRT<-as.factor(ctcar$TRT)
ctcox<-coxph(Surv(Time,Status==0)~TRT+Age, data = ctcar)
ctcox
summary(ctcox)
ctfit<-survfit(Surv(Time,Status==0)~TRT, type=c("kaplan-meier"), data =ctcar )
ctfit
ctvis<-ggsurvplot(ctfit, censor.shape="|", censor.size=3,ggtheme = theme_light(), risk.table = "abs_pct",
                  risk.table.y.text.col = T,risk.table.y.text = FALSE,ncensor.plot = TRUE,
                  data=ctcar)
ctvis

surv_summary(fit)
ctcar$TRT<-relevel(ctcar$TRT, ref = "S+CT")
ggforest(ctcox)