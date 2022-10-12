library(ggplot2)
library(ggpubr)
library(summarytools)
library(Hmisc)
library(dplyr)
library(tidyr)
library(tidyverse)
library(gridExtra)
library(sjPlot)
library(lme4)
library(lmerTest)
library(r2glmm)
library(performance) 
library(interactions)
library("readxl")

#linear mixed model, with grouping by id and group (stress/control)
#(stage): biomarkers (breath, HRV), self-reported current anxiety (VAS, state)
#level 1 (id)   : demographics and clinical, self-reported average anxiety (trait)
#level 2 (group): demographics and clinical, self-reported average anxiety (trait)

#outcome variables: 
#group, HRV, reported trait, state, and VAS anxiety 




#-------------Demographic and clinical data
demo_data=read_excel(path='F:/My Documents/MPH/Biostatistics_projects/4 Stress biomarkers/Questionnaires.xlsx', 
                       na=c("-99", "NA")) #sheet="data",

#renaming, labeling and formatting demographic and clinical variables
names(demo_data)[names(demo_data)=='Subject Code']<-'id'
names(demo_data)[names(demo_data)=='Birth Place']<-'birth_place'
names(demo_data)[names(demo_data)=='NATIVE LANGUAGE']<-'language'
names(demo_data)[names(demo_data)=='STATUS (1=MARRIED, 2=SINGLE)']<-'married'
names(demo_data)[names(demo_data)=='YEARS OF STUDY']<-'education'
names(demo_data)[names(demo_data)=='WORKING (1=YES, 2=NO)']<-'employed'
names(demo_data)[names(demo_data)=='MILITARY SERVICE (1=YES, 2=NO)']<-'military'
names(demo_data)[names(demo_data)=='DID YOU EXPERIENCE ANY PANIC ATTACKS IN THE PAST? (1=YES, 2=NO)']<-'panic'
names(demo_data)[names(demo_data)=='IS THERE ANY FAMILY RECORD OF MENTAL ILLNESS? (1=YES, 2=NO)']<-'family_history'

label(demo_data$birth_place)<-"Place of birth"
label(demo_data$married)<-"Married"
label(demo_data$education)<-"Education (years)"
label(demo_data$employed)<-"Employment status"
label(demo_data$military)<-"Military service"
label(demo_data$panic)<-"History of a panic attack"
label(demo_data$family_history)<-"Family history of mental illness"

demo_data$birth_place[demo_data$birth_place!="Israel"]<-"Other"
demo_data$birth_place<-factor(demo_data$birth_place, levels=c("Israel", "Other"))

demo_data$language[demo_data$language=="HEBREW"]<-"Hebrew"
demo_data$language[demo_data$language=="ARABIC"]<-"Arabic"
demo_data$language[demo_data$language=="RUSSIAN"]<-"Russian"
demo_data$language[demo_data$language=="FINNISH" | demo_data$language=="PORTUGESE" | demo_data$language=="ENGLISH_AND_HEBREW"]<-"Other"
demo_data$language<-factor(demo_data$language, levels=c("Hebrew", "Arabic", "Russian", "Other"))

demo_data$married[demo_data$married==2]<-0
demo_data$married<-factor(demo_data$married, levels=c(0,1), labels=c("No", "Yes"))

demo_data$employed[demo_data$employed==2]<-0
demo_data$employed<-factor(demo_data$employed, levels=c(0,1), labels=c("No", "Yes"))

demo_data$military[demo_data$military==2]<-0
demo_data$military<-factor(demo_data$military, levels=c(0,1), labels=c("No", "Yes"))

demo_data$panic[demo_data$panic==2]<-0
demo_data$panic<-factor(demo_data$panic, levels=c(0,1), labels=c("No", "Yes"))

demo_data$family_history[demo_data$family_history==2]<-0
demo_data$family_history<-factor(demo_data$family_history, levels=c(0,1), labels=c("No", "Yes"))

#-------------------VAS anxiety
VAS_data=read_excel(path='F:/My Documents/MPH/Biostatistics_projects/4 Stress biomarkers/Questionnaires.xlsx', 
                     sheet="VAS", na=c("-99", "NA"))
names(VAS_data)[names(VAS_data)=='Subject']<-'id'
names(VAS_data)[names(VAS_data)=='Anxiety Level']<-'vas'
Hmisc::label(VAS_data$vas)<-"Anxiety (self-reported VAS)"

#--------------------state and trait
state_data=read_excel(path='F:/My Documents/MPH/Biostatistics_projects/4 Stress biomarkers/Questionnaires.xlsx', 
                    sheet="State Scale", na=c("-99", "NA"))
names(state_data)[names(state_data)=='Subject']<-'id'
names(state_data)[names(state_data)=='Grade']<-'state'
Hmisc::label(state_data$state)<-"State anxiety"

trait_data=read_excel(path='F:/My Documents/MPH/Biostatistics_projects/4 Stress biomarkers/Questionnaires.xlsx', 
                      sheet="Trait Scale", na=c("-99", "NA"))
names(trait_data)[names(trait_data)=='Subject']<-'id'
names(trait_data)[names(trait_data)=='Grade']<-'trait'
Hmisc::label(trait_data$trait)<-"Trait anxiety"


#combining the 3 data sets
df<-merge(demo_data, trait_data, by=c("id"))
data<-merge(df, VAS_data, by=c("id"))
data<-merge(data, state_data, by=c("id", "Stage"))


#-------HRV data
hrv_data=read_excel(path='F:/My Documents/MPH/Biostatistics_projects/4 Stress biomarkers/HRV metrics.xlsx', 
                    na=c("-99", "NA"))

names(hrv_data)[names(hrv_data)=='Subject Code']<-'id'

hrv_data$Group<-factor(hrv_data$Group, levels=c("Stress", "Control"))
hrv_data$Stage<-factor(hrv_data$Stage, levels=c("A", "B", "C", "D"))

#converting hrv data from long to wide format
hrv_data<-spread(hrv_data, key = Metric, value = Value)

#exploring hrv
#unpaired t test comparing control and stress across hrv vars by stage
cdata<-hrv_data%>%filter(Stage=="A")%>%filter(Group=="Control")
sdata<-hrv_data%>%filter(Stage=="A")%>%filter(Group=="Stress")
t.test(cdata$PPG_Rate_Mean,sdata$PPG_Rate_Mean, alternative = "two.sided", var.equal = FALSE)

cdata<-hrv_data%>%filter(Stage=="B")%>%filter(Group=="Control")
sdata<-hrv_data%>%filter(Stage=="B")%>%filter(Group=="Stress")
t.test(cdata$PPG_Rate_Mean,sdata$PPG_Rate_Mean, alternative = "two.sided", var.equal = FALSE)

cdata<-hrv_data%>%filter(Stage=="C")%>%filter(Group=="Control")
sdata<-hrv_data%>%filter(Stage=="C")%>%filter(Group=="Stress")
t.test(cdata$PPG_Rate_Mean,sdata$PPG_Rate_Mean, alternative = "two.sided", var.equal = FALSE)

cdata<-hrv_data%>%filter(Stage=="D")%>%filter(Group=="Control")
sdata<-hrv_data%>%filter(Stage=="D")%>%filter(Group=="Stress")
t.test(cdata$PPG_Rate_Mean,sdata$PPG_Rate_Mean, alternative = "two.sided", var.equal = FALSE)

cdata<-hrv_data%>%filter(Group=="Control")
sdata<-hrv_data%>%filter(Group=="Stress")
t.test(cdata$HRV_SDNN,sdata$HRV_SDNN, alternative = "two.sided", var.equal = FALSE)

#t-test for the complete dataset, stress vs. control: 
#significant difference: PPG_rate_Mean, HRV_MeanNN
#non-significant: HRV_CVNN, HRV_SDNN

#t-test per stage:
#HRV_SDNN, HRV_CVNN does not differentiate control from stress in the 4 stages
#HRV_MeanNN had p-value<0.05 for stage B, borderline for C and D and not for A
#PPG_Rate_Mean had p-value<0.05 for stages B, C, D and not for A

df<-hrv_data%>%select(c(id, Stage, Group, PPG_Rate_Mean, HRV_MeanNN))
data<-merge(data, df,by=c("id", "Stage"))

names(data)[names(data)=='HRV_MeanNN']<-'meanrr'
label(data$meanrr)<-"Mean R-R interval"
names(data)[names(data)=='PPG_Rate_Mean']<-'rate'
label(data$rate)<-"Mean heart rate"
#adjusting one outlier (113->104)
data$rate[data$rate>104]<-104


ggplot(data, aes(x=rate, y=state, color=Group))+geom_point()+facet_grid(.~Stage)+
  geom_smooth(method = "lm", se=FALSE)
ggplot(data, aes(x=rate, y=vas, color=Group))+geom_point()+facet_grid(.~Stage)+
  geom_smooth(method = "lm", se=FALSE)

ggplot(data, aes(x=rate, color=Group))+geom_histogram(binwidth=5)
summary(data$rate[data$Group=="Stress"])
summary(data$rate[data$Group=="Control"])
hist(data$rate)

ggplot(data, aes(x=mean, y=state, color=Group))+geom_point()+facet_grid(.~Stage)+
  geom_smooth(method = "lm", se=FALSE)
ggplot(data, aes(x=mean, y=vas, color=Group))+geom_point()+facet_grid(.~Stage)+
  geom_smooth(method = "lm", se=FALSE)

ggplot(data, aes(x=mean, color=Group))+geom_histogram(binwidth=50)
summary(data$mean[data$Group=="Stress"])
summary(data$mean[data$Group=="Control"])
hist(data$rate)

#testing hrv variables as predictors of vas and state with mixed model
m<-lmer(scale(vas)~ scale(mean) +(1|Group/id)+(1|Stage) ,data=data, REML = T)#-0.26
summary(m)$coefficients[2,1]
r2beta(m, method = 'nsj', partial = T)#0.064

m<-lmer(scale(vas)~ scale(rate) +(1|Group/id)+(1|Stage) ,data=data, REML = T)#0.32
summary(m)$coefficients[2,1]
r2beta(m, method = 'nsj', partial = T)#0.091
#rate is better than mean

m<-lmer(scale(state)~ scale(mean) +(1|Group/id)+(1|Stage) ,data=data, REML = T)#-0.15
summary(m)$coefficients[2,1]
r2beta(m, method = 'nsj', partial = T)#0.022

m<-lmer(scale(state)~ scale(rate) +(1|Group/id)+(1|Stage) ,data=data, REML = T)#0.15
summary(m)$coefficients[2,1]
r2beta(m, method = 'nsj', partial = T)#0.10
#rate is better than mean

tab_model(m)

ggplot(data, aes(x=breath1, y=cvnn, color=Group))+geom_point()+
  geom_smooth(method="lm", se=FALSE)+facet_grid(.~Stage)

ggplot(data, aes(x=Stage, y=cvnn))+geom_boxplot()#+facet_grid(.~Group)

#-------------------------------Breath data
br_data=read_excel(path='F:/My Documents/MPH/Biostatistics_projects/4 Stress biomarkers/Breath Analysis.xlsx', 
                     na=c("-99", "NA"))

names(br_data)[names(br_data)=='Subject']<-'id'

br_data$Group<-factor(br_data$Group, levels=c("Stress", "Control"))
br_data$Stage<-factor(br_data$Stage, levels=c("A", "B", "C", "D"))

#creating a variable for 5X8=40 biomarkers' names
br_data$marker<-paste(br_data$Sensor, br_data$Feature, sep="")
markers<-unique(br_data$marker)

#converting breath data from long to wide format
br_data<-br_data%>%select(-c(Sensor, Feature))
br_data<-spread(br_data, key = marker, value = Value)

#merging with the rest of the data
data<-merge(data, br_data, by=c("id", "Stage"))

df<-hrv_data%>%select(c(id, Stage, HRV_CVNN))
data<-merge(data, df,by=c("id", "Stage"))

#slope is highly correlative to deltaRstart, AUC is highly correlative to deltaRend
auc_indx <- grepl('AUC', colnames(data))
data<-data[, !auc_indx]
slope_indx <- grepl('Slope', colnames(data))
data<-data[, !slope_indx]

#--------------------cluster analysis
#normalizing the data
mydata<-br_data%>%filter(Stage=="A")
z <- mydata[,-c(1:3)]
means <- apply(z,2,mean)
sds <- apply(z,2,sd)
nor <- scale(z,center=means,scale=sds)
#calculating distance
distance = dist(nor)
#hierarchical agglomerative clustering 
mydata.hclust = hclust(distance)
plot(mydata.hclust)


library(ComplexHeatmap)
breaksList = seq(0, 15, by = 1)
sbr_data<-br_data%>%filter(Group=="Stress" & Stage=="B")%>%select(-c("id", "Stage", "Group"))%>%nor()
cbr_data<-br_data%>%filter(Group=="Control" & Stage=="B")%>%select(-c("id", "Stage", "Group"))%>%nor()
tbr_data<-br_data%>%select(-c("id", "Stage", "Group"))%>%nor()
ht1 = Heatmap(sbr_data, name = "Stress group")#stress group
ht2 = Heatmap(cbr_data, name = "Control group")#control group
tiff("F:/My Documents/MPH/Biostatistics_projects/4 Stress biomarkers/heatmap.tiff", width = 10, height = 10, units = 'in', res = 300)
ht1 + ht2
dev.off()

heatmap(as.matrix(sbr_data), scale = "column")



library(RobRSVD)
library(NMF) 
#cl_data<-br_data[,4:43]+0.001
#estim.r <- nmf(cl_data, 2, nrun=1, seed=123456)
#plot(estim.r)


#---------------------------------
#identifying participants for which there is data for HRV and also breath
par1<-unique(br_data$id)
par2<-unique(hrv_data$id)
both<-intersect(par1, par2)
#total 26 participants, filtering those who have full data
data<-data%>% filter(id %in% both)
names(data)[names(data)=='HRV_CVNN']<-'hrv'
label(data$hrv)<-"Heart rate variability"

#-------------creating breath1_diff
a_breath<-data%>%filter(Stage=="A")%>%select(id, breath1, breath2)
data<-merge(data, a_breath, by=c("id"))
names(data)[names(data)=="breath1.x"]<-"breath1"
names(data)[names(data)=="breath2.x"]<-"breath2"
data$breath1_d<-data$breath1-data$breath1.y
data$breath2_d<-data$breath2-data$breath2.y

#-------------------------------sample description
library(table1)
t1<-data[!duplicated(data$id),]
#table(t1$Group)
#14 in the stress group, 12 in the control group

pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a fisher's exact test of independence
    p <- fisher.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

#socio-demographic
table1(~ Gender+Age+birth_place+married+education+employed+military+panic+family_history
       | Group, data=t1, overall=F, extra.col=list(`P-value`=pvalue))

#clinical baseline
table1(~ vas+trait+state+hrv
       | Group, data=t1, overall=F, extra.col=list(`P-value`=pvalue))

ggdensity(data, x = "trait", add = "mean", rug = TRUE,
              color = "Group", fill = "Group",
              palette = c("#00AFBB", "#E7B800"))+xlab("Trait anxiety score")
#trait anxiety:
#p-value is 0.07, maybe due to the small sample size.
#looks like there is a difference in the distribution of trait anxiety
#between intervention and control

#EDA 
#https://towardsdatascience.com/top-3-methods-for-handling-skewed-data-1334e0debf45
#applying transformations for variables that are not normally distributed
summary(sqrt(data$vas))#range: 0-94, right asymmetric 
hist(sqrt(data$vas))

#log
summary(log(data$vas+1))#range: 0-94, right asymmetric 
hist(sqrt(data$vas+1))

summary((data$vas^(1/3)))
hist((data$vas^(1/3)))

summary(data$trait)#range:20-60, normal distribution
hist(data$trait)

summary(data$state)#range:20-60, right asymmetric distribution
hist(sqrt(data$state))
hist(data$state^(1/3))
hist(log(data$state))

summary(data$heart_rate)#range:53-114, right asymmetric distribution
hist(data$heart_rate)
hist(sqrt(data$heart_rate))

summary(data$meanrr)#range:528-1154, almost normal distribution
hist(data$meanrr)

summary(data$breath1)#range:-9 -4.8, 
hist(data$breath1)


summary(data$breath2)#range: -3.4-1.5, 
hist(data$breath2)


#---1. Baseline: differences and correlational patterns
adata<-data%>%filter(Stage=="A")
sadata<-adata%>%filter(Group=="Stress")
cadata<-adata%>%filter(Group=="Control")

bdata<-data%>%filter(Stage=="B")
sbdata<-bdata%>%filter(Group=="Stress")
cbdata<-bdata%>%filter(Group=="Control")

#Stage A
M1 <- cor(sadata[, c("sen1", "sen2", "rate")], use="pairwise.complete.obs")
as<-corrplot.mixed(M1, upper="ellipse", addrect = 2,main="Stress group at baseline",mar=c(0,0,2,0))

M2 <- cor(cadata[, c("sen1", "sen2", "rate")], use="pairwise.complete.obs")
ac<-corrplot.mixed(M2, upper="ellipse", addrect = 2,main="Control group at baseline",mar=c(0,0,2,0))
#there are correlational differences, sen1 and sen2 are more correlated for the control group

grid.arrange(as, ac, ncol=2)

#Stage B
M1 <- cor(sbdata[, c("sen1", "sen2", "rate")], use="pairwise.complete.obs")
corrplot.mixed(M1, upper="ellipse", addrect = 2,main="Stress group at stage B",mar=c(0,0,2,0))

M2 <- cor(cbdata[, c("sen1", "sen2", "rate")], use="pairwise.complete.obs")
corrplot.mixed(M2, upper="ellipse", addrect = 2,main="Control group at stage B",mar=c(0,0,2,0))
#sen1 and sen2, rate and sen2 are more correlated for the control group



ggplot(adata, aes(x=rate, y=vas, color=Gender))+geom_point()+
  geom_smooth(method = "lm", se=FALSE)
ggplot(adata, aes(x=rate, y=state, color=Gender))+geom_point()+
  geom_smooth(method = "lm", se=FALSE)
#females show a positive trend for hrv and state/vas while males have no distinct trend
ggplot(adata, aes(x=hrv, y=trait, color=Gender))+geom_point()+
  geom_smooth(method = "lm", se=FALSE)
#both males and females have a small positive trend for trait and hrv, with higher slope for females

#correlational patterns for biochemical changes
cdata<-adata%>%filter(Group=="Control")
sdata<-adata%>%filter(Group=="Stress")
M2 <- cor(cdata[, c(42:49)], use="pairwise.complete.obs")
M1 <- cor(sdata[, c(42:49)], use="pairwise.complete.obs")

sp<-corrplot.mixed(M1, upper="ellipse", addrect = 2,main="Correlations for stress group",mar=c(0,0,2,0))
cp<-corrplot.mixed(M2, upper="ellipse", addrect = 2,main="Correlations for control group",mar=c(0,0,2,0))
#stronger correlations in the stress group: S1, S4, S5, S6, S7, S8
#weaker correlation in the stress group: S2, S3

#exploring correlations of biochemical measures
adata$abdiff1<-bdata$S1deltaRstart-adata$S1deltaRstart
adata$abdiff2<-bdata$S2deltaRstart-adata$S2deltaRstart
adata$abdiff3<-bdata$S3deltaRstart-adata$S3deltaRstart
adata$abdiff4<-bdata$S4deltaRstart-adata$S4deltaRstart
adata$abdiff5<-bdata$S5deltaRstart-adata$S5deltaRstart
adata$abdiff6<-bdata$S6deltaRstart-adata$S6deltaRstart
adata$abdiff7<-bdata$S7deltaRstart-adata$S7deltaRstart
adata$abdiff8<-bdata$S8deltaRstart-adata$S8deltaRstart

#correlational patterns for biochemical changes
cdata<-adata%>%filter(Group=="Control")
sdata<-adata%>%filter(Group=="Stress")
M2 <- cor(cdata[, c(42:49)], use="pairwise.complete.obs")
M1 <- cor(sdata[, c(42:49)], use="pairwise.complete.obs")

sp<-corrplot.mixed(M1, upper="ellipse", addrect = 2,main="Correlations for stress group",mar=c(0,0,2,0))
cp<-corrplot.mixed(M2, upper="ellipse", addrect = 2,main="Correlations for control group",mar=c(0,0,2,0))
#stronger correlations in the stress group: S1, S4, S5, S6, S7, S8

abdata<-data%>%filter(Stage=="A" |Stage=="B" )

#individual trends plot, from A to B, for each biomarker by stress/control
ggplot(abdata, aes(y=S2deltaRstart, x=Stage, group=id)) +facet_grid(.~Group)+
  geom_point(size = 1.2, alpha = .8)+#, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)

ggplot(abdata, aes(y=s1, x=Stage, group=id)) +facet_grid(.~Group)+
  geom_point(size = 1.2, alpha = .8)+#, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)

ggplot(abdata, aes(y=S3deltaRstart, x=Stage, group=id)) +facet_grid(.~Group)+
  geom_point(size = 1.2, alpha = .8)+#, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)

ggplot(abdata, aes(y=S4deltaRstart, x=Stage, group=id)) +facet_grid(.~Group)+
  geom_point(size = 1.2, alpha = .8)+#, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)
#
ggplot(abdata, aes(y=S5deltaRstart, x=Stage, group=id)) +facet_grid(.~Group)+
  geom_point(size = 1.2, alpha = .8)+#, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)

ggplot(abdata, aes(y=S6deltaRstart, x=Stage, group=id)) +facet_grid(.~Group)+
  geom_point(size = 1.2, alpha = .8)+#, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)

ggplot(abdata, aes(y=S7deltaRstart, x=Stage, group=id)) +facet_grid(.~Group)+
  geom_point(size = 1.2, alpha = .8)+#, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)
#lower baseline values for stress group

ggplot(abdata, aes(y=S8deltaRstart, x=Stage, group=id)) +facet_grid(.~Group)+
  geom_point(size = 1.2, alpha = .8)+#, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)

#A-B difference in S1 vs. vas by group
ggplot(adata, aes(x=abdiff1, y=vas, color=Group))+geom_point()+
  geom_smooth(method = "lm", se=FALSE)
ggplot(adata, aes(x=abdiff1, y=state, color=Group))+geom_point()+
  geom_smooth(method = "lm", se=FALSE)
#control has a positive correlation between diff1 and vas. while stress has no distinctive trend
#this means that in the control group higher vas is associated with a higher difference between 
#A and B (higher increase in S1deltaRstart). However, state shows a negative association
#A-B difference in S1 vs. vas by group
ggplot(adata, aes(x=abdiff2, y=vas, color=Group))+geom_point()+
  geom_smooth(method = "lm", se=FALSE)
ggplot(adata, aes(x=abdiff2, y=state, color=Group))+geom_point()+
  geom_smooth(method = "lm", se=FALSE)

#same correlation between S1 and S2 differences for both groups
ggplot(adata, aes(x=abdiff2, y=abdiff1, color=Group))+geom_point()+
  geom_smooth(method = "lm", se=FALSE)

ggplot(adata, aes(x=abdiff2, y=abdiff3, color=Group))+geom_point()+
  geom_smooth(method = "lm", se=FALSE)

ggplot(adata, aes(x=abdiff3, y=abdiff4, color=Group))+geom_point()+
  geom_smooth(method = "lm", se=FALSE)

#distribution of sensor A-B differences
ggboxplot(adata, x = "Group", y = "abdiff6",
               color = "Group", palette =c("#00AFBB", "#E7B800"),
               add = "jitter")
#most boxplots are pretty much the same for both groups

#correlations for breath1 and breath2
cor(adata$breath1[adata$Group=="Stress"], adata$breath2[adata$Group=="Stress"] )#0.3
cor(adata$breath1[adata$Group=="Control"], adata$breath2[adata$Group=="Control"])#0.65

bdata<-data%>%filter(Stage=="B")
cor(bdata$breath1[bdata$Group=="Stress"], bdata$breath2[bdata$Group=="Stress"] )#0.27
cor(bdata$breath1[bdata$Group=="Control"], bdata$breath2[bdata$Group=="Control"])#-0.12

par(mfrow = c(1, 2))
#Stage A
M1 <- cor(sadata[,7:9], use="pairwise.complete.obs")
corrplot.mixed(M1, upper="ellipse", addrect = 2,main="Stress group at stage A",mar=c(0,0,2,0))

M2 <- cor(cadata[, 7:9], use="pairwise.complete.obs")
corrplot.mixed(M2, upper="ellipse", addrect = 2,main="Control group at stage A",mar=c(0,0,2,0))

par(mfrow = c(1, 1))

#2. --------------222222222---------------------------------------
#test whether the groups differ in time dynamics for biomarkers, hrv, state and vas
#in other words: group has a significant interaction with stage(=time)
m<-lmer(vas~ 1+(1|Stage)+(1|id) ,data=data, REML = T)
icc(m)#icc=0.781
m<-lmer(vas~ 1+(1|Group/id)+(1|Stage) ,data=data, REML = T)
icc(m)#0.792

#mixed model assumptions:
#https://ademos.people.uic.edu/Chapter18.html
#1) data points are independent
#2)linear relationship between predictor and response (although models can be expanded to fit curvilinear data)
#plot the model's residuals vs. the observed values of the outcome variable
#plot(resid(m1), Anxiety). an evenly scattered plot indicates linearity.
#3) homogeneity of variance - the variance of the residuals is equal across groups 
#(using anova, this is a variation of LEvene's test)
#4)the residuals are normally distributed- using a qqplot with a qqline
#residuals are normally distributed
#qqnorm(resid(model6)) 
#qqline(resid(model6), col = "red") # add a perfect fit line


#m1<-lmer(vas~ Group*Stage*Gender+(1|id),data=data, REML = T)
m<-lmer(vas~ Group*Stage+(1|id),data=data, REML = T)
tab_model(m, show.ci=FALSE)
#interaction is significant for stage B!
library(emmeans)
mylist <- list(Stage=c("A", "B", "C", "D"), Group=c("Stress", "Control"), Gender=c("M", "F"))
emmip(m, Group ~Stage, at=mylist,CIs=TRUE)
#both groups had the same vas in baseline, stress group had a higher vas in B (significant)
#C and D, with a similar recuperation trend

m<-lmer(state~ Group*Stage+(1|id),data=data, REML = T)
emmip(m, Group ~Stage, at=mylist,CIs=TRUE)
#same as vas

m<-lmer(rate~ Group*Stage+(1|id),data=data, REML = T)
tab_model(m)
emmip(m, Group ~Stage, at=mylist,CIs=TRUE)
#interaction is significant in B

m<-lmer(meanrr~ Group*Stage+(1|id),data=data, REML = T)
tab_model(m)
emmip(m, Group ~Stage, at=mylist,CIs=TRUE)

#breath sensors
m<-lmer(sen1~ Group*Stage+(1|id),data=data, REML = T)
tab_model(m, show.ci=FALSE)

m<-lmer(sen2~ Group*Stage+(1|id),data=data, REML = T)
tab_model(m, show.ci=FALSE)
m<-lmer(sen3~ Group*Stage+(1|id),data=data, REML = T)
tab_model(m, show.ci=FALSE)

#--------------------------------333333333
#calculating the difference in breath1 between stages A and B
adata<-data%>%filter(Stage=="A")
bdata<-data%>%filter(Stage=="B")
bdata$breath1dif<-(bdata$breath1-adata$breath1)
#comparing stress and control
t.test(bdata$breath1dif[bdata$Group=="Control"], bdata$breath1dif[bdata$Group=="Stress"] )
#mean=0.79 CI(-1.5, 3.4)
#not statistically significant
#maybe use bootstrapping to calculate p-value?
#https://stats.stackexchange.com/questions/136661/using-bootstrap-under-h0-to-perform-a-test-for-the-difference-of-two-means-repl?noredirect=1&lq=1
#https://stats.stackexchange.com/questions/20701/computing-p-value-using-bootstrap-with-r
abdata<-data%>%filter(Stage=="A" | Stage=="B")

ggplot(abdata, aes(x=Stage, y=breath1, group=id))+facet_grid(.~Group)+
  geom_point(size = 1.2, alpha = .8)+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)
#breath1 had lager variance in the stress group, but stress and control look similar
ggplot(data, aes(x=breath1))+geom_histogram(binwidth = 0.5)
outlier(data$breath1)

ggplot(data, aes(y = breath1)) + geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

ggdensity(data, x = "breath1", add = "mean", rug = TRUE,
          color = "Group", fill = "Group",
          palette = c("#00AFBB", "#E7B800"))

ggdensity(data, x = "breath2", add = "mean", rug = TRUE,
          color = "Group", fill = "Group",
          palette = c("#00AFBB", "#E7B800"))+facet_grid(.~Stage)

ggplot(data, aes(x=Stage, y=breath1, group=id))+facet_grid(.~Group)+
  geom_point(size = 1.2, alpha = .8)+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)


#------------------------bootstrapping for hypotheses testing
#https://stats.stackexchange.com/questions/20701/computing-p-value-using-bootstrap-with-r
#https://blog.methodsconsultants.com/posts/understanding-bootstrap-confidence-interval-output-from-the-r-boot-package/

#H1: there is a difference in groups' mean breath1_d in stage B
df<-data%>%filter(Stage=="B")%>%select(breath1_d, Group)
df<-data%>%filter(Stage=="C")%>%select(breath1_d, Group)
df<-data%>%filter(Stage=="D")%>%select(breath1_d, Group)

library(boot)

meanDiff =function(x, w){
  y <- tapply(x[w,1], x[w,2], mean)
  y[1]-y[2]}

myboot = boot(df, meanDiff, R = 1000, strata = df[,2])
ci = boot.ci(myboot)
pvalue<-with(myboot, pnorm(abs((2*t0 - mean(t) - 1) / sqrt(var(t)[1,1])), lower.tail=F)*2)
#all cis contain 0

#differences in means by stage, for the same group
df<-data%>%filter(Group=="Stress")%>%filter(Stage=="B" | Stage=="C")%>%select(breath1_d, Stage)
myboot = boot(df, meanDiff, R = 1000, strata = df[,2])
ci = boot.ci(myboot)

#----------------------------------444444444
#Develop predictive models of HRV at baseline, reported trait, state, 
#and VAS anxiety at baseline based on biochemical measures at baseline.
adata<-data%>%filter(Stage=="A")

M1 <- cor(adata[, c(14:15,17:26)], use="pairwise.complete.obs")
corrplot.mixed(M1, upper="ellipse", addrect = 2,main="Stress group",mar=c(0,0,2,0))

#based on corrplot
m<-lm(rate~s8+s7+s4  ,data=adata)
tab_model(m)
#R2=0.42

#hrv and breath1+2
ggplot(adata, aes(x=cvnn, y=breath1))+geom_point()+geom_smooth()
ggplot(adata, aes(x=cvnn, y=breath2))+geom_point()+geom_smooth()

#vas and breath1+2
ggplot(adata, aes(x=vast, y=breath1))+geom_point()+geom_smooth(method="lm")
ggplot(adata, aes(x=vas, y=breath2))+geom_point()+geom_smooth()

#--------------------------------------- KNN regression
#https://www.datatechnotes.com/2020/10/knn-regresion-example-in-r.html
#http://www.sthda.com/english/articles/35-statistical-machine-learning-essentials/142-knn-k-nearest-neighbors-essentials/

#predicting heart rate at baseline by breath
adata<-data%>%filter(Stage=="A")

library(caret)
#creating a data frame for the KNN regression
kdata<-adata%>% select(breath1, breath2, heart_ratet)

##Generate a random number that is 90% of the total number of rows in dataset.
#set.seed(12)
#ran <- createDataPartition(kdata$heart_ratet, p = .9, list = F)
ran <- sample(1:nrow(kdata), 0.9 * nrow(kdata)) 

##the normalization function is created
#nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Run normalization on first 4 columns of dataset because they are the predictors
#norm_kdata<-as.data.frame(lapply(kdata[,1:2], nor))
#adata_norm <- as.data.frame(lapply(adata[,c("Gender","heart_ratet","breath1", "breath2")], nor))

train<-kdata[ran,]
test<-kdata[-ran,]

train_x = train[, -3]
train_x = scale(train_x)[,]
train_y = train[,3]

test_x = test[, -3]
test_x = scale(test[,-3])[,]
test_y = test[,3]


##run knn function
knnmodel = knnreg(train_x, train_y)

#derive predictions for the test set
pred_y = predict(knnmodel, data.frame(test_x))

print(data.frame(test_y, pred_y))

mse = mean((test_y - pred_y)^2)
mae = caret::MAE(test_y, pred_y)
rmse = caret::RMSE(test_y, pred_y)
cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)


#------------------LOOCV
#https://stackoverflow.com/questions/51118220/how-can-i-use-loocv-in-r-with-knn
bdata<-data%>%filter(Stage=="B")
kdata<-bdata%>% select(breath1, breath2, Group)%>%data.frame()
kdata<-adata%>% select(breath1, breath2, heart_ratet)
kdata<-data.frame(y=adata$heart_ratet, x1=adata$breath1, x2=adata$breath2)

#specify the cross-validation method
ctrl <- trainControl(method = "LOOCV")

#fit a regression model and use LOOCV to evaluate performance
model <- train(y ~ x1 + x2, data = kdata, method = "lm", trControl = ctrl)
model <- train(kdata[,2:3], kdata[,1], method="knn", trControl = ctrl)

#view summary of LOOCV               
print(model)



#------------------------------KNN classification algorithm
#https://towardsdatascience.com/k-nearest-neighbors-algorithm-with-examples-in-r-simply-explained-knn-1f2c88da405c
#predicting group by breath at stage B
# KNN
adata<-data%>%filter(Stage=="B")

#creating a data frame for the KNN algorithm
kdata<-adata%>% select(breath1, breath2, Group)

##Generate a random number that is 90% of the total number of rows in dataset.
set.seed(23)
#ran <- sample(1:nrow(kdata), 0.9 * nrow(kdata)) 
ran<-kdata$Group %>% createDataPartition(p = 0.9, list = FALSE)
##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Run normalization on first 4 columns of dataset because they are the predictors
norm_kdata<-as.data.frame(lapply(kdata[,1:2], nor))
#adata_norm <- as.data.frame(lapply(adata[,c("Gender","heart_ratet","breath1", "breath2")], nor))

##extract training set
train_x <- norm_kdata[ran,] 
##extract testing set
test_x <- norm_kdata[-ran,] 
##extract 5th column of train dataset because it will be used as 'cl' argument in knn function.
train_y <- kdata[ran,3]
##extract 5th column if test dataset to measure the accuracy
test_y <- kdata[-ran,3]
##load the package class

library(class)
##run knn function
pr <- knn(train_x, test_x, cl=train_y, prob=TRUE, k=5)

##create confusion matrix
tab <- table(pr,test_y)
##this function divides the correct predictions by total number of predictions
#that tell us how accurate the model is.
#accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
#accuracy(tab)#67% accuracy



#-------------------knn with loocv on the full data for categorical outcome
adata<-data%>%filter(Stage=="B")
#creating a data frame for the KNN algorithm
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }
kdata<-adata%>% select(breath1, breath2, Group)%>%mutate(breath1=nor(breath1), breath2=nor(breath2))

pr<-knn.cv(train = train_x, cl = as.factor(train_y), 
       k = 4, prob = FALSE,                        # test for different values of k
       use.all = TRUE)

pr<-knn.cv(train = kdata[,1:2], cl = as.factor(kdata[,3]),k =8, prob = FALSE,use.all = TRUE)

tab <- table(pr,kdata[,3])
sensitivity(pr, kdata[,3], positive = levels(pr)[1])
specificity(pr, kdata[,3], negative = levels(pr)[2])
#for k=8 sensitivity is 0.5 and specificity is 0.75

#-----finding the right k using caret, with a plot
#https://stackoverflow.com/questions/51118220/how-can-i-use-loocv-in-r-with-knn
library(caret)

train.control <- trainControl(method  = "LOOCV")

fit <- train(Group~breath1+breath2,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:20),
             trControl  = train.control,
             metric     = "Accuracy",
             data       = kdata)
#highest accuracy was 0.58 for k=18

train.control <- trainControl(method  = "LOOCV", classProbs = TRUE)
fit <- train(Group~breath1+breath2,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:20),
             trControl  = train.control,
             metric     = "ROC",
             data       = kdata)
#highest accuracy was 0.58 for k=18

#better to check accuracy with a ROC curve?
library(pROC)
data$fitted.results <- predict(model,newdata=data, type='response', allow.new.levels = TRUE)
#calculating and plotting ROC and AUC
roc_obj <- roc(data$Diagnosis, data$fitted.results)
plot(roc_obj, print.thres = "best",print.auc=T, main="ROC curve for model 2")
#coords(roc_obj, "best", "threshold")
#spe:0.863, sen: 0.85, AUC=0.935
#probabilities <- model %>% predict(test.data, type = "response")
#predicted.classes <- ifelse(probabilities > 0.491, 1, 0)
#test<-as.numeric(test.data$Diagnosis)-1
#mean(predicted.classes == test)


#---------------------------------55555555
#prediction models building:
#build the classifier with SVM (support vector machine) or k-nearest neighbors (binary classifiers)
#maybe build a classifier for stress yes (state>median) or no(state<median)
#https://machinelearningmastery.com/neural-networks-crash-course/
#https://towardsdatascience.com/k-nearest-neighbors-algorithm-with-examples-in-r-simply-explained-knn-1f2c88da405c

#model testing and evaluation:
#K-fold Cross-validation 
#LOOCV is a special case of k-Fold Cross-Validation where k is equal to the size of data (n).
#it has 2 disadvantages: 1. Computationally expensive when n is large, 2. high variance or over fitting, since
#we are using all the data set except for one observation.
#model evaluation: kappa

#------------------------------------Stage B
adata$state[is.na(adata$state)]<-31
bdata<-data%>%filter(Stage=="B")
#bdata$hr_diff<-(bdata$heart_ratet-adata$heart_ratet)/adata$heart_ratet
bdata$state_diff<-bdata$state-adata$state
bdata$vas_diff<-bdata$vas-adata$vas
bdata$breath1_diff<-bdata$breath1-adata$breath1
bdata$breath2_diff<-bdata$breath2-adata$breath2
#adjusting outliers
#bdata$breath1_diff[bdata$breath1_diff>20]<-0
#bdata$breath1_diff[bdata$breath1_diff< -10]<-0

ggplot(bdata, aes(x=breath1_diff, y=cvnn_diff))+geom_point()+geom_smooth(method="lm")
#+facet_grid(.~Group)
#t.test(bdata$breath1_diff[bdata$Group=="Control"], bdata$breath1_diff[bdata$Group=="Stress"])
ggplot(bdata, aes(x=breath1_diff, y=vas_diff))+geom_point()+geom_smooth(method="lm")#facet_grid(.~Group)
#
ggplot(bdata, aes(x=breath1_diff, y=state_diff))+geom_point()+geom_smooth(method="lm")
#

ggplot(bdata, aes(x=breath1_diff, y=breath1))+geom_point()+facet_grid(.~Group)+
  geom_smooth(method="lm")
#


ggplot(bdata, aes(x=breath1_diff, color=Group))+geom_boxplot()#+facet_grid(.~Group)
ggplot(bdata, aes(x=breath2_diff, color=Group))+geom_boxplot()


#hrv and breath1+2
ggplot(bdata, aes(x=cvnn, y=breath1))+geom_point()+geom_smooth()
ggplot(bdata, aes(x=cvnn_diff, y=breath2_diff))+geom_point()+geom_smooth()

#vas and breath1+2
ggplot(adata, aes(x=vast, y=breath1))+geom_point()+geom_smooth(method="lm")
ggplot(adata, aes(x=vas, y=breath2))+geom_point()+geom_smooth()

#--------------------------------------HRV
#linear regression with group as a predictor/moderator
m1<-lm(hr_diff~breath1*Group, data=bdata)
m2<-lm(hr_diff~breath1+Group, data=bdata)

m1<-lm(hr_diff~breath1_diff*Group, data=bdata)
m2<-lm(hr_diff~breath1_diff+Group, data=bdata)

m1<-lm(hr_diff~breath2_diff+breath1_diff, data=bdata)
m2<-lm(hr_diff~breath2_diff+Group, data=bdata)
tab_model(m1, m2, show.ci=FALSE)

#mixed model
m1<-lmer(hr_diff~ breath1_diff +breath2_diff+(1|Group) ,data=bdata, REML = T)
tab_model(m1, show.ci=FALSE)
#Rsqr=0.105, breath1 is significant with an estimate of 0

#KNN regression
#with breath_diff
kdata<-bdata%>% select(breath1_diff, breath2_diff, hr_diff)%>%
  mutate(breath1_diff=nor(breath1_diff), breath2_diff=nor(breath2_diff), hr_diff=nor(hr_diff))

#with breath
kdata<-bdata%>% select(breath1, breath2, hr_diff)%>%
  mutate(breath1=nor(breath1), breath2=nor(breath2), hr_diff=nor(hr_diff))

kdata<-bdata%>% select(breath1, breath1_diff, hr_diff)%>%
  mutate(breath1=nor(breath1), breath2=nor(breath1_diff), hr_diff=nor(hr_diff))


#train.control <- trainControl(method  = "LOOCV")
fit <- train(x=kdata[,1:2], y=as.numeric(kdata[,3]),
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:20),
             trControl  = train.control,
             data       = kdata)
sd(kdata$hr_diff)#0.218

#-----------------------------------VAS
m1<-lm(vas_diff~breath1*Group, data=bdata)
m2<-lm(vas_diff~breath1+Group, data=bdata)

m1<-lm(vas_diff~breath1_diff*Group, data=bdata)
m2<-lm(vas_diff~breath1_diff+Group, data=bdata)

m1<-lm(vas_diff~breath2_diff+breath1_diff, data=bdata)
m2<-lm(vas_diff~breath1_diff+Group, data=bdata)
tab_model(m1, m2, show.ci=FALSE)

#mixed model
m1<-lmer(vas_diff~ breath1_diff +breath2_diff+(1|Group) ,data=bdata, REML = T)
tab_model(m1, show.ci=FALSE)
#Rsqr=0.005, breath2 has an estimate of 1.43

#KNN regression
#with breath_diff
kdata<-bdata%>% select(breath1_diff, breath2_diff, vas_diff)%>%
  mutate(breath1_diff=nor(breath1_diff), breath2_diff=nor(breath2_diff), vas_diff=nor(vas_diff))

#with breath
kdata<-bdata%>% select(breath1, breath2, vas_diff)%>%
  mutate(breath1=nor(breath1), breath2=nor(breath2), vas_diff=nor(vas_diff))


#train.control <- trainControl(method  = "LOOCV")
fit <- train(x=kdata[,1:2], y=as.numeric(kdata[,3]),
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:20),
             trControl  = train.control,
             data       = kdata)
#k=18, RMSE=0.20
sd(kdata$vas_diff)#0.197

#--------------------------------state
m1<-lm(state_diff~breath1*Group, data=bdata)
m2<-lm(state_diff~breath1+Group, data=bdata)

m1<-lm(state_diff~breath1_diff*Group, data=bdata)
m2<-lm(state_diff~breath1_diff+Group, data=bdata)

m1<-lm(state_diff~breath2_diff+breath1_diff, data=bdata)
m2<-lm(state_diff~breath2_diff+Group, data=bdata)
tab_model(m1, m2, show.ci=FALSE)

#mixed model
m1<-lmer(state_diff~ breath1_diff +breath2_diff+(1|Group) ,data=bdata, REML = T)
tab_model(m1, show.ci=FALSE)
#Rsqr=0.011,

m1<-lmer(state~ vas+trait+(1|Group/id)+(1|Stage) ,data=data, REML = T)
tab_model(m1, show.ci=FALSE)

#KNN regression
#with breath_diff
kdata<-bdata%>% select(breath1_diff, breath2_diff, breath1, breath2, state_diff)%>%
  mutate(breath1_diff=nor(breath1_diff), breath2_diff=nor(breath2_diff), state_diff=nor(state_diff))

#with breath
kdata<-bdata%>% select(breath1, breath2, state_diff)%>%
  mutate(breath1=nor(breath1), breath2=nor(breath2), state_diff=nor(state_diff))

kdata<-bdata%>% select(breath1, breath1_diff, state_diff)%>%
  mutate(breath1=nor(breath1), breath1_diff=nor(breath1_diff), state_diff=nor(state_diff))


#train.control <- trainControl(method  = "LOOCV")
fit <- train(x=kdata[,1:2], y=as.numeric(kdata[,3]),
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:20),
             trControl  = train.control,
             data       = kdata)
sd(kdata$state_diff)#0.22


#-------------------------Stage CCCCCC
cdata<-data%>%filter(Stage=="C")
cdata$hr_diff<-(cdata$heart_ratet-adata$heart_ratet)/adata$heart_ratet
cdata$state_diff<-cdata$state-adata$state
cdata$vas_diff<-cdata$vas-adata$vas
cdata$breath1_diff<-(cdata$breath1-adata$breath1)#/adata$breath1
cdata$breath2_diff<-(cdata$breath2-adata$breath2)#/adata$breath2

#--------------------------------------HRV CCC
#linear regression with group as a predictor/moderator
m1<-lm(hr_diff~breath1*Group, data=cdata)
m2<-lm(hr_diff~breath1+Group, data=cdata)

m1<-lm(hr_diff~breath1_diff*Group, data=cdata)
m2<-lm(hr_diff~breath1_diff+Group, data=cdata)

m1<-lm(hr_diff~breath2_diff+breath1_diff, data=cdata)
m2<-lm(hr_diff~breath2_diff+Group, data=cdata)
tab_model(m1, m2, show.ci=FALSE)

#mixed model
m1<-lmer(hr_diff~ breath1_diff +breath2_diff+(1|Group) ,data=cdata, REML = T)
tab_model(m1, show.ci=FALSE)
#Rsqr=0.105, breath1 is significant with an estimate of 0

#KNN regression
#with breath_diff
kdata<-cdata%>% select(breath1_diff, breath2_diff, hr_diff)%>%
  mutate(breath1_diff=nor(breath1_diff), breath2_diff=nor(breath2_diff), hr_diff=nor(hr_diff))

#with breath
kdata<-cdata%>% select(breath1, breath2, hr_diff)%>%
  mutate(breath1=nor(breath1), breath2=nor(breath2), hr_diff=nor(hr_diff))

kdata<-cdata%>% select(breath1, breath1_diff, hr_diff)%>%
  mutate(breath1=nor(breath1), breath2=nor(breath1_diff), hr_diff=nor(hr_diff))


#train.control <- trainControl(method  = "LOOCV")
fit <- train(x=kdata[,1:2], y=as.numeric(kdata[,3]),
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:20),
             trControl  = train.control,
             data       = kdata)
sd(kdata$hr_diff)#0.265




#-----------------------------------imputation
#imputing missing values of state anxiety
m1<-lmer(state~ vas+trait+(1|Group/id)+(1|Stage) ,data=data, REML = T)
m1<-lmer(state~ vas+(1|id)+(1|Stage) ,data=data, REML = T)
#Rsq=0.741
m1<-lmer(state~ vas+trait+(1|id) ,data=data, REML = T)
tab_model(m1, show.ci=FALSE)
#marginal/conditional Rsq=0.697/0.782
predict(m1, missing)
#31, 54

missing<-data%>%filter(is.na(state))%>%select
train<-data%>%filter(!is.na(state))
train_x<-train[,c(2,12:13, 16)]
train_y<-train[,15]
knnmodel = knnreg(train_x, train_y)

#derive predictions for the test set
pred_y = predict(knnmodel, data.frame(missing))

print(data.frame(test_y, pred_y))


ggplot(cdata, aes(x=breath1_d, y=state, color=Group))+geom_point()
cdata$breath1_d[cdata$breath1_d>10]<-10
cdata$breath1_d[cdata$breath1_d< -10]<- -8

#-----------------KNN model with 95% breath data
pca <- prcomp(br_data[c(4:43)], center = TRUE, scale = TRUE)
pc1<-pca$x[,1]
pc2<-pca$x[,2]
pc3<-pca$x[,3]
pc4<-pca$x[,4]
pc5<-pca$x[,5]
pc6<-pca$x[,6]
#adding breath vars to the data set
data<-data%>%mutate(breath1=pc1, breath2=pc2, breath3=pc3, breath4=pc4, breath5=pc5,breath6=pc6)
adata<-data%>%filter(Stage=="A")
kdata<-adata%>% select(breath1, breath2, breath3, breath4, breath5, breath6, vas)%>%
  mutate(breath1=nor(breath1), breath2=nor(breath2), breath3=nor(breath3), breath4=nor(breath4), 
         breath5=nor(breath5), breath6=nor(breath6),vas=nor(vas))


#train.control <- trainControl(method  = "LOOCV")
fit <- train(x=kdata[,1:6], y=as.numeric(kdata[,7]),
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:20),
             trControl  = train.control,
             data       = kdata)
#K=2, rmse=0.31
sd(kdata$vas)#0.30


#-----------------KNN model of trait by 