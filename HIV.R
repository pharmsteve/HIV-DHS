#R Code for Paper: "HIV infection risk among women in South Africa: exploring the interplay between financial autonomy, sexual autonomy and intimate partner violence"

library(haven) #To read Stata dataset into R
library(descr)

wom<- read_dta("C://Dataset//ZAIR71FL.DTA") #Import women's dataset
hiv<- read_dta("C://Dataset//ZAAR71FL.DTA") #Import HIV dataset

#Select relevant variables from the main dataset
sub<- subset(wom, select = c(v001,v002,v003,v005,v012,v013,v025,v133,v149,v745a,v170,v169a,v190,v201,v501,v502,v531,v836,v834a,v714,v731,v739,
                             v833a,v766b,v850a,v850b,d104,d106,d107,d108,v746,v473b, d005))

#Create similar "ID" variables in the women's dataset and HIV dataset for merging
sub$ID <- as.numeric(paste(sub$v001, sub$v002, sub$v003, sep = ""))
hiv$ID <- as.numeric(paste(hiv$hivclust, hiv$hivnumb, hiv$hivline, sep = ""))

#Merge both datasets by women's ID
mer<-merge(sub,hiv,by="ID")

#Retain people who have ever been in a union (currently or previously had a partner or husband)
mer<- subset(mer, mer$v502!=0 )

#Create HIV assay result variable
mer$hiv<-NA
mer$hiv[mer$hiv03==0]<-0
mer$hiv[mer$hiv03==1]<-1
freq(mer$hiv)
mer<- subset(mer, mer$hiv!="NA" )

#Age
mer$age<- mer$v012
mer$ageCAT<-NA
mer$ageCAT[mer$age>17 & mer$age<=29]<-1
mer$ageCAT[mer$age>=30 & mer$age<=39]<-2
mer$ageCAT[mer$age>=40 & mer$age<=49]<-3
mer$ageCAT <- factor(mer$ageCAT,
                     levels = c(1,2,3),
                     labels = c("18-29", "30-39", "40-49"))
freq(mer$age)

#Residence
mer$rsd<-NA
mer$rsd[mer$v025==1]<-1
mer$rsd[mer$v025==2]<-2
mer$rsd<- factor(mer$rsd,
                 levels = c(1,2),
                 labels=c("Urban", "Rural"))
freq(mer$rsd)

#Education
mer$edu<- mer$v133 #education in years
mer$edu1<-NA
mer$edu1[mer$v149==0]<-4
mer$edu1[mer$v149==1]<-4
mer$edu1[mer$v149==2]<-3
mer$edu1[mer$v149==3]<-3
mer$edu1[mer$v149==4]<-2
mer$edu1[mer$v149==5]<-1
mer$edu1<- factor(mer$edu1,
                  levels = c(1,2,3,4),
                  labels=c("Higher","Completed Secondary","Completed Primary","Less than primary"))
freq(mer$edu1)

mer$edu2<-NA
mer$edu2[mer$v149==0]<-2
mer$edu2[mer$v149==1]<-2
mer$edu2[mer$v149==2]<-2
mer$edu2[mer$v149==3]<-2
mer$edu2[mer$v149==4]<-1
mer$edu2[mer$v149==5]<-1
mer$edu2<- factor(mer$edu2,
                  levels = c(1,2),
                  labels=c("Completed Secondary", "Less than Secondary"))
freq(mer$edu2)

#Marital status
mer$mar<- NA
mer$mar[mer$v501==1]<-1
mer$mar[mer$v501==2]<-2
mer$mar[mer$v501==3|mer$v501==4|mer$v501==5]<-1
mer$mar<- factor(mer$mar,
                 levels = c(1,2),
                 labels=c("Ever married", "Never married"))
freq(mer$mar)

#Gainfully employed in the past year or not
mer$work<-NA
mer$work[mer$v731==1]<-1
mer$work[mer$v731==2]<-1
mer$work[mer$v731==3]<-1
mer$work[mer$v731==0]<-2
mer$work<- factor(mer$work,
                  levels = c(1,2),
                  labels=c("Yes", "No"))

#Wealth Index
mer$wealth<-NA
mer$wealth[mer$v190==5]<-1
mer$wealth[mer$v190==4]<-2
mer$wealth[mer$v190==3]<-3
mer$wealth[mer$v190==2]<-4
mer$wealth[mer$v190==1]<-5
mer$wealth<- factor(mer$wealth,
                    levels = c(1,2,3,4,5),
                    labels=c("Richest", "Rich", "Middle", "Poor","Poorest"))
freq(mer$wealth)

#Has bank account
mer$bank<-NA
mer$bank[mer$v170==1]<-1
mer$bank[mer$v170==0]<-2
mer$bank<- factor(mer$bank,
                  levels = c(1,2),
                  labels=c("Yes", "No"))
freq(mer$bank)

#Earns more than partner
mer$earn<-NA
mer$earn[mer$v746==1]<-1
mer$earn[mer$v746==2]<-2
mer$earn[mer$v746==3]<-2
mer$earn[mer$v746==4]<-1
mer$earn[mer$v746==8]<-2
mer$earn[is.na(mer$earn)]<-2
mer$earn<- factor(mer$earn,
                  levels = c(1,2),
                  labels=c("Yes", "No"))

freq(mer$earn)

#Independently decides how her earnings are spent
mer$dec<-NA
mer$dec[mer$v739==1]<-1
mer$dec[mer$v739==2]<-2
mer$dec[mer$v739==4]<-2
mer$dec[is.na(mer$dec)]<-2
mer$dec<- factor(mer$dec,
                 levels = c(1,2),
                 labels=c("Yes", "No"))
freq(mer$dec)

#Age-disparate relationship with last partner
mer$disp<- NA
mer$disp<- ifelse(mer$v834a-mer$age >=10, 2,1)
mer$disp[is.na(mer$disp)]<-1
mer$disp<- factor(mer$disp,
                  levels = c(1,2),
                  labels=c("No", "Yes"))
freq(mer$disp)

#Number of sexual partners in past year
mer$partners<-mer$v766b

#Multiple sexual partners in past year
mer$multi<-NA
mer$multi[mer$v766b==0]<-1
mer$multi[mer$v766b==1]<-1
mer$multi[mer$v766b==2]<-2
mer$multi[mer$v766b==3]<-2
mer$multi[mer$v766b==4]<-2
mer$multi[mer$v766b==95]<-2
mer$multi<- factor(mer$multi,
                   levels = c(1,2),
                   labels=c("No", "Yes"))
freq(mer$multi)

#Age at sexual debut <15yrs
mer$debut<-ifelse(mer$v531<15, 1,0)
freq(mer$debut)

#Can refuse sex
mer$ref<-NA
mer$ref[mer$v850a==1]<-1
mer$ref[is.na(mer$ref)]<-2
mer$ref<- factor(mer$ref,
                 levels = c(1,2),
                 labels=c("Yes", "No"))
freq(mer$ref)

#Can ask partner to use a condom
mer$ask<-NA
mer$ask[mer$v850b==1]<-1
mer$ask[is.na(mer$ask)]<-2
mer$ask<- factor(mer$ask,
                 levels = c(1,2),
                 labels=c("Yes", "No"))
freq(mer$ask)

#Experienced Intimate Partner Violence
mer$IPV<-NA
mer$IPV[mer$d104==0&mer$d106==0&mer$d107==0&mer$d108==0]<-1
mer$IPV[mer$d104==1&mer$d106==0&mer$d107==0&mer$d108==0]<-2
mer$IPV[mer$d106==1]<-3
mer$IPV[mer$d107==1]<-3
mer$IPV[mer$d108==1]<-4
mer$IPV<- factor(mer$IPV,
                 levels = c(1,2,3,4),
                 labels=c("None", "Emotional only", "Physical", "Sexual"))
freq(mer$IPV)

mer$cluster<-mer$v001 #Cluster variable
mer$hivwt<- mer$hiv05/1000000 #HIV Weights

mer<- subset(mer, select = -c(v012,v025,v133,v149,v731,v190,v170,v169a,v745a,v502,
                              v746,v739,v834a,v833a,v201,v850a,v850b,v766b,
                              v001,v002,v003,v005,v013,v501, v502,v531,v836,v714,v473b,
                              hivclust,hivnumb,hivline,hiv01,hiv02,hiv03,hiv07,hiv08,
                              d005,hiv05, hiv06)) #remove redundant variables

rm(wom,hiv) #remove redundant datasets

#Create Women's Financial Autonomy Index
mer$fin<- NA
mer$fin[is.na(mer$fin)]=0
mer$fin[mer$work=="Yes"]<-(mer$fin+1)
mer$fin[mer$bank=="Yes"]<-(mer$fin+1)
mer$fin[mer$earn=="Yes"]<-(mer$fin+1)
mer$fin[mer$dec=="Yes"]<-(mer$fin+1)
freq(mer$fin)

#Create tertiles of financial autonomy index
quantile(mer$fin, probs = seq(0, 1, 1/3))
mer$fin2<-NA
mer$fin2[mer$fin==0]<-3
mer$fin2[mer$fin==1]<-2
mer$fin2[mer$fin>=2]<-1
mer$fin2<- factor(mer$fin2,
                  levels = c(1,2,3),
                  labels=c("High", "Medium", "Low"))
freq(mer$fin2)

#Create Women's Sexual Autonomy Index
mer$sex<- NA
mer$sex[is.na(mer$sex)]=0
mer$sex[mer$ref=="Yes"]<-(mer$sex+1)
mer$sex[mer$ask=="Yes"]<-(mer$sex+1)
freq(mer$sex)

#Create tertiles of sexual autonomy index
quantile(mer$sex, probs = seq(0, 1, 1/3))
mer$sex2<-NA
mer$sex2[mer$sex==0]<-3
mer$sex2[mer$sex==1]<-2
mer$sex2[mer$sex==2]<-1
mer$sex2<- factor(mer$sex2,
                  levels = c(1,2,3),
                  labels=c("High", "Medium", "Low"))

mer1<-subset(mer, mer$IPV!="NA")

#ANALYSIS

mean(mer1$age)
sd(mer1$age)

#TABLE 1: Descriptive Stats
library(expss) #This package enables the computation of frequencies using sampling weights
fre(mer1$edu1, weight = mer1$hivwt)
fre(mer1$ageCAT, weight = mer1$hivwt)
fre(mer1$rsd, weight = mer1$hivwt)
fre(mer1$mar, weight = mer1$hivwt)
fre(mer1$wealth, weight = mer1$hivwt)
fre(mer1$work, weight = mer1$hivwt)
fre(mer1$bank, weight = mer1$hivwt)
fre(mer1$earn, weight = mer1$hivwt)
fre(mer1$dec, weight = mer1$hivwt)
fre(mer1$ref, weight = mer1$hivwt)
fre(mer1$ask, weight = mer1$hivwt)
fre(mer1$IPV, weight = mer1$hivwt)
fre(mer1$debut, weight = mer1$hivwt)
fre(mer1$multi, weight = mer1$hivwt)
fre(mer1$disp, weight = mer1$hivwt)
fre(mer1$hiv, weight = mer1$hivwt)

#TABLE 2: Relationships between main explanatory varaibles
crosstab(mer1$fin2, mer1$IPV, prop.c = T, chisq = T) #crosstab of IPV vs financial autonomy
crosstab(mer1$sex2, mer1$IPV, prop.r = T, chisq = T) #crosstab of IPV vs sexual autonomy
crosstab(mer1$sex2, mer1$fin2, prop.r = T, chisq = T)#crosstab of sexual and financial autonomy
cor(mer1$fin, mer1$sex) #correlation of sexual and financial autonomy indices

#Stacked bar graph in fig 1 
#Table below was prepared from the crosstabulation of financial and sexual autonomy above)

sex<- c(rep("cLow", 3), rep("bMedium", 3), rep("aHigh", 3))
fin<- c("cLow", "bMedium", "aHigh", "cLow", "bMedium", "aHigh", "cLow", "bMedium", "aHigh")
frequency<- c(72,92,54,69,83,86,98,90,85)
tab<-data.frame(sex,fin,frequency)

library(ggplot2)
library(RColorBrewer)
ggplot(tab, aes(fill=fin, y=frequency, x=sex)) + 
  geom_bar(position="fill", stat="identity")+
  labs(x="Sexual autonomy", y="Proportion", col="Site")+
  scale_fill_brewer(palette = "Dark2")+
  theme_bw()

# Chi-square test for trend: No violence across financial autonomy levels
vio.vector <- c(176, 199, 171)
total.vector<- c(225, 265, 239)
prop.trend.test(vio.vector, total.vector)

# Chi-square test for trend: No violence across sexual autonomy levels
vio.vector <- c(211, 176, 159)
total.vector<- c(273, 238, 218)
prop.trend.test(vio.vector, total.vector)

# Chi-square test for trend: sexual across sexual autonomy levels
vio.vector <- c(5, 9, 9)
total.vector<- c(225, 265, 239)
prop.trend.test(vio.vector, total.vector)

#TABLE 3, Section A: Bivariate associations of HIV with women's characteristics
crosstab(mer1$hiv, mer1$edu1, prop.c = T, chisq = T)
crosstab(mer1$hiv, mer1$ageCAT, prop.c = T, chisq = T)
crosstab(mer1$hiv, mer1$rsd, prop.c = T, chisq = T)
crosstab(mer1$hiv, mer1$mar, prop.c = T, chisq = T)
crosstab(mer1$hiv, mer1$wealth, prop.c = T, chisq = T)
crosstab(mer1$hiv, mer1$fin2, prop.c = T, chisq = T)
crosstab(mer1$hiv, mer1$work, prop.c = T, chisq = T)
crosstab(mer1$hiv, mer1$bank, prop.c = T, chisq = T)
crosstab(mer1$hiv, mer1$earn, prop.c = T, chisq = T)
crosstab(mer1$hiv, mer1$dec, prop.c = T, chisq = T)
crosstab(mer1$hiv, mer1$sex2, prop.c = T, chisq = T)
crosstab(mer1$hiv, mer1$ref, prop.c = T, chisq = T)
crosstab(mer1$hiv, mer1$ask, prop.c = T, chisq = T)
crosstab(mer1$hiv, mer1$IPV, prop.c = T, chisq = T)
crosstab(mer1$hiv, mer1$debut, prop.c = T, chisq = T)
crosstab(mer1$hiv, mer1$disp, prop.c = T, chisq = T)
crosstab(mer1$hiv, mer1$multi, prop.c = T, chisq = T)

#TABLE 3, Section B: Bivariate logistic regressions of HIV with women's characteristics

library(miceadds) #Executes cluster-robust standard errors for general linear models
library(sandwich) #Required by miceadds

lm1<-glm.cluster(hiv~edu1, data=mer1, cluster = mer1$cluster, family=binomial)
exp(cbind(OR = coef(lm1), confint.default(lm1))) #HIV vs education
lm1<-glm.cluster(hiv~ageCAT, data=mer1, cluster = mer1$cluster, family=binomial)
exp(cbind(OR = coef(lm1), confint.default(lm1))) #HIV vs age
lm1<-glm.cluster(hiv~rsd, data=mer1, cluster = mer1$cluster, family=binomial)
exp(cbind(OR = coef(lm1), confint.default(lm1))) #HIV vs residence
lm1<-glm.cluster(hiv~mar, data=mer1, cluster = mer1$cluster, family=binomial)
exp(cbind(OR = coef(lm1), confint.default(lm1))) #HIV vs marital status
lm1<-glm.cluster(hiv~wealth, data=mer1, cluster = mer1$cluster, family=binomial)
exp(cbind(OR = coef(lm1), confint.default(lm1))) #HIV vs household wealth
lm1<-glm.cluster(hiv~fin2, data=mer1, cluster = mer1$cluster, family=binomial)
exp(cbind(OR = coef(lm1), confint.default(lm1))) #HIV vs financial autonomy tertiles
lm1<-glm.cluster(hiv~work, data=mer1, cluster = mer1$cluster, family=binomial)
exp(cbind(OR = coef(lm1), confint.default(lm1))) #HIV vs working status
lm1<-glm.cluster(hiv~bank, data=mer1, cluster = mer1$cluster, family=binomial)
exp(cbind(OR = coef(lm1), confint.default(lm1))) #HIV vs personal bank account
lm1<-glm.cluster(hiv~earn, data=mer1, cluster = mer1$cluster, family=binomial)
exp(cbind(OR = coef(lm1), confint.default(lm1))) #HIV vs earns more than partner
lm1<-glm.cluster(hiv~dec, data=mer1, cluster = mer1$cluster, family=binomial)
exp(cbind(OR = coef(lm1), confint.default(lm1))) #HIV vs decides how earnings are spent
lm1<-glm.cluster(hiv~sex2, data=mer1, cluster = mer1$cluster, family=binomial)
exp(cbind(OR = coef(lm1), confint.default(lm1))) #HIV vs sexual autonomy
lm1<-glm.cluster(hiv~ref, data=mer1, cluster = mer1$cluster, family=binomial)
exp(cbind(OR = coef(lm1), confint.default(lm1))) #HIV vs can refuse sex
lm1<-glm.cluster(hiv~ask, data=mer1, cluster = mer1$cluster, family=binomial)
exp(cbind(OR = coef(lm1), confint.default(lm1))) #HIV vs can ask partner to use condom
lm1<-glm.cluster(hiv~IPV, data=mer1, cluster = mer1$cluster, family=binomial)
exp(cbind(OR = coef(lm1), confint.default(lm1))) #HIV vs IPV
lm1<-glm.cluster(hiv~debut, data=mer1, cluster = mer1$cluster, family=binomial)
exp(cbind(OR = coef(lm1), confint.default(lm1))) #HIV vs age at sexual debut <=15 years
lm1<-glm.cluster(hiv~disp, data=mer1, cluster = mer1$cluster, family=binomial)
exp(cbind(OR = coef(lm1), confint.default(lm1))) #HIV vs age disparate relationship
lm1<-glm.cluster(hiv~multi, data=mer1, cluster = mer1$cluster, family=binomial)
exp(cbind(OR = coef(lm1), confint.default(lm1))) #HIV vs multiple sexual partners


# TABLE 4: Multivariate logistic regression model

#Quadratic term for age
mer1$ages<-mer1$age^2

#Model
freq(mer1$mar)
lm1<-glm.cluster(hiv~sex2+fin2+IPV+edu2+age+ages+rsd+mar+wealth+partners, data=mer1, cluster = mer1$cluster, family=binomial)
summary(lm1)
exp(cbind(OR = coef(lm1), confint.default(lm1)))

#Test for model fit
par(mfrow = c(2, 2))
plot(lm1$glm_res) #Looks good!