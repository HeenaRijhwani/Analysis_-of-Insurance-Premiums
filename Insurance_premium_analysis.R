#setwd("...")
df<-read.csv("MedicalPremium.csv")
#Data cleaning and EDA
#first 6 rows
head(df)
#last 6 rows
tail(df)
#dimensions of data
dim(df)
nrow(df)
ncol(df)

colnames(df)
#datatypes of columns
sapply(df,class)
#Converting to required data types
df$Diabetes<-factor(df$Diabetes, ordered=F, labels=c('No','Yes'))
df$BloodPressureProblems<-factor(df$BloodPressureProblems, ordered=F,labels=c("No","Yes"))
df$AnyChronicDiseases<-factor(df$AnyChronicDiseases, ordered=F,labels=c("No","Yes"))
df$AnyTransplants<-factor(df$AnyTransplants, ordered=F,labels=c("No","Yes"))
df$KnownAllergies<-factor(df$KnownAllergies, ordered=F,labels=c("No","Yes"))
df$HistoryOfCancerInFamily<-factor(df$HistoryOfCancerInFamily, ordered=F,labels=c("No","Yes"))
unique(df$NumberOfMajorSurgeries)
df$NumberOfMajorSurgeries<-factor(df$NumberOfMajorSurgeries, ordered=T,levels=c(0,1,2,3))
#rechecking datatypes
sapply(df,class)

#checking for null values
colSums(is.na(df))
#no null values found

#taking 500 observations 
set.seed(123)
df2<-df[sample(nrow(df),500),]
dim(df2) #500 observations, 11 rows

#checking for outliers
#for Height column
lower.bound<-quantile(df2$Height,0.25)-1.5*IQR(df2$Height)
upper.bound<-quantile(df2$Height,0.75)+1.5*IQR(df2$Height)
df2$Height[df2$Height<lower.bound | df2$Height>upper.bound]
#no outliers
#for Weight column
lower.bound2<-quantile(df2$Weight,0.25)-1.5*IQR(df2$Weight)
upper.bound2<-quantile(df2$Weight,0.75)+1.5*IQR(df2$Weight)
df2$Weight[df2$Weight<lower.bound2 | df2$Weight>upper.bound2]
boxplot(df2$Weight,main="Boxplot for Weight to check for outliers")
df2<-subset(df2,df2$Weight>lower.bound2 & df2$Weight<upper.bound2)
dim(df2)
#for charges column
lower.bound3<-quantile(df2$PremiumPrice,0.25)-1.5*IQR(df2$PremiumPrice)
upper.bound3<-quantile(df2$PremiumPrice,0.75)+1.5*IQR(df2$PremiumPrice)
df2$PremiumPrice[df2$PremiumPrice<lower.bound3 | df2$PremiumPrice>upper.bound3]
df2<-subset(df2,df2$PremiumPrice>lower.bound3 & df2$PremiumPrice<upper.bound3)
dim(df2)
write.csv(df2,"Medical_Premiums_cleaned.csv") #saving cleaned data

#Research question 1: Does family history of Cancer have an association with insurance premiums
#Summarizing the data based on groups- splitting into subsets of customers with/without family history of Cancer and calculating summary for each group
aggregate(df2$PremiumPrice, by =list(df2$HistoryOfCancerInFamily), FUN=summary)
#or
library("dplyr")
df2%>%group_by(HistoryOfCancerInFamily)%>%summarise(mean=mean(PremiumPrice))
df2%>%group_by(HistoryOfCancerInFamily)%>%summarise(median=median(PremiumPrice))
df2%>%group_by(HistoryOfCancerInFamily)%>%summarise(stddev=sd(PremiumPrice))
df.a<-subset(df2,HistoryOfCancerInFamily=="No")
df.b<-subset(df2,HistoryOfCancerInFamily=="Yes")

#Boxplot to compare the 2 groups
par(mfrow=c(1,1))
range(df2$PremiumPrice)
boxplot(df2$PremiumPrice~df2$HistoryOfCancerInFamily,ylim=c(15000,40000),xlab="History of Cancer in family", ylab="Premium prices", main="Boxplot to compare insurance costs paid by those with and without a history of cancer in their family",col=c('pink','lavender'))


#Step 1: H0:mean insurance cost for customers without family history of Cancer=mean hospital insurance cost for customers with family history of Cancer
#Ha:mean insurance cost for customers with family history of Cancer>insurance cost for customers without family history of Cancer

#Step 2: Select test statistic: t statistic for 2 sample t test 
#t=xbar1-xbar2/(sqrt((s1**2)/n1 + (s2**2)/n2))

#Step 3: critical t value
degree.of.freedom<-min(nrow(df.a)-1,nrow(df.b)-1)
degree.of.freedom #52
critical.t<-qt(0.975, degree.of.freedom)
critical.t #2

#Step 4:Compute statistic
t.test(df.b$PremiumPrice,df.a$PremiumPrice,conf.level=0.95,alternative="greater")
#t=2.0767
#or
t<-(mean(df.b$PremiumPrice)-mean(df.a$PremiumPrice))/(sqrt( ((sd(df.a$PremiumPrice)^2)/nrow(df.a))+((sd(df.b$PremiumPrice)^2)/nrow(df.b))))
t
#Step 4:Conclusion
#Since t> critical.t, reject null hypothesis. At 0.05 significance level, we conclude that customers with a family history of Cancer pay higher for insurance as comapred to customers without a family history of Cancer.

#Assumptions of t test:
#Independence: Both samples, insurance prices for customers with family history of Cancer and customers without family history of Cancer are independent of each other. They do not influence each other. 
#Data values for insurance policy are randomly selected from entire population.
#Same Measurement: The variable of interest i.e. mean of  both samples is measured in the same manner by adding all values and dividing by number of values.
#No outliers in the data. They have been removed during data cleaning.
#Continuous data: Since data values represent policy costs, they are continuous measurements.
#No strong skew observed.

par(mfrow=c(1,2))
range(df.a$PremiumPrice)
range(df.b$PremiumPrice)
hist(df.a$PremiumPrice,breaks=seq(15000,40000,5000),xlim=c(15000,40000),main="Histogram for customers: without family Cancer history",xlab="Premium price",prob=TRUE)
lines(density(df.a$PremiumPrice),lwd=2,col="black")
hist(df.b$PremiumPrice,xlim=c(15000,40000),main="with family history of Cancer", xlab="Premium price",prob=TRUE)
lines(density(df.b$PremiumPrice),lwd=2,col="black")
mean(df.b$PremiumPrice)
median(df.b$PremiumPrice)
mean(df.a$PremiumPrice)
median(df.a$PremiumPrice)
library("moments")
skewness(df.a$PremiumPrice)
skewness(df.b$PremiumPrice)
#CI
(mean(df.a$PremiumPrice)-mean(df.b$PremiumPrice))-(critical.t*sqrt((sd(df.a$PremiumPrice^2)/nrow(df.a))+ (sd(df.b$PremiumPrice^2)/nrow(df.b))))
(mean(df.a$PremiumPrice)-mean(df.b$PremiumPrice))+(critical.t*sqrt((sd(df.a$PremiumPrice^2)/nrow(df.a))+ (sd(df.b$PremiumPrice^2)/nrow(df.b))))                                                   
mean(df.b$PremiumPrice)
median(df.b$PremiumPrice)

#Research question 2- Examining association between age, weight, height and premium costs using correlation tests. How well does a combined model with age, height, weight predict insurance costs using Multiple Linear Regression. Are age, height, weight significant predictors of health care costs using Global F test and subsequent t tests?
#For age
#Set up the hypotheses and select the alpha level
#H0:B=0  (there is no linear association)
#H1:B!=0  (there is a linear association)
#??=0.05

#Select the appropriate test statistic
#t=r*sqrt(n???2/1???r^2)  
degf=nrow(df2)-2 #n-2
degf

#State the decision rule
qt(0.975,df=degf)
#Decision Rule: Reject H0 if |t|>=1.96, otherwise accept H0.


#Compute the test statistic
r<-cor(df2$Age,df2$PremiumPrice)
t2<-r*(sqrt(483/(1-r^2)))
t2 #22.53

#Conclusion
#Reject H0 since 22.53>1.96. We have significant evidence at the alpha=0.05 level that B!=0. That is, there is evidence of a significant linear association between age and premium costs.

#For height
#Set up the hypotheses and select the alpha level
#H0:B=0  (there is no linear association)
#H1:B!=0  (there is a linear association)
#alpha=0.05

#Select the appropriate test statistic
degf=nrow(df2)-2 #n-2
degf

#State the decision rule
qt(0.975,df=degf) #1.96
#Decision Rule: Reject H0 if |t|>=1.96, otherwise accept H0.


#Compute the test statistic
r2<-cor(df2$Height,df2$PremiumPrice)
t3<-r2*(sqrt(483/(1-r2^2)))
t3 #-1.03

#Conclusion
#Fail to reject H0 since -1.03<1.96. We do not have significant evidence at the ??=0.05 level that ?????0. That is, there is not enough evidence to conclude significant linear association between height and premium costs.

#For weight
#Set up the hypotheses and select the alpha level
#H0:B=0  (there is no linear association)
#H1:B!=0  (there is a linear association)
#alpha=0.05

#Select the appropriate test statistic
degf=nrow(df2)-2 #n-2
degf

#State the decision rule
qt(0.975,df=degf)
#Decision Rule: Reject H0 if |t|>=1.96, otherwise accept H0.


#Compute the test statistic
r3<-cor(df2$Weight,df2$PremiumPrice)
t4<-r3*(sqrt(483/(1-r3^2)))
t4 #3.39

#Conclusion
#Reject H0 since 3.39>1.96. We have significant evidence at the ??=0.05 level that ?????0. That is, there is evidence of a significant linear association between weight and premium costs.

#MLR
#Here response variable= premium rice and explanatory variables are age, weight, height
mdl<-lm(data=df2,PremiumPrice~Height+Age+Weight)
summary(mdl)
plot(mdl)
hist(resid(mdl),main="Histogram for residuals of MLR model")
#R2= 0.54, hence 54% of the variation in Premium prices is explained by the linear model.
#y=6951-11.94x1+318.16x2+79.82x3, whwre y=premium price, x1=height, x2=age and x3=weight.
#B0 (intercept): value of y when x =0. In this case value of insuarnce cost when age, height and weight are 0 will be 6951.60. B0 interpretation is not relevant here since these parametsrs cannot be zero.
#B1(coefficient): Premium price decreases by 11.94 when there is a unit increase in height, after controlling for age and weight.  Premium price increases by 318.16 when there is a unit increase in age, after controlling for height and weight. Premium price increases by 79.82 when there is a unit increase in weight, after controlling for age and height.

#RMSE
sqrt(mean((df2$PremiumPrice-fitted(mdl))^2))


#Applying log transform on y to improve model
y<-log(df2$PremiumPrice)
mdllog<-lm(data=df2,y~Age+Height+Weight)
summary(mdllog)
sqrt(mean((df2$PremiumPrice-fitted(mdllog))^2))

#Checking for assumptions of MLR
#Assumption1: Linearity between y(premium price) and each independent variable
par(mfrow=c(1,1))
plot(mdl,which=1)

#Additivity assumption is satisfied as well since there is no categorical variable.
#Assumption2: Independence- Each row represents a unique customer, hence this assumption is valid.
install.packages("GGally")
require(GGally)
df3<-df2[,c(1,6,7)]
ggpairs(df3) + theme_bw()

#Assumption3: Constant variance. This can be verified using the scale location plot. The red line here is mostly horizontal, there are no obvious bumps. 
#Assumption4:4.Normality-Residuals should follow a normal distribution. This can be verified from the Q-Q plot. Distribution is close to normal with light deviation in the region of higher quantiles. 

#Checking for influence points- points with id 74,625 and 447
df3<-df2[-74,]
mdl2<-lm(df3$PremiumPrice~df3$Height+df3$Weight+df3$Age)
summary(mdl)
summary(mdl2)
df3b<-df2[-447,]
mdl2b<-lm(df3b$PremiumPrice~df3b$Height+df3b$Weight+df3b$Age)
summary(mdl2b)
df3c<-df2[-625,]
mdl2c<-lm(df3c$PremiumPrice~df3c$Height+df3c$Weight+df3c$Age)
summary(mdl2c)
#No influence points found.

#Global F test
#Step 1: Set hypothesis
#H0: B1=0 i.e. B(age)=0,B(height)=0 and B(weight)=0 (no association)
#Ha: B1!=0 i.e. B(age)!=0 and/or B(height)!=0 and/or B(women)!=0 (atleast one of the predictors is significant)

#Step 2: Decide test statistic
#F=Regression mean square/Residual mean square
degree.of.freedom1=3 #k
dim(df2)
degree.of.freedom2=485-4 #n-k-1
degree.of.freedom2 #481

#Step 3:Decision rule 
alpha=0.05
qf(0.95,df1=degree.of.freedom1,df2=degree.of.freedom2)
#If f>= critical f value, i.e. f>=2.623, reject null hypothesis. Otherwise, accept null hypothesis.

#Step 4: Calculate test statistic
summary(mdl)
#F statistic=188.9

#Step 5: Conclusion
#Since F> critical , reject null hypothesis that B=0.
#Thus at 0.05 significance level we can say that atleast one out of age, height and weight are significant predictors of insurance cost.

#t tests on each individual parameter
#For age
#Step 1: Set hypothesis
#H0: B(age)=0(after controlling for height,weight)
#Ha: B(age)!=0 (after controlling for height,weight)

#Step 2: select test statistic
#one sample 2 sided t test
#t=B/SE(B)
degree.of.freedom4<-nrow(df2)-4 #n-k-1
degree.of.freedom4

#Step 3: State decision rule
#If |t|>= critical t value, reject null hypothesis, otherwise accept H0.
qt(0.975,df=degree.of.freedom4) #1.964

#Step 4: Compute test statistic
summary(mdl)
#t=23.217
#or
318.16/13.70

#Step 5: Conclusion
#Since |t|> critical t, reject null hypothesis.
#Thus at 0.05 significance level we can say that B(age)!=0 after controlling for height and weight.


#For height
#Step 1: Set hypothesis
#H0: B(height)=0(after controlling for age,weight)
#Ha: B(height)!=0 (after controlling for age,weight)

#step 2: select test statistic
#one sample 2 sided t test
#t=B/SE(B)
degree.of.freedom5<-nrow(df2)-4 #n-k-1

#Step 3: State decision rule
#If |t|>= critical t value, reject null hypothesis, otherwise accept H0.
qt(0.975,df=degree.of.freedom5) #1.964

#Step 4: Compute test statistic
summary(mdl)
#t=-0.626

#Step 5: Conclusion
#Since |t|< critical t, fail to reject null hypothesis.
#Thus at 0.05 significance level we do not have enough evidence to conclude that B(height)!=0 after controlling for age and weight.

#For weight
#Step 1: Set hypothesis
#H0: B(weight)=0(after controlling for age,height)
#Ha: B(weight)!=0 (after controlling for age,height)

#step 2: select test statistic
#one sample 2 sided t test
#t=B/SE(B)
degree.of.freedom6<-nrow(df2)-4 #n-k-1

#Step 3: State decision rule
#If |t|>= critical t value, reject null hypothesis, otherwise accept H0.
qt(0.975,df=degree.of.freedom6) #1.964

#Step 4: Compute test statistic
summary(mdl)
#t=5.456

#Step 5: Conclusion
#Since |t|> critical t, reject null hypothesis.
#Thus at 0.05 significance level we can say that B(weight)!=0 after controlling for age and height.
#Thus age,height are significant contributors towards premium costs after controlling for other variables.

#Model is significant (from Global F test). On further t tests, we found that two out of the 3 features are significant.

#Confidence intervals
confint(mdl,level=0.95)

##Research question 3: Are premium prices paid by young people, middle aged people and seniors the same?
df2$Agegroup[df2$Age >= 18 & df2$Age < 34] = "Young"
df2$Agegroup[df2$Age >= 34 & df2$Age < 50] = "Middle.aged"
df2$Agegroup[df2$Age >=50] = "Senior"
df2$Agegroup<-factor(df2$Agegroup)
#Using One way ANOVA (Analysis of Variance) to compare means of more than 2 groups.
#1. Set up the hypotheses and select the alpha level
#H0 ??? u(young) = u(middleaged) =u(µsenior) (All underlying population means are equal)
#H1 ??? uii != uj for some i and j (Not all of the underlying population means are equal)
#alpha = 0.05

#2. Select the appropriate test statistic
#F =Mean Square Between/Mean Square Within 
#k ??? 1 = 2 and n ??? k = 485 ??? 3 = 482 degrees of freedom
nrow(df2)-3

#3. State the decision rule 
qf(0.95, df1=2, df2=482)
#critical F=3.0144
#Decision Rule: Reject H0 if F ??? 3.0144. Otherwise, do not reject H0.

#4. Compute the test statistic
m<-aov(PremiumPrice~Agegroup,data=df2)
summary(m)
#or
m2<-lm(PremiumPrice~Agegroup,data=df2)
anova(m2)


#5. Conclusion
#Reject H0 since 244.4 > 3.0144.
#We have significant evidence at alpha = 0.05 level that there is a difference in Premium prices among yound, middle aged and senior people.

#Count of people in each group
table(df2$Agegroup)
#summarising price based on age groups
aggregate(df2$PremiumPrice, by=list(df2$Agegroup), summary)
#Boxplot for premium prices across 3 age categories
boxplot(df2$PremiumPrice~df2$Agegroup,xlab="Age groups", ylab="Premium prices", main="Boxplot to compare insurance costs paid by young, middle aged and senior people",col=c('pink','lavender',"light green"))

#Pairwise comparisons
#Step 1: State hypothesis
#H0, null hypothesis: ui=uj, means of both groups are equal 
#Ha, alternate hypothesis, ui !=uj, groups means are not equal

#Step 2: Decide test statistic
#t = B/ SE
#two sided t test
#degree of freedom=n-k, where n=number of observations in each group and k=number of groups

#Step 3: State decision rule
#If |t|>= critical t value, reject null hypothesis. Otherwise accept H0.
#Or if p<0.05, reject null hypothesis. Otherwise accept H0.

#Step 4: Calculate test statistic
TukeyHSD(m) #using tukey procedure to adjust for family wise type 1 error rate.

#Anova and Regression
#creating dummy variables

df2$young<-ifelse(df2$Agegroup=="Young",1,0)
df2$middle<-ifelse(df2$Agegroup=="Middle.aged",1,0)
df2$old<-ifelse(df2$Agegroup=="Old",1,0)
head(df2)

m3<-lm(PremiumPrice~young+middle,data=df2) #setting Seniors as reference group (using k-1 dummy variables in linear model to avoid multicollinearity)
summary(m3)
anova(m3)

#Checking assumptions
hist(df2$PremiumPrice,xlim=c(13000,40000),ylim=c(0,140),ylab="Price",main="Histogram for price")
mean(df2$PremiumPrice)
median(df2$PremiumPrice)
aggregate(df2$PremiumPrice, by=list(df2$Agegroup), var)
