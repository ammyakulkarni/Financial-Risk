setwd("C:/Users/Amit Kulkarni/Documents/R Programming/Financial Risk Analytics")
data=read.csv("vd.csv")
#dim(data)
#head(data)
#nrow(data)
#str(data)
#summary(data)
View(data)
sum(is.na(data))
data=data[-c(1,22)]
#checking for missing values
sum(is.na(data))
gg_miss_upset(data)
#checking the intersection of the missing data
gg_miss_upset(data, nsets= n_var_miss(data))
# to consider values other than the missing values below code to be done
data1<-data[!(is.na(data$Other.income))|
              !(is.na(data$Deferred.tax.liability))|
              !(is.na(data$Contingent.liabilities))|
              !(is.na(data$Investments))|
              !(is.na(data$PE.on.BSE)),]

gg_miss_upset(data1)

gg_miss_upset(data1, nsets= n_var_miss(data1))

#Checking the data distribution for all the variables

p.data <- data1
p.data %>%
  gather(metric, value)%>%
  ggplot(aes(value, fill = metric))+
  geom_density(show.legend = FALSE)+
  facet_wrap(~metric, scales = "free")

#Convert data to long format for plotting
p.stack<-melt(p.data)
p.stack
ggplot(data=p.stack, aes(y=value))+
  geom_boxplot(color= "dark blue", fill="red", aplha=0.3)+
  facet_wrap(~variable, scales = "free_x", nrow = 3)+
  coord_flip()

#looking at oulier treatment
aa<-quantile(p.data$Total.assets, probs = c(0.01, 0.99),na.rm = TRUE)
aa
summary(p.data$Total.assets)

p.range<- NA
for(i in 1:(ncol(p.data))){
  Statistic <- data.frame(
    "column" = colnames(p.data[i]),
    "min value" = min(p.data[[i]], na.rm = TRUE),
    "1st Percentile" = quantile(p.data[[i]],probs = c(0.01), na.rm = TRUE),
    "max value" = max(p.data[[i]], na.rm = TRUE),
    "99th Percentile" = quantile(p.data[[i]],probs = c(0.99), na.rm = TRUE))
  p.range<-rbind(p.range, Statistic)
}
p.range <- data.table :: data.table(p.range)
p.range
summary(p.range)
#setting capping and flooring to the data at 1 and 99 percentile
p.newdata<-p.data %>%
  transmute_all(funs(squish(.,quantile(.,c(0.01,0.99), na.rm = TRUE))))
summary(p.newdata)
# rechecking the boxplot after capping and flooring
p.stack<-melt(p.newdata)
ggplot(data=p.stack, aes(y=value))+
  geom_boxplot(color= "red", fill="dark blue", aplha=0.3)+
  facet_wrap(~variable, scales = "free_x", nrow = 3)+
  coord_flip()
# Rechecking the distribution post the capping and flooring
p.newdata%>%
  gather(metric, value)%>%
  ggplot(aes(value, fill = metric))+
  geom_density(show.legend = FALSE)+
  facet_wrap(~metric, scales = "free")
# imputing missing values
p.newdata <- p.newdata%>%
  mutate_if(is.numeric, zoo:: na.aggregate)
summary(p.newdata)
View(p.newdata)
View(data1)
dim(p.newdata)
colnames(p.newdata)[49]<-"Networth.Next.Year"
View(p.newdata)
#adding feature variable to dataset
p.newdata$default <- ifelse(p.newdata$Networth.Next.Year<=0, 1, 0)
#re-arranging data syntax example

p.newdata<- p.newdata%>% select(default, everything())
View(p.newdata)
#creating new ratios
p.newdata$Prof.rat<- p.newdata$Total.income/p.newdata$Total.assets
p.newdata$liab<- p.newdata$Total.capital- p.newdata$Borrowings
p.newdata$lev.rat<- p.newdata$Borrowings/p.newdata$liab
p.newdata$liq.rat<- p.newdata$Current.assets/p.newdata$Current.liabilities...provisions
final.data<- p.newdata[-c(3:6, 8:10, 16:20, 22,31, 32,33, 38:46,49)]
View(final.data)
final.data <- final.data[-c(11,18)]

#checking for correlation and multicollinerity
correlation = corrplot(cor(final.data[,-c(25)]), type = "upper")

#Logistic Regression model building
m1 = lm(final.data$default ~ Total.assets, data=final.data)
m2 = lm(final.data$default ~ final.data$Profit.after.tax, data=final.data)
m3 = lm(final.data$default ~ final.data$PBDITA.as...of.total.income, data=final.data)
m4 = lm(final.data$default ~ final.data$PBT.as...of.total.income, data=final.data)
m5 = lm(final.data$default ~ final.data$PAT.as...of.total.income, data=final.data)
m6 = lm(final.data$default ~ final.data$Cash.profit.as...of.total.income, data=final.data)
m7 = lm(final.data$default ~ final.data$PAT.as...of.net.worth, data=final.data)
m8 = lm(final.data$default ~ final.data$Sales, data=final.data)
m9 = lm(final.data$default ~ final.data$Borrowings, data=final.data)
m10 = lm(final.data$default ~ final.data$Deferred.tax.liability, data=final.data)
m11 = lm(final.data$default ~ final.data$Cumulative.retained.profits, data=final.data)
m12= lm(final.data$default ~ final.data$Capital.employed, data=final.data)
m13 = lm(final.data$default ~ final.data$TOL.TNW, data=final.data)
m14 = lm(final.data$default ~ final.data$Total.term.liabilities...tangible.net.worth, data=final.data)
m15 = lm(final.data$default ~ final.data$Contingent.liabilities...Net.worth...., data=final.data)
m16= lm(final.data$default ~ final.data$Contingent.liabilities, data=final.data)
m17= lm(final.data$default ~ final.data$Net.working.capital, data=final.data)
m18= lm(final.data$default ~ final.data$Quick.ratio..times., data=final.data)
m19 = lm(final.data$default ~ final.data$Current.ratio..times., data=final.data)
m20= lm(final.data$default ~ final.data$Debt.to.equity.ratio..times., data=final.data)
m21= lm(final.data$default ~ final.data$EPS, data=final.data)
m22= lm(final.data$default ~ final.data$Adjusted.EPS, data=final.data)
m23 = lm(final.data$default ~ final.data$Networth.Next.Year, data=final.data)
m24 = lm(final.data$default ~ final.data$PE.on.BSE, data=final.data)
m25= lm(final.data$default ~ final.data$Prof.rat, data=final.data)
m26= lm(final.data$default ~ final.data$liab, data=final.data)
#m27= lm(final.data$default ~ final.data$lev.rat, data=final.data)
m28= lm(final.data$default ~ final.data$liq.rat, data=final.data)
summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)
summary(m9)
summary(m10)
summary(m11)
summary(m12)
summary(m13)
summary(m14)
summary(m15)
summary(m16)
summary(m17)
summary(m18)
summary(m19)
summary(m20)
summary(m21)
summary(m22)
summary(m23)
summary(m24)
summary(m25)
summary(m26)
summary(m28) 
final.data<-final.data[-c(28)]
m29= lm(final.data$default ~ ., data=final.data)
summary(m29)

#modelling using the logistic regression
library(SDMTools)
library(pROC)
library(Hmisc)
str(final.data)
lrm1 = glm (final.data$Default...1~ .,data=final.data, family= binomial)

summary(lrm1)
pred.rd<-predict(lrm1, newdata=final.data, type="response")
table(final.data$Default...1, pred.rd>0.5)
(625+29)/nrow(na.omit(final.data))
29/(15+29)
library(ROCR)
final.data$predict <- predict(ROCRpred, final.data,type="prob")
as.numeric(performance(ROCRpred,"auc")@y.values)
perf= performance(ROCRpred, "tpr","fpr")
plot(perf)
final.data$predict.score = predict(, final.data, type = "prob")
#Deciling
library(scales)
library(data.table)
decile=function(x) {
  deciles=vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10]=quantile(x,i, na.rm=T)
  }
  return(
ifelse(x<deciles[1],1,
ifelse(x<deciles[2],2,
ifelse(x<deciles[3],3,
ifelse(x<deciles[4],4,
ifelse(x<deciles[5],5,
ifelse(x<deciles[6],6,
ifelse(x<deciles[7],7,
ifelse(x<deciles[8],8,
ifelse(x<deciles[9],9,10
))))))))))
}








