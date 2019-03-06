#install.packages
install.packages("lattice")
install.packages("mice")
install.packages("readr")
install.packages("VIM")
install.packages("dplyr")
install.packages("MASS")
install.packages("rmarkdown")
install.packages("ROCR")
install.packages("plyr")
install.packages("corrplot")
install.packages("RColorBrewer")

#read library
library(readr)

#import dataset
german_credit_data <- read.csv(file="C:/Users/U085461/Documents/R/Project/german_credit_data.csv");

# Rename a column in R
names(german_credit_data)[1]<-"ID" 

german_credit_data
# list the structure of mydata
str(german_credit_data)

# dimensions of an object
dim(german_credit_data)
#1000   11#

# class of an object (numeric, matrix, data frame, etc)
class(german_credit_data)
#"data.frame"#

#Testing for Missing Values
is.na(german_credit_data)

#Get summary
summary(german_credit_data)

#read library
library(mice)

#missing value 
md.pattern(german_credit_data)

#read library
library(VIM)
#ok#

#missing value
german_credit_data_plot<- aggr(german_credit_data,
                               col=c('navyblue','red'),
                               numbers=TRUE, 
                               sortVars=TRUE,
                               labels=names(german_credit_data), 
                               cex.axis=.7,
                               gap=5,
                               ylab=c("Missing data","Pattern"))

# range
range(german_credit_data$Age)
# [1] 19 75

# range
range(german_credit_data$Credit.amount)
# [1] 250 18424

#histogram
german_credit_data_age<-hist(german_credit_data$Age,
                             main="Histogram for Age", 
                             xlab="Age", 
                             border="blue", 
                             col="green",
                             prob = TRUE)
lines(density(german_credit_data$Age)) 

german_credit_data_credit.amount<-hist(german_credit_data$Credit.amount,
                                       main="Histogram for Credit.amount", 
                                       xlab="Credit.amount", 
                                       border="blue", 
                                       col="green",
                                       prob = TRUE)
lines(density(german_credit_data$Credit.amount)) 

german_credit_data_duration<-hist(german_credit_data$Duration,
                                  main="Histogram for Duration", 
                                  xlab="Duration", 
                                  border="blue", 
                                  col="green",
                                  prob = TRUE)
lines(density(german_credit_data$Duration))

# BAR CHART
barchart(table(german_credit_data$Sex), 
         main="Sex Distribution", 
         xlab="Number of Sex")

barchart(table(german_credit_data$Housing), 
         main="Housing Distribution", 
         xlab="Number of Housing")

barchart(table(german_credit_data$Saving.accounts), 
         main="Saving.accounts Distribution", 
         xlab="Number of Saving.accounts")

barchart(table(german_credit_data$Purpose), 
         main="Purpose Distribution", 
         xlab="Number of Purpose")

barchart(table(german_credit_data$Risk), 
         main="Risk Distribution", 
         xlab="Number of Risk")

barchart(table(german_credit_data$Checking.account), 
         main="Checking.account Distribution", 
         xlab="Number of Checking.account")


library(dplyr)

#filter missing value
df <- filter(german_credit_data,is.na(Checking.account) )

#Join Table
german_credit_data_v1 <- anti_join(german_credit_data,df,by="ID")

#Structure
str(german_credit_data_v1)

#missing value
german_credit_data_plot_v1<- aggr(german_credit_data_v1,
                                  col=c('navyblue','red'),
                                  numbers=TRUE, 
                                  sortVars=TRUE,
                                  labels=names(german_credit_data_v1), 
                                  cex.axis=.7,
                                  gap=5,
                                  ylab=c("Missing data","Pattern"))


#filter missing value
df_1 <- filter(german_credit_data,is.na(Saving.accounts) )

#Join table
german_credit_data_v2 <- anti_join(german_credit_data_v1,df_1,by="ID")

#missing value
german_credit_data_plot_v2<- aggr(german_credit_data_v2,
                                  col=c('navyblue','red'),
                                  numbers=TRUE, 
                                  sortVars=TRUE,
                                  labels=names(german_credit_data_v1), 
                                  cex.axis=.7,
                                  gap=5,
                                  ylab=c("Missing data","Pattern"))

#Structure
str(german_credit_data_v2)

x<-german_credit_data_v2$Age
y<-german_credit_data_v2$job

#Convert values for target
library(plyr)
german_credit_data$Risk<- revalue(german_credit_data$Risk, c("bad"="1"))
german_credit_data$Risk<- revalue(german_credit_data$Risk, c("good"="0"))

# Correlations with significance levels

library(corrplot)
library(RColorBrewer)

x <- cor(german_credit_data_v2[,unlist(lapply(german_credit_data_v2, is.numeric))])

corrplot(x, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

corrplot(x,type="lower")

#divided by train and test
require(caTools) 
set.seed(123) 
sample = sample.split(german_credit_data_v2,SplitRatio = 0.70)
train =subset(german_credit_data_v2,sample ==TRUE)
test=subset(german_credit_data_v2, sample==FALSE)

#Logistic Regression
full<-glm(Risk~., family=binomial, data=german_credit_data_v2)
summary(full)

#vif values
car::vif(full)

#Stepwise Model Path
library(MASS)
step<- stepAIC(full,trace=FALSE)
step$anova

#direction="forward"
forward <- stepAIC(full,direction = "forward", trace=FALSE)
forward$anova

#direction="backward"
backward <- stepAIC(full,direction = "backward", trace=FALSE)
backward$anova

#Modelling
full1<-glm(Risk~Saving.accounts+Duration+Checking.account+Purpose, family=binomial, data=train)
summary(full1)

full2<-glm(Risk~Saving.accounts+Duration+Checking.account+Purpose+Sex, family=binomial, data=train)
summary(full2)

#Alternative Modelling 1
library(ROCR)

p <- predict(full1, newdata=test, type="response")
fitted.results <- predict(full1,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Risk)
print(paste('Accuracy',1-misClasificError))

#Alternative Modelling 2
library(ROCR)

p <- predict(full2, newdata=test, type="response")
fitted.results <- predict(full2,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Risk)
print(paste('Accuracy',1-misClasificError))

#resource
#https://www.kaggle.com/uciml/german-credit