library(tidyverse)
library(tinytable)
library(readxl)
library(dplyr)
library(gridExtra)
library(RColorBrewer)
library(gtsummary)
library(GGally)
library(gridExtra)
library(car)
library(ggplot2)
library(tidymodels)
library(fastDummies)
install.packages('devtools')
devtools::install_github('mortenarendt/data4consumerscience')

library(data4consumerscience)
data("plantbaseddiet")
data=na.omit(plantbaseddiet)
res <- kmeans(plantbaseddiet[,c('a_meat', 'a_dairy','a_eggs')], 4)
data("pork")
data=pork
write.csv(data,file = "~/data.csv")
data$clusters <- factor(res$cluster,labels = c('High Meat','Low all','High Dairy','High All'))
data
write.csv()
X=as.matrix(cbind(dummy_cols(data$education)[,3:5],
                  dummy_cols(data$gender)[,2],
                  data$age,
                  dummy_cols(data$income)[,3:6],
                  dummy_cols(data$health)[,3:6],
                  dummy_cols(data$price)[,3:6],
                  dummy_cols(data$clusters)[,2:4]
                  ))
ncol(X)
beta=as.matrix(c(
                 3,10,15,
                 10,
                 -2,
                 2,3,5,15,
                 2,2.1,2.1,2.3,
                 2,2,-20,-25,
                 -15,15,-20
                 ))
p=1/(1+exp(-(55+X%*%beta+rnorm(nrow(X),5,5))))
hist(p)
mean(p>.5)
x=c()
for(i in 1:length(p)) x[i]<-rbinom(1,1,p[i])
data$choice=x
library(tidymodels)
set.seed("4485") # pour avoir les mêmes échantillons
choice_split <- group_initial_split(data = data,group=ID,strata=choice)
train_data <- training(choice_split)
proportions(table(train_data$choice))
test_data <- testing(choice_split)
proportions(table(test_data$choice))

data=data[,colnames(data)[c(1:3,7:12,42:43)]]
formula1=as.formula(paste("choice~age+",paste(paste("factor(",colnames(data)[c(2,4,5,9,10)],")",sep=""),collapse="+"),sep=""))

mod=lm(formula1,data=train_data)
summary(mod)
Anova(mod,type="III")
prob=predict(mod,test_data)
Yh=as.numeric(prob>=.5)
hist(Yh)
Y=test_data$choice
sum(Yh==0 & Y==0)/sum(Y==0)
sum(Yh==1 & Y==1)/sum(Y==1)

write.csv(data,"data/data_RL.csv")

library(nnet)

plantbaseddiet
res <- kmeans(plantbaseddiet[,c('a_meat', 'a_dairy','a_eggs')], 4)

plantbaseddiet$clusters <- factor(res$cluster,labels = c('High Meat','Low all','High Dairy','High All'))

formula2=as.formula(paste("clusters~age+",paste(paste("factor(",colnames(plantbaseddiet)[c(2,5:13)],")",sep=""),collapse="+"),sep=""))
mod=multinom(formula2,data=plantbaseddiet)
Anova(mod,type="III")

Ypred=predict(mod)
Yobs=plantbaseddiet$clusters
tab=table(Ypred,Yobs)
sum(diag(tab))/sum(tab)
