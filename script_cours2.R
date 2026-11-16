library(rsample)
data=read.csv("~/winequality-red.csv",sep=";")
library(gtsummary)
data$quality=factor(data$quality,
                    levels=c("mauvais","médiocre","moyen",
                             "bon","très bon","excellent"))

data$ID=factor(1:nrow(data))
data_split <- group_initial_split(data = data,
                                    group=ID,
                                    strata=quality)
train_data <- training(data_split)
test_data <- testing(data_split)
View(data)
proportions(table(data$quality))
proportions(table(train_data$quality))
proportions(table(test_data$quality))
