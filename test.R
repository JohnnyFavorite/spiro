setwd("~/Google Drive/MSE Mainz/Masterarbeit/GLI_2012___GAMLSS_in_action")

data <- read.csv("data.csv", header=TRUE)

data$centre <- factor(data$centre) 
data$sex <- factor(data$sex)

#split dataframe by sex
data_split <- split.data.frame(data, data$sex)

data_male <- data.frame(data_split[1])



lmMod <- lm(data_male$X1.fev ~ data_male$X1.height+data_male$X1.centre+data_male$X1.age ,data = data_male)
selectedModel <- step(lmMod)
summary(selectedModel)


#cross validation -> randomly permutate dataframe, split into train and test set 

#randomize order
data_male <- data_male[sample(1:nrow(data_male)), ]

#split 80:20 (train to test 4261:1462)

data_male_train <- data_male[1:4261,]
data_male_test <- data_male[4262:nrow(data_male),]

#train model with training set

lmMod <- lm(data_male_train$X1.fev ~ data_male_train$X1.height+data_male_train$X1.centre+data_male_train$X1.age ,data = data_male_train)
selectedModel <- step(lmMod)
summary(selectedModel)

#estimate fev in test set

data_male_test$X1.est <- -6.772+0.065*data_male_test$X1.height-0.016*data_male_test$X1.age

#calculate MSE

data_male_test$sqerror <- (data_male_test$X1.fev- data_male_test$X1.est)^2
mse <- mean(data_male_test$sqerror)


#Plots
a <- ggplot(data, aes(x=height, y=fev)) + geom_point() + geom_smooth(method="auto") 
a <- a + theme_classic() + labs(title= expression(Height~"vs"~FEV[1]),
                                subtitle = "Men and Women combined",
                                y= expression(FEV[1]~(l)), x = "Height (cm)")
a 

b <- ggplot(data, aes(x=age, y=fev)) + geom_point() + geom_smooth(method="auto")
b <- b + theme_classic() + labs(title= expression(Age~"vs"~FEV[1]),
                                subtitle = "Men and Women combined",
                                y= expression(FEV[1]~(l)), x = "Age (yrs)")
b

c <- ggplot(data, aes(centre, fev) + geom_bar(stat="identity"))
c <- c + theme_classic() + labs(title= expression(FEV[1]~According~to~Study~Centre),
                                subtitle = "Men and Women combined",
                                y= expression(FEV[1]~(l)), x = "Study Centre")
c

