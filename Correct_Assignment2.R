#to start with installing all the packages and library required to perform EDA on dataset and applying ANN on dataset
install.packages("janitor")
install.packages("skimr")
install.packages("mosaic")
install.packages("FactoMineR")
install.packages("ggfortify")
install.packages("corrplot")
install.packages("rsample")
install.packages("patchwork")
library(tidyverse)
library(broom)
library(janitor)
library(skimr)
library(dplyr)
library(readr)
library(FactoMineR)
library(GGally)
library(leaflet)
library(car)
library(ggfortify)
library(corrplot)
library(rsample)
library(caret)
library(patchwork)

#dataset 
setwd('C:/Users/Nagraj M Srivatsa/Documents/semester 2/Data Mining/Assignment2')
HeartFailure_dataset = read.csv("heart_failure_clinical_records_dataset.csv")
head(HeartFailure_dataset)
summary(HeartFailure_dataset)
skim(HeartFailure_dataset)

#--------------------------------------------------------------------------------------
#Exploratory Data Analysis

# Lets analyse the binary values like anaemia,diabetes,
#high_blood_pressure,sex,smoking and death event using pie charts and histograms

#Death_Event
 pie_char<-matrix(table(HeartFailure_dataset$DEATH_EVENT),ncol=1,byrow=T)
 pie(pie_char,labels=sprintf("%.2f%%", prop.table(pie_char)*100),
     main="Death Event Pie Chart",
     col=c("green","red"))
 legend("left", legend = c("Survived", "Died"), fill = c("green", "red"))
 
#anemia 
pie_char<-matrix(table(HeartFailure_dataset$anaemia),ncol=1,byrow=T)
pie(pie_char,labels=sprintf("%.2f%%", prop.table(pie_char)*100),
     main="Anaemia Pie Chart",
     col=c("blue","red"))
legend("left", legend = c("Not-anemic", "Anemic"), fill = c("blue", "red"))
 
#diabetes
 pie_char<-matrix(table(HeartFailure_dataset$diabetes),ncol=1,byrow=T)
 pie(pie_char,labels=sprintf("%.2f%%", prop.table(pie_char)*100),
     main="diabetes Pie Chart",
     col=c("light blue","red"))
 legend("left", legend = c("No","Yes"), fill = c("light blue", "red"))
 
 #high_blood_pressure
 pie_char<-matrix(table(HeartFailure_dataset$high_blood_pressure),ncol=1,byrow=T)
 pie(pie_char,labels=sprintf("%.2f%%", prop.table(pie_char)*100),
     main="high_blood_pressure Pie Chart",
     col=c("darkgoldenrod1","red"))
 legend("left", legend = c("No","Yes"), fill = c("darkgoldenrod1", "red")) 
 
 #sex
 pie_char<-matrix(table(HeartFailure_dataset$sex),ncol=1,byrow=T)
 pie(pie_char,labels=sprintf("%.2f%%", prop.table(pie_char)*100),
     main="sex Pie Chart",
     col=c("cornflowerblue","red"))
 legend("left", legend = c("Female","Male"), fill = c("cornflowerblue", "red")) 
 
 #smoking
 pie_char<-matrix(table(HeartFailure_dataset$smoking),ncol=1,byrow=T)
 pie(pie_char,labels=sprintf("%.2f%%", prop.table(pie_char)*100),
     main="smoking Pie Chart",
     col=c("light grey","red"))
 legend("left", legend = c("Non-smoker","Smoker"), fill = c("light grey", "red")) 
 
 #------------------------------------------------------------------------------------- 
 #Analyzing Distribution of data of age and other features
 
 #Age and sex
 ggplot(HeartFailure_dataset, aes(x = age, fill = factor(sex))) +
   geom_histogram(bins = 30, position = "dodge") +
   ggtitle("Distribution of male and female") +
   xlab("Age") +
   ylab("Frequency Distribution") +
   scale_fill_manual(values = c("blue","green"), name = "Sex", labels = c("Female", "Male")) +
   theme(legend.position="left")
 
 #Age and diabetes
 ggplot(HeartFailure_dataset, aes(x = age, fill = factor(diabetes))) +
   geom_histogram(bins = 30, position = "dodge") +
   ggtitle("Distribution of diabetes and Non diabetes") +
   xlab("Age") +
   ylab("Frequency Distribution") +
   scale_fill_manual(values = c("blue","green"), name = "diabetes", labels = c("No", "Yes")) +
   theme(legend.position="left")
 
 #Age and high blood pressure
 ggplot(HeartFailure_dataset, aes(x = age, fill = factor(high_blood_pressure))) +
   geom_histogram(bins = 30, position = "dodge") +
   ggtitle("Distribution of high blood pressure and Non hyper tension") +
   xlab("Age") +
   ylab("Frequency Distribution") +
   scale_fill_manual(values = c("blue","green"), name = "BP", labels = c("No", "Yes")) +
   theme(legend.position="left")
 
 #Age and smoking
 ggplot(HeartFailure_dataset, aes(x = age, fill = factor(smoking))) +
   geom_histogram(bins = 30, position = "dodge") +
   ggtitle("Distribution of smoker and non smoker") +
   xlab("Age") +
   ylab("Frequency Distribution") +
   scale_fill_manual(values = c("blue","green"), name = "Smoking", labels = c("No", "Yes")) +
   theme(legend.position="left")

 #-----------------------------------------------------------------------------------------------------------------
  # Bar charts to compare with death event
 
 plot_AnemiaDE <- ggplot(data = Heartdataset, mapping = aes(x = anaemia, y = ..count.., fill = DEATH_EVENT)) + 
   geom_bar(stat = "count", position='dodge',color=("black"))+
   labs(title = "Barplot of anaemia")+
   scale_fill_manual(values = c("blue","darkgrey")) +
   theme_bw()
 plot_diabetesDE <- ggplot(data = Heartdataset, mapping = aes(x = diabetes, y = ..count.., fill = DEATH_EVENT)) + 
   geom_bar(stat = "count", position='dodge',color=("black"))+
   labs(title = "Barplot of diabetes")+
   scale_fill_manual(values = c("blue","darkgrey"))
   theme_bw()
 
 plot_BPDE <- ggplot(data = Heartdataset, mapping = aes(x = high_blood_pressure, y = ..count.., fill = DEATH_EVENT)) + 
   geom_bar(stat = "count", position='dodge',color=("black"))+
   labs(title = "Barplot of high_blood_pressure")+
   scale_fill_manual(values = c("blue","darkgrey"))+
   theme_bw()
 
 plot_SmkerDE <- ggplot(data = Heartdataset, mapping = aes(x = smoking, y = ..count.., fill = DEATH_EVENT)) + 
   geom_bar(stat = "count", position='dodge',color=("black"))+
   labs(title = "Barplot of smoking")+
   scale_fill_manual(values = c("blue","darkgrey"))+
   theme_bw()

 plot_AnemiaDE+plot_diabetesDE+plot_BPDE+plot_SmkerDE
 
 #-------------------------------------------------------------------------------------------------
#Now lets understand the  non binary columns like age creatine_phosphate and extra.
 
 boxplot(HeartFailure_dataset$creatinine_phosphokinase, col=(c("gold","red")),main="Boxplot of creatinine_phosphokinase",ylim=c(0,3000))
 boxplot(HeartFailure_dataset$ejection_fraction, col=(c("blue","red")),main="Boxplot of ejection_fraction")
 boxplot(HeartFailure_dataset$platelets, col=(c("green","red")), main="Boxplot of platelets", log="y")
 boxplot(HeartFailure_dataset$serum_creatinine, main="Boxplot of serum_creatinine",ylim=c(0,5))
 boxplot(HeartFailure_dataset$serum_sodium, col=(c("cornflowerblue","red")), main="Boxplot of serum_sodium")
 boxplot(HeartFailure_dataset$time, col=(c("darkgreen","red")), main="Boxplot of time")
 boxplot(HeartFailure_dataset$age,  main="Boxplot of age")
 
#---------------------------------------------------------------------------------------------------------- 
 
 summary(HeartFailure_dataset)
 Heartdataset<- HeartFailure_dataset %>%
   summarise(DEATH_EVENT = as.factor(DEATH_EVENT),
             anaemia = as.factor(anaemia),
             diabetes = as.factor(diabetes),
             high_blood_pressure = as.factor(high_blood_pressure),
             sex = as.factor(sex),
             smoking = as.factor(smoking),
             age,creatinine_phosphokinase, ejection_fraction, platelets, serum_creatinine, serum_sodium, time)
 
 favstats(~age,data = Heartdataset)
favstats(age~DEATH_EVENT,data = Heartdataset)
Age_death <- ggplot(Heartdataset,aes(x=DEATH_EVENT, y=age,fill=DEATH_EVENT))+
  geom_boxplot()+scale_fill_manual(values=c("darkgoldenrod1","cornflowerblue"))+
  theme_bw()+
  labs(title ="How age affects death events?")
Age_death
attach(Heartdataset)
par(mfrow=c(3,3))
boxplot(creatinine_phosphokinase$DEATH_EVENT, col=(c("gold","red")),main="Boxplot of creatinine_phosphokinase",ylim=c(0,3000))
boxplot(ejection_fraction~DEATH_EVENT, col=(c("blue","red")),main="Boxplot of ejection_fraction")
boxplot(platelets~DEATH_EVENT, col=(c("green","red")), main="Boxplot of platelets", log="y")
boxplot(serum_creatinine~DEATH_EVENT, main="Boxplot of serum_creatinine",ylim=c(0,5))
boxplot(serum_sodium~DEATH_EVENT, col=(c("cornflowerblue","red")), main="Boxplot of serum_sodium")
boxplot(time~DEATH_EVENT, col=(c("darkgreen","red")), main="Boxplot of time")
boxplot(age~DEATH_EVENT,  main="Boxplot of age")

#---------------------------------------------------------------------------------------------

#correlation between the features

datasetCorCoef = HeartFailure_dataset
datasetCorCoef$heart_attack = as.numeric(datasetCorCoef$heart_attack)
cor(datasetCorCoef)

# pair plots
ggpairs(datasetCorCoef, columns = 1:6,"black", 
        ggplot2::aes(colour=as.factor('HeartFailure_dataset$heart_attack'), alpha=9))

#---------------------------------------------------------------------------------------------
# Normalize the data set before feeding (between 0 and 1)


HeartFailure_dataset$age<-(HeartFailure_dataset$age-min(HeartFailure_dataset$age)) / 
  (max(HeartFailure_dataset$age)- min(HeartFailure_dataset$age))

HeartFailure_dataset$serum_creatinine<-(HeartFailure_dataset$serum_creatinine 
                                        - min(HeartFailure_dataset$serum_creatinine)) / 
  (max(HeartFailure_dataset$serum_creatinine) - min(HeartFailure_dataset$serum_creatinine))

HeartFailure_dataset$platelets<-(HeartFailure_dataset$platelets - min(HeartFailure_dataset$platelets)) / 
  (max(HeartFailure_dataset$platelets) - min(HeartFailure_dataset$platelets))

HeartFailure_dataset$creatinine_phosphokinase<-(HeartFailure_dataset$creatinine_phosphokinase 
                                                - min(HeartFailure_dataset$creatinine_phosphokinase))
/ (max(HeartFailure_dataset$creatinine_phosphokinase) - min(HeartFailure_dataset$creatinine_phosphokinase))

HeartFailure_dataset$ejection_fraction<-(HeartFailure_dataset$ejection_fraction 
                                         - min(HeartFailure_dataset$ejection_fraction)) / 
  (max(HeartFailure_dataset$ejection_fraction) - min(HeartFailure_dataset$ejection_fraction)
   - min(HeartFailure_dataset$ejection_fraction))

HeartFailure_dataset$serum_sodium<-(HeartFailure_dataset$serum_sodium - min(HeartFailure_dataset$serum_sodium)) / 
  (max(HeartFailure_dataset$serum_sodium) - min(HeartFailure_dataset$serum_sodium))

HeartFailure_dataset$time<-(HeartFailure_dataset$time - min(HeartFailure_dataset$time)) / 
  (max(HeartFailure_dataset$time) - min(HeartFailure_dataset$time))

View(HeartFailure_dataset)
head(HeartFailure_dataset)
#--------------------------------------------------------------------------------------------
#Factor Analysis:

install.packages("nFactors")
require(nFactors)

# Importing the dataset
head(HeartFailure_dataset)
summary(HeartFailure_dataset)


ev<-eigen(cor(HeartFailure_dataset))
nS<-nScree(x=ev$values)
plotnScree(nS, legend = FALSE)

print(ev$values)

fa=factanal(HeartFailure_dataset, factors = 5, rotation = "varimax")
print(fa)

#----------------------------------------------------------------------------------------------
#Modelling

#Model_1 without activation function hidden-5,3 (78%)

HeartFailure_dataset[1:12] <- scale(HeartFailure_dataset[1:12])

summary(HeartFailure_dataset)

install.packages("neuralnet")
library(neuralnet)
set.seed(12345)

nuer <- sample(2, nrow(HeartFailure_dataset), replace = TRUE, prob = c(0.7, 0.7))
train.data <- HeartFailure_dataset[nuer == 1, ]
test.data <- HeartFailure_dataset[nuer == 2, ]

nn1 <- neuralnet(formula = DEATH_EVENT ~ age + anaemia + creatinine_phosphokinase + diabetes + ejection_fraction + high_blood_pressure + 
                  platelets + serum_creatinine + serum_sodium + sex + smoking + time, 
                data = train.data, hidden = c(5, 3), 
                err.fct = "ce", linear.output = FALSE)


summary(nn1)

nn1$response[1:20]

nn1$net.result[[1]][1:20]

nn1$result.matrix

plot(nn1)

#Training
mypredict <- compute(nn1, nn1$covariate)$net.result
mypredict <- apply(mypredict, c(1), round)

mypredict[1:20]

model1_training=table(mypredict, train.data$DEATH_EVENT, dnn =c("Predicted", "Actual"))
confusionMatrix(model1_training)

#testing
testPred <- compute(nn1, test.data[, 0:12])$net.result
testPred <- apply(testPred, c(1), round)

model1_test<-table(testPred, test.data$DEATH_EVENT, dnn = c("Predicted", "Actual"))

confusionMatrix(model1_test)

########################################################

#Modelling
#Model2 with sigmoid activation function(81%)

HeartFailure_dataset[1:12] <- scale(HeartFailure_dataset[1:12])

summary(HeartFailure_dataset)

install.packages("neuralnet")
library(neuralnet)
set.seed(12345)

nuer1 <- sample(2, nrow(HeartFailure_dataset), replace = TRUE, prob = c(0.7, 0.3))
train.data <- HeartFailure_dataset[nuer1 == 1, ]
test.data <- HeartFailure_dataset[nuer1 == 2, ]

nn2 <- neuralnet(formula = DEATH_EVENT ~ age + anaemia + creatinine_phosphokinase + diabetes + ejection_fraction + high_blood_pressure + 
                   platelets + serum_creatinine + serum_sodium + sex + smoking + time, 
                 data = train.data, hidden = c(5, 3), 
                 act.fct = "logistic", linear.output = FALSE)


summary(nn2)

nn2$response[1:20]

nn2$net.result[[1]][1:20]

nn2$result.matrix

plot(nn2)

#Training
mypredict <- compute(nn2, nn2$covariate)$net.result
mypredict <- apply(mypredict, c(1), round)

mypredict[1:20]

model2_training=table(mypredict, train.data$DEATH_EVENT, dnn =c("Predicted", "Actual"))
confusionMatrix(model2_training)

#test
testPred <- compute(nn2, test.data[, 0:12])$net.result
testPred <- apply(testPred, c(1), round)

model2_test<-table(testPred, test.data$DEATH_EVENT, dnn = c("Predicted", "Actual"))

confusionMatrix(model2_test)

##################################################################################

#Modelling

#Model_3 8,4 hidden layers(80%)

HeartFailure_dataset[1:12] <- scale(HeartFailure_dataset[1:12])

summary(HeartFailure_dataset)

install.packages("neuralnet")
library(neuralnet)
set.seed(12345)

nuer3 <- sample(2, nrow(HeartFailure_dataset), replace = TRUE, prob = c(0.7, 0.3))
train.data <- HeartFailure_dataset[nuer3 == 1, ]
test.data <- HeartFailure_dataset[nuer3 == 2, ]

nn3 <- neuralnet(formula = DEATH_EVENT ~ age + anaemia + creatinine_phosphokinase + diabetes + ejection_fraction + high_blood_pressure + 
                  platelets + serum_creatinine + serum_sodium + sex + smoking + time, 
                data = train.data, hidden = c(8, 4), 
                act.fct = "logistic", linear.output = FALSE)

summary(nn3)

nn3$response[1:20]

nn3$net.result[[1]][1:20]

nn3$result.matrix

plot(nn3)

#Training
mypredict3 <- compute(nn3, nn3$covariate)$net.result
mypredict3 <- apply(mypredict3, c(1), round)

mypredict3[1:20]

model3_training=table(mypredict3, train.data$DEATH_EVENT, dnn =c("Predicted", "Actual"))
confusionMatrix(model3_training)

#test
testPred3 <- compute(nn3, test.data[, 0:12])$net.result
testPred3 <- apply(testPred3, c(1), round)

model3_test<-table(testPred3, test.data$DEATH_EVENT, dnn = c("Predicted", "Actual"))

confusionMatrix(model3_test)

#-----------------------------------------------------------------------------------------------




