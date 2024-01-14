#Packages and libraries

install.packages("DescTools") 

library(DescTools)

install.packages("ResourceSelection")
library(ResourceSelection)

install.packages("car")
library(car)

#define Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#Import model development data

Train <- read.csv("A3_Model_development_H622.csv")

names(Train)

summary(Train)
str(Train)

#Create dummy variables

Train.Quant <- data.frame(SeniorCitizen = Train$SeniorCitizen,
                          Tenure = Train$tenure,
                          MonthlyCharges = Train$MonthlyCharges,
                          TotalCharges = Train$TotalCharges)


Train.Quant$MaleGender <- ifelse(Train$gender == "Male", 1,0) 
Train.Quant$Partner <- ifelse(Train$Partner == "Yes", 1,0)
Train.Quant$Dependents <- ifelse(Train$Dependents == "Yes", 1,0)
Train.Quant$PhoneService <- ifelse(Train$PhoneService == "Yes", 1,0)
Train.Quant$LongContract <- ifelse(Train$Contract == "Long term", 1,0)
Train.Quant$PaperlessBilling <- ifelse(Train$PaperlessBilling == "Yes", 1,0)

Train.Quant <- as.data.frame(scale(Train.Quant))

Train.Quant <- as.data.frame(lapply(Train.Quant,min_max_norm))

Train.Quant$Churn <- ifelse(Train$Churn == "Yes", 1,0)




#Create preliminary model

names(Train.Quant)

summary(Train.Quant)

boxplot(Train.Quant)

model1 <- glm(Churn ~SeniorCitizen +
                Tenure +
                MonthlyCharges +
                TotalCharges +
                MaleGender +
                Partner +
                Dependents +
                PhoneService + 
                LongContract +
                PaperlessBilling,
              data=Train.Quant, family=binomial)

vif(model1)
PseudoR2(model1, which = "Nagelkerke") 

#VIF over 10 for total charges and tenure. Remove total charges

model2 <- glm(Churn ~SeniorCitizen +
                Tenure +
                MonthlyCharges +
                MaleGender +
                Partner +
                Dependents +
                PhoneService + 
                LongContract +
                PaperlessBilling,
              data=Train.Quant, family=binomial)

vif(model2)
summary(model2)

PseudoR2(model1, which = "Nagelkerke")
PseudoR2(model2, which = "Nagelkerke") 


model21 <- glm(Churn ~SeniorCitizen +
                TotalCharges +
                MonthlyCharges +
                MaleGender +
                Partner +
                Dependents +
                PhoneService + 
                LongContract +
                PaperlessBilling,
              data=Train.Quant, family=binomial)

vif(model21)
summary(model21)

PseudoR2(model21, which = "Nagelkerke")


model31 <- glm(Churn ~SeniorCitizen +
                 TotalCharges +
                 MonthlyCharges +
                 PhoneService + 
                 LongContract +
                 PaperlessBilling,
               data=Train.Quant, family=binomial)

vif(model31)
summary(model31)

PseudoR2(model31, which = "Nagelkerke")


#Remove gender, partner, and dependents

model3 <- glm(Churn ~SeniorCitizen +
                Tenure +
                MonthlyCharges +
                PhoneService + 
                LongContract +
                PaperlessBilling,
              data=Train.Quant, family=binomial)

vif(model3)
summary(model3)

anova(model3, test="Chisq")

h3<-hoslem.test(Train.Quant$Churn, fitted(model3), g=10)
h3

PseudoR2(model3, which = "Nagelkerke")
exp(coef(model3))


install.packages("reghelper")
library("reghelper")          


model7.std <- beta(model7) 
model7.std


#Take out SeniorCitizen
model4 <- glm(Churn ~Tenure +
                MonthlyCharges +
                PhoneService + 
                LongContract +
                PaperlessBilling,
              data=Train.Quant, family=binomial)

#Take out PaperlessBillining
model5 <- glm(Churn ~SeniorCitizen +
                          Tenure +
                          MonthlyCharges +
                          PhoneService + 
                          LongContract,
                        data=Train.Quant, family=binomial)


#Take out senior/paperless
model6 <- glm(Churn ~Tenure +
                MonthlyCharges +
                PhoneService + 
                LongContract,
              data=Train.Quant, family=binomial)


PseudoR2(model4, which = "Nagelkerke")

PseudoR2(model5, which = "Nagelkerke")

PseudoR2(model6, which = "Nagelkerke")

summary(model4)  
summary(model5)
summary(model6)
summary(model3)

#put back in dependents

model7 <- glm(Churn ~SeniorCitizen +
                         Tenure +
                         MonthlyCharges +
                         Dependents +
                         PhoneService + 
                         LongContract +
                         PaperlessBilling,
                       data=Train.Quant, family=binomial)

summary(model7)
PseudoR2(model7, which = "Nagelkerke")

model8 <- glm(Churn ~SeniorCitizen +
                TotalCharges +
                MonthlyCharges +
                PhoneService + 
                LongContract +
                PaperlessBilling,
              data=Train.Quant, family=binomial)
summary(model8)
PseudoR2(model8, which = "Nagelkerke")


Train.result = fitted(model3, type = "response")
Train.pred = rep("No",length(Train.result))
Train.pred[Train.result > 0.5] = "Yes"
table(Train.pred,Train$Churn)


#Load prediction data

Yosoku <- read.csv("A3_Prediction_H622.csv")

Yosoku.Quant <- data.frame(SeniorCitizen = Yosoku$SeniorCitizen,
                          Tenure = Yosoku$tenure,
                          MonthlyCharges = Yosoku$MonthlyCharges,
                          TotalCharges = Yosoku$TotalCharges)

Yosoku.Quant$MaleGender <- ifelse(Yosoku$gender == "Male", 1,0) 
Yosoku.Quant$Partner <- ifelse(Yosoku$Partner == "Yes", 1,0)
Yosoku.Quant$Dependents <- ifelse(Yosoku$Dependents == "Yes", 1,0)
Yosoku.Quant$PhoneService <- ifelse(Yosoku$PhoneService == "Yes", 1,0)
Yosoku.Quant$LongContract <- ifelse(Yosoku$Contract == "Long term", 1,0)
Yosoku.Quant$PaperlessBilling <- ifelse(Yosoku$PaperlessBilling == "Yes", 1,0)

Yosoku.Quant <- as.data.frame(scale(Yosoku.Quant))

Yosoku.Quant <- as.data.frame(lapply(Yosoku.Quant,min_max_norm))

#apply regression

ChurnPred <- predict.glm(model3, newdata = Yosoku.Quant, type = "response")

Yosoku.Final <- Yosoku.Quant

Yosoku.Final$Churn <- ChurnPred

write.csv(Yosoku.Final, "PredictionOutput.csv")

summary(model3)

Train.Quant$Pred <- Train.result
write.csv(Train.Quant, "TrainingResults.csv")
