library("MASS")
library("car")

rm(list=ls())
###Checkpoint 1:  Data Understanding
#Load the Data
carmileage <- read.csv("carMPG.csv", stringsAsFactors=FALSE)

str(carmileage)

########Check point 2: Data Cleaning and Preparation
carmileage <- unique(carmileage)

# remove entries having "?" in Horsepower 
carmileage <- carmileage[carmileage$Horsepower!='?', ]

#Making Variables as numeric

carmileage$Cylinders <- as.numeric(carmileage$Cylinders)

carmileage$Horsepower <- as.numeric(carmileage$Horsepower)

carmileage$Weight <- as.numeric(carmileage$Weight)

#Correcting the Factor and CarCompany variable 

carmileage$Origin <- as.factor(carmileage$Origin)

carmileage$Car_Company <- gsub("([A-Za-z]+).*", "\\1", carmileage$Car_Name)

carmileage$Car_Company <- gsub("chevy","chevrolet",carmileage$Car_Company)

carmileage$Car_Company <- gsub("maxda","mazda",carmileage$Car_Company)

carmileage$Car_Company <- gsub("toyouta","toyota",carmileage$Car_Company)

carmileage$Car_Company <- gsub("vw","volkswagen",carmileage$Car_Company)

carmileage$Car_Company <- gsub("vokswagen","volkswagen",carmileage$Car_Company)

carmileage$Car_Company <- as.factor(carmileage$Car_Company)

### calculation  foe age of car and add it as a new variable 
PresentYear <- as.integer(format(Sys.Date(), "%Y"))
carmileage$Car_Age <- PresentYear - carmileage$Model_year
carmileage$Car_Age <- as.numeric(carmileage$Car_Age)

dummy1 <- as.data.frame(model.matrix(~Origin ,data = carmileage))
dummy1 <- dummy1[, -1 ]
carmileage <- cbind(carmileage,dummy1[, -3])
dummy2 <- as.data.frame(model.matrix(~Car_Company,data = carmileage))
dummy2 <- dummy2[, -1 ]
carmileage <- cbind(carmileage,dummy2[, -31]) 
carmileage <- carmileage[ -c(7:10) ]
str(carmileage)
##### Split the data 70 % training and 30 % test
set.seed(100)

indices = sample(1:nrow(carmileage),0.7*nrow(carmileage))

Train = carmileage[indices,]
Test = carmileage[-indices,]
##### Model Development
model_1 <- lm(MPG~.,data=Train)

summary(model_1)

step <- stepAIC(model_1,direction = "both")

step

model_2 <- lm(MPG ~ Cylinders + Displacement + Weight + Car_Age + 
                Origin3 + Car_Companyaudi + Car_Companydatsun + Car_Companyfiat + 
                Car_Companymercedes + Car_Companyoldsmobile + Car_Companyplymouth + 
                Car_Companypontiac + Car_Companyrenault + Car_Companytriumph + 
                Car_Companyvolkswagen, data = Train)

summary(model_2)

vif(model_2)

## Removing Dispacement
model_3 <- lm(MPG ~ Cylinders  + Weight + Car_Age + 
                Origin3 + Car_Companyaudi + Car_Companydatsun + Car_Companyfiat + 
                Car_Companymercedes + Car_Companyoldsmobile + Car_Companyplymouth + 
                Car_Companypontiac + Car_Companyrenault + Car_Companytriumph + 
                Car_Companyvolkswagen, data = Train)

summary(model_3)

vif(model_3)

# Removing Cylinder 

model_4 <- lm(MPG ~  Weight + Car_Age + 
                Origin3 + Car_Companyaudi + Car_Companydatsun + Car_Companyfiat + 
                Car_Companymercedes + Car_Companyoldsmobile + Car_Companyplymouth + 
                Car_Companypontiac + Car_Companyrenault + Car_Companytriumph + 
                Car_Companyvolkswagen, data = Train)

summary(model_4)

vif(model_4)

# Removing Mercedes
model_5 <- lm(MPG ~  Weight + Car_Age + 
                Origin3 + Car_Companyaudi + Car_Companydatsun + Car_Companyfiat + 
                 Car_Companyoldsmobile + Car_Companyplymouth + 
                Car_Companypontiac + Car_Companyrenault + Car_Companytriumph + 
                Car_Companyvolkswagen, data = Train)

summary(model_5)

vif(model_5)

# Removing Car_Companyaudi

model_6 <- lm(MPG ~  Weight + Car_Age + 
                Origin3 +  Car_Companydatsun + Car_Companyfiat + 
                Car_Companyoldsmobile + Car_Companyplymouth + 
                Car_Companypontiac + Car_Companyrenault + Car_Companytriumph + 
                Car_Companyvolkswagen, data = Train)

summary(model_6)

vif(model_6)

# Removing Car_Companyplymouth

model_7 <- lm(MPG ~  Weight + Car_Age + 
                Origin3 +  Car_Companydatsun + Car_Companyfiat + 
                Car_Companyoldsmobile  + 
                Car_Companypontiac + Car_Companyrenault + Car_Companytriumph + 
                Car_Companyvolkswagen, data = Train)

summary(model_7)

vif(model_7)

# Removing Car_Companyoldsmobile
model_8 <- lm(MPG ~  Weight + Car_Age + 
                Origin3 +  Car_Companydatsun + Car_Companyfiat + 
                Car_Companypontiac + Car_Companyrenault + Car_Companytriumph + 
                Car_Companyvolkswagen, data = Train)

summary(model_8)

# Removing Car_Companyrenault     
model_9 <- lm(MPG ~  Weight + Car_Age + 
                Origin3 +  Car_Companydatsun + Car_Companyfiat + 
                Car_Companypontiac +  Car_Companytriumph + 
                Car_Companyvolkswagen, data = Train)

summary(model_9)


vif(model_10)

# Removing Car_Companytriumph
model_10 <- lm(MPG ~  Weight + Car_Age + 
                Origin3 +  Car_Companydatsun + Car_Companyfiat + 
                Car_Companypontiac +   
                Car_Companyvolkswagen, data = Train)

summary(model_10)
vif(model_10)

# Removing Datsun

model_11 <- lm(MPG ~  Weight + Car_Age + 
                 Origin3 +   Car_Companyfiat + 
                 Car_Companypontiac +   
                 Car_Companyvolkswagen, data = Train)

summary(model_11)
vif(model_11)

model_12 <- lm(MPG ~  Weight + Car_Age + 
                 Origin3 +   Car_Companyfiat + 
                 Car_Companypontiac +   
                 Car_Companyvolkswagen, data = Train)

summary(model_11)
vif(model_11)

# Removing Fiat
model_12 <- lm(MPG ~  Weight + Car_Age + 
                 Origin3 +Car_Companypontiac +   
                 Car_Companyvolkswagen, data = Train)

summary(model_12)
vif(model_12)

# Removing Car_Companypontiac 

model_13 <- lm(MPG ~  Weight + Car_Age + 
                 Origin3 +Car_Companyvolkswagen, data = Train)

summary(model_13)
vif(model_13)

Milage_Predicted <- predict(model_13,Test[,-c(1)])
Test$Predicted_Milage <- Milage_Predicted
cor(Test$MPG,Test$Predicted_Milage)
cor(Test$MPG,Test$Predicted_Milage)^2
plot(model_13)


