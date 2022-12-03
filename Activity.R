### Import libraries ###
library(magrittr)
library(dplyr)
library(ggplot2)
library(GGally)


### Import data ###
dataset <- read.csv("module_5_auto.csv", header = TRUE)
dataset[1:5,]


### Name the columns ###
dataset <- dataset[-c(1,3,10,31)]
cols <- sapply(dataset, is.character)
dataset[cols] <- lapply(dataset[cols], factor)
column_names <- c("No.","NormalizeLosses","Make","Aspiration","Doors","BodyStyles","DriveWheels","WheeBase","Length",
                  "Width","Height","Weight","EngineType","Cylinders","EngineSize","FuelSystem","Bore","Stroke",
                  "CompressionRatio","HP","PeakRPM","CityMPG","HighwayMPG","Price","Lper100kmcity","BinnedHP","Diesel")
names(dataset) = column_names
str(dataset) 


### Data cleaning ###
nrow(dataset)
dataset <- na.omit(dataset)
nrow(dataset)
if(nrow(dataset) == nrow(unique(dataset[c(1:27)]))) {print("There is no duplicated car.")} else {dataset <- dataset[!duplicated(dataset[-c(1:27)]),]}+{print("There exist duplicated cars")}


### Data visualization ###
# Descriptive statistics for all attributes
summary(dataset)
sd(dataset$NormalizeLosses)
sd(dataset$WheelBase)
sd(dataset$Length)
sd(dataset$Width)
sd(dataset$Height)
sd(dataset$Weight)
sd(dataset$EngineSize)
sd(dataset$Bore)
sd(dataset$Stroke)
sd(dataset$CompressionRatio)
sd(dataset$HP)
sd(dataset$PeakRPM)
sd(dataset$CityMPG)
sd(dataset$HighwayMPG)
sd(dataset$Price)
sd(dataset$Lper100kmcity)

# Remove dependent column
chisq.test(dataset$CityMPG,dataset$Lper100kmcity)
dataset <- dataset[-c(25)]
summary(dataset)

# Create new column
dataset <- cbind(dataset, CombinedMPG = (0.45*dataset$HighwayMPG+0.55*dataset$CityMPG)) 

# Graph: Fuel Consumption Efficiency By Make
make_data <- dataset %>%
  group_by(Make) %>%
  summarise(lower = min(CombinedMPG), upper = max(CombinedMPG), p = median(CombinedMPG))

ggplot(data=make_data, mapping = aes(x = Make, y = p))+
  ggtitle("Fuel consumption rate by Brands")+
  xlab("Make")+
  ylab("Miles per gallon")+
  theme(legend.position = "none")+
  geom_pointrange(size = 0.2, mapping = aes(ymin = lower, ymax = upper))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Graph: Preparation for One-way ANOVA
dataset <- cbind(dataset, LogCombinedMPG = log(dataset$CombinedMPG))
plot(density(dataset$CombinedMPG), main = "Distribution of Fuel Consumption Efficiency", xlab = "")
plot(density(dataset$LogCombinedMPG), main = "Distribution of Fuel Consumption Efficiency", xlab = "")


### One-way ANOVA ###
one_way_ANOVA <- aov(LogCombinedMPG ~ EngineType, data = dataset)
summary(one_way_ANOVA)
TukeyHSD(one_way_ANOVA) #Compare pairwise between engine type
plot(TukeyHSD(one_way_ANOVA,conf.level=0.95),las=1,cex.axis=0.6)

shapiro.test(x = residuals(object = one_way_ANOVA))
bartlett.test(LogCombinedMPG ~ EngineType, data = dataset)


### Prediction model ###
# Data preparation
predict_dataset <- dataset[c(12,13,15,20,27)]
str(predict_dataset)

# Plot correlation graph
ggpairs(data = predict_dataset, columns = 1:5, title = "Correlation of data")

# Split train and test data
seed <- sample(c(rep(0,0.7*nrow(predict_dataset)), rep(1,0.3*nrow(predict_dataset))))
seed #Show the value of the "seed"
data.train <- predict_dataset[seed==0, ]
data.test <- predict_dataset[seed==1, ]

# Prediction model 1: Multiple Linear Regression
model1 <- lm(CombinedMPG ~ Weight + HP + EngineSize, data = data.train)
summary(model1) 

# Prediction model 2: General Linear Regression
model2 <- glm(CombinedMPG ~ Weight + HP + EngineSize + EngineType, data=data.train)
summary(model2) 

# One-way ANOVA to choose the appropriate model
anova(model1,model2,test="Chisq")

data.test$Predicted <- predict(model1, data.test)
plot(abs(data.test$Predicted - data.test$CombinedMPG), pch = 18, xlab = "i^(th) testcase",
     ylab = "Error",main="Absolute error in predictions of model")
ggplot(data.test, aes(x = CombinedMPG,y=Predicted))+
  geom_point()+
  stat_smooth(method = "lm", col = "blue")

