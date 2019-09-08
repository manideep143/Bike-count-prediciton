#Set the filepath
setwd("C:/manideep/edwisor/Project - 2")
getwd()

#Load the csv data
bike_rent = read.csv("day.csv")

#------------------------------------------- Missing Value Analysis------------------------------------------------------
# Missing Value Analaysis

missing_val = data.frame(apply(bike_rent,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
row.names(missing_val) = NULL
names(missing_val)[1] = "percent"
missing_val$percent = (missing_val$percent/nrow(bike_rent)) * 100
missing_val = missing_val[order(-missing_val$percent),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]

# No missing values found

#---------------------------------------------Outlier Analysis-------------------------------------------------------
#Detect outliers

library(ggplot2)

out_names = colnames(bike_rent[,11:13])
for (i in 1:length(out_names))
  {
     assign(paste0("gn",i), ggplot(aes_string(y = (out_names[i]), x = "cnt"), data = subset(bike_rent))+ 
             stat_boxplot(geom = "errorbar", width = 0.5) +
             geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                           outlier.size=1, notch=FALSE) +
              theme(legend.position="bottom")+
             labs(y=out_names[i],x="cnt")+
              ggtitle(paste("Box plot of responded for",out_names[i])))
}

gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)

#Outliers are found in the columns hum and windspeed

#Remove outliers

for(i in out_names){
  print(i)
  val = bike_rent[,i][bike_rent[,i] %in% boxplot.stats(bike_rent[,i])$out]
  print(length(val))
  bike_rent = bike_rent[which(!bike_rent[,i] %in% val),]
}

#--------------------------------------------------Feature Engineering--------------------------------------------
#changing the datatypes
bike_rent$dteday = as.Date(bike_rent$dteday)
bike_rent$season = as.factor(bike_rent$season)
bike_rent$yr = as.factor(bike_rent$yr)
bike_rent$mnth = as.factor(bike_rent$mnth)
bike_rent$holiday = as.factor(bike_rent$holiday)
bike_rent$weekday = as.factor(bike_rent$weekday)
bike_rent$workingday = as.factor(bike_rent$workingday)
bike_rent$weathersit = as.factor(bike_rent$weathersit)
bike_rent$temp = as.double(bike_rent$temp)
bike_rent$atemp = as.double(bike_rent$atemp)
bike_rent$hum = as.double(bike_rent$hum)
bike_rent$windspeed = as.double(bike_rent$windspeed)
bike_rent$casual = as.numeric(bike_rent$casual)
bike_rent$registered = as.numeric(bike_rent$registered)
bike_rent$cnt = as.numeric(bike_rent$cnt)

#----------------------------------------------------Feature Selection-----------------------------------------------
library(corrplot)

cor_matrix = cor(bike_rent[,10:13])
cor_matrix
corrplot(cor_matrix, type = "lower", tl.col = "red")

#Chi-square test
category_index = sapply(bike_rent,is.factor)
category_data = bike_rent[,category_index]

for (i in 1:7)
{
  print(names(category_data)[i])
  print(chisq.test(table(bike_rent$cnt,category_data[,i])))
}

#-----------------------------------------------------Dimension Reduction---------------------------------------------
library(dplyr)
bike_rent = select(bike_rent, -c(atemp,holiday,workingday,casual,registered))

#------------------------------------------------------EDA-------------------------------------------------------------

#Distribution of cnt
hist(bike_rent$cnt, xlab = 'cnt', ylab = 'frequency',main = 'cnt distribution', col = 'blue1')

#Bike Rentals based on Weathersituations
ggplot(data=bike_rent, aes(x=weathersit,group=1)) + 
  geom_bar(fill="blue1",size = 2) + ylab("no of bikes rented")

#Bike Rentals Monthly
ggplot(data=bike_rent, aes(x=mnth,group=1)) + 
  geom_bar(fill="blue1",size = 2) + ylab("no of bikes rented")

#Sales by Season
ggplot(data=bike_rent, aes(x=season,group=1)) + 
  geom_bar(fill="blue1",size = 2) + ylab("no of bikes rented")

#------------------------------------------------------Model Development---------------------------------------------------

#Split the data
library(caret)
X = createDataPartition(bike_rent[,"cnt"], p = .80, list = FALSE)
train = bike_rent[ X,]
test  = bike_rent[-X,]




#Decision tree regressor
library(rpart)
dt_model = rpart(cnt~.,data = train,method = "anova")
dt_predictions = predict(dt_model,test[,-11])

#MEAN ABBSOLUTE PERCENTAGE ERROR
mape = function(x,y){
  mean(abs((x-y)/x))*100
}

#metrics for decision tree
mape(test[,11],dt_predictions)

#Random Forest
library(randomForest)
RF_model = randomForest(cnt ~ ., train, importance = TRUE, ntree = 500)
RF_Predictions = predict(RF_model, test[,-11])
plot(RF_model)

#metrics for random forest
mape(test[,11], RF_Predictions)

#Random Forest is found to be efficient
write.csv(RF_Predictions, "final_output_r.csv", row.names = T)


