rm(list = ls(all=TRUE))

library(xlsx)
train <- read.xlsx('Train_UWu5bXk.xlsx',sheetIndex = 1)
test <- read.xlsx('Test_u94Q5KV.xlsx', sheetIndex = 1)

temp <- data.frame(Item_Outlet_Sales=rep("None",nrow(test)),test[,])

full_data <- rbind(train,temp) 

str(full_data)


colnames(full_data)[colSums(is.na(full_data)) > 0]
#[1] "Item_Weight" "Outlet_Size"

summary(full_data)


unique_values <- apply(full_data, 2, function(x)length(unique(x)))


var1 <- sapply(full_data , is.factor)
cat_var <- full_data[var1]
cat_var <- subset(cat_var, select = - Item_Identifier)
unique_values1 <- apply(cat_var, 2, unique)
unique_values1

temp1 <- full_data
temp1 <- subset(temp1, select = - Item_Outlet_Sales)

anova_mod <- rpart(Item_Weight ~ . , 
                   data=temp1[!is.na(temp1$Item_Weight), ], method="anova", na.action=na.omit)  

x <- predict(anova_mod,temp1[is.na(temp1$Item_Weight),])

full_data[is.na(full_data$Item_Weight),"Item_Weight"] <- x


temp2 <- full_data
temp2 <- subset(temp2, select = - Item_Outlet_Sales)
library(mice)
miceMod <- mice(temp2[, !names(temp2) %in% "Item_Outlet_Sales"], method="rf")  
miceOutput <- complete(miceMod)  # generate the completed data.

Item_Outlet_Sales <- full_data$Item_Outlet_Sales
new <- cbind(miceOutput,Item_Outlet_Sales)

length(which(new$Item_Outlet_Sales == 'None'))

library(sqldf)

sqldf('select AVG(Item_Outlet_Sales), Outlet_Type from new group by Outlet_Type ')

sqldf('select COUNT(Item_Visibility) from new where Item_Visibility = 0 ')

new$Item_Visibility[new$Item_Visibility == 0] <- mean(new$Item_Visibility)

sqldf('select COUNT(Item_Visibility) from new where Item_Visibility = 0 ')

visibility_avg <- mean(new$Item_Visibility)

get_first_2_char <-sapply(new[,'Item_Identifier'], substring, 1, 2)
length(get_first_2_char)

 temp3 <- ''
for(i in 1:length(get_first_2_char)){
  if(get_first_2_char[i] == 'FD'){
    temp3[i] <- 'Food' 
  }else if(get_first_2_char[i] == 'DR'){
    temp3[i] <- 'Drinks'
  }else{
    temp3[i] <- 'Non-Consumable'
  }
}
 
new$Item_Type_Combined <-temp3

length(new$Item_Identifier)

s <- sqldf('select AVG(Item_Outlet_Sales),Item_Type from new group by Item_Type')

summary(s)
nrow(s)

temp4 <- ''
for(i in 1:nrow(s)){
  if(s[i,1] >1293 && s[i,1] <= 1673){
    temp4[i] <- 'High_Sales_Item_Type'
  }else {
    temp4[i] <- 'Low_Sales_Item_Type'
  }
}

s$category_by_sales <- temp4

new_full_data <- sqldf('select * from new n INNER JOIN s on n.Item_Type = s.Item_Type ')

library(dplyr)

new_full_data <- new_full_data %>% subset(., select=which(!duplicated(names(.)))) 

colnames(new_full_data)

colnames(new_full_data)[which(names(new_full_data) == "AVG(Item_Outlet_Sales)")] <- "Avg_Item_Outlet_Sales"

unique(new_full_data$Outlet_Establishment_Year)

new_full_data['Outlet_Establishment_Year'] <- 2013 - new_full_data$Outlet_Establishment_Year

summary(new_full_data$Outlet_Establishment_Year)

summary(new_full_data)

temp4 <- new_full_data

for(i in 1:nrow(temp4)){
  if(temp4[i,'Item_Fat_Content'] == 'LF' || temp4[i,'Item_Fat_Content'] == 'low fat' || temp4[i,'Item_Fat_Content'] == 'Low Fat' ){
    
    temp4[i,'Item_Fat_Content'] <- 'Low Fat'
  }else{
    temp4[i,'Item_Fat_Content'] <- 'Regular'
  }
}

new_full_data <- d

unique(new_full_data$Item_Fat_Content)

summary(new_full_data)

summary(temp5)

temp5 <- new_full_data

temp5$Item_Fat_Content <- as.character(temp5$Item_Fat_Content)

for(i in 1:nrow(temp5)){
  if(temp5[i,'Item_Type_Combined'] == 'Non-Consumable'){
    temp5[i,'Item_Fat_Content'] <- 'Non-Edible'
  }else{
    next
  }
}

unique(temp5$Item_Fat_Content)

temp5$Item_Fat_Content <- as.factor(temp5$Item_Fat_Content)

temp5$Item_Type_Combined <- as.factor(temp5$Item_Type_Combined)

temp5$category_by_sales <- as.factor(temp5$category_by_sales)

str(temp5)

summary(temp5)

temp5$Item_Visibility[temp5$Item_Visibility == 0] <- mean(temp5$Item_Visibility)


train <- temp5[which(!temp5$Item_Outlet_Sales == 'None'),]

test <- temp5[which(temp5$Item_Outlet_Sales == 'None'),]

test <- subset(test, select = - Item_Outlet_Sales)

summary(train$Item_Outlet_Sales)

train$Item_Outlet_Sales <- as.character(train$Item_Outlet_Sales)

train$Item_Outlet_Sales <- as.numeric(train$Item_Outlet_Sales)

 ## Creating a baseline model

mean_sales <- mean(train$Item_Outlet_Sales)

test$Item_Outlet_Sales <- mean_sales

alg0 <- test
alg1 <- train

library(xlxs)

write.xlsx(alg0,'C:/Users/devarsh patel/Desktop/New folder/alg0.xlsx')
write.xlsx(alg1,'C:/Users/devarsh patel/Desktop/New folder/alg1.xlsx')

## Linear Regression
data_linear_train <- read.csv('data_train_python.csv')

data_linear_train <- subset(data_linear_train,select = - c(Item_Type,Outlet_Identifier))

str(data_linear_train)

Model_Linear <- lm(Item_Outlet_Sales~.,data = data_linear_train)

summary(Model_Linear)

AIC(Model_Linear)

BIC(Model_Linear)

data_linear_test <- read.csv('data_test_python.csv')


predicted_values <- predict(Model_Linear,data_linear_test)

data_linear_test$Item_Outlet_Sales <- predicted_values

LinearModelSub <- data_linear_test
  
write.xlsx(LinearModelSub,'C:/Users/devarsh patel/Desktop/New folder/LinearModelSub.xlsx')

library(RcmdrMisc)

Model_Linear2.0 <- stepwise(Model_Linear, direction="backward",criterion = "BIC")

BIC(Model_Linear2.0)

summary(Model_Linear2.0)

predicted_values_stepwise <- predict(Model_Linear2.0,data_linear_test)

data_linear_test$Item_Outlet_Sales <- predicted_values_stepwise

LinearModelSub_stepwise <- data_linear_test

write.xlsx(LinearModelSub_stepwise,'C:/Users/devarsh patel/Desktop/New folder/LinearModelSub_stepwise.xlsx')

#Decision Tree
summary(train)
str(train)

library(rpart)
 Model_Decision_Tree <- rpart(Item_Outlet_Sales ~ . , data = train)
 
data_decision_tree <- train
 
data_decision_tree <- subset(train, select = -Item_Outlet_Sales)
 
out <- predict(Model_Decision_Tree,newdata = test)

test$Item_Outlet_Sales <- out 

DecisionTree <- test

test <- subset(test, select = - Item_Outlet_Sales)

write.xlsx(DecisionTree,'C:/Users/devarsh patel/Desktop/New folder/DecisionTree.xlsx')

## Random Forests

data_random_forest <- train
data_random_forest <- subset(data_random_forest,select = - Item_Identifier)

predictors <- subset(data_random_forest, select = - c(Item_Outlet_Sales,Item_Identifier))

str(predictors)

response <- data_random_forest$Item_Outlet_Sales

bestmtry <- tuneRF(predictors,response)

##best mtry was 4
Model_Random_forest <- randomForest(data_random_forest$Item_Outlet_Sales~., data = data_random_forest, mtry = 4,ntree = 601)

Model_Random_forest

RandomForest <- test

RandomForest$Item_Outlet_Sales <- predict(Model_Random_forest,test)

write.xlsx(RandomForest,'C:/Users/devarsh patel/Desktop/New folder/RandomForest.xlsx')

