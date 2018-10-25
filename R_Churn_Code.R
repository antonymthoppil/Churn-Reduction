rm(list = ls())
setwd("C://Users/tamat/Desktop/Antony/Projects/Doing/Churn Reduction")
# #loading Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "e1071",
      "DataCombine", "pROC", "doSNOW", "class", "readxl","ROSE")
# 
# #install.packages if not
# #lapply(x, install.packages)
#
# #load Packages
lapply(x, require, character.only = TRUE)
rm(x)

#Input Train & Test Data Source
train_Original = read.csv('Train_data.csv',header = T,na.strings = c(""," ","NA"))
test_Original = read.csv('Test_data.csv',header = T,na.strings = c(""," ","NA"))
#Creating backup of orginal data 
train = train_Original  
test = test_Original
###########################################################################
#                  EXPLORING DATA
###########################################################################

#viewing the data
head(train,4)
dim(train)

#structure of data or data types
str(train)  

#Summary of data 
summary(train)

#unique value of each count
apply(train, 2,function(x) length(table(x)))

#Replacing the dot b/w collumn name to underscore for easy to use
names(train) <- gsub('\\.','_',names(train))
names(test) <- gsub('\\.','_',names(test))

#Convertiung area code as factor
train$area_code <- as.factor(train$area_code)
test$area_code <- as.factor(test$area_code)

#Removing phone number
train$phone_number <- NULL
test$phone_number <- NULL

#Let's see the percentage of our target variable
round(prop.table(table(train$Churn))*100,2)
# False.   True. 
# 85.51   14.49 

#Our target Class is suffering from target imbalance

#########################################################################
#          Checking Missing data
#########################################################################
apply(train, 2, function(x) {sum(is.na(x))}) #2 for columns as in R 1 = Row & 2 = Col 
apply(test, 2, function(x) {sum(is.na(x))}) 

#Hence no missing data found

#########################################################################
#          Visualizing teh data
#########################################################################

#library(ggplot2)
#Target class distribution 
ggplot(data = train,aes(x = Churn))+
  geom_bar() +  labs(y='Churn Count', title = 'Customer Churn or Not')

# Churning of customer according to State
ggplot(train, aes(fill=Churn, x=state)) +
  geom_bar(position="dodge") + labs(title="Churning ~ State")

# Churning of customer according to Voice Mail Plan
ggplot(train, aes(fill=Churn, x=voice_mail_plan)) +
  geom_bar(position="dodge") + labs(title="Churning ~ Voice Mail Plan")

# Churning of customer according to international_plan
ggplot(train, aes(fill=Churn, x=international_plan)) +
  geom_bar(position="dodge") + labs(title="Churning ~ international_plan")

# Churning of customer according to area_code
ggplot(train, aes(fill=Churn, x=area_code)) +
  geom_bar(position="dodge") + labs(title="Churning ~ Area Code")

# Churning of customer according to area_code by international_plan
ggplot(train, aes(fill=Churn, x=area_code)) +
  geom_bar(position="dodge") + facet_wrap(~international_plan)+
  labs(title="Churning ~ Area Code by International Plan")

# Churning of customer according to area_code by voicemail_plan
ggplot(train, aes(fill=Churn, x=area_code)) +
  geom_bar(position="dodge") + facet_wrap(~voice_mail_plan)+
  labs(title="Churning ~ Area Code by Voice Mail Plan")

# Churn of international_plan by voice_mail_plan and Area Code
ggplot(train, aes(fill=Churn, x=international_plan)) +
  geom_bar(position="dodge") + facet_wrap(area_code~voice_mail_plan)+
  labs(title="Churn of international_plan by voice_mail_plan and Area Code")

# Churn ~ international_plan by voice_mail_plan
ggplot(train, aes(fill=Churn, x=international_plan)) +
  geom_bar(position="dodge") + facet_wrap(~voice_mail_plan)+
  labs(title="Churn ~ international_plan by voice_mail_plan")

#########################################################################
#                   EDA
#########################################################################

#Function for Assigning factors of var to levels
cat_to_num <- function(df){
  for(i in 1:ncol(df)){
    if(class(df[,i]) == 'factor'){
      df[,i] = factor(df[,i],labels = (1:length(levels(factor(df[,i])))))
    }
  }
  return(df)
}

#Converting Categorical to level -> factors
train = cat_to_num(train)
test = cat_to_num(test)

#all numeric var
num_index = sapply(train, is.numeric)
num_data = train[,num_index]
num_col = colnames(num_data) #storing all the column name

#Checking for categorical features
cat_index = sapply(train,is.factor) #Fetching all the categorical index & later data
cat_data = train[,cat_index]
cat_col = colnames(cat_data)[-5]  #Removing target var

################################################################
#               Outlier Analysis
################################################################

#  #We are skipping outliers analysis becoz we already have an Class Imbalance problem.

# for (i in 1:length(num_col))
# {
#   assign(paste0("gn",i),
#          ggplot(aes_string(y = (num_col[i]), x = 'Churn'),data = train) +
#            stat_boxplot(geom = "errorbar", width = 0.5) +
#            geom_boxplot(outlier.colour="blue", fill = "skyblue",
#                         outlier.shape=18,outlier.size=1, notch=FALSE) +
#            labs(y=num_col[i],x="Churn")+
#            ggtitle(paste("Box plot of responded for",num_col[i])))
# }

#gn1
#
## Plotting plots together
#gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
#gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)
#gridExtra::grid.arrange(gn7,gn8,gn9,ncol=3)
#gridExtra::grid.arrange(gn10,gn11,gn12,ncol=3)
# gridExtra::grid.arrange(gn13,gn14,gn15,ncol=3)

# #Removing oulier by replacing with NA and then impute
# for(i in num_col){
#   print(i)
#   outv = train[,i][train[,i] %in% boxplot.stats(train[,i])$out]
#   print(length(outv))
#   train[,i][train[,i] %in% outv] = NA
# }
# 
# #checking all the missing values
# library(DMwR)
# sum(is.na(train))
# train = knnImputation(train, k=3)  #as it gives error so we going via mean or median

################################################################
#               Feacture Selection
################################################################

#Here we will use corrgram library to find corelation

##Correlation plot
# library(corrgram)

corrgram(train[,num_index],
         order = F,  #we don't want to reorder
         upper.panel=panel.pie,
         lower.panel=panel.shade,
         text.panel=panel.txt,
         main = 'CORRELATION PLOT')
#We can see var the highly corr related var in plot marked dark blue. 
#Dark blue color means highly positive cor related

##---------Chi Square Analysis----------------------------------

for(i in cat_col){
  print(names(cat_data[i]))
  print((chisq.test(table(cat_data$Churn,cat_data[,i])))[3])  #printing only pvalue
}

##-----------------Removing Highly Corelated and Independent var----------------------
train = subset(train,select= -c(state,total_day_charge,total_eve_charge,
                                    total_night_charge,total_intl_charge))

test = subset(test,select= -c(state,total_day_charge,total_eve_charge,
                                total_night_charge,total_intl_charge))


################################################################
#               Feacture Scaling
################################################################

#all numeric var
num_index = sapply(train, is.numeric)
num_data = train[,num_index]
num_col = colnames(num_data) #storing all the column name


#Checking Data of Continuous Variable

################  Histogram   ##################
hist(train$total_day_calls)
hist(train$total_day_minutes)
hist(train$account_length)

#Most of the data is uniformally distributed
#Using data Standardization/Z-Score here

for(i in num_col){
   print(i)
   train[,i] = (train[,i] - mean(train[,i]))/sd(train[,i])
   test[,i] = (test[,i] - mean(test[,i]))/sd(test[,i])
}

################################################################
#               Sampling of Data
################################################################

# #Divide data into train and test using stratified sampling method

# library(caret)
set.seed(101)
split_index = createDataPartition(train$Churn, p = 0.66, list = FALSE)
trainset = train[split_index,]
validation_set  = train[-split_index,]

#Checking Train Set Target Class
table(trainset$Churn)
# 1    2 
# 1881  319 

# #Our class is Imbalanced 
# Synthetic Over Sampling the minority class & Under Sampling Majority Class to have a good Training Set

# #library(ROSE)  #---> Lib for Over and Under Sampling

trainset <- ROSE(Churn~.,data = trainset,p = 0.5,seed = 101)$data     

table(trainset$Churn) # 1 = 1101  2 = 1099


#Removing All the custom variable from memory
# library(DataCombine)
rmExcept(c("test_Original","train_Original","train","test","trainset","validation_set"))

###########################################################################################

#  #              Basic approach towards ML - Models
# # Let's just get a basic idea of how models perform on our already preprocesed data
# # Later on we will select the best model and will make it more efficient for our Dataset

###########################################################################################

# #function for calculating the FNR,FPR,Accuracy
calc <- function(cm){
  TN = cm[1,1]
  FP = cm[1,2]
  FN = cm[2,1]
  TP = cm[2,2]
  # #calculations
  print(paste0('Accuracy :- ',((TN+TP)/(TN+TP+FN+FP))*100))
  print(paste0('FNR :- ',((FN)/(TP+FN))*100))
  print(paste0('FPR :- ',((FP)/(TN+FP))*100))
  print(paste0('FPR :- ',((FP)/(TN+FP))*100))
  print(paste0('precision :-  ',((TP)/(TP+FP))*100)) 
  print(paste0('recall//TPR :-  ',((TP)/(TP+FP))*100))
  print(paste0('Sensitivity :-  ',((TP)/(TP+FN))*100))
  print(paste0('Specificity :-  ',((TN)/(TN+FP))*100))
  plot(cm)
}

### ##----------------------- Random Forest ----------------------- ## ###
# library(randomForest)

set.seed(101)
RF_model = randomForest(Churn ~ ., trainset,ntree= 500,importance=T,type='class')
plot(RF_model)
  #Predict test data using random forest model
RF_Predictions = predict(RF_model, validation_set[,-15])

##Evaluate the performance of classification model
cm_RF = table(validation_set$Churn,RF_Predictions)
confusionMatrix(cm_RF)
calc(cm_RF)
plot(RF_model)
# Result on validaton set
# [1] "Accuracy :- 86.2312444836717"
# [1] "FNR :- 17.6829268292683"
# [1] "FPR :- 13.1062951496388"
# [1] "FPR :- 13.1062951496388"
# [1] "precision :-  51.5267175572519"
# [1] "recall//TPR :-  51.5267175572519"
# [1] "Sensitivity :-  82.3170731707317"
# [1] "Specificity :-  86.8937048503612"

### ##----------------------- LOGISTIC REGRESSION ----------------------- ## ###
set.seed(101)
logit_model = glm(Churn ~., data = trainset, family =binomial(link="logit")) 
summary(logit_model)
#Prediction
logit_pred = predict(logit_model,newdata = validation_set[,-15],type = 'response')

#Converting Prob to number or class
logit_pred = ifelse(logit_pred > 0.5, 2,1)
#logit_pred = as.factor(logit_pred)
##Evaluate the performance of classification model
cm_logit = table(validation_set$Churn, logit_pred)
confusionMatrix(cm_logit)
calc(cm_logit)
plot(logit_model)
#roc(validation_set$Churn~logit_pred)

# Result on validaton set
# [1] "Accuracy :- 78.1994704324801"
# [1] "FNR :- 28.6585365853659"
# [1] "FPR :- 20.6398348813209"
# [1] "FPR :- 20.6398348813209"
# [1] "precision :-  36.9085173501577"
# [1] "recall//TPR :-  36.9085173501577"
# [1] "Sensitivity :-  71.3414634146341"
# [1] "Specificity :-  79.360165118679"
# ROC = 0.8017

### ##----------------------- KNN ----------------------- ## ###
set.seed(101)
## KNN impletation
# library(class)

##Predicting Test data
#knn_Pred = knn(train = trainset[,1:14],test = validation_set[,1:14],cl = trainset$Churn, k = 5)

knn_Pred = knn(train = trainset[,1:14],test = validation_set[,1:14],cl = trainset$Churn, k = 5,prob = T)
#Confusion matrix
cm_knn = table(validation_set$Churn,knn_Pred)
confusionMatrix(cm_knn)
calc(cm_knn)

# Result on validaton set
# [1] "Accuracy :- 78.8172992056487"
# [1] "FNR :- 46.3414634146341"
# [1] "FPR :- 16.9246646026832"
# [1] "FPR :- 16.9246646026832"
# [1] "precision :-  34.9206349206349"
# [1] "recall//TPR :-  34.9206349206349"
# [1] "Sensitivity :-  53.6585365853659"
# [1] "Specificity :-  83.0753353973168"

### ##----------------------- Naive Bayes ----------------------- ## ###

# library(e1071) #lib for Naive bayes
set.seed(101)
#Model Development and Training
naive_model = naiveBayes(Churn ~., data = trainset, type = 'class')
#prediction
naive_pred = predict(naive_model,validation_set[,1:14])

#Confusion matrix
cm_naive = table(validation_set[,15],naive_pred)
confusionMatrix(cm_naive)
calc(cm_naive)

# Result on validaton set
# [1] "Accuracy :- 83.1421006178288"
# [1] "FNR :- 29.2682926829268"
# [1] "FPR :- 14.7574819401445"
# [1] "FPR :- 14.7574819401445"
# [1] "precision :-  44.7876447876448"
# [1] "recall//TPR :-  44.7876447876448"
# [1] "Sensitivity :-  70.7317073170732"
# [1] "Specificity :-  85.2425180598555"

####################################################################################################

# # As according to out problem statement we need to find out the customer which will Move or Not.

# # " Reduction customer churn is important because cost of acquiring a new customer is higher then retaining the older one."

## From above statement it's clear that Cost matters alot. 
## We are using default threshold cutoff here for Churning and Not Churn  

# # So according to requirement we are finalizing RandomForest as our Final model as under train data set 
# # Random forest model out performs the all other model in FNR,FPR and Accuracy.



###################################################################
# #             Knowing the right hyper parameters tuning
# # As this process will take a bit time so here i have commented the code 
###################################################################

#Using doSNOW lib for segmenting the clustering onto task as a faster approch
# library(doSNOW)

# # #Best mtry  ======   found best as = 4 
# cl <- makeCluster(6) #clustering approach using doSNOW pkg
# registerDoSNOW(cl)
# 
# trControl <- trainControl(method = "cv",number = 10,search = "grid")
# set.seed(101)
# tuneGrid <- expand.grid(.mtry = c(2:8))
# rf_mtry <- train(Churn~.,data = trainset,method = "rf",metric = "Accuracy",
#                  tuneGrid = tuneGrid,trControl = trControl,importance = TRUE,ntree = 800)
# best_mtry <- rf_mtry$bestTune$mtry              
# print(best_mtry)

# # #Looking for best ntree  ====  found best as = 800
# store_maxtrees <- list()
# tuneGrid <- expand.grid(.mtry = best_mtry)
# for (ntree in c(200, 300, 350, 400, 450, 500, 550, 600, 700,800, 1000)) {
#   set.seed(101)
#   rf_maxtrees <- train(Churn~.,data = df_trainset,method = "rf",metric = "Accuracy",tuneGrid = tuneGrid,
#                        trControl = trControl,importance = TRUE,ntree = ntree)
#   key <- toString(ntree)
#   store_maxtrees[[key]] <- rf_maxtrees
# }
# results_tree <- resamples(store_maxtrees)
# summary(results_tree)
# 
# stopCluster(cl)


rmExcept(c("train_Original","test_Original","train","test","trainset","validation_set","calc"))

###########################################################################################
#  #              Final Random Forest Model with tuning parameters
###########################################################################################

set.seed(101)
final_model = randomForest(Churn~.,data = trainset,ntree=800,mtry=4,importance=TRUE,type = 'class')
final_validation_pred = predict(final_model,validation_set[,-15])
cm_final_valid = table(validation_set[,15],final_validation_pred)
confusionMatrix(cm_final_valid)
calc(cm_final_valid)
#Result on validation set after parameter tuning
# [1] "Accuracy :- 86.4960282436011"
# [1] "FNR :- 17.0731707317073"
# [1] "FPR :- 12.8998968008256"
# [1] "FPR :- 12.8998968008256"
# [1] "precision :-  52.1072796934866"
# [1] "recall//TPR :-  52.1072796934866"
# [1] "Sensitivity :-  82.9268292682927"
# [1] "Specificity :-  87.1001031991744"


#Variable Importance
importance(final_model) #builting function in Random forest lib
varImpPlot(final_model) #builtin func

#Plotting ROC curve and Calculate AUC metric
# library(pROC)
PredictionwithProb <-predict(final_model,validation_set[,-15],type = 'prob')
auc <- auc(validation_set$Churn,PredictionwithProb[,2])
auc
# # AUC = 89.47
plot(roc(validation_set$Churn,PredictionwithProb[,2]))

###################################################################################

# #       Final Prediction On test Data set 

###################################################################################

rmExcept(c("final_model","train","test","train_Original","test_Original","calc"))

set.seed(101)
final_test_pred = predict(final_model,test[,-15])
cm_final_test = table(test[,15],final_test_pred)
confusionMatrix(cm_final_test)
calc(cm_final_test)

# #Final Test Prediction
# [1] "Accuracy :- 85.9028194361128"
# [1] "FNR :- 15.625"
# [1] "FPR :- 13.8600138600139"
# [1] "FPR :- 13.8600138600139"
# [1] "precision :-  48.586118251928"
# [1] "recall//TPR :-  48.586118251928"
# [1] "Sensitivity :-  84.375"
# [1] "Specificity :-  86.1399861399861"

#Plotting ROC curve and Calculate AUC metric
# library(pROC)
finalPredictionwithProb <-predict(final_model,test[,-15],type = 'prob')
auc <- auc(test$Churn,finalPredictionwithProb[,2])
auc
# # AUC = 91.74
plot(roc(test$Churn,finalPredictionwithProb[,2]))

################################################################################################################################
#  #            Saving output to file ( For re run uncomment the code (ctrl+shift+c))
################################################################################################################################

test_Original$predicted_output <- final_test_pred
test_Original$predicted_output <- gsub(1,"False",test_Original$predicted_output)
test_Original$predicted_output <- gsub(2,"True",test_Original$predicted_output)

#Entire Comparison
write.csv(test_Original,'C://Users/parve/Documents/Data_Science_Project/Data/R_Output/Final_Full_Data_Output.csv',row.names = F)

#Phonenumber and Churning class and probab
submit <- data.frame(test_Original$state,
                     test_Original$area.code,
                     test_Original$international.plan,
                     test_Original$voice.mail.plan,
                     test_Original$phone.number,
                     test_Original$predicted_output,
                     finalPredictionwithProb[,1],
                     finalPredictionwithProb[,2])

colnames(submit) <- c("State","Area Code","International Plan","Voice Mail Plan","Phone_Number",
                      "Predicted_Output","Probability_of_False","Probability_of_True")

write.csv(submit,file = 'C://Users/parve/Documents/Data_Science_Project/Data/R_Output/Final_Churn_Class_Probab.csv',row.names = F)
rm(list = ls())