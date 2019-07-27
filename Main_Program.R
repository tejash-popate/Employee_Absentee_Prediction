rm(list = ls())
#path="C:/Users/ELdrago/DesktopEdwisor/Projects/Employee Absenteeism"
#setwd(path)


#-------load required libraries
x=c("tidyverse", "corrgram", "DMwR", "caret", "randomForest", "unbalanced",
    "C50", "dummies", "e1071", "Information","MASS", "rpart", "gbm", "ROSE",'tidyverse','Hmisc','funModeling',
    'sampling', 'DataCombine', 'inTrees','readxl','mice','missMDA','data.table','lsr')
lapply(x,require,character.only= TRUE) 
rm(x)

#load the dataset
employee_df=read_xls("C:/Users/ELdrago/Desktop/Edwisor/Projects/Employee Absenteeism/R_Code/Absenteeism_at_work_Project.xls")
employee_df=as.data.frame(employee_df)
View(employee_df)
str(employee_df)
#-------------------------------------------
#-------------------1)Exploratory Data Analysis

#replacing whitespaces from the column names with underscore (_) for simplicity
colnames(employee_df)<-gsub(" ","_",colnames(employee_df))
colnames(employee_df)<-gsub("/","_",colnames(employee_df))
colnames(employee_df)

cont_vars = c('Distance_from_Residence_to_Work', 'Service_time', 'Age',
                    'Work_load_Average_day', 'Transportation_expense',
                    'Hit_target', 'Weight', 'Height', 
                    'Body_mass_index', 'Absenteeism_time_in_hours')

cat_vars = c('ID','Reason_for_absence','Month_of_absence','Day_of_the_week',
                     'Seasons','Disciplinary_failure', 'Education', 'Social_drinker',
                     'Social_smoker', 'Son', 'Pet')
categorical_data=subset(employee_df,select=cat_vars)
continuous_data=subset(employee_df,select=cont_vars)
target_var='Absenteeism_time_in_hours'

# Univariate Analysis
num_eda <- function(data)
{
  glimpse(data)
  df_status(data)
  profiling_num(data)
  plot_num(data)
  describe(data)
}
cat_eda <- function(data)
{
  glimpse(data)
  df_status(data)
  freq(data) 
  plot_num(data)
  describe(data)
}

cat_eda(categorical_data)
num_eda(continuous_data)


#-----------------2) Missing Value Analysis

show_missing_value<-function(dataset){
# creating dataframe with missing percentage
missing_val=data.frame(apply(dataset,2,function(x) {sum(is.na(x))}))
#convert row names into column
missing_val$columns=row.names(missing_val)
row.names(missing_val)= NULL

# renaming first variable name(column name)
names(missing_val)[1]="Missing_percentage"
#calculate percentage
missing_val$Missing_percentage=(missing_val$Missing_percentage/nrow(dataset))*100

# arrange in descending order
missing_val=missing_val[order(-missing_val$Missing_percentage),]
#rearranging the columns
missing_val=missing_val[,c(2,1)]
#saving this dataframe on disk
write.csv(missing_val,"Missing_val.csv",row.names=F)
#View(missing_val)
#save the plot
# 1. Open jpeg file
jpeg("missing_value_plot.jpg", width = 350, height = 350)
# data visualization using bar graph plot (only top three missing percentages)
ggplot(data = missing_val, aes(x=reorder(columns, -Missing_percentage),y = Missing_percentage))+
  geom_bar(stat = "identity",fill = "red")+xlab("Parameter")+
  ggtitle("Missing data percentage (Train)") + theme_bw()
# 3. Close the file
dev.off()
}

show_missing_value(employee_df)
#-----ordering data by column 'ID'
#employee_df=(employee_df[order(employee_df$ID),])
temp_data=copy(employee_df)
#View(temp_data)
employee_df=copy(temp_data)
#----pattern of missing values
md.pattern(employee_df)
#---------Imputation Results
# Original Value= 179
# Mean Imputation= 221
# Median Imputation= 225
# KNN (k=5) Imputation= 179
#------Imputation of missing values-------------------
# creating a test element
employee_df$Transportation_expense[170]=NA
# 1) imputation using mean of variable
#employee_df$Transportation_expense[is.na(employee_df$Transportation_expense)] = mean(employee_df$Transportation_expense, na.rm = T)
#employee_df$Transportation_expense[170]
# 2) imputation using median of the variable
#employee_df$Transportation_expense[is.na(employee_df$Transportation_expense)] = median(employee_df$Transportation_expense, na.rm = T)
#employee_df$Transportation_expense[170]
# 3) KNN imputation
employee_df = knnImputation(employee_df, k = 5)
employee_df$Transportation_expense[170]
# 4) MICE imputation 
#employee_df <- mice(employee_df,m=5,maxit=50,meth='pmm',seed=500)
#employee_df$Transportation_expense[70]
# Checking for missing value
sum(is.na(employee_df))

#---------------3) Outlier Analysis
for (i in 1:length(cont_vars))
 {
  assign(paste0("gn",i), ggplot(aes_string(y = (cont_vars[i]), x = target_var), data = subset(employee_df))+ 
            stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=10,
                         outlier.size=1, notch=FALSE) +
            theme(legend.position="bottom")+
            labs(y=cont_vars[i],x=target_var)+
           ggtitle(paste("Box plot of",cont_vars[i])))
}
# plotting plots together
gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,ncol=2)
gridExtra::grid.arrange(gn5,gn6,ncol=2)
gridExtra::grid.arrange(gn7,gn8,ncol=2)
gridExtra::grid.arrange(gn9,gn10,ncol=2)
# removing outliers using boxplot method
df=copy(employee_df) # for backup
for(i in cont_vars){
    print(i)
    val=employee_df[,i][employee_df[,i] %in% boxplot.stats(employee_df[,i])$out ]
    employee_df=employee_df[which(!employee_df[,i] %in% val),]
}
str(employee_df)
# replacing outliers with NA and use KNN imputation
for(i in cont_vars){
     print(i)
     val=employee_df[,i][employee_df[,i] %in% boxplot.stats(employee_df[,i])$out ]
     employee_df[,i][(employee_df[,i] %in% val)]= NA
}
sum(is.na(employee_df))
#KNN imputation
employee_df=knnImputation(employee_df, k=5)

#----------------4) Feature Selection
# Correlation plot
jpeg("correlation_plot.jpg", width = 350, height = 350)
corrgram(employee_df[,cont_vars],order=F,upper.panel = panel.pie,text.panel = panel.txt, main="Correlation Plot")
dev.off()

# ANOVA test for categorical variables
library("lsr")
anova_test = aov(Absenteeism_time_in_hours ~ ID + Day_of_the_week + Education + Social_smoker + Social_drinker+ Pet + Son  + Reason_for_absence + Seasons + Month_of_absence + Disciplinary_failure, data = employee_df)
summary(anova_test)

# dimension reduction
employee_df = subset(employee_df, select = -c(Body_mass_index))
cleaned_data=copy(employee_df)
write.csv(cleaned_data,"cleaned_data_by_tejashpopate.csv",row.names=F)
#----------------5) Feauture Scaling
#Histogram for normality check
hist(employee_df$Absenteeism_time_in_hours)
hist(employee_df$Work_load_Average_day)
hist(employee_df$Transportation_expense)
cont_vars=c('Distance_from_Residence_to_Work', 'Service_time', 'Age',
            'Work_load_Average_day', 'Transportation_expense',
            'Hit_target', 'Height', 
            'Weight')
cat_vars=c('ID','Reason_for_absence','Disciplinary_failure', 
           'Social_drinker', 'Son', 'Pet', 'Month_of_absence', 'Day_of_the_week', 'Seasons',
           'Education', 'Social_smoker')
# Normalization
for(i in cont_vars)
{
  print(i)
  employee_df[,i] = (employee_df[,i] - min(employee_df[,i]))/(max(employee_df[,i])-min(employee_df[,i]))
}

# Creating dummy variables for categorical variables
employee_df = dummy.data.frame(employee_df, cat_vars)

#--------------------MODEL DEVELOPEMENT

rmExcept("employee_df")

# splitting the dataset
#Divide data into train and test using stratified sampling method
set.seed(1234)
train.index=sample(1:nrow(employee_df),0.8*nrow(employee_df))
#train.index=createDataPartition(employee_df$Absenteeism_time_in_hours,p=.80,list=F)
train_data=employee_df[train.index,]
test_data=employee_df[-train.index,]
# r squared evaluation
rsq <- function(x, y) summary(lm(y~x))$r.squared

#----------------------- 1) Linear Regression
set.seed(1234)
#Develop Model on training data
fit_LR = lm(Absenteeism_time_in_hours ~ ., data = train_data)
#Lets predict for training data
pred_LR_train = predict(fit_LR, train_data[,names(test_data) != "Absenteeism_time_in_hours"])
#Lets predict for testing data
pred_LR_test = predict(fit_LR,test_data[,names(test_data) != "Absenteeism_time_in_hours"])
# For training data 
print(regr.eval(train_data[,"Absenteeism_time_in_hours"],pred_LR_train,stats=c("rmse","mse","mae")))
cat("rsq for train", rsq(train_data[,"Absenteeism_time_in_hours"],pred_LR_train))
# For testing data 
print(regr.eval(test_data[,"Absenteeism_time_in_hours"],pred_LR_test,stats=c("rmse","mse","mae")))
cat("rsq for test", rsq(test_data[,"Absenteeism_time_in_hours"],pred_LR_test))
#------------------------2) Decision Trees
set.seed(1234)
#Develop Model on training data
fit_DT = rpart(Absenteeism_time_in_hours ~., data = train_data, method = "anova")
#Summary of DT model
summary(fit_DT)
#write rules into disk
write(capture.output(summary(fit_DT)), "Rules.txt")
#Lets predict for training data
pred_DT_train = predict(fit_DT, train_data[,names(test_data) != "Absenteeism_time_in_hours"])
#Lets predict for training data
pred_DT_test = predict(fit_DT,test_data[,names(test_data) != "Absenteeism_time_in_hours"])
# For training data 
print(regr.eval(train_data[,"Absenteeism_time_in_hours"],pred_DT_train,stats=c("rmse","mse","mae")))
cat("rsq for train", rsq(train_data[,"Absenteeism_time_in_hours"],pred_DT_train))
# For testing data 
print(regr.eval(test_data[,"Absenteeism_time_in_hours"],pred_DT_test,stats=c("rmse","mse","mae")))
cat("rsq for test", rsq(test_data[,"Absenteeism_time_in_hours"],pred_DT_test))

#-------------------------3) Random Forest
set.seed(1234)
#Develop Model on training data
fit_RF = randomForest(Absenteeism_time_in_hours~., data = train_data)
#Lets predict for training data
pred_RF_train = predict(fit_RF, train_data[,names(test_data) != "Absenteeism_time_in_hours"])
#Lets predict for testing data
pred_RF_test = predict(fit_RF,test_data[,names(test_data) != "Absenteeism_time_in_hours"])
# For training data 
print(regr.eval(train_data[,"Absenteeism_time_in_hours"],pred_RF_train,stats=c("rmse","mse","mae")))
cat("rsq for train", rsq(train_data[,"Absenteeism_time_in_hours"],pred_RF_train))
# For testing data 
print(regr.eval(test_data[,"Absenteeism_time_in_hours"],pred_RF_test,stats=c("rmse","mse","mae")))
cat("rsq for test", rsq(test_data[,"Absenteeism_time_in_hours"],pred_RF_test))
#---------------------------End of the Project----------------------------#

