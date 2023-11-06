# ASSESSING THE CHURN RATE OF USERS ON VARIOUS DIGITAL SERVICES

# Importing Data 
setwd("C:/Users/DEBORAH KALEVOR/Desktop/MSC BDA/SEM 1/Data Science/Deborah Kalevor")
Internet_Usage<-read.csv("Internetuesersdata.csv")
Internet_Usage
View(Internet_Usage)

# Packages necessary for Analysis
library(ggplot2)
library(ggraph)
library(Matrix)
library(dplyr)

#Data Structure
View(Internet_Usage)
str(Internet_Usage)
head(Internet_Usage)
tail(Internet_Usage)
glimpse(Internet_Usage)
summary(Internet_Usage)

# Data cleaning 
Internet_Usage1<-Internet_Usage%>%
  filter(Contract%in%c("Month-to-month","One year","Two year"),
         InternetService%in%c("DSL","Fiber optic"))
(Internet_Usage1)


#checking for missing values
sum(is.na(Internet_Usage1))

#Remove Missing Numbers
Internet_Usage_Clean <- na.omit(Internet_Usage1)


#Check for removed values
sum(is.na(Internet_Usage_Clean))
glimpse(Internet_Usage_Clean)



##VISUALIZING CATEGORICAL VARIABLES
#visualizing data
#Bar plots of categorical variables
#install.packages("gridExtra")
library(gridExtra)

######GRID 1
gender <- ggplot(Internet_Usage_Clean, aes(x=gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

PhoneService <- ggplot(Internet_Usage_Clean, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("Phone Service") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

InternetService <- ggplot(Internet_Usage_Clean, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

Contract <- ggplot(Internet_Usage_Clean, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

PaymentMethod <- ggplot(Internet_Usage_Clean, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

SenoirCitizen <- ggplot(Internet_Usage_Clean, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

grid.arrange(gender, PhoneService, InternetService, Contract, PaymentMethod,SenoirCitizen, ncol=2)


#########GRID 2
Patner <- ggplot(Internet_Usage_Clean, aes(x=Partner)) + ggtitle("Partner") + xlab("Partner") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
Dependents <- ggplot(Internet_Usage_Clean, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
Multiple_Lines <- ggplot(Internet_Usage_Clean, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("Multiple Lines") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
OnlineSecurity <- ggplot(Internet_Usage_Clean, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
OnlineBackup <- ggplot(Internet_Usage_Clean, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
DeviceProtection <- ggplot(Internet_Usage_Clean, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(Patner, Dependents, Multiple_Lines, OnlineSecurity,OnlineBackup,DeviceProtection, ncol=2)



########GRID 3
TechSupport <- ggplot(Internet_Usage_Clean, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
StreamingTV <- ggplot(Internet_Usage_Clean, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
StreamingMovies <- ggplot(Internet_Usage_Clean, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
PaperlessBilling <- ggplot(Internet_Usage_Clean, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
Tenure <- ggplot(Internet_Usage_Clean, aes(x=tenure.Month)) + ggtitle("Tenure") + xlab("Tenure ") +
  geom_density()
MonthlyCharges <- ggplot(Internet_Usage_Clean, aes(x=MonthlyCharges)) + ggtitle("Monthly Charges") + xlab("Monthly Charges ") +
  geom_density()
grid.arrange(TechSupport, StreamingTV, StreamingMovies, PaperlessBilling,Tenure,MonthlyCharges, ncol=2)


##### understanding the data based on some features######
#Selection of Features for analysis
Final_internet_usage <- Internet_Usage_Clean%>%
  select(gender, PhoneService, InternetService, Contract,PaymentMethod,
         MonthlyCharges,TotalCharges,Churn)
View(Final_internet_usage)
glimpse(Final_internet_usage)

# factorizing categorical variables
Final_internet_usage1 <- within(
  Final_internet_usage, {
    gender <- factor(gender, labels = c("Female", "Male"))
    PhoneService <- factor(PhoneService, labels = c("No","Yes"))
    InternetService <- factor(InternetService, labels = c("DSL", "Fiber optic"))
    Contract <- factor(Contract, labels = c("Month-to-month", "One year", "Two year"))
    PaymentMethod <- factor(PaymentMethod, labels = c("Bank transfer (automatic)",
                                                      "Credit card(automatic)",
                                                      "Electronic check",
                                                      "Mailed check"))
  })

glimpse(Final_internet_usage1)


####################################################################################
##BASIC ANALYSIS USING DPLYR

# Customers who use phone service based by Gender  
Female_PhoneService<- Final_internet_usage1%>%
  group_by(gender)%>%
  filter(gender=="Female")%>%
  dplyr::count(PhoneService)
Female_PhoneService

Female_PhoneService%>%
  rename(customers=n)
 

Male_PhoneService<- Final_internet_usage1%>%
  group_by(gender)%>%
  filter(gender=="Male")%>%
  dplyr::count(PhoneService)
Male_PhoneService

Male_PhoneService%>%
  rename(customers=n)

###### Analysis showing a healthy gap of phone service users among both genders.


# customers using DSL
DSL_usage<- Final_internet_usage1%>%
  filter(InternetService=="DSL")
DSL_usage
DSL_usage%>%
  dplyr::count(InternetService)
## visualizing
ggplot(DSL_usage, aes(x = MonthlyCharges, y = TotalCharges, color = InternetService)) +
  geom_point() +
  labs(title = "DSL Internet Service Usage",
       x = "Monthly Charges",
       y = "Total Charges") +
  theme_minimal()

# Customers Using Fiber optic
Fibre_usage<- Final_internet_usage1%>%
  filter(InternetService=="Fiber optic")
Fibre_usage
Fibre_usage%>%
  dplyr::count(InternetService)

### visualizing
ggplot(Fibre_usage, aes(x = MonthlyCharges, y = TotalCharges, color = InternetService)) +
  geom_point() +
  labs(title = "Fiber Optic Internet Service Usage",
       x = "Monthly Charges",
       y = "Total Charges") +
  theme_minimal()

###### per analysis on internet services we can conclude that most customers 
#prefer using a fiber optic to a DSL internet service######


# customers per contract and Internet service
Fibre_usage_count <- Final_internet_usage1%>%
  group_by(Contract,InternetService)%>%
  dplyr::count(Contract,InternetService)
Fibre_usage_count

Fibre_usage_count%>%
  rename(customers=n)

######## Analysis shows that customers prefer the fiber optic internet service 
# on a monthly charge and DSL has more customer subscription bi-yearly and just a 
#little less yearly as compared to the monthly subscriptions.###############
# as compared to fiber optic#################


# customers per gender and internet service 
Gender_usage_count <- Final_internet_usage1%>%
  group_by(gender,InternetService)%>%
  dplyr::count(gender,InternetService)
Gender_usage_count


Gender_usage_count%>%
  rename(customers=n)

## visualizing the info
ggplot(Gender_usage_count, aes(x = gender, y = n, fill = InternetService)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Gender and Internet Service Usage Count",
       x = "Gender",
       y = "Usage Count") +
  theme_minimal()
############### Both genders prefer Fiber optic per the visualization and analysis.


#customers with different contact using DSL
Diff_contract<- Final_internet_usage1%>%
  group_by(InternetService)%>%
  filter(InternetService=="DSL")%>%
  dplyr::count(Contract)
Diff_contract

Diff_contract%>%
  rename(customers=n)

# visualizing 
ggplot(Diff_contract, aes(x = Contract, y = n, fill = Contract)) +
  geom_bar(stat = "identity") +
  labs(title = "Contract Count for DSL Internet Service",
       x = "Contract Type",
       y = "Usage Count") +
  theme_minimal()

#customers with only Monthly contract using DSL
Only_MonthlyContract<- Final_internet_usage1%>%
  group_by(Contract)%>%
  filter(Contract=="Month-to-month",InternetService=="DSL")%>%
  dplyr::count(InternetService)
Only_MonthlyContract

Only_MonthlyContract%>%
  rename(customers=n)


#customers using fibre optic with yearly contract
Only_yearlyContract<- Final_internet_usage1%>%
  group_by(Contract)%>%
  filter(Contract=="One year",InternetService=="Fiber optic")%>%
  dplyr::count(InternetService)
Only_yearlyContract

Only_yearlyContract%>%
  rename(customers=n)


# mean, maximum and minimum monthly and total charges
Final_internet_usage1%>%
  group_by(gender, PaymentMethod)%>%
  summarise(MeanTotalCharges = mean(TotalCharges),
            MinimumTotalCharge = min(TotalCharges),
            MaximumTotalCharge = max(TotalCharges))

Final_internet_usage1%>%
  group_by(gender, PaymentMethod)%>%
  summarise(MeanMonthlyCharges = mean(MonthlyCharges),
            MinimumMonthlyCharge = min(MonthlyCharges),
            MaximumMonthlyCharge = max(MonthlyCharges))




#########################################################################################
######## CALCULATING CHURN RATE

#install.packages("MASS")
#install.packages("ggthemes")
#install.packages("corrplot")
#install.packages("caret",dependencies = TRUE)
#install.packages("party")


# packages necessary for analysis.
library(caret)
library(plyr)
library(corrplot)
library(ggthemes)
library(MASS)
library(party)
library(randomForest)

churn <- Internet_Usage
str(churn)

#missing values
sapply(churn, function(x) sum(is.na(x)))

#remove 8 missing records
churn <- churn[complete.cases(churn), ]

#Re-code columns and changing them to factors
cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues
                   (churn[,cols_recode1][,i], 
                    from =c("No internet service"),to=c("No")))
}


churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))

########### grouping tenure into tenure groups in a different column #######
 
#checking the minimum and maximum tenure 
min(churn$tenure); max(churn$tenure)

group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}
churn$tenure_group <- sapply(churn$tenure,group_tenure)
churn$tenure_group <- as.factor(churn$tenure_group)
 glimpse(churn)

churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))
churn$Churn <- as.factor(mapvalues(churn$Churn,
                                   from=c("No", "Yes"),
                                   to=c("0","1")))
#Remove columns we don't need
churn$customerID <- NULL
churn$tenure.Month <- NULL



#Exploratory data analysis and feature selection
#Correlation between numerical values, how similar are the numeric values
numeric.var <- sapply(churn, is.numeric)
corr.matrix <- cor(churn[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

#They is a positive correlation between monthly charges and Total Charges,
# thus we use only one for analysis
churn$TotalCharges <- NULL
glimpse(churn)

#Logistic Regression
#First, we split the data into training and testing sets:
intrain<- createDataPartition(churn$Churn,p=0.8,list=FALSE)
# it mostly either 70/30 or 80/20 when training and testing a data
set.seed(2083)
training<- churn[intrain,]
testing<- churn[-intrain,]

#Fitting the Logistic Regression Model:
LogModel <- glm(Churn ~ .,family=binomial(link="logit"),data=training)# glm is used for logistics regression,
###meaning churn is a function of all the other columns in the data set, 
##thus we are using all the columns to analyze churn.
print(summary(LogModel))
## for the result, the more star assigned to a feature the more vital it is in churn analysis
## we can say per the results that Contact, paperless billing and tenure group are the 
#most vital for the analysis.


#Assessing the predictive ability of the Logistic Regression model
# testing the data to see its accuracy
#testing$Churn[testing$Churn=="No"] <- "0"
#testing$Churn[testing$Churn=="Yes"] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))# this shows a 79% accuracy

#Odds Ratio
## we are using the odd ratio to detect the the important columns
library(MASS)
exp(cbind(OR=coef(LogModel), confint(LogModel)))
## this confirms the analysis of the glm function


#Decision Tree
#Decision Tree visualization
#per the analysis we are focusing on only Contract ,tenure and paperless billing since these 
#feature strongly predict churn
#Convert variables used in model to factors
training$Contract <- factor(training$Contract)
training$tenure_group <- factor(training$tenure_group)
training$PaperlessBilling <- factor(training$PaperlessBilling)
tree <- ctree(Churn~Contract+tenure_group+PaperlessBilling, training)
plot(tree)


#Decision Tree Confusion Matrix
#Decision Tree Confusion Matrix
testing$Contract <- factor(testing$Contract)
testing$tenure_group <- factor(testing$tenure_group)
testing$PaperlessBilling <- factor(testing$PaperlessBilling)

pred_tree <- predict(tree, testing)
print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree, Actual = testing$Churn)

#Decision Tree Accuracy
p1 <- predict(tree, training)
tab1 <- table(Predicted = p1, Actual = training$Churn)
tab2 <- table(Predicted = pred_tree, Actual = testing$Churn)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))
#### shows a 77% accuracy on the decision tree


###### Create a new decision tree using different features
grid.newpage()
training$MonthlyCharges <- factor(training$MonthlyCharges)
training$PaymentMethod <- factor(training$PaymentMethod)
training$InternetService <- factor(training$InternetService)
tree2 <- ctree(Churn ~ MonthlyCharges + PaymentMethod + InternetService, training)
# Plot the new decision tree
plot(tree2)

#Decision tree 2 accuracy
levels(training$MonthlyCharges)
levels(training$PaymentMethod)
levels(training$InternetService)

# Convert features to a factor variable with the same levels as in the training data
testing$MonthlyCharges <- factor(testing$MonthlyCharges, levels = levels(training$MonthlyCharges))
testing$PaymentMethod <- factor(testing$PaymentMethod, levels = levels(training$PaymentMethod))
testing$InternetService <- factor(testing$InternetService, levels = levels(training$InternetService))

# Create a confusion matrix to evaluate the accuracy of the predictions
confusionMatrix(predicted, testing$Churn)

#### the decision tree accuracy is 73% and sensitivity 80%