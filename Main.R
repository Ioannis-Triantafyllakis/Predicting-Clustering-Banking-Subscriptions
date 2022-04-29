# Importing the libraries i will need
library(ggplot2)
library(ggpubr)
library(caret)
library(pROC)
library(mlbench)
library(dplyr)
library(binr)
library(Hmisc)
library(psych)
library(naivebayes)
library(e1071)
library(cluster)
library(purrr)
library(randomForest)


# Importing the dataset
data <- read.csv("C:\\Users\\user\\Dropbox\\My PC (DESKTOP-FQ14F0C)\\Desktop\\M.Sc\\Winter Semester\\Statistics for Business Analytics II\\Assignment_1_log_reg\\project I 2021-2022\\project_I_2021_2022.csv",
                 stringsAsFactors = TRUE)
  
# Taking a look at the data
str(data)

# Checking for missing values
sum(is.na(data))
sum(is.null(data))

# Converting columns to the right datatype

# Converting columns of "int" type to "num" type

# Getting a summary of the columns
summary(data)
data_clust <- data
# Here we construct a new dataframe with numerical and integer columns only
index <- sapply(data, class)!= "factor"                
data_num <- data[,index]

# Here we construct a new dataframe with categorical columns only
index <- sapply(data, class) == "factor"
data_cat <- data[,index]

# Exploratory analysis in numeric variables
par(mfrow=c(2,2)); 
hist(data_num[,1], main=names(data_num)[1], xlab="Age of customer", col="darkgreen")
hist(data_num[,2], main=names(data_num)[2], xlab="Duration of the call(in sec.)", col="darkgreen")
hist(data_num[,3], main=names(data_num)[3], xlab="No. of contacts", col="darkgreen")
hist(data_num[,4], main=names(data_num)[4], xlab="Days passed after last contact", col="darkgreen")

par(mfrow=c(2,2)); 
hist(data_num[,5], main=names(data_num)[5], xlab="No. of contacts before this campaign", col="darkgreen")
hist(data_num[,6], main=names(data_num)[6], xlab="Employment Variation Rate", col="darkgreen")
hist(data_num[,7], main=names(data_num)[7], xlab="Consumer Price Index", col="darkgreen")
hist(data_num[,8], main=names(data_num)[8], xlab="Consumer Confidence Index", col="darkgreen")

par(mfrow=c(1,2)); 
hist(data_num[,9], main=names(data_num)[9], xlab="Euribor 3-month interest rate", col="darkgreen")
hist(data_num[,10], main=names(data_num)[10], xlab="No. of people currently employed", col="darkgreen")

# Exploratory analysis in factor variables

# Plotting job and marital variables
a <- ggplot(data, aes(x=reorder(job, job, function(x)-length(x)))) +
  geom_bar(fill='darkgreen') +  labs(x='Job') + ggtitle("Job of Customer") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

b <- ggplot(data, aes(x=reorder(marital, marital, function(x)-length(x)))) +
  geom_bar(fill='darkgreen') +  labs(x='Marital Status') + ggtitle("Marital Status of Customer") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

figure_ab <- ggarrange(a, b,
                       labels = c("A", "B"),
                       ncol = 2, nrow = 1)
# Showing the plot of job and marital
figure_ab

# Plotting education and default variable
c <- ggplot(data, aes(x=reorder(education, education, function(x)-length(x)))) +
  geom_bar(fill='darkgreen') +  labs(x='Education') + ggtitle("Education level") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

d <- ggplot(data, aes(x=reorder(default, default, function(x)-length(x)))) +
  geom_bar(fill='darkgreen') +  labs(x='Default') + ggtitle("Credit in Default") 

figure_cd <- ggarrange(c, d,
                       labels = c("C", "D"),
                       ncol = 2, nrow = 1)
# Showing the plot of education and default 
figure_cd

# Plotting housing and loan variables
e <- ggplot(data, aes(x=reorder(housing, housing, function(x)-length(x)))) +
  geom_bar(fill='darkgreen') +  labs(x='Housing') + ggtitle("Housing") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

f <- ggplot(data, aes(x=reorder(loan, loan, function(x)-length(x)))) +
  geom_bar(fill='darkgreen') +  labs(x='Loan') + ggtitle("Loan") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

figure_ef <- ggarrange(e, f,
                       labels = c("E", "F"),
                       ncol = 2, nrow = 1)
# Showing the plots of housing and loan
figure_ef

# Plotting contact and month variables
g <- ggplot(data, aes(x=reorder(contact, contact, function(x)-length(x)))) +
  geom_bar(fill='darkgreen') +  labs(x='Contact') + ggtitle("Contact")

h <- ggplot(data, aes(x=reorder(month, month, function(x)-length(x)))) +
  geom_bar(fill='darkgreen') +  labs(x='Month') + ggtitle("Month") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

figure_gh <- ggarrange(g, h,
                       labels = c("G", "H"),
                       ncol = 2, nrow = 1)
# Showing the plot of contact and month
figure_gh

# Plotting day_of_week and poutcome variables
i <- ggplot(data, aes(x=reorder(day_of_week, day_of_week, function(x)-length(x)))) +
  geom_bar(fill='darkgreen') +  labs(x='Day') + ggtitle("Day of week contact") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

j <- ggplot(data, aes(x=reorder(poutcome, poutcome, function(x)-length(x)))) +
  geom_bar(fill='darkgreen') +  labs(x='Outcome') + ggtitle("Previous campaign outcome") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

figure_ij <- ggarrange(i, j,
                       labels = c("I", "J"),
                       ncol = 2, nrow = 1)
# Showing the plot of day_of_week and poutcome
figure_ij

# Plotting the SUBSCRIBED variable
figure_k <- ggplot(data, aes(x=reorder(SUBSCRIBED, SUBSCRIBED, function(x)-length(x)))) +
  geom_bar(fill='darkgreen') +  labs(x='Subscription outcome') + ggtitle("Subscribed") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

# Showing the plot of SUBSCRIBED
figure_k

#####################################################
############ CLASSIFICATION #########################
#####################################################

# Data Examination 
set.seed(1234)

# Here, i find that the 13th column (pdays) has  extremely low variance (close to zero), 
# thus it can not be included in the K-Nearest Neighbors later on. For this reason i will
# discard it.
x = nearZeroVar(data)
x
data <- data[ ,-nearZeroVar(data)]
 

# Below, i recode the factor variables into numeric because K-Nearest Neighbor can not handle
# non-numeric variables

# Recoding "job" variable
data <- data %>%  # Recoding "job" variable
  mutate(job = case_when(
    job == 'admin.' ~ '1',
    job == 'blue-collar' ~ '2',
    job == 'entrepreneur' ~ '3',
    job == 'housemaid' ~ '4',
    job == 'management' ~ '5',
    job == 'retired' ~ '6',
    job == 'self-employed' ~ '7',
    job == 'services' ~ '8',
    job == 'student' ~ '9',
    job == 'technician' ~ '10',
    job == 'unemployed' ~ '11',
    job == 'unknown' ~ '12'
  ))

# Recoding "marital" variable
data <- data %>%  
  mutate(marital = case_when(
    marital == 'divorced' ~ '1',
    marital == 'married' ~ '2',
    marital == 'single' ~ '3',
    marital == 'unknown' ~ '4'
  ))

# Recoding "education" variable
data <- data %>% 
  mutate(education = case_when(
    education == "basic.4y" ~ '1',
    education == "basic.6y" ~ '2',
    education == "basic.9y" ~ '3',
    education == "high.school" ~ '4',
    education == "illiterate" ~ '5',
    education == "professional.course" ~ '6',
    education == "university.degree" ~ '7',
    education == "unknown" ~ '8'
  ))

# Recoding "default" variable
data <- data %>%
  mutate(default = case_when(
    default == "no" ~ '1',
    default == "yes" ~ '2',
    default == "unknown" ~ '3'
  ))

# Recoding "housing" variable
data <- data %>%
  mutate(housing = case_when(
    housing == "no" ~ '1',
    housing == "yes" ~ '2',
    housing == "unknown" ~ '3'
  ))

# Recoding "loan" variable
data <- data %>%
  mutate(loan = case_when(
    loan == "no" ~ '1',
    loan == "yes" ~ '2',
    loan == "unknown" ~ '3'
  ))
    
# Recoding "contact" variable
data <- data %>%
  mutate(contact = case_when(
    contact == "cellular" ~ '1',
    contact == "telephone" ~ '2'
  ))

# Recoding "month" variable
data <- data %>%
  mutate(month = case_when(
    month == "mar" ~ '1',
    month == "apr" ~ '2',
    month == "may" ~ '3',
    month == "jun" ~ '4',
    month == "jul" ~ '5',
    month == "aug" ~ '6',
    month == "sep" ~ '7',
    month == "oct" ~ '8',
    month == "nov" ~ '9',
    month == "dec" ~ '10'
  ))

# Recoding "day_of_week" variable
data <- data %>%
  mutate(day_of_week = case_when(
    day_of_week == "mon" ~ '1',
    day_of_week == "tue" ~ '2',
    day_of_week == "wed" ~ '3',
    day_of_week == "thu" ~ '4',
    day_of_week == "fri" ~ '5'
  ))


# Recoding "poutcome" variable
data <- data %>%
  mutate(poutcome = case_when(
    poutcome == "failure" ~ '1',
    poutcome == "nonexistent" ~ '2',
    poutcome == "success" ~ '3'
  ))


# We can see that the variables we recoded are now of "chr" data type
# I will now convert columns of "chr" type to "num" type
data$job <- as.numeric(data$job)
data$marital <- as.numeric(data$marital)
data$education <- as.numeric(data$education)
data$default <- as.numeric(data$default)
data$housing <- as.numeric(data$housing)
data$loan <- as.numeric(data$loan)
data$contact <- as.numeric(data$contact)
data$month <- as.numeric(data$month)
data$day_of_week <- as.numeric(data$day_of_week)
data$poutcome <- as.numeric(data$poutcome)


# Data Partition
ind <- sample(3, nrow(data), replace =TRUE, prob = c(0.6, 0.3, 0.1))

training <- data[ind == 1, ]
test <- data[ind == 2, ]
valid <- data[ind == 3, ]

########## Random Forest ##########
rf_model <- randomForest(SUBSCRIBED ~., data = training)

pred_rf <- predict(rf_model, test)
pred_rf_val <- predict(rf_model, valid)

# Confusion Matrix for the random forest model
conf_matr_test_rf <- table(Observed = test$SUBSCRIBED,Predicted = pred_rf)
conf_matr_val_rf <- table(Observed = valid$SUBSCRIBED,Predicted = pred_rf_val)

# Accuracy of random forest model
accuracy_rf_test <- sum(diag(conf_matr_test_rf))/sum(conf_matr_test_rf)
accuracy_rf_val <- sum(diag(conf_matr_val_rf))/sum(conf_matr_val_rf)


######## K-Nearest Neighbor ###########

# trainControl() is from "caret" package
trControl <- trainControl(method = "repeatedcv",
                          number = 10, # This is the number of iterations
                          repeats = 3)
# When the argument "method" is equal to "repeatedcv", the algorithm conducts repeated
# K-Fold cross-validation, and when "repeats" argument is equal to 3, it means that this
# K-Fold Cross-Validation will occur 3 times.

knn_model <- train(SUBSCRIBED ~ ., # I train a model with all independent variables 
             data =training,
             method = 'knn',
             tuneGrid = expand.grid(k = c(10, 30, 40, 60, 80, 100, 200)), # The algorithm evaluates the model for every integer between 1 and 20
             trControl =trControl,
             preProc =c("center", "scale") 
             )
# In the "preProc" argument, i Standardize the data, meaning that every variable now
# will have a mean value of 0 and a standard deviation of 1. Rescaling reduces the 
# influence of extreme values on K-NN distance function.
# NOTE: Distance is calculated by the Euclidian Distance Formula.


# Model Performance
knn_model
#plot(fit)
#varImp(fit)

# Test data
pred_knn <- predict(knn_model, newdata =test)
# Confusion matrix for in-sample predictions
conf_matr_test_knn <- table(test$SUBSCRIBED, pred_knn)

# Validation data
pred_knn_val <- predict(knn_model, newdata =valid)
# Confusion matrix for out-of sample predictions
conf_matr_val_knn <- table(valid$SUBSCRIBED, pred_knn_val)

conf_matr_test_knn  
conf_matr_val_knn

accuracy_knn_test <- sum(diag(conf_matr_test_knn))/sum(conf_matr_test_knn) # Accuracy on test data
accuracy_knn_val <- sum(diag(conf_matr_val_knn))/sum(conf_matr_val_knn) # Accurace on validation data

# Printing the above
accuracy_knn_test
accuracy_knn_val

######## Naive Bayes Classifier ########

nb_model <- naive_bayes(SUBSCRIBED ~., data = training, laplace = 1)
nb_model
plot(nb_model)

pred_nb <- predict(nb_model, test)

# Confusion matrix for test data
conf_matr_test_nb <- table(pred_nb, test$SUBSCRIBED)
conf_matr_test_nb

# Accuracy of predictions in test data
accuracy_nb_test <- sum(diag(conf_matr_test_nb))/sum(conf_matr_test_nb) # Accuracy on test data
accuracy_nb_test

pred_nb_val <- predict(nb_model, valid)
# Confusion matrix for validation data
conf_matr_val_nb <- table(pred_nb_val, valid$SUBSCRIBED)
conf_matr_val_nb

# Accuracy of predictions in validation data
accuracy_nb_val <- sum(diag(conf_matr_val_nb))/sum(conf_matr_val_nb)
accuracy_nb_val


######## Support Vector Machines ########

svm_model <- svm(SUBSCRIBED ~ .,
                 data = training,
                 type = "C-classification",                 
                 kernel = "radial", 
                 scale = TRUE)
summary(svm_model)

# Confusion Matrix for test data
pred_svm <- predict(svm_model, test)
conf_matr_test_svm <- table(Predicted = pred_svm, Actual = test$SUBSCRIBED)
conf_matr_test_svm

accuracy_svm_test <- sum(diag(conf_matr_test_svm)/sum(conf_matr_test_svm))

# Confusion Matrix for valid data
pred_svm_val <- predict(svm_model, valid)
conf_matr_val_svm <- table(Predicted = pred_svm_val, Actual = valid$SUBSCRIBED)
conf_matr_val_svm

accuracy_svm_val <- sum(diag(conf_matr_val_svm)/sum(conf_matr_val_svm))


#####################################################
################ CLUSTERING #########################
#####################################################


data_clust$SUBSCRIBED <- NULL
data_clust$contact <- NULL
data_clust$month  <- NULL
data_clust$day_of_week <- NULL
data_clust$duration <- NULL
data_clust$emp.var.rate <- NULL
data_clust$cons.price.idx <- NULL
data_clust$euribor3m <- NULL
data_clust$nr.employed <- NULL
data_clust$cons.conf.idx <- NULL

str(data_clust)

# Trying a sample from data only
data_sample <- data_clust[1:15000, ]

# Creating a Gower Distance Matrix
gower_dissimilarity_mtrx <- daisy(data_sample, metric=c("gower"), stand =TRUE)

dist <- gower_dissimilarity_mtrx


# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- pam(dist, k, diss = TRUE)
  km.res$silinfo$avg.width
}

k.values <- 2:5

# extract avg silhouette for 2-5 clusters
set.seed(123)
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes", 
     main = "Average Silhouette vs. No of Clusters")


# Implementing clustering with 3 clusters
set.seed(1234)
pam_final <- pam(dist, 3, diss = TRUE)

# Average silhoutte width of observations when clustering with 3 clusters
pam_final$silinfo$avg.width

plot(silhouette(pam_final$cluster, dist), border=NA)


data_clust[pam_final$medoids, ]
