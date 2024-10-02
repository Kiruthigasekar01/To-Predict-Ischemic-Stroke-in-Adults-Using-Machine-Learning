# To-Predict-Ischemic-Stroke-in-Adults-Using-Machine-Learning
#Stroke is the significant cause of death and disability worldwide and effective treatments depends upon the early identification. The use of machine learning algorithms for predicting the likelihood of an ischemic stroke in high-risk patients has shown potential. 

# Load required libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Set working directory and load the CSV file
setwd(dirname(file.choose()))
getwd()
Stroke_data <- read.csv("Stroke_data.csv", stringsAsFactors = FALSE)

# Print the structure of Stroke_data
str(Stroke_data)

# Check for missing values:
sum(is.na(Stroke_data))
colSums(is.na(Stroke_data))
colSums(Stroke_data == "N/A")

# Check for duplicate values
n_occur <- data.frame(table(Stroke_data$pid))
n_occur[n_occur$Freq > 1,]
sum(Stroke_data$gender == "Other")
sum(Stroke_data$smoking == "Unknown")

# Visualization
# Create and arrange multiple plots using grid.arrange
p1 <- Stroke_data %>%
  ggplot(aes(x = gender, fill = risk)) +
  geom_bar()
p2 <- Stroke_data %>%
  ggplot(aes(x = nhiss, fill = risk)) +
  geom_bar()
p3 <- Stroke_data %>%
  ggplot(aes(x = mrs, fill = risk)) +
  geom_bar()
p4 <- Stroke_data %>%
  ggplot(aes(x = systolic, fill = risk)) +
  geom_bar()
p5 <- Stroke_data %>%
  ggplot(aes(x = distolic, fill = risk)) +
  geom_bar()
p6 <- Stroke_data %>%
  ggplot(aes(x = glucose, fill = risk)) +
  geom_bar()
p7 <- Stroke_data %>%
  ggplot(aes(x = paralysis, fill = risk)) +
  geom_bar()
p8 <- Stroke_data %>%
  ggplot(aes(x = smoking, fill = risk)) +
  geom_bar()
p9 <- Stroke_data %>%
  ggplot(aes(x = cholestrol, fill = risk)) +
  geom_bar()
p10 <- Stroke_data %>%
  ggplot(aes(x = tos, fill = risk)) +
  geom_bar()
grid.arrange(grobs = list(p1, p2),
             ncol = 2,
             top = "Count of Levels for Each Factor"
)
grid.arrange(grobs = list(p3, p4),
             ncol = 2,
             top = "Count of Levels for Each Factor"
)
grid.arrange(grobs = list(p5, p6),
             ncol = 2,
             top = "Count of Levels for Each Factor"
)
grid.arrange(grobs = list(p7, p8),
             ncol = 2,
             top = "Count of Levels for Each Factor"
)
grid.arrange(grobs = list(p9, p10),
             ncol = 2,
             top = "Count of Levels for Each Factor"
)

# Create and arrange multiple plots using grid.arrange
dat_prop <- Stroke_data %>%
  group_by(gender) %>%
  summarise(prop = sum(risk == "1")/length(gender))
p1 <- dat_prop %>%
  ggplot(aes(x = gender, y = prop)) +
  geom_col(fill = "#00BFC4")
dat_prop <- Stroke_data %>%
  group_by(nhiss) %>%
  summarise(prop = sum(risk == "1")/length(nhiss))
p2 <- dat_prop %>%
  ggplot(aes(x = nhiss, y = prop)) +
  geom_col(fill = "#00BFC4")
dat_prop <- Stroke_data %>%
  group_by(mrs) %>%
  summarise(prop = sum(risk == "1")/length(mrs))
p3 <- dat_prop %>%
  ggplot(aes(x = mrs, y = prop)) +
  geom_col(fill = "#00BFC4")
dat_prop <- Stroke_data %>%
  group_by(systolic) %>%
  summarise(prop = sum(risk == "1")/length(systolic))
p4 <- dat_prop %>%
  ggplot(aes(x = systolic, y = prop)) +
  geom_col(fill = "#00BFC4")
dat_prop <- Stroke_data %>%
  group_by(distolic) %>%
  summarise(prop = sum(risk == "1")/length(distolic))
p5 <- dat_prop %>%
  ggplot(aes(x = distolic, y = prop)) +
  geom_col(fill = "#00BFC4")
dat_prop <- Stroke_data %>%
  group_by(glucose) %>%
  summarise(prop = sum(risk == "1")/length(glucose))
p6 <- dat_prop %>%
  ggplot(aes(x = glucose, y = prop)) +
  geom_col(fill = "#00BFC4")
dat_prop <- Stroke_data %>%
  group_by(paralysis) %>%
  summarise(prop = sum(risk == "1")/length(paralysis))
p7 <- dat_prop %>%
  ggplot(aes(x = paralysis, y = prop)) +
  geom_col(fill = "#00BFC4")
dat_prop <- Stroke_data %>%
  group_by(smoking) %>%
  summarise(prop = sum(risk == "1")/length(smoking))
p8 <- dat_prop %>%
  ggplot(aes(x = smoking, y = prop)) +
  geom_col(fill = "#00BFC4")
dat_prop <- Stroke_data %>%
  group_by(cholestrol) %>%
  summarise(prop = sum(risk == "1")/length(cholestrol))
p9 <- dat_prop %>%
  ggplot(aes(x = cholestrol, y = prop)) +
  geom_col(fill = "#00BFC4")
dat_prop <- Stroke_data %>%
  group_by(tos) %>%
  summarise(prop = sum(risk == "1")/length(tos))
p10 <- dat_prop %>%
  ggplot(aes(x = tos, y = prop)) +
  geom_col(fill = "#00BFC4")
grid.arrange(grobs = list(p1, p2),
             ncol = 2,
             top = "Proportion of Strokes_Risk for Each Factor"
)
grid.arrange(grobs = list(p3, p4),
             ncol = 2,
             top = "Proportion of Strokes_Risk for Each Factor"
)
grid.arrange(grobs = list(p5, p6),
             ncol = 2,
             top = "Proportion of Strokes_Risk for Each Factor"
)
grid.arrange(grobs = list(p7, p8),
             ncol = 2,
             top = "Proportion of Strokes_Risk for Each Factor"
)
grid.arrange(grobs = list(p9, p10),
             ncol = 2,
             top = "Proportion of Strokes_Risk for Each Factor"
)

# Create and arrange multiple plots using grid.arrange
p1 <- Stroke_data %>%
  ggplot(aes(x = gender, y = age, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p2 <- Stroke_data %>%
  ggplot(aes(x = nhiss, y = age, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p3 <- Stroke_data %>%
  ggplot(aes(x = mrs, y = age, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p4 <- Stroke_data %>%
  ggplot(aes(x = systolic, y = age, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p5 <- Stroke_data %>%
  ggplot(aes(x = distolic, y = age, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p6 <- Stroke_data %>%
  ggplot(aes(x = glucose, y = age, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p7 <- Stroke_data %>%
  ggplot(aes(x = paralysis, y = age, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p8 <- Stroke_data %>%
  ggplot(aes(x = smoking, y = age, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p9 <- Stroke_data %>%
  ggplot(aes(x = cholestrol, y = age, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p10 <- Stroke_data %>%
  ggplot(aes(x = tos, y = age, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
grid.arrange(grobs = list(p1, p2), 
             ncol = 2,
             top = "Risk and Age Across Factors"
)
grid.arrange(grobs = list(p3, p4), 
             ncol = 2,
             top = "Risk and Age Across Factors"
)
grid.arrange(grobs = list(p5, p6), 
             ncol = 2,
             top = "Risk and Age Across Factors"
)
grid.arrange(grobs = list(p7, p8), 
             ncol = 2,
             top = "Risk and Age Across Factors"
)
grid.arrange(grobs = list(p9, p10), 
             ncol = 2,
             top = "Risk and Age Across Factors"
)

# Create and arrange multiple plots using grid.arrange
p1 <- Stroke_data %>%
  ggplot(aes(x = gender, y = glucose, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p2 <- Stroke_data %>%
  ggplot(aes(x = nhiss, y = glucose, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p3 <- Stroke_data %>%
  ggplot(aes(x = mrs, y = glucose, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p4 <- Stroke_data %>%
  ggplot(aes(x = systolic, y = glucose, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p5 <- Stroke_data %>%
  ggplot(aes(x = distolic, y = glucose, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p6 <- Stroke_data %>%
  ggplot(aes(x = paralysis, y = glucose, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p7 <- Stroke_data %>%
  ggplot(aes(x = smoking, y = glucose, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p8 <- Stroke_data %>%
  ggplot(aes(x = tos, y = glucose, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
grid.arrange(grobs = list(p1, p2), 
             ncol = 2,
             top = "Risk and Glucose Level Across Factors"
)
grid.arrange(grobs = list(p3, p4), 
             ncol = 2,
             top = "Risk and Glucose Level Across Factors"
)
grid.arrange(grobs = list(p5, p6), 
             ncol = 2,
             top = "Risk and Glucose Level Across Factors"
)
grid.arrange(grobs = list(p7, p8), 
             ncol = 2,
             top = "Risk and Glucose Level Across Factors"
)

# Create and arrange multiple plots using grid.arrange
p1 <- Stroke_data %>%
  ggplot(aes(x = gender, y = bmi, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p2 <- Stroke_data %>%
  ggplot(aes(x = nhiss, y = bmi, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p3 <- Stroke_data %>%
  ggplot(aes(x = mrs, y = bmi, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p4 <- Stroke_data %>%
  ggplot(aes(x = systolic, y = bmi, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p5 <- Stroke_data %>%
  ggplot(aes(x = distolic, y = bmi, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p6 <- Stroke_data %>%
  ggplot(aes(x = glucose, y = bmi, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p7 <- Stroke_data %>%
  ggplot(aes(x = paralysis, y = bmi, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p8 <- Stroke_data %>%
  ggplot(aes(x = smoking, y = bmi, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p9 <- Stroke_data %>%
  ggplot(aes(x = cholestrol, y = bmi, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
p10 <- Stroke_data %>%
  ggplot(aes(x = tos, y = bmi, color = risk)) +
  geom_boxplot() +
  theme(legend.position="none")
grid.arrange(grobs = list(p1, p2), 
             ncol = 2,
             top = "Stroke_Risk and BMI Across Factors"
)
grid.arrange(grobs = list(p3, p4), 
             ncol = 2,
             top = "Stroke_Risk and BMI Across Factors"
)
grid.arrange(grobs = list(p5, p6), 
             ncol = 2,
             top = "Stroke_Risk and BMI Across Factors"
)
grid.arrange(grobs = list(p7, p8), 
             ncol = 2,
             top = "Stroke_Risk and BMI Across Factors"
)
grid.arrange(grobs = list(p9, p10), 
             ncol = 2,
             top = "Stroke_Risk and BMI Across Factors"
)

# Create and arrange multiple plots using grid.arrange
p1 <- Stroke_data %>%
  ggplot(aes(x = age, fill = risk)) +
  geom_density(alpha = 0.5) +
  theme(legend.position="none")
p2 <- Stroke_data %>%
  ggplot(aes(x = glucose, fill = risk)) +
  geom_density(alpha = 0.5) +
  theme(legend.position="none")
p3 <- Stroke_data %>%
  ggplot(aes(x = bmi, fill = risk)) +
  geom_density(alpha = 0.5) +
  theme(legend.position="none")
p4 <- Stroke_data %>%
  ggplot(aes(x = age, fill = risk)) +
  geom_histogram() +
  theme(legend.position="none")
p5 <- Stroke_data %>%
  ggplot(aes(x = glucose, fill = risk)) +
  geom_histogram() +
  theme(legend.position="none")
p6 <- Stroke_data %>%
  ggplot(aes(x = bmi, fill = risk)) +
  geom_histogram() +
  theme(legend.position="none")
grid.arrange(grobs = list(p1, p2, p3,
                          p4, p5, p6), 
             nrow = 2,
             top = "Distribution of Continuous Variables"
)

# Create a new data frame called ischemic_stroke with selected columns from Stroke_data
ischemic_stroke <- data.frame(Stroke_data$sn.no, Stroke_data$age, Stroke_data$nhiss,Stroke_data$mrs, Stroke_data$systolic, Stroke_data$distolic, Stroke_data$glucose,Stroke_data$paralysis, Stroke_data$smoking,Stroke_data$bmi, Stroke_data$cholestrol,Stroke_data$tos, Stroke_data$risk)

# Rename the column names in the ischemic_stroke data frame
colnames(ischemic_stroke) <- c("SN.No","Age","NHISS","MRS","Systolic","Distolic","Glucose","Paralysis","Smoking","BMI","Cholestrol","TOS","Risk")

# Display the structure of the ischemic_stroke data frame
str(ischemic_stroke)

# View the ischemic_stroke data frame
View(ischemic_stroke)

# Create a boxplot for the ischemic_stroke data frame
boxplot(ischemic_stroke,main="Boxplot for Stroke Prediction",xlab="Dependent and Independant Variables",ylab="count",col="Bisque")

# Define a function called replace_outlier that replaces extreme values with percentiles
replace_outlier <- function(x){
  for (i in which(sapply(x, is.numeric))) {
    quantiles <- quantile( x[,i], c(.05, .95 ), na.rm =TRUE)
    x[,i] = ifelse(x[,i] < quantiles[1] , quantiles[1], x[,i])
    x[,i] = ifelse(x[,i] > quantiles[2] , quantiles[2], x[,i])}
  x}

# Replace the outliers in the ischemic_stroke data frame using the replace_outlier function
ischemic_stroke = replace_outlier(ischemic_stroke)

# Create a boxplot for the ischemic_stroke data frame with outliers replaced
boxplot(ischemic_stroke, xlab="numerical values", ylab="Count", col="Bisque", main="Outliers Replaced :")


#Normalization (3 Methods Used):

#1st Method min-max scaling:
ischemic_stroke.mms<- apply(ischemic_stroke , MARGIN = 2, FUN = function(x) (x - min(x))/diff(range(x)))
boxplot(ischemic_stroke.mms, main= "Min-Max Scaling ",xlab="numerical values",ylab="count")

#2nd Method z-score:
ischemic_stroke.z1 <- apply(ischemic_stroke, MARGIN = 2, FUN = function(x) (x - mean(x))/sd(x))
ischemic_stroke.z2 <- apply(ischemic_stroke, MARGIN = 2, FUN = function(x) (x - mean(x))/(2*sd(x)))
boxplot(ischemic_stroke.z1,main= "Standard deviation 1",xlab="numerical values",ylab="count")
boxplot(ischemic_stroke.z2,main= "Standard deviation 2",xlab="numerical values",ylab="count")

#3rd Method soft Max Scaling:
library(DMwR2)
help(SoftMax)
sts <- apply(ischemic_stroke, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 1, mean(x), sd(x))))
boxplot (sts, main = "Soft Max, lambda = 1")
sts <- apply(ischemic_stroke, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 2, mean(x), sd(x))))
boxplot (sts, main = "Soft Max, lambda = 2")
sts <- apply(ischemic_stroke, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 3, mean(x), sd(x))))
boxplot (sts, main = "Soft Max, lambda = 3")
sts <- apply(ischemic_stroke, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 4, mean(x), sd(x))))
boxplot (sts, main = "Soft Max, lambda = 4")
sts <- apply(ischemic_stroke, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 5, mean(x), sd(x))))
boxplot (sts, main = "Soft Max, lambda = 5")


#Checking the Given Variables are Normally Distributed through Ks test:

#If norm > 0.05 then it's Normally Distributed.
ks.test(ischemic_stroke$SN.No,"pnorm", mean(ischemic_stroke$SN.No), sd(ischemic_stroke$SN.No))
ks.test(ischemic_stroke$Age,"pnorm", mean(ischemic_stroke$Age), sd(ischemic_stroke$Age))
ks.test(ischemic_stroke$NHISS,"pnorm", mean(ischemic_stroke$NHISS), sd(ischemic_stroke$NHISS))
ks.test(ischemic_stroke$Systolic,"pnorm", mean(ischemic_stroke$Systolic), sd(ischemic_stroke$Systolic))
ks.test(ischemic_stroke$Glucose,"pnorm", mean(ischemic_stroke$Glucose), sd(ischemic_stroke$Glucose))
ks.test(ischemic_stroke$Smoking,"pnorm", mean(ischemic_stroke$Smoking), sd(ischemic_stroke$Smoking))
ks.test(ischemic_stroke$Cholestrol,"pnorm", mean(ischemic_stroke$Cholestrol), sd(ischemic_stroke$Cholestrol))
ks.test(ischemic_stroke$Risk,"pnorm", mean(ischemic_stroke$Risk), sd(ischemic_stroke$Risk))

# Correlations among numeric variables
cor.matrix <- cor(ischemic_stroke, use = "pairwise.complete.obs", method = "pearson")
round(cor.matrix, digits = 2)
cor.df <- as.data.frame(cor.matrix)
View(cor.df)
round(cor.df,2)

#Co-relation Map
library(corrgram)
corrgram(ischemic_stroke, order=FALSE, cor.method = "pearson", lower.panel=panel.cor,
         upper.panel=panel.pie, text.panel=panel.txt, main="Patients with Ischemic Stroke (pearson correlation)")

#Partial correlation
library(ppcor)

#calculate partial correlation using Pearson
pcor.test(ischemic_stroke$Age,ischemic_stroke$Risk,ischemic_stroke$NHISS)
pcor.test(ischemic_stroke$Age,ischemic_stroke$NHISS,ischemic_stroke$Risk)

#Kaiser-Meyer-Olkin statistics: if overall MSA > 0.6, proceed to factor analysis
library(psych)
KMO(cor(ischemic_stroke))

#Calculate KMO statistic
KMO_result <- KMO(ischemic_stroke)

#Check if overall MSA is greater than 0.6
if(KMO_result$MSA > 0.6){
  #Proceed to factor analysis
  fa_result <- fa(ischemic_stroke)
  print(fa_result)
} else {
  print("Overall measure of sampling adequacy is not greater than 0.6. Factor analysis cannot be performed.")
}

# Test dependent variable for normality
# graphically
qqnorm(ischemic_stroke$Age, xlab = "Theoretical Quantiles: Age Factor in  Ischemic Stroke" )
qqline(ischemic_stroke$Age,col = 2) ## red color

# K-S test
ks.test(ischemic_stroke$Age, "pnorm", mean(ischemic_stroke$Age), "pnorm"(ischemic_stroke$Risk))

#Machine Learning Technique

#Setting seed and creating a sample for training and testing data
set.seed(1)
sample <- sample(nrow(ischemic_stroke), floor(0.75 * nrow(ischemic_stroke)))
train_data <- ischemic_stroke[sample,]
test_data <- ischemic_stroke[-sample,]

#Loading required libraries
library(randomForest)
library(e1071)
library(e1071)
library(glmnet)
library(rpart)
library(ggplot2)

#Random Forest
set.seed(123)
rf_model <- randomForest( Risk ~ ., data = train_data, importance = TRUE)
rf_model

#Predict on test data
rf_predictions <- predict(rf_model, newdata = test_data)
rf_confusion_matrix <- table(rf_predictions, test_data$Risk)
rf_confusion_matrix

#Gaussian Naive Bayes
nb_model <- naiveBayes(Risk ~ ., data = train_data)
nb_model

#Predict on test data
nb_predictions <- predict(nb_model, newdata = test_data)
nb_confusion_matrix <- table(nb_predictions, test_data$Risk)
nb_confusion_matrix

#Support Vector Machine
svm_model <- svm(Risk ~ ., data = train_data)
svm_model

#Predict on test data
svm_predictions <- predict(svm_model, newdata = test_data)
svm_confusion_matrix <- table(svm_predictions, test_data$Risk)
svm_confusion_matrix

#Logistic Regression
set.seed(123)
x <- model.matrix(Risk ~ ., data = train_data)[,-1]
y <- train_data$Risk
cv_model <- cv.glmnet(x, y, family = "multinomial")
lambda.min <- cv_model$lambda.min
logit_model <- glmnet(x, y, family = "multinomial", alpha = 1, lambda = lambda.min)
logit_model

#Predict on test data
logit_predictions <- predict(logit_model, newx = model.matrix(Risk ~ ., data = test_data)[,-1], type = "response")
logit_predictions[logit_predictions >= 0.5] <- 1
logit_predictions[logit_predictions < 0.5] <- 0

# Make sure both vectors have the same length
if (length(logit_predictions) != length(test_data$Risk)) {
  logit_predictions <- logit_predictions[1:length(test_data$Risk)]
}
logit_confusion_matrix <- table(logit_predictions, test_data$Risk)
logit_confusion_matrix

#Decision Tree
set.seed(123)
tree_model <- rpart(Risk ~ ., data = train_data, method = "class")
tree_model
plot(tree_model)
text(tree_model, use.n = TRUE, all = TRUE, cex = 0.8)
tree_predictions <- predict(tree_model, newdata = test_data, type = "class")
tree_confusion_matrix <- table(tree_predictions, test_data$Risk)
tree_confusion_matrix

#Comparison of all models
accuracy <- c(
  rf_acc = sum(diag(rf_confusion_matrix))/sum(rf_confusion_matrix),
  nb_acc = sum(diag(nb_confusion_matrix))/sum(nb_confusion_matrix),
  svm_acc = sum(diag(svm_confusion_matrix))/sum(svm_confusion_matrix),
  logit_acc = sum(diag(logit_confusion_matrix))/sum(logit_confusion_matrix),
  tree_acc = sum(diag(tree_confusion_matrix))/sum(tree_confusion_matrix)
)
accuracy

#Round the accuracy to 3 decimal points
accuracy <- round(accuracy, 3)
accuracy

#Plotting accuracy for all models

accuracy_df <- data.frame(model = names(accuracy), accuracy = accuracy)
ggplot(data = accuracy_df, aes(x = model, y = accuracy)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(accuracy, 3)), vjust = -0.5) +
  ggtitle("Accuracy of Different Machine Learning Models")

