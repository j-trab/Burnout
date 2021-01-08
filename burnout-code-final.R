#CAPSTONE CYO. BURNOUT IN THE WORPLACE

################################################################################
#IMPORTING DATASET 

#Identify data file
filename <- "train.csv"
dir <- "/Users/jacopotrabona/Downloads/burnout"
fullpath <- file.path(dir, filename)

#Copy data file into project directory
file.copy(fullpath, "train.csv")

#Load data set 
if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

dat <- read_csv("train.csv")

################################################################################
#REQURED PACKAGES

#Install required packages 
if(!require(caret)) install.packages("caret")
if(!require(skimr)) install.packages("skimr")
if(!require(hrbrthemes)) install.packages("hrbrthemes")
if(!require(ggcorplot)) install.packages("ggcorplot")
if(!require(RANN)) install.packages("RANN")
if(!require(gam)) install.packages("gam")
if(!require(randomForest)) install.packages("randomForest")

#Load required packages
library(caret)
library(skimr)
library(hrbrthemes)
library(ggcorrplot)
library(RANN)
library(randomForest)
library(gam)

################################################################################
#PRELIMINARY DATA EXPLORATION AND DATA CLEANSING

#Structure of the dataset
str(dat)

#Head of the dataset
head(dat)

#Dataset dimensions
dim(dat)

#Identify NAs
is.na(dat)

#Number of NAs
sum(is.na(dat))

#NAs are 20.31648 % of our data 
(4622 * 100) / 22750

#Number of NAs across columns
colSums(is.na(dat))

#Remove observations with NAs in the Burn Rate column
newdat <- dat[!is.na(dat$`Burn Rate`),]

#Number of NAs in newdat 
sum(is.na(newdat))

#NAs are 14.90336 % of data
(3223 * 100) / 21626

#Columns with NAs
colSums(is.na(newdat))

#Number of observations with NAs in both Resource Allocation and Mental Fatigue 
is.na(newdat) %>% 
  as.data.frame() %>% 
  filter(`Resource Allocation` == TRUE & `Mental Fatigue Score` == TRUE) %>%
  nrow()

#Observations still containing missing values are approx. 14%
((3223 - 187) * 100) / 21626

#Correlation between Burn Rate and Mental Fatigue Score
cor(newdat$`Mental Fatigue Score`, newdat$`Burn Rate`, use = "complete.obs")

#Correlation between Burn Rate and Resource Allocation
cor(newdat$`Resource Allocation`, newdat$`Burn Rate`, use = "complete.obs")

#Omit row containing missing values 
newdat <- na.omit(newdat)

#Check for presence of NAs
sum(is.na(newdat))

################################################################################
#EXPLORATORY DATA ANALYSIS (EDA)

#Dataset summary statistics 
summary(newdat) 

skimr::skim(newdat)

#Burn Rate distribution (0.1)
newdat %>% ggplot(aes(`Burn Rate`)) +
  geom_histogram(binwidth = 0.1, colour = "black", alpha = 0.7) +
  labs(title = "Burn Rate Distirbution") +
  theme_ipsum()

#Burn Rate distribution (0.01)
newdat %>% ggplot(aes(`Burn Rate`)) +
  geom_histogram(binwidth = 0.01, colour = "black", alpha = 0.6) +
  labs(title = "Burn Rate Distirbution") +
  theme_ipsum()

##################
#Study the minimum

#Filter and study variables for Burn Rate = 0
newdat %>% filter(`Burn Rate` <= 0.1) %>% summary()

#Skim summary 
newdat %>% filter(`Burn Rate` <= 0.1) %>% skimr::skim(.)

#Mental Fatigue Distribution for the minimum 
newdat %>% filter(`Burn Rate` <= 0.1) %>% 
  ggplot(aes(`Mental Fatigue Score`)) +
  geom_histogram(binwidth = 0.1, colour = "black", alpha = 0.7) +
  theme_ipsum()

#Designation Distribution for the minimum 
newdat %>% filter(`Burn Rate` <= 0.1) %>% 
  ggplot(aes(Designation)) +
  geom_histogram(binwidth = 0.1, colour = "black", alpha = 0.7) +
  theme_ipsum()


##################
#Study the maximum

#Filter and study variables for Burn Rate = 1
newdat %>% filter(`Burn Rate` >= 0.9) %>% summary()

#Skim summary 
newdat %>% filter(`Burn Rate` >= 0.9) %>% skim() 

#Mental Fatigue Distribution for the maximum
newdat %>% filter(`Burn Rate` >= 0.9) %>% 
  ggplot(aes(`Mental Fatigue Score`)) +
  geom_histogram(binwidth = 0.1, colour = "black", alpha = 0.7) +
  theme_ipsum()

#Designation Distribution for the maximum
newdat %>% filter(`Burn Rate` >= 0.9) %>% 
  ggplot(aes(Designation)) +
  geom_histogram(binwidth = 0.1, colour = "black", alpha = 0.7) +
  theme_ipsum()

#Resource Allocation Distirbution for the maximum 
newdat %>% filter(`Burn Rate` >= 0.9) %>% 
  ggplot(aes(`Resource Allocation`)) +
  geom_histogram(binwidth = 0.1, colour = "black", alpha = 0.7) +
  theme_ipsum()

#Gender Barplot
newdat %>% filter(`Burn Rate` >= 0.9) %>% 
  ggplot(aes(Gender)) +
  geom_bar(alpha = 0.7) +
  theme_ipsum()

#WFH Barplot
newdat %>% filter(`Burn Rate` >= 0.9) %>% 
  ggplot(aes(`WFH Setup Available`)) +
  geom_bar(alpha = 0.7) +
  theme_ipsum()

###############################
#Relationships between variables 

#Correlations between continuous variables
correlations <- cor(newdat[,6:9], use = "pairwise.complete", 
                    method = "spearman")

#Correlations Plot
ggcorrplot(corr = correlations) +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, size = 9),
        axis.text.y = element_text(hjust = 1, vjust = 1, size = 9),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

#Mental Fatigue Score
newdat %>% ggplot(aes(`Mental Fatigue Score`, `Burn Rate`)) +
  geom_boxplot(alpha=0.5) +
  geom_jitter(color="black", size = 0.4, alpha = 0.05) +
  theme_ipsum() +
  scale_fill_brewer(palette="Dark1") +
  theme(legend.position = "none") 


#Resource Allocation
newdat %>% ggplot(aes(`Resource Allocation`, `Burn Rate`)) +
  geom_boxplot(alpha=0.5) +
  geom_jitter(color="black", size = 0.4, alpha = 0.05) +
  theme_ipsum() +
  scale_fill_brewer(palette="Dark1") +
  theme(legend.position = "none")


#Designation 
newdat %>% ggplot(aes(Designation, `Burn Rate`)) +
  geom_boxplot(alpha=0.5) +
  geom_jitter(color="black", size = 0.4, alpha = 0.05) +
  theme_ipsum() +
  scale_fill_brewer(palette="Dark1") +
  theme(legend.position = "none") 

#Gender 
newdat %>% ggplot(aes(Gender, `Burn Rate`, fill = Gender)) +
  geom_boxplot(alpha=0.5) +
  geom_jitter(color="black", size = 0.4, alpha = 0.05) +
  theme_ipsum() +
  scale_fill_brewer(palette="Dark1") +
  theme(legend.position = "none") 

#Company Type
newdat %>% ggplot(aes(`Company Type`, `Burn Rate`, fill = `Company Type`)) +
  geom_boxplot(alpha=0.5) +
  geom_jitter(color="black", size = 0.4, alpha = 0.05) +
  theme_ipsum() +
  scale_fill_brewer(palette="Dark3") +
  theme(legend.position = "none") 

#WHF Setup
newdat %>% ggplot(aes(`WFH Setup Available`, `Burn Rate`, 
                      fill = `WFH Setup Available`)) +
  geom_boxplot(alpha=0.5) +
  geom_jitter(color="black", size = 0.4, alpha = 0.05) +
  theme_ipsum() +
  scale_fill_brewer(palette="Dark3") +
  theme(legend.position = "none") 

#####################################
#Assemble boxplots in the same layout 

#Gender 
gender_bxp <- ggboxplot(newdat, x = "Gender", y = "Burn Rate",
                        color = "Gender", palette = "jco") + 
  theme_ipsum(base_size = 8, caption_size = 6) + theme(legend.position = "none")
 
gender_bxp

#Company type 
company_bxp <- ggboxplot(newdat, x = "Company Type", y = "Burn Rate",
                        color = "Company Type", palette = "jco") + 
  theme_ipsum(base_size = 8, caption_size = 6) + theme(legend.position = "none")

company_bxp

#WFH
WFH_bxp <- ggboxplot(newdat, x = "WFH Setup Available", y = "Burn Rate",
                        color = "WFH Setup Available", palette = "jco") + 
  theme_ipsum(base_size = 8, caption_size = 6) + theme(legend.position = "none")

WFH_bxp

#Assemble
ggarrange(gender_bxp, company_bxp, WFH_bxp + theme_ipsum(base_size = 5, caption_size = 6), legend = "none",
          ncol = 3, nrow = 1, label.y = c(1, "none", "none"))

help("theme_ipsum")
################
#Date of Joining 
newdat %>% ggplot(aes(`Date of Joining`, `Burn Rate`)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  theme_ipsum()

#Date of Joining means
newdat %>% group_by(`Date of Joining`) %>%
  summarise(Mean_Burn_Rate = mean(`Burn Rate`)) %>%
  ggplot(aes(`Date of Joining`, Mean_Burn_Rate)) +
  geom_point() +
  geom_smooth() +
  theme_ipsum()

#Date of Joining Rate 
range(newdat$`Date of Joining`)


################################################################################
#DATA FORMATTING

#Drop Id and Date of Joining variables
newdat_good <- newdat[,3:9] %>% as.data.frame()

#Re-code dataframe names
newnames <- c("Gender", "Company_Type",
              "WFH_Setup_Available", "Designation", "Resource_Allocation",
              "Mental_Fatigue_Score", "Burn_Rate")

oldnames <- c("Gender", "Company Type",
             "WFH Setup Available", "Designation", "Resource Allocation",
             "Mental Fatigue Score", "Burn Rate")

t <- newdat_good %>% 
  rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames)

################################################################################
#DATA SPLITTING

#Set.seed
set.seed(1234, sample.kind = "Rounding")

#Create test index for data splitting
tst_index <- createDataPartition(t$Burn_Rate, times = 1, p = 0.2, list = FALSE)

#Create train set
train_set <- t %>% slice(-tst_index)

#Create test set
test_set <- t %>% slice(tst_index)

################################################################################
#CONVERT TO FACTOR

#Re-code categorical variables

#Show columns class
sapply(train_set, class)

#Convert characters to factors (train set)
temp <- map_df(train_set[,1:3], as.factor) 

train_set[,1:3] <- temp[,1:3]

#Convert characters to factors (test set)
temp1 <- map_df(test_set[,1:3], as.factor)

test_set[,1:3] <- temp1[,1:3]

################################################################################
#MODELS

###################
#Linear Regression

#Set seed
set.seed(1234, sample.kind = "Rounding")

#Specify train controls
control <- trainControl(method = "repeatedcv", p = .2, repeats = 4, number = 10)

#Train linear model
lm_fit <- train(Burn_Rate ~ Mental_Fatigue_Score, method = "lm", 
                preProcess = c("center", "scale"), data = train_set, 
                trControl = control)

#Predict with linear model
lm_pred <- predict(lm_fit, test_set)

#Linear model RMSE
RMSE(lm_pred, test_set$Burn_Rate)

#Linear model CV results
lm_fit$results

#Create results table
results <- data.frame(Method = "1 Linear Regression", Tune = "/",
                      RMSE_CV = 0.06508212, RMSE_TEST = 0.06495063)

results %>% knitr::kable()

###############################
#Multivariate Linear Regression

#Set seed
set.seed(1234, sample.kind = "Rounding")

#Specify train controls
control <- trainControl(method = "repeatedcv", p = .2, repeats = 4, number = 10)

#Train lm multivariate
lm_fit_multivariate <- train(Burn_Rate ~ ., method = "lm", 
                             preProcess = c("center", "scale"), 
                             data = train_set, trControl = control)

#Predict lm mutivariate  fit 
lm_pred_multivariate <- predict(lm_fit_multivariate, test_set)

#Lm multivariate RMSE 
RMSE(lm_pred_multivariate, test_set$Burn_Rate)

#CV results
lm_fit_multivariate$results

#Add row to results table
results <- results %>% add_row(Method = "2 Multivariate Linear Regression",
                               Tune = "/", RMSE_CV = 0.05576466, 
                               RMSE_TEST = 0.05564016) 

results %>% knitr::kable()


####################
#K-nearest Neighbors

###################
#KNN (Default Tune)

#Set seed
set.seed(1234, sample.kind = "Rounding")

#Train KNN
knn_fit <- train(Burn_Rate ~ ., method = "knn", data = train_set)

#Predict with KNN 
knn_pred <- predict(knn_fit, test_set)

#KNN RMSE
RMSE(knn_pred, test_set$Burn_Rate)

#Access model fit 
knn_fit

#Plot k results
ggplot(knn_fit, highlight = TRUE) +
  theme_ipsum()

#CV results
knn_fit$results

#Add row to results table
results <- results %>% add_row(Method = "3 K-nearest Neighbors",
                               Tune = "K = 9", RMSE_CV = 0.05724738,
                               RMSE_TEST = 0.05423132) 
                               
results %>% knitr::kable()



##############
#Optimized KNN

#Set seed
set.seed(1234, sample.kind = "Rounding")

#Specify train controls
control <- trainControl(method = "repeatedcv", p = .2, repeats = 4, number = 10)

#Specify tuning parameters
tunegrid <- data.frame(k = 8:170)

#Train KNN 2nd
knn_fit2 <- train(Burn_Rate ~ ., method = "knn", data = train_set, 
                  tuneGrid = tunegrid, trControl=control)

#Predict with KNN 2nd 
knn_pred2 <- predict(knn_fit2, test_set)

#KNN 2nd RMSE 
RMSE(knn_pred2, test_set$Burn_Rate)

#Access model results
knn_fit2$results

knn_fit2$bestTune

knn_fit2$finalModel

#Plot k results
ggplot(knn_fit2, highlight = TRUE) +
  theme_ipsum()

#Add row to results table
results <- results %>% add_row(Method = "4 K-nearest Neighbors (Optimized)",
                               Tune = "K = 91", RMSE_CV = 0.05453934 ,
                               RMSE_TEST = 0.05438856)

results %>% knitr::kable()

####################################
#Locally Weighted Regression (loess)

#Specify train controls
control <- trainControl(method = "repeatedcv", p = .2, repeats = 4, number = 10)

#Specify tuning parameters
tunegrid <- expand.grid(span = c(0.2, 0.3, 0.4, 0.5, 0.6), degree = 1)

#Set seed
set.seed(1234, sample.kind = "Rounding")

#Loess fit
loess_fit <- train(Burn_Rate ~ ., method = "gamLoess", tuneGrid = tunegrid, 
                   preProcess = c("center", "scale"), 
                   trControl = control, data = train_set)
#Predict with loess
loess_pred <- predict(loess_fit, test_set)

#Loess RMSE
RMSE(loess_pred, test_set$Burn_Rate)

#Plot tuning results
ggplot(loess_fit, highlight = TRUE) +
  theme_ipsum()

#Access fit results
loess_fit$resample

loess_fit$results

loess_fit

#Add row to results table
results <- results %>% add_row(Method = "5 Loess",
                               Tune = "Span = 0.2", RMSE_CV = 0.05454619,
                               RMSE_TEST = 0.0543843)

results %>% knitr::kable()

####################
#Random Forests (rf)

#Set.seed
set.seed(1234, sample.kind = "Rounding")

#Train rf
rf_fit <- train(Burn_Rate ~ ., method = "rf", data = train_set)

#Predict with rf 
rf_pred <- predict(rf_fit, test_set)

#rf RMSE
RMSE(rf_pred, test_set$Burn_Rate)

#Add row to results table
results <- results %>% add_row(Method = "6 Random Forests", 
                               Tune = "Mtry = 2, Ntree = 500", 
                               RMSE_CV = 0.05537689, RMSE_TEST = 0.05502211) 

results %>% knitr::kable()

#Access model results
rf_fit$results

rf_fit$resample

rf_fit$bestTune

rf_fit$finalModel

#Access model's variable importance
varImp(rf_fit, scale = FALSE)

#Plot mtryies against RMSEs
ggplot(rf_fit, highlight = TRUE) +
  theme_ipsum()

########################
#Optimized Random Forest

#Define train control
control <- trainControl(method = "repeatedcv", p = .2, repeats = 4, number = 10,
                        search = "grid")

#Define train grid
tunegrid <- data.frame(mtry = 1:6)

#Set.seed
set.seed(1234, sample.kind = "Rounding")

#Train rf 2nd
rf_fit_tune <- train(Burn_Rate ~ ., method = "rf", 
                 data = train_set, ntree = 1000, tuneGrid = tunegrid, 
                 trControl=control)

#Predict with rf 2nd 
rf_pred_tune <- predict(rf_fit_tune, test_set)

#rf 2nd RMSE
RMSE(rf_pred_tune, test_set$Burn_Rate)

#Add entry to results table
results <- results %>% add_row(Method = "7 Random Forests", 
                               Tune = "Mtry = 3, Ntree = 1000", 
                               RMSE_CV = 0.05435957, RMSE_TEST = 0.0537501) 

#Plot mtryies vs RMSEs
ggplot(rf_fit_tune, highlight = TRUE) +
  theme_ipsum()

#Access results
rf_fit_tune$finalModel

rf_fit_tune$results

rf_fit_tune$modelInfo

#Access variable importance
varImp(rf_fit_tune, scale = FALSE)

# Format font for rbase plot
quartzFonts(avenir = c("Avenir Book", "Avenir Black", "Avenir Book Oblique", 
                        "Avenir Black Oblique"))

par(family = 'avenir')

plot(rf_fit_tune$finalModel) 

##########
#ENSEMBLES

#Create ensemble functions

ensemble1 <- function(ts){
  (predict(rf_fit_tune, ts) + 
     predict(knn_fit2, ts) + 
     predict(loess_fit, ts)) / 3
}

ensemble2 <- function(ts){
  (predict(rf_fit_tune, ts) + 
     predict(knn_fit2, ts) + 
     predict(lm_fit_multivariate, ts) +
     predict(loess_fit, ts)) / 4
}

ensemble3 <- function(ts){
  (predict(rf_fit_tune, ts) + 
     predict(knn_fit2, ts) + 
     predict(lm_fit, ts) +
     predict(lm_fit_multivariate, ts) +
     predict(loess_fit, ts)) / 5
}

#Create ensemble list 
ensembles_list <- list(ensemble1, ensemble2, ensemble3)


#Create repeated (4times) 10 fold cross validation function for our ensembles
repeatedcv_ensembles <- function(ensemblef) {
  mean(replicate(4, {
    
    folds <- createFolds(train_set$Burn_Rate, k = 10, list = TRUE, 
                         returnTrain = TRUE)
    res <- vector("numeric", 10)
    for (i in 1:10) {
      tr <- train_set[folds[[i]],]
      ts <- train_set[-folds[[i]],]
      
      errors <- RMSE(ensemblef(ts), ts[,7]) 
      res[i] <- errors
      
    }
    mean(res)
  }
  )
  )
}

#Set.seed
set.seed(1234, sample.kind = "Rounding")

#Run sapply()
sapply(ensembles_list, repeatedcv_ensembles)

#Re-run ensemble one on the test set
ensamble1(test_set) %>% RMSE(., test_set$Burn_Rate)

#Result table
results <- results %>% add_row(Method = "8 Ensemble", 
                               Tune = "Knn + Rf + Loess", 
                               RMSE_CV = 0.05046548, RMSE_TEST = 0.05296494)

################################################################################
#PREDICT MENTAL FATIGUE SCORE

#Edit train set
train_set2nd <- train_set %>% select(! Burn_Rate)

####
#KNN
#Train KNN
knn_fit_Mental <- train(Mental_Fatigue_Score ~ ., method = "knn", 
                        data = train_set2nd, tuneGrid = data.frame(k = 4:80),
                        trcontrol = trainControl(method = "cv", number = 10))

#Predict with KNN 
knn_pred_Mental <- predict(knn_fit_Mental, test_set)

#KNN RMSE
RMSE(knn_pred_Mental, test_set$Mental_Fatigue_Score)
1.129071









