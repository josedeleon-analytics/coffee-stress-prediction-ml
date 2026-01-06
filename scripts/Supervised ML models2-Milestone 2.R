# Coffee and Health Data Analysis
# Looking at how coffee consumption relates to various health metrics

# --- SECTION 1: Setup ---
# Install these if you don't have them already
# install.packages("pheatmap")
# install.packages("RColorBrewer")
# install.packages("corrplot")
# install.packages("gridExtra")

# Load libraries
library(dplyr)          # for data wrangling
library(ggplot2)        # for plots
library(RColorBrewer)   # nice colors
library(pheatmap)       # heatmaps
library(corrplot)       # correlation plots
library(gridExtra)      # arrange multiple plots

# --- SECTION 2: Load Data ---
# Reading the dataset
coffee <- read.csv("synthetic_coffee_health_10000.csv")

# --- SECTION 3: Check for Missing Data ---
# See if any NA exists
any(is.na(coffee))

# Count total NA values
sum(is.na(coffee))

# Count NA per column
colSums(is.na(coffee))

# --- SECTION 4: Feature Engineering ---
# Create new numeric variables from categorical ones
# Need these for correlations later
coffee <- coffee %>%
  mutate(
    # Converting sleep quality to numbers (0 is good, 2 is poor)
    Sleep_Quality_Num = case_when(
      Sleep_Quality == "Excellent" ~ 0,
      Sleep_Quality == "Good" ~ 1,
      Sleep_Quality == "Fair" ~ 2,
      Sleep_Quality == "Poor" ~ 3,
      TRUE ~ NA_real_
    ),
    # Same for stress level
    Stress_Level_Num = case_when(
      Stress_Level == "Low" ~ 0,
      Stress_Level == "Medium" ~ 1,
      Stress_Level == "High" ~ 2,
      TRUE ~ NA_real_
    ),
    # And health issues
    Health_Issues_Num = case_when(
      Health_Issues == "None" ~ 0,
      Health_Issues == "Mild" ~ 1,
      Health_Issues == "Moderate" ~ 2,
      Health_Issues == "Severe" ~ 3,
      TRUE ~ NA_real_
    )
  )

# --- SECTION 5: Quick Data Overview ---
# Basic structure checks
dim(coffee)       # rows, columns
glimpse(coffee)   # column names, types, and a few values
head(coffee, 5)   # first 5 rows

# --- SECTION 6: Look at Categorical Variables ---
# Stress_Level distribution
ggplot(coffee, aes(x = Stress_Level)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Distribution of Stress Level")

# Sleep_Quality distribution
ggplot(coffee, aes(x = Sleep_Quality)) +
  geom_bar(fill = "darkgreen") +
  theme_minimal() +
  labs(title = "Distribution of Sleep Quality")

# Health_Issues distribution
ggplot(coffee, aes(x = Health_Issues)) +
  geom_bar(fill = "purple") +
  theme_minimal() +
  labs(title = "Distribution of Health Issues")

# --- SECTION 7: Correlation Analysis ---
# Correlation plot
# Select numeric variables for correlation analysis
numeric_vars <- coffee %>%
  select(Age, Coffee_Intake, Caffeine_mg, Sleep_Hours, BMI, Heart_Rate, 
         Physical_Activity_Hours, Smoking, Alcohol_Consumption,
         Sleep_Quality_Num, Stress_Level_Num, Health_Issues_Num)

# Calculate correlation matrix
cor_matrix <- cor(numeric_vars, use = "complete.obs")

# Create correlation plot
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         order = "hclust",
         tl.cex = 0.7,
         tl.col = "black",
         addCoef.col = "black",
         number.cex = 0.6,
         col = colorRampPalette(c("blue", "white", "red"))(100),
         main = "Correlation Plot - Coffee Health Data")

# --- SECTION 8: Univariate Analysis (Looking at each variable individually) ---
# 8a) Continuous variables: histograms + boxplots (distribution & outliers)

# Load gridExtra for arranging plots
library(gridExtra)

# Create histograms - see what the distributions look like
hist_age <- ggplot(coffee, aes(x = Age)) +
  geom_histogram(bins = 60, color = "white", fill = "steelblue") +
  labs(title = "Age Distribution", x = "Age", y = "Count") +
  theme_minimal()

hist_coffee <- ggplot(coffee, aes(x = Coffee_Intake)) +
  geom_histogram(bins = 60, color = "white", fill = "darkgreen") +
  labs(title = "Coffee Intake (cups/day)", x = "Coffee_Intake", y = "Count") +
  theme_minimal()

hist_caffeine <- ggplot(coffee, aes(x = Caffeine_mg)) +
  geom_histogram(bins = 60, color = "white", fill = "coral") +
  labs(title = "Caffeine (mg)", x = "Caffeine_mg", y = "Count") +
  theme_minimal()

hist_sleep <- ggplot(coffee, aes(x = Sleep_Hours)) +
  geom_histogram(bins = 60, color = "white", fill = "purple") +
  labs(title = "Sleep Hours", x = "Sleep_Hours", y = "Count") +
  theme_minimal()

hist_bmi <- ggplot(coffee, aes(x = BMI)) +
  geom_histogram(bins = 60, color = "white", fill = "orange") +
  labs(title = "BMI", x = "BMI", y = "Count") +
  theme_minimal()

hist_hr <- ggplot(coffee, aes(x = Heart_Rate)) +
  geom_histogram(bins = 60, color = "white", fill = "red") +
  labs(title = "Heart Rate (bpm)", x = "Heart_Rate", y = "Count") +
  theme_minimal()

hist_activity <- ggplot(coffee, aes(x = Physical_Activity_Hours)) +
  geom_histogram(bins = 60, color = "white", fill = "darkblue") +
  labs(title = "Physical Activity (hrs/week)", x = "Physical_Activity_Hours", y = "Count") +
  theme_minimal()

# Create boxplots - good for spotting outliers
box_age <- ggplot(coffee, aes(y = Age, x = 1)) +
  geom_boxplot(outlier.alpha = 0.3, fill = "steelblue") +
  labs(title = "Age", x = NULL, y = "Age") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

box_coffee <- ggplot(coffee, aes(y = Coffee_Intake, x = 1)) +
  geom_boxplot(outlier.alpha = 0.3, fill = "darkgreen") +
  labs(title = "Coffee Intake", x = NULL, y = "Coffee_Intake") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

box_caffeine <- ggplot(coffee, aes(y = Caffeine_mg, x = 1)) +
  geom_boxplot(outlier.alpha = 0.3, fill = "coral") +
  labs(title = "Caffeine", x = NULL, y = "Caffeine_mg") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

box_sleep <- ggplot(coffee, aes(y = Sleep_Hours, x = 1)) +
  geom_boxplot(outlier.alpha = 0.3, fill = "purple") +
  labs(title = "Sleep Hours", x = NULL, y = "Sleep_Hours") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

box_bmi <- ggplot(coffee, aes(y = BMI, x = 1)) +
  geom_boxplot(outlier.alpha = 0.3, fill = "orange") +
  labs(title = "BMI", x = NULL, y = "BMI") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

box_hr <- ggplot(coffee, aes(y = Heart_Rate, x = 1)) +
  geom_boxplot(outlier.alpha = 0.3, fill = "red") +
  labs(title = "Heart Rate", x = NULL, y = "Heart_Rate") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

box_activity <- ggplot(coffee, aes(y = Physical_Activity_Hours, x = 1)) +
  geom_boxplot(outlier.alpha = 0.3, fill = "darkblue") +
  labs(title = "Physical Activity", x = NULL, y = "Physical_Activity_Hours") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Arrange all histograms in a grid
grid.arrange(hist_age, hist_coffee, hist_caffeine, hist_sleep,
             hist_bmi, hist_hr, hist_activity,
             ncol = 3, nrow = 3,
             top = "Distribution of Continuous Variables - Histograms")

# Arrange all boxplots in a grid
grid.arrange(box_age, box_coffee, box_caffeine, box_sleep,
             box_bmi, box_hr, box_activity,
             ncol = 4, nrow = 2,
             top = "Distribution of Continuous Variables - Boxplots")

# --- SECTION 9: Categorical Comparisons (How do groups differ?) ---
# 9a) Categorical comparisons using line graphs

# Calculate means and standard errors for line graphs

# How does coffee intake vary by sleep quality?
coffee_sleep_summary <- coffee %>%
  group_by(Sleep_Quality) %>%
  summarise(mean_coffee = mean(Coffee_Intake, na.rm = TRUE),
            se = sd(Coffee_Intake, na.rm = TRUE)/sqrt(n()),
            .groups = 'drop')

lp1 <- ggplot(coffee_sleep_summary, aes(x = Sleep_Quality, y = mean_coffee, group = 1)) +
  geom_line(color = "brown", size = 1.2) +
  geom_point(size = 3, color = "darkred") +
  geom_errorbar(aes(ymin = mean_coffee - se, ymax = mean_coffee + se), 
                width = 0.1, color = "darkred") +
  labs(title = "Coffee Intake by Sleep Quality",
       x = "Sleep Quality", y = "Mean Coffee Intake (cups/day)") +
  theme_minimal()

# Do stressed people sleep less?
sleep_stress_summary <- coffee %>%
  group_by(Stress_Level) %>%
  summarise(mean_sleep = mean(Sleep_Hours, na.rm = TRUE),
            se = sd(Sleep_Hours, na.rm = TRUE)/sqrt(n()),
            .groups = 'drop')

lp2 <- ggplot(sleep_stress_summary, aes(x = Stress_Level, y = mean_sleep, group = 1)) +
  geom_line(color = "darkblue", size = 1.2) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = mean_sleep - se, ymax = mean_sleep + se), 
                width = 0.1, color = "blue") +
  labs(title = "Sleep Hours by Stress Level",
       x = "Stress Level", y = "Mean Sleep Hours") +
  theme_minimal()

# Heart rate across health issue severity
hr_health_summary <- coffee %>%
  group_by(Health_Issues) %>%
  summarise(mean_hr = mean(Heart_Rate, na.rm = TRUE),
            se = sd(Heart_Rate, na.rm = TRUE)/sqrt(n()),
            .groups = 'drop')

lp3 <- ggplot(hr_health_summary, aes(x = Health_Issues, y = mean_hr, group = 1)) +
  geom_line(color = "red", size = 1.2) +
  geom_point(size = 3, color = "darkred") +
  geom_errorbar(aes(ymin = mean_hr - se, ymax = mean_hr + se), 
                width = 0.1, color = "darkred") +
  labs(title = "Heart Rate by Health Issues",
       x = "Health Issues", y = "Mean Heart Rate (bpm)") +
  theme_minimal()

# Do people with health issues exercise less?
activity_health_summary <- coffee %>%
  group_by(Health_Issues) %>%
  summarise(mean_activity = mean(Physical_Activity_Hours, na.rm = TRUE),
            se = sd(Physical_Activity_Hours, na.rm = TRUE)/sqrt(n()),
            .groups = 'drop')

lp4 <- ggplot(activity_health_summary, aes(x = Health_Issues, y = mean_activity, group = 1)) +
  geom_line(color = "green", size = 1.2) +
  geom_point(size = 3, color = "darkgreen") +
  geom_errorbar(aes(ymin = mean_activity - se, ymax = mean_activity + se), 
                width = 0.1, color = "darkgreen") +
  labs(title = "Physical Activity by Health Issues",
       x = "Health Issues", y = "Mean Physical Activity (hrs/week)") +
  theme_minimal()

# BMI and sleep quality relationship
bmi_sleep_summary <- coffee %>%
  group_by(Sleep_Quality) %>%
  summarise(mean_bmi = mean(BMI, na.rm = TRUE),
            se = sd(BMI, na.rm = TRUE)/sqrt(n()),
            .groups = 'drop')

lp5 <- ggplot(bmi_sleep_summary, aes(x = Sleep_Quality, y = mean_bmi, group = 1)) +
  geom_line(color = "orange", size = 1.2) +
  geom_point(size = 3, color = "darkorange") +
  geom_errorbar(aes(ymin = mean_bmi - se, ymax = mean_bmi + se), 
                width = 0.1, color = "darkorange") +
  labs(title = "BMI by Sleep Quality",
       x = "Sleep Quality", y = "Mean BMI") +
  theme_minimal()

# Does stress lead to more caffeine consumption?
caffeine_stress_summary <- coffee %>%
  group_by(Stress_Level) %>%
  summarise(mean_caffeine = mean(Caffeine_mg, na.rm = TRUE),
            se = sd(Caffeine_mg, na.rm = TRUE)/sqrt(n()),
            .groups = 'drop')

lp6 <- ggplot(caffeine_stress_summary, aes(x = Stress_Level, y = mean_caffeine, group = 1)) +
  geom_line(color = "purple", size = 1.2) +
  geom_point(size = 3, color = "darkviolet") +
  geom_errorbar(aes(ymin = mean_caffeine - se, ymax = mean_caffeine + se), 
                width = 0.1, color = "darkviolet") +
  labs(title = "Caffeine Intake by Stress Level",
       x = "Stress Level", y = "Mean Caffeine (mg)") +
  theme_minimal()

# Put all the trend plots together
grid.arrange(lp1, lp2, lp3, lp4, lp5, lp6,
             ncol = 3, nrow = 2,
             top = "Categorical Variable Trends")


#===============================================================================================================================
#Milestone 2 : Supervised ML Models

#Libraries
#install.packages("ggplot2", dependencies = TRUE)
#install.packages("tidymodels", dependencies = TRUE)


library(MLmetrics)
library(tidyverse)
library(janitor)
library(skimr)
library(caret)
library(ranger)
library(pROC)      # multiclass AUC
library(nnet)      # multinom
library(rpart.plot)

names(coffee)

#Reproducibility: data splits will give the same result over and over 
set.seed(123)

glimpse(coffee)
skim(coffee)


# Target as ordered factor (optional, we’ll still do standard multiclass)
coffee <- coffee %>%
  mutate(
    Stress_Level = factor(Stress_Level, levels = c("Low","Medium","High"))
  )

# =========================
# 2) EDA-DRIVEN PREP & FEATS
# =========================
# Drop ID; drop Caffeine_mg (perfectly collinear with Coffee_Intake per EDA), Sleep_Quality and Health_Issues because, these last 2 were encoded into numbers
drop_cols <- c("ID", "Caffeine_mg", "Sleep_Quality", "Health_Issues","Stress_Level_Num", 
               "Sleep_Quality_Num","lifestyle_risk")#drop Stress_Level_Num because is linked to the Stress_Level target variable and Sleep_Quality_Num is being dropped after being detected as a leaker—it had a perfect one-to-one relationship with the target Stress_Level

coffee <- coffee %>%
  select(-any_of(drop_cols)) %>%
  mutate(
    # simple, interpretable features grounded in EDA
    sleep_deficit   = pmax(7.5 - Sleep_Hours, 0)                   # hours below ~7.5
    )
View(coffee)
names(coffee)

#Quick sanity plots 
 coffee %>% ggplot(aes(Stress_Level, fill = Stress_Level)) + geom_bar() + theme_minimal()

 
 # 3) TRAIN/TEST SPLIT & BASE PREPROCESSING
 # =========================
 idx   <- createDataPartition(coffee$Stress_Level, p = 0.8, list = FALSE)  # stratified
 train <- coffee[idx, ]
 test  <- coffee[-idx, ]


  
 # Dummy encoding (k-1) to avoid full multicollinearity
 dv <- dummyVars(~ . , data = select(train, -Stress_Level), fullRank = TRUE)
 
 x_train <- predict(dv, newdata = train) %>% as.data.frame()
 x_test  <- predict(dv, newdata  = test ) %>% as.data.frame()

 
 # Scale numerics
 pp <- preProcess(x_train, method = c("center","scale"))
 x_train_pp <- predict(pp, x_train)
 x_test_pp  <- predict(pp, x_test)
 
 y_train <- train$Stress_Level
 y_test  <- test$Stress_Level
 
 train_pp <- cbind(x_train_pp, Stress_Level = y_train)
 test_pp  <- cbind(x_test_pp,  Stress_Level = y_test)
 
  
 
 
 # --- Align TEST columns to TRAIN columns ---
 missing_cols <- setdiff(colnames(x_train), colnames(x_test))
 if (length(missing_cols)) for (mc in missing_cols) x_test[[mc]] <- 0
 extra_cols   <- setdiff(colnames(x_test), colnames(x_train))
 if (length(extra_cols)) x_test <- x_test[, setdiff(colnames(x_test), extra_cols), drop = FALSE]
 x_test <- x_test[, colnames(x_train), drop = FALSE]
 #testing N/A
 bad_cols_train <- names(which(colSums(is.na(x_train_pp)) > 0))
 bad_cols_test  <- names(which(colSums(is.na(x_test_pp))  > 0))
 
 #extra testing 
 # returns columns that are a perfect 1:1 map to Stress_Level (smoking gun)
 leakers <- sapply(names(x_train_pp), function(col) {
   # for each distinct value of the predictor, how many Stress classes appear?
   mx <- tapply(train_pp$Stress_Level, x_train_pp[[col]], function(v) length(unique(v)))
   # if EVERY value maps to exactly ONE class -> perfect map
   all(mx == 1)
 })
 
 which(leakers)  # if any TRUE, drop those columns -->the result here was the Stress_Level_Num variable

 
 

 
 
  
 
 #########
 
 #View(coffee)
 # =========================
 # 4) CV CONTROL (handle class imbalance)
 # =========================
 # Use upsampling within folds; keep macro-style metrics later
 ctrl <- trainControl(
   method          = "repeatedcv",
   number          = 5,
   repeats         = 1,
   classProbs      = TRUE,
   summaryFunction = multiClassSummary,   # accuracy, Kappa, logLoss, etc.
   sampling        = "up",                # ↑ minority classes inside CV
   savePredictions = "final"
 )
 
 #Sanity Checks
#Split per class of the target variable 
 table(coffee$Stress_Level)
#Split per class of the target variable in the training set and in the test set
 table(train$Stress_Level)
 table(test$Stress_Level)
 
 
 #Verifying Stratification
 library(caret)
 ups <- upSample(x = subset(train, select = -Stress_Level),
                 y = train$Stress_Level)
 table(ups$Class)
 nrow(ups) #results: low = medium = high = 5592
 
 #class proportions in both splits (train and test) 
 round(100 * prop.table(table(train$Stress_Level)), 1) # results: low =69.9%, medium = 20.5%, high = 9.5%
 round(100 * prop.table(table(test$Stress_Level)), 1)  # results: low =69.9%, medium = 20.5%, high = 9.5%
 
 
 
 
 
 # =========================
 # 5) MODELS
 # =========================
 #install.packages("MLmetrics")
 #library(MLmetrics)
 
 #### (A) Multinomial Logistic Regression — baseline & interpretable
 multinom_fit <- train(
   Stress_Level ~ .,
   data      = train_pp,
   method    = "multinom",
   trControl = ctrl,
   preProcess = NULL, # already centered/scaled above
   tuneGrid  = expand.grid(decay = c(0, 1e-4, 1e-3, 1e-2)),    # ← λ grid
   trace = FALSE
 )
 

 
 ##### (B) Random Forest (ranger) — non-linear + interactions
 rf_fit <- train(
   Stress_Level ~ .,
   data      = train_pp,     #preprocessed frame
   method    = "ranger",
   trControl = ctrl,      # includes sampling="up"
   tuneGrid  = expand.grid(
     mtry = floor(c(0.25, 0.5, 0.75) * ncol(x_train_pp)), #  mtry grid ≈ {0.23, 0.49, 0.74}·p
     splitrule = "gini",
     min.node.size = c(1, 5, 10) #  min.node.size grid
   ),
   num.trees = 400,  #  400 trees (this sets the forest size)
   importance = "impurity"
 )
 
 
 ##### (C) Gradient Boosting (xgbTree) — model only
 library(xgboost)
 xgboost::xgb.set.config(verbosity = 0)  # quiet logs
 
 # compact grid (fast but strong)
 xgb_grid <- expand.grid(
   nrounds = c(100, 200),       # ← tried 100 and 200 rounds
   max_depth = c(3, 5),         # ← depth grid {3, 5}
   eta = c(0.1, 0.2),
   gamma = 0,
   colsample_bytree = c(0.8, 1.0),  # ← column sampling {0.8, 1.0}
   min_child_weight = c(1, 5),
   subsample = 0.8                  # ← row sampling {0.8}
 )
 
 # use xgboost's internal threads; caret will run serially if no doParallel
 ncores <- max(1, parallel::detectCores() - 1)
 
 xgb_fit <- caret::train(
   Stress_Level ~ .,
   data      = train_pp,
   method    = "xgbTree",
   trControl = ctrl,          # same Cross-Validation you used for A & B.  includes sampling="up"
   tuneGrid  = xgb_grid,     # ← this is where the grid above is searched
   nthread   = ncores,
   tree_method = "hist",
   max_bin     = 256
 )
 
 
 
 # 6) EVALUATION HELPERS (TEST SET)
 # =========================
 library(caret)
 library(pROC)
 
 stopifnot(exists("multinom_fit"), exists("rf_fit"), exists("xgb_fit"))
 stopifnot(exists("x_test_pp"), exists("test_pp"))
 
 # Macro-F1 helper
 macro_f1 <- function(y_true, y_pred) {
   lev <- levels(y_true)
   f1s <- sapply(lev, function(cl) {
     tp <- sum(y_true==cl & y_pred==cl)
     fp <- sum(y_true!=cl & y_pred==cl)
     fn <- sum(y_true==cl & y_pred!=cl)
     prec <- ifelse(tp+fp==0, 0, tp/(tp+fp))
     rec  <- ifelse(tp+fn==0, 0, tp/(tp+fn))
     if ((prec+rec)==0) 0 else 2*prec*rec/(prec+rec)
   })
   mean(f1s)
 }
 
 # Generic evaluator for any caret classifier with prob output
 eval_model <- function(fit, model_name, x_test, y_true) {
   # Hard labels
   pred_class <- predict(fit, newdata = x_test)
   # Probabilities (reorder columns to match factor level order)
   pred_prob  <- predict(fit, newdata = x_test, type = "prob")
   pred_prob  <- pred_prob[, levels(y_true), drop = FALSE]
   
   cm  <- caret::confusionMatrix(pred_class, y_true)
   acc <- unname(cm$overall["Accuracy"])
   mF1 <- macro_f1(y_true, pred_class)
   
   # OvR multiclass AUC (macro-like)
   auc_ovr <- as.numeric(pROC::multiclass.roc(
     response  = y_true,
     predictor = as.matrix(pred_prob)
   )$auc)
   
   list(
     name = model_name,
     acc = acc,
     mF1 = mF1,
     auc = auc_ovr,
     cm  = cm
   )
 }
 
 # Evaluate all three
 res_logit <- eval_model(multinom_fit, "Multinomial Logistic", x_test_pp, test_pp$Stress_Level)
 res_rf    <- eval_model(rf_fit,       "Random Forest",        x_test_pp, test_pp$Stress_Level)
 res_xgb   <- eval_model(xgb_fit,      "XGBoost",              x_test_pp, test_pp$Stress_Level)
 
 # Extract Kappa from each confusion matrix
 kappa_logit <- as.numeric(res_logit$cm$overall["Kappa"])
 kappa_rf    <- as.numeric(res_rf$cm$overall["Kappa"])
 kappa_xgb   <- as.numeric(res_xgb$cm$overall["Kappa"])
 
 # Summary table
 results_tbl <- data.frame(
   Model        = c(res_logit$name, res_rf$name, res_xgb$name),
   Accuracy     = c(res_logit$acc,  res_rf$acc,  res_xgb$acc),
   Macro_F1     = c(res_logit$mF1,  res_rf$mF1,  res_xgb$mF1),
   ROC_AUC_OvR  = c(res_logit$auc,  res_rf$auc,  res_xgb$auc),
   Kappa        = c(kappa_logit,    kappa_rf,    kappa_xgb),
   check.names  = FALSE
 )
 
 results_tbl$Accuracy     <- round(results_tbl$Accuracy,    4)
 results_tbl$Macro_F1     <- round(results_tbl$Macro_F1,    4)
 results_tbl$ROC_AUC_OvR  <- round(results_tbl$ROC_AUC_OvR, 4)
 results_tbl$Kappa        <- round(results_tbl$Kappa,       4)
 
 print(results_tbl) # accuracy, macro-f1, roc-auc, Kappa
 
 # Confusion matrices
 cat("\n--- Confusion Matrix: Multinomial Logistic ---\n"); print(res_logit$cm)
 cat("\n--- Confusion Matrix: Random Forest -----------\n"); print(res_rf$cm)
 cat("\n--- Confusion Matrix: XGBoost ------------------\n"); print(res_xgb$cm)
 
 
 #Visualizations
#XGBoost Variable Importance 
 plot(varImp(xgb_fit), top = 10, main = "XGBoost Variable Importance")
 
#Confusion Matrix (XGBoost)
 library(caret)
 library(ggplot2)
 library(dplyr)
 library(tidyr)
 
 # 1) Predictions + confusion matrix
 pred_xgb <- predict(xgb_fit, newdata = x_test_pp)
 cm_xgb   <- caret::confusionMatrix(pred_xgb, test_pp$Stress_Level)
 
 # 2) Tidy to data frame
 cm_df <- as.data.frame(cm_xgb$table)
 names(cm_df) <- c("Reference","Prediction","Freq")
 
 # 3) (Optional) ensure consistent class order
 cls <- levels(test_pp$Stress_Level)
 cm_df$Reference  <- factor(cm_df$Reference,  levels = cls)
 cm_df$Prediction <- factor(cm_df$Prediction, levels = cls)
 
 # 4) Plot heatmap of counts
 p_counts <- ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
   geom_tile(color = "white") +
   geom_text(aes(label = Freq), size = 5) +
   scale_fill_gradient(low = "grey90", high = "steelblue") +
   labs(title = "Confusion Matrix (XGBoost) — Counts",
        x = "True class (Reference)", y = "Predicted class") +
   theme_minimal(base_size = 13)
 
 print(p_counts)
 
 # 5) Save if needed
 ggsave("xgb_confusion_matrix_counts.png", p_counts, width = 8, height = 6, dpi = 150)
 
