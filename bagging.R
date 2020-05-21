if (!require("pacman")) install.packages("pacman")
pacman::p_load(DataExplorer,here, readr,plyr,gridExtra, glmnet, tidymodels,kableExtra, stargazer,ROCit, caret)

#Load train and test data
urlfile1= "https://raw.githubusercontent.com/elior631/Kaggle-competition-No.1/master/train.csv"
urlfile2= "https://raw.githubusercontent.com/elior631/Kaggle-competition-No.1/master/test.csv"

train_set<- read_csv(url(urlfile1))
test_set<- read_csv(url(urlfile2))


train_set<- train_set %>%
  mutate(fem_edu = female*edyrs, fem_col = female*colldeg,
         fem_bl = female*black, bl_edu =black*edyrs, bl_col = black*colldeg,
         edu2 = edyrs*edyrs) 


test_set<- test_set %>%
  mutate(fem_edu = female*edyrs, fem_col = female*colldeg,
         fem_bl = female*black, bl_edu =black*edyrs, bl_col = black*colldeg,
         edu2 = edyrs*edyrs) 

set.seed(1994)
data_split<- initial_split(train_set, prop = 0.7)
train_set1 <- training(data_split)
test_set1 <- testing(data_split)

# Fit a model

x_train <- train_set1[, c(3:45)] %>%
  data.matrix()
y_train <- train_set1$lnwage

x_test <- test_set1[, c(3:45)] %>%
  data.matrix()
y_test <- test_set1$lnwage  

test_set_noID <- data.matrix(test_set[, c(2:44)])


formula_full <- lnwage ~ . -ID

fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 3)
ada <- train(formula_full,
             data = train_set1,
             method = "ada",
             trControl = fitControl)

models_pred <- data.frame(
  ada = predict(ada, newdata = test_set1),
  truth = test_set1$lnwage)

rmse(models_pred, truth, ada)

pred<- predict(ada, newdata = test_set) 

to_print <- data.frame( test_set$ID, pred) %>%
  rename( "lnwage" = pred )



write.csv(
  to_print,
  file = ("E_Lior6_ada.csv"),
  row.names = FALSE
)

