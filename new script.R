if (!require("pacman")) install.packages("pacman")
pacman::p_load(DataExplorer,readr, glmnet, tidymodels,kableExtra, stargazer,ROCit, caret)

#Load train and test data
urlfile1= "https://raw.githubusercontent.com/elior631/Kaggle-competition-No.1/master/train.csv"
urlfile2= "https://raw.githubusercontent.com/elior631/Kaggle-competition-No.1/master/test.csv"

train_set<- read_csv(url(urlfile1))
test_set<- read_csv(url(urlfile2))
