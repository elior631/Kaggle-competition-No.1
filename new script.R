if (!require("pacman")) install.packages("pacman")
pacman::p_load(DataExplorer,readr,plyr, glmnet, tidymodels,kableExtra, stargazer,ROCit, caret)

#Load train and test data
urlfile1= "https://raw.githubusercontent.com/elior631/Kaggle-competition-No.1/master/train.csv"
urlfile2= "https://raw.githubusercontent.com/elior631/Kaggle-competition-No.1/master/test.csv"

train_set<- read_csv(url(urlfile1))
test_set<- read_csv(url(urlfile2))

#Define sex as factor
train_set$female <- as.factor(train_set$female)

# visualization of the data

# DataExplorer::create_report(train_set)


train_set %>% 
  ggplot(aes(x = edyrs, y = lnwage, color = female)) + 
  scale_color_discrete(name = "Sex") +
  xlab('Years of education')+
  ylab('Log of wage')+
  geom_point()

log_sex_mean <- ddply(train_set, "female", summarise, grp.mean=mean(lnwage))

train_set %>%
  ggplot(aes(x = lnwage, fill = female)) +
  geom_density(alpha = 0.4) +
  geom_vline(data = log_sex_mean, aes(xintercept = grp.mean),
              linetype="twodash") +
  xlab("Log of wage") +
  scale_fill_brewer(palette="Dark2")



