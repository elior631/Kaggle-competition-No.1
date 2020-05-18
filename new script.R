if (!require("pacman")) install.packages("pacman")
pacman::p_load(DataExplorer,readr,plyr,gridExtra, glmnet, tidymodels,kableExtra, stargazer,ROCit, caret)

#Load train and test data
urlfile1= "https://raw.githubusercontent.com/elior631/Kaggle-competition-No.1/master/train.csv"
urlfile2= "https://raw.githubusercontent.com/elior631/Kaggle-competition-No.1/master/test.csv"

train_set<- read_csv(url(urlfile1))
test_set<- read_csv(url(urlfile2))


# Define as factors all binary features
vec <- c(3, 8:39)
train_set[vec]<- lapply(train_set[vec], factor)

# visualization of the data

# DataExplorer::create_report(train_set)


train_set %>% 
  ggplot(aes(x = edyrs, y = lnwage, color = female)) + 
  scale_color_discrete(name = "Sex") +
  xlab('Years of education')+
  ylab('Log of wage')+
  geom_point()

# lnwage distribution by sex
log_sex_mean <- ddply(train_set, "female", summarise, grp.mean=mean(lnwage))

sex_p <- train_set %>%
  ggplot(aes(x = lnwage, fill = female)) +
  geom_density(alpha = 0.3) +
  geom_vline(data = log_sex_mean, aes(xintercept = grp.mean),
              linetype="twodash") +
  xlab("Log of wage") +
  scale_fill_brewer(palette="Dark2")

# lnwage distribution by black/white
log_black_mean <- ddply(train_set, "black", summarise, grp.mean=mean(lnwage))

black_p<- train_set %>%
  ggplot(aes(x = lnwage, fill= black)) +
  geom_density(alpha = 0.3) +
  geom_vline(data = log_black_mean, aes(xintercept = grp.mean),
             linetype="twodash") +
  xlab("Log of wage") +
  scale_fill_brewer(palette="Dark2")

# lnwage distribution by hispanic

log_hisp_mean <- ddply(train_set, "hisp", summarise, grp.mean=mean(lnwage))

hisp_p <- train_set %>%
  ggplot(aes(x = lnwage, fill= hisp)) +
  geom_density(alpha = 0.3) +
  geom_vline(data = log_hisp_mean, aes(xintercept = grp.mean),
             linetype="twodash") +
  xlab("Log of wage") +
  scale_fill_brewer(palette="Dark2")

# lnwage distribution by college degree

log_colldeg_mean <- ddply(train_set, "colldeg", summarise, grp.mean=mean(lnwage))

colldeg_p <- train_set %>%
  ggplot(aes(x = lnwage, fill = colldeg)) +
  geom_density(alpha = 0.3) +
  geom_vline(data = log_colldeg_mean, aes(xintercept = grp.mean),
             linetype="twodash")
  xlab("Log of wage") +
  scale_fill_brewer(palette="Dark2")
  
  
  # Plot all graph together
  

  grid.arrange(colldeg_p, hisp_p, black_p, sex_p)

    