if (!require("pacman")) install.packages("pacman")
pacman::p_load(DataExplorer,here, readr,plyr,gridExtra, glmnet, tidymodels,kableExtra, stargazer,ROCit, caret)

#Load train and test data
urlfile1= "https://raw.githubusercontent.com/elior631/Kaggle-competition-No.1/master/train.csv"
urlfile2= "https://raw.githubusercontent.com/elior631/Kaggle-competition-No.1/master/test.csv"

train_set<- read_csv(url(urlfile1))
test_set<- read_csv(url(urlfile2)) %>%
  data.matrix()


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
  
# גרפים לא אחידים לסדר
  grid.arrange(colldeg_p, hisp_p, black_p, sex_p)

    
# The model
  set.seed(1994)
  data_split<- initial_split(train_set, prop = 0.7)
  train_set1 <- training(data_split)
  test_set1 <- testing(data_split)
  
# Fit a model

  x_train <- train_set1[, c(3:39)] %>%
    data.matrix()
    y_train <- train_set1$lnwage
    
    x_test <- test_set1[, c(3:39)] %>%
      data.matrix()
    y_test <- test_set1$lnwage  
    
    test_set_noID <- data.matrix(test_set[, c(2:38)])

  
  fit_Lasso <- glmnet(x_train, y_train, alpha = 0.5)
  fit_Lasso %>% 
    plot(xvar = "lambda")
  
  # Picking the best Lambda
  
  cv_fit <- cv.glmnet(x_train, y_train, alpha = 0.5)
  cv_fit %>%
    plot()
  
  # pick Lambdas
  pred_cv_min <- as.vector(predict(cv_fit, s = "lambda.min", newx = x_test))
  pred_cv_1se <- as.vector(predict(cv_fit, s = "lambda.1se", newx = x_test))
  
  
  comp_models <- data.frame("Min Lambda" = pred_cv_min, pred_cv_1se)
 
   comp_models %>%
    tidy()


   multi_metric <- metric_set(rmse, rsq, mae)
   multi_metric(data = test_set1, truth = y_test, estimate = pred_cv_1se) %>%
     tibble() %>%
     mutate(.metric = c("RMSE", "R-Squared", "MAE"))
   
   
   pred_cv_1se <- as.vector(predict(cv_fit, s = "lambda.1se",
                                    newx = test_set_noID))

   results <- data.frame(test_set[,1], pred_cv_1se)
  
   write.csv(
     results ,
    file = ("E_Lior2.csv"),
     row.names = FALSE
   )
   
   