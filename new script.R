if (!require("pacman")) install.packages("pacman")
pacman::p_load(DataExplorer,here, readr,plyr,gridExtra, glmnet, tidymodels,kableExtra, stargazer,ROCit, caret)

#Load train and test data
urlfile1= "https://raw.githubusercontent.com/elior631/Kaggle-competition-No.1/master/train.csv"
urlfile2= "https://raw.githubusercontent.com/elior631/Kaggle-competition-No.1/master/test.csv"

train_set<- read_csv(url(urlfile1))
test_set<- read_csv(url(urlfile2))

# Define as factors all binary features
vec <- c(3, 8:39)
train_set_viz <- train_set
train_set_viz[vec]<- lapply(train_set_viz[vec], factor)

#########################################################
#visualization of the data

# DataExplorer::create_report(train_set)

correl <- train_set %>%
  as.data.frame() %>%
  cor()

correl[lower.tri(correl,diag=TRUE)]=NA # put NA
correl<-as.data.frame(as.table(correl)) # as a dataframe
correl<-na.omit(correl) # remove NA
correl<-correl[with(correl, order(-Freq)), ]
correl<- correl[correl$Var1 == "lnwage",]

correl<- correl[correl$Freq >= 0.15 | correl$Freq <= -0.15 ,]

correl %>%
  ggplot(aes(x=Var2 , y= Freq)) +
  geom_col(aes( fill = Freq)) +
   xlab('Variables') +
 ylab('') +
  ggtitle("Correlation with lnwage") +
  labs(fill = "Correlation") +
  theme(legend.position = "bottom",
      plot.title = element_text(hjust = 0.5))



  train_set_viz %>% 
  ggplot(aes(x = edyrs, y = lnwage, color = female)) + 
  scale_color_discrete(name = "Sex") +
  xlab('Years of education')+
  ylab('Log o()f wage')+
  geom_point()

# lnwage distribution by sex
log_sex_mean <- ddply(train_set_viz, "female", summarise, grp.mean=mean(lnwage))

sex_p <- train_set_viz %>%
  ggplot(aes(x = lnwage, fill = female)) +
  geom_density(alpha = 0.3) +
  geom_vline(data = log_sex_mean, aes(xintercept = grp.mean),
              linetype="twodash") +
  xlab("Log of wage") +
  scale_fill_brewer(palette="Dark2")

# lnwage distribution by black/white
log_black_mean <- ddply(train_set_viz, "black", summarise, grp.mean=mean(lnwage))

black_p<- train_set_viz %>%
  ggplot(aes(x = lnwage, fill= black)) +
  geom_density(alpha = 0.3) +
  geom_vline(data = log_black_mean, aes(xintercept = grp.mean),
             linetype="twodash") +
  xlab("Log of wage") +
  scale_fill_brewer(palette="Dark2")



log_hisp_mean <- ddply(train_set_viz, "hisp", summarise, grp.mean=mean(lnwage))

hisp_p <- train_set_viz %>%
  ggplot(aes(x = lnwage, fill= hisp)) +
  geom_density(alpha = 0.3) +
  geom_vline(data = log_hisp_mean, aes(xintercept = grp.mean),
             linetype="twodash") +
  xlab("Log of wage") +
  scale_fill_brewer(palette="Dark2")


log_colldeg_mean <- ddply(train_set_viz, "colldeg", summarise, grp.mean=mean(lnwage))

colldeg_p <- train_set_viz %>%
  ggplot(aes(x = lnwage, fill = colldeg)) +
  geom_density(alpha = 0.3) +
  geom_vline(data = log_colldeg_mean, aes(xintercept = grp.mean),
             linetype="twodash")
  xlab("Log of wage") +
  scale_fill_brewer(palette="Dark2")
  

  grid.arrange(colldeg_p, hisp_p, black_p, sex_p)

  
  
#########################################################

  #The model


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
    
    
  fit_Lasso <- glmnet(x_train, y_train, alpha = 0)
  fit_Lasso %>% 
    plot(xvar = "lambda")
  
  # Picking the best Lambda
  
  cv_fit <- cv.glmnet(x_train, y_train, alpha = 0)
  cv_fit %>%
    plot()
  
  # pick Lambdas
  pred_cv_min <- as.vector(predict(cv_fit, s = "lambda.min", newx = x_test))
  pred_cv_1se <- as.vector(predict(cv_fit, s = "lambda.1se", newx = x_test))
  
  
  comp_models <- data.frame("Min Lambda" = pred_cv_min, pred_cv_1se)
 
   comp_models %>%
    tidy()


   multi_metric <- metric_set(rmse, rsq, mae)
   multi_metric(data = test_set1, truth = y_test, estimate = pred_cv_min) %>%
     tibble() %>%
     mutate(.metric = c("RMSE", "R-Squared", "MAE"))
   
   
   pred_cv_1se_test <- as.vector(predict(cv_fit, s = "lambda.1se",
                                    newx = test_set_noID))

   results <- data.frame(test_set[,1], pred_cv_1se_test) %>%
     rename( "lnwage" = pred_cv_1se_test )
     
  
   write.csv(
     results ,
    file = ("E_Lior4.csv"),
     row.names = FALSE
   )
   
   
   