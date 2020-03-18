library(tidyverse)
library(data.table)
library(caTools)
library(inspectdf)
library(Hmisc)
library(e1071)
library(ROCR)

df <- fread('C:/Users/dell/Downloads/Social_Network_Ads (1).csv', na.strings = "")
inspect_na(df)
# FILLING NA----

# find mean age for both gender separately and fill NAs
male_mean_age <- df[df$Gender=="Male","Age"][[1]] %>% mean(na.rm=TRUE) %>% round()
female_mean_age <- df[df$Gender=="Female","Age"][[1]] %>% mean(na.rm = TRUE) %>% round()

df[df$Gender=="Male" & is.na(df$Age) ,"Age"][[1]] <- male_mean_age
df[df$Gender=="Female" & is.na(df$Age), "Age"][[1]] <- female_mean_age

df %>% glimpse()
# divide ages into 5 parts and for each age parts find mean of salary and fill NAs

df$Age_factor<-df$Age %>% cut2(g=5)
sal_mean1 <- df[df$Age_factor=="[18,29)","EstimatedSalary"][[1]] %>% mean(na.rm=T) %>% round()
df[df$Age_factor=="[18,29)" & is.na(df$EstimatedSalary), "EstimatedSalary"] <- sal_mean1

sal_mean2 <- df[df$Age_factor=="[29,39)","EstimatedSalary"][[1]] %>% mean(na.rm=T) %>% round()
df[df$Age_factor=="[29,39)" & is.na(df$EstimatedSalary), "EstimatedSalary"] <- sal_mean2

sal_mean3 <- df[df$Age_factor=="[39,41)","EstimatedSalary"][[1]] %>% mean(na.rm=T) %>% round()
df[df$Age_factor=="[39,41)" & is.na(df$EstimatedSalary),"EstimatedSalary"] <- sal_mean3

sal_mean4 <- df[df$Age_factor=="[41,48)","EstimatedSalary"][[1]] %>% mean(na.rm=T)  %>% round()       
df[df$Age_factor=="[41,48)" & is.na(df$EstimatedSalary), "EstimatedSalary"] <- sal_mean4

sal_mean5 <- df[df$Age_factor=="[48,60]","EstimatedSalary"][[1]] %>% mean(na.rm=T) %>% round()
df[df$Age_factor=="[48,60]" & is.na(df$EstimatedSalary), "EstimatedSalary"] <- sal_mean5

inspect_na(df)
df[is.na(df$Gender)]

# Gender
df$sal_factor <- df$EstimatedSalary %>% cut2(g=5)

df[is.na(df$Gender),c("sal_factor","Age_factor")] %>% unique()

mode1 <- df[df$Age_factor=="[18,29)" & df$sal_factor=="[38000, 58000)","Gender"][[1]] %>% 
  na.omit() %>% as.matrix() %>% table() %>% as.data.frame() %>% 
  arrange(desc(Freq)) %>% .[1,1] %>% as.character()
df[df$Age_factor=="[18,29)" & df$sal_factor=="[38000, 58000)" & is.na(df$Gender),"Gender"][[1]] <- mode1

mode2<-df[df$Age_factor=="[29,39)" & df$sal_factor=="[58000, 76000)", "Gender"][[1]] %>% 
  na.omit() %>% as.matrix() %>% table() %>% as.data.frame() %>% arrange(desc(Freq)) %>% .[1,1] %>% as.character()
df[df$Age_factor=="[29,39)" & df$sal_factor=="[58000, 76000)" & is.na(df$Gender), "Gender"][[1]] <- mode2


mode3<-df[df$Age_factor=="[41,48)" & df$sal_factor=="[58000, 76000)","Gender"][[1]] %>% 
  na.omit() %>% as.matrix() %>% table() %>% as.data.frame() %>% arrange(desc(Freq)) %>% .[1,1] %>% as.character()

df[df$Age_factor=="[41,48)" & df$sal_factor=="[58000, 76000)" & is.na(df$Gender), "Gender"][[1]] <- mode3

mode4<-df[df$Age_factor=="[48,60]" & df$sal_factor=="[76000, 96000)","Gender"][[1]] %>% 
  na.omit() %>% as.matrix() %>% table() %>% as.data.frame() %>% arrange(desc(Freq)) %>% .[1,1] %>% as.character()

df[df$Age_factor=="[48,60]" & df$sal_factor=="[76000, 96000)" & is.na(df$Gender), "Gender"][[1]] <- mode4

mode5<-df[df$Age_factor=="[39,41)" & df$sal_factor=="[58000, 76000)","Gender"][[1]] %>% 
  na.omit() %>% as.matrix() %>% table() %>% as.data.frame() %>% arrange(desc(Freq)) %>% .[1,1] %>% as.character()

df[df$Age_factor=="[39,41)" & df$sal_factor=="[58000, 76000)" & is.na(df$Gender), "Gender"][[1]] <- mode5

inspect_na(df)

df<-df %>% select(-sal_factor,-Age_factor)
str(df)

df <- df %>% rename(ID = 'User ID')
df$Purchased <- df$Purchased %>% as.factor()
df$Gender <- df$Gender %>% as.factor() %>% as.numeric()

# MODELING----

m_data <- df %>% select(-ID)
set.seed(123)
split <- m_data$Purchased %>% sample.split(SplitRatio = 0.80) 
train_set <- m_data %>% subset(split==T)
test_set  <- m_data %>% subset(split==F)

# future scaling
train_set <- train_set[,-4] %>% scale() %>% as.data.frame() %>% cbind(Purchased = train_set$Purchased)
test_set <- test_set[,-4] %>% scale() %>% as.data.frame() %>% cbind(Purchased = test_set$Purchased)

#fitting SVM----
model <- svm(formula = Purchased~.,
             data    = train_set,
             type    = 'C-classification',
             probability = T,
             kernel  = 'linear'
  )
# predicting test results for SVM
predd <- model %>% predict(test_set %>% select(-Purchased), probability = TRUE)

probb <- attr(predd, "probabilities")

# Evaluation Metrices (Accuracy & AUC) for SVM ----
train_lab <- train_set %>% pull(Purchased)
test_lab <- test_set %>% pull(Purchased)
cn <- train_lab %>% unique() %>% length()

pp <- probb %>% 
  as.data.frame() %>% 
  mutate(label = test_lab) %>%
  bind_cols(predd = as.data.frame(predd))

confusion_matrix <- table(actual=pp$label, predicted=pp$pred)

t_neg <- confusion_matrix[1]
f_neg <- confusion_matrix[2]
f_pos <- confusion_matrix[3]
t_pos <- confusion_matrix[4]

accuracy  = (t_pos+t_neg)/(t_pos+t_neg+f_neg+f_pos)
recall    = t_pos/(t_pos+f_neg)
precision = t_pos/(t_pos+f_pos)
f1_score  = (2*recall*precision)/(recall+precision)
tibble(accuracy, f1_score, recall,precision)

pred_object <- prediction(pp$`1`, test_set$Purchased)

# Trashold
eval <- pred_object %>% performance("acc")
trashold <- eval %>% slot("x.values") %>% .[[1]] %>% .[-1] %>% max() %>% round(2)

# ROC curve
roc_curve <- pred_object %>% performance("tpr","fpr")
roc_curve %>% plot(colorize=T,
                   main="ROC curve",
                   ylab="Sensitivity",
                   xlab="1-Specivity")
abline(a=0,b=1)

# AUC
AUC <- pred_object %>% performance("auc")
AUC_test <- AUC %>% slot("y.values") %>% .[[1]] %>% round(2)

# Check overfitting for SVM ----

predd <- model %>% predict(train_set %>% select(-Purchased), probability=T)
probb <- attr(predd,"probabilities")

pp <- probb %>% 
      as.data.frame() %>% 
      mutate(label=train_lab) %>% 
      bind_cols(pred=as.data.frame(predd))

pred_object <- prediction(pp$`1`,train_set$Purchased)
AUC <- pred_object %>% performance('auc')
AUC_train <- AUC %>% slot("y.values") %>% .[[1]] %>% round(2)

tibble(AUC_train, AUC_test)








  