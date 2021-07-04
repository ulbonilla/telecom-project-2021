
###### setup #########

rm(list=ls())

# the path directory contains all working files for this project
path <- "C:/Users/User/OneDrive/Documentos"  # change if needed
setwd(path)

Sys.setlocale("LC_TIME", "English")

###### libraries ######

# libraries used in this project are authomatically installed
if (!require("readr")) install.packages("readr")
library("readr")
if (!require("kaggler")) install.packages("kaggler")
library("kaggler") # kaggle API
if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")
if (!require("knitr")) install.packages("knitr")
library("knitr") # tables
if (!require("rlang")) install.packages("rlang")
library("rlang")
if (!require("sjPlot")) install.packages("sjPlot")
library("sjPlot") # chart grid
if (!require("caret")) install.packages("caret")
library("caret")
if (!require("randomForest")) install.packages("randomForest")
library("randomForest") # random forest
if (!require("naivebayes")) install.packages("naivebayes")
library("naivebayes") # naive bayes
if (!require("nnet")) install.packages("nnet")
library("nnet") # neural networks
if (!require("glmnet")) install.packages("glmnet")
library("glmnet") # logistic
if (!require("Hmisc") ) install.packages("Hmisc")
library("Hmisc") # for correlation table

###### data ######

key <- str_c(path,'/kaggle.json') # kaggle API key

if (file.exists(key)) {
  
  kaggler::kgl_auth(creds_file = key) # to run this code you need a kaggle key!!
  
  response <- kgl_datasets_download_all(owner_dataset = "radmirzosimov/telecom-users-dataset")
  
  download.file(response[["url"]],  str_c(path,"/data.zip"), mode="wb")
  
  unzip(str_c(path,"/data.zip"), exdir = str_c(path,"/data"), overwrite = TRUE)
  
  rm(response) # not used
  
  data <- read.csv(str_c(path,"/data/telecom_users.csv"), 
                   stringsAsFactors = TRUE) %>% 
          select(-X)
  
} else { # without the key, the data can be downloaded from github
  
  data <- read.csv("https://raw.githubusercontent.com/ulbonilla/telecom-project-2021/main/telecom_users.csv", 
          stringsAsFactors = TRUE) %>% 
          select(-X)

}



###### analysis ######

# table 1: class and number of NAs by column
table1 <- data %>% 
          lapply(function(x) class(x)) %>% # class of columns
          as.data.frame() %>% 
          gather(key = "column_name", value = "class") %>% 
          bind_cols(sapply(data, function(x) sum(is.na(x))) ) %>% 
          bind_cols(sapply(data, function(x) dim(data)[1] - sum(duplicated(x)))) %>% 
          set_names("column_name", "class", "NAs", "unique_values") 

knitr::kable(table1, # knitr tables have a better format
             align = "ccc", 
             col.names = c("Column name", 
                           "Class", 
                           "Number of NAs", 
                           "Number of unique values"))

# data1 contains the 10 rows with NAs to be inspected manually
data1 <- data %>% 
          filter(is.na(TotalCharges)) %>% 
          select(customerID, tenure, TotalCharges)

# SeniorCitizen is not a numeric variable, but a categorical one
data <- data %>% 
        mutate(TotalCharges = ifelse(is.na(TotalCharges), 0, TotalCharges),
               SeniorCitizen = factor(SeniorCitizen))

# inspect numerical variables

# graph 1: historgrams of numeric variables
graph1 <-   data %>% 
            select_if(is.numeric) %>% 
            gather(key = "column", value = "value") %>%  
            mutate(column = ifelse(column == "tenure", "Tenure", column),
                   column = ifelse(column == "MonthlyCharges", "Monthly Charges", column),
                   column = ifelse(column == "TotalCharges", "Total Charges", column)) %>% 
            ggplot() + 
            geom_histogram(aes(x= value, fill = column), alpha=0.6) +
            facet_wrap(~column, scales = "free_x") +
            theme_bw() +
            theme(legend.position = "none") +
            xlab("") +
            ylab("") +
            ggtitle(label = "Histograms of numeric variables")

show(graph1)

# table 2: statistics of numerical variables
table2 <- data %>% 
          select_if(is.numeric) %>% 
          gather(key = "column_name", value = "value") %>% 
          group_by(column_name) %>% 
          summarise(mean = format(round( mean(value, na.rm=TRUE),2), nsmall =2, big.mark = ","),
                    max = format(round( max(value, na.rm=TRUE),2), nsmall =2, big.mark = ","),
                    min = format(round( min(value, na.rm=TRUE),2), nsmall =2, big.mark = ","),
                    sd = format(round( sd(value, na.rm=TRUE),2), nsmall =2, big.mark = ",") ) 

knitr::kable(table2, 
             align = "ccc", 
             col.names = c("Column name", 
                           "Mean", 
                           "Maximum", 
                           "Minumum",
                           "Standard deviation"))

# table 3: correlation between numerical variables
table3 <- rcorr(as.matrix(data[,c("tenure", "MonthlyCharges", "TotalCharges")]))[[1]] %>% 
          as.data.frame()

knitr::kable(table3, 
             align = "ccc")

# graph 2: density of mean charges
graph2 <-   data %>% 
            summarise(MeanCharges = TotalCharges/tenure)  %>% 
            ggplot() + 
            geom_density(aes(x= MeanCharges),  fill="skyblue") +
            theme_bw() +
            theme(legend.position = "none") +
            xlab("") +
            ylab("") +
            ggtitle(label = "Density of mean charges")

show(graph2)

# MeanCharges will be used instead of TotalCharges and MonthlyCharges
data <- data %>% 
        mutate(MeanCharges = ifelse(tenure==0, 0, TotalCharges/tenure)) %>% 
        select(-TotalCharges, -MonthlyCharges)

# inspect categorical variables

# this function is used to create pie charts
pie_fun <- function(col_name, data){
  
  data %>% 
    group_by(!!sym(col_name)) %>% 
    summarise(porcentaje=n()/dim(data)[1],
              porcentaje_f = format(round(porcentaje*100,2),2)) %>% 
    ggplot(aes(x = "",y= porcentaje, fill = !!sym(col_name)))  +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start=0) + 
    geom_text(aes(label=porcentaje_f), 
              color = "white", 
              size=2, 
              position = position_stack(vjust = 0.5)) +
    xlab("Porcentaje (%)") +
    ylab("") +
    labs(fill = "") +
    ggtitle(label = str_c("Pie chart of column ", col_name)) +
    theme(text = element_text(size=6),
          legend.position = "bottom")
  
}

# names of categorical columns
names_f <- names(select_if(data, is.factor))[-c(1, 17)]

pie_charts_grid <- plot_grid(map(names_f, pie_fun, data), margin = c(.1, .1, .1, .1), tags = NULL)

ggsave(plot=pie_charts_grid  ,paste0(path, "/pie_charts_grid.png"), width = 8, height = 12)

show(pie_charts_grid)

# Churn pie chart is replicated on its own
churn_d <-  data %>% 
            group_by(Churn) %>% 
            summarise(porcentaje=n()/dim(data)[1],
                      porcentaje_f = format(round(porcentaje*100,2),2)) 

pie_fun("Churn", data)

# pre processing


# the first pre processing task is to convert all data to numeric data

y <- data %>% select(Churn)
x <- data[-1] %>% select(-Churn)

dummies <- caret::dummyVars(" ~ .", data = x)

# this is the data used for the models
data2 <- predict(dummies, newdata = x) %>% 
          data.frame() %>% 
          bind_cols(y)

utils::str(data2 , vec.len=4,width = 75, strict.width= "cut")

write.csv(data2, str_c(path, "/data2.csv"))

# the second task is to split the date into training and testing sets

index <- createDataPartition(data2$Churn, p = 0.8, list = FALSE)

train_data <- data2[index, ]
test_data  <- data2[-index, ]

write.csv(train_data, str_c(path, "/train_data.csv"))
write.csv(test_data, str_c(path, "/test_data.csv"))

# finally, i add the training settings

ctrl <- trainControl(method = "repeatedcv", 
                     number = 6, 
                     verboseIter = FALSE ,
                     classProbs =  TRUE,
                     repeats = 3)


###### models ######

# support vector machine
model_svm <- caret::train(Churn ~ .,
                           data = train_data,
                           method = "svmLinear",
                           preProcess = c("scale", "center"),
                           trControl = ctrl,
                           na.action = na.exclude)

saveRDS(object = model_svm, file = str_c(path, "/model_svm.rds"))

# naive bayes
model_nb <- caret::train(Churn ~ .,
                         data = train_data,
                         method = "naive_bayes",
                         preProcess = c("scale", "center"),
                         trControl = ctrl,
                         na.action = na.exclude)

saveRDS(object = model_nb, file = str_c(path, "/model_nb.rds"))

# random forests
model_rf <- caret::train(Churn ~ . ,
                         data = train_data,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = ctrl,
                         na.action = na.exclude)

saveRDS(object = model_rf, file = str_c(path, "/model_rf.rds"))

# neural networks
model_nn <- caret::train(Churn ~ . ,
                         data = train_data,
                         method = "nnet",
                         preProcess = c("scale", "center"),
                         trControl = ctrl,
                         na.action = na.exclude,
                         MaxNWts=500,
                         maxit = 100, 
                         linout = FALSE)

saveRDS(object = model_nn, file = str_c(path, "/model_nn.rds"))

# logistic regression
model_lr <- caret::train(Churn ~ . ,
                         data = train_data,
                         method = 'glmnet',
                         preProcess = c("scale", "center"),
                         trControl = ctrl,
                         na.action = na.exclude,
                         family = 'binomial')

saveRDS(object = model_lr, file = str_c(path, "/model_lr.rds"))

###### models evaluation ####

model_svm <- read_rds(str_c(path, "/model_svm.rds"))

model_svm

model_nb <- read_rds(str_c(path, "/model_nb.rds"))

model_nb

model_rf <- read_rds(str_c(path, "/model_rf.rds"))

model_rf

plot(model_rf)

model_nn <- read_rds(str_c(path, "/model_nn.rds"))

model_nn

plot(model_nn)

model_lr <- read_rds(str_c(path, "/model_lr.rds"))

model_lr

plot(model_lr)

######### results ############

models_files <- c("model_rf",
                  "model_svm",
                  "model_nb",
                  "model_lr",
                  "model_nn")

models_names <- c("Random Forest", 
                  "Support Vector Machine", 
                  "Naive Bayes", 
                  "Logistic Regression",
                  "Neural Network")

accuracy <- list()
other_metrics <- list()

# loop to extract the metrics from all models
for (i in 1:length(models_files)) {
  
  model <- readRDS(str_c(path, "/", models_files[i] ,".rds"))
  
  test_pred <- predict(model, newdata = test_data)
  
  accuracy[[i]] <- confusionMatrix(table(test_pred, test_data$Churn))[[3]]
  
  other_metrics[[i]] <- confusionMatrix(table(test_pred, test_data$Churn))[[4]]
  
}

# confusionMatrix computes two sets of metrics which i organize into 2 tables
accuracy <- bind_rows(lapply(accuracy, as.data.frame.list)) %>% 
            mutate(Model = models_names) %>% 
            gather("Metric", "Value", -Model) 

other_metrics <-  bind_rows(lapply(other_metrics, as.data.frame.list)) %>% 
                  mutate(Model = models_names) %>% 
                  gather("Metric", "Value", -Model)


table4 <- accuracy %>% 
          mutate(Value = format(round( Value, 4), nsmall =4, big.mark = ",") )  %>% 
          spread(Model, Value)

knitr::kable(table4, 
             align = "ccc")

table5 <- other_metrics %>% 
          mutate(Value = format(round( Value, 4), nsmall =4, big.mark = ",") )  %>% 
          spread(Model, Value)

knitr::kable(table5, 
             align = "ccc")





