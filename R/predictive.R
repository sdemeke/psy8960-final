### Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(tidytext)
library(textdata)
library(tm)
library(qdap)
library(textstem)
library(RWeka)
library(caret)
library(parallel)
library(doParallel)
library(tictoc)


### Data Import and Cleaning

#Load combined data file
full_dat <- read_rds("../rds/combined_dataset.rds") %>% 
#create new column of combined positive and negative review for dtm 
  mutate(OverallReview = paste(PosReview, NegReview, sep = " ") )

## Process text (PosReview and NegReview columns) using sentiment analysis to derive scores for predictive models

#create corpus objects for positive and negative reivews
pos_rev_corpus <- VCorpus(VectorSource(full_dat$PosReview))
neg_rev_corpus <- VCorpus(VectorSource(full_dat$NegReview))
overall_rev_corpus <- VCorpus(VectorSource(full_dat$OverallReview))

#function to clean and process corpus objects
clean_tokenize_text <- function(corpus) {
  
  clean_corpus <- corpus %>% 
    tm_map(content_transformer(replace_abbreviation)) %>% 
    tm_map(content_transformer(replace_contraction)) %>%
    tm_map(removeNumbers) %>%  
    tm_map(removePunctuation) %>% 
    tm_map(content_transformer(str_to_lower)) %>% 
    tm_map(removeWords, c("google",stopwords("en"))) %>% 
    tm_map(stripWhitespace) %>% 
    tm_map(content_transformer(lemmatize_strings)) 
  
  clean_corpus_filt <- tm_filter(clean_corpus, FUN = function(x)  { return(nchar(stripWhitespace(x$content)[[1]]) > 0) } )
  
  return(clean_corpus_filt)
}


create_bigram_dtm <- function(clean_corpus_filt) {

  #create DTM matrix with unigrams and bigrams
  corp_dtm <- DocumentTermMatrix(clean_corpus_filt,
                                 control = list(
                                   tokenize = function(x) { 
                                     NGramTokenizer(x, 
                                                    Weka_control(min=1, max=2)) }
                                 )
  ) 
  
  #as_tibble(as.matrix(corp_dtm)) %>% view() #terms look ok, not too many weird so preprocessing steps are fine
  
  #if using bigram approach, need to slim
  corp_slim_dtm <- removeSparseTerms(corp_dtm, .98) 
  #for 98% of documents a token must be zero for it to be removed from matrix
  #use above dtm - join to original df and apply to machine learning from raw word count
  #for use in ml, applying removeSparseTerms more harshly

  #convert to df for easy join with original dataset
  corp_slim_dtm_mat <- as.matrix(corp_slim_dtm)
  
  #to merge with og, create new employeeid col where document=employee id
  corp_slim_dtm_df <-  data.frame(
    EmployeeID = as.numeric(rownames(corp_slim_dtm_mat)), corp_slim_dtm_mat
  )

return(corp_slim_dtm_df)
}

create_tdm_sentiment <- function(clean_corpus_filt) {
#prepare for sentiment analysis

#unigrams only
  corp_tdm <- TermDocumentMatrix(clean_corpus_filt,
                                 control = list(
                                   tokenize = function(x) { 
                                     NGramTokenizer(x, 
                                                    Weka_control(min=1, max=1)) }
                                 )
  ) 
  #still need to slim here? yes but being less harsh because inner join with sentiment dictionary removes many terms anyway
  corp_slim_tdm <- removeSparseTerms(corp_tdm, .997) #341 terms
  
  
sentiment_df <- tidy(corp_slim_tdm) %>% #tidy organizes terms by document and includes count column for each term 
  inner_join(get_sentiments("bing"),by = c("term"="word")) %>% #bing and afinn result in similar number of remaining terms. 
  mutate(sentiment = recode( #we want numeric sentiment values for predictive model so i assign +1/-1 as positive and negative sentiment
    sentiment, "positive" = 1, "negative" = -1
  )) %>% 
    group_by(document) %>% 
    mutate(total_count = sum(count),#within each document/review, sum up total count of terms
           word_freqp = count/total_count) %>% #for each term, compute its proportion of the total count (repeated words have higher frequency)
    summarise(wt_sentiment = mean(word_freqp*sentiment)) %>% #weight the sentiment of each term by its frequency and average all weighted sentiments by document
    ungroup() %>% 
    mutate(EmployeeID = as.numeric(document), .keep ="unused") #document is same as employee id, rename for easy merge later with full dataset
  
  
  return(sentiment_df)
  
}


## Apply clean tokenize function to pos and neg corpus
pos_rev_clean_corpus <- clean_tokenize_text(pos_rev_corpus) 
neg_rev_clean_corpus <- clean_tokenize_text(neg_rev_corpus)
overall_rev_clean_corpus <- clean_tokenize_text(overall_rev_corpus)


## Create DTM using overall (combined positive and negative)
overall_rev_bigram_dtm_df <- create_bigram_dtm(overall_rev_clean_corpus) 
#159 terms using 98% sparsity


## Compute sentiments positive and negative reviews and combine to overall sentiment score
pos_rev_sentiment_df <- create_tdm_sentiment(pos_rev_clean_corpus) %>% 
  rename(PosReviewSentWt = wt_sentiment)
#mean(pos_rev_sentiment_df$PosReviewSentWt) #total mean=.54

#repeat for negative reviews
neg_rev_sentiment_df <- create_tdm_sentiment(neg_rev_clean_corpus) %>% 
  rename(NegReviweSentWt = wt_sentiment)
#mean(neg_rev_sentiment_df$NegReviweSentWt) #total mean=0.05, not negative but much lower than positive mean

## Add text-based predictors (overall DTM and computed positive and negative sentiment) to original dataset

full_dat_tidy <- full_dat %>% 
  left_join(pos_rev_sentiment_df, by = "EmployeeID") %>% 
  left_join(neg_rev_sentiment_df, by = "EmployeeID") %>% 
  #mutate(OverallSentiment = PosReviewSentWt + NegReviweSentWt) %>% 
  #not sure whether to use pos and neg as predictors (they don't really correlate) or overall score as 1 pred
  left_join(overall_rev_bigram_dtm_df, by = "EmployeeID") 
#comparing original text to the final sentiment values
#for some of the positive reviews, neg sentiment assigned. ex "cost is never a concern"
#side effect of sentiment analysis using document term matrix where context can get lost



### Run classification ML models to predict attrition 


## Create dataset containing only relevant predictors and outcomes and dummy code all categorical
full_dat_tidy_ml <- full_dat_tidy %>% 
  select(-c(EmployeeID, Over18, PosReview, NegReview, OverallReview))
  #employee ID is not a relevant predictor. Over18 is a factor with only 1 level which leads to errors in ML model
 #also exclude original, unprocessed text data

#frequency of turnover in observed data
prop.table(table(full_dat_tidy_ml$Attrition))
# No       Yes 
# 0.8387755 0.1612245 

#For all categorical variables (currently stored as factors), I dummy code them using caret::dummyVars()
#The outcome, Attrition, can be left as a factor variable 

full_dat_tidy_dummy <- dummyVars(Attrition~., data = full_dat_tidy_ml)
#update data with dummy vars
full_dat_tidy_dummy_final <- as_tibble(predict(full_dat_tidy_dummy, newdata = full_dat_tidy_ml))
#add back outcome var
full_dat_tidy_dummy_final <- cbind(Attrition = full_dat_tidy_ml$Attrition, full_dat_tidy_dummy_final)
#225 total columns now

## Create 2 datasets, one with text-based predictors and one without
#exclude all text-based variables 
full_dat_ml_no_text <- full_dat_tidy_dummy_final %>% #33 cols, 32 predictors
  select(-c(PosReviewSentWt, NegReviweSentWt, names(overall_rev_bigram_dtm_df)[-1] )) 

#include text-based variables
full_dat_ml_text <- full_dat_tidy_dummy_final #225 cols, 224 predictors with dummy coded categoricals


### Analysis

## Create test and train datasets to use for all models

#create 75/25 train test split
set.seed(24)
cv_index <- createDataPartition(full_dat_ml_text$Attrition, p = 0.75, list = FALSE)

#with text
train_data_txt <- full_dat_ml_text[cv_index,] 

#without text
train_data_no_txt <- full_dat_ml_no_text[cv_index,] 

test_data <- full_dat_ml_text[-cv_index,]
#test will be the same, can leave as is since predictor cols wont be used?


## Run ML models across both datasets using multiple models to compare

#function to create test/train data and run ml models
train_ml_model <- function(train_data, ml_model =  c("glm","glmnet","ranger","xgbTree")) { 
  
  set.seed(24)
  
  no_folds <- 10
  
  fold_indices <- createFolds(train_data$Attrition, k = no_folds)
  
  myControl <- trainControl(
    method = "cv", 
    number = no_folds, 
    verboseIter = TRUE,
    indexOut = fold_indices 
  )
  
  model <- train(
    Attrition~.,
    data = train_data, 
    metric = "Accuracy", #for classification models
    method = ml_model,
    preProcess = c("center","scale","nzv","medianImpute"), 
    na.action = na.pass,
    trControl = myControl
  )
  
    return(model)
  
}


##Use parallelization
local_cluster <- makeCluster(detectCores()-1) #7
registerDoParallel(local_cluster)

#train models using text-based predictors, save computation time
start <- Sys.time()
model_txt_glm <- train_ml_model(train_data = train_data_txt, ml_model = "glm")
end <- Sys.time()
time_model_txt_glm <- as.numeric(difftime(end,start,units="secs"))

start <- Sys.time()
model_txt_glmnet <- train_ml_model(train_data = train_data_txt, ml_model = "glmnet")
end <- Sys.time()
time_model_txt_glmnet <- as.numeric(difftime(end,start,units="secs"))

start <- Sys.time()
model_txt_ranger <- train_ml_model(train_data = train_data_txt, ml_model = "ranger")
end <- Sys.time()
time_model_txt_ranger <- as.numeric(difftime(end,start,units="secs"))

start <- Sys.time()
model_txt_xgbTree <- train_ml_model(train_data = train_data_txt, ml_model = "xgbTree")
end <- Sys.time()
time_model_txt_xgbTree <- as.numeric(difftime(end,start,units="secs"))


model_txt_list <- list("glm" = model_txt_glm,
                       "glmnet" = model_txt_glmnet,
                      "ranger" =  model_txt_ranger,
                       "xgbTree" = model_txt_xgbTree)

#train model excluding text-based predictors
start <- Sys.time()
model_no_txt_glm <- train_ml_model(train_data = train_data_no_txt, ml_model = "glm")
end <- Sys.time()
time_model_no_txt_glm <- as.numeric(difftime(end,start,units="secs"))

start <- Sys.time()
model_no_txt_glmnet <- train_ml_model(train_data = train_data_no_txt, ml_model = "glmnet")
end <- Sys.time()
time_model_no_txt_glmnet <- as.numeric(difftime(end,start,units="secs"))

start <- Sys.time()
model_no_txt_ranger <- train_ml_model(train_data = train_data_no_txt, ml_model = "ranger")
end <- Sys.time()
time_model_no_txt_ranger <- as.numeric(difftime(end,start,units="secs"))

start <- Sys.time()
model_no_txt_xgbTree <- train_ml_model(train_data = train_data_no_txt, ml_model = "xgbTree")
end <- Sys.time()
time_model_no_txt_xgbTree <- as.numeric(difftime(end,start,units="secs"))

model_no_txt_list <- list("glm" = model_no_txt_glm,
                          "glmnet" = model_no_txt_glmnet,
                          "ranger" =  model_no_txt_ranger,
                          "xgbTree" = model_no_txt_xgbTree)


#turn off parallelization
stopCluster(local_cluster)
registerDoSEQ()

#summarize results
summary(resamples(model_txt_list))
summary(resamples(model_no_txt_list))

#Visualize
dotplot(resamples(model_no_txt_list))
dotplot(resamples(model_txt_list))
#for both, ranger and zgbtree > by accuracy and kappa


### Publication

get_ml_summary_results <- function(ml_model, test_data, model_time) {
 
  #test
  ml_model <- model_txt_list[["glm"]]
  model_time <- time_model_txt_glm
  
  # str_remove(round(
  #   resample_sum$statistics$Rsquared[,"Mean"],2
  # ),"^0")

 results <- tibble(
    model_name = ml_model$method,
    cv_acc = mean(ml_model[["results"]][["Accuracy"]]),
    cv_kappa = mean(ml_model[["results"]][["Kappa"]]),
    
    computation_time = paste(str_remove(round(model_time,2),"^0"),"seconds")
  )
   
   # predicted <- predict(model, test_data, na.action = na.pass)
   # 
   # confusionMatrix(predicted, test_data$Attrition)
   # 
   # 
   # results <- tibble(
   #   model_name = ml_model,
   #   cv_acc = max( model[["results"]][["Accuracy"]]),
   #   cv_kappa = max(model[["results"]][["Kappa"]]),
   #   ho_acc = cor(as.numeric(predicted), as.numeric(test_data$Attrition))^2
   #   )
  
  return(results)
  
}

lapply(model_txt_list, get_ml_summary_results)
