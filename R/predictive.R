# Script Settings and Resources
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

#Load combined data file
full_dat <- read_rds("../rds/combined_dataset.rds")

### Process text (PosReview and NegReview columns) using sentiment analysis to derive scores for predictive models

#create corpus objects for positive and negative reivews
pos_rev_corpus <- VCorpus(VectorSource(full_dat$PosReview))
neg_rev_corpus <- VCorpus(VectorSource(full_dat$NegReview))

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
  
  #remove empty documents
  
  clean_corpus_filt <- tm_filter(clean_corpus, FUN = function(x)  { return(nchar(stripWhitespace(x$content)[[1]]) > 0) } )
  
  #create DTM matrix with unigrams
  corp_dtm <- DocumentTermMatrix(clean_corpus_filt,
                                 control = list(
                                   tokenize = function(x) { 
                                     NGramTokenizer(x, 
                                                    Weka_control(min=1, max=1)) }
                                 )
  ) 
  
  
  return(corp_dtm)
}


#apply clean tokenize function to pos and neg corpus
pos_rev_dtm <- clean_tokenize_text(pos_rev_corpus) 
neg_rev_dtm <- clean_tokenize_text(neg_rev_corpus)


#Sentiment Analysis

#sentiment function
compute_sentiment <- function(dtm_obj) {
  
  tidy_rev_dtm <- tidy(dtm_obj) %>% #tidy organizes terms by document and includes count column for each term 
    inner_join(get_sentiments("bing"),by = c("term"="word")) %>% #assigns 'positive' or 'negative' to any terms that exist in bing dictionary
    mutate(sentiment = recode( #we want numeric sentiment values for predictive model so i assign +1/-1 as positive and negative sentiment
      sentiment, "positive" = 1, "negative" = -1
    )) %>% 
    group_by(document) %>% 
    mutate(total_count = sum(count),#within each document/review, sum up total count of terms
           word_freqp = count/total_count) %>% #for each term, compute its proportion of the total count (repeated words have higher frequency)
    summarise(wt_sentiment = mean(word_freqp*sentiment)) %>% #weight the sentiment of each term by its frequency and average all weighted sentiments by document
    ungroup() %>% 
    mutate(EmployeeID = as.numeric(document), .keep ="unused") #document is same as employee id, rename for easy merge later with full dataset
  
  
  return(tidy_rev_dtm)
  
}


#compute sentiments for positive review tdm
pos_rev_dtm_tidy <- compute_sentiment(dtm_obj = pos_rev_dtm) %>% 
  rename(PosReviewSentWt = wt_sentiment)
#mean(pos_rev_dtm_tidy$wt_sentiment) #total mean=.49

#repeat for negative reviews
neg_rev_dtm_tidy <- compute_sentiment(dtm_obj = neg_rev_dtm) %>% 
  rename(NegReviweSentWt = wt_sentiment)
#mean(neg_rev_dtm_tidy$wt_sentiment) #total mean=0.00

#merge processed text columns with full data
full_dat_tidy <- full_dat %>% 
  left_join(pos_rev_dtm_tidy) %>% 
  left_join(neg_rev_dtm_tidy) 
#comparing original text to the final sentiment values
#for some of the positive reviews, neg sentiment assigned. ex "cost is never a concern"
#side effect of sentiment analysis using document term matrix where context can get lost
#can i just use the dtm matrix as predictors?


### Run classification ML model to predict attrition 

#create datasets with and without textual data

full_dat_tidy_text <- full_dat_tidy %>% 
  select(-c(PosReview,NegReview)) #ignore original non-processed columns


full_dat_tidy_no_text <- full_dat_tidy_text %>% 
  select(-c(PosReviewSentWt,NegReviweSentWt)) #remove sentiment scores


#frequency of turnover in observed data
prop.table(table(full_dat_tidy$Attrition))
# No       Yes 
# 0.8386219 0.1613781 

#Visualization?


#function to create test/train data and run ml models
getMLResults <- function(dat, ml_model =  c("glm","glmnet","ranger","xgbTree")) { 
  
  #testing
  dat <- full_dat_tidy_text
  ml_model <- "glmnet"
  
  dat <- dat %>% 
    select(-c(EmployeeID, Over18))
  #don't want employee id as predictor
  #over18 is a factor with no variance (only 1 level) not excluded by preprocess so excluded here
  
  set.seed(24)
  
  no_folds <- 10
  cv_index <- createDataPartition(dat$Attrition, p = 0.75, list = FALSE)
  train_data <- dat[cv_index,] 
  test_data <- dat[-cv_index,] 
  
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
    metric = "Accuracy", #for classif models
    method = ml_model,
    preProcess = c("center","scale","nzv","medianImpute"), 
    na.action = na.pass,
    trControl = myControl
  )
  
  
  predicted <- predict(model, test_data, na.action = na.pass)

  confusionMatrix(predicted, test_data$Attrition)
  
  
  results <- tibble(
    model_name = ml_model,
    cv_acc = max( model[["results"]][["Accuracy"]]),
    cv_kappa = max(model[["results"]][["Kappa"]]),
    ho_acc = cor(as.numeric(predicted), as.numeric(test_data$Attrition))^2
    )
  
  return(model)
  
  
}




