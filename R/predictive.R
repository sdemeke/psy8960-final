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
full_dat <- read_rds("../rds/combined_dataset.rds") %>% 
#create new column of combined positive and negative review for dtm 
  mutate(OverallReview = paste(PosReview, NegReview, sep = " ") )

### Process text (PosReview and NegReview columns) using sentiment analysis to derive scores for predictive models

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
  
  
  
  # compare_them <- function(og_corp,proc_corp,random_index) {
  #   
  #   list(
  #     random_index,
  #     "Original Corpus Sample" =  og_corp[[random_index]]$content,
  #     
  #     "Cleaned Corpus Sample" = proc_corp[[random_index]]$content
  #   )
  # }
  # 
  # compare_them(og_corp = pos_rev_corpus,
  #              proc_corp = clean_corpus,
  #              random_index = sample(1:length(pos_rev_corpus),1))
  # #looks good
  
  
  
  #remove empty documents
  
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
  
  ##use above dtm - join to original df and apply to machine learning from raw word count
  #for use in ml, applying removeSparseTerms more harshly
  #as_tibble(as.matrix(corp_slim_dtm)) %>% View
  
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


#Step 1 - Apply clean tokenize function to pos and neg corpus
pos_rev_clean_corpus <- clean_tokenize_text(pos_rev_corpus) 
neg_rev_clean_corpus <- clean_tokenize_text(neg_rev_corpus)
overall_rev_clean_corpus <- clean_tokenize_text(overall_rev_corpus)


#Step 2A - Create DTM using overall (combined positive and negative)
overall_rev_bigram_dtm_df <- create_bigram_dtm(overall_rev_clean_corpus) 
#159 terms using 98% sparsity


#Step 2B - compute sentiments positive and negative reviews and combine to overall sentiment score
pos_rev_sentiment_df <- create_tdm_sentiment(pos_rev_clean_corpus) %>% 
  rename(PosReviewSentWt = wt_sentiment)
#mean(pos_rev_sentiment_df$PosReviewSentWt) #total mean=.54

#repeat for negative reviews
neg_rev_sentiment_df <- create_tdm_sentiment(neg_rev_clean_corpus) %>% 
  rename(NegReviweSentWt = wt_sentiment)
#mean(neg_rev_sentiment_df$NegReviweSentWt) #total mean=0.05, not negative but much lower than positive mean

#Step 3 - Add text-based predictors (overall DTM and computed positive and negative sentiment) to original dataset

full_dat_tidy <- full_dat %>% 
  left_join(pos_rev_sentiment_df, by = "EmployeeID") %>% 
  left_join(neg_rev_sentiment_df, by = "EmployeeID") %>% 
  #mutate(OverallSentiment = PosReviewSentWt + NegReviweSentWt) %>% 
  #not sure whether to use pos and neg as predictors (they don't really correlate) or overall score as 1 pred
  left_join(overall_rev_bigram_dtm_df, by = "EmployeeID") 
#comparing original text to the final sentiment values
#for some of the positive reviews, neg sentiment assigned. ex "cost is never a concern"
#side effect of sentiment analysis using document term matrix where context can get lost



### Run classification ML model to predict attrition 


#Step 4 - create dataset containing only relevant predictors and outcomes and dummy code all categorical
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
full_dat_tidy_dummy_final <- cbind(Attrition = full_dat_tidy_ml$Attrition, full_dat_tidy_dummy_update)
#225 total columns now

#Step 5 - create 2 datasets, one with text-based predictors and one without
#exclude all text-based variables 
full_dat_ml_no_text <- full_dat_tidy_dummy_final %>% #33 cols, 32 predictors
  select(-c(PosReviewSentWt, NegReviweSentWt, names(overall_rev_bigram_dtm_df)[-1] )) 

#include text-based variables
full_dat_ml_text <- full_dat_tidy_dummy_final #225 cols, 224 predictors with dummy coded categoricals


#Step 6 - run ML models across both datasets using multiple models to compare

#function to create test/train data and run ml models
get_ml_model <- function(dat, ml_model =  c("glm","glmnet","ranger","xgbTree")) { 
  
  #testing
  dat <- full_dat_tidy_text
  ml_model <- "glmnet"
  
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
    metric = "Accuracy", #for classification models
    method = ml_model,
    preProcess = c("center","scale","nzv","medianImpute"), 
    na.action = na.pass,
    trControl = myControl
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
  
  return(model)
  
  
}


