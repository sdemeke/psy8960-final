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

#Process text (PosReview and NegReview columns) using sentiment analysis to derive scores for predictive models

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
  
  #create TDM matrix with unigrams/bigrams
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








