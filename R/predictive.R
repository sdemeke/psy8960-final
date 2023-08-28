# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(tidytext)
library(textdata)
library(tm)
library(qdap)
library(textstem)
library(RWeka)

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

pos_rev_dtm_tidy <- tidy(pos_rev_dtm) %>% #tidy organizes terms by document and includes count column for each term 
  inner_join(get_sentiments("bing"),by = c("term"="word")) %>% #assigns 'positive' or 'negative' to any terms that exist in bing dictionary
   mutate(sentiment = recode( #we want numeric sentiment values for predictive model so i assign +1/-1 as positive and negative sentiment
    sentiment, "positive" = 1, "negative" = -1
  )) %>% 
  group_by(document) %>% 
  mutate(total_count = sum(count),#within each document/review, sum up total count of terms
         word_freqp = count/total_count) %>% #for each term, compute its proportion of the total count (repeated words have higher frequency)
  summarise(wt_pos_sentiment = mean(word_freqp*sentiment)) %>% #weight the sentiment of each term by its frequency and average all weighted sentiments by document
  ungroup() 
#mean(pos_rev_dtm_tidy$wt_pos_sentiment) #total mean=.49

#sentiment function
compute_sentiment <- function(tdm_obj) {
  
  
  
  
}







