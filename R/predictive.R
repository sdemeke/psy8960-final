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

}