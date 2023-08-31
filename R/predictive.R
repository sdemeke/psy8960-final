### Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(tidytext)
#library(textdata)
library(tm)
library(qdap)
library(textstem)
library(RWeka)
library(caret)
library(parallel)
library(doParallel)


### Data Import and Cleaning

#Total process of creating text-derived predictors as input for a classification model predicting attrition from all given variables

#Load combined data file
full_dat <- read_rds("../rds/combined_dataset.rds") %>% 
#create new column of combined positive and negative review for use in a combined DocumentTermMatrix 
  mutate(OverallReview = paste(PosReview, NegReview, sep = " ") )

## Part 1 - Creating text-based predictors
#I chose to process the text data for two purposes 1) create a DocumentTermMatrix of the overall review (positive + negative) and sentiment analysis-derived scores for the positive and negative reviews separately

#create corpus objects for positive, negative, and overall reivews
pos_rev_corpus <- VCorpus(VectorSource(full_dat$PosReview))
neg_rev_corpus <- VCorpus(VectorSource(full_dat$NegReview))
overall_rev_corpus <- VCorpus(VectorSource(full_dat$OverallReview))

#custom function to clean and process corpus objects
#after processing, empty documents are removed from corpus and filtered corpus is returned
clean_tokenize_text <- function(corpus) {
  
  clean_corpus <- corpus %>% 
    tm_map(content_transformer(replace_abbreviation)) %>% 
    tm_map(content_transformer(replace_contraction)) %>%
    tm_map(removeNumbers) %>%  
    tm_map(removePunctuation) %>% 
    tm_map(content_transformer(str_to_lower)) %>% 
    tm_map(removeWords, stopwords("en")) %>% 
    tm_map(stripWhitespace) %>% 
    tm_map(content_transformer(lemmatize_strings)) 
  
  clean_corpus_filt <- tm_filter(clean_corpus, FUN = function(x)  { return(nchar(stripWhitespace(x$content)[[1]]) > 0) } )
  
  return(clean_corpus_filt)
}

#custom function to create a bigram DocumentTermMatrix from a processed corpus and prepare the DTM to merge with full dataset
#after creating the DTM, sparse terms are removed. I use a harsher sparsity threshold than in prior assignments because the DTM will be used as predictor in a machine learning model and I don't want unnecessary terms that may increase noise too much to be included 
#the slim DTM is converted to a matrix for easier joining with the origninal dataset
#an EmployeeID variable is added to the matrix as future join identifier (employee id is the same as the corpus document identifier stored in rownames of the matrix)
#slimmed DTM (converted to data frame object) is returned
create_bigram_dtm <- function(clean_corpus_filt) {

  #create DTM matrix with unigrams and bigrams
  corp_dtm <- DocumentTermMatrix(clean_corpus_filt,
                                 control = list(
                                   tokenize = function(x) { 
                                     NGramTokenizer(x, 
                                                    Weka_control(min=1, max=2)) }
                                 )
  ) 
  

  corp_slim_dtm <- removeSparseTerms(corp_dtm, .99) 

  corp_slim_dtm_mat <- as.matrix(corp_slim_dtm)
  
  corp_slim_dtm_df <-  data.frame(
    EmployeeID = as.numeric(rownames(corp_slim_dtm_mat)), corp_slim_dtm_mat
  )

return(corp_slim_dtm_df)
}

#custom function to create slim TDM from cleaned corpus, merge with sentiment dictionary, and compute weighted sentiment scores
#for the sentiment analysis, I create a TermDocumentMatrix instead of DTM so terms are in rows for easier computation of word counts and frequencies
#this function creates a TDM of only unigrams (sentiment dictionaries are based on words not phrases)
#a less harsh sparsity threshold is used because only a fraction of the words in the corpora will match with words in the sentiment dictionaries and being too harsh might lead to very few terms leftover
#the slim DTM is converted to data frame using tidy() which also computes word counts an merged with the Bing sentiment dictionary (this dictionary retained most number of terms) with an inner_join
#the sentiment values from Bing are characters which are recoded as numeric values ('positive' = 1, 'negative' = -1)
#within each document, total word count is computed and then the frequency of each term (repeated words have higher frequencies)
#finally, the sentiment of each term is weighted by its frequency for a final weighted sentiment score averaged across all terms in a document (weighted mean sentiment)
create_tdm_sentiment <- function(clean_corpus_filt) {

  corp_tdm <- TermDocumentMatrix(clean_corpus_filt,
                                 control = list(
                                   tokenize = function(x) { 
                                     NGramTokenizer(x, 
                                                    Weka_control(min=1, max=1)) }
                                 )
  ) 
  corp_slim_tdm <- removeSparseTerms(corp_tdm, .997) 
  
  
sentiment_df <- tidy(corp_slim_tdm) %>%  
  inner_join(get_sentiments("bing"),by = c("term"="word")) %>% 
  mutate(sentiment = recode( 
    sentiment, "positive" = 1, "negative" = -1
  )) %>% 
    group_by(document) %>% 
    mutate(total_count = sum(count),
           word_freqp = count/total_count) %>% 
    summarise(wt_sentiment = mean(word_freqp*sentiment)) %>% 
    ungroup() %>% 
    mutate(EmployeeID = as.numeric(document), .keep ="unused") 
  
  
  return(sentiment_df)
  
}


## Apply clean tokenize function to pos, neg, and overall corpora
pos_rev_clean_corpus <- clean_tokenize_text(pos_rev_corpus) 
neg_rev_clean_corpus <- clean_tokenize_text(neg_rev_corpus)
overall_rev_clean_corpus <- clean_tokenize_text(overall_rev_corpus)


## Create DTM using overall corpus only (combined positive and negative)
overall_rev_bigram_dtm_df <- create_bigram_dtm(overall_rev_clean_corpus) 
#159 terms using 98% sparsity
#335 using 99

## Compute weighted mean sentiment scores using positive and negative reviews 
#mean weighted positive sentiment is .54 which indicates responses to this item were generally positive
pos_rev_sentiment_df <- create_tdm_sentiment(pos_rev_clean_corpus) %>% 
  rename(PosReviewSentWt = wt_sentiment)
#mean(pos_rev_sentiment_df$PosReviewSentWt) mean=.54

#repeat for negative reviews
#mean weighted negative sentiment is .05 which is not negative but much lower than positive mean which indicates these reviews were more negative
#looking at both pos and neg columns, the limitation of term sentiment analysis is noticeable. Given a TDM was used, the sentiment analysis cannot take into account context around a word
#for some of the positive review records, a negative value of sentiment is assigned. ex "cost is never a concern"
#overall, sentiment analysis did the job and the more positive mean for the supposedly positive reviews is supported so I am retaining these variables to use in the classification model as predictors of turnover
neg_rev_sentiment_df <- create_tdm_sentiment(neg_rev_clean_corpus) %>% 
  rename(NegReviweSentWt = wt_sentiment)
#mean(neg_rev_sentiment_df$NegReviweSentWt) #mean=0.05, not negative but much lower than positive mean

## Add text-based predictors (overall DTM and computed positive and negative sentiment) to original dataset using left joins 

full_dat_tidy <- full_dat %>% 
  left_join(pos_rev_sentiment_df, by = "EmployeeID") %>% 
  left_join(neg_rev_sentiment_df, by = "EmployeeID") %>% 
  left_join(overall_rev_bigram_dtm_df, by = "EmployeeID") 



## Part 2 - Run classification ML models to predict turnover 


## Create dataset containing only relevant predictors and outcomes and dummy code all categorical variables

#certain variables are removed. e.g., employee ID is not a relevant predictor. Over18 is a factor with only 1 level which leads to errors in ML model
#also exclude unprocessed text data 
full_dat_tidy_ml <- full_dat_tidy %>% 
  select(-c(EmployeeID, Over18, PosReview, NegReview, OverallReview))
  

#frequency of turnover in observed data. overall base rate of attrition is low.
prop.table(table(full_dat_tidy_ml$Attrition))
# No       Yes 
# 0.8387755 0.1612245 

#For all categorical variables (currently stored as factors), I dummy code them using caret::dummyVars()
#The outcome, Attrition, can be left as a factor variable 
#To dummy code, I use caret function dummyVars() which identifies the non-numeric predictor variables and creates dummy variables
full_dat_tidy_dummy <- dummyVars(Attrition~., data = full_dat_tidy_ml)
#update dataset with dummy vars
full_dat_tidy_dummy_final <- as_tibble(predict(full_dat_tidy_dummy, newdata = full_dat_tidy_ml))
#outcome is dropped from the dummy variable object, add it back
full_dat_tidy_dummy_final <- cbind(Attrition = full_dat_tidy_ml$Attrition, full_dat_tidy_dummy_final)

## Create 2 datasets, one with text-based predictors and one without
#exclude all text-based variables 
full_dat_ml_no_text <- full_dat_tidy_dummy_final %>% 
  select(-c(PosReviewSentWt, NegReviweSentWt, names(overall_rev_bigram_dtm_df)[-1] )) 

#include text-based variables
full_dat_ml_text <- full_dat_tidy_dummy_final 


### Analysis

## Create test and train datasets to use across all models

#create 75/25 train test split
set.seed(24)
cv_index <- createDataPartition(full_dat_ml_text$Attrition, p = 0.75, list = FALSE)

#with text
train_data_txt <- full_dat_ml_text[cv_index,] 

#without text
train_data_no_txt <- full_dat_ml_no_text[cv_index,] 

#since test is only used to asses holdout accuracy, don't need to create text and no text versions
test_data <- full_dat_ml_text[-cv_index,]


## Run ML models across both datasets using multiple models to compare

#function to create test/train data and run ml models
#uses 10-fold cross-validiation 
#preprocessing steps remove zero variance variables, impute missing via median of existing data, and standardizes variables
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


##Use parallelization to speed up model training for more complex algorithms
local_cluster <- makeCluster(detectCores()-1) #7 cores on my machine
registerDoParallel(local_cluster)

#train models using text-based predictors, store computation time to be used as one of a few indicators when choosing 'best' model
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


model_txt_list <- list("Generalized Linear Model" = model_txt_glm,
                       "GLM Elastic Net" = model_txt_glmnet,
                      "Random Forest" =  model_txt_ranger,
                       "Extreme Gradient Boosting" = model_txt_xgbTree)

#train models excluding text-based predictors
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

model_no_txt_list <- list("Generalized Linear Model" = model_no_txt_glm,
                          "GLM Elastic Net" = model_no_txt_glmnet,
                          "Random Forest" =  model_no_txt_ranger,
                          "Extreme Gradient Boosting" = model_no_txt_xgbTree)


#turn off parallelization
stopCluster(local_cluster)
registerDoSEQ()

#take a peek at results and plot accuracies
#for both text and no text versions, random forest and extreme gradient boost models have increased accuracy
summary(resamples(model_txt_list))
summary(resamples(model_no_txt_list))
dotplot(resamples(model_no_txt_list))
dotplot(resamples(model_txt_list))


### Publication

#custom function to extract desired indicators of accuracy and other variables across models
#a confusion matrix (binary 2x2 classification) is created to extract additional indicators of model accuracy beyond kappa and simple accuracy, namely sensitivity and specificity
#the default way that R assigned levels to Attrition variable led to some confusion with the Sensitivity and Specificity indicators so I releveled it. Now, sensitivity refers to accuracy at predicting Yes events as it should.
#the final results table shows model name, cross-validated accuracy and holdout values of accuracy, kappa, sensitivity, and specifity. Each model's computation time (note paralellized processing) is also recorded 
get_ml_summary_results <- function(ml_model, test_data, model_time, model_name) {
 

  predicted_values <- predict(ml_model, test_data, na.action = na.pass)
  predicted_values <- factor(predicted_values, levels = c("Yes","No"))
  expected_values <- test_data$Attrition
  expected_values <- factor(expected_values, levels = c("Yes","No"))
  
  conf_mat <- confusionMatrix(data=predicted_values, reference=expected_values)

 results <- tibble(
    "Model" = model_name,
    "Cross-validated Accuracy" = str_remove(round(mean(ml_model[["results"]][["Accuracy"]]), 2), "^0"),
    "Cross-validated kappa" = str_remove(round(mean(ml_model[["results"]][["Kappa"]]),2), "^0"),
    "Holdout Accuracy" = str_remove(round(conf_mat$overall[1],  2), "^0"),
    "Holdout kappa" = str_remove(round(conf_mat$overall[2],   2), "^0"),
    "Sensitivity" = str_remove(round(conf_mat$byClass[1], 2), "^0"),
    "Specificity" = str_remove(round(conf_mat$byClass[2], 2), "^0"),
    "Computation time (seconds)" = paste(str_remove(round(model_time,2),"^0"),"seconds")
  )
   

  return(results)
  
}


#Apply above function to list of text-including and excluding models and save output to csv files
final_results_txt <- bind_rows(
  get_ml_summary_results(ml_model= model_txt_glm, test_data = test_data, model_time = time_model_txt_glm, model_name = "Generalized Linear Model"),
  get_ml_summary_results(ml_model= model_txt_glmnet,test_data = test_data, model_time = time_model_txt_glmnet, model_name ="GLM Elastic Net" ),
  get_ml_summary_results(ml_model= model_txt_ranger,test_data = test_data, model_time = time_model_txt_ranger, model_name = "Random Forest"),
  get_ml_summary_results(ml_model= model_txt_xgbTree, test_data = test_data,model_time = time_model_txt_xgbTree, model_name =  "Extreme Gradient Boosting"),
  
) %>% mutate("With Text?" = "Yes")

final_results_no_txt <- bind_rows(
  get_ml_summary_results(ml_model= model_no_txt_glm, test_data = test_data, model_time = time_model_no_txt_glm,model_name = "Generalized Linear Model"),
  get_ml_summary_results(ml_model= model_no_txt_glmnet,test_data = test_data, model_time = time_model_no_txt_glmnet, model_name = "GLM Elastic Net"),
  get_ml_summary_results(ml_model= model_no_txt_ranger,test_data = test_data, model_time = time_model_no_txt_ranger,model_name = "Random Forest"),
  get_ml_summary_results(ml_model= model_no_txt_xgbTree, test_data = test_data,model_time = time_model_no_txt_xgbTree,model_name = "Extreme Gradient Boosting"),
  
) %>%  mutate("With Text?" = "No")

#below code combines above tables and saves resulting overall table
final_results_combined <- bind_rows(final_results_no_txt,final_results_txt)
write_csv(final_results_combined, "../out/predictive_model_results.csv")


### Answers to Questions

#1) 

#overall summary table
final_results_combined


#What characteristics of how you created the final model likely made the biggest impact in maximizing its performance? How do you know? Be sure to interpret specific numbers in the table you just created.

#My final model is the Generalized Linear Model. This model had high mean accuracy in the cross-validated (.90 text or no text) and holdout samples (.84 and .86).
#While the random forest and extreme gradient boost models had  higher accuracy values in cross-validated samples, GLM performed just as well for holdout samples (both GLM and random forest .86 for no text) and had much faster computation times (9 vs 48 and 122 seconds for text models for GLM, random forest, and extreme gradient).
#Kappa is another accuracy indicator that takes chance into account. Random forest has the highest kappa (.80 with text and .86 without) in cross-validated samples but is outperformed by GLM for out-of-sample kappa in both text and no text cases
#Finally, the sensitivity (prediction of turnover) was generally lower than specificity (prediction of non-turnover) across all models but GLM had the higest or near highest values across both cases of including and excluding text.
#Even though random forest and extreme gradient boost models had higher specificity (predicting non-turnover), it seems more likely that we would want to more accurately predict those who choose to attrit as the worse outcome so we can prioritize sensitivity.
#The extreme gradient boost model had just as high or higher sensitivity compared to GLM but its computational time dampens its performance.
#A sensitivity of .44 (GLMnet value for both text and no text) can be interpreted as correctly predicting 44 out of 100 actual turnovers as opposed to just 27 out of 100 (example random forest sensitivity in text model)

#2)

#What is the incremental predictive accuracy gained by including text data in your model versus not including text data? 
final_results_combined %>% filter(Model == "Generalized Linear Model")

#The incremental predictive accuracy gained by including text data for the GLM has little support in this case. The cross-validated accuracy is the same (.9 for both) but the kappa for the model using textual data is slightly higher.
#The holdout accuracy for the no text model is higher (.86 vs .84) but the reverse is true for holdout kappa (.37 vs .41).
#The specificity of the GLM using textual data is also lower (.91 vs .94) and, most notably, the computation time for the text model is over four times greater for the model using text-derived predictors.
#Based on my logic above, predicting actual turnover (sensitivity) is important and the GLM performs the same in this category for both cases so including the information from processed review data like the sentiment scores does not improve this accuracy.
#All in all, adding the text-derived predictors, specifically the hundreds of variables added by including the slimmed DocumentTermMatrix does not noticeably improve the performanc of the GLM and minimizes its computational speed.
#It is possible that the information captured in the text-derived predictors is already included among other variables like the satisfaction ratings or there could be some biasing with people not honestly reporting how they feel on these open-ended items. 
#Turning to methodology, results could differ if we exclude the DTM approach and solely relied on the derived sentiment scores or used harsher sparsity etc. 
