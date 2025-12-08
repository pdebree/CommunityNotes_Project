# Modeling Status 

note_vars <- c("misleadingOther", "misleadingFactualError", "misleadingManipulatedMedia", 
               "misleadingOutdatedInformation", "misleadingMissingImportantContext", 
               "misleadingUnverifiedClaimAsFact", "misleadingSatire", "notMisleadingOther",               
               "notMisleadingFactuallyCorrect", "notMisleadingOutdatedButNotWhenWritten", 
               "notMisleadingClearlySatire", "notMisleadingPersonalOpinion", "trustworthySources", 
               "isMediaNote") #, "misinformed_or_misleading")              

rating_vars <- c("helpfulClear", "helpfulGoodSources", "helpfulUnbiasedLanguage", "notHelpfulIncorrect", 
                 "notHelpfulSourcesMissingOrUnreliable", "notHelpfulHardToUnderstand", 
                 "notHelpfulArgumentativeOrBiased", "notHelpfulSpamHarassmentOrAbuse", 
                 "notHelpfulOpinionSpeculation", "helpfulnessScore")

# Note: we could make better predictions for output with this data. We could find, for example, a metric of inflamatory language, using NLP methods but that is not the aim her. The aim is to compare classification outputs based only on the notes and ratings information. There is likely a large advantage in using LLMs because they may inherently capture this. 

ready_human_notes_ratings <- function(notes, ratings) {
  
  # we have to split the categorical variable helpfulness level into three - as this is how our grok output looks (plus this lets us have the reference variable).- we do not encode "NOT_HELPFUL" because this is our reference.
  ratings_meaned <- ratings %>% mutate(helpfulness_score = case_when(
    helpfulnessLevel == "HELPFUL" ~ 1, 
    helpfulnessLevel == "SOMEWHAT_HELPFUL" ~ 0.5,
    TRUE ~ 0)) %>% group_by(noteId) %>% summarize(
    across(c("helpfulClear", "helpfulGoodSources", "helpfulUnbiasedLanguage", "notHelpfulIncorrect", "notHelpfulSourcesMissingOrUnreliable", "notHelpfulHardToUnderstand", "notHelpfulArgumentativeOrBiased", "notHelpfulSpamHarassmentOrAbuse", "notHelpfulOpinionSpeculation"), mean, na.rm = TRUE),
    helpfulnessScore=mean(helpfulness_score))
  
  # We keep the note writer's ideas about their note because this may interact with the note ratings. Ie. If both the raters and the note write think the original tweet is misleading this is more likely to end up with a LockedStatus than if there is a discrepancy in opinion. 
  
  # Only missing values found in the topic assignment row. 
  
  notes_output <- notes %>% mutate(
    # Turn into dummy variable
    misinformed_or_misleading = ifelse(classification == "MISINFORMED_OR_POTENTIALLY_MISLEADING",1,0),
    # Turn output variable into dummy variable
    locked_helpful = ifelse(lockedStatus == "CURRENTLY_RATED_HELPFUL",1,0)
  ) %>% dplyr::select(
    # remove outputs that are not relevant to 
    -tweetId, -noteAuthorParticipantId, -classification, -month_year,
    -has_grok_output, -scams, -messi_ronaldo, -gaza_conflict, -ukraine_conflict, 
    -note_text, -currentDecidedBy, -lockedStatus, -time_created, -keyATM_topic)
  
  # need to make sure that the grok_output noteIds are in a separate test set, so that we can compare how they perform compared to the 
  # training - we can't have them in the training set.
  
  # We keep noteId in this set (though it will not be used in the modeling) for data wrangling and comparison of behavior with the different groups.
  
  # We take the mean of all the ratings variables (except noteId), but have a more specific method for finding the average of the helpfulness level. This is because there are three categories. And on the *****REFERENCE***** it encodes this as 1, 0.5 and 0. We will do the same thing to our g
  
  
  rated_notes <- left_join(notes_output, ratings_meaned, by="noteId")
  

  return(rated_notes)
}

ready_grok <- function(llm_ratings, notes){
  llm_ratings_output <- llm_ratings %>% 
    # Turn logical values into numerics
    mutate(across(where(is.logical), as.integer), 
           helpfulnessScore = case_when(
             helpfulnessLevel_helpful == 1 ~ 1, 
             helpfulnessLevel_somewhathelpful == 1 ~ 0.5, 
             TRUE ~ 0 
           )) %>% dplyr::select(-helpfulnessLevel_helpful, - helpfulnessLevel_somewhathelpful, - helpfulnessLevel_nothelpful)
  
    # add in Trustworthy Source variable 
  llm_ratings_output_joined <- left_join(llm_ratings_output, notes %>% dplyr::select(noteId, trustworthySources), by="noteId", )
  
  return(llm_ratings_output_joined)
}


get_llm_note_ids <- function(grok_outputs, rated_notes) {
  # We are going to save all notes that were rated by grok (and randomly selected) to be 
  # in our test set. (There will still be ukraine values in the training set.)
  # The subset is small enough that we feel that the ukraine and gaza notees are still 
  # represented fairly proportionatley in the ratining and test sets generally. 

  
  output <- list(non_grok_rated_notes =rated_notes %>% filter(!(noteId %in% grok_outputs[["noteId"]])),
                   grok_rated_notes=rated_notes %>% filter(noteId %in% grok_outputs[["noteId"]]))
  return(output)
}


fit_log_rf <- function(notes_data){
  if(isFALSE(tibble::is_tibble(notes_data))){
    stop("restaurant_data should be a tibble")
  }
  set.seed(123) # For reproducibility
  
  train_ratio <- 0.7 
  smp_size <- floor(train_ratio * nrow(notes_data))
  
  train_indices <- sample(seq_len(nrow(notes_data)), size = smp_size)
  
  train <- notes_data[train_indices, ] %>% dplyr::select(locked_helpful, trustworthySources, all_of(rating_vars)) 
  test <- notes_data[-train_indices, ] %>% dplyr::select(locked_helpful, trustworthySources, all_of(rating_vars))
  
  outcomes <- test$locked_helpful
  
  # logistic regression  
  fit.lm <- glm(locked_helpful ~ ., data=train, family="binomial")
  logistic_predictions <- predict(fit.lm, newdata = test, type = "response")
  rocr_prediction_test <- ROCR::prediction(logistic_predictions, outcomes)
  auc_logistic <- c(performance(rocr_prediction_test, "auc")@y.values[[1]])
  
  # random forest
  fit.rf <- ranger(locked_helpful ~ .,
                   data = train, 
                   num.trees = 50,
                   respect.unordered.factors = TRUE, 
                   probability = TRUE)
  
  rf_pred <- predict(fit.rf, data = test, type="response")
  rf_predictions <- rf_pred$predictions[,1]
  
  rocr_prediction_test <- ROCR::prediction(rf_predictions, outcomes)
  auc_rf <- c(performance(rocr_prediction_test, "auc")@y.values[[1]])
  
  # output 
  output <- list(outcomes, logistic_predictions, rf_predictions, auc_logistic, auc_rf, fit.lm, fit.rf)
  names(output) <- c("outcomes", "logistic_predictions", "rf_predictions", "auc_logistic", "auc_rf", "log_model", "rf_model")
  
  return(output)
  
}


find_diffs <- function(ratings_frame_combined){
  
  rating_diffs <- ratings_frame_combined %>%
    # get values that start with the base names
    dplyr::select(starts_with(helpfulness_names), noteId, topic) %>% 
    # 
    mutate(across(
      .cols = all_of(paste0(helpfulness_names, ".human")), 
      .fns = ~ . - get(gsub("\\.human$", ".grok", cur_column())), 
      .names = "diff.{gsub('.human$', '', col)}" 
    ),
    # create sum of squares for row
    sum_o_squares = rowSums(across(all_of(starts_with("diff")), ~ .^2))) %>% 
    
    # Select only the newly created difference columns
    dplyr::select(starts_with("diff."), noteId, sum_o_squares, topic)
  
  return(rating_diffs)
  
}


