# R Script containing functions for cleaning Community Notes Data 

library(tidyverse)
# For language input
library(ellmer)
library(cld2)
library(stringr)
library(openai)

remove_depreciated_notes_values <- function(notes){
  return(notes %>% select(-harmful, -believable, -validationDifficulty))
}

make_notes_times <- function(notes_data){
  # Takes in a data frame of community notes and returns a data frame with cleaned time attributes
  return(notes_data %>% mutate(
    note_text = summary,
    time_created = as.POSIXct(createdAtMillis/1000, origin = "1970-01-01", tz = "UTC"),
    month_year = format(time_created, "%Y-%m")) %>%
      dplyr::select(-summary, -createdAtMillis)) 
  
}

make_ratings_times <- function(ratings_data){
  # Takes in a data frame of community notes ratings and returns a data frame with cleaned time attributes
  return(ratings_data %>% mutate(
    time_created = as.POSIXct(createdAtMillis/1000, origin = "1970-01-01", tz = "UTC"), 
    month_year = format(time_created, "%Y-%m")) %>% 
      dplyr::select(-createdAtMillis))
}


make_notes_history_times <- function(notes_status_history_data){
  # Takes in a data frame of community notes history and returns a data frame with cleaned time attributes
  
  return(notes_status_history <- notes_status_history_data %>% mutate(
    time_created = as.POSIXct(createdAtMillis/1000, origin = "1970-01-01", tz = "UTC"),
    timestamp_final_scoring_output = as.POSIXct(
      timestampMinuteOfFinalScoringOutput/1000, origin = "1970-01-01", tz = "UTC"), 
    timestamp_most_recent_change =  as.POSIXct(
      timestampMillisOfMostRecentStatusChange/1000, origin = "1970-01-01", tz = "UTC")
  ) %>% 
    dplyr::select(-createdAtMillis, -timestampMinuteOfFinalScoringOutput, -timestampMillisOfMostRecentStatusChange))
}

make_user_enrollment_status_times <- function(user_enrollment_status_data) {
  return(user_enrollment_status_data %>% 
           mutate(
             time_last_earned_out = as.POSIXct(timestampOfLastEarnOut/1000, origin = "1970-01-01", tz = "UTC"),
             time_last_state_change = as.POSIXct(timestampOfLastStateChange/1000, origin = "1970-01-01", tz = "UTC")) 
         %>% dplyr::select(-timestampOfLastEarnOut, -timestampOfLastStateChange))
}


make_bat_signals_times <- function(bat_signals_data) {
  return(bat_signals_data %>% mutate(
    time_created = as.POSIXct(createdAtMillis/1000, origin = "1970-01-01", tz = "UTC")) %>% 
      dplyr::select(-createdAtMillis))
  
}


remove_missing_notes <- function(notes_data) {
  return(notes_data %>% filter(!is.na(note_text)))
}

remove_missing_noteId <- function(notes_data){
  
  # required that the dataframe passed to this has a noteId vector 
  # Throw exception here? 
  
  # Find Note Ids for Missing Values 
  duplicated_noteIds <- notes_data %>% filter(duplicated(noteId)) %>% pull(noteId)
  # Remove the duplicate row ids - this removes 8 rows from the dataset. 
  notes_data <- notes_data %>% filter(!(noteId %in%duplicated_noteIds))
  
  return(notes_data)
}


limit_dates <- function(notes){
  # assertion that month_year appears in the dataset
  # can take either the notes dataframe or the ratings dataframe.
  return(notes %>% filter(month_year > "2022-11", month_year < "2025-10"))
}



limit_nonnmr_notes <- function(notes_status_history_data) {
  # first we limit to only notes with a firstNonNMRStatus values - to ensure we have only notes that have enough note
  notes_status <- notes_status_history_data %>% 
    # get rid of rows that only have needs more ratings values
    filter(!is.na(firstNonNMRStatus)) %>% 
    # get lockedStatus and noteId and filter to only conclusive outputs - take currentDecidedBy
    select(lockedStatus, noteId, currentDecidedBy) %>% filter(
      # limit to only notes "CURRENTLY_RATED_NOT_HELPFUL" or "CURRENTLY_RATED_HELPFUL
      lockedStatus %in% c("CURRENTLY_RATED_NOT_HELPFUL", "CURRENTLY_RATED_HELPFUL"))
  return(notes_status)
}


limit_english <- function(notes_data) {
  
  notes_lang <- notes_data %>% mutate(
    # detect language of the note 
    is_english = 
      ifelse(cld2::detect_language(note_text) == "en", TRUE, FALSE)) %>% 
    # select only rows in english and then remove the column
    filter(is_english) %>% select(-is_english)
  
  return(notes_lang)
  
}


clean_notes <- function(notes) {
  notes <- remove_missing_notes(notes)
  notes <- remove_missing_noteId(notes)
  notes <- remove_depreciated_notes_values(notes)
  return(notes)
}

clean_notes_history <- function(notes_status_history){
  notes_status_history <- remove_missing_noteId(notes_status_history)
  return(notes_status_history)
}


clean_bat_signals <- function(bat_signals_data) {
  # tweetIds should. be 19 characters long
  bat_signals_data <- bat_signals_data %>% filter(nchar(tweetId) == 19) %>% group_by(tweetId) %>% summarize(sources = list(sourceLink[!is.na(sourceLink)])) %>% ungroup()
  
  return(bat_signals_data)
}


# Combine the notes and notes_status_history to make a dataset of only valid notes 
get_valid_notes <- function(notes_data, notes_status_history){
  notes_data <- limit_dates(notes_data)
  notes_status <- limit_nonnmr_notes(notes_status_history)
  notes_valid <- inner_join(notes_data, notes_status, by="noteId")
  notes_valid <- limit_english(notes_valid)
  return(notes_valid)
}

ratings_reader <- function(single_ratings_filepath, valid_note_ids){
  temp_table <- fread(single_ratings_filepath, 
                      sep = "\t", 
                      # We drop the non-relevant rating values - "ratingSourceBucketed", "version", "agree", "disagree" and 
                      # "raterParticipantId" and "ratedOnTweetID"
                      # a note on rater participant ID (this would be really useful in further evals - with nuance into the raters) 
                      # we do the filtering during the read in with the hopes that this will make the amount of data more reasonable
                      select=c("noteId", "createdAtMillis", "helpfulnessLevel", "helpfulClear", "helpfulGoodSources",
                               "helpfulUnbiasedLanguage", "notHelpfulIncorrect", "notHelpfulSourcesMissingOrUnreliable", 
                               "notHelpfulHardToUnderstand", "notHelpfulArgumentativeOrBiased", "notHelpfulSpamHarassmentOrAbuse",
                               "notHelpfulOpinionSpeculation", "raterParticipantId", "ratedOnTweetId"))
  
  table <- temp_table %>% 
    # make month data - and remove non-relevant data files
    mutate(
      time_created = as.POSIXct(createdAtMillis/1000, origin = "1970-01-01", tz = "UTC"), 
      month_year = format(time_created, "%Y-%m")) %>% select(-createdAtMillis, -time_created) %>% 
    # filter on as much as possible to make the search for relevant notes on as small a subset as possible
    filter(month_year > "2022-11", month_year < "2025-10") %>% 
    # get only the relevant noteId ratings 
    filter(noteId %in% valid_note_ids)
  return(table)
}


read_in_ratings <- function(valid_notes) {
  # This function does the read in for the ratings - we have decided to explicitly do this in such a way that the amount of data we have to read in is limited (because there is so much data.
  # We do lose the ability to do some inference about the dataset as a whole as a result of this choice. 
  
  # this is hard coded for the data to be in a directory within the data directory that contains only the ratings data. 
  ratings_file_paths <- list.files(
    path = "data/ratings_data",
    pattern = "\\.tsv$",
    full.names = TRUE)
  
  # apply the ratings_reader function to all of the .tsv files while reading in (limit the rows and columns on read in)
  data_list <- lapply(
    ratings_file_paths,
    function(file_path) {
      ratings_reader(file_path, valid_note_ids = valid_notes_ids)
    })
  
  return(rbindlist(data_list, use.names = TRUE))
}


