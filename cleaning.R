# R Script containing functions for cleaning Community Notes Data 

library(tidyverse)
# For language input
library(ellmer)
library(cld2)
library(stringr)
library(openai)

remove_depreciated_notes_values <- function(notes){
  # Removes columns that are not relevant in our subset of data
  return(notes %>% dplyr::select(-harmful, -believable, -validationDifficulty))
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
  # Takes in a data frame of enrollment status for Community Notes users and returns a data frame with cleaned time attributes
  return(user_enrollment_status_data %>% 
           mutate(
             time_last_earned_out = as.POSIXct(timestampOfLastEarnOut/1000, origin = "1970-01-01", tz = "UTC"),
             time_last_state_change = as.POSIXct(timestampOfLastStateChange/1000, origin = "1970-01-01", tz = "UTC")) 
         %>% dplyr::select(-timestampOfLastEarnOut, -timestampOfLastStateChange))
}


make_bat_signals_times <- function(bat_signals_data) {
  # Takes in a data frame of requests for Community Notes and returns a data frame with cleaned time attributes
  return(bat_signals_data %>% mutate(
    time_created = as.POSIXct(createdAtMillis/1000, origin = "1970-01-01", tz = "UTC")) %>% 
      dplyr::select(-createdAtMillis))
  
}


remove_missing_notes <- function(notes_data) {
  # Removes data points with missing text
  return(notes_data %>% filter(!is.na(note_text)))
}

remove_missing_noteId <- function(notes_data){
  # Find Note Ids for Missing Values 
  duplicated_noteIds <- notes_data %>% filter(duplicated(noteId)) %>% pull(noteId)
  # Remove the duplicate row ids - this removes 8 rows from the dataset. 
  notes_data <- notes_data %>% filter(!(noteId %in%duplicated_noteIds))
  return(notes_data)
}


limit_dates <- function(notes){
  # assertion that month_year appears in the dataset
  # can take either the notes data frame or the ratings data frame.
  return(notes %>% filter(month_year > "2022-11", month_year < "2025-10"))
}



limit_nonnmr_notes <- function(notes_status_history_data) {
  # first we limit to only notes with a firstNonNMRStatus values - to ensure we have only notes that have enough note
  notes_status <- notes_status_history_data %>% 
    # get rid of rows that only have needs more ratings values
    filter(!is.na(firstNonNMRStatus)) %>% 
    # get lockedStatus and noteId and filter to only conclusive outputs - take currentDecidedBy
    dplyr::select(lockedStatus, noteId, currentDecidedBy) %>% filter(
      # limit to only notes "CURRENTLY_RATED_NOT_HELPFUL" or "CURRENTLY_RATED_HELPFUL
      lockedStatus %in% c("CURRENTLY_RATED_NOT_HELPFUL", "CURRENTLY_RATED_HELPFUL"))
  return(notes_status)
}


limit_english <- function(notes_data) {
  # Using cld2 (a google language detection model) limits the notes to only those 
  # that can be reasonably assumed to be in English.
  notes_lang <- notes_data %>% mutate(
    # detect language of the note 
    is_english = 
      ifelse(cld2::detect_language(note_text) == "en", TRUE, FALSE)) %>% 
    # select only rows in english and then remove the column
    filter(is_english) %>% dplyr::select(-is_english)
  
  return(notes_lang)
  
}


clean_notes <- function(notes) {
  # Does a range of cleaning tasks for the notes data frame
  notes <- remove_missing_notes(notes)
  notes <- remove_missing_noteId(notes)
  notes <- remove_depreciated_notes_values(notes)
  return(notes)
}

clean_notes_history <- function(notes_status_history){
  # Does a range of cleaning tasks for the notes_status_history data frame
  notes_status_history <- remove_missing_noteId(notes_status_history)
  return(notes_status_history)
}


clean_bat_signals <- function(bat_signals_data) {
  # Does a range of cleaning tasks for the notes_status_history data frame
  # tweetIds should. be 19 characters long
  bat_signals_data <- bat_signals_data %>% filter(nchar(tweetId) == 19) %>% 
    group_by(tweetId) %>% summarize(sources = list(sourceLink[!is.na(sourceLink)])) %>% ungroup()
  
  return(bat_signals_data)
}



get_valid_notes <- function(notes_data, notes_status_history){
  # Combine the notes and notes_status_history to make a dataset of only valid notes 
  # Does a range of cleaning tasks to do this work
  notes_data <- limit_dates(notes_data)
  notes_status <- limit_nonnmr_notes(notes_status_history)
  notes_valid <- inner_join(notes_data, notes_status, by="noteId")
  notes_valid <- limit_english(notes_valid)
  return(notes_valid)
}

ratings_reader <- function(single_ratings_filepath, valid_note_ids){
  # Function to read in a single ratings data frame (from its filepath)
  
  # Use the fread function for speed of processing 
  temp_table <- fread(single_ratings_filepath, 
                      sep = "\t", 
                      # We drop the non-relevant rating values - "ratingSourceBucketed", "version", "agree", "disagree" and 
                      # "raterParticipantId" and "ratedOnTweetID"
                      # The filtering is done during the read in with the hopes that this will make the amount of data more reasonable.
                      select=c("noteId", "createdAtMillis", "helpfulnessLevel", "helpfulClear", "helpfulGoodSources",
                               "helpfulUnbiasedLanguage", "notHelpfulIncorrect", "notHelpfulSourcesMissingOrUnreliable", 
                               "notHelpfulHardToUnderstand", "notHelpfulArgumentativeOrBiased", "notHelpfulSpamHarassmentOrAbuse",
                               "notHelpfulOpinionSpeculation", "raterParticipantId", "ratedOnTweetId"),
                      colClasses = c("noteId" = "character"))
  
  # Make alterations to the read in data for ease of use (includes filtering dates to limit size)
  table <- temp_table %>% 
    # Similar time variable management as found in the `make_notes_times()` function.
    mutate(
      time_created = as.POSIXct(createdAtMillis/1000, origin = "1970-01-01", tz = "UTC"), 
      month_year = format(time_created, "%Y-%m")
      ) %>% dplyr::select(-createdAtMillis, -time_created) %>% 
    # filter on as much as possible to make the search for relevant notes on as small a subset as possible
    filter(month_year > "2022-11" & month_year < "2025-11") %>%
    # get only the relevant noteId ratings
    filter(noteId %in% valid_note_ids)
  return(table)
}


read_in_ratings <- function(valid_notes_ids) {
  # This function does the read in for the ratings - iterating over the filepaths that hold the ratings data. 
    # We do lose the ability to do some inference about the dataset as a whole as a result of this choice. 
  
  # This is hard coded for the data to be in a `ratings_data` directory within the data directory. 
  ratings_file_paths <- list.files(
    path = "data/ratings_data",
    pattern = "\\.tsv$",
    full.names = TRUE)
  
  # Apply the ratings_reader function to all of the .tsv files while reading in (limit the rows and columns on read in)
  data_list <- lapply(
    ratings_file_paths,
    function(file_path) {
      ratings_reader(file_path, valid_note_ids = english_valid_notes_ids)
    })
  
  return(rbindlist(data_list, use.names = TRUE))
}


