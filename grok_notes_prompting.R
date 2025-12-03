# Library imports 
library(tidyverse)
library(httr2)
library(jsonlite)
library(usethis)

# Helper function to create a boolean property definition 
make_bool_prop <- function(desc, name) {
  # at one point name was necessary, when trying to make a fairly strict JSON output.
  list(type = "boolean", description = desc)
}

make_json_schema <- function() {
  
  # rating attributes to request from LLM
  relevant_rating_attributes <- c(
    "helpfulnessLevel_helpful", "helpfulnessLevel_somewhathelpful", "helpfulnessLevel_nothelpful", "helpfulClear",
    "helpfulGoodSources", "helpfulUnbiasedLanguage", "notHelpfulIncorrect", "notHelpfulSourcesMissingOrUnreliable",
    "notHelpfulHardToUnderstand", "notHelpfulArgumentativeOrBiased", "notHelpfulSpamHarassmentOrAbuse",
    "notHelpfulOpinionSpeculation")
  
  # create list of rating properties for llm to fill - pulled from Community Notes definitions
  rating_properties <- list()
  rating_properties$helpfulnessLevel_helpful <- make_bool_prop("TRUE if this note is Helpful",
                                                               name="helpfulnessLevel_helpful")
  rating_properties$helpfulnessLevel_somewhathelpful <- make_bool_prop("TRUE if the note is overall Somewhat Helpful",
                                                                       name="helpfulnessLevel_somewhathelpful")
  rating_properties$helpfulnessLevel_nothelpful <- make_bool_prop("TRUE if the note is overall Not Helpful",
                                                                  name="helpfulnessLevel_nothelpful")
  rating_properties$helpfulClear <- make_bool_prop("TRUE if the note is Clear and/or well-written", name="helpfulClear")
  rating_properties$helpfulGoodSources <- make_bool_prop("TRUE if the note Cites high-quality sources",
                                                         name="helpfulGoodSources")
  rating_properties$helpfulUnbiasedLanguage <- make_bool_prop("TRUE if the note has Neutral or unbiased language",
                                                              name="helpfulUnbiasedLanguage")
  rating_properties$notHelpfulIncorrect <- make_bool_prop("TRUE if the note contains Incorrect information",
                                                          name="notHelpfulIncorrect")
  rating_properties$notHelpfulSourcesMissingOrUnreliable <- make_bool_prop(
    "TRUE if the note has Sources missing or is unreliable", name="notHelpfulSourcesMissingOrUnreliable")
  rating_properties$notHelpfulHardToUnderstand <- make_bool_prop("TRUE if the note is Hard to understand",
                                                                 name="notHelpfulHardToUnderstand")
  rating_properties$notHelpfulArgumentativeOrBiased <- make_bool_prop(
    "TRUE if the note uses Argumentative or biased language", name="notHelpfulArgumentativeOrBiased")
  rating_properties$notHelpfulSpamHarassmentOrAbuse <- make_bool_prop(
    "TRUE if the note contains Spam, harassment, or abuse", name="notHelpfulSpamHarassmentOrAbuse")
  rating_properties$notHelpfulOpinionSpeculation <- make_bool_prop(
    "TRUE if the note is based on opinion or speculation, not fact.", name="notHelpfulOpinionSpeculation")
  
  # create JSON schema
  json_schema <- list(
    type = "object",
    `$schema` = "https://json-schema.org/draft/2020-12/schema",
    properties = rating_properties, 
    required = relevant_rating_attributes,
    title = "CommunityNoteRating", 
    description = "A set of boolean ratings for a Community Note."
  )
  
  return(json_schema)
}


make_user_prompt <- function(text) {
  # makes prompts from raw text 
  return(paste0(
    "**Community Note:** ", text, "\n\n"))}


make_request_message <- function(note_text_list) {
  # make a list object to hold messages (system prompt and user prompts)
  messages <- list(
    list(role = "system", content = system_prompt))
  
  # make new messages based on pass in user_prompts
  new_messages <- lapply(note_text_list, function(text) {
    list(role = "user", content = make_user_prompt(text))})
  messages <- c(messages, new_messages)
  return(messages)
  
}

perform_request <- function(model=grok_model, messages=notes_messages, api_key=grok_api_key, notes_json_schema){
  # create a request body 
  request_body <- list(
    model = grok_model,
    messages = notes_messages,
    json_schema = notes_json_schema, 
    mode = "json")
  
  # create the request
  grok_request <- request(grok_url) %>%
    req_method("POST") %>%
    req_headers(Authorization = paste("Bearer", api_key),
                Accept = "application/json") %>%
    req_body_json(request_body)
  
  # perform request and pull out JSON data 
  request_output <- req_perform(grok_request)

  return(request_output)
}
