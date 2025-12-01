# Holds code for topic modeling within the community notes data

Topics.UkraineConflict = c(
  "ukrain",  
  "russia",
  "kiev",
  "kyiv",
  "moscow",
  "zelensky",
  "putin")

Topics.GazaConflict =  c(
  "israel",
  "palestin",  # intentionally shortened for expanded matching
  "gaza",
  "jerusalem",
  "hamas")

Topics.MessiRonaldo = c(
  "messi",  # intentional whitespace to prevent prefix matches
  "ronaldo")

Topics.Scams = c(
  "scam",
  "undisclosed ad",  # intentional whitespace
  "terms of service",  # intentional whitespace
  "help.x.com",
  "x.com/tos",
  "engagement farm",  # intentional whitespace
  "spam",
  "gambling",
  "apostas",
  "apuestas",
  "dropship",
  "drop ship",  # intentional whitespace
  "promotion")


limit_to_four_topics <- function(notes){
  notes <- notes %>% rowwise() %>% filter(sum(ukraine_conflict, gaza_conflict, messi_ronaldo, scams) > 0) %>% ungroup()
  return(notes)
}

limit_gaza_ukraine <- function(notes){
  notes <- notes %>% rowwise() %>% filter(sum(ukraine_conflict, gaza_conflict) > 0) %>% ungroup()
  return(notes)
}

mark_topics <- function(notes) {
  # Needs to take in a dataframe that has the note_text column. 
  
  ukraine_pattern <- paste(Topics.UkraineConflict, collapse = "|")
  gaza_pattern <-  paste(Topics.GazaConflict, collapse = "|")
  messi_pattern <-  paste(Topics.MessiRonaldo, collapse = "|")
  scams_pattern <-  paste(Topics.Scams, collapse = "|")
  
  notes <- notes %>% mutate(
    ukraine_conflict = grepl(pattern = ukraine_pattern, x = note_text, ignore.case = TRUE),
    gaza_conflict = grepl(pattern = gaza_pattern, x = note_text, ignore.case = TRUE),
    messi_ronaldo = grepl(pattern = messi_pattern, x = note_text, ignore.case = TRUE),
    scams = grepl(pattern = scams_pattern, x = note_text, ignore.case = TRUE))
  
  return(notes)
}




