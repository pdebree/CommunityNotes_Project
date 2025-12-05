# Holds code for topic modeling within the community notes data

Topics.UkraineConflict = c(
  "ukrain",
  "russia",
  "kiev",
  "kyiv",
  "moscow",
  #"zelensky",
  "putin")

Topics.GazaConflict =  c(
  "israel",
  "palestin",  # intentionally shortened for expanded matching
  "gaza",
  "jerusalem",
  "hama") # stemmed to hama - questionable

Topics.MessiRonaldo = c(
  "messi",  # intentional whitespace to prevent prefix matches
  "ronaldo")

Topics.Scams = c(
  #"scam",
  #"undisclosed",  # intentional whitespace
  "term",  # intentional whitespace
  #"help.x.com",
  #"x.com/tos",
  #"engag",  # intentional whitespace
  "farm",
  #"spam",
  #"gambl",
  #"apostas",
  #"apuestas",
  "dropship")
  #"drop ship",  # intentional whitespace
#promotion")



keywords <- list(
  Ukraine = Topics.UkraineConflict,
  Gaza = Topics.GazaConflict,
  Messi = Topics.MessiRonaldo,
  Scams = Topics.Scams
)

# keywords for only gaza and ukraine
keywords_gzuk <- list(
  Ukraine = Topics.UkraineConflict,
  Gaza = Topics.GazaConflict
)




limit_to_four_topics <- function(notes){
  # Limits notes to only those that contain the seed terms for the four topics
  notes <- notes %>% rowwise() %>% filter(sum(ukraine_conflict, gaza_conflict, messi_ronaldo, scams) > 0) %>% ungroup()
  return(notes)
}

limit_gaza_ukraine <- function(notes){
  # Limits notes to only those that contain the seed terms for the the ukraine and gaza conflicts
  notes <- notes %>% rowwise() %>% filter(sum(ukraine_conflict, gaza_conflict) > 0) %>% ungroup()
  return(notes)
}

mark_topics <- function(notes) {
  # Marks notes that contain the seed terms for the four topcis
  
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




