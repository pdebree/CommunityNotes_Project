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

text_preprocessing <- function(notes_text) {
  
  custom_stopwords <- c(
    "quote", "video", "account", "accounts", "post", "claim", "use", "quotes", "image")
  
  # Create tokens  - could look at making sub-words?
  data_tokens <- quanteda::tokens(
    full_notes_text,
    # Preprocessing Steps 
    remove_numbers = TRUE,
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_separators = TRUE, 
    remove_url = TRUE
  ) %>%
    # make all tokens lower case
    tokens_tolower() %>%
    # removes list of stop words imported from the quanteda package. 
    tokens_remove(
      c(
        quanteda::stopwords("english"), custom_stopwords
      )
    ) %>%
    # stem words
    tokens_wordstem(
      language = quanteda_options("language_stemmer"),
      verbose = quanteda_options("verbose")
    ) %>% 
    # removes tokens of length 2 or less
    tokens_select(min_nchar = 3) 
  
  # word_counts_vector <- colSums(data_dfm)
  # head(sort(word_counts_vector, decreasing = TRUE), 200)
  
  data_dfm <- dfm(data_tokens) %>%
    # min_termfreq - removes features that appear less than five times across all documents 
    # min_docfreq - removes features that appear in less than 2 distinct documents. 
    # get 4200 from looking at the data. 
    dfm_trim(min_termfreq = 5, min_docfreq = 2, max_termfreq = 3000)
  
  # find indices for rows that are not empty
  logical_empty <- unname(rowSums(data_dfm) > 0)
  
  
  # Subset dfm to only include those that do not have empty rows. 
  data_dfm_filtered <- dfm_subset(
    data_dfm,
    logical_empty
  )
  
  docs <- keyATM_read(
    texts = data_dfm_filtered)
  
  output <- list(docs=docs, logical_empty=logical_empty)
  return(output)
  
}
  
  
assign_topics <- function(notes_frame, keyATM_output, non_empty_logical) {
    # pull out the proportions of appearance in each topic group (out of the 10)
    theta_matrix <- keyATM_output$theta
    
    # Find the column in which the highest likelihood topic appears. 
    highest_prop_topic <- apply(theta_matrix, 1, which.max)
    
    # pull out topic names (this let's us see our seed topics)
    topic_labels <- colnames(theta_matrix)
    
    # assign indices only to documents that were used in the topic modeling 
    # remember we omitted some!
    assigned_topics_name_filtered <- topic_labels[highest_prop_topic]
    
    
    non_empty_doc_indices <- which(non_empty_logical)
    
    # create column of NA values 
    notes_frame$keyATM_topic <- NA
    
    # Assign the topic group the documents (based on their indices)
    notes_frame[non_empty_doc_indices, "keyATM_topic"] <- assigned_topics_name_filtered
    
    return(notes_frame)
  }
  



