# Regex to locate links in text
find_link <- regex("
  \\[   # Grab opening square bracket
  .+?   # Find smallest internal text as possible
  \\]   # Closing square bracket
  \\(   # Opening parenthesis
  .+?   # Link text, again as small as possible
  \\)   # Closing parenthesis
  ",
                   comments = TRUE)

# Function that removes links from text and replaces them with superscripts that are 
# referenced in an end-of-document list. 
sanitize_links <- function(text){
  if(PDF_EXPORT){
    str_extract_all(text, find_link) %>% 
      pluck(1) %>% 
      walk(function(link_from_text){
        title <- link_from_text %>% str_extract('\\[.+\\]') %>% str_remove_all('\\[|\\]') 
        link <- link_from_text %>% str_extract('\\(.+\\)') %>% str_remove_all('\\(|\\)')
        
        # add link to links array
        links <<- c(links, link)
        
        # Build replacement text
        new_text <- glue('{title}<sup>{length(links)}</sup>')
        
        # Replace text
        text <<- text %>% str_replace(fixed(link_from_text), new_text)
      })
  }
  text
}

# Take entire positions dataframe and removes the links 
# in descending order so links for the same position are
# right next to each other in number. 
strip_links_from_cols <- function(data, cols_to_strip){
  for(i in 1:nrow(data)){
    for(col in cols_to_strip){
      data[i, col] <- sanitize_links(data[i, col])
    }
  }
  data
}

# Take a position dataframe and the section id desired
# and prints the section to markdown. Updated to handle missing values with defaults.
print_section <- function(position_data, section_id){
  filtered_data <- position_data %>% 
    filter(section == section_id) %>%
    arrange(desc(end))
  
  if(nrow(filtered_data) == 0) {
    cat("No data found for section:", section_id, "\n")
    return(invisible(NULL))
  }
  
  for(i in 1:nrow(filtered_data)) {
    row <- filtered_data[i,]
    
    # Handling missing values with defaults
    title <- ifelse(is.na(row$title), "Title not available", row$title)
    loc <- ifelse(is.na(row$loc), "Location not available", row$loc)
    institution <- ifelse(is.na(row$institution), "Institution not available", row$institution)
    timeline <- if(is.na(row$start) || row$start == row$end) {
      ifelse(is.na(row$end), "Timeline not available", row$end)
    } else {
      paste(row$end, "-", row$start)
    }
    
    # Print section details
    cat("\n### ", title, "\n\n")
    cat(loc, "\n\n")
    cat(institution, "\n\n")
    cat(timeline, "\n\n")
    
    # Handle descriptions
    descriptions <- c(row$description_1, row$description_2, row$description_3)
    descriptions <- descriptions[!is.na(descriptions) & descriptions != ""]
    
    if(length(descriptions) > 0) {
      cat(paste0("- ", descriptions, collapse = "\n"), "\n\n")
    }
  }
}

# Construct a bar chart of skills
build_skill_bars <- function(skills, out_of = 5){
  bar_color <- "#969696"
  bar_background <- "#d9d9d9"
  skills %>% 
    mutate(width_percent = round(100 * level / out_of)) %>% 
    glue_data(
      "<div class = 'skill-bar'",
      "style = \"background:linear-gradient(to right,",
      "{bar_color} {width_percent}%,",
      "{bar_background} {width_percent}% 100%)\" >",
      "{skill}",
      "</div>"
    ) %>% 
    cat(sep = "\n")
}
