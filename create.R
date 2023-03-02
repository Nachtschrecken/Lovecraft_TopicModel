# This file creates the csv out of the provided text files
#
# Text files should contain the following in direct order:
# TITLE, followed by empty line
# Date, followed by empty line
# Raw Text, paragraphs seperated by empty lines

library(stringr)

# Structure to load all files
folder_path <- "dataset"
file_list <- list.files(folder_path)
text_id <- 0

# basic structure for data frames
df <- data.frame(
          text_id=character(),
          title=character(), 
          date=character(), 
          text=character())

# For loop over all text files
for (file in file_list) {
  
  # Set the file path to the input text file
  input_file <- paste0("dataset/",file)
  
  # text_id column
  text_id <- text_id + 1
  
  # Read in the text file as a single character string
  text <- readChar(input_file, file.info(input_file)$size)
  
  # Split the text into paragraphs using regex
  paragraphs <- str_split(text, "\\n\\n")
  
  # Create a data frame with the paragraph numbers and text
  df <- rbind(df, data.frame(
            text_id = text_id,
            title = str_split_fixed(text, "\\n\\n", 3)[,1], 
            date = str_split_fixed(text, "\\n\\n", 3)[,2], 
            text = unlist(paragraphs)[-c(1:2)]))
}

# create ids for csv
doc_id = c(1:3896)

# append ids to dataframe
df$doc_id <- doc_id
df <- df[, c(5,1,2,3,4)]

# Write the data frame to a CSV file
write.csv(df, "paragraphs.csv", row.names = FALSE)