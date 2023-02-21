# Load necessary libraries
library(stringr)

# Set the path to the folder containing the text files
folder_path <- "dataset"

# Get a list of all text files in the folder
file_list <- list.files(folder_path)
text_id <- 0

for (file in file_list) {
  
  # Set the file path to the input text file
  input_file <- paste0("dataset/",file)
  
  text_id <- text_id + 1
  
  # Read in the text file as a single character string
  text <- readChar(input_file, file.info(input_file)$size)
  
  # Split the text into paragraphs using regex
  paragraphs <- str_split(text, "\\n\\n")
  
  if (text_id == 1) {
    df <- data.frame(
              doc_id = 1:(length(unlist(paragraphs))-2), 
              text_id = text_id,
              title = str_split_fixed(text, "\\n\\n", 3)[,1], 
              date = str_split_fixed(text, "\\n\\n", 3)[,2], 
              text = unlist(paragraphs)[-c(1:2)])
  }
  
  # Create a data frame with the paragraph numbers and text
  df <- rbind(df, data.frame(
            doc_id = 1:(length(unlist(paragraphs))-2), 
            text_id = text_id,
            title = str_split_fixed(text, "\\n\\n", 3)[,1], 
            date = str_split_fixed(text, "\\n\\n", 3)[,2], 
            text = unlist(paragraphs)[-c(1:2)]))
  
  # Write the data frame to a CSV file
  write.table(df, "paragraphs.csv", row.names = FALSE, append = TRUE)
}





