TextCleaning <- function(filename,location) {
  
  #calling all the required libraries
  library(rvest)
  library(dplyr)
  library(tm)
  library(wordcloud)
  library(wordcloud2)
  library(syuzhet)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(reshape2)
  library(dplyr)
  library(tidyr)
  library(tidytext)
  
  #use the fullcontent file location direct from the webscraper return and use it here. 
  #Corpus function will read the content from all .csv files in the specified location and creates a corpus 
  textcontent <- Corpus(DirSource(paste(location,"/content",sep="")))
  inspect(textcontent)
  
  #the tm_map function performs various functions to transform the text:-
  #to lowercase
  textcontent <- tm_map(textcontent, tolower)
  
  #removes all the punctuation 
  textcontent <- tm_map(textcontent, removePunctuation)
  
  #removes numbers
  textcontent <- tm_map(textcontent, removeNumbers)
  
  #removes a set of stopwords like in, a, an by specifying the type like 'english'
  textcontent <- tm_map(textcontent, removeWords, stopwords('english'))
  
  #removeUrl function removes all the links from the document like https://
  removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
  textcontent <- tm_map(textcontent, content_transformer(removeURL))
  
  #removes words that don't make sense or are incomplete 
  textcontent <- tm_map(textcontent, removeWords, c('aapl', 'apple'))
  
  #replacing meaningless words
  textcontent <- tm_map(textcontent, gsub, 
                        pattern = 'stocks', 
                        replacement = 'stock')
  
  #removing every unnecessary spacing between words and sentences
  textcontent <- tm_map(textcontent, stripWhitespace)
  inspect(textcontent)
  
  #saving the cleaned data on the device
  outfile = paste0(location,"CleanedStockOutput_", format(Sys.time(), "%d-%b-%Y %H.%M"), ".txt")
  writeLines(as.character(textcontent), outfile)
  
  outfile
}