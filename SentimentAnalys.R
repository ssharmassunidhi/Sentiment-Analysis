SentimentAnalys <- function(filename,location) {
  
  #calling all the required libraries
  library(tm)
  library(wordcloud)
  library(stringr)
  library(ggplot2)
  
  #this function will take the location and the filename of the cleaned file
  #filename2 <- paste(location2,filename2,sep="")
  #print(filename2)
  
  #conn will set a connection between RStudio and the file to be ready to open
  conn2 <- file(filename,open="r")
 
  #this function will read the file line by line with the connection established 
  textcontent1 <-readLines(conn2)
  print(textcontent1)
  
  #this function will interpret each element of the file as a document.
  textcontent1 <- VectorSource(textcontent1)
  
  #this function will create a vector corpus of all the individual documents that are created and treated by R
  textcontent1 <- VCorpus(textcontent1)
  
  #this function will construct a term-document matrix or a document-term matrix, which has all the frequencies of the individual words
  tdm1 <- TermDocumentMatrix(textcontent1)
  
  
  print(tdm1)
  
  #converting term document matrix to a simple matrix for simple plotting and classification
  tdm1 <- as.matrix(tdm1)
  print(tdm1)
  
  #this function will form row and column sums and means for objects for the sparse matrix created
  w1 <- rowSums(tdm1)
  
  #converting to a data frame 
  w1 <- data.frame(names(w1), w1)
  
  #assigning column names to the data frame
  colnames(w1) <- c('word', 'freq')
  
  #converting to a tibble 
  w2 <- tibble::as.tibble(w1)
  
  print(w2)
  
  #using the dplyr package, we can get various sentiments from different methods like the nrc method
  #here we use the sentiment joy
  nrc_joy <- get_sentiments("nrc") %>% 
    filter(sentiment == "joy")
  
  #joining by word will give us the individual word's sentiment and level
  w2 %>%
    inner_join(nrc_joy) %>%
    count(word, sort = TRUE)
  
  #we use the bing sentiments method that gives us positive and negative sentiments of words
  w2_sentiment <- w2 %>%
    inner_join(get_sentiments("bing")) %>%
    count(sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
    mutate(sentiment = positive - negative)
  
  print(w2_sentiment)
  
  
  #creating a plot of the bing sentiment with the help of ggplot2
  ggplot(w2_sentiment, aes(negative, sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, ncol = 2, scales = "free_x")
  
  #saving the plot as png
  outputGGP2 <- paste0(location,"stockbingg_", format(Sys.time(), "%d-%b-%Y %H.%M"), ".png")
  ggsave(outputGGP2, width=15, height=10)

  #saving the data frame as csv
  output2 <- paste0(location,"nrc_sentiment_bing_", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv")
  write.csv(w2_sentiment, output2, row.names = FALSE)
  
  #using the dplyr package, we can get various sentiments from different methods like the afinn method
  afinn <- w2 %>% 
    inner_join(get_sentiments("afinn")) %>% 
    group_by(index = freq %/% 2) %>% 
    summarise(sentiment = sum(value)) %>% 
    mutate(method = "AFINN")
  
  print(afinn)
  
  #creating a plot 
  ggplot(afinn, aes(index, sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~method, ncol = 1, scales = "free_y")
  
  #saving afinn values and sentiments as a csv file
  output3 <- paste0(location,"stockafinn_", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv")
  write.csv(afinn, output3, row.names = FALSE)

  #saving plot as a png
  outputGGP3 <- paste0(location,"stockafinnplot_", format(Sys.time(), "%d-%b-%Y %H.%M"), ".png")
  ggsave(outputGGP3, width=15, height=10)

  #this function assigns sentiments to every individual word
  emotions <- get_nrc_sentiment(w2$word)
  
  #this function forms a new column specially for emotions
  emo_bar <- colSums(emotions)
  
  #creating a data frame
  emo_sum <- data.frame(count=emo_bar, emotion=names(emo_bar))
  
  #creating a plot
  ggplot(emo_sum, aes(x=reorder(emotion,-count), y=count)) +
    geom_bar(stat= 'identity')
  
  #saving as a csv file
  output4 <- paste0(location,"stockemosum_nrc_", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv")
  write.csv(emo_sum, output4, row.names = FALSE)
  
  #saving plot as a png file
  outputGGP4 <- paste0(location,"stockemosumnrc_", format(Sys.time(), "%d-%b-%Y %H.%M"), ".png")
  ggsave(outputGGP4, width=15, height=10)

  #using the dplyr package, we can get various sentiments from different methods like the bing method
  bing_word_counts <- w2 %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE)
  
  #getting top 10 bing sentiments by order
  bing_top10 <- bing_word_counts%>%
    group_by(sentiment) %>%
    slice_max(order_by = n, n= 10) %>%
    ungroup() %>%
    mutate(word= reorder(word, n))
  
  print(bing_top10)
  
  #creating a bing plot
  bing_top10 %>%
    ggplot(aes(word, n, fill= sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y="contribution to sentiment", x= NULL) +
    coord_flip()
  
  #saving as a csv file
  output5 <- paste0(location,"stockbingg1_", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv")
  write.csv(bing_top10, output5, row.names = FALSE)
  
  #saving plot as a png file
  outputGGP5 <- paste0(location,"stockbinggplot_", format(Sys.time(), "%d-%b-%Y %H.%M"), ".png")
  ggsave(outputGGP5, width=15, height=10)

  
  #using the dplyr package, we can get various sentiments from different methods like the loughran method
  loughran_word_counts <- w2 %>%
    inner_join(get_sentiments("loughran")) %>%
    count(word, sentiment, sort= TRUE)
  
  #creating loughran top 10 words and their sentiments by order
  loughran_top10 <- loughran_word_counts %>%
    group_by(sentiment) %>%
    slice_max(order_by = n, n=10) %>%
    ungroup() %>%
    mutate(word = reorder(word,n))
  
  print(loughran_top10)
  
  #creating a plot
  loughran_top10 %>%
    ggplot(aes(word, n, fill= sentiment)) +
    geom_col(show.legend=FALSE) +
    facet_wrap(~sentiment, scales= "free_y") +
    labs(y= "contribution to sentiment", x= NULL) +
    coord_flip()
  
  #saving as a csv file
  output6 <- paste0(location,"stockloughran_", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv")
  write.csv(loughran_top10, output6, row.names = FALSE)
  
  #saving plot as a png file
  outputGGP6 <- paste0(location,"stockloughranplot_", format(Sys.time(), "%d-%b-%Y %H.%M"), ".png")
  ggsave(outputGGP6, width=15, height=10)

  
  output2
  output3
  output4
  output5
  output6
  
  outputGGP2
  outputGGP3
  outputGGP4
  outputGGP5
  outputGGP6
  
  
}