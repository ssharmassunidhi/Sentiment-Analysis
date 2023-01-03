WebScrape <- function(filename,location) {
  #This method will take the name of the file that has all the weblinks,
  #and location of that link file
  library(rvest)
  library(dplyr)
  
  #this will paste the location and filename from the called location
  filename <- paste0(location,filename,sep="")
  print(filename)
  
  
  #step 1 is to combine the location and filename to prepare the  file to read
  #this function reads and opens the file in r and make a connection between r and the file
  conn <- file(filename,open="r")
  
  #giving the dataset a specific name 'fullCont
  fullCont<- ""
  
  #Step 2 is to read the file line by line with this function
  linn <-readLines(conn)
  
  #this step will create a for loop and specify after disintegrating the link from the type of the link
  for (i in 1:length(linn)){
    
    type=strsplit(linn[i],",")[[1]][2]
    link= strsplit(linn[i],",")[[1]][1]
    print(paste("type=",type,sep=""))
    print(paste("link=",link,sep=""))
    page = read_html(link)
    
    #the if else function specifies the coding method in accordance with the input ie. if the input is a, then the code to extract data will be b, else c
    #the html_nodes function takes the data from the web to extract from an html form, we use SelectorGadget to input the codes of selected data from the html page
    if(type=="businesstoday"){
      content = page %>% html_nodes(".field__item p , .sab-head-tranlate-sec h2 , h1")%>% html_text()
      print(content)
      
    }else if(type=="moneycontrol"){
      content = page %>% html_nodes("p , h2 , h1") %>% html_text()
    }else if(type=="financialexpress"){
      content = page %>% html_nodes("#pcl-full-content p , .wp-block-post-excerpt__excerpt , .wp-block-post-title") %>% html_text()
    }else if(type=="economictimes"){
      content = page %>% html_nodes(".artText , .artTitle") %>% html_text()
    }else if(type=="outlookindia"){
      content = page %>% html_nodes("#articleBody p , .story-summary , h1") %>% html_text()
    }else if(type=="hindustantimes"){
      content = page %>% html_nodes(".inlineAlsoReadWidget_71663328280537+ h3 , .inlineStoryWidget_71663328280537+ h3 , .htStoryDetail_71663328280537 p+ h3 , .htStoryDetail_71663328280537 p , .article:nth-child(1) .articleDescription , .article:nth-child(1) h1") %>% html_text()
    }else if(type=="eletsonline"){
      content = page %>% html_nodes("#listenButton1~ p , .td-post-title .entry-title") %>% html_text()
    }else if(type=="msn"){
      content = page %>% html_nodes("msnews-views-title , p") %>% html_text()
    }else if(type=="latestly"){
      content = page %>% html_nodes("p:nth-child(11) , p:nth-child(10) , p:nth-child(9) , p:nth-child(2) , p:nth-child(7) , p:nth-child(5) , #content p:nth-child(1) , h2 , .article_title") %>% html_text()
    }else if(type=="dbtnews"){
      content = page %>% html_nodes(".HideMeOnDesktop+ h3 , p+ h3 , hr+ p , .tagdiv-type > p , .td-post-title .entry-title") %>% html_text()
    #}else if(type=="equitypandit"){
      #content = page %>% html_nodes(".entry-content p , .penci-entry-title") %>% html_text()
    }else if(type=="thecsrjournal"){
      content = page %>% html_nodes("h6:nth-child(12) , h6:nth-child(11) , h6:nth-child(10) , h6:nth-child(9) , h6:nth-child(8) , h6:nth-child(16) , h6:nth-child(15) , h6:nth-child(14) em , em+ em , h6:nth-child(13) , h6:nth-child(6) , h6:nth-child(5) , h6:nth-child(4) , .td-g-rec-id-content_top+ h6 , .td-pb-padding-side .entry-title") %>% html_text()
    }else if(type=="zeenews"){
      content = page %>% html_nodes(".photo-description p , .photo_summary p , h1") %>% html_text()
    }else if(type=="sakshi"){
      content = page %>% html_nodes("p:nth-child(8) , p:nth-child(9) , p:nth-child(6) , p:nth-child(1) , #page-title , #image_bd_ad+ p") %>% html_text()
    }else if(type=="timesnownews"){
      content = page %>% html_nodes(".photo-description p , .photo_summary p , h1") %>% html_text()
    }else if(type=="apdirect"){
      content = page %>% html_nodes("p , .mb-0") %>% html_text()
    }else if(type=="affairscloud"){
      content = page %>% html_nodes("ul+ p , p+ ul li , .td_block_template_1~ p+ p , .d-none , .td-post-title .entry-title") %>% html_text()
    }else if(type=="expresspharma"){
      content = page %>% html_nodes(".single-post-content p:nth-child(1) , .single-post-content p:nth-child(2) , .single-post-content p:nth-child(3) , .single-post-content p:nth-child(4) , .post-subtitle , .single-post-title .post-title") %>% html_text()
    }else if(type=="investing"){
      content = page %>% html_nodes("p+ p , .article-item-title") %>% html_text()
    }else if(type=="pfizer"){
      content = page %>% html_nodes(".article-body p , .text-container , .article-title") %>% html_text()
    }else if(type=="businesswire"){
      content = page %>% html_nodes(".bw-release-story p:nth-child(2) , p:nth-child(3) , p:nth-child(4) , p:nth-child(6) , p > i , p:nth-child(1) , b") %>% html_text()
    }else if(type=="pipelinereview"){
      content = page %>% html_nodes("p:nth-child(6) , p:nth-child(7) , p:nth-child(8) , p:nth-child(9) , p:nth-child(10) , p:nth-child(11) strong , p:nth-child(12) , p:nth-child(15) , p:nth-child(16) , p:nth-child(20) , #bd_results li , ul+ p , em , h2") %>% html_text()
    }else if(type=="tradearabia"){
      content = page %>% html_nodes("#atext div , h2") %>% html_text()
    }else if(type=="cxotoday"){
      content = page %>% html_nodes("p:nth-child(22) strong , p:nth-child(18) , ul:nth-child(29) li , p:nth-child(16) , p:nth-child(14) , p:nth-child(12) , p:nth-child(10) , p:nth-child(8) , ul:nth-child(26) li , ul:nth-child(32) li , p:nth-child(36) strong , ul:nth-child(34) li , p:nth-child(37) , p:nth-child(39) strong , p:nth-child(40) , p:nth-child(42) , ul:nth-child(23) li , p:nth-child(6) , em , h1") %>% html_text()
    }else if(type=="nasdaq"){
      content = page %>% html_nodes("p:nth-child(3) , p:nth-child(6) , p:nth-child(9) , p:nth-child(10) , .taboola-placeholder+ h2 , p:nth-child(14) , h2+ p , p:nth-child(17) , p:nth-child(23) , p:nth-child(22) , .caption+ p , p:nth-child(24) , p+ h2 , .subscribelink+ p , .article-header__headline span") %>% html_text()
    }else if(type=="gulfnews"){
      content = page %>% html_nodes("p , h1") %>% html_text()
    }else if(type=="news18"){
      content = page %>% html_nodes("#8 , #7 , #6 , #5 , #4 , #3 , #2 , #1 , #0 , .article_heading") %>% html_text()
    }else if(type=="mintgenie"){
      content = page %>% html_nodes(".lazy-loaded , .news-details-container__title") %>% html_text()
    }else if(type=="ffnews"){
      content = page %>% html_nodes(".margin-big p , .c-h1") %>% html_text()
    }else if(type=="openpr"){
      content = page %>% html_nodes(".p-4:nth-child(1) , h1") %>% html_text()
    }else if(type=="mahindra"){
      content = page %>% html_nodes(".para-cm:nth-child(12) , .para-cm:nth-child(11) , .para-cm+ .para-cm em , .para-cm:nth-child(13) , .para-cm:nth-child(10) , .sub-txt , .pr-detail-txt") %>% html_text()
    }else if(type=="business-standard"){
      content = page %>% html_nodes("p:nth-child(8) b , p:nth-child(10) b , p:nth-child(13) b , style+ p , .p-content p:nth-child(3) , p:nth-child(4) , p:nth-child(5) , p:nth-child(6) , p:nth-child(9) , p:nth-child(11) , p:nth-child(12) , p:nth-child(14) , p:nth-child(15) , p:nth-child(16) , p:nth-child(17) , p:nth-child(19) b , p:nth-child(18) , p:nth-child(20) , p:nth-child(21) , p:nth-child(22) , p:nth-child(23) , p:nth-child(24) , p:nth-child(7) , .headline") %>% html_text()
    }else if(type=="newspatrolling"){
      content = page %>% html_nodes(".entry-title , .entry p") %>% html_text()
    }else if(type=="arcweb"){
      content = page %>% html_nodes(".text-align-justify , #block-archadvisory-page-title .field-wrapper") %>% html_text()
    }else if(type=="thefederal"){
      content = page %>% html_nodes("p:nth-child(3) b , p > .amp-wp-fe3f5cc , .wp-caption-text , .amp-wp-title") %>% html_text()
    }else if(type=="livemint"){
      content = page %>% html_nodes("p") %>% html_text()
    }else if(type=="indiatimes"){
      content = page %>% html_nodes(".medium") %>% html_text()
    }else if(type=="equitypandit"){
      content = page %>% html_nodes(".post p , .post strong , .post h1") %>% html_text()
    }else if(type=="adgully"){
      content = page %>% html_nodes("#content-area iframe , #content-area p , .entry-title") %>% html_text()
    }else if(type=="reporter"){
      content = page %>% html_nodes("h2:nth-child(13) , .entry p:nth-child(3) , h2:nth-child(15) , p:nth-child(17) , .entry h2+ p , #ShowSuggestionInPost+ h2 , iframe+ p , .page-title") %>% html_text()
    }else if(type=="telegraphindia"){
      content = page %>% html_nodes("p , .fs-13 , .fs-20.noto-regular , .mb-2") %>% html_text()
    }else if(type=="etfdailynews"){
      content = page %>% html_nodes("p:nth-child(2) , h2+ p , p:nth-child(16) , h2:nth-child(14) , #ShowSuggestionInPost+ h2 , p:nth-child(1) , .page-title") %>% html_text()
    }

    #this function creates an appendix for the data created and all the output content generated in one place
    fullCont = append(fullCont,content)
    
    
    
  }
  #closing the connection between r and the file
  close(conn)
  print(fullCont)
  
  fullCont <- as.data.frame(fullCont)
  
  #saving the output as csv file
  output <- paste0(location,"/content/stockScrapedData_", format(Sys.time(), "%d-%b-%Y_%H.%M"), ".csv")
  write.csv(fullCont, output)
  
  output
}