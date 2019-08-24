#Loading the rvest package

library('rvest')
library(janeaustenr)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)

years <- c('2012','2013','2014','2015','2016','2017')

# Extracting data from Web Train as final and test as final21

for(i in years)
{
  url21<-paste("https://www.cars.com/research/toyota-camry",i,sep='-')
  url2<-paste(url21,"consumer-reviews/?nr=250&pg=1",sep='/')
  # url2 <- "https://www.cars.com/research/toyota-camry-2017/consumer-reviews/?nr=250&pg=1"
  webpage <- read_html(url2)
  #getting total number of reviews 
  rank_data_html1 <- html_node(webpage,'.sort-display')
  rank_data1 <- html_text(rank_data_html1)
  rank_data1[1]
  print(rank_data1)
  
  test<-sapply(strsplit(rank_data1[1], "of "), "[", 2)
  print(test)
  test21<-sapply(strsplit(test, " reviews"), "[", 1)
  test21
  print(test21)
  url1<-paste(test21,'pg=1',sep='&')
  # url1
  url21<-paste("https://www.cars.com/research/toyota-camry",i,sep='-')
  url2<-paste(url21,"consumer-reviews/?nr",sep='/')
  
  #Specifying the url for desired website to be scrapped
  url <- paste(url2,url1,sep ='=')
  
  #https://www.cars.com/research/toyota-camry-2017/consumer-reviews/?nr=250&pg=1
  
  #Reading the HTML code from the website
  webpage <- read_html(url)
  
  
  #Using CSS selectors to scrap the rankings section
  rank_data_html <- html_nodes(webpage,'.mmy-reviews__blurb')
  
  #Converting the ranking data to text
  rank_data <- html_text(rank_data_html)
  
  #Let's have a look at the rankings
  print(rank_data)
  
  rank_data[2]
  comments<-sapply(strsplit(rank_data, "\n                                                "), "[", 3)
  
  rank_data_html21 <- html_nodes(webpage,'.mmy-reviews__review')
  # rank_data_html21 <- html_attr(webpage,'rating')
  rank_data_html21
  rank_data21 <- html_text(rank_data_html21)
  rank_data21
  # xml_attrs(xml_child(xml_child(rank_data_html21[[1]], 2), 1))[["rating"]]
  
  count1<-as.numeric(test21)
  count1
  StarRating<- 1:count1
  yearCurrent<- 1:count1
  yeardata<- 1:6
  for(j in 1:count1)
    
    
  {
    StarRating[j]<-xml_attrs(xml_child(xml_child(rank_data_html21[[j]], 2), 1))[["rating"]]
    
  }
  
  datatest  = data.frame(comments,StarRating)
  datatest$year = i
  # datatest
  # K<-1
  # yeardata[k]<- datatest
  # K<- K+1
  if(i=='2012'){
    final <-datatest
  }else{
    if(i!='2017'){
      final<-rbind(final,datatest)
    }else{
      final21 <-datatest
      
    }
  }
  
}
final$normalizedReview <- gsub('[[:punct:] ]+',' ',final$comments)
final$normalizedReview <-tolower(final$normalizedReview)
final21$normalizedReview <- gsub('[[:punct:] ]+',' ',final21$comments)
final21$normalizedReview <-tolower(final21$normalizedReview)
tag<-c("service","price", "handling", "interior")
term_regex <- paste0('(', paste(tag, collapse = '|'), ')')
final <- final %>% 
  mutate(Tag = sapply(stringr::str_extract_all(normalizedReview, term_regex),
                      function(x) paste(unique(x), collapse=' ')))
final21 <- final21 %>% 
  mutate(Tag = sapply(stringr::str_extract_all(normalizedReview, term_regex),
                      function(x) paste(unique(x), collapse=' ')))

# Findind Sentiment score for test data

text_df <- data_frame(line = 1:length(final$normalizedReview), text = final$normalizedReview)

head(text_df)

text_df <- text_df %>%
  unnest_tokens(word, text)

afinn <- get_sentiments("afinn")
text_tidy<- text_df %>% 
  anti_join(stop_words, by = c("word" = "word"))

sentimentscore <- text_tidy %>%
  inner_join(afinn) %>%
  group_by(line) %>% 
  summarise(sentiment = mean(score)) %>% mutate()
sentimentscore1 <- text_tidy %>%
  left_join(afinn) %>%
  group_by(line) %>% 
  summarise(sentiment = mean(score)) %>% mutate()
# df$project <- final$Normalized_Review
sentimentscore2 <- sentimentscore1 %>% left_join(sentimentscore , by = "line")
sentimentscore2$sentiment.y[is.na(sentimentscore2$sentiment.y)]<-0
final$sentimentscore<-sentimentscore2$sentiment.y

# Findind Sentiment score for test data

text_df1 <- data_frame(line = 1:length(final21$normalizedReview), text = final21$normalizedReview)

head(text_df1)

text_df1 <- text_df1 %>%
  unnest_tokens(word, text)

afinn <- get_sentiments("afinn")
text_tidy<- text_df1 %>% 
  anti_join(stop_words, by = c("word" = "word"))

sentimentscore30 <- text_tidy %>%
  inner_join(afinn) %>%
  group_by(line) %>% 
  summarise(sentiment = mean(score)) %>% mutate()
sentimentscore31 <- text_tidy %>%
  left_join(afinn) %>%
  group_by(line) %>% 
  summarise(sentiment = mean(score)) %>% mutate()
# df$project <- final$Normalized_Review
sentimentscore32 <- sentimentscore31 %>% left_join(sentimentscore30 , by = "line")
sentimentscore32$sentiment.y[is.na(sentimentscore32$sentiment.y)]<-0
final21$sentimentscore<-sentimentscore32$sentiment.y


# Building Model
modelbuilding <-final
modelbuilding$StarRating1 <- as.factor(modelbuilding$StarRating)
#Multinomial Logistic regression
library(nnet)
modelbuilding$out<-relevel(modelbuilding$StarRating1, ref="1")
mymodel<-multinom(out~sentimentscore, data=modelbuilding)
# summary(mymodel)

# Prediction
predictedvalues <- predict(mymodel,final21)
final21$predictedValue <- predictedvalues


#Calculating accuracy
cm<- table(predictedvalues,final21$StarRating)
print(cm)
accuracy<- sum(diag(cm)/sum(cm))
print(accuracy)

reduceddata <- filter(final, Tag %in% c("service","interior","handling","price"))
avgtag<-reduceddata %>% 
  group_by(Tag)%>%  summarize( 'Average Sentiment' = mean(sentimentscore), 'Average Rating'= mean(as.numeric(StarRating)))

# cm<-confusionMatrix(predictedvalues, final21$StarRating)

Server<-(function(input, output,session) {
  
  output$RawData <- DT::renderDataTable(
    DT::datatable(
      final, options = list(
        lengthMenu = list(c(10, 15, -1), c('10', '15', 'All')),
        pageLength = 10,searching = TRUE
      )
    )
  )
  output$testData <- DT::renderDataTable(
    DT::datatable(
      final21, options = list(
        lengthMenu = list(c(10, 15, -1), c('10', '15', 'All')),
        pageLength = 10,searching = TRUE
      )
    )
  )
  output$modelSummary <- renderPrint(
    summary(mymodel)
  )
  output$predictedmodel <- renderPrint(
    predictedvalues
  )
  # reduceddata <- filter(final, Tag %in% c("Service","Interior","Handling","Price"))
  output$ratingAverage1<- renderTable({average<-final %>% group_by(StarRating) %>%
    summarize( 'Average Sentiment' = mean(sentimentscore))})
  
  
  output$ratingTagAverage1<- renderTable({
    
    final31 <- final[!final$Tag == "",]
    final31 <- final31 %>% unnest_tokens(Tags1, Tag)
    average<-final31 %>% group_by(Tags1) %>%
    summarize( 'Average Sentiment' = mean(sentimentscore))})

  output$ratingAverage2<- renderTable({avgerage<-final %>%
    summarize( 'Average Sentiment' = mean(sentimentscore), 'Average Rating'= mean(as.numeric(StarRating)))
  })
  output$ratingAverage3<- renderTable({
    
    reduceddata <- filter(final, Tag %in% c("service","interior","handling","price"))
    avgtag<-reduceddata %>% 
      group_by(Tag)%>%  summarize( 'Average Sentiment' = mean(sentimentscore), 'Average Rating'= mean(as.numeric(StarRating)))
    
  })
  output$table <- renderTable(cm)
 output$caption<- renderText({paste("Accuracy score of the model is " , accuracy)})
 output$caption1<-renderText({"Multinomial Logistic regression"})
 
 output$Plot<-renderPlot({
   finalta<-data.frame(final,stringsAsFactors = F)

   
   tfidf <- (final) %>%mutate(Line = row_number()) %>%
     unnest_tokens(word, normalizedReview)
   tfidf <- tfidf[!tfidf$Tag == "",]
   
   tfidf <- tfidf %>% unnest_tokens(Tags1, Tag)
print("3")
tfidf1 <- tfidf[, c("word", "Tags1")]

tfidf1 <- tfidf1 %>% count(Tags1, word, sort = TRUE) %>% ungroup() %>%
  bind_tf_idf(word, Tags1, n) %>% arrange(desc(tf_idf))


  
  grf <- tfidf1 %>% 
    group_by(Tags1) %>% 
    top_n(10, tf_idf) %>% 
    ungroup() %>%
    mutate(word = reorder(word, tf_idf)) %>%
    ggplot(aes(word, tf_idf, fill = Tags1)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~Tags1, ncol = 2, scales = "free") +
    coord_flip()
  
  return(grf)
  })
  
  
})





