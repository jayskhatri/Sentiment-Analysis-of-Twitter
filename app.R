library(shiny)
library(shinythemes)


# Define UI for application that draws a histogram
ui <- fluidPage(
  #theme = shinytheme("darkly"),
  shinythemes::themeSelector(),
  # Application title
  titlePanel("Sentiment Analysis of Tweets (Team: Jay, Isha and Priyank)"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      textInput("searchString", "Enter Search: ","BJP"),
      sliderInput("maxTweet", "Select Number of Tweets",0,1500,100,step=50),
      actionButton("goBtn", "Search", icon("twitter"),
                   style="color: #fff; background-color: #337ab7"),
      print(h6("  Be Patient, Good Things Take Time!"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Histogram of Overall Score",plotOutput("plot1")),
        tabPanel("Histogram of Positive Score", plotOutput("plot2")),
        tabPanel("Histogram of Negative Score", plotOutput("plot3")),
        tabPanel("Pie chart (By Polarity)", plotOutput("plot4")),
        tabPanel("3D-Pie chart (By Polarity)", plotOutput("plot5")),
        tabPanel("Pie chart (By Emotions)", plotOutput("plot6")),
        tabPanel("WordCloud",plotOutput("plot7")),
        tabPanel("Tweets",dataTableOutput("plot8"))
        
        , type = "pills"
      )
    )
  )
)

#server function 
server <- function(input, output) {
  library(ROAuth)
  library(twitteR)
  
  consumer_key <-"xxx"
  consumer_secret <- "xxx"
  access_token<-"xxx"
  access_secret <- "xxx"
  
  setup_twitter_oauth(consumer_key ,consumer_secret, access_token,  access_secret )
  positivewords=readLines("positive_words.txt")
  negativewords=readLines("negative_words.txt")
  
  pos.words <-c(positivewords)
  neg.words <-c(negativewords)
  
  
  cred <- OAuthFactory$new(consumerKey="xxx", consumerSecret="xxx",requestURL="https://api.twitter.com/oauth/request_token",accessURL="https://api.twitter.com/oauth/access_token",authURL="https://api.twitter.com/oauth/authorize")
  
  #cred$handshake(cainfo="cacert.pem")
  #score.sentiment function
  score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
  {
    library(plyr)
    library(stringr)
    list=lapply(sentences, function(sentence, pos.words, neg.words)
    {
      sentence = gsub('[[:punct:]]',' ',sentence)
      sentence = gsub('[[:cntrl:]]','',sentence)
      sentence = gsub('\\d+','',sentence)
      sentence = gsub('\n','',sentence)
      
      sentence = tolower(sentence)
      word.list = str_split(sentence, '\\s+')
      words = unlist(word.list)
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      pp=sum(pos.matches)
      nn = sum(neg.matches)
      score = sum(pos.matches) - sum(neg.matches)
      list1=c(score, pp, nn)
      return (list1)
    }, pos.words, neg.words)
    score_new=lapply(list, `[[`, 1)
    pp1=score=lapply(list, `[[`, 2)
    nn1=score=lapply(list, `[[`, 3)
    
    scores.df = data.frame(score=score_new, text=sentences)##
    positive.df = data.frame(Positive=pp1, text=sentences)
    negative.df = data.frame(Negative=nn1, text=sentences)
    
    list_df=list(scores.df, positive.df, negative.df)
    return(list_df)
  }
  
  afterFetch = function(obj.tweets)
  {
    sample=NULL  #Initialising  #We can get the text from df$text, which are the cleand tweets
    for (tweet in obj.tweets)
      sample = c(sample,tweet$getText())
    
    #Removing emoticons
    s <- searchTwitter("#emoticons")
    df <- do.call("rbind", lapply(obj.tweets, as.data.frame))
    df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
    
    # Clean the tweets
    result = score.sentiment(df$text, pos.words, neg.words)
    
    library(reshape)
    #Creating a copy of result data frame
    test1=result[[1]]
    test2=result[[2]]
    test3=result[[3]]
    
    #Creating three different data frames for Score, Positive and Negative
    #Removing text column from data frame
    test1$text=NULL
    test2$text=NULL
    test3$text=NULL
    
    #Storing the first row(Containing the sentiment scores) in variable q
    osc=test1[1,]
    psc=test2[1,]
    nsc=test3[1,]
    oosc=melt(osc, var="Score")
    ppsc=melt(psc, var="Positive")
    nnsc=melt(nsc, var="Negative") 
    oosc["Score"] = NULL
    ppsc["Positive"] = NULL
    nnsc["Negative"] = NULL
    
    #Creating data frame
    table1 = data.frame(Text=result[[1]]$text, Score=oosc)
    table2 = data.frame(Text=result[[2]]$text, Score=ppsc)
    table3 = data.frame(Text=result[[3]]$text, Score=nnsc)
    
    #Merging three data frames into one
    final_table=data.frame(Text=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value)
    return(final_table)
  }
  
  data1 = eventReactive( input$goBtn, {
    obj.tweets = searchTwitter(input$searchString,lang='en', n=input$maxTweet, resultType='recent', retryOnRateLimit = 10)
    
    final_table = afterFetch(obj.tweets)
    
    #Making percentage columns
    p =final_table$Positive/(final_table$Positive+final_table$Negative)
    p[ is.nan(p) ] <- 0
    final_table$Postive_percentage=p
    n=final_table$Negative/(final_table$Positive+final_table$Negative)
    n[ is.nan(n) ] <- 0
    final_table$Neg_percent=n
    
    #Creating Histogram
    hist(final_table$Score, xlab = "Score" , main = paste("Histogram of Overall Score of Sentiment of tweets"), col =rainbow(7))
  })
  
  output$plot1<-renderPlot({
    withProgress({
      setProgress(message = "Processing Histogram")
      data1()
    })
  })
  
  data2 = eventReactive(input$goBtn,{ 
    obj.tweets = searchTwitter(input$searchString,lang='en', n=input$maxTweet, resultType='recent', retryOnRateLimit = 10)
    
    final_table = afterFetch(obj.tweets)
    
    #Making percentage columns
    
    p=final_table$Positive/(final_table$Positive+final_table$Negative)
    p[ is.nan(p) ] <- 0
    final_table$Postive_percentage=p
    n=final_table$Negative/(final_table$Positive+final_table$Negative)
    n[ is.nan(n) ] <- 0
    final_table$Neg_percent=n
    
    
    #Creating Histogram
    hist(final_table$Positive, xlab = "Positive Score" , main = paste("Histogram of Positive Score of Sentiment of tweets"), col =rainbow(7))
  })
  
  output$plot2<-renderPlot({
    withProgress({
      setProgress(message = "Processing Histogram")
      data2()
    })
  })
  
  data3 = eventReactive(input$goBtn, {
    obj.tweets = searchTwitter(input$searchString,lang='en', n=input$maxTweet, resultType='recent', retryOnRateLimit = 10)
    
    final_table = afterFetch(obj.tweets)
    
    #Making percentage columns
    p=final_table$Positive/(final_table$Positive+final_table$Negative)
    p[ is.nan(p) ] <- 0
    final_table$Postive_percentage=p
    n=final_table$Negative/(final_table$Positive+final_table$Negative)
    n[ is.nan(n) ] <- 0
    final_table$Neg_percent=n
    
    #Creating Histogram
    hist(final_table$Negative, xlab = "Negative Score" , main = paste("Histogram of Negative Score of Sentiment of tweets"), col =rainbow(7))
  })
  
  output$plot3<-renderPlot({
    withProgress({
      setProgress(message = "Processing Histogram")
      data3()
    })   
  })
  
  data4 = eventReactive(input$goBtn, {
    obj.tweets = searchTwitter(input$searchString,lang='en', n=input$maxTweet, resultType='recent', retryOnRateLimit = 10)
    
    final_table = afterFetch(obj.tweets)
    
    #Making percentage columns
    p=final_table$Positive/(final_table$Positive+final_table$Negative)
    p[ is.nan(p) ] <- 0
    final_table$Postive_percentage=p
    n=final_table$Negative/(final_table$Positive+final_table$Negative)
    n[ is.nan(n) ] <- 0
    final_table$Neg_percent=n
    
    library(plotrix)
    
    slices <- c(sum(final_table$Positive), sum(final_table$Negative))
    lbls<-c('Positive','Negative')
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    pie(slices, labels = lbls, col=rainbow(length(lbls)), main='Pie chart of Sentiment Analysis of tweets')
    
  })
  
  output$plot4<-renderPlot({
    withProgress({
      setProgress(message = "Processing Pie-Chart")
      data4()
    })
  })
  
  data5 = eventReactive(input$goBtn, {
    
    obj.tweets = searchTwitter(input$searchString,lang='en', n=input$maxTweet, resultType='recent', retryOnRateLimit = 10)
    
    final_table = afterFetch(obj.tweets)
    
    #Making percentage columns
    p=final_table$Positive/(final_table$Positive+final_table$Negative)
    p[ is.nan(p) ] <- 0
    final_table$Postive_percentage=p
    n=final_table$Negative/(final_table$Positive+final_table$Negative)
    n[ is.nan(n) ] <- 0
    final_table$Neg_percent=n
    
    library(plotrix)
    
    slices <- c(sum(final_table$Positive), sum(final_table$Negative))
    lbls<-c('Positive','Negative')
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    pie3D(slices, labels = lbls, explode=0.0, col=rainbow(length(lbls)), main='3D- Pie chart of Sentiment Analysis tweets')
  })
  output$plot5<-renderPlot({
    withProgress({
      setProgress(message = "Processing 3D Pie-Chart")
      data5()
    })
  })
  
  data6 = eventReactive(input$goBtn, {
    obj.tweets = searchTwitter(input$searchString,lang='en', n=input$maxTweet, resultType='recent', retryOnRateLimit = 10)
    
    final_table = afterFetch(obj.tweets)
    
    #Making percentage columns
    p=final_table$Positive/(final_table$Positive+final_table$Negative)
    p[ is.nan(p) ] <- 0
    final_table$Postive_percentage=p
    n=final_table$Negative/(final_table$Positive+final_table$Negative)
    n[ is.nan(n) ] <- 0
    final_table$Neg_percent=n
    
    library(plotrix)
    
    slices <- c(sum(final_table$Positive), sum(final_table$Negative))
    lbls<-c('Positive','Negative')
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    
    Sc=final_table$Score
    good<- sapply(final_table$Score, function(Sc) Sc > 0 && Sc <= 3)
    pos1=final_table$Score[good]
    pos1_len=length(pos1)
    
    vgood<- sapply(final_table$Score, function(Sc) Sc > 3 && Sc < 5)
    pos2=final_table$Score[vgood]
    pos2_len=length(pos2)
    
    vvgood<- sapply(final_table$Score, function(Sc) Sc >= 6)
    pos3=final_table$Score[vvgood]
    pos3_len=length(pos3)
    
    Sc=final_table$Score
    bad<- sapply(final_table$Score, function(Sc) Sc < 0 && Sc >= -3)
    neg1=final_table$Score[bad]
    neg1_len=length(neg1)
    
    vbad<- sapply(final_table$Score, function(Sc) Sc < -3 && Sc >= -5)
    neg2=final_table$Score[vbad]
    neg2_len=length(neg2)
    
    vvbad<- sapply(final_table$Score, function(Sc) Sc <= -6)
    neg3=final_table$Score[vvbad]
    neg3_len=length(neg3)
    
    neutral= sapply(final_table$Score, function(Sc) Sc == 0)
    neu=final_table$Score[neutral]
    neu_len=length(neu)
    
    slices1 <- c(pos1_len, neg3_len, neg1_len, pos2_len,  neg2_len, neu_len, pos3_len)
    lbls1 <- c( 'Good','Awful','Unsatisfactory', 'Great', 'Poor', 'Neutral', 'Outstanding')##
    pct=round(slices1/sum(slices1)*100)
    lbls1 <- paste(lbls1, pct) # add percents to labels 
    lbls1 <- paste(lbls1,'%',sep='') # ad % to labels 
    pie(slices1,labels = lbls1, col=rainbow(length(lbls1)),
        main='Percentage of tweets with particular sentiment',radius = 1, cex = 1)
  })
  output$plot6<-renderPlot({
    withProgress({
      setProgress(message = "Processing Pie-Chart of Emotions")
      data6()
    })
  })
  
  data7 = eventReactive(input$goBtn, {
    
    #WORDCLOUD
    library(wordcloud)
    
    #install.packages('tm')
    library(tm)
    
    obj1.tweets=searchTwitter(input$searchString, lang='en', n=input$maxTweet, resultType='recent', retryOnRateLimit = 10)
    df <- do.call('rbind', lapply(obj1.tweets, as.data.frame))
    obj1_text <- sapply(df$text,function(row) iconv(row, 'latin1', 'ASCII', sub = ''))
    #str(obj1_text) -> gives character vector
    obj1_corpus = Corpus(VectorSource(obj1_text))
    
    #clean text
    obj1_clean = tm_map(obj1_corpus, removePunctuation)
    obj1_clean = tm_map(obj1_clean, content_transformer(tolower))
    obj1_clean = tm_map(obj1_clean, removeWords, stopwords('english'))
    obj1_clean = tm_map(obj1_clean, removeNumbers)
    obj1_clean = tm_map(obj1_clean, stripWhitespace)
    
    #cleaning most frequent words
    wordcloud(obj1_clean, random.order=F,max.words=1000, col=rainbow(7))
    
  })
  
  output$plot7 <- renderPlot({
    withProgress({
      setProgress(message = "Loading Word-Cloud")
      data7()
    })
  })
  
  data8 = eventReactive(input$goBtn,{
    obj1.tweets = searchTwitter(input$searchString,lang='en', n=input$maxTweet, resultType='recent', retryOnRateLimit = 10)
    
    df <- do.call('rbind', lapply(obj1.tweets, as.data.frame))
    jk = data.frame(df$text,df$created,df$screenName,df$favoriteCount,df$retweetCount ,df$location)
    jk$df.text = sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
    colnames(jk) = c("Tweets", "Date", "Username", "Fav Count", "RT Count", "Location")
    
    tweetOutput = jk
  })
  
  output$plot8 <- renderDataTable({
    withProgress({
      setProgress(message = "Loading Tweets")
      data8()})
  }, options = list(lengthMenu = c(10, 30, 50), pageLength = 5)
  )
}

# Run the application 
shinyApp(ui = ui, server = server)