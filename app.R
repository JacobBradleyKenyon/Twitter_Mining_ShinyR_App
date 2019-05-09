###################################################################################################################  
###  Twitter Mining ShinyR Application                                                                          ###
###  User is required to obtain own twitter authorisation credentials                                           ###
###  Visit https://jacobbradleykenyon.shinyapps.io/TwitMine/ to run app without personal twitter authorisation  ###
###################################################################################################################

## Required Packages
  library(twitteR)
  library(ROAuth)
  library(tm)
  library(wordcloud)
  library(shiny)
  library(shinydashboard)
  library(RColorBrewer)
  library(wesanderson)
  library(MASS)
  library(mvtnorm)
  library(DT)




#### UI - Custom css utilising HTML function
ui <- shinyUI(
  fluidPage(
      tags$head(tags$style(HTML(
        '.navbar-default .navbar-brand{color: #A5C869;}
        .navbar-default .navbar-nav > .active > a:focus {color: rgba(0, 171, 240,1); background-color: #989dbc ;},
        .navbar-default .navbar-nav > .active > a:active {color: rgba(0, 171, 240,1); background-color: #989dbc ;}, 
        .navbar-default .navbar-nav > .active > a:hover {color: rgba(0, 171, 240,1); background-color: #989dbc ;}'))),

  ## Top Navigation Panel Aesthetics
  tags$head(tags$style(HTML(".navbar{background-image: linear-gradient(rgba(120, 185, 215, 0.8), rgba(15, 65, 85, 0.8) ),
                              url('twitter.jpg');
                              border-radius: 1rem;}"))),
  
  ## Background Aesthetics
    tags$head(tags$style(HTML("body{background-color: #989dbc}"))),
  
  ## Black Leather - Sidebar panel
    tags$head(tags$style(HTML(".well{
      background-image: linear-gradient(rgba(50, 60, 65, .7), rgba(90, 100, 105, .7) ); border-radius: 1.35rem;}" ))),


#### Setting Navigation
navbarPage(tags$span(style="color:white; font-family: Sans-Serif;", tags$strong("Twitter Mining App")), id= "mainNavbarPage", theme= "flatly.css",
                      
## Word Cloud Tab Panel
  tabPanel(icon=icon("line-chart"), tags$span(style="color: black; font-family: Sans-Serif;", tags$strong("Word Cloud")), 
    fluidPage(
      sidebarLayout(
        sidebarPanel(width = 3,
          fluidRow(
            column(12,
              textInput("KeyWords", "Key words to search:", value="Donald Trump"),
              textInput("OmitWords", "Words to omit:", value="president"),
    
              sliderInput("N",
                          HTML("Max number of recent <em>tweets</em>:"),
                          min=100, max=1000, step=100, value=200),
              
              sliderInput("MAX",
                          "Maximum words in cloud:",
                          min=10, max=500, step=10, value=100),
              
              radioButtons("type", "Dispay Color:",
                           list("FantasticFox"   ="F",
                                "GrandBudapest"  ="GB",
                                "Rushmore"       ="RM",
                                "Cavalcanti"     ="CV",
                                "Rainbow"        ="RB",
                                "Dark Colors"    ="DK",
                                "Blues"          ="blue",
                                "Reds"           ="red",
                                "Black and White"="BW"),
                           selected="RM")),
          
                column(12, align="center", offset=0,
                       submitButton("Search", icon=icon('search'), width="50%"),
                       tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"),
                       br(),
                       HTML("<b>Enter words to search/omit separated by spaces.</b>"),br(),
                       HTML("Enjoy!")
                       )
          )
        ),  ## End of Side bar panel Code
  
      ## Show a plot of the generated distribution
        mainPanel(width = 9,
           plotOutput("distPlot", width='100%', height=500), br(),
                  HTML("A word cloud is a visual representation of text data with the importance of each tag shown in font size or color."),br(),
                  HTML("All tweets from Twitter containing the <em>Keywords to search:</em> are collected and the <em>Max number of recent tweets</em> are then used to construct the word cloud.")
               )
      )
    )
  ), ## End Main Panel and Word Cloud Tab Panel Code


## Data Table of cleanish Tweets, Searchable due to use of DT package
  tabPanel(tags$span(style="color: black; font-family: Sans-Serif;", tags$strong("Tweets")),icon=icon('twitter'),
      DTOutput("Tweets")
      ) 
    )
  )
) 

## END OF UI CODE ##




#### Define server logic required to draw a histogram
server <- function(input, output) {
  observe({
    ## Access token to Twitter
      Access_token    <-'YOUR ACCESS TOKEN HERE'
      Consumer_key    <-'YOUR CONSUMER KEY HERE'
      Consumer_secret <-'YOUR CONSUMER SECRET HERE'
      Access_Secret   <-'YOUR ACCESS SECRET HERE'
    ## 
      setup_twitter_oauth(consumer_key    = Consumer_key,
                          consumer_secret = Consumer_secret,
                          access_token    = Access_token,
                          access_secret   = Access_Secret)
      token <- get("oauth_token", twitteR:::oauth_cache)
    

  ## Collecting Search Engine Words into Corpus
    Keywords <- Corpus(VectorSource(input$KeyWords))
  
  ## Collecting Omitted Words into Corpus   
    if(nchar(input$OmitWords) >= 1){
      Omitwords <- Corpus(VectorSource(input$OmitWords))
    }
  
  ## Initial Twitter search conditioned on Keywords searched and number of maximum tweets to retreive
    TweetSearch <- searchTwitter(Keywords[[1]][1]$content, n=input$N, lang='en') 
    
  ## Pulling out words
    rm_Keywords <- strsplit(Keywords[[1]][1]$content, " ", ",")[[1]]
    
    if(nchar(input$OmitWords) >= 1){
      rm_OmitWords <- strsplit(Omitwords[[1]][1]$content, " ", ",")[[1]]
    }
    
  ## Building Corpus via getText function
    TweetText   <- sapply(TweetSearch, function(x) x$getText())
    TweetCorpus <- Corpus(VectorSource(TweetText))
    
  ## Remove URLS via gsub function
    removeURL   <- function(x) gsub("http[^[:space:]]*", "", x)
    clean_Tweet <- tm_map(TweetCorpus, content_transformer(removeURL))
    
  ## Remove \n via gsub function
    remove_tab  <- function(x) gsub("\n", "", x)
    clean_Tweet <- tm_map(clean_Tweet, content_transformer(remove_tab))
    
  ## Remove anything other than English letters or space via gsub function
    removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
    clean_Tweet    <- tm_map(clean_Tweet, content_transformer(removeNumPunct))
    clean_Tweet    <- tm_map(clean_Tweet, removePunctuation)   
  
  ## Setup to print out Tweets in new tab
     Tweet_print <- data.frame(Tweets = sapply(clean_Tweet, as.character), stringsAsFactors = FALSE)
    
 ## Transform to all lower case
    clean_Tweet <- tm_map(clean_Tweet,content_transformer(tolower))
    Keywords    <- tm_map(Keywords, content_transformer(tolower))
    rm_Keywords <- strsplit(Keywords[[1]][1]$content, " ", ",")[[1]]
    
    if(nchar(input$OmitWords) >= 1){
      Omitwords   <- tm_map(Omitwords, content_transformer(tolower))
      Omitwords   <- strsplit(Omitwords[[1]][1]$content, " ", ",")[[1]]
      clean_Tweet <- tm_map(clean_Tweet, removeWords, Omitwords)
    } 
    
    clean_Tweet  <- tm_map(clean_Tweet, removeWords, rm_Keywords)
    
  ## Collecting stopwords to omit from tweets conditioned on whether or not Omitwords was specified 
    if(nchar(input$OmitWords) >= 1){
      rm_Words <- c(stopwords('en'), rm_Keywords, rm_OmitWords, gsub("[^[:alpha:][:space:]]*", "",stopwords('en')))
    }
    if(nchar(input$OmitWords) < 1){
      rm_Words <- c(stopwords('en'), rm_Keywords, gsub("[^[:alpha:][:space:]]*", "",stopwords('en')))
    }
    
    ## Removing common words (i.e. "and", "the", etc.) and My stopwords 
      clean_Tweet <- tm_map(clean_Tweet, removeWords, rm_Words)
      clean_Tweet <- tm_map(clean_Tweet, removeNumbers)
      
    ## Cleaning up the whitespace left over
    clean_Tweet <- tm_map(clean_Tweet, stripWhitespace)
    

#### Word Cloud Plot
    output$distPlot <- renderPlot({
      # Setting plotting Area
      par(xpd=FALSE, mai=c(.01, .01, .01, .01))
      # Error message for when no tweets exist
      validate(need(length(TweetSearch ) > 0, "Sorry there are no Tweets regarding your input."))
      
      # Plot according to colour scheme indicated
      if(input$type=="F"){
        wordcloud:::wordcloud(clean_Tweet, ordered.colors=F,  
                              col=wes_palette(n=5, name="FantasticFox"),
                              max.words=input$MAX, 
                              fixed.asp=TRUE)
      }
      if(input$type=="GB"){
        wordcloud:::wordcloud(clean_Tweet, ordered.colors=F, 
                              col=wes_palette(n=4, name="GrandBudapest"),
                              max.words=input$MAX, 
                              fixed.asp=TRUE)
      }
      if(input$type=="RM"){
        wordcloud:::wordcloud(clean_Tweet,ordered.colors=F,  
                              col=wes_palette(n=5, name="Rushmore"),
                              max.words=input$MAX, 
                              fixed.asp=TRUE)
      }
      if(input$type=="CV"){
        wordcloud:::wordcloud(clean_Tweet, ordered.colors=F, 
                              col=wes_palette(n=5, name="Cavalcanti"),
                              max.words=input$MAX, 
                              fixed.asp=TRUE)
      }
      if(input$type=="RB"){
        wordcloud:::wordcloud(clean_Tweet, ordered.colors=F, 
                              col=rainbow(10),
                              max.words=input$MAX, 
                              fixed.asp=TRUE)
      }
      if(input$type=="DK"){
        wordcloud:::wordcloud(clean_Tweet,ordered.colors=F, 
                              col=colorRampPalette(brewer.pal(n=8, "Dark2"))(100),
                              max.words=input$MAX, 
                              fixed.asp=TRUE)
      }
      if(input$type=="blue"){
        wordcloud:::wordcloud(clean_Tweet,ordered.colors=F, 
                              col=colorRampPalette(brewer.pal(n=9, "Blues"))(100),
                              max.words=input$MAX, 
                              fixed.asp=TRUE)
      }
      if(input$type=="red"){
        wordcloud:::wordcloud(clean_Tweet,ordered.colors=F, 
                              col=colorRampPalette(brewer.pal(n=9, "Reds"))(100),
                              max.words=input$MAX, 
                              fixed.asp=TRUE)
      }
      if(input$type=="BW"){
        wordcloud(clean_Tweet, random.order=F, 
                  max.words=input$MAX, 
                  fixed.asp=TRUE)
      }
    })
    
#### Data Table of Tweets
    output$Tweets <- renderDT({
      datatable(data=Tweet_print,
                rownames=FALSE,
                editable = FALSE,
                options=list(autoWidth=FALSE, scrollX=TRUE)) %>%
                formatStyle(0, target= 'row', lineHeight='40%', "white-space"="nowrap")
      
      })
    
  }) 
}

# Run the application 
shinyApp(ui=ui, server=server)

