suppressMessages(library(ggplot2))
suppressMessages(library(tidytext))
suppressMessages(library(dplyr))
suppressMessages(library(snakecase))
suppressMessages(library(tm))
suppressMessages(library(SnowballC))
suppressMessages(library(stringr))
suppressMessages(library(snakecase))
suppressMessages(library(stringr))
suppressMessages(library(forcats))
suppressMessages(library(DT))
suppressMessages(library(RJSONIO))
suppressMessages(library(knitr))

readEmails <- function(in_file='data/nick_email.tsv'){
  data <- suppressWarnings(read.table(in_file, header = F, sep='\t', fill=T))
  colnames(data) <- c("from", "to", "date", "message")
  
  matchNames <- function(x){
    splits <- str_split_fixed(x, ", ", 2)
    paste(splits[,2], splits[,1], sep = ' ')
  }
  
  cleanData <- data %>% 
    filter(from!='') %>% 
    filter(message!='') %>% 
    mutate(message = gsub("None,", "", message)) %>% 
    mutate(to = htmlStrip(to)) %>% 
    mutate(from = htmlStrip(from)) %>% 
    mutate(from = trimws(from)) %>% 
    mutate(to = trimws(to)) %>% 
    mutate(to = ifelse(grepl(', ', to), matchNames(to), to)) %>% 
    mutate(to = to_upper_camel_case(to, sep_out = " ")) %>% 
    mutate(message = urlStrip(message)) %>% 
    select(-from) %>% 
    droplevels()
  
  return(cleanData)
}


wordFreq <- function(wordlength=3, corpus, top=15 ){
  
  all <- corpus %>%
    filter(nchar(as.character(word))>=wordlength)
  
  text_zize <- 300 / top 
  
  d <- all[1:15,]
  d  <- transform(d , word = reorder(word, freq))
  
  division <- plyr::round_any(ceiling(max(d$freq)/10), 10, f = ceiling)
  
  if(max(d$freq)>=100){
    division <- plyr::round_any(ceiling(max(d$freq)/10), 50, f = ceiling)
  }
  
  if(max(d$freq)>=500){
    division <- plyr::round_any(ceiling(max(d$freq)/10), 100, f = ceiling)
  }
  
  p <- ggplot(d)
  p <- p + geom_bar(aes(word, freq, fill="#5A9ED6"),stat='identity')
  p <- p + scale_y_continuous("Word frequency", breaks=seq(0,max(d$freq),by=division),expand=c(0.01,0))
  p <- p + scale_x_discrete("Word", expand = c(0.01,0.01))
  
  p <- p + cleanTheme() +
    theme(
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y = element_text(size=20),
      axis.text.x = element_text(size=15),
      panel.grid.major.x = element_line(color="grey80", size = 0.5, linetype = "dotted")
      
    )
  p <- p + scale_fill_identity()
  p <- p + coord_flip()
  p
}


contributions <- function(file_in='data/nick_email.tsv', df=NA, top_words = 5, method='loughran'){
  
  ifelse(is.na(df),
         data <- readEmails(file_in),
         data <- df)
  
  tokens <- data %>% 
    group_by(to) %>% 
    mutate(text = message) %>% 
    unnest_tokens(word, text) %>% 
    ungroup() %>% 
    select(to, word) %>% 
    droplevels()
  
  excludedWords <- c("none", "nick", "riddiford")
  
  filtToks <- tokens %>% 
    filter(!word %in% excludedWords) %>% 
    select(to, word) %>%
    droplevels()
  
  sentimentedTokens <- filtToks %>%
    group_by(to, word) %>% 
    inner_join(get_sentiments(method)) %>% # pull out only sentiment words
    count(sentiment, sort =T) 
  
  # Get the top words in each category
  topWords <- sentimentedTokens %>%
    mutate(wordCount=n) %>% 
    group_by(sentiment, word) %>%
    tally() %>% 
    top_n(n=top_words)
  
  # return(topWords)
  
  p <- ggplot(topWords)
  p <- p + geom_bar(aes(fct_reorder(word, nn), nn, fill = sentiment), stat = "identity")
  p <- p + guides(fill = FALSE)
  p <- p + cleanTheme() +
    theme(
      panel.grid.major.x = element_line(color = "grey80", size = 0.5, linetype = "dotted"),
      # axis.text.y = element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size=40))
  p <- p + scale_y_continuous(expand = c(0.01,0.01))
  p <- p + coord_flip()
  p <- p + facet_wrap(~sentiment, scales='free')


  p
    
  
}


emailSentiments <- function(file_in='data/nick_email.tsv', df=NA, recipient = NA, top_recipients = 5, method='loughran'){

  ifelse(is.na(df),
         data <- readEmails(file_in),
         data <- df)

  if(is.na(recipient)) {
  # Get the top 5 recipients
  topRecips <- data %>%
    group_by(to) %>% 
    tally() %>% 
    top_n(n=top_recipients)
  
  
  filtData <- data %>% 
    filter(to %in% topRecips$to) %>% 
    droplevels()
  } else {
    filtData <- data %>% 
      filter(to == recipient) %>% 
      droplevels()
  }
  
  tokens <- filtData %>% 
    group_by(to) %>% 
    mutate(text = message) %>% 
    unnest_tokens(word, text) %>% 
    ungroup() %>% 
    select(to, word) %>% 
    droplevels()
  
  excludedWords <- c("none", "nick", "riddiford")
  
  filtToks <- tokens %>% 
    filter(!word %in% excludedWords) %>% 
    select(to, word) %>%
    droplevels()
  
  sentimentedTokens <- filtToks %>%
    group_by(to) %>% 
    inner_join(get_sentiments(method)) %>% # pull out only sentiment words
    count(sentiment, sort =T) 
  
  senByto <- sentimentedTokens %>% 
    group_by(to) %>% 
    as.data.frame() %>% 
    mutate(sentiment = factor(sentiment)) %>% 
    mutate(count = as.numeric(n)) %>% 
    select(to, sentiment, count) %>% 
    droplevels()
  
  sentPerc <- senByto %>% 
    group_by(to) %>% 
    mutate(total=sum(abs(count))) %>% 
    mutate(perc = abs(count)/total*100) %>% 
    mutate(perc = round(perc)) %>% 
    arrange(-total)
  
    p <- ggplot(sentPerc)
    p <- p + geom_bar(aes(fct_reorder(sentiment, -perc), perc, fill = sentiment), stat = "identity")
    p <- p + guides(fill = FALSE)
    p <- p + cleanTheme() +
      theme(
        panel.grid.major.y = element_line(color = "grey80", size = 0.5, linetype = "dotted"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title.x = element_blank())
    p <- p + scale_y_continuous("Percentage of words", breaks=seq(0,100, by=10))
    p <- p + facet_wrap(~to)
   
    p 
}


#### some funs ####

view_kable <- function(x, ...){
  tab <- paste(capture.output(kable(x, ...)), collapse = '\n')
  tf <- tempfile(fileext = ".html")
  writeLines(tab, tf)
  viewer(tf)
}


RemoveEmail <- function(x) {
  require(stringr)
  str_replace_all(x,"[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", "")
}


htmlStrip <- function(y) {
  return(gsub("<.*?>", "", y))
}

urlStrip <- function(z){
  return(gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", z))
}


#' makeCorpus
#'
#' Make a word corpus using tm pacakge from a datframe 
#' @param d A dataframe containing messages
#' @param wordlength Minimum word length 
#' @keywords corpus
#' @import tm, SnowballC, dplyr
#' @export
#' 
makeCorpus <- function(d){
  
  excludedWords <- c("best", "wishes", 'thanks', 'regards', 'Dear', 'to')
  
  docs <- Corpus(VectorSource(d$message)) %>%
    tm_map(content_transformer(htmlStrip)) %>%  # removing email ids
    tm_map(content_transformer(RemoveEmail)) %>%  # removing email ids
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(tolower))  %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(removeWords, excludedWords) %>%
    tm_map(stripWhitespace)
  
  # dataframe of terms
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  all <- data.frame(word = names(v),freq=v)
  
  all <- all %>% 
    filter(nchar(as.character(word)) < 25)
  
  return(all)
  
}



my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")

#Customize ggplot2's default theme settings
#This tutorial doesn't actually pass any parameters, but you may use it again in future tutorials so it's nice to have the options
theme_lyrics <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none")
{
  theme(plot.title = element_text(hjust = 0.5), #Center the title
        axis.ticks = aticks, #Set axis ticks to on or off
        panel.grid.minor = pgminor, #Turn the minor grid lines on or off
        legend.title = lt, #Turn the legend title on or off
        legend.position = lp) #Turn the legend on or off
}

#Customize the text tables for consistency using HTML formatting
my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}


cleanTheme <- function(base_size = 12){
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(color="black", size = 0.5),
    axis.line.y = element_line(color="black", size = 0.5),
    axis.text = element_text(size=15),
    axis.title.x=element_text(size=15),
    axis.title.y=element_text(size=15),
    strip.text = element_text(size=15)
  )
}

