suppressMessages(library(SnowballC))
suppressMessages(library(wordcloud))
suppressWarnings(suppressMessages(library(dplyr)))
suppressMessages(library(tm))
suppressMessages(library(tidytext))

library(ggplot2)
library(forcats)
library(snakecase)
library(stringr)
library(formattable) #For the color_tile function
# library(evaluate)
library(knitr)


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
    droplevels()
  
  return(cleanData)
}

wordFreq <- function(file_in='data/nick_email.tsv', cloud=F, wordlength=3, top=15 ){
  
  all <- makeCorpus(df=data, wordlength = wordlength)
  
  if(cloud){
    wordcloud(words = all$word, freq = all$freq, min.freq = 5,
              max.words=100, random.order=FALSE, rot.per=0.35,
              colors=brewer.pal(8, "Dark2"),scale=c(4,1.2))
  } else{

    d <- all[1:top,]
    d  <- transform(d , word = reorder(word, freq))

    division <- as.integer(max(d$freq)/10)
    p <- ggplot(d)
    p <- p + geom_bar(aes(word, freq, fill="springgreen3"),stat='identity')
    p <- p + scale_y_continuous("Word frequency", breaks=seq(0,max(d$freq),by=division),expand=c(0.01,0))
    p <- p + scale_x_discrete("Word", expand = c(0.01,0.01))

    p <- p + cleanTheme() +
      theme(
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text = element_text(size=15),
        panel.grid.major.x = element_line(color="grey80", size = 0.5, linetype = "dotted")

      )

    p <- p + scale_fill_identity()
    p <- p + coord_flip()
    p
  }
}


contributions <- function(file_in='data/nick_email.tsv', df=NA, recipients = 5, method='loughran'){
  
  if(is.na(df)){
    data <- readEmails(file_i)
  } else {
    data <- df
  }
  
  tokens <- data %>% 
    unnest_tokens(word, message) %>% 
    filter(!word %in% excludedWords) %>% 
    anti_join(stop_words) #Data provided by the tidytext package
  
    
  sentimentedTokens <- tokens %>%
    inner_join(get_sentiments(method)) %>% # pull out only sentiment words
    count(word) %>% 
    arrange(desc(n))
  
  
  wordtbl <- sentimentedTokens %>%
    #Right join gets all words in `my_word_list` to show null
    mutate(word = color_tile("lightblue", "lightblue")(word),
           instances = color_tile("lightpink", "lightpink")(n))
    select(-n) %>% #Remove these fields
    my_kable_styling(caption = "Dependency on Word Form")
  
    viewer <- getOption("viewer")
    viewer("http://localhost:8000")
    view_kable(head(wordtbl), format = 'html', table.attr = "class=nofluid")
    
  
}


emailSentiments <- function(file_in='data/nick_email.tsv', df=NA, recipients = 5, method='loughran'){

  if(is.na(df)){
    data <- readEmails(file_i)
  } else {
    data <- df
  }


  # Get the top 5 recipients
  topRecips <- data %>%
    group_by(to) %>% 
    tally() %>% 
    top_n(n=recipients)
  
  filtData <- cleanData %>% 
    filter(to %in% topRecips$to) %>% 
    droplevels()
  
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

makeCorpus <- function(df, wordlength=4){
  
  excludedWords <- c("none", "nick", "riddiford")
  
  docs <- Corpus(VectorSource(df$message)) %>%
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
    filter(nchar(as.character(word))>=wordlength) %>%
    filter(nchar(as.character(word)) < 25) %>%
    droplevels()
  
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

