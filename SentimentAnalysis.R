

if (!require(tidytext)) {install.packages("tidytext")}
if (!require(tidyr)) {install.packages("tidyr")}
if (!require(dplyr)) {install.packages("dplyr")}

require(tidytext)
require(tidyr)
require(dplyr)

# Sentiment Analysis using Bing

senti_bing<- function(corpus){
  
  textdf = data_frame(text = corpus) # converting text data into dataframe  
  
  senti<-  textdf %>%
    mutate(Doc_No = row_number()) %>%   # build line num variable
    ungroup() %>%
    unnest_tokens(word, text) %>%   #Unnesting in word tokens
    inner_join(get_sentiments("bing")) %>%  #  merging with sentiments in bing
    count(sentiment, index = Doc_No %/% 1, sort = FALSE) 
  
  senti_df = data.frame(senti %>% spread(sentiment, n, fill = 0))  #reshaping by sentiments
  
  return(senti)
} # Function ends


# Sentiment Analysis using nrc

senti_nrc<- function(corpus){
  
  textdf = data_frame(text = corpus) # converting text data into dataframe  
  
  senti<-  textdf %>%
    mutate(Doc_No = row_number()) %>%   # build line num variable
    ungroup() %>%
    unnest_tokens(word, text) %>%   #Unnesting in word tokens
    inner_join(get_sentiments("nrc")) %>%  #  merging with sentiments in nrc
    count(sentiment, index = Doc_No %/% 1, sort = FALSE) 
  
  senti_df = data.frame(senti %>% spread(sentiment, n, fill = 0))  #reshaping by sentiments
  
  return(senti)
} # Function ends
