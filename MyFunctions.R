
  if (!require(tidytext)) {install.packages("tidytext")}
  if (!require(stringr)) {install.packages("stringr")}
  if (!require(magrittr)) {install.packages("magrittr")}
  if (!require(dplyr)) {install.packages("dplyr")}
  if (!require(tibble)) {install.packages("tibble")}
  if (!require(wordcloud)) {install.packages("wordcloud")}
  if (!require(ggplot2)) {install.packages("ggplot2")}
  
  library(wordcloud)
  library(ggplot2)
  library(magrittr)
  library(tibble)
  library(tidytext)
  library(stringr)

#  Function 1: Text Cleaning

text.clean = function(corpus, mystopwords= NA)                  # text data
{

  
  corpus  =  gsub("<.*?>", " ", corpus)               # regex for removing HTML tags
  corpus  =  iconv(corpus, "latin1", "ASCII", sub="") # Keep only ASCII characters
  corpus  =  gsub("[^[:alnum:]]", " ", corpus)        # keep only alpha numeric 
  corpus  =  tolower(corpus)                          # convert to lower case characters
  corpus  =  gsub("\\d+", "", corpus)                    # removing numbers
  corpus  =  stringr::str_replace_all(corpus,"[\\s]+", " ")  # removing white space
  corpus  =  gsub("^\\s+|\\s+$", "", corpus)          # remove leading and trailing white space
  
  # Read Stopwords list
  stpw1 = mystopwords
  stpw2 = tidytext::stop_words$word    # tidytext package stop word list; 
  comn  = unique(c(stpw1, stpw2))         # Union of two list
  all_stopwords = unique(gsub("'"," ",comn)) # final stop word list after removing punctuation
  
  
  # Function to removing stop words created above 
  removeStopwords<- function( string){
    
    x <- unlist(strsplit(string, " "))
    x <- x[!(x %in% all_stopwords)]
    x <- paste(x, collapse = " ") 
    
    return(x)
  }
  
  # Applying the function on the cleaned corpus to return a character vector
  final<-as.character(lapply(corpus, function (x) removeStopwords (x)))
  
  final = final[(final != "")]    # purge empty rows
  
  return(final)
}


# Function 2 : Func to build a DTM with both TF and TFIDF weighting schemes

build_dtm<-function(x,
                    n = 1 # dimension of ngram 
)
{


  textdf = data_frame(text = x) # yields length(x) x1 tibble. i.e., each doc = 1 row here.
  
  ### create doc id and group_by it
  textdf_doc = textdf %>% mutate(doc = seq(1:nrow(textdf))) %>% group_by(doc)
  
  ### Count of the number of times an ngram occurs grouped by  document
  textdf_word = textdf_doc %>% 
    group_by(doc) %>% 
    unnest_tokens(ngram, text, token = "ngrams", n = n)  %>% 
    group_by(doc,ngram)%>%
    mutate(word1 = 1) %>% 
    # select(doc, word1) %>%
    group_by(doc, ngram) %>% 
    summarise(term_count = sum(word1))
  
  ## converting to a tibble to create a dtm
  ap_tidy = tibble::as_data_frame(textdf_word)
  ap_tidy
  
  
  # cast into a Document-Term Matrix
  dtm_tf<-ap_tidy %>%
    cast_dtm(doc, ngram, term_count)
  
  dtm_tfidf<-ap_tidy %>%
    cast_dtm(doc, ngram, term_count, weighting= tm::weightTfIdf)
  
  
  out<-list("dtm_tf" = dtm_tf, "dtm_tfidf"= dtm_tfidf )
  
  return(out)
}




## Function 3: Visual Inspection of dtm

display_graphics <- function(dtm, #Document term matrix
                             max.words1=100,	# max no. of words to accommodate
                             min.freq=2,	# min.freq of words to consider
                             title1="", # write within double quotes
                             n_bar=15,  # No. oftokens to be shown in barchart
                             s=5,    # no. of central nodes
                             k1=5
)   # max no. of connections  
{        
  
 
  
  #-----------------------------------------------------------#
  # Wordcloud            #
  #-----------------------------------------------------------#
  
  if (ncol(dtm) > 20000){   # if dtm is overly large, break into chunks and solve
    
    tst = round(ncol(dtm)/100)  # divide DTM's cols into 100 manageble parts
    a = rep(tst,99)
    b = cumsum(a);rm(a)
    b = c(0,b,ncol(dtm))
    
    ss.col = c(NULL)
    for (i in 1:(length(b)-1)) {
      tempdtm = dtm[,(b[i]+1):(b[i+1])]
      s = colSums(as.matrix(tempdtm))
      ss.col = c(ss.col,s)
      print(i)		} # i loop ends
    
    tsum = ss.col
    
  } else { tsum = apply(dtm, 2, sum) }
  
  tsum = tsum[order(tsum, decreasing = T)]       #terms in decreasing order of freq
  # head(tsum)
  # tail(tsum)
  
  # windows()  # New plot window
  wordcloud(names(tsum), tsum,     # words, their freqs 
            scale = c(3.5, 0.5),     # range of word sizes
            min.freq,                     # min.freq of words to consider
            max.words = max.words1,       # max #words
            colors = brewer.pal(8, "Dark2"))    # Plot results in a word cloud 
  title(sub = title1)     # title for the wordcloud display
  
  
  
  #-----------------------------------------------------------#
  # Bar Chart            #
  #-----------------------------------------------------------#
  
  
  a0 = apply(dtm, 2, sum)
  a1 = order(a0, decreasing = TRUE)
  tsum = a0[a1]
  
  # plot barchart for top tokens
  test = as.data.frame(round(tsum[1:n_bar],0))
  
  # windows()  # New plot window
  
  print( ggplot(test, aes(x = rownames(test), y = test)) + 
           geom_bar(stat = "identity", fill = "Blue") +
           geom_text(aes(label = test), vjust= -0.20) + 
           theme(axis.text.x = element_text(angle = 90, hjust = 1)))
  
  
  
  #-----------------------------------------------------------#
  # A cleaned up or 'distilled' COG Plot            #
  #-----------------------------------------------------------#
  
  dtm1 = as.matrix(dtm)   # need it as a regular matrix for matrix ops like %*% to apply
  adj.mat = t(dtm1) %*% dtm1    # making a square symmatric term-term matrix 
  diag(adj.mat) = 0     # no self-references. So diag is 0.
  a0 = order(apply(adj.mat, 2, sum), decreasing = T)   # order cols by descending colSum
  adj.mat = as.matrix(adj.mat[a0[1:50], a0[1:50]])   # taking the top 50 rows and cols only
  
  mat1<-adj.mat
  library(igraph)
  a = colSums(mat1) # collect colsums into a vector obj a
  b = order(-a)     # nice syntax for ordering vector in decr order  
  
  mat2 = mat1[b, b]     # order both rows and columns along vector b
  
  diag(mat2) =  0
  
  ## +++ go row by row and find top k adjacencies +++ ##
  
  wc = NULL
  
  for (i1 in 1:s){ 
    thresh1 = mat2[i1,][order(-mat2[i1, ])[k1]]
    mat2[i1, mat2[i1,] < thresh1] = 0   # neat. didn't need 2 use () in the subset here.
    mat2[i1, mat2[i1,] > 0 ] = 1
    word = names(mat2[i1, mat2[i1,] > 0])
    mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
    wc = c(wc,word)
  } # i1 loop ends
  
  
  mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
  ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
  mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
  graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)    # Create Network object
  graph = simplify(graph) 
  V(graph)$color[1:s] = "green"
  V(graph)$color[(s+1):length(V(graph))] = "pink"
  
  graph = delete.vertices(graph, V(graph)[ degree(graph) == 0 ]) # delete singletons?
  
  plot(graph, 
       layout = layout.kamada.kawai, 
       main = title1)
  # 
  # out<-list("WordCloud" =disp_wordcloud, "BarChart" =disp_barchart, 
  #           "Co-occurrence Graph"= disp_cog)       
  # 
  # return(out)
} # func ends
                             
                             
                             
                             
#Function to compute time difference from the peak time
minutes<-function(timestamp, centroid,units= "mins"){
  
    # timestamp should be a "POSIXct" "POSIXt"  object
    # centroid should be a time of the "%H:%M" format
    center<-as.POSIXlt(paste(substr(as.POSIXlt(centroid, tz = "", format= c("%H:%M")), 1, 8), "01",
                             substr(as.POSIXlt(centroid, tz = "", format= c("%H:%M")),11,23), sep="" ))
    Difftime<-difftime( timestamp,center, units=units)
  
  }
      
