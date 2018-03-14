
################################################################################################
                                #### LOADING LIBRARIES ######
###############################################################################################

source('https://raw.githubusercontent.com/avantikapal22/Text-Analytics/master/MyFunctions.R')
try(require(dplyr) || install.packages("dplyr"))
library(dplyr)
require(tidytext) || install.packages("tidytext")
library(tidytext)
try(require(tidyr) || install.packages("tidyr"))
library(tidyr)
require(tibble)
library(tm)
library(Matrix.utils)
library(lubridate)
library(SnowballC)
library(topicmodels)
library(reshape2)
library(e1071)
library(entropy)


################################################################################################
                              #### CUSTOMISED FUNCTIONS ######
###############################################################################################

#Extract Dates
extractD<-function(string){
  
  k<-str_extract(string,"\\d{5}")
  return(k)
}

#Extract Company
extractCo<-function(string){
  
  k<-unlist(strsplit(string, " ", 2))[1]
  return(k)
}


################################################################################################
                                #### READING IN THE DATA ######
###############################################################################################

### Sample Dataset on Reviews on Glassdoor
data<-read.csv("/Users/avantikapal/Documents/ISB/DAM/Assignments/Final_Project/Sample1.csv", header=T, sep=",")[,-1]
data$year<-year(data$Date)
data$qtr<-NA
data$qtr[month(data$Date)<=3 &month(data$Date)>=1]<-"1"
data$qtr[month(data$Date)<=6 &month(data$Date)>3]<-"2"
data$qtr[month(data$Date)<=9 &month(data$Date)>6]<-"3"
data$qtr[month(data$Date)<=12 &month(data$Date)>9]<-"4"

data$D1<-paste(data$year,data$qtr, sep="")

### Sample Dataset on Reviews on Glassdoor
data1<-read.csv("/Users/avantikapal/Documents/ISB/DAM/Assignments/Final_Project/Sample2.csv", header=T, sep=",")[,-1]
data1$year<-year(data1$Date)
data1$qtr<-NA
data1$qtr[month(data1$Date)<=3 &month(data1$Date)>=1]<-"1"
data1$qtr[month(data1$Date)<=6 &month(data1$Date)>3]<-"2"
data1$qtr[month(data1$Date)<=9 &month(data1$Date)>6]<-"3"
data1$qtr[month(data1$Date)<=12 &month(data1$Date)>9]<-"4"

data1$D1<-paste(data1$year,data1$qtr, sep="")

Full_Data<-rbind(data, data1)
rev<-Full_Data$Review
co<-str_replace_all(unique(Full_Data$Company), "-", " ") %>% tolower()


## Human Capital Glosaary
Glossary<-readLines("/Users/avantikapal/Documents/ISB/DAM/Assignments/Final_Project/HR Mgmt Keywords.txt")
g<-tolower(Glossary)
g<-wordStem(g)

mystopwords<-c(co,"starstarstarstarstar", "company", "managementcurrent", "employee", "anonymous",
               "employeecurrent", "ceoi", "institute", "sas", "facebookshare","twittershare",
               "whatsappshare", "emailcopy", "linklink", "copied", "flag", "inappropriateflag",
               "inappropriatehelpful", "response", "ago", "edit", "delete", "sas", "institute", "pst","helpul",
               "management", "managementformer", "pdf", "time", "share", "organisation", "current", "engineer", 
               "technical", "york", "google", "bank", "capital", "job", "manager", "software","analyst", "managers", 
               "companies", "business", "technology", "holdings","aug","oct", "feb","sep","uk", "america", "employeeformer",
               "office", "dec", "adobe", "morgan", "ca", "ericsson", "systems","staff", "pdt", "senior",
               "helpful", "nov", "jan","apr", "may", "jun","jul", "consultant", "england", "ny", "associate",
               "cons","sachs", "goldman","pros", "assistant", "starstarstarstarstarwork", "employees", "people", "life",
               "balanceculture", "benefitssenior", "opportunitiescomp", "valuescareer", "tx", "technologies", "organisations",
               "teleperformance", "san", "teams", "balancecareer","mar", "industry","executive","starstarstarstarstarcurrent",
               "times", "president", "feb", "jan", "february")


k<-data_frame(mystopwords)

rev<-as.character(rev)
textdf = data_frame(text = rev) # yields length(x) x1 tibble. i.e., each doc = 1 row here.

################################################################################################
                        #### TOKENISATION AND DTM CREATION ######
###############################################################################################

### create doc id and group_by it
textdf_doc = textdf %>% mutate(doc = paste(Full_Data$Company,Full_Data$D1),year=Full_Data$year ) %>% group_by(doc)

### Count of the number of times an ngram occurs grouped by  document
textdf_word = textdf_doc %>% 
  unnest_tokens(word, text)  %>% 
  anti_join(stop_words)%>%
  anti_join(k, by=c("word"="mystopwords"))%>%
  mutate(word = wordStem(word))%>%
  group_by(doc,word)%>%
  mutate(word1 = 1) %>% 
  # select(doc, word1) %>%
  group_by(doc, word) %>% 
  summarise(term_count = sum(word1))%>%
  mutate("nchar"=nchar(word))%>%
  filter(nchar>3)

## converting to a tibble to create a dtm
ap_tidy = tibble::as_data_frame(textdf_word)
ap_tidy

# cast into a Document-Term Matrix
dtm_tf<-ap_tidy %>%
  cast_dtm(doc, word, term_count)


################################################################################################
                            #### FITTING TOPIC MODEL ######
###############################################################################################

para_lda <- LDA(dtm_tf, k = 3, control = list(seed = 1234))
para_lda    

# examine per-topic-per-word probabilities.
para_topics <- tidy(para_lda, matrix = "beta")
para_topics    

# use dplyr's top_n() to find the top 5 terms within each topic.
top_terms <- para_topics %>%
  group_by(topic) %>%
  top_n(300, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms   



k1<-top_terms[top_terms$term %in% g,]
table(k1$topic)

################################################################################################
                          ### WORDCLOUD VISUALISATION ######
###############################################################################################



wordcloud(k1$term[k1$topic==1], k1$beta[k1$topic==1],     # words, their freqs
          scale = c(3.5, 0.5),     # range of word sizes
          10,                     # min.freq of words to consider
          max.words = 50,       # max #words
          colors = brewer.pal(8, "Dark2")) 

wordcloud(k1$term[k1$topic==2], k1$beta[k1$topic==2],     # words, their freqs
          scale = c(3.5, 0.5),     # range of word sizes
          10,                     # min.freq of words to consider
          max.words = 50,       # max #words
          colors = brewer.pal(8, "Dark2")) 


wordcloud(k1$term[k1$topic==3], k1$beta[k1$topic==3],     # words, their freqs
          scale = c(3.5, 0.5),     # range of word sizes
          10,                     # min.freq of words to consider
          max.words = 50,       # max #words
          colors = brewer.pal(8, "Dark2")) 


################################################################################################
                          ### CHOOSING TOPIC 1 ######
###############################################################################################

culture_index<-k1$term[k1$topic==1]

# Frequency Distribution
temp<-ap_tidy[ap_tidy$word %in% culture_index, ]
l1<-aggregate(temp$term_count~temp$word, temp, sum, na.rm=T)
l1$Proportion<-l1$`temp$term_count`/sum(temp$term_count)

# Now visualize via ggplot
library(ggplot2)
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

################################################################################################
                                  #### CONSTRUCTING TIMESERIES OF THE INDEX ######
###############################################################################################

CI<-k1[k1$topic==1,]

# Converting dtm to matrix
var<-as.matrix(dtm_tf)

var1<-subset(var, select=culture_index)

Timeseries_CI<-var1 %*% CI$beta


Company_Cultural_Index<-data.frame("ID"=rownames(var1), "Cultural_Index"=Timeseries_CI )
Company_Cultural_Index$Year<-as.numeric(sapply(Company_Cultural_Index$ID, function(x) extractD(x) ))
Company_Cultural_Index$Company<-sapply(as.character(Company_Cultural_Index$ID), function(x) extractCo(x) )
Company_Cultural_Index$Company[Company_Cultural_Index$Company=="Southwest"]<-"Southwest Airlines"

Profit<-read.csv("/Users/avantikapal/Documents/ISB/DAM/Assignments/Final_Project/Qtr_Profit.csv", header=T, sep=",")
Profit_Long<- gather(Profit, Year, Profit, X20174:X20053, factor_key=TRUE)
Profit_Long$Year<-as.numeric(sapply(Profit_Long$Year, function(x) extractD(x) ))

Profit_Long1<-Profit_Long[order(Profit_Long[,1], Profit_Long[,2]), ]
Profit_Long1<-Profit_Long1[ Profit_Long1$Company!="Bain and Company"&
                              Profit_Long1$Company!= "Boston Consulting Group" & Profit_Long1$Company!= "Mckinsey"&
                              Profit_Long1$Company!="Slalom"& Profit_Long1$Company!="HEB", ]

Combined<-merge(Profit_Long1, Company_Cultural_Index[, c("Company","Year", "Cultural_Index")],
                by=c("Company", "Year"), all.x=T)
write.csv(Combined, "/Users/avantikapal/Documents/ISB/DAM/Assignments/Final_Project/ProfitvsCulture.csv")
m<-na.omit(Combined)

cor(m$Profit,m$Cultural_Index)

################################################################################################
                          #### VISUALISATION ######
###############################################################################################



### Plot Profit vs cultural index
p <- ggplot(m[m$Company=="Adobe-India",], aes(x = Year))+
    geom_line(aes(y = Profit, colour = "Profit"))+
    geom_line(aes(y = Cultural_Index, colour = "Cultural_Index"))+
    facet_wrap( ~ Company)
    scale_y_continuous(sec.axis = sec_axis(~.*-0.486 +1.020,name = "Cultural_Index"))+
    

# modifying colours and theme options
p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Air temperature [??C]",
              x = "Date and time",
              colour = "Parameter")
p <- p + theme(legend.position = c(0.8, 0.9))
p


# topic_test <- ggplot(Combined, aes(x=Year))+
#   geom_point(aes(y=Profit),color='red')+
#   geom_point(aes(y=Actual), color='darkblue')+
#   facet_wrap( ~ Company)+
#   ylab("Net Profit Margin")

