
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


################### Train Data ###############################


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

## Dataset on Profits for select firms
Profit<-read.csv("/Users/avantikapal/Documents/ISB/DAM/Assignments/Final_Project/Qtr_Profit.csv", header=T, sep=",")
Profit_Long<- gather(Profit, Year, Profit, X20174:X20053, factor_key=TRUE)
Profit_Long$Year<-as.numeric(sapply(Profit_Long$Year, function(x) extractD(x) ))

Profit_Long1<-Profit_Long[order(Profit_Long[,1], Profit_Long[,2]), ]
Profit_Long1<-Profit_Long1[ Profit_Long1$Company!="Bain and Company"&
                              Profit_Long1$Company!= "Boston Consulting Group" & Profit_Long1$Company!= "Mckinsey"&
                              Profit_Long1$Company!="Slalom"& Profit_Long1$Company!="HEB", ]


Profit_Long1$ID<-paste(Profit_Long1$Company, Profit_Long1$Year, sep=" ")


################################################################################################
#### PREPROCESSING THE DATA ######
###############################################################################################

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
#### TOKENISING AND CREATING A DTM ######
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
#### WORDCLOUD ######
###############################################################################################

dtm<-dtm_tf
max.words1=100# max no. of words to accommodate
min.freq=2

# Wordcloud
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

################################################################################################
#### SUPPORT VECTOR REGRESSION ######
###############################################################################################

# Converting dtm to matrix
var<-as.matrix(dtm_tf)

#Train Data

var_train<-var[rownames(var) %in% (Profit_Long1[Profit_Long1$Year>=20121, "ID"]),]
Profit_train<-Profit_Long1[Profit_Long1$ID%in%rownames(var_train),]

#Test Data

var_test<-var[rownames(var) %in% (Profit_Long1[Profit_Long1$Year<20121, "ID"]),]
Profit_test<-Profit_Long1[Profit_Long1$ID%in%rownames(var_test),]


# Running Support Vector Regression on train data

model = svm(var_train,Profit_train[,3])
model

out = predict(model,var_train)
out

intercept<-model$rho

# Confusion matrix
confusion<-data.frame(out,Profit_train[,3])
confusion$Error<-confusion[,2]-confusion[,1]

w <- t(model$coefs) %*% model$SV                 # weight vectors
w <- apply(w, 2, function(v){sqrt(sum(v^2))})  # weight
w <- sort(w, decreasing = T)
print(w)

# weights for various words
weight_term<-as.data.frame(w)
weight_term$word<-rownames(weight_term)


# windows()  # New plot window
wordcloud(weight_term$word, w,     # words, their freqs
          scale = c(3.5, 0.5),     # range of word sizes
          10,                     # min.freq of words to consider
          max.words = 5000,       # max #words
          colors = brewer.pal(8, "Dark2")) 



confusion$ID<-rownames(confusion)
confusion$Date<-as.numeric(sapply(confusion$ID, function(x) extractD(x) ))
confusion$Company<-sapply(confusion$ID, function(x) extractCo(x) )
colnames(confusion)<-c("Predicted", "Actual", "Error", "ID", "Date", "Company")

p <- ggplot(confusion, aes(x=Date))+
   geom_point(aes(y=Predicted),color='red')+
  geom_point(aes(y=Actual), color='darkblue')+
  facet_wrap( ~ Company)+
  ylab("Net Profit Margin")



# Running Support Vector Regression on test data


out1 = predict(model,var_test)
out1

# Confusion matrix for test
confusion_test<-data.frame(out1,Profit_test[,3])
confusion_test$Error<-confusion_test[,2]-confusion_test[,1]


confusion_test$ID<-rownames(confusion_test)
confusion_test$Date<-as.numeric(sapply(confusion_test$ID, function(x) extractD(x) ))
confusion_test$Company<-sapply(confusion_test$ID, function(x) extractCo(x) )
colnames(confusion_test)<-c("Predicted", "Actual", "Error", "ID", "Date", "Company")



p_test <- ggplot(confusion_test, aes(x=Date))+
  geom_point(aes(y=Predicted),color='red')+
  geom_point(aes(y=Actual), color='darkblue')+
  facet_wrap( ~ Company)+
  ylab("Net Profit Margin")

