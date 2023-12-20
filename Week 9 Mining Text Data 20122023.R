#mining Text Data
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("syuzhet")
install.packages("ggplot2")
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(ggplot2)

#Read Text data
text = readLines(file.choose())
text
class(text)

#define Courpus text
docs = Corpus(VectorSource(text))
class(docs)

#data cleaning of text data
#  i. Remove simbol
#define blank space

toSpace = content_transformer(function(x,pattern)gsub(pattern,"",x))

#convert all special character into blank space


docs2 = tm_map(docs,toSpace,"/")
docs2 = tm_map(docs2,toSpace,"@")
docs2 = tm_map(docs2,toSpace,"|")
docs2 = tm_map(docs2,toSpace,"!")
docs2 = tm_map(docs2,toSpace,",")
docs2 = tm_map(docs2,toSpace,":")
#docs2 = tm_map(docs2,toSpace,".")
inspect(docs2)

#ii Convet to lower case
docs2 = tm_map(docs2,content_transformer(tolower))
docs2
inspect(docs2)

# iii remove numbers
docs2 = tm_map(docs2,removeNumbers)
inspect(docs2)

# iv Remove stop word
docs2 = tm_map(docs2,removeWords, stopwords("english"))
inspect(docs2)

#Remove punntucion 
docs2 = tm_map(docs2,removePunctuation)

#Remove white space
docs2 = tm_map(docs2,stripWhitespace)

inspect(docs2)

#Word token
library(tidyverse)
library(tokenizers)
install.packages("tokenizers")

word.Tk = tokenize_words(text)

#text steaming 
docs3 = tm_map(docs2,stemDocument)
inspect(docs3)

#Document - term matricx
dtm = TermDocumentMatrix(docs3)
dtm
m = as.matrix(dtm)
m
dim(m)
#freq work table of words
v = sort(rowSums(m),decreasing = T)
d =data.frame(word=names(v),freq=v)

#plot word freq 
#first 20 most freq word
barplot(d[1:20,]$freq,names.arg=d[1:20,]$word,
        main="Most Frequent Word")

#word cloud
set.seed(12)
wordcloud(words = d$word,freq=d$freq,min.freq = 2,
          max.words = 100,random.order = F, color=brewer.pal(8,"Dark2"))


### Word Association 
#analyze the assocition between frequent words
#Eg: Which word words are associted "freedom"?
findAssocs(dtm,terms = "freedom",corlimit = 0.3)$freedom

# assiotion with particulur word
findAssocs(dtm,terms = c("freedom","dream",'will'),corlimit = 0.25)

# assiotion with  word at least 10times
findAssocs(dtm,terms =findFreqTerms(dtm,lowfreq=10),corlimit = 0.25)


#Sentiment Analysis
install.packages('sentimentr')
library(sentimentr)

x = 'Sentiment Anlysis is super fun'
sentiment(x)

y = 'Sentiment Anlysis is super boring. I do love working in R'
sentiment(y)

sentiment_sz = get_sentiment(text, method ='syuzhet')
text

hist(sentiment_sz)
mean(sentiment_sz)
summary(sentiment_sz)


#Emotion classification
Ec<- get_nrc_sentiment(text)
td<-data.frame(t(Ec))
td_new<- data.frame(rowSums(td))

# transformation & cleaning
names(td_new)[1]<-"count"
td_new<- cbind("sentiment"= rownames(td_new), td_new)
rownames(td_new) <-NULL
td_new2<- td_new[1:8,]

quickplot(sentiment, data= td_new2, weight = count,
          geom = "bar", fill = sentiment, ylab = "count") +
  ggtitle("Survey Sentiment")
