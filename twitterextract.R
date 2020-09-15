
library(rvest)
library(XML)
library(magrittr)
remove(suppressWarnings())

assign("last.warning", NULL, envir = baseenv())

#######################

#devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")
library("twitteR")

#install.packages("ROAuth")
library("ROAuth")

cred <- OAuthFactory$new(consumerKey='FXTquJNbgDG2dH81XYVqNZFAb', # Consumer Key (API Key)
                         consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

#install.packages("base64enc")
library(base64enc)

#install.packages("httpuv")
library(httpuv)

setup_twitter_oauth("FXTquJNbgDG2dH81XYVqNZFAb", # Consumer Key (API Key)
                    "3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO", #Consumer Secret (API Secret)
                    "529590041-qOXLd769cQEUTbXg3iRqCd33pC1K6xoORrGOMJDh",  # Access Token
                    "WlqZJwXFQzf64IuojkbKh1jdT5cnSY8U44pqmz6Sc1d4A")  #Access Token Secret

#registerTwitterOAuth(cred)

Tweets <- userTimeline('@imR045', n = 100,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)

write.csv(TweetsDF, "Rohit.csv",row.names = F)

#txt
write.table(TweetsDF$text, file = "Rohit.txt", sep = "\n")
getwd()


library(corpus)
library(tm)
library(tmap)


#install.packages("wordcloud")
library(wordcloud)

#install.packages("tm")
library(tm)

#install.packages("slam")
library(slam)

#install.packages("topicmodels")
library(topicmodels)

#install.packages("syuzhet")
library("syuzhet")

######################

x <- readLines("C:\\Users\\hp\\Documents\\Rohit.txt")
x
length(x)

mydata.corpus <- Corpus(VectorSource(x))

mydata.corpus <- tm_map(mydata.corpus, removePunctuation)

my_stopwords <- c(stopwords('english'),"brothers", "sisters", "the", "due", "are", "not", "for", "this", "and",  "that", "there", "new", "near", "beyond", "time", "from", "been", "both", "than",  "has","now", "until", "all", "use", "two", "ave", "blvd", "east", "between", "end", "have", "avenue", "before",    "just", "mac", "being",  "when","levels","remaining","based", "still", "off", "over", "only", "north", "past", "twin", "while","then")

mydata.corpus <- tm_map(mydata.corpus, removeWords, my_stopwords)

mydata.corpus <- tm_map(mydata.corpus, removeNumbers)

mydata.corpus <- tm_map(mydata.corpus, stripWhitespace)



#making wordcloud

dtm=TermDocumentMatrix(mydata.corpus)
m=as.matrix(dtm)
v=sort(rowSums(m),decreasing = TRUE)
d=data.frame(word=names(v),freq=v)
head(d,10)
set.seed(1234)
wordcloud(words = d$word,freq=d$freq,min.freq = 1,max.words = 800,random.order = FALSE, rot.per = 0.10,colors = brewer.pal(9,"Dark2"))


#plots

s_v <- get_sentences(x)
class(s_v)
str(s_v)
head(s_v)

sentiment_vector <- get_sentiment(s_v, method = "bing")
head(sentiment_vector)

afinn_s_v <- get_sentiment(s_v, method = "afinn")
head(afinn_s_v)

nrc_vector <- get_sentiment(s_v, method="nrc")
head(nrc_vector)

sum(sentiment_vector)
mean(sentiment_vector)
summary(sentiment_vector)

# plot
plot(sentiment_vector, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red")

# To extract the sentence with the most negative emotional valence
negative <- s_v[which.min(sentiment_vector)]
negative


# negative
#[1] "\"A bit shabby &amp; smelling of books, dust and happiness...""



# and to extract the most positive sentence
positive <- s_v[which.max(sentiment_vector)]
positive


#[1] "I consider my work with these heroic &amp;
#beautiful women a great privilege as it imparts dig. https://t.co/ylIA8WhfnB\"


# more depth
poa_v <- x
poa_sent <- get_sentiment(poa_v, method="bing")
plot(
  poa_sent, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

# percentage based figures
percent_vals <- get_percentage_values(poa_sent)

plot(
  percent_vals, 
  type="l", 
  main=" Percentage-Based Means", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)

ft_values <- get_transformed_values(
  poa_sent, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  ft_values, 
  type ="h", 
  main ="", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)


