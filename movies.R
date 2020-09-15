library(rvest)
library(XML)
library(magrittr)

library(rvest)
library(XML)
library(magrittr)


# movies Reviews #############################
aurl <- "https://www.imdb.com/title/tt0993846/reviews?ref_=tt_ql_3"
IMDB_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
write.table(IMDB_reviews,"wolf.txt",row.names = F)
getwd()


library(tm)
library(tmap)
y <- readLines("D:\\excelR\\assignment\\txt files")
y
length(y)

mydata.corpus <- Corpus(VectorSource(y))

mydata.corpus <- tm_map(mydata.corpus, removePunctuation)

my_stopwords <- c(stopwords('english'),"brothers", "sisters", "the", "due", "are", "not", "for", "this", "and",  "that", "there", "new", "near", "beyond", "time", "from", "been", "both", "than",  "has","now", "until", "all", "use", "two", "ave", "blvd", "east", "between", "end", "have", "avenue", "before",    "just", "mac", "being",  "when","levels","remaining","based", "still", "off", "over", "only", "north", "past", "twin", "while","then")

mydata.corpus <- tm_map(mydata.corpus, removeWords, my_stopwords)

mydata.corpus <- tm_map(mydata.corpus, removeNumbers)

mydata.corpus <- tm_map(mydata.corpus, stripWhitespace)



install.packages("wordcloud")
library(wordcloud)






#making wordcloud

dtm=TermDocumentMatrix(mydata.corpus)
m=as.matrix(dtm)
v=sort(rowSums(m),decreasing = TRUE)
d=data.frame(word=names(v),freq=v)
head(d,10)
set.seed(1234)
wordcloud(words = d$word,freq=d$freq,min.freq = 3,max.words = 800,random.order = FALSE, rot.per = 0.10,colors = brewer.pal(9,"Dark2"))





install.packages("tm")
library(tm)

install.packages("slam")
library(slam)

install.packages("topicmodels")
library(topicmodels)






#plots







install.packages("syuzhet")
library("syuzhet")



my_example_text <- readLines("C:\\Users\\Ashish\\Documents\\wolf.txt")


s_v <- get_sentences(y)
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




# and to extract the most positive sentence
positive <- s_v[which.max(sentiment_vector)]
positive




# more depth
poa_v <- my_example_text
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






# Making positive wordcloud function
makeposwordc = function(d$word){
  freq = sort(rowSums(as.matrix(d$word)), decreasing = TRUE)
  # matching positive words
  pos.matches = match(names(freq), c(pos.words,"approvals"))
  pos.matches = !is.na(pos.matches)
  freq_pos <- freq[pos.matches]
  names <- names(freq_pos)
  windows()
  wordcloud(names,freq_pos,scale = c(4,.5), colors = brewer.pal(8, "Dark2"))
}

