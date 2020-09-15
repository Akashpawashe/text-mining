getwd()
setwd("D:\\excelR\\assignment\\txt files")
library(rvest)
library(XML)
library(magrittr)
remove(suppressWarnings())
# Amazon Reviews ######
aurl <- "https://www.amazon.in/Apple-AirPods-Wireless-Charging-Case/product-reviews/B07QDRYVCZ/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews"
amazon_review <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_review <- c(amazon_review,rev)
}

#storing all reviews in file
write.table(amazon_review,"airpods.txt",row.names = F)

#Both works
write.table(amazon_reviews, file = "appl.txt", sep = "\n")




install.packages("wordcloud")
library(wordcloud)

x <- readLines("D:\\excelR\\assignment\\txt files")
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
wordcloud(words = d$word,freq=d$freq,min.freq = 3,max.words = 800,random.order = FALSE, rot.per = 0.10,colors = brewer.pal(9,"Dark2"))


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
#[1] "\"The product which is given is of poor qualiti and a Defect product"



# and to extract the most positive sentence
positive <- s_v[which.max(sentiment_vector)]
positive


#[1] "I got those from Amazon just in two day and the product is genuine, I have activ warranti on Apple websit and it work magic with my iPhon XS Max, believ me it profound joy to use these magic thing with iPhon XS Max,
#it work flawless with my iPhone, MacBook and Apple Watch."


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

