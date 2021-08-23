rm(list=ls())

library(ggplot2)
library(tidyverse)
library(tidytext)
library(tm)
library(wordcloud)
library(ggraph)
library(igraph)
library(quanteda)
library(topicmodels)
library(tidyr)
library(plotly)

Data1 <- read.csv2("C:/Users/juliana.forero/Desktop/Alqueria/mentions.csv") 
str(Data1)

Data1$text <- as.character(Data1$text)
Data1$Query.Name <- as.character(Data1$Query.Name)
Data1$Title <- as.character(Data1$Title)
Data1$Sentiment <- as.character(Data1$Sentiment)
Data1$Page.Type <- as.character(Data1$Page.Type)
Data1$Full.Name <- as.character(Data1$Full.Name)
Data1$Gender <- as.character(Data1$Gender)


tidy_books <- Data1 %>% unnest_tokens(word, text)


stop_word <- bind_rows(data_frame(word = tm::stopwords("spanish"),
                                  lexicon = "SMART"), data.frame(word = c(c(1:5000), "ver",  "nhoy", "asi", "junto", "n", "rt", "pic.twitter.com", "bit.ly", "hoy", "tyc", "ey", "si", "ser", "solo", "raydmdl", "asi", "m", "rfvsrbnugg", "vs", "1,5", "1.500.000", "c", "9na", "rruldftfji", "t", "vhwhoaxvp5", "jthgsicy7b", "pgn_col", "s", "0", "36.000", "6onihuoc8x", "ncb41ud76y", "2vabf3t5hy", "00", "c4r0y3syzl", "oxdw6oy7wg", "ruluqcr2b7", "7caudpephj", "3209754897", "q9hays3vir", "2tk9ox8v33", "36.384", "6vtdatzagt")))


tidy_books <- tidy_books %>% anti_join(stop_word)

count <- tidy_books %>% count(word, sort = TRUE)%>% mutate(Participacion=round((n/sum(n))*100,2)) 

a <- tidy_books %>% count(word, sort = TRUE) %>% filter(n > 140) %>%  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()+ labs(x="Frecuencia de mención",
                                                                      y="Palabra",
                                                                      title="Frecuencia de palabras que aparecen en las menciones")

ggplotly(a)


# AnÃ¡lisis de sentimientos
download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
              "lexico_afinn.en.es.csv")

afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()

tuits_afinn <- afinn %>% mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa"))

tidy_books %>% inner_join(tuits_afinn, by=c("word"="Palabra")) %>% count(word, sort=TRUE)

a <- tidy_books %>% inner_join(tuits_afinn, by=c("word"="Palabra")) %>% count(word, sort=TRUE) %>%
  filter(n > 70) %>%
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(word,n)) + xlab(NULL) + coord_flip() + geom_col()+ labs(x="Palabra",
                                                                     y="Frecuencia de palabra",
                                                                     title="Frecuencia de palabras asociadas a un sentimiento según léxico afin en español, que aparecen en las menciones")


ggplotly(a)



Tidy_Tokens2 <- tidy_books %>% group_by(Gender) %>% mutate(id = row_number())

Sentiment_gender <- Tidy_Tokens2 %>% inner_join(tuits_afinn, by=c("word"="Palabra")) %>% count(Page.Type,index=id %/% 10, Tipo)   %>% 
  spread(Tipo, n, fill= 0) %>% mutate(sentiment = Positiva - Negativa)

ggplot(Sentiment_gender, aes(index, sentiment, fill = Page.Type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Page.Type,ncol = 2, scales= "free_x")




## Wordclouds

tidy_books %>% anti_join(stop_word) %>% count(word) %>% 
  with(wordcloud(word, n, scale=c(9,.5), max.words = 100, colors = "red"))


######## Bi-gramos ######

bigram <- Data1 %>% unnest_tokens(word, text, token = "ngrams", n = 10) %>% 
  count(word, sort = TRUE)

bigram_sep <- bigram %>%  separate(word, c("word1", "word2"), sep = " ")

bigram_filt <- bigram_sep %>%  filter(!word1 %in% stop_word$word) %>%  
  filter(!word2 %in% stop_word$word)

bigram_counts <- bigram_filt %>% count(word1, word2, sort = TRUE)

bigram_unit <- bigram_filt %>%  unite(word, word1, word2, sep = " ")

bigram_graph <- bigram_counts %>% filter(n > 2) %>%  graph_from_data_frame()

set.seed(2019)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") + geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a, end_cap = circle(.07, 'inches')) + 
  geom_node_point(color = "lightpink", size = 5) + geom_node_text(aes(label = name, vjust = 1, hjust = 1))




## Corpus
mycorpus <- corpus(Data1$text)
mytoks <- tokens(mycorpus, remove_punct = TRUE)
mytoks <- tokens_remove(mytoks, stop_word$word)
my_dfm <- dfm(mytoks, remove = stopwords("spanish"), stem = TRUE, remove_punct = TRUE, remove_numbers = TRUE)

summary(mycorpus)

kwic(mycorpus, "crema")


# Bigrams con quanteda

toks_ngram <- tokens_ngrams(mytoks, n = 2:4)
head(toks_ngram[[1]], 10)
tail(toks_ngram[[1]], 10)

toks_skip <- tokens_ngrams(mytoks, n = 2, skip = 1:2)
head(toks_skip[[1]], 50)


# Modelo
quant_dfm <- dfm_trim(my_dfm, min_termfreq = 12)
set.seed(100)
if (require(topicmodels)) {
  my_lda_fit20 <- LDA(convert(quant_dfm, to = "topicmodels"), k = 6)
  get_terms(my_lda_fit20, 8)
}

# Probabilidad de aparicion de terminos en los topics

set.seed(100)
ap_topic <- tidy(my_lda_fit20, matrix = "beta")

ap_top_terms <- ap_topic %>%  group_by(topic) %>%  top_n(10, beta) %>%  ungroup() %>% arrange(topic, -beta)
set.seed(100)
ap_top_terms %>%  mutate(term = reorder(term, beta)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) + geom_col(show.legend = FALSE) + 
  facet_wrap(~topic, scales = "free") + coord_flip()


# Log-Ratio
beta_spread <- ap_topic %>% mutate(topic = paste0("topic", topic)) %>% spread(topic, beta) %>% 
  filter(topic1>  .001 | topic2 > .001) %>%  mutate(log_ratio = log2(topic2/topic1))




# Código Juli -------------------------------------------------------------
rm(list=ls())

# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


Data1 <- read.csv2("C:/Users/juliana.forero/Desktop/Alqueria/mentions.csv") 
str(Data1)

Data1$text <- as.character(Data1$text)
Data1$Query.Name <- as.character(Data1$Query.Name)
Data1$Title <- as.character(Data1$Title)
Data1$Sentiment <- as.character(Data1$Sentiment)
Data1$Page.Type <- as.character(Data1$Page.Type)
Data1$Full.Name <- as.character(Data1$Full.Name)
Data1$Gender <- as.character(Data1$Gender)


## Corpus
docs <- Corpus(VectorSource(Data1$text))


inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
#docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove spanish common stopwords
docs <- tm_map(docs, removeWords, stopwords("spanish"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

findFreqTerms(dtm, lowfreq = 4)

findAssocs(dtm, terms = "recetas", corlimit = 0.3)


#head(d, 10)

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


# Sentiment analysis ------------------------------------------------------

library(tibble)
library(dplyr)

corpus <- SimpleCorpus(VectorSource(Data1$text))
# And lets see what we have
View(corpus)

# 1. Stripping any extra white space:
corpus <- tm_map(corpus, stripWhitespace)
# 2. Transforming everything to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))
# 3. Removing numbers 
corpus <- tm_map(corpus, removeNumbers)
# 4. Removing punctuation
corpus <- tm_map(corpus, removePunctuation)
# 5. Removing stop words
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))

#stopwords("spanish")

corpus[[1]]$content


corpus <- tm_map(corpus, stemDocument)
corpus[[1]]$content

DTM <- DocumentTermMatrix(corpus)
View(DTM)

inspect(DTM)

sums <- as.data.frame(colSums(as.matrix(DTM)))
sums <- rownames_to_column(sums) 
colnames(sums) <- c("term", "count")
sums <- arrange(sums, desc(count))
head <- sums[1:75,]

set.seed(1234)

wordcloud(words = head$term, freq = head$count, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


findFreqTerms(head, lowfreq = 4)

findAssocs(head, term = "recetas", corlimit = 0.3)

barplot(head[1:10,]$count, las = 2, names.arg = head[1:10,]$term,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")



#### Sentimientos ####

library(SentimentAnalysis)
library(syuzhet)
library(ggplot2)
library(tibble)
library(dplyr)

corpus <- SimpleCorpus(VectorSource(Data1$text))
# And lets see what we have
View(corpus)

# 1. Stripping any extra white space:
corpus <- tm_map(corpus, stripWhitespace)
# 2. Transforming everything to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))
# 3. Removing numbers 
corpus <- tm_map(corpus, removeNumbers)
# 4. Removing punctuation
corpus <- tm_map(corpus, removePunctuation)
# 5. Removing stop words
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))

#stopwords("spanish")

#corpus[[1]]$content

corpus <- tm_map(corpus, stemDocument)
#corpus[[1]]$content

DTM <- DocumentTermMatrix(corpus)

sent <- analyzeSentiment(DTM, language = "spanish")
# were going to just select the Harvard-IV dictionary results ..  
sent <- sent[,1:4]
#Organizing it as a dataframe
sent <- as.data.frame(sent)
# Now lets take a look at what these sentiment values look like. 
head(sent)


summary(sent$SentimentGI)

# Start by attaching to other data which has the company names 
final <- bind_cols(Data1, sent)
# now lets get the top 5 
final %>% group_by(Page.Type) %>%
  summarize(sent = mean(SentimentGI)) %>%
  arrange(desc(sent)) %>%
  head(n= 5)

# And now lets get the bottom 5 
final %>% group_by(Page.Type) %>%
  summarize(sent = mean(SentimentGI)) %>%
  arrange(sent) %>%
  head(n= 5)

sent2 <- get_nrc_sentiment(Data1$Page.Type)
# Let's look at the corpus as a whole again:
sent3 <- as.data.frame(colSums(sent2))
sent3 <- rownames_to_column(sent3) 
colnames(sent3) <- c("emotion", "count")
ggplot(sent3, aes(x = emotion, y = count, fill = emotion)) + 
  geom_bar(stat = "identity") + theme_minimal() + 
  theme(legend.position="none", panel.grid.major = element_blank()) + 
  labs( x = "Emotion", y = "Total Count") + ggtitle("Sentiment of Job Descriptions") + 
  theme(plot.title = element_text(hjust=0.5))


       
##### Otro intento

Data1 <- read.csv2("C:/Users/juliana.forero/Desktop/Alqueria/mentions.csv") 

Data1$text <- as.character(Data1$text)

a <- get_sentences(Data1$text)

sentiment=sentiment_by(a)

summary(sentiment$ave_sentiment)

qplot(sentiment$ave_sentiment,   geom="histogram",binwidth=0.1,main="Review Sentiment Histogram")

df$ave_sentiment=sentiment$ave_sentiment
df$sd_sentiment=sentiment$sd



##################################

library(tidyverse)
library(tidytext)
library(tm)
library(lubridate)
library(zoo)
library(scales)

Data1 <- read.csv2("C:/Users/juliana.forero/Desktop/Alqueria/mentions.csv") 

Data1$text <- as.character(Data1$text)

tema_graf <-
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#EBEBEB", colour = NA),
        legend.position = "none",
        legend.box.background = element_rect(fill = "#EBEBEB", colour = NA))


download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
              "lexico_afinn.en.es.csv")

afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()

tuits <- 
  Data1 %>%
  separate(Date, into = c("Fecha", "Hora"), sep = " ") %>%
  separate(Fecha, into = c("Año", "Mes", "Dia"), sep = "-",
           remove = FALSE) %>%
  mutate(Fecha = ymd(Fecha),
         Semana = week(Fecha) %>% as.factor(),
         text = tolower(text))

tuits_afinn <- 
  tuits %>%
  unnest_tokens(input = "text", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>% 
  rename("Género" = Gender)

# Total
tuits_afinn %>%
  count(Género)

# Únicas
tuits_afinn %>% 
  group_by(Género) %>% 
  distinct(Palabra) %>% 
  count()


map(c("Positiva", "Negativa"), function(sentimiento) {
  tuits_afinn %>%
    filter(Tipo ==  sentimiento) %>%
    group_by(Género) %>%
    count(Palabra, sort = T) %>%
    top_n(n = 10, wt = n) %>%
    ggplot() +
    aes(Palabra, n, fill = Género) +
    geom_col() +
    facet_wrap("Género", scales = "free") +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    labs(title = sentimiento) +
    tema_graf
})


tuits_afinn <-
  tuits_afinn %>%
  filter(Palabra != "no") %>% 
  filter(Género != 0)


tuits_afinn_fecha <-
  tuits_afinn %>% 
  #group_by(status_id) %>%
  mutate(Suma = mean(Puntuacion)) %>%
  group_by(Género, Fecha) %>%
  summarise(Media = mean(Puntuacion)) %>% 
  filter(Género != 0)

tuits_afinn_fecha %>%  
  ggplot() +
  aes(Fecha, Media, color = Género) +
  geom_hline(yintercept = 0, alpha = .35) +
  geom_line() +
  facet_grid(Género~.) +
  tema_graf +
  theme(legend.position = "none")


### ESTO NO:

tuits_afinn_fecha %>%
  ggplot() +
  aes(Fecha, Puntuacion, color = Género) +
  geom_smooth(method = "loess", fill = NA) +
  tema_graf


tuits_afinn %>%
  ggplot() +
  aes(Fecha, Puntuacion, color = Género) +
  geom_point(color = "#E5E5E5") + 
  geom_smooth(method = "loess", fill = NA) +
  facet_wrap(~Género) +
  tema_graf

### eSTO SI:

tuits_afinn %>%
  count(Género, Tipo) %>%
  group_by(Género) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(Género, Proporcion, fill = Tipo) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  tema_graf +
  theme(legend.position = "top")


tuits_afinn %>%
  group_by(Género, Fecha) %>%
  count(Tipo) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(Fecha, Proporcion, fill = Tipo) +
  geom_col(width = 1) +
  facet_grid(Género~.) +
  scale_y_continuous(labels = percent_format()) +
  scale_x_date(expand = c(0, 0)) +
  tema_graf +
  theme(legend.position = "top")


tuits %>%
  ggplot() +
  aes(Género, Puntuacion_tuit, fill = Género) +
  geom_boxplot() +
  tema_graf

