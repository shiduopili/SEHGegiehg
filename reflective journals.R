#---------Data import  (the text from the pdf documents have been imported using "pdftools" package from R and saved into .xlsx with two columns: text and Year); 
## chinese texts were translated to English. 

install.packages("readxl")
library(readxl)
text <-
  read_excel(
    "~/......xlsx"
  )

#---------Data pre-processing
##lemmatization
install.packages("textstem")
library(textstem)
library(stringr)
text$Text <- lemmatize_strings(text$Text)


##remove the numbers, hyperlinks, punctuations,unmeaningful words, keep English words only etc.
library(tm)
text$Text <- removeNumbers(text$Text)
text$Text <- removePunctuation(text$Text)
text$Text <- trimws(text$Text)  ##remove white space
text$Text <-
  str_replace_all(text$Text, "[^[:ascii:]]", "") ##remove special characters
text$Text <-
  str_replace_all(text$Text, " ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "") ##remove hyper links
text$Text <- tolower(text$Text)
text <- cbind(ID = c(32:1), text)

## get the tokens
library(dplyr)
install.packages("tidytext")
library(tidytext)
install.packages("textdata")
library(textdata)
tidy_text <- text %>%
  unnest_tokens(word, Text)   ##unnest_token can also remove the punctuation and convert to lower cases

## check each token against an English dictionary and remove those non-English word e.g. names,misspelled words
install.packages("qdap")
library(qdap)
library(qdapDictionaries)
is.word  <- function(x)
  x %in% GradyAugmented
tidy_text <- tidy_text[which(is.word(tidy_text$word)), ]

##remove the stop words
data("stop_words")
stop_words <-
  rbind(
    "your own stop words",
    stop_words
  )

tidy_text <- tidy_text %>%
  anti_join(stop_words)




#-----------Data analysis
word_count <- tidy_text %>%
  count(word, sort = TRUE) %>%
  top_n(50)

library(ggplot2)
###Figure 1. the top 20 words used in the journals
word_count %>% top_n(20) %>% ggplot(aes(reorder(word, n), n)) +
  geom_bar(stat = "identity") + xlab("words") + coord_flip()

###Figure 2. word cloud
##word cloud
install.packages("wordcloud")
library(wordcloud)
tidy_text %>%
  count(word, sort = TRUE) %>%
  with(
    wordcloud(
      word,
      n,
      max.words = 120,
      min.freq = 2,
      random.order = FALSE,
      rot.per = 0.35,
      colors = brewer.pal(8, "Dark2")
    )
  )

##bigram
text_bigrams <- text %>%
  unnest_tokens(bigram, Text, token = "ngrams", n = 2)

text_bigrams %>%
  count(bigram, sort = TRUE)

library(tidyr)
bigrams_separated <- text_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(word1 %in% tidy_text$word) %>%
  filter(word2 %in% tidy_text$word)
bigrams_filtered_year <- bigrams_separated %>% group_by(Year) %>%
  filter(word1 %in% tidy_text$word) %>%
  filter(word2 %in% tidy_text$word) %>%
  ungroup()
bigram_counts <- bigrams_filtered  %>% group_by(Year)  %>%
  count(word1, word2, sort = TRUE)  %>% ungroup()

library(tidyr)
bigrams_united <- bigram_counts %>%
  unite(bigram, word1, word2, sep = " ") %>%
  top_n(40)

##Figure3. top 20 adjacent words
bigrams_united  %>% top_n(20) %>%  mutate(bigram = reorder(bigram, n)) %>% ggplot(aes(bigram, n)) +
  geom_bar(stat = "identity") + xlab("adjacent words (bigrams)") + coord_flip()


###sentimental analysis (there are no neutral in nrc and bing, which may result in noun such as government as "negative",
## afinn seems to work the best)

afinn <- get_sentiments("afinn")
afinn_pos <- afinn %>% filter(value > 0)
afinn_neg <- afinn %>% filter(value < 0)

##adjust the sentiment
negation_words <-
  c("not", "no", "never", "without", "didnt", "dont", "cannot")
bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(afinn, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)
bigrams_sentiment_ID <- bigrams_separated %>%
  left_join(afinn, by = c(word2 = "word"))
##change the sentiment of those starting with negation word
bigrams_sentiment_ID <-
  bigrams_sentiment_ID %>% mutate(value = ifelse(word1 %in% negation_words, (-1) *
                                                   value, value))
textbigrams_sentiment_ID <- bigrams_sentiment_ID %>%
  group_by(ID, Year) %>%
  dplyr::summarise(sentiment = sum(value, na.rm = T)) %>%
  mutate(method = "AFINN") %>%
  ungroup()

##Figure 4. Sensitivity score of each journal
textbigrams_sentiment_ID %>%
  ggplot(aes(x = factor(ID), y = sentiment)) +
  geom_bar(stat = "identity") + theme_bw() + xlab("Journal ID") +
  ylab("Sentiment (+:positive; -:negative)")

##positive and negative words
pos_top20 <- tidy_text %>%
  inner_join(afinn_pos) %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(sentiment = "Positive")

neg_top20 <- tidy_text %>%
  inner_join(afinn_neg) %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(sentiment = "Negative")

## Figure 5. the top 20 positive and negative words
pos_top20 %>% rbind(neg_top20) %>%
  group_by(sentiment) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) +
  facet_wrap( ~ sentiment, scales = "free_y") + coord_flip()

## Figure 6. word cloud of positive and negative words
library(reshape2)
tidy_text %>%
  inner_join(get_sentiments("afinn")) %>%
  mutate(sentiment = ifelse(value > 0, "positive", "negative")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 80)


###word relationship
install.packages("widyr")
library(widyr)
word_pairs <- tidy_text %>%
  pairwise_count(word, ID, sort = TRUE)  ##the word correlation WITHIN each document

##Figure 7. word pairs
word_pairs %>%
  filter(n >= 25) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name),
                 repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void() + theme(legend.position = "none")

##Figure 8. word correlations
word_cors <- tidy_text  %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, ID, sort = TRUE)

install.packages("ggraph")
library(ggraph)
library(dplyr)
library(igraph)
word_cors %>%
  filter(correlation >= .5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = TRUE) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


#-----------comparing Y1 and Y2
## word count
word_count_Y1 <-
  tidy_text %>% filter(Year == "One") %>% count(word, sort = TRUE) %>%
  top_n(20) %>% mutate(Year = "One")
word_count_Y2 <-
  tidy_text %>% filter(Year == "Two") %>% count(word, sort = TRUE) %>%
  top_n(20) %>% mutate(Year = "Two")

p1 <- word_count_Y1 %>% mutate(word = reorder(word, n))  %>%
  ggplot(aes(word, n, fill = Year)) + geom_bar(stat = "identity", fill =
                                                 "#FF9999") + xlab("") +
  ylab("Year 1") + coord_flip()
  
p2 <- word_count_Y2 %>% mutate(word = reorder(word, n))  %>%
  ggplot(aes(word, n, fill = Year)) + geom_bar(stat = "identity", fill =
                                                 "#56B4E9") + xlab("") +
  ylab("Year 2") + coord_flip()

grid.arrange(p1, p2, ncol = 2)

##word count(bigrams)
p3 <-
  bigrams_united %>% filter(Year == "One") %>% top_n(15) %>% ggplot(aes(reorder(bigram, n), n)) +
  geom_bar(stat = "identity", fill = "#FF9999") + coord_flip() + xlab("") +
  ylab("Year 1")

p4 <-
  bigrams_united %>% filter(Year == "Two") %>% top_n(20) %>% ggplot(aes(reorder(bigram, n), n)) +
  geom_bar(stat = "identity", fill = "#56B4E9") + coord_flip() + xlab("") +
  ylab("Year 2")

library(gridExtra)
grid.arrange(p3, p4, ncol = 2)

##positive and negative words
p5 <- tidy_text %>% filter(Year == "One") %>%
  inner_join(afinn_pos) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n)) + geom_bar(stat = "identity", fill =
                                                                        "#1B9E77") +
  xlab("Positive") + ylab("Year 1") + coord_flip()

p6 <- tidy_text %>% filter(Year == "Two") %>%
  inner_join(afinn_pos) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n)) + geom_bar(stat = "identity", fill =
                                                                        "#66A61E") +
  xlab("") + ylab("Year 2") + coord_flip()

p7 <- tidy_text %>% filter(Year == "One") %>%
  inner_join(afinn_neg) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n)) + geom_bar(stat = "identity", fill =
                                                                        "#E6AB02") +
  xlab("Negative") + ylab("") +
  coord_flip()

p8 <- tidy_text %>% filter(Year == "Two") %>%
  inner_join(afinn_neg) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n)) + geom_bar(stat = "identity", fill =
                                                                        "#A6761D") +
  xlab("") + ylab("") +
  coord_flip()
grid.arrange(p5, p6, p7, p8, ncol = 2)


##word frequency
text_words <- tidy_text %>%
  count(Year, word, sort = TRUE)

total_words <- text_words %>%
  group_by(Year) %>%
  summarize(total = sum(n))
text_words <- left_join(text_words, total_words)

freq <- text_words %>% mutate(freq = n / total)

library(tidyr)

freq <- freq %>%
  select(Year, word, freq) %>%
  spread(Year, freq) %>%
  arrange(One, Two)

##figure. raw frequency
library(scales)

ggplot(freq, aes(One, Two)) +
  geom_jitter(
    alpha = 0.1,
    size = 1,
    width = 0.2,
    height = 0.2
  ) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") + xlab("Year 1") + ylab("Year 2")


##log ratio of using a certain words
word_ratios <- tidy_text %>%
  count(word, Year) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(Year, n, fill = 0) %>%
  mutate_if(is.numeric, list( ~ (. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(One / Two)) %>%
  arrange(desc(logratio))

##figure
word_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("log odds ratio (Year 1/Year 2)") +
  scale_fill_discrete(name = "", labels = c("Year 1", "Year 2"))

##sentimental difference between Y1 and Y2
textbigrams_sentiment_ID %>%
  ggplot(aes(x = factor(ID), y = sentiment, fill = Year)) +
  geom_bar(stat = "identity") + theme_bw() + xlab("Journal ID") +
  ylab("Sentiment (+:positive; -:negative)")   
  
t.test(textbigrams_sentiment_ID$sentiment ~ textbigrams_sentiment_ID$Year)
mean(textbigrams_sentiment_ID$sentiment[textbigrams_sentiment_ID$Year ==
                                          "One"])
sd(textbigrams_sentiment_ID$sentiment[textbigrams_sentiment_ID$Year == "One"])
mean(textbigrams_sentiment_ID$sentiment[textbigrams_sentiment_ID$Year ==
                                          "Two"])
sd(textbigrams_sentiment_ID$sentiment[textbigrams_sentiment_ID$Year == "Two"])

##word relationship
install.packages("widyr")
library(widyr)

##word pairs
p9 <- tidy_text %>% filter(Year == "One") %>%
  pairwise_count(word, ID, sort = TRUE) %>% mutate(n = as.integer(n)) %>%
  filter(n >= 15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "lightblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name),
                 repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void() + theme(legend.position = "none")

p10 <- tidy_text %>% filter(Year == "Two") %>%
  pairwise_count(word, ID, sort = TRUE) %>%
  filter(n >= 11) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name),
                 repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void() + theme(legend.position = "none")

grid.arrange(p9, p10, ncol = 2)

##word correlations
word_cors_Y1 <- tidy_text  %>% filter(Year == "One") %>%
  group_by(word) %>%
  filter(n() >= 15) %>%
  pairwise_cor(word, ID, sort = TRUE)

word_cors_Y2 <- tidy_text  %>% filter(Year == "Two") %>%
  group_by(word) %>%
  filter(n() >= 15) %>%
  pairwise_cor(word, ID, sort = TRUE)

install.packages("ggraph")
library(ggraph)
library(dplyr)
library(igraph)
p11 <- word_cors_Y1 %>%
  filter(correlation >= .6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = TRUE) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

p12 <- word_cors_Y2 %>%
  filter(correlation >= .6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = TRUE) +
  geom_node_point(color = "cyan4", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

grid.arrange(p11, p12, ncol = 2)

