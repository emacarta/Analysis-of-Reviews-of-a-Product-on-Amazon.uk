#LIBRERIE USATE
library('dplyr')
library('tidyr')
library('readr')
library('ggplot2')
library('tidytext')
library('tidyverse')
library('widyr')
library('ggraph')
library('igraph')
library('ggthemes')
library('tm')
library('textclean')
library('textdata')
library('topicmodels')
library('wordcloud')
library('reshape2')
library('sentimentr')
library('stringr')

#IMPORTAZIONE DATASET
nintendo_r <- read_csv("nintendo_r.csv")
View(nintendo_r)

#ASSEGNO UN NOME AL DATASET
n <- nintendo_r

#PRE-ELABORAZIONE DEI DATI------------------------------------------------------
#PULIZIA GENERALE DELLE COLONNE 
delR <- c('.0 out of 5')
delD <- c('Reviewed in the United Kingdom on')
delDM <- c(' January ',' February ',' March ',' April ',' May ',' June '
           ,' July ',' August ',' September ',' October ',' November ',' December ')

n$RATING <- mgsub(pattern = delR, replacement = ' ', n$RATING)
n$DATE <- mgsub(pattern = delD, replacement = ' ', n$DATE)
n$DATE <- mgsub(pattern = delDM, replacement = '_', n$DATE)
n$DATE <-gsub('^.*?_','_',n$DATE)
n$DATE <- mgsub(pattern = '_', replacement = '', n$DATE)

#ASSEGNO AD OGNI RECENSIONE UN ID DA 1-500
n$id = (1:500)

#CREO UN DATASET FATTO SOLO DALLE RECENSIONI E DEI TITOLI 
text <- n$TEXT
text <- as.data.frame(text)
title <- n$TITLE
title <- as.data.frame(title)

#CREO UN DATASET FATTO DAL RATING, DAL TESTO, ANNO, ED IL LORO ID 
r <- n$RATING
t <- n$TEXT
d <- n$DATE
id <- n$id
rt <- data.frame(id,r,d,t)
colnames(rt) <- c('id','RATING','DATE','TEXT')

#CREO UN DATASET FATTO DAL RATING, DAL TITOLO, ANNO ED IL LORO ID 
tt <- n$TITLE
rtt <- data.frame(id,r,d,tt)
colnames(rtt) <- c('id','RATING','DATE','TITLE')

#TOKENIZAZIONE E PULIZIA DELLE RECENSIONI   
rev_words <- rt %>%
  unnest_tokens(word, TEXT) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)

#TOKENIZAZIONE E PULIZIA DEI TITOLI DELLE RECENSIONI    
tit_words <- rtt %>%
  unnest_tokens(word, TITLE) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)

#ANALISI PRELIMINARI------------------------------------------------------------
#VISUALIZAZIONE GRAFICA DELLE STELLE
n %>%
  group_by(RATING) %>%
  summarize(stars = n_distinct(id)) %>%
  ggplot(aes(stars, RATING)) +
  geom_col() +
  labs(y = NULL)

#VISUALIZAZIONE GRAFICA del NUMERO RECENSIONI IN BASE AGLI ANNI 
dd <- data.frame(n$DATE)

reviews_for_year <- dd %>%
  count(n.DATE, sort = TRUE) 

reviews_for_year %>%
  mutate(year = reorder(n.DATE, n)) %>%
  ggplot(aes(n, year)) +
  geom_col() +
  labs(y = NULL)


#FREQUENZA----------------------------------------------------------------------
#FREQUENZA PAROLE NELLE RECENSIONE
freq <- rev_words %>%
  count(word, sort = TRUE) 
  
freq %>%
  filter(n > 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

#WORDCLOUD
rev_words %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

#FREQUENZA DELLE PAROLE DELLE RECENSIONI IN BASE ALLE STELLE 
words_by_stars <- rev_words %>%
  count(RATING, word, sort = TRUE) %>%
  ungroup()

#FREQUENZA DELLE PAROLE DELLE RECENSIONI IN BASE ALL'ANNO 
words_by_year <- rev_words %>%
  count(DATE, word, sort = TRUE) %>%
  ungroup()

#FREQUENZA DELLE PAROLE DEI TITOLI DELLE RECENSIONI IN BASE ALLE STELLE 
words_by_stars2 <- tit_words %>%
  count(RATING, word, sort = TRUE) %>%
  ungroup()

#FREQUENZA PAROLE NEI TITOLI DELLE RECENSIONI
freqT <- tit_words %>%
  count(word, sort = TRUE) 

freqT %>%
  filter(n > 4) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)



#TF-IDF-------------------------------------------------------------------------
#TESTI RECENSIONI
tf_idf_stars <- words_by_stars %>%
  bind_tf_idf(word, RATING, n) %>%
  arrange(desc(tf_idf))

tf_idf_stars %>%
  arrange(desc(tf_idf))

tf_idf_stars %>%
  group_by(RATING) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = RATING)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~RATING, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

#TITOLI DELLE RECENSIONI 
tf_idf_stars2 <- words_by_stars2 %>%
  bind_tf_idf(word, RATING, n) %>%
  arrange(desc(tf_idf))

tf_idf_stars2 %>%
  arrange(desc(tf_idf))

tf_idf_stars2 %>%
  group_by(RATING) %>%
  slice_max(tf_idf, n = 6) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = RATING)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~RATING, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

#TOPIC MODELLING----------------------------------------------------------------
#CONEVRTIAMO I NOSTRI DATI IN UNA DTM 
rev_dtm <- rev_words %>%
  unite(document, RATING, id) %>%
  count(document, word) %>%
  cast_dtm(document, word, n)

#LDA
rev_lda <- LDA(rev_dtm, k = 6, control = list(seed = 2410))

#ORDINIAMO DTM
rev_topics <- tidy(rev_lda, matrix = "beta") 

#TOP 10 TERMINI PIù COMUNI 
rev_top_terms <- rev_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

rev_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

#SENTIMENT----------------------------------------------------------------------
#ANALISI DELLE PAROLE PIU' O MENO NEGATIVE IN BASE ALLE STELLE##################
#RECENSIONI 
rating_sentiments <- words_by_stars %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(RATING) %>%
  summarize(value = sum(value * n) / sum(n))

rating_sentiments %>%
  mutate(rating = reorder(RATING, value)) %>%
  ggplot(aes(value, rating, fill = value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Average sentiment value", y = NULL)

#TITOLI 
rating_sentiments2 <- words_by_stars2 %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(RATING) %>%
  summarize(value = sum(value * n) / sum(n))

rating_sentiments2 %>%
  mutate(rating = reorder(RATING, value)) %>%
  ggplot(aes(value, rating, fill = value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Average sentiment value", y = NULL)

#ANALISI DEL SENTIMENT PAROLA PER PAROLA########################################
#RECENSIONI
contributions <- rev_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(occurences = n(),
            contribution = sum(value))

#TITOLI
contributions2 <- tit_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(occurences = n(),
            contribution = sum(value))

#PAROLE CON MAGGIOR EFFETTO SUL SENTIMENT#######################################
#RECENSIONI
contributions %>%
  slice_max(abs(contribution), n = 25) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(contribution, word, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  labs(y = NULL)

#TITOLI
contributions2 %>%
  slice_max(abs(contribution), n = 25) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(contribution, word, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  labs(y = NULL)

#PAROLE CON MAGGIOR EFFETTO SUL SENTIMENT IN OGNI SINGOLO RATING################
#RECENSIONI
top_sentiment_words <- words_by_stars %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  mutate(contribution = value * n / sum(n))

top_sentiment_words %>%
  group_by(RATING) %>%
  slice_max(abs(contribution), n = 15) %>%
  mutate(term = reorder_within(word, contribution, RATING)) %>%
  ggplot(aes(contribution, word, fill = factor(RATING))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ RATING, scales = "free") +
  scale_y_reordered()

#TITOLI
top_sentiment_words2 <- words_by_stars2 %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  mutate(contribution = value * n / sum(n))

top_sentiment_words2 %>%
  group_by(RATING) %>%
  slice_max(abs(contribution), n = 15) %>%
  mutate(term = reorder_within(word, contribution, RATING)) %>%
  ggplot(aes(contribution, word, fill = factor(RATING))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ RATING, scales = "free") +
  scale_y_reordered()

#ANALISI DEL SENTIMENT PER RECENSIONE###########################################
#SENTIMENT GENERALE SU RECENSIONI DI OGNI DIMENSIONE
#RECENSIONI
text_dtm <- text %>%
  as.matrix()
sentiment_text <- text_dtm %>%
  sentiment_by()

sentiment_text %>%
  arrange(desc(ave_sentiment))

#TITOLI
title_dtm <- title %>%
  as.matrix()
sentiment_title <- title_dtm %>%
  sentiment_by()

sentiment_title %>%
  arrange(desc(ave_sentiment))

#SENTIMENT SU RECENSIONI CHE UN NUMERO DI PAROLE ELEVATO 
sentiment_big_text <- rev_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(sentiment = mean(value),
            words = n()) %>%
  ungroup() %>%
  filter(words >= 10)

sentiment_big_text %>%      
  arrange(desc(sentiment))

#PAROLE PIU' POSITIVE EPIU' NEGATIVE USATE NELLE RECENSIONI#####################
#RECENSIONI
bing_word_counts <- rev_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Sentiment",
       y = NULL)

#TITOLO
bing_word_counts2 <- tit_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts2 %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Sentiment",
       y = NULL)


#WORDCLOUD SENTIMENT 
rev_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

#ANALISI N-GRAMMI--------------------------------------------------------------- 
rev_bigrams <- text %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

rev_bigram_counts <- rev_bigrams %>%
  count( bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- rev_bigram_counts %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#LISTA DI PAROLE INTERESSANTI DA ANALIZZARE 
negate_words <- c('not', 'no', "don't")
good_words <- c('good','fine','nice','perfect')
problems_words <- c('joystick','drift')

rev_bigram_counts %>%
  filter(word1 %in% negate_words) %>%
  count(word1, word2, wt = n, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  mutate(contribution = value * n) %>%
  group_by(word1) %>%
  slice_max(abs(contribution), n = 10) %>%
  ungroup() %>%
  mutate(word2 = reorder_within(word2, contribution, word1)) %>%
  ggplot(aes(contribution, word2, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free", nrow = 4) +
  scale_y_reordered() +
  labs(x = "Sentiment value * # of occurrences",
       y = "Word cheked")

rev_bigram_counts %>%
  filter(word1 %in% good_words) %>%
  count(word1, word2, wt = n, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  mutate(contribution = value * n) %>%
  group_by(word1) %>%
  slice_max(abs(contribution), n = 10) %>%
  ungroup() %>%
  mutate(word2 = reorder_within(word2, contribution, word1)) %>%
  ggplot(aes(contribution, word2, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free", nrow = 3) +
  scale_y_reordered() +
  labs(x = "Sentiment value * # of occurrences",
       y = "Word cheked")

rev_bigram_counts %>%
  filter(word1 %in% problems_words) %>%
  count(word1, word2, wt = n, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  mutate(contribution = value * n) %>%
  group_by(word1) %>%
  slice_max(abs(contribution), n = 10) %>%
  ungroup() %>%
  mutate(word2 = reorder_within(word2, contribution, word1)) %>%
  ggplot(aes(contribution, word2, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free", nrow = 3) +
  scale_y_reordered() +
  labs(x = "Sentiment value * # of occurrences",
       y = "Word cheked")

#VISUALIZAZIONE GRFICA RETE DI BIGRAMMI
bigram_graph <- bigrams_filtered %>%
  filter(n > 2) %>%
  graph_from_data_frame()

set.seed(1223)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()







