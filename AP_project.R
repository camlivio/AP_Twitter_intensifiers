library(rtweet)
library(tidytext)
library(tidyverse)
library(skimr)
library(dplyr)
library(ggplot2)
library(readr)

##rtweet is missing -- install it again
install.packages("rtweet")
library(rtweet)

##your info
token <- create_token(
  app= "BP project", 
  consumer_key = "XXXXXXXXXXXXXXXX",
  consumer_secret = "XXXXXXXXXXXXXXX", 
  access_token = "XXXXXXXXXXXXXXXXXXXX", 
  access_secret = "XXXXXXXXXXXXXXXXXXXXXX")
token

geocode_luanda <- "-8.85853,13.23543,250mi"
n = 50000
search_string <- 'bom OR boa OR feliz OR fixe OR cool OR especial OR longo OR triste OR grande OR longe'

ang <- search_tweets(
  search_string, 
  n = n, 
  include_rts = FALSE, lang = "pt",
  geocode = geocode_luanda,
  retryonratelimit = TRUE,
  type = "mixed"
)

nrow(b_ang)
# add location info so we don't lose it when combining everything together
b_ang$our_location <- "Angola"

####
new_ang <- read_csv("~/Desktop/Lusitanistas_Leipzig/new_ang.csv")

###great -- no duplicates 
duplicated(new_ang)
sum(duplicated(new_ang))

nrow(new_ang)
ncol(new_ang)

### how many users compose the sample?
new_ang %>%  
  count(screen_name)
#### 1,149 different users 


### cleaning and tokezining 
tweets_tokenized <- new_ang %>%
  unnest_tokens(word, text)
tweets_tokenized %>%
  head()

####remove stopwords 
tweets_clean <- tweets_tokenized %>% 
  anti_join(get_stopwords(language= "pt"))

### remove stop words & other noisy terms
tokens_to_remove <- c("https", "t.co", "amp", "et")

tweets_cleaned <- tweets_clean %>% 
  filter(!(word %in% tokens_to_remove))

### n-grams
tweets_bigrams <- new_ang %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
tweets_bigrams

tweets_bigrams %>%
  count(bigram, sort = TRUE)

#### now trigrams 
tweets_tri <- new_ang %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)
tweets_tri


#### not really very useful ://

bigrams_separated <- tweets_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

#### now for trigrams 
trigrams_separated <- tweets_tri %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word)

trigram_counts <- trigrams_filtered %>% 
  count(word1, word2, word3, sort = TRUE)

trigram_counts

####### for bigrams
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

###the code below is more useful cause we can see the words that the intensifiers modify the most 
bigrams_filtered %>%
  filter(word1 == "bwe") %>%
  count(word2, sort = TRUE)

bigrams_filtered

### now checking trigrams bué de X 
trigrams_separated %>%
  filter(word1 == "bué", word2 == "da") %>%
  count(word1, word2, word3, sort = TRUE)

###not plot the sequence bué de X
trigrams_separated %>%
  filter(word1 == "bue", word2 == "de") %>%
  count(word3, sort = TRUE) %>% 
  ggplot(aes(x = fct_reorder(word3, n),  y= n)) +
  geom_col() + 
  ggtitle("Words that appear in the sequence *bué de*")


library(forcats)

inten_bue <- bigrams_filtered %>%
  filter(word1 == "bue") %>%
  count(word2, sort = TRUE) %>% 
  ggplot(aes(x = fct_reorder(word2, n),  y= n)) +
  geom_col() + 
  ggtitle("Words that appear after *bué*")

i_bue <- inten_bue + coord_flip()
i_bue

adv_bue <- bigrams_filtered %>%
  filter(word2 == "bué") %>%
  count(word1, sort = TRUE) %>% 
  ggplot(aes(x = fct_reorder(word1, n),  y= n)) +
  geom_col() + 
  ggtitle("Words that appear before *bué*")

a_bue <- adv_bue + coord_flip()
a_bue
library(ggpubr)
ggarrange(a_bue, i_bue,
          ncol = 2, nrow = 1)

################################################
###checking general characteristics of the data
################################################

new_ang %>%
  ggplot(aes(location)) +
  geom_bar() + coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Users' location")

new_ang %>%
  ggplot(aes(source)) +
  geom_bar() + coord_flip() +
  labs(x = "Count",
       y = "Date",
       title = "Equipment/network used to tweet")

new_ang %>%
  ggplot(aes(display_text_width)) +
  geom_bar() + coord_flip() +
  labs(x = "count",
       y = "# of words",
       title = "how long are the tweets?")

###tagging################
### Camila: the tagged file is saved as x.rds file in the Lusitanistas_Leipzig folder in your laptop
##########################
library(udpipe)
library(tidyverse)
dl <- udpipe_download_model(language = "portuguese")
str(dl)
udmodel_port <- udpipe_load_model(file = "portuguese-bosque-ud-2.5-191206.udpipe")
x <- udpipe_annotate(udmodel_port, x = b_ang4$text)
x <- as.data.frame(x)
str(x)
saveRDS(x, file = "x.RDS") 

#### loading the annotated data
x <- readRDS("~/Desktop/Lusitanistas_Leipzig/x.rds")

library(lattice)
library(udpipe)
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")

adj <- stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "purple", 
         main = "Most occurring adjectives", xlab = "Raw count")


stats <- subset(x, upos %in% c("ADV")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 35), col = "purple", 
         main = "advérbios mais frequentes - Twitter dataset", xlab = "count")



rake <- stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                               relevant = x$upos %in% c("ADV", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "purple", 
         main = "Co-occurrence of Intensifier + Adjective identified by RAKE", 
         xlab = "Rake")


## Using Pointwise Mutual Information Collocations
x$word <- tolower(x$token)
stats <- keywords_collocation(x = x, term = "word", group = "doc_id")
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ pmi, data = head(subset(stats, freq > 5), 20), col = "purple", 
         main = "Keywords identified by PMI Collocation", 
         xlab = "PMI (Pointwise Mutual Information)")

## Using a sequence of POS tags (noun phrases / verb phrases)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "M*A", 
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
ggplot(stats, col = "purple", 
       main = "mais frequentes adv+adj", xlab = "Frequency")

stats

cooc <- cooccurrence(x = subset(x, upos %in% c("ADV", "ADJ")), 
                     term = "lemma", 
                     group = c("doc_id", "paragraph_id", "sentence_id"))
head(cooc)

library(igraph)
library(ggraph)
library(ggplot2)
wordnetwork <- head(cooc, 50)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Cooccurrences within sentence", subtitle = "Adverbs & Adjective")

cooc <- cooccurrence(x$lemma, relevant = x$upos %in% c("ADV", "ADJ"), skipgram = 1)
head(cooc)

wordnetwork <- head(cooc, 40)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "lightblue") +
  geom_node_text(aes(label = name), col = "purple", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  labs(title = "Words following one another", subtitle = "Adverbs & Adjective")

x$id <- unique_identifier(x, fields = c("sentence_id", "doc_id"))
dtm <- subset(x, upos %in% c("ADV", "ADJ"))
dtm <- document_term_frequencies(dtm, document = "id", term = "lemma")
dtm <- document_term_matrix(dtm)
dtm <- dtm_remove_lowfreq(dtm, minfreq = 5)
termcorrelations <- dtm_cor(dtm)
y <- as_cooccurrence(termcorrelations)
y <- subset(y, term1 < term2 & abs(cooc) > 0.2)
y <- y[order(abs(y$cooc), decreasing = TRUE), ]
head(y, 50)
wordnetwork2 <- head(y, 40)
wordnetwork2 <- graph_from_data_frame(wordnetwork2)
ggraph(wordnetwork2, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "lightblue") +
  geom_node_text(aes(label = name), col = "purple", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  labs(title = "Words following one another", subtitle = "Adverbs & Adjective")

######################
######Looking manually 
######################

library(tidyverse)
library(stringr)
library(tm)
x %>%
  group_by(dep_rel) %>%
  count() 

####using quanteda
library(quanteda)
library(readtext)
library(quanteda.corpora)
library(lubridate)
library(tidyverse)
library(tidytext)


toks_ang <- tokens(new_ang$text)
View(toks_ang)
as.character(toks_ang)
##### manually looking for adjectives
kw_multiword_fixe <- kwic(toks_ang, pattern = phrase(c('fixe')))
head(kw_multiword_fixe, 20)

kw_multiword_bom <- kwic(toks_ang, pattern = phrase(c('bom')))
head(kw_multiword_bom, 30)

kw_multiword_feliz <- kwic(toks_ang, pattern = phrase(c('feliz')))
head(kw_multiword_feliz, 50)

kw_multiword_triste <- kwic(toks_ang, pattern = phrase(c('triste')))
head(kw_multiword_triste, 50)

kw_multiword_especial <- kwic(toks_ang, pattern = phrase(c('especial')))
head(kw_multiword_especial, 50)

kw_multiword_gostoso <- kwic(toks_ang, pattern = phrase(c('gostoso')))
head(kw_multiword_gostoso, 50)

###nada de interessante aqui
kw_multiword_longo <- kwic(toks_ang, pattern = phrase(c('longo')))
head(kw_multiword_longo, 50)

###aqui também não
kw_multiword_geral <- kwic(toks_ang, pattern = phrase(c('geral')))
head(kw_multiword_geral, 50)

kw_multiword_cool <- kwic(toks_ang, pattern = phrase(c('cool')))
head(kw_multiword_cool, 100)

kw_multiword_foda <- kwic(toks_ang, pattern = phrase(c('foda')))
head(kw_multiword_foda, 100)

kw_multiword_lindo <- kwic(toks_ang, pattern = phrase(c('lindo')))
head(kw_multiword_lindo, 100)

kw_multiword_puta <- kwic(toks_ang, pattern = phrase(c('puta')))
head(kw_multiword_puta, 100)

####uma olhada nos intensificadores themselves
kw_multiword_super <- kwic(toks_ang, pattern = phrase(c('super')))
head(kw_multiword_super, 100)

kw_multiword_bem <- kwic(toks_ang, pattern = phrase(c('bem')))
head(kw_multiword_bem, 500)

kw_multiword_bue <- kwic(toks_ang, pattern = phrase(c('bue')))
head(kw_multiword_bue, 500)
###29 matches

kw_multiword_bué <- kwic(toks_ang, pattern = phrase(c('bué')))
head(kw_multiword_bué, 500)
###42

kw_multiword_mó <- kwic(toks_ang, pattern = phrase(c('mó')))
head(kw_multiword_mó, 500)

#####Com regex aqui:
kw_multiword_issimo <- kwic(toks_ang, pattern = phrase('.issim?'), valuetype = c("regex"))
head(kw_multiword_issimo, 30)


isim <- kwic(toks, phrase(".ísim?"),valuetype = c("regex"))
head(isim, 10)