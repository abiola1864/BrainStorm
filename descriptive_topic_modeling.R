
# LOAD LIBRARIES
library(tidyverse)
library(tidyverse)
library(lubridate)
library(scales)
library(tidylo)
library(ggrepel)




# READ TSV FILES

jan = read_tsv("https://raw.githubusercontent.com/microsoft/BingCoronavirusQuerySet/master/data/2020/QueriesByState_2020-01-01_2020-01-31.tsv")

feb = read_tsv("https://raw.githubusercontent.com/microsoft/BingCoronavirusQuerySet/master/data/2020/QueriesByCountry_2020-02-01_2020-02-29.tsv")

mar = read_tsv("https://raw.githubusercontent.com/microsoft/BingCoronavirusQuerySet/master/data/2020/QueriesByState_2020-03-01_2020-03-31.tsv")

apr = read_tsv("https://raw.githubusercontent.com/microsoft/BingCoronavirusQuerySet/master/data/2020/QueriesByState_2020-04-01_2020-04-30.tsv")

may = read_tsv("https://raw.githubusercontent.com/microsoft/BingCoronavirusQuerySet/master/data/2020/QueriesByState_2020-05-01_2020-05-31.tsv")

jun = read_tsv("https://raw.githubusercontent.com/microsoft/BingCoronavirusQuerySet/master/data/2020/QueriesByState_2020-06-01_2020-06-30.tsv")

jul = read_tsv("https://raw.githubusercontent.com/microsoft/BingCoronavirusQuerySet/master/data/2020/QueriesByState_2020-07-01_2020-07-31.tsv")

aug = read_tsv("https://raw.githubusercontent.com/microsoft/BingCoronavirusQuerySet/master/data/2020/QueriesByState_2020-08-01_2020-08-31.tsv")

sep = read_tsv("https://raw.githubusercontent.com/microsoft/BingCoronavirusQuerySet/master/data/2020/QueriesByState_2020-09-01_2020-09-30.tsv")

oct = read_tsv("https://raw.githubusercontent.com/microsoft/BingCoronavirusQuerySet/master/data/2020/QueriesByState_2020-10-01_2020-10-31.tsv")

nov = read_tsv("https://raw.githubusercontent.com/microsoft/BingCoronavirusQuerySet/master/data/2020/QueriesByState_2020-11-01_2020-11-30.tsv")

dec = read_tsv("https://raw.githubusercontent.com/microsoft/BingCoronavirusQuerySet/master/data/2020/QueriesByState_2020-12-01_2020-12-31.tsv")

jan21= read_tsv("https://raw.githubusercontent.com/microsoft/BingCoronavirusQuerySet/master/data/2021/QueriesByState_2021-01-01_2021-01-31.tsv")




# JOIN DATA FRAMES
df_compile <- rbind(jan, mar, apr,may, jun, jul, aug, sep, oct, nov, dec, jan21)

# JOIN DATA FRAMES
df_compile <- rbind(mar, apr)



df_compile1  <- df_compile %>% 
  filter(Country == 'United States') %>% 
  mutate(Month = floor_date(Date, "month"))


df_compile2<-df_compile1 %>% 
  group_by(Month) %>% 
  distinct(Query) %>% 
  count() %>% 
  ungroup() %>%   
  ggplot(aes(Month, n)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = comma_format()) +
  theme_minimal() +
  labs(x = NULL, y = NULL,
       title = "Count of unique COVID-19 search queries per month",
       caption = "by: @abiola1864\nSource: Bing Coronavirus Query Set")

df_compile2
#string lengths of queries

df_compile1  %>% 
  distinct(Query, IsImplicitIntent) %>% 
  mutate(qlength = str_count(Query, '\\w+')) 


k<-df_compile1  %>% 
  distinct(Query, IsImplicitIntent) %>% 
  mutate(qlength = str_count(Query, '\\w+')) %>% 
  filter(qlength>3) %>% 
  unique()
k




covid_logodds <- df_compile1  %>% 
    group_by(Month, IsImplicitIntent) %>% 
   count(Query, sort = TRUE) %>% 
  ungroup() %>% 
  bind_log_odds(Month, Query, n) %>% 
  arrange(-log_odds_weighted)
covid_logodds

c1<-covid_logodds %>% 
  group_by(Month) %>% 
  top_n(10, log_odds_weighted) %>% 
  ungroup() %>% 
  mutate(Month = factor(Month),
         Query = fct_reorder(Query, log_odds_weighted)) %>% 
  ggplot(aes(Query, log_odds_weighted, fill = IsImplicitIntent)) +
  geom_col(alpha = 0.7) +
  coord_flip() +
  facet_wrap(~Month, scales = 'free_y') +
  scale_fill_manual(values = c("#D55E00", "#0072B2")) +
  theme_bw() +
  theme(legend.position = 'top') +
  labs(title = 'Top COVID-19 search queries by month based on weighted log odds',
       caption = "by: @eeysirhc\nSource: Bing Coronavirus Query Set",
       x = NULL, y = "Weighted Log Odds") 
c1


c2<-covid_logodds %>% 
  filter(Month == '2020-04-01' | Month == '2021-01-01') %>% 
  group_by(Month) %>% 
  top_n(30, n) %>% 
  ungroup() %>% 
  ggplot(aes(n, log_odds_weighted, label = Query, color = IsImplicitIntent)) +
  geom_point() +
  geom_text_repel() +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(values = c("#D55E00", "#0072B2")) +
  facet_grid(Month ~ .) +
  expand_limits(x = 0) +
  theme_bw() +
  theme(legend.position = 'top') +
  labs(title = 'Top COVID-19 search queries based on weighted log odds',
       caption = "by: @abiola1864\nSource: Bing Coronavirus Query Set",
       x = "Frequency", y = "Weighted Log Odds")

c2



library(tidytext)
library(tm)
library(text2vec)

?stopwords

covid_words <- df_compile1  %>% 
  select(Query) %>% 
  unnest_tokens(word, Query) %>% 
  filter(!str_detect(word, "[^[:alpha:]]")) %>%
  anti_join(stop_words)


tokens <- list(covid_words$word)

it <- itoken(tokens, progressbar = TRUE)

?prune_vocabulary
vocab <- create_vocabulary(it)
vocab <- prune_vocabulary(vocab, term_count_min = 150)

vectorizer <- vocab_vectorizer(vocab)





tcm <- create_tcm(it, vectorizer, skip_grams_window = 3)

glove <- GlobalVectors$new(rank = 150, x_max = 20)

wv_main <- glove$fit_transform(tcm, n_iter = 5000, convergence_tol = 0.0001)
wv_context <- glove$components
word_vectors <- wv_main + t(wv_context)





# FUNCTION TO RETRIEVE TOP 10 MOST SIMILAR WORDS WITH COSINE > 0.50
find_similar_words <- function(word, n = 10) {
  similarities <- word_vectors[word, , drop = FALSE] %>%
    sim2(word_vectors, y = ., method = "cosine")
  
  similarities[,1] %>% sort(decreasing = TRUE) %>% 
    head(n) %>% 
    bind_rows() %>% 
    gather(word, cosine) %>% 
    filter(cosine > 0.50) 
}

stimulus <- find_similar_words("stimulus")
age <- find_similar_words("twitter")
age 






###check


## You need to use VectorSource before using Corpus
library(tm)
myCorpus <- Corpus(VectorSource(df_compile1$Query))
tdm <- TermDocumentMatrix(myCorpus)
class(tdm)

#method2
require(quanteda)
# Your sample data 
# Important to make sure the sentence variable is not converted to type factor
newcorpus <- corpus(df_compile1, text_field = "Query") # you have to tell it the name of the text field
# lots of options to dfm read the help pages
newdfm <- dfm(newcorpus, remove_punct = TRUE, remove = stopwords("english"), stem = TRUE)
class(newdfm)


#fit the model
library(topicmodels)

k <- 3 #number of topics
seed = 1234 #necessary for reproducibility
#fit the model passing the parameters discussed above
#you could have more control parameters but will just use seed here
lda <- topicmodels::LDA(newdfm, k = k, method = "GIBBS", control = list(seed = seed))


tidy(lda, matrix = "beta") %>% filter(term == "mask")




#define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")

#customize ggplot2's default theme settings
#this tutorial doesn't actually pass any parameters, but you may use it again in future tutorials so it's nice to have the options
theme_lyrics <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none")
{
  theme(plot.title = element_text(hjust = 0.5), #center the title
        axis.ticks = aticks, #set axis ticks to on or off
        panel.grid.minor = pgminor, #turn on or off the minor grid lines
        legend.title = lt, #turn on or off the legend title
        legend.position = lp) #turn on or off the legend
}

#customize the text tables for consistency using HTML formatting
my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}

word_chart <- function(data, input, title) {
  data %>%
    #set y = 1 to just plot one variable and use word as the label
    ggplot(aes(as.factor(row), 1, label = input, fill = factor(topic) )) +
    #you want the words, not the points
    geom_point(color = "transparent") +
    #make sure the labels don't overlap
    geom_label_repel(nudge_x = .2,  
                     direction = "y",
                     box.padding = 0.1,
                     segment.color = "transparent",
                     size = 3) +
    facet_grid(~topic) +
    theme_lyrics() +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
          #axis.title.x = element_text(size = 9),
          panel.grid = element_blank(), panel.background = element_blank(),
          panel.border = element_rect("lightgray", fill = NA),
          strip.text.x = element_text(size = 9)) +
    labs(x = NULL, y = NULL, title = title) +
    #xlab(NULL) + ylab(NULL) +
    #ggtitle(title) +
    coord_flip()
}




num_words <- 10 #number of words to visualize

#create function that accepts the lda model and num word to display
top_terms_per_topic <- function(lda_model, num_words) {
  
  #tidy LDA object to get word, topic, and probability (beta)
  topics_tidy <- tidy(lda_model, matrix = "beta")
  
  
  top_terms <- topics_tidy %>%
    group_by(topic) %>%
    arrange(topic, desc(beta)) %>%
    #get the top num_words PER topic
    slice(seq_len(num_words)) %>%
    arrange(topic, beta) %>%
    #row is required for the word_chart() function
    mutate(row = row_number()) %>%
    ungroup() %>%
    #add the word Topic to the topic labels
    mutate(topic = paste("Topic", topic, sep = " "))
  #create a title to pass to word_chart
  title <- paste("LDA Top Terms for", k, "Topics")
  #call the word_chart function you built in prep work
  word_chart(top_terms, top_terms$term, title)
}
#call the function you just built!
top_terms_per_topic(lda, num_words)

