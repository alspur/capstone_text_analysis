library(tidyverse)
library(stringr)
library(pdftools)
library(tidytext)
library(ggraph)
library(igraph)

# create empty df
bigram_df <- tibble(word1 = character(), word2 = character(), 
                    n = numeric(), report_num = numeric())

# loop through each capstone pdf
for(k in 1:38){
  
  # parse text from capstone pdf
  report_text <- pdf_text(str_c("reports/capstone", k,".pdf"))
  
  # unnest text into bigrams
  report_bigrams <- tibble(chapter = report_text) %>%
    unnest_tokens(bigram, chapter,token = "ngrams", n = 2)
  
  # split bigrams into separate columns
  bigrams_sep <- report_bigrams %>% 
    separate(bigram, c("word1", "word2"), sep = " ")
  
  # filter out stopwords, numbers, dupes; add count and report number
  bigrams_filtered <- bigrams_sep %>% 
    filter(!word1 %in% stop_words$word) %>% 
    filter(!word2 %in% stop_words$word) %>% 
    filter(!str_detect(word1, "[0-9]")) %>% 
    filter(!str_detect(word2, "[0-9]")) %>% 
    filter(word1 != word2) %>%
    count(word1, word2, sort = TRUE) %>%
    mutate(report_num = k)
  
  # add to df
  bigram_df <- bind_rows(bigram_df, bigrams_filtered)
  
}

# prep data for graphing; only count bigrams w/ n > 20
bigram_graph <- bigram_df %>% 
  filter(n > 20) %>% 
  graph_from_data_frame()

# set arrow type
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

set.seed(23)

# plot bigram graph
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.03, 'inches')) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
