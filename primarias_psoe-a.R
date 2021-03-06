
#Load libraries.
library(tidyverse)
library(tm)
library(tidytext)
library(pdftools)
library(here)
library(ggthemes)

#Load fonts.
windowsFonts("Bahnschrift" = windowsFont("Bahnschrift"))

#Scrape data from PDFs.
diaz <- pdf_text("diaz.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "D�az")
espadas <- pdf_text("espadas.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "Espadas")
hierro <- pdf_text("hierro.pdf") %>% read_lines() %>% as.data.frame() %>% rename("word" = 1) %>% unnest_tokens(word, word) %>% mutate(party = "Hierro")

full_parties <- mget(ls()) %>% bind_rows() 


#Remove custom stopwords.

#stopwords <- c()

#Remove stopwords from corpus.
parties_wo_stopwords<- full_parties %>% anti_join(as_tibble(stopwords("spanish")), by = c("word" = "value"))


#Create new dataframe with word counts.

count_words <- parties_wo_stopwords %>% count(party, word, sort = TRUE)

total_words <- count_words %>% group_by(party) %>% summarise(total = sum(n))

words_parties <- left_join(count_words, total_words)


#Remove custom stopwords.

stopwords <- c("caso", "tan", "justamente", "�nico", "cara", "aut�noma", "expresado", "puesto",
               "aut�nomas", "primero", "tipo", "cuanto", "peso", "luis", 
               "efectivamente", "necesidad", "necesidades", "claramente",
               "forma", "debemos", "sentido", "decir", "necesitamos",
               "base", "situaci�n", "fundamental", 
               "rincones", "despu�s", "bastante", "congreso",
               "centros")

parties_tf_idf <- words_parties %>%
  filter(n > 2 & !word %in% stopwords) %>% 
  bind_tf_idf(word, party, n) %>% 
  mutate(party = factor(party, levels = c("D�az", "Espadas", "Hierro")))


order_color <- c("mediumvioletred", "seagreen3", "tan4")

#Plot.

p <- parties_tf_idf %>% 
  group_by(party)%>%
  slice_max(tf_idf, n = 9, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, reorder_within(str_to_title(word), tf_idf, party), fill = party)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(~party, ncol = 3, scales = "free") +
  labs(title = "�Cu�les son las palabras m�s diferenciales de los candidatos en las primarias del PSOE-A?", 
       subtitle = "Palabras m�s frecuentes durante el debate en comparaci�n con su frecuencia en el del resto (tf-idf)", 
       y = NULL, x = NULL, caption = "Fuente: https://www.youtube.com/watch?v=-VpUc0SKgl4 | @dataR_amateur") +
  theme_test(base_family = "Bahnschrift") +
  theme(strip.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(color = "black", size = 10),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = order_color)
p

p+ggsave("primariaspsoeA.png", width = 13, height = 8.5, dpi = 500)