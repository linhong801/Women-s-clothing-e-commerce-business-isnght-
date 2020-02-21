##############################################################################
###Analyze the whole text to understand women clothing Ecommerce Indstry####### 

#import packages
# install.packages("NLP")
library(tm)
library(dplyr)
library(tidytext)
library(ggplot2)
library(stringr)
library(wordcloud2)

#read the document and transfer to a dataframe 
Ecommerce<- read.csv(file="/Users/lilinhong/Downloads/text analytics/A1/Womens Clothing E-Commerce Reviews.csv",header = TRUE)
Ecommerce$Review.Text <- as.character(Ecommerce$Review.Text)
original_review_text <-tibble(line=1:length(Ecommerce$Review.Text),text=Ecommerce$`Review.Text`)
original_review_text

#########Tokenize text and Plot a graph to find out key word ##########
original_review_text%>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE) %>%
  filter(n > 1500) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

#######Tokenize text 
token_review<- original_review_text%>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
# #################################n-grams graph
library(igraph)
library(ggraph)

count_bigrams_review <-token_review  %>%
  unnest_tokens(bigrams_separated, word, token = "ngrams", n=2)%>%
  separate(bigrams_separated, c("word1", "word2"), sep = " ")%>%
  count(word1, word2, sort=TRUE)


# n-grams graph
bigram_graph <- count_bigrams_review %>%
  filter(n>50) %>%
  graph_from_data_frame()
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)



# word sentiment 
sentiment_review <- token_review %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()
sentiment_review %>%
  group_by(sentiment) %>%
  top_n(30) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

### Wordcloud 
library(wordcloud2)
token_review %>%
  count(word) %>%
  top_n(80) %>%
  wordcloud2(fontFamily = "wqy-microhei")

library(reshape2)
library(wordcloud)

original_review_text %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  anti_join(stop_words) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 80)

###########################################################
#########Analyze data by different dataframe###############
Ecommerce<- read.csv(file="/Users/lilinhong/Downloads/text analytics/A1/Womens Clothing E-Commerce Reviews.csv",header = TRUE)

## separate by department name 
dresses <- subset(Ecommerce, Ecommerce$Department.Name == 'Dresses')
bottoms <- subset(Ecommerce, Ecommerce$Department.Name == 'Bottoms')
Tops <- subset(Ecommerce, Ecommerce$Department.Name == 'Tops')

Ecommerce_2 <- rbind(dresses, bottoms, Tops)
Ecommerce_2= Ecommerce_2 %>% select(Review.Text, Department.Name)
Ecommerce_2 <- rbind(dresses, bottoms, Tops)

dresses<- Ecommerce_2%>%
  filter(Ecommerce_2$Department.Name == "Dresses")

dresses <- tibble(line = 1:length(dresses$Review.Text), text = dresses$Review.Text)
dresses$text <- as.character(dresses$text)

token_dresses<- dresses%>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
token_dresses
######################3
bottoms<- Ecommerce_2%>%
  filter(Ecommerce_2$Department.Name == "Bottoms")

bottoms <- tibble(line = 1:length(bottoms$Review.Text), text = bottoms$Review.Text)
bottoms$text <- as.character(bottoms$text)

token_bottoms<- bottoms%>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
token_bottoms

##########################3
tops<- Ecommerce_2%>%
  filter(Ecommerce_2$Department.Name == "Tops")

tops <- tibble(line = 1:length(tops$Review.Text), text = tops$Review.Text)
tops$text <- as.character(tops$text)

token_tops<- tops%>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
token_tops

###gutenbergr
library(gutenbergr)
library(tidyr)
frequency <- bind_rows(mutate(token_tops, author="tops"),#adjust
                       mutate(token_dresses, author= "dresses"),#
                       mutate(token_bottoms, author="bottoms")#
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%  
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `tops`, `bottoms`) ##

#let's plot the correlograms:
library(scales)
ggplot(frequency, aes(x=proportion, y=`dresses`, 
                      color = abs(`dresses`- proportion)))+#
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "dresses", x=NULL)


cor.test(data=frequency[frequency$author == "tops",],
         ~proportion + `dresses`)

cor.test(data=frequency[frequency$author == "bottoms",],
         ~proportion + `dresses`)

#####################TF-IDF framework#########################

Ecommerce_2$Review.Text <- as.character(Ecommerce_2$Review.Text)
junk_word<-data_frame(
  word=c("shirt", "dress", "top", "bottom", "pants"),
  lexicon= "junk")
print(junk_word)

department<- Ecommerce_2 %>%
  unnest_tokens(word, Review.Text) %>%
  anti_join(stop_words) %>%
  anti_join(junk_word) %>%
  count(Department.Name, word, sort=TRUE) %>%
  ungroup()
department_group <- department %>%
  group_by(Department.Name) %>%
  summarize(total=sum(n))

department<-left_join(department, department_group)
department

########## ZIPF's law ################

freq_by_rank <- department  %>%
  group_by(Department.Name) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank
#let's plot ZIPF's Law
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = Department.Name)) +
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

######################The bind_tf_idf Function#########################
department <- department %>%
  bind_tf_idf(word, Department.Name, n)


department %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(Department.Name) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = Department.Name)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~Department.Name, ncol = 2, scales = "free") +
  coord_flip()

###############################3



