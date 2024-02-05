## Lab 2: Text analysis

# So far, we covered basics for text analysis. 
# Let's practice what we learned from the new data set. We will use the set of abstracts from sociology journals indexed in KCI (Korea Citation Index). 
# However, feel free to use your own data if you want to practice. 

# Let's call the data and see how it looks like. 
load("lab2_practice_data.Rdata")
head(abstract) # They don't have column names, so let's put names for them

colnames(abstract) <- c("journalID", "pubYear", "text")

# Tidy up the data!
#install.packages("rvest")
#library(rvest)
#install.packages("dplyr")
#library(dplyr)
tidy_abstract <- abstract %>% unnest_tokens(word, text)

head(tidy_abstract) 

#### Q1: Call top 10 words that are most common in this corpus.
head(tidy_abstract,10)

#### Remove stop words ####
#install.packages("tidytext")
#library("tidytext")
data("stop_words")

#### Q2-1: Now delete stop words from the review
tidy_abstract <- tidy_abstract %>% anti_join(stop_words)

#### Q2-2: Again, call top 100 words that are most common in this corpus.
head(tidy_abstract,100)

#### Remove punctuation ####
# Ah! tidytext already did the job for us. 

#### Q3: Remove numbers 
# [CODE BY YOUR SELF]
tidy_abstract <- tidy_abstract[-grep("\\b\\d+\\b", tidy_abstract$word),]

#### Standardize it to lower case ####
# Ah! tidytext again already did the job for us. 

#### Q4: Remove white spaces 
tidy_abstract$word <- gsub("Expand   ", "", tidy_abstract$word)

#### Stemming 
# Check how 'Korea' is differently shown in the word list
table(tidy_abstract$word[grepl("korean", tidy_abstract$word)])

#### Q5-1: Now let's stem it. 
#library(SnowballC) # a package helps stemming for english
tidy_abstract <- tidy_abstract %>% 
  mutate_at("word", ~wordStem(., language="en"))

#### Q5-2 Again, check how 'Korea' is differently shown in the word list and explain what has been changed. 
table(tidy_abstract$word[grepl("korean", tidy_abstract$word)])


# entries "koreans" (172 entries) and "korean's" (8 entries) has changed from 
# perviously. Since their stem is "korean", additional 180 entries was added to
# the count of word of "korean".
# The rest of the entries: "inkorean", "koreanchildren", "koreannuresri", "koreansocieti",
# "of'korean", "ofkorean" are just the mistakes when writing, forgetting to write 
# between them


#### Q6: Draw a bar plot showing the frequency of words
# install.packages("ggplot2")
library(ggplot2)
tidy_abstract %>%
  count(word, sort=TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(., aes(n, word)) +
  geom_col() 

#### NOW THE CLEANING IS DONE! ####
#### Topic of opinion mining or sentiment analysis ####
#install.packages("textdata")
#library(textdata)

#### Q7: Using 'afinn', calculate average 'feeling' by variable 'pubYear'.
# Show how the data looks like by just typing the name of your data set. 
#get_sentiments("afinn") # negative for negative values
 
tidy_abstract_afinn <- tidy_abstract %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(pubYear) %>%
  summarise(mean_feeling = mean(value)) 

summary(tidy_abstract_afinn$mean_feeling)

# Draw a bar plot with the data
ggplot(tidy_abstract_afinn, aes(x=pubYear, y=mean_feeling)) +
  geom_bar(stat="identity")


