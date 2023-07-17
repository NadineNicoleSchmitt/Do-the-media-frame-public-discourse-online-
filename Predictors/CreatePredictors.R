# Change working directory 
setwd("C:/Users/nadin/OneDrive/Dokumente/Dissertation")

# Clear the environment by removing any defined objects/functions
rm(list = ls())

library(ggplot2)
library(gender)
library(tidyverse)
library(stringi)
library(quanteda)
library(quanteda.textstats)


#load datafiles
load("Final/articles.Rdata")
#load("Final/HB_comment.Rdata")
#load("Final/Zeit_comment.Rdata")
#load("Final/Welt_comment.Rdata")
load("Final/comments.Rdata")

#predictors: date, lengthTitle, lengthArticle, typetokenratio article,  genderOfAuthor, ReadabilityArticle, topicArticle,
#SentimentsTitle, SentimentsArticle, 
#output: numberComments, sentimentComments

articlesRich <- articles

#Date

hb <- filter(articles, articles$outlet == "HB")
zeit <- filter(articles, articles$outlet == "Zeit")
welt <- filter(articles, articles$outlet == "Welt")


ggplot(hb, aes(x=date, y=numberComments)) + 
  geom_point()+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()


ggplot(zeit, aes(x=date, y=numberComments)) + 
  geom_point()+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(welt, aes(x=date, y=numberComments)) + 
  geom_point()+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()


#gender of author

#extract first name
#for simplicity we extract the first name of the first author when an article has multiple authors
names <- articles$author

names <- gsub("Dr.", "",names) #remove Dr.
names <- gsub("med.", "", names)#remove Dr.med
names <- gsub("Eine Rezension von ", "", names)
names <- gsub("Prof.   h. c. ", "", names)
names <- gsub("Prof. ", "", names)
names <- gsub("Professor. ", "", names)
names <- gsub("Protokoll: ", "", names)
names <- gsub("Quiz: ", "", names)
names <- gsub("Text: ", "", names)
names <- str_trim(names)

firstName <- sapply(strsplit(names, " "), `[`, 1)
save(firstName, file="firstName.Rdata")

#stri_isempty(c("Hi", ""))
#test <- stri_isempty(firstName)
#unique(test)

#test gender
#testing = c("unknown", 'Alice', 'Alice', 'Bob', 'Charles', 'Jürgen', 'Andreas', "Miasaus")
#gender_results <- gender(testing) # Returns a data frame

## A tibble: 6 × 6
#name    proportion_male proportion_female gender year_min year_max
#<chr>             <dbl>             <dbl> <chr>     <dbl>    <dbl>
# 1 Alice            0.0034            0.997  female     1932     2012
#2 Alice            0.0034            0.997  female     1932     2012
#3 Andreas          0.992             0.008  male       1932     2012
#4 Bob              0.997             0.0031 male       1932     2012
#5 Charles          0.995             0.0051 male       1932     2012
#6 unknown          0.473             0.527  female     1932     2012

#unknown gives gender female
#names like Jürgen, Miasaus not included

#gender_df <- gender_results %>% 
  #select(name, gender) %>%
  #distinct()

# A tibble: 5 × 2
#name    gender
#<chr>   <chr> 
#1 Alice   female
#2 Andreas male  
#3 Bob     male  
#4 Charles male  
#5 unknown female

#gender_df <- filter(gender_df, gender_df$name != "unknown") #remove unknown

# A tibble: 4 × 2
#name    gender
#<chr>   <chr> 
#1 Alice   female
#2 Andreas male  
#3 Bob     male  
#4 Charles male 

#test_df <- data.frame("name" = testing)

#name
#1 unknown
#2   Alice
#3   Alice
#4     Bob
#5 Charles
#6  Jürgen
#7 Andreas
#8 Miasaus

#test_df<- merge(test_df, gender_df, by = "name", all.x = TRUE)
#name gender
#1   Alice female
#2   Alice female
#3 Andreas   male
#4     Bob   male
#5 Charles   male
#6  Jürgen   <NA>
#7 Miasaus   <NA>
#8 unknown   <NA>

#genderFirstName <- gender(firstName) # Returns a data frame
save(genderFirstName, file= "genderFirstName.Rdata")
load("genderFirstName.Rdata")


gender_df <- genderFirstName %>% 
  select(name, gender) %>%
  rename(firstNameAuthor=name, genderAuthor = gender)%>% 
  distinct()


gender_df <- filter(gender_df, gender_df$firstNameAuthor != "unknown") #remove unknown

#unknownNames <- filter(articlesRich, is.na(articlesRich$genderAuthor))
#write.csv(unknownNames, "unknownNames.csv")
unknownNames <- read_xlsx("unknownNames.xlsx") #manually labeled gender

gender_df <- rbind(gender_df, unknownNames)

#add firstName to articles
articlesRich$firstNameAuthor <- firstName

articlesRich<- merge(articlesRich, gender_df, by = "firstNameAuthor", all.x = TRUE)

articlesRich <- articlesRich[, c(2,3,4,5,6,7,8,9,10,1,11)]

#set NAs to unknown
articlesRich$genderAuthor[is.na(articlesRich$genderAuthor)] <- "unknown"

#set them as factor
articlesRich$genderAuthor <- as.factor(articlesRich$genderAuthor)

###########################################################################


#TopicArticle
load("topicsArticle.Rdata")

articlesRich <- merge(articlesRich, topicsArticle, by= "Id", all.x=TRUE )

articlesRich$TopicArticle <- as.factor(articlesRich$TopicArticle)

ggplot(articles) + 
  geom_bar(aes(x=topics, fill = as.factor(outlet)),stat="count", position="dodge") + 
  #scale_fill_manual(values = c("blue", "pink")) + 
  ylab("Count article associted with topic") 

ggplot(hb) + 
  geom_point(aes(x=topics, y = numberComments))

ggplot(zeit) + 
  geom_point(aes(x=topics, y = numberComments))
ggplot(welt) + 
  geom_point(aes(x=topics, y = numberComments))



###############################################################################################
#length of title (ntoken)
corpus_title <- articlesRich %>%
  corpus(text_field = "title")

#get ntokens
ntokens <- ntoken(corpus_title)

articlesRich$numberTokensTitle <- ntokens

# extract summary statistics of corpus
#ntokens_df <- summary(corpus_title, n = ndoc(corpus_title))

#number types/ number tokens
#typeToTokenRatio <- ntokens_df$Types/ntokens_df$Tokens

########################################################################################
#length of article (ntoken)
corpus_articles <- articlesRich %>%
  corpus(text_field = "article")

#get ntokens
ntokens <- ntoken(corpus_articles)

articlesRich$numberTokensArticle <- ntokens

# extract summary statistics of corpus
ntokens_df <- summary(corpus_articles, n = ndoc(corpus_articles))

#number types/ number tokens
typeToTokenRatio <- ntokens_df$Types/ntokens_df$Tokens

articlesRich$typeToTokenRatioArticle <- typeToTokenRatio

###########################################################################################
#readabilty score

readability <- textstat_readability(corpus_articles, measure ="Flesch")

articlesRich$readabilityArticle <- readability$Flesch



###########################################################################################

#sentiment
#score = (sim * tdf)/sum(tdf)

#example
colors_df <- data.frame("id" = c(1,2,3,4,5), 
                     "text"= c("Blue is a beautiful color, but I do not like blue and prefer red",
                               "I hate red and orange dresses. Dresses have to be yellow!!!",
                               "The Ukraine colors are blue and yellow",
                               "Is white a color?",
                              "Rainbow has multiple colors such as  blue, yellow, red and orange"))

#named vector of weights of our dictionary words
color_dic <- c(0.5, 0.4, 0.1, 0.2, 0.1, 0.2, 0.4)
names(color_dic) <- c("blue", "red", "black", "yellow", "white", "orange", "green")

#create corpus
colors_corpus <- colors_df %>%
  corpus(text_field = "text")

#create dfm with tf-idf
colors_dfm <- colors_corpus %>%
  tokens() %>%
  tokens(remove_punct = TRUE) %>%
  tokens_tolower() %>%
  dfm() %>%
  dfm_tfidf()

#dfm with only the terms in our dictionary
colors_dfm_dic <- colors_dfm %>%
  dfm_select(pattern = names(color_dic)) #only words in dictionary

#weight the tdfs
colors_dfm_weighted <- dfm_weight(colors_dfm_dic, weight = color_dic)

#calculate score
numerator <- rowSums(colors_dfm_weighted)
denominator <-rowSums(colors_dfm) 
score <- numerator/denominator  
colors_df$score <- score


########################load dictionaries and create named vector###################################
load("fear_scores.Rdata")
load("anger_scores.Rdata")
load("trust_scores.Rdata")
load("negative_scores.Rdata")

fear_dic <- fear_scores[,1]
names(fear_dic) <- row.names(fear_scores)

anger_dic <- anger_scores[,1]
names(anger_dic) <- row.names(anger_scores)

trust_dic <- trust_scores[,1]
names(trust_dic) <- row.names(trust_scores)

negative_dic <- negative_scores[,1]
names(negative_dic) <- row.names(negative_scores)

###################sentiment title##########################################
#create corpus
title_corpus <- articlesRich %>%
  corpus(text_field = "title")

#create dfm with tf-idf
title_dfm <- title_corpus %>%
  tokens() %>%
  tokens(remove_punct = TRUE) %>%
  tokens_tolower() %>%
  dfm() %>%
  dfm_tfidf()

#title sentiment fear
#dfm with only the terms in our fear dictionary
title_dfm_fear <- title_dfm %>%
  dfm_select(pattern = names(fear_dic)) #only words in dictionary

#weight the tdfs
title_dfm_fear_weighted <- dfm_weight(title_dfm_fear, weight = fear_dic)

#calculate fear score
numerator <- rowSums(title_dfm_fear_weighted)
denominator <-rowSums(title_dfm) 
score <- numerator/denominator  
articlesRich$titleFearScore <- score


#title sentiment anger
#dfm with only the terms in our anger dictionary
title_dfm_anger <- title_dfm %>%
  dfm_select(pattern = names(anger_dic)) #only words in dictionary

#weight the tdfs
title_dfm_anger_weighted <- dfm_weight(title_dfm_anger, weight = anger_dic)

#calculate anger score
numerator <- rowSums(title_dfm_anger_weighted)
denominator <-rowSums(title_dfm) 
score <- numerator/denominator  
articlesRich$titleAngerScore <- score


#title sentiment trust
#dfm with only the terms in our trust dictionary
title_dfm_trust <- title_dfm %>%
  dfm_select(pattern = names(trust_dic)) #only words in dictionary

#weight the tdfs
title_dfm_trust_weighted <- dfm_weight(title_dfm_trust, weight = trust_dic)

#calculate trust score
numerator <- rowSums(title_dfm_trust_weighted)
denominator <-rowSums(title_dfm) 
score <- numerator/denominator  
articlesRich$titleTrustScore <- score

#title sentiment negative
#dfm with only the terms in our negative dictionary
title_dfm_negative <- title_dfm %>%
  dfm_select(pattern = names(negative_dic)) #only words in dictionary

#weight the tdfs
title_dfm_negative_weighted <- dfm_weight(title_dfm_negative, weight = negative_dic)

#calculate trust score
numerator <- rowSums(title_dfm_negative_weighted)
denominator <-rowSums(title_dfm) 
score <- numerator/denominator  
articlesRich$titleNegativeScore <- score


####################################sentiment article####################################################
#create corpus
article_corpus <- articlesRich %>%
  corpus(text_field = "article")

#create dfm with tf-idf
article_dfm <- article_corpus %>%
  tokens() %>%
  tokens(remove_punct = TRUE) %>%
  tokens_tolower() %>%
  dfm() %>%
  dfm_tfidf()

#article sentiment fear
#dfm with only the terms in our fear dictionary
article_dfm_fear <- article_dfm %>%
  dfm_select(pattern = names(fear_dic)) #only words in dictionary

#weight the tdfs
article_dfm_fear_weighted <- dfm_weight(article_dfm_fear, weight = fear_dic)

#calculate fear score
numerator <- rowSums(article_dfm_fear_weighted)
denominator <-rowSums(article_dfm) 
score <- numerator/denominator  
articlesRich$articleFearScore <- score


#article sentiment anger
#dfm with only the terms in our anger dictionary
article_dfm_anger <- article_dfm %>%
  dfm_select(pattern = names(anger_dic)) #only words in dictionary

#weight the tdfs
article_dfm_anger_weighted <- dfm_weight(article_dfm_anger, weight = anger_dic)

#calculate anger score
numerator <- rowSums(article_dfm_anger_weighted)
denominator <-rowSums(article_dfm) 
score <- numerator/denominator  
articlesRich$articleAngerScore <- score


#article sentiment trust
#dfm with only the terms in our trust dictionary
article_dfm_trust <- article_dfm %>%
  dfm_select(pattern = names(trust_dic)) #only words in dictionary

#weight the tdfs
article_dfm_trust_weighted <- dfm_weight(article_dfm_trust, weight = trust_dic)

#calculate trust score
numerator <- rowSums(article_dfm_trust_weighted)
denominator <-rowSums(article_dfm) 
score <- numerator/denominator  
articlesRich$articleTrustScore <- score

#article sentiment negative
#dfm with only the terms in our negative dictionary
article_dfm_negative <- article_dfm %>%
  dfm_select(pattern = names(negative_dic)) #only words in dictionary

#weight the tdfs
article_dfm_negative_weighted <- dfm_weight(article_dfm_negative, weight = negative_dic)

#calculate trust score
numerator <- rowSums(article_dfm_negative_weighted)
denominator <-rowSums(article_dfm) 
score <- numerator/denominator  
articlesRich$articleNegativeScore <- score



###############sentiment comments######################################################
#create corpus
comments_corpus <- comments %>%
  corpus(text_field = "comment")

#create dfm with tf-idf
comments_dfm <- comments_corpus %>%
  tokens() %>%
  tokens(remove_punct = TRUE) %>%
  tokens_tolower() %>%
  dfm() %>%
  dfm_tfidf()

#comment sentiment fear
#dfm with only the terms in our fear dictionary
comments_dfm_fear <- comments_dfm %>%
  dfm_select(pattern = names(fear_dic)) #only words in dictionary

#weight the tdfs
comments_dfm_fear_weighted <- dfm_weight(comments_dfm_fear, weight = fear_dic)

#calculate fear score
numerator <- rowSums(comments_dfm_fear_weighted)
denominator <-rowSums(comments_dfm) 
score <- numerator/denominator  
comments$commentsFearScore <- score


#comment sentiment anger
#dfm with only the terms in our anger dictionary
comments_dfm_anger <- comments_dfm %>%
  dfm_select(pattern = names(anger_dic)) #only words in dictionary

#weight the tdfs
comments_dfm_anger_weighted <- dfm_weight(comments_dfm_anger, weight = anger_dic)

#calculate anger score
numerator <- rowSums(comments_dfm_anger_weighted)
denominator <-rowSums(comments_dfm) 
score <- numerator/denominator  
comments$commentsAngerScore <- score


#comment sentiment trust
#dfm with only the terms in our trust dictionary
comments_dfm_trust <- comments_dfm %>%
  dfm_select(pattern = names(trust_dic)) #only words in dictionary

#weight the tdfs
comments_dfm_trust_weighted <- dfm_weight(comments_dfm_trust, weight = trust_dic)

#calculate trust score
numerator <- rowSums(comments_dfm_trust_weighted)
denominator <-rowSums(comments_dfm) 
score <- numerator/denominator  
comments$commentsTrustScore <- score

#comments sentiment negative
#dfm with only the terms in our negative dictionary
comments_dfm_negative <- comments_dfm %>%
  dfm_select(pattern = names(negative_dic)) #only words in dictionary

#weight the tdfs
comments_dfm_negative_weighted <- dfm_weight(comments_dfm_negative, weight = negative_dic)

#calculate trust score
numerator <- rowSums(comments_dfm_negative_weighted)
denominator <-rowSums(comments_dfm) 
score <- numerator/denominator  
comments$commentsNegativeScore <- score

#mean scores

#calculate mean comments scores fear
comments_fear <- comments %>%
  group_by(Id) %>%
  summarise(commentsFearScore= mean(commentsFearScore) )
#merge with articlesRich
articlesRich <- merge(articlesRich, comments_fear, by = "Id", all.x = TRUE)

#calculate mean comments scores anger
comments_anger <- comments %>%
  group_by(Id) %>%
  summarise(commentsAngerScore= mean(commentsAngerScore) )
#merge with articlesRich
articlesRich <- merge(articlesRich, comments_anger, by = "Id", all.x = TRUE)

#calculate mean comments scores trust
comments_trust <- comments %>%
  group_by(Id) %>%
  summarise(commentsTrustScore= mean(commentsTrustScore) )
#merge with articlesRich
articlesRich <- merge(articlesRich, comments_trust, by = "Id", all.x = TRUE)

#calculate mean comments scores negative
comments_negative <- comments %>%
  group_by(Id) %>%
  summarise(commentsNegativeScore= mean(commentsNegativeScore) )
#merge with articlesRich
articlesRich <- merge(articlesRich, comments_negative, by = "Id", all.x = TRUE)


save(articlesRich, file = "articlesRich.Rdata")

#save in smaller data files
articlesRich1 <- articlesRich[1:10000,]
save(articlesRich1, file = "articlesRich1.Rdata" )
articlesRich2 <- articlesRich[10001:17869,]
save(articlesRich2, file = "articlesRich2.Rdata" )
