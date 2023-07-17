# Does the media frame public discourse online? 
## The impact of content variations of German news articles related to Covid-19 on the public discourse online by using NLP methods to analyze their comments

## Data
I scraped news articles and their comments from 3 different news outlets (Zeit, Welt, Handelsblatt) related to the topic of Covid-19 from 01.01.2020 to 30.06.2023. I used [WebScraper.io](https://webscraper.io/) (Google Chrome Extension) and R scripts. The files can be found [here](Data/Datafiles).

![NumberArticlesComments.JPG](https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/Data/Datafiles/NumberArticlesComments.JPG)


> __Note__: Because of the size file limit, I split the articles and comments files into smaller files seperately, and they have to be loaded and combined again as shown in the following:
```markdown
#articles
load("articles1.Rdata")
load("articles2.Rdata")
articles <- rbind(articles1, articlest2)

#comments
load("comments1.Rdata")
load("comments2.Rdata")
load("comments3.Rdata")
load("comments4.Rdata")
load("comments5.Rdata")
comments <- rbind(comments1, comments2)
comments <- rbind(comments, comments3)
comments <- rbind(comments, comments4)
comments <- rbind(comments, comments5)
```
### Descriptive Analysis - Number of Comments
Please run this script: [descriptiveAnalysis.R](https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/Data/DescriptiveAnalysis/descriptiveAnalysis.R)

<img src="https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/Data/DescriptiveAnalysis/NumberCommentsHB.JPG" width="750">

<img src="https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/Data/DescriptiveAnalysis/NumberCommentsZeit.JPG" width="750">

<img src="https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/Data/DescriptiveAnalysis/NumberCommentsWelt.JPG" width="750">

<img src="https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/Data/DescriptiveAnalysis/%23ArticlesWithOrWithoutComments.JPG" width="750">



## Topic Modeling
Please run this script: [getTopicsOfArticles.R](https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/TopicModelling/getTopicsOfArticles.R)


I used the searchK function to find the best value for K and chose K=17
![Search_k.JPG](https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/TopicModelling/Search_k.JPG)
![Search_k_detail.JPG](https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/TopicModelling/Search_k_detail.JPG)

The model for K=17 can be found [here](https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/TopicModelling/topicModel.Rdata) and this [file](https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/TopicModelling/Topics.pdf) represents the topics. I used this model to label each article in our corpus with a topic and used [this](https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/TopicModelling/topicsArticle.Rdata) in the [CreatePredictors.R script](https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/Predictors/CreatePredictors.R).

## Dictionary Expansion with Self-trained Word Embeddings
Please run these script: [estimateEmbeddings.R](https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/DictinaryExpansion/estimateEmbeddings.R) and [prepareDictionary.R](https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/DictinaryExpansion/prepareDictionary.R)

I used all articles and comments to train own word embeddings by running following R script:

```markdown
glove = GlobalVectors$new(rank = 150, x_max = 2500L, learning_rate = .145)
textAll_main = glove$fit_transform(textAll_fcm, n_iter = 500, convergence_tol = 0.005, n_threads = 8)

textAll_context = glove$components

word_vectors = textAll_main + t(textAll_context)

save(word_vectors, file = "word_vectors_150.Rdata")
```

I started to use words from the [NRC lexicon](https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/DictinaryExpansion/German-NRC-EmoLex.txt) in the categories **fear**, **anger**, **trust** and **negative** and for each I expanded them with the word embeddings like this (shown for fear):

```markdown
#fear
nrc_fear <- filter(nrc, nrc$fear ==1)
fear_words <- nrc_fear$German.Word


#1) extract words from embeddings which are in fear words
fear_emb <- word_vectors[rownames(word_vectors) %in% fear_words,]

#2) calculate mean embedding vector of the fear dictionary words
fear_mean <- colMeans(fear_emb) #is a numeric vector

#3) calculate the similarity between the mean fear vector and every other word in our embeddings
cos_sim <- sim2 (x=word_vectors,
                 y= matrix(fear_mean, nrow=1))

#4) sore results
fear_scores <- data.frame(score = cos_sim[,1], in_original_dictionary = dimnames(cos_sim)[[1]] %in% fear_words)
fear_scores <- fear_scores[order(fear_scores$score, decreasing = TRUE),]
  
head(fear_scores,30)

save(fear_scores, file ="fear_scores.Rdata")
```
Instead of choosing the e.g. top 500 words (i.e. all words in the embeddings where given a score), I use these scores to calculate the sentiment scores.
I did it as shown in this very simple example:

```
#score = (sim * tdf)/sum(tdf)

#example
colors_df <- data.frame("id" = c(1,2,3,4,5), 
                     "text"= c("Blue is a beautiful color, but I do not like blue and prefer red",
                               "I hate red and orange dresses. Dresses have to be yellow!!!",
                               "The Ukraine colors are blue and yellow",
                               "Is white a color?",
                              "Rainbow has multiple colors such as  blue, yellow, red and orange"))

#named vector of weights of our dictionary words (this are the score files calculated above)
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
```

## Prepare Predictors
Please run this script: [CreatePredictors.R](https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/Predictors/CreatePredictors.R)

Predictors/ independent variables: date, lengthTitle, lengthArticle, typetokenratio article,  genderOfAuthor, ReadabilityArticle, topicArticle,
SentimentsTitle, SentimentsArticle 
Output/ dependent variables: numberComments, sentimentComments

The [articlesRich1](https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/Predictors/articlesRich1.Rdata) and [articlesRich2](https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/Predictors/articlesRich2.Rdata) files contain all predictors and outputs - I stored them again seperately and to use them run following code:

```markdown
#articles
load("articlesRich1.Rdata")
load("articlesRich22.Rdata")
articlesRich <- rbind(articles1, articlest2)
```
## Analysis
