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

The model for K=17 can be found [here](https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/TopicModelling/topicModel.Rdata) and this [file](https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/TopicModelling/Topics.pdf) represents the topics. I used this model to label each article in our corpus with a topic and used [this](https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/TopicModelling/topicsArticle.Rdata) in the preparePredictors.r script.
