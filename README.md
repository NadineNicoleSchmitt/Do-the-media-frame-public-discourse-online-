# Does the media frame public discourse online? 
## The impact of content variations of German news articles related to Covid-19 on the public discourse online by using NLP methods to analyze their comments

## Data
I scraped news articles and their comments from 3 different news outlets (Zeit, Welt, Handelsblatt) related to the topic of Covid-19 from 01.01.2020 to 30.06.2023. I used [WebScraper.io](https://webscraper.io/) (Google Chrome Extension) and R scripts. The files can be found [here](Data/Datafiles).

![NumberArticlesComments.JPG](https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/Data/NumberArticlesComments.JPG)


> __Note__: Because of the size file limit, I split the comments file for Zeit and Welt into three files seperately, and they have to be loaded and combined again as shown in the following:
```markdown
load("Zeit_comment1.Rdata")
load("Zeit_comment2.Rdata")
load("Zeit_comment3.Rdata")
zeit_comment <- rbind(zeit_comment1, zeit_comment2)
zeit_comment <- rbind(zeit_comment, zeit_comment3)
```


## Topic Modeling
![Search_k.JPG](https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/TopicModeling/Search_k.JPG)
![Search_k_detail.JPG](https://github.com/NadineNicoleSchmitt/Does-the-media-frame-public-discourse-online/blob/main/TopicModeling/Search_k_detail.JPG)
