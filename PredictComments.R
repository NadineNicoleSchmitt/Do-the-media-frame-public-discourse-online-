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


#load datafile
load("articlesRich.Rdata")

hb <- filter(articlesRich, articlesRich$outlet == "HB")
zeit <- filter(articlesRich, articlesRich$outlet == "Zeit")
welt <- filter(articlesRich, articlesRich$outlet == "Welt")

hb_with_Comments <- filter(hb, hb$numberComments!= 0)
zeit_with_Comments <- filter(zeit, zeit$numberComments!= 0)
welt_with_Comments <- filter(welt, welt$numberComments!= 0)


##############################Date###################################################################

#hb date vs. numberComments
ggplot(hb, aes(x=date, y=numberComments)) + 
  geom_point()+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()


#zeit date vs. commentsFearScore
ggplot(zeit_with_Comments, aes(x=date, y=commentsFearScore)) + 
  geom_point()+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

#zeit date vs. commentsAngerScore
ggplot(zeit_with_Comments, aes(x=date, y=commentsAngerScore)) + 
  geom_point()+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

#zeit date vs. commentsTrustScore
ggplot(zeit_with_Comments, aes(x=date, y=commentsTrustScore)) + 
  geom_point()+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

#zeit date vs. commentsNegativeScore
ggplot(zeit_with_Comments, aes(x=date, y=commentsNegativeScore)) + 
  geom_point()+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()


#Zeit date vs. numberComments
ggplot(zeit, aes(x=date, y=numberComments)) + 
  geom_point()+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()


#Zeit date vs. commentsFearScore
ggplot(zeit_with_Comments, aes(x=date, y=commentsFearScore)) + 
  geom_point()+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

#Zeit date vs. commentsAngerScore
ggplot(zeit_with_Comments, aes(x=date, y=commentsAngerScore)) + 
  geom_point()+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

#zeit date vs. commentsTrustScore
ggplot(zeit_with_Comments, aes(x=date, y=commentsTrustScore)) + 
  geom_point()+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

#zeit date vs. commentsNegativeScore
ggplot(zeit_with_Comments, aes(x=date, y=commentsNegativeScore)) + 
  geom_point()+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()



#Welt date vs. numberComments
ggplot(welt, aes(x=date, y=numberComments)) + 
  geom_point()+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()


#Welt date vs. commentsFearScore
ggplot(welt_with_Comments, aes(x=date, y=commentsFearScore)) + 
  geom_point()+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

#Welt date vs. commentsAngerScore
ggplot(welt_with_Comments, aes(x=date, y=commentsAngerScore)) + 
  geom_point()+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

#welt date vs. commentsTrustScore
ggplot(welt_with_Comments, aes(x=date, y=commentsTrustScore)) + 
  geom_point()+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

#welt date vs. commentsNegativeScore
ggplot(welt_with_Comments, aes(x=date, y=commentsNegativeScore)) + 
  geom_point()+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()



#################################Gender Author##########################################

#hb gender vs. numberComments
ggplot(hb, aes(x=genderAuthor, y=numberComments)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


#hb gender vs. commentsFearScore
ggplot(hb_with_Comments, aes(x=genderAuthor, y=commentsFearScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#hb gender vs. commentsAngerScore
ggplot(hb_with_Comments, aes(x=genderAuthor, y=commentsAngerScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#hb gender vs. commentsTrustScore
ggplot(hb_with_Comments, aes(x=genderAuthor, y=commentsTrustScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#hb gender vs. commentsNegativeScore
ggplot(hb_with_Comments, aes(x=genderAuthor, y=commentsNegativeScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))




#zeit gender vs. numberComments
ggplot(zeit, aes(x=genderAuthor, y=numberComments)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


#zeit gender vs. commentsFearScore
ggplot(zeit_with_Comments, aes(x=genderAuthor, y=commentsFearScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#zeit gender vs. commentsAngerScore
ggplot(zeit_with_Comments, aes(x=genderAuthor, y=commentsAngerScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#zeit gender vs. commentsTrustScore
ggplot(zeit_with_Comments, aes(x=genderAuthor, y=commentsTrustScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#zeit gender vs. commentsNegativeScore
ggplot(zeit_with_Comments, aes(x=genderAuthor, y=commentsNegativeScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))



#welt gender vs. numberComments
ggplot(welt, aes(x=genderAuthor, y=numberComments)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


#welt gender vs. commentsFearScore
ggplot(welt_with_Comments, aes(x=genderAuthor, y=commentsFearScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#welt gender vs. commentsAngerScore
ggplot(welt_with_Comments, aes(x=genderAuthor, y=commentsAngerScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#welt gender vs. commentsTrustScore
ggplot(welt_with_Comments, aes(x=genderAuthor, y=commentsTrustScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#welt gender vs. commentsNegativeScore
ggplot(welt_with_Comments, aes(x=genderAuthor, y=commentsNegativeScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


#####################################Topic########################################################

#hb topic vs. numberComments
ggplot(hb, aes(x=TopicArticle, y=numberComments)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#topic 2 (incidence), 5 (information about virus), 7 (policy), 8 (european esm), 
#11 (restrictions, lockdown), 16 (vaccines)


#hb topic vs. commentsFearScore
ggplot(hb_with_Comments, aes(x=TopicArticle, y=commentsFearScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#hb topic vs. commentsAngerScore
ggplot(hb_with_Comments, aes(x=TopicArticle, y=commentsAngerScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#hb topic vs. commentsTrustScore
ggplot(hb_with_Comments, aes(x=TopicArticle, y=commentsTrustScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#hb topic vs. commentsNegativeScore
ggplot(hb_with_Comments, aes(x=TopicArticle, y=commentsNegativeScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))



#Zeit topic vs. numberComments
ggplot(zeit, aes(x=TopicArticle, y=numberComments)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


#zeit topic vs. commentsFearScore
ggplot(zeit_with_Comments, aes(x=TopicArticle, y=commentsFearScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#zeit topic vs. commentsAngerScore
ggplot(zeit_with_Comments, aes(x=TopicArticle, y=commentsAngerScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#zeit topic vs. commentsTrustScore
ggplot(zeit_with_Comments, aes(x=TopicArticle, y=commentsTrustScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#zeit topic vs. commentsNegativeScore
ggplot(zeit_with_Comments, aes(x=TopicArticle, y=commentsNegativeScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


#welt topic vs. numberComments
ggplot(welt, aes(x=TopicArticle, y=numberComments)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


#welt topic vs. commentsFearScore
ggplot(welt_with_Comments, aes(x=TopicArticle, y=commentsFearScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#welt topic vs. commentsAngerScore
ggplot(welt_with_Comments, aes(x=TopicArticle, y=commentsAngerScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#welt topic vs. commentsTrustScore
ggplot(welt_with_Comments, aes(x=TopicArticle, y=commentsTrustScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#welt topic vs. commentsNegativeScore
ggplot(welt_with_Comments, aes(x=TopicArticle, y=commentsNegativeScore)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))



################################NumberTokensTitle##############################


ggplot(articlesRich, aes(x=numberTokensTitle, y=numberComments)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=numberTokensTitle, y=commentsFearScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=numberTokensTitle, y=commentsAngerScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=numberTokensTitle, y=commentsTrustScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=numberTokensTitle, y=commentsNegativeScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

################################NumberTokensArticle##############################


ggplot(articlesRich, aes(x=numberTokensArticle, y=numberComments)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=numberTokensArticle, y=commentsFearScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=numberTokensArticle, y=commentsAngerScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=numberTokensArticle, y=commentsTrustScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=numberTokensArticle, y=commentsNegativeScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()


################################TypeToTokenRatioArticle##############################


ggplot(articlesRich, aes(x=typeToTokenRatioArticle, y=numberComments)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=typeToTokenRatioArticle, y=commentsFearScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=typeToTokenRatioArticle, y=commentsAngerScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=typeToTokenRatioArticle, y=commentsTrustScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=typeToTokenRatioArticle, y=commentsNegativeScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()


################################Readability##############################


ggplot(articlesRich, aes(x=readabilityArticle, y=numberComments)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=readabilityArticle, y=commentsFearScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=readabilityArticle, y=commentsAngerScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=readabilityArticle, y=commentsTrustScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=readabilityArticle, y=commentsNegativeScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()


################################TitleFearScore##############################


ggplot(articlesRich, aes(x=titleFearScore, y=numberComments)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=titleFearScore, y=commentsFearScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=titleFearScore, y=commentsAngerScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=titleFearScore, y=commentsTrustScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=titleFearScore, y=commentsNegativeScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

################################TitleAngerScore##############################


ggplot(articlesRich, aes(x=titleAngerScore, y=numberComments)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=titleAngerScore, y=commentsFearScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=titleAngerScore, y=commentsAngerScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=titleAngerScore, y=commentsTrustScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=titleAngerScore, y=commentsNegativeScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()


################################TitleTrustScore##############################


ggplot(articlesRich, aes(x=titleTrustScore, y=numberComments)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=titleTrustScore, y=commentsFearScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=titleTrustScore, y=commentsAngerScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=titleTrustScore, y=commentsTrustScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=titleTrustScore, y=commentsNegativeScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()


################################TitleNegativeScore##############################


ggplot(articlesRich, aes(x=titleNegativeScore, y=numberComments)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=titleNegativeScore, y=commentsFearScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=titleNegativeScore, y=commentsAngerScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=titleNegativeScore, y=commentsTrustScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=titleNegativeScore, y=commentsNegativeScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()


################################ArticleFearScore##############################


ggplot(articlesRich, aes(x=articleFearScore, y=numberComments)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=articleFearScore, y=commentsFearScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=articleFearScore, y=commentsAngerScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=articleFearScore, y=commentsTrustScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=articleFearScore, y=commentsNegativeScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

################################articleAngerScore##############################


ggplot(articlesRich, aes(x=articleAngerScore, y=numberComments)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=articleAngerScore, y=commentsFearScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=articleAngerScore, y=commentsAngerScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=articleAngerScore, y=commentsTrustScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=articleAngerScore, y=commentsNegativeScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()


################################articleTrustScore##############################


ggplot(articlesRich, aes(x=articleTrustScore, y=numberComments)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=articleTrustScore, y=commentsFearScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=articleTrustScore, y=commentsAngerScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=articleTrustScore, y=commentsTrustScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=articleTrustScore, y=commentsNegativeScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()


################################articleNegativeScore##############################


ggplot(articlesRich, aes(x=articleNegativeScore, y=numberComments)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=articleNegativeScore, y=commentsFearScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=articleNegativeScore, y=commentsAngerScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=articleNegativeScore, y=commentsTrustScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(articlesRich, aes(x=articleNegativeScore, y=commentsNegativeScore)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()





#Regression Poisson
poisson <- glm(numberComments ~ date + genderAuthor + TopicArticle + numberTokensTitle + numberTokensArticle + 
              typeToTokenRatioArticle + readabilityArticle + titleFearScore + titleAngerScore +
              titleTrustScore + titleNegativeScore + articleFearScore + articleAngerScore + articleTrustScore +
              articleNegativeScore, articlesRich, family =poisson(link ="log"))
summary(poisson)

poisson_fear <- glm(commentsFearScore ~ date + genderAuthor + TopicArticle + numberTokensTitle + numberTokensArticle + 
                 typeToTokenRatioArticle + readabilityArticle + titleFearScore + titleAngerScore +
                 titleTrustScore + titleNegativeScore + articleFearScore + articleAngerScore + articleTrustScore +
                 articleNegativeScore, articlesRich, family =poisson(link ="log"))
summary(poisson_fear)

poisson_anger <- glm(commentsAngerScore ~ date + genderAuthor + TopicArticle + numberTokensTitle + numberTokensArticle + 
                      typeToTokenRatioArticle + readabilityArticle + titleFearScore + titleAngerScore +
                      titleTrustScore + titleNegativeScore + articleFearScore + articleAngerScore + articleTrustScore +
                      articleNegativeScore, articlesRich, family =poisson(link ="log"))
summary(poisson_anger)


poisson_trust <- glm(commentsTrustScore ~ date + genderAuthor + TopicArticle + numberTokensTitle + numberTokensArticle + 
                      typeToTokenRatioArticle + readabilityArticle + titleFearScore + titleAngerScore +
                      titleTrustScore + titleNegativeScore + articleFearScore + articleAngerScore + articleTrustScore +
                      articleNegativeScore, articlesRich, family =poisson(link ="log"))
summary(poisson_trust)

poisson_negative <- glm(commentsNegativeScore ~ date + genderAuthor + TopicArticle + numberTokensTitle + numberTokensArticle + 
                       typeToTokenRatioArticle + readabilityArticle + titleFearScore + titleAngerScore +
                       titleTrustScore + titleNegativeScore + articleFearScore + articleAngerScore + articleTrustScore +
                       articleNegativeScore, articlesRich, family =poisson(link ="log"))
summary(poisson_negative)
