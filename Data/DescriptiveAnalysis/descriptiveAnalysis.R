# Change working directory 
setwd("C:/Users/nadin/OneDrive/Dokumente/Dissertation")

# Clear the environment by removing any defined objects/functions
rm(list = ls())

library(qcc)
library(readxl)
library(formattable)

##########################descriptive analysis######################################################################

#load datafiles
load("Final/articles.Rdata")
#load("Final/HB_comment.Rdata")
#load("Final/Zeit_comment.Rdata")
#load("Final/Welt_comment.Rdata")
load("Final/comments.Rdata")

hb <- filter(articles, articles$outlet == "HB")
zeit <- filter(articles, articles$outlet == "Zeit")
welt <- filter(articles, articles$outlet == "Welt")


ggplot(hb, aes(x=numberComments)) +
 geom_histogram(colour="black", fill="white")+
 labs(title= "Histogram number of Comments HB")

hb_numberComments <- hb %>% 
  group_by(numberComments) %>%
  summarise(count= n())

counts <- hb_numberComments$count
names(counts) <- hb_numberComments$numberComments
hb_numberComments_plot <- pareto.chart(counts, 
                                       main = "Pareto Chart for Number of Comments outlet Handelsblatt", 
                                       col = rainbow(length(counts)), 
                                       cumperc = seq(0,100, by= 2.5))

zeit_numberComments <- zeit %>% 
  group_by(numberComments) %>%
  summarise(count= n())

#ggplot(zeit_numberComments) +
  #geom_point(aes(x= numberComments, y = count)) 


ggplot(zeit, aes(x=numberComments)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title= "Density number of Comments Zeit")

#ggplot(zeit, aes(x=numberComments)) +
  #geom_histogram(binwidth=.5, colour="black", fill="white")


zeit_numberComments <-  zeit_numberComments%>%
  mutate(group = as.integer(zeit_numberComments$numberComments/ 50)) 

zeit_numberComments50 <- zeit_numberComments %>% 
  group_by(group) %>%
  summarise(count50= sum(count))

counts <- zeit_numberComments50$count50
name <- paste(zeit_numberComments50$group*50, "to")
name <- paste(name, (zeit_numberComments50$group*50+50))
names(counts) <- name
zeit_numberComments_plot <- pareto.chart(counts, 
                                         main = "Pareto Chart for Number of Comments outlet Zeit", 
                                         col = rainbow(length(counts)), 
                                         cumperc = seq(0,100, by= 2.5))

ggplot(welt, aes(x=numberComments)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title= "Density number of Comments Welt")

welt_numberComments <- welt %>% 
  group_by(numberComments) %>%
  summarise(count= n())

welt_numberComments <-  welt_numberComments%>%
  mutate(group = as.integer(welt_numberComments$numberComments/ 50)) 

welt_numberComments50 <- welt_numberComments %>% 
  group_by(group) %>%
  summarise(count50= sum(count))

counts <- welt_numberComments50$count50
name <- paste(welt_numberComments50$group*50, "to")
name <- paste(name, (welt_numberComments50$group*50+50))
names(counts) <- name
welt_numberComments_plot <- pareto.chart(counts, 
                                         main = "Pareto Chart for Number of Comments outlet Welt", 
                                         col = rainbow(length(counts)), 
                                         cumperc = seq(0,100, by= 2.5))
                                         
#table for number Comments
numberComments <- data.frame("#Articles with no comments" = c(0,0,0),
                             "#Articles with comments"= c(0,0,0))
colnames(numberComments) <- c("#Articles with no comments", "#Articles with comments")
rownames(numberComments) <- c("HB", "Zeit", "Welt")


numberComments[1,1] <- paste(hb_numberComments[1,2], paste(paste("(", round((hb_numberComments[1,2]/nrow(hb))*100,2)), "%)"))
withComments <- sum(hb_numberComments$count) - hb_numberComments[1,2]
numberComments[1,2] <- paste(withComments, paste(paste("(", round((withComments/nrow(hb))*100,2)), "%)"))

numberComments[2,1] <- paste(zeit_numberComments[1,2], paste(paste("(", round((zeit_numberComments[1,2]/nrow(zeit))*100,2)), "%)"))
withComments <- sum(zeit_numberComments$count) - zeit_numberComments[1,2]
numberComments[2,2] <- paste(withComments, paste(paste("(", round((withComments/nrow(zeit))*100,2)), "%)"))

numberComments[3,1] <- paste(welt_numberComments[1,2], paste(paste("(", round((welt_numberComments[1,2]/nrow(welt))*100,2)), "%)"))
withComments <- sum(welt_numberComments$count) - welt_numberComments[1,2]
numberComments[3,2] <- paste(withComments, paste(paste("(", round((withComments/nrow(welt))*100,2)), "%)"))

formattable(numberComments)

