# Change working directory 
setwd("C:/Users/nadin/OneDrive/Dokumente/Dissertation")

# Clear the environment by removing any defined objects/functions
rm(list = ls())

library(qcc)

#load datafiles
hb <- read_xlsx("Final/HandelsblattArtikel.xlsx")
zeit <- read_xlsx("Final/ZeitArtikel.xlsx")
welt <- read_xlsx("Final/WeltArtikel.xlsx")
hb_comment <- read_xlsx("Final/HandelsblattComment.xlsx")
zeit_comment <- read_xlsx("Final/ZeitComment.xlsx")
welt_comment <- read_xlsx("Final/WeltComment.xlsx")

#identify articles where we not found comments 
hb_with_Comments  <- filter(hb, hb$numberComments != 0)
zeit_with_Comments  <- filter(zeit, zeit$numberComments != 0)
welt_with_Comments  <- filter(welt, welt$numberComments != 0)

hb_with_Comments_false <- filter(hb_with_Comments, !(hb_with_Comments$Id %in% hb_comment$Id))
zeit_with_Comments_false <- filter(zeit_with_Comments, !(zeit_with_Comments$Id %in% zeit_comment$Id))
welt_with_Comments_false <- filter(welt_with_Comments, !(welt_with_Comments$Id %in% welt_comment$Id))

welt <- filter(welt, !(welt$Id %in% welt_with_Comments_false$Id))


#check for NA
na <- hb[!complete.cases(hb), ]
na <- zeit[!complete.cases(zeit), ] #title is missing
zeit <- filter(zeit, !(zeit$Id %in% na$Id)) #remove them
#remove zeit comments for which there are NAs in article Zeit file
zeit_comment <- filter(zeit_comment, !(zeit_comment$Id %in% na$Id))

na <- welt[!complete.cases(welt), ]
na <- hb_comment[!complete.cases(hb_comment), ] #HB-3477 comment is missing
hb_comment <- hb_comment[complete.cases(hb_comment), ] #remove NA
na <- zeit_comment[!complete.cases(zeit_comment), ] #comments missing
zeit_comment <- zeit_comment[complete.cases(zeit_comment), ] #remove NA
na <- welt_comment[!complete.cases(welt_comment), ] #comments missing
welt_comment <- welt_comment[complete.cases(welt_comment), ] #remove NA

#add column outlet
hb$outlet <- rep("HB", nrow(hb))
zeit$outlet <- rep("Zeit", nrow(zeit))
welt$outlet <- rep("Welt", nrow(welt))


#save them all
save(hb, file = "Final/HB.Rdata")
save(zeit, file = "Final/Zeit.Rdata")
save(welt, file = "Final/Welt.Rdata")
save(hb_comment, file = "Final/HB_comment.Rdata")
save(zeit_comment, file = "Final/Zeit_comment.Rdata")
save(welt_comment, file = "Final/Welt_comment.Rdata")

#save in smaller data files
zeit_comment1 <- zeit_comment[1:150000,]
save(zeit_comment1, file = "Final/Zeit_comment1.Rdata" )
zeit_comment2 <- zeit_comment[150001:280000,]
save(zeit_comment2, file = "Final/Zeit_comment2.Rdata" )
zeit_comment3 <- zeit_comment[280001:315195,]
save(zeit_comment3, file = "Final/Zeit_comment3.Rdata" )


welt_comment1 <- welt_comment[1:150000,]
save(welt_comment1, file = "Final/Welt_comment1.Rdata" )
welt_comment2 <- welt_comment[150001:300000,]
save(welt_comment2, file = "Final/Welt_comment2.Rdata" )
welt_comment3 <- welt_comment[300001:410172,]
save(welt_comment3, file = "Final/Welt_comment3.Rdata" )

#all articles in one file
articles <- rbind(hb, zeit)
articles <- rbind(articles, welt)
save(articles, file = "Final/articles.Rdata" )


#create table number of articles/ comments
df <- data.frame("ZeitOnline" = c(nrow(zeit),nrow(zeit_comment)),
                 "Welt" = c(nrow(welt),nrow(welt_comment)),
                 "Handelsblatt" = c(nrow(hb),nrow(hb_comment)),
                 "Total" = c(0,0))

rownames(df) <- c("Article", "comments")
df[1,4] <- df[1,1] + df[1,2] +df[1,3]
df[2,4] <- df[2,1] + df[2,2] +df[2,3]
formattable(df)

##########################descriptive analysis######################################################################

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

zeit_numberComments_plot <- ggplot(zeit_numberComments) +
  geom_point(aes(x= numberComments, y = count)) 
  

ggplot(zeit, aes(x=numberComments)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

ggplot(zeit, aes(x=numberComments)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")


zeit_numberComments <-  zeit_numberComments%>%
  mutate(group = as.integer(zeit_numberComments$numberComments/ 50)) 

zeit_numberComments50 <- zeit_numberComments %>% 
  group_by(group) %>%
  summarise(count50= sum(count))

counts <- zeit_numberComments50$count50
names(counts) <- zeit_numberComments50$group*50
zeit_numberComments_plot <- pareto.chart(counts, 
                                       main = "Pareto Chart for Number of Comments outlet Zeit (0 indicates 0-50 comments)", 
                                       col = rainbow(length(counts)), 
                                       cumperc = seq(0,100, by= 2.5))

welt_numberComments <- welt %>% 
  group_by(numberComments) %>%
  summarise(count= n())

welt_numberComments <-  welt_numberComments%>%
  mutate(group = as.integer(welt_numberComments$numberComments/ 50)) 

welt_numberComments50 <- welt_numberComments %>% 
  group_by(group) %>%
  summarise(count50= sum(count))

counts <- welt_numberComments50$count50
names(counts) <- welt_numberComments50$group*50
welt_numberComments_plot <- pareto.chart(counts, 
                                         main = "Pareto Chart for Number of Comments outlet Welt (0 indicates 0-50 comments)", 
                                         col = rainbow(length(counts)), 
                                         cumperc = seq(0,100, by= 2.5))

