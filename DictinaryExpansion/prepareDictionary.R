# Change working directory 
setwd("C:/Users/nadin/OneDrive/Dokumente/Dissertation")

# Clear the environment by removing any defined objects/functions
rm(list = ls())


library(quanteda)

#load self-trained embeddings
load("Embeddings/word_vectors_150.Rdata")

#use nrc dictionary
nrc <- read.delim("German-NRC-EmoLex.txt")

###############################fear#######################################################
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

###########################################anger#########################################################

#anger
nrc_anger <- filter(nrc, nrc$anger ==1)
anger_words <- nrc_anger$German.Word


#1) extract words from embeddings which are in fear words
anger_emb <- word_vectors[rownames(word_vectors) %in% anger_words,]

#2) calculate mean embedding vector of the anger dictionary words
anger_mean <- colMeans(anger_emb) #is a numeric vector

#3) calculate the similarity between the mean anger vector and every other word in our embeddings
cos_sim <- sim2 (x=word_vectors,
                 y= matrix(anger_mean, nrow=1))

#4) sore results
anger_scores <- data.frame(score = cos_sim[,1], in_original_dictionary = dimnames(cos_sim)[[1]] %in% anger_words)
anger_scores <- anger_scores[order(anger_scores$score, decreasing = TRUE),]

head(anger_scores,30)

save(anger_scores, file ="anger_scores.Rdata")


###############################################trust##############################################################
#trust

nrc_trust <- filter(nrc, nrc$trust ==1)
trust_words <- nrc_trust$German.Word


#1) extract words from embeddings which are in fear words
trust_emb <- word_vectors[rownames(word_vectors) %in% trust_words,]

#2) calculate mean embedding vector of the trust dictionary words
trust_mean <- colMeans(trust_emb) #is a numeric vector

#3) calculate the similarity between the mean trust vector and every other word in our embeddings
cos_sim <- sim2 (x=word_vectors,
                 y= matrix(trust_mean, nrow=1))

#4) sore results
trust_scores <- data.frame(score = cos_sim[,1], in_original_dictionary = dimnames(cos_sim)[[1]] %in% trust_words)
trust_scores <- trust_scores[order(trust_scores$score, decreasing = TRUE),]

head(trust_scores,30)

save(trust_scores, file ="trust_scores.Rdata")



#########################################negative#############################################################

#negative

nrc_negative <- filter(nrc, nrc$negative ==1)
negative_words <- nrc_negative$German.Word


#1) extract words from embeddings which are in fear words
negative_emb <- word_vectors[rownames(word_vectors) %in% negative_words,]

#2) calculate mean embedding vector of the negative dictionary words
negative_mean <- colMeans(negative_emb) #is a numeric vector

#3) calculate the similarity between the mean negative vector and every other word in our embeddings
cos_sim <- sim2 (x=word_vectors,
                 y= matrix(negative_mean, nrow=1))

#4) sore results
negative_scores <- data.frame(score = cos_sim[,1], in_original_dictionary = dimnames(cos_sim)[[1]] %in% negative_words)
negative_scores <- negative_scores[order(negative_scores$score, decreasing = TRUE),]

head(negative_scores,30)

save(negative_scores, file ="negative_scores.Rdata")
