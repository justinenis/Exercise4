library(ggplot2)
library(tidyverse)
library(rsample)
library(dbplyr)
library(LICORS)  
library(foreach)
library(mosaic)
library(mvtnorm)
library(NbClust)
library(cluster)
library(dendextend)
library(factoextra)
library(tm)
library(gamlr)
library(SnowballC)
library(slam)
library(proxy)
library(igraph)
library(arules)
library(arulesViz)
library(pander)
library(caret)
library(e1071)
library(naivebayes)


#Question 1 

#imports data
wine = read.csv('/Users/franklinstudent/Desktop/GitHub/Exercise 4/wine.csv')


#creates a dummy variable for red and creates new column with dummy variable 
wine = wine %>%
  mutate(wine_color = ifelse(color == "red", 1, 0))


red_wine = wine %>%
  filter(wine_color == 1)

white_wine = wine %>%
  filter(wine_color == 0)

X = as.matrix(wine[,1:11])
y = wine[,14]

#running a pca on x 
pc_wine = prcomp(X, scale=TRUE)

#PCA using the 11 chemical properties 
winedata = pc_wine$x[,1:11]
pcr1 = lm(y ~ winedata)

#plots fitted values of PCAs
plot1 = plot(fitted(pcr1), y)

#combines original data set and PCA data
wine_plus_PCA = cbind(wine, winedata)

#creation of 95% confidence of ellipse 
figure1 = ggplot(wine_plus_PCA, aes(PC1, fixed.acidity, col = color, fill = color)) + 
  stat_ellipse(geom = "polygon", col= "black", alpha = 0.5)+ 
  geom_point(shape = 21, col = "black") + ggtitle("Figure 1")+ 
  theme(plot.title = element_text(hjust = 0.5))

#displays figure 1
figure1

#correlations between PC1 and 11 chemical properties
chem_cor= cor(wine[,-(12:14)], wine_plus_PCA[,15])
chem_cor

figure2 = ggplot(wine_plus_PCA, aes(PC1, wine_color, col = color, fill = color))  +  
  geom_point(shape = 21, col = "black") +
  ggtitle("Figure 2")+ theme(plot.title = element_text(hjust = 0.5))

figure2

#correlation between each chemical feature and pc1 and pc2 
wine_pred = cor(wine_plus_PCA[,14], wine_plus_PCA[,15])

wine_pred

#plot of PC1 and wine quality
ggplot(wine_plus_PCA, aes(PC1, quality, col = color, fill = color)) + 
  stat_ellipse(geom = "polygon", col= "black", alpha = 0.5)+ 
  geom_point(shape = 21, col = "black")

#correlation between quality and PC1
quality_cor = cor(wine[,12], wine_plus_PCA[,15])
quality_cor

#removes non-numeric variable
wine_dummy = wine[,-13]

#creates correlation plot 
cor_plot = ggcorrplot::ggcorrplot(cor(wine_dummy), hc.order = TRUE)





# HIERARCHIAL CLUSTERING

#11 chemical propterties 
X1 = wine[,1:11]
X1 = scale(X1, center = TRUE, scale=TRUE)


#elbow
k_grid = seq(2, 30, by=1)
SSE_grid = foreach(k = k_grid, .combine = 'c') %do% {
  cluster_k = kmeans(X1, k, nstart = 50)
  cluster_k$tot.withinss
}


#SSE plot, finding optimal k
plot(SSE_grid)


#finding distances
d_wine = dist(X1, method = "euclidean")
wine.hc = hclust(d = d_wine, method = "ward.D2")

#dendrogram
plot(wine.hc, labels = FALSE, hang = -1)

hc.col = color_branches(as.dendrogram(wine.hc), k = 2, labels = FALSE)
plot(hc.col)

rect.hclust(wine.hc, k = 2, border ='blue')



#cuts tree at k = 2, and creates cluster variable
cluster <- cutree(wine.hc, k = 2)

#distribution of clusters in 2 groups
table(cluster)

#creates a vector of cluster 
cluster_wine = as.matrix(cluster)

#combines wine data set and cluster vector
wine_plus_clust = cbind(wine, cluster_wine)

#creates new vector to re-classify cluster vector values as 1 for red or 0 for white
wine_plus_clust = wine_plus_clust %>%
  mutate(wine_pred = ifelse(cluster_wine == 1, 1, 0))


#correlation for 11 chemical properties
cor(wine_plus_clust[,14], wine_plus_clust[,16])


#correlation for quality
cor(wine_plus_clust[,14], wine_plus_clust[,12])






wine$color <- as.factor(cluster)

ggplot(wine, aes(x = sulphates, y = density, col = color)) + geom_point()




#### finds and gives barplot of optimal k ###############
nb.fit <-NbClust(wine[,-(12:13)], distance = 'euclidean', min.nc = 2, max.nc = 10, 
                 method = 'complete' , index = 'all')

fviz_nbclust(nb.fit)

###################################
















# Question 2
social = read.csv('/Users/franklinstudent/Desktop/GitHub/Exercise 4/social_marketing.csv')

#########
#renames rows as column X
rownames(social) <- c(social$X)
head(social)

#removes the first column labeled X
social = social[,-1]
#############


#creates matrix with only numerical vectors
X_social = social[,2:37]
X_social = scale(X_social, center=TRUE, scale=TRUE)
distances = dist(X_social, method = "euclidean")

#hierarchical clustering SINGLE = NO, COMPLETE = MAYBE, AVERAGE = MAYBE, WARD.D2 = YES
cluster_social = hclust(distances, method = 'ward.D2')
plot(cluster_social, labels = FALSE, hang = -1)


hc.social = color_branches(as.dendrogram(cluster_social), k = 10, labels = FALSE)
plot(hc.social)

rect.hclust(hc.social, k = 10, border ='blue')

table$hc.social

cor(X_social)

ggcorrplot::ggcorrplot(cor(X_social))

ggcorrplot::ggcorrplot(cor(X_social), hc.order = TRUE)





#creates k grid
k_grid = seq(2, 30, by=1)
SSE_grid = foreach(k = k_grid, .combine = 'c') %do% {
  cluster_k = kmeans(X_social, k, nstart = 50)
  cluster_k$tot.withinss
}

#SSE plot
plot(SSE_grid)

#k = 9
clusters <- cutree(cluster_social, k = 9)
table(clusters)

ggplot(X_social, aes(spam, adult, col = factor(clusters))) + 
  geom_point()


########## FINDS OPTIMAL K
nb.fit <-NbClust(X_social, distance = 'euclidean', min.nc = 2, max.nc = 10, 
                 method = 'complete' , index = 'all')

fviz_nbclust(nb.fit)











clusterGroups = cutree(cluster_social, k = 9)
tapply(social$spam, clusterGroups, mean)

clusterGroups[257]
cluster2 = subset(social, clusterGroups == 2)







ggplot(social)



qplot(spam, adult, data=social, color=factor(clusters))





k <- list()
for(i in 1:45){
  k[[i]]<-kmeans(X_social, i)
}

k

betweenss_totss <- list()
for(i in 1:45){
  betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}

plot(1:45, betweenss_totss, type = "b", 
     ylab = "Between SS/ Total SS", xlab = "Clusters (k)")








#combines original data set and PCA data
social_plus_PCA = cbind(social, socialdata)


#creation of 95% confidence of ellipse 
ggplot(social_plus_PCA, aes(sports_fandom, outdoors, col = outdoors, fill = outdoors)) + 
  stat_ellipse(geom = "polygon", col= "black", alpha = 0.7)+ 
  geom_point(shape = 21, col = "black")

#correlation between each chemical feature and pc1 and pc2 
cor(wine[,-13], wine_plus_PCA[,14:15])











social = social %>%
  mutate(bots = spam + adult) 

social = social[, -(35:36)]

X_social = social[,1:36]
X_social = scale(X_social, center=TRUE, scale=TRUE)
y = social[,35]

prc_social = prcomp(X_social, scale=TRUE)
socialdata = pc_social$x[,1:34]

pc_social = lm(y ~ socialdata)
plot_social = plot(fitted(pc_social), y)


summary(pc_wine)
plot(pc_wine, type = "l")
biplot(pc_wine, scale = 0)



#combines original data set and PCA data
social_plus_PCA = cbind(social, socialdata)


#creation of 95% confidence of ellipse 
ggplot(social_plus_PCA, aes(PC1, chatter, col = bots, fill = bots)) + 
  stat_ellipse(geom = "polygon", col= "black", alpha = 0.5)+ 
  geom_point(shape = 21, col = "black")

#correlation between each chemical feature and pc1 and pc2 
cor(social[,1], social_plus_PCA[,36:37])


# Question 3
#groceries = read.delim('/Users/franklinstudent/Desktop/GitHub/Exercise 4/groceries.txt',sep =",", header = TRUE, dec =".")
groceries = read.transactions('/Users/franklinstudent/Desktop/GitHub/Exercise 4/groceries.txt', sep = ",", format = "basket")


#plot of top 20 items purchased
itemFrequencyPlot(groceries, topN = 20, main = "\nTop 20 items purchased\n", col = "lightblue" )


#creates table
crossTable(groceries, measure = "support", sort = TRUE)[1:5, 1:5] %>%
  pander(split.table = Inf, round = 3)


# Now run the 'apriori' algorithm
# Look at rules with support > .01 & confidence >.1 & length (# artists) <= 5
groceries_rules = apriori(groceries, 
                          parameter=list(support=.001, confidence=.1, minlen = 2))


# observe lift = 3
plot(groceries_rules, measure = c("support", "lift"), shading = "confidence")


groceries_rules = apriori(groceries, 
                          parameter=list(support=.003, confidence=.5, minlen = 2))


#confidence = .65, lift = 3
plot(groceries_rules)

inspect(subset(groceries_rules, lift > 3 & confidence > 0.65))

# "two key" plot: coloring is by size (order) of item set
plot(groceries_rules, method='two-key plot')




groceries_rules_toplift = sort(groceries_rules, by = "lift", decreasing = TRUE)[1:10]
arules::inspect(groceries_rules_toplift)

groceries_rules_toplift = sort(groceries_rules, by = "support", decreasing = TRUE)[1:10]
arules::inspect(groceries_rules_toplift)


rules_soda_1 = apriori(groceries, parameter = list(support = 0.001,
                                                   confidence = 0.15,
                                                   minlen = 2,
                                                   target = 'rules'),
                       appearance = list(default = 'rhs', lhs = 'other vegetables'),
                       control = list(verbose = FALSE))
arules::inspect(sort(rules_soda_1, by= 'support', decreasing = TRUE)[1:5])

plot(rules_soda_1, method = "graph", interactive = FALSE, shading = NA)



rules_soda_r = apriori(groceries, parameter = list(support = 0.001,
                                                   confidence = 0.50,
                                                   minlen = 2,
                                                   target = 'rules'),
                       appearance = list(default = 'lhs', rhs = 'soda'),
                       control = list(verbose = FALSE))
arules::inspect(sort(rules_soda_r, by= 'support', decreasing = TRUE)[1:10])

plot(rules_soda_r, method = "graph", interactive = FALSE, shading = NA)



library(tm) 
library(magrittr)
library(e1071)
library(caret)
library(dplyr)
library(doParallel)
library(foreach)
library(randomForest)
library(plyr)

#Question 4

# Remember to source in the "reader" wrapper function
# it's stored as a Github gist at:
# https://gist.github.com/jgscott/28d9d1287a0c3c1477e2113f6758d5ff
readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), id=fname, language='en') }

## Rolling two directories together into a single training corpus
train_dirs = Sys.glob('../C50/C50train/*')
file_list = NULL
labels_train = NULL
for(author in train_dirs) {
  author_name = substring(author, first=17)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  labels_train = append(labels_train, rep(author_name, length(files_to_add)))
}
all_txt = lapply(file_list, readerPlain)
mynames = file_list %>%
  { strsplit(., '/', fixed=TRUE) } %>%
  { lapply(., tail, n=2) } %>%
  { lapply(., paste0, collapse = '') } %>%
  unlist
names(all_txt) = mynames
names(all_txt) = sub('.txt', '', names(all_txt))



corpus_train = Corpus(VectorSource(all_txt))

corpus_train = corpus_train %>% 
  tm_map(., content_transformer(tolower)) %>% 
  tm_map(., content_transformer(removeNumbers)) %>% 
  tm_map(., content_transformer(removeNumbers)) %>% 
  tm_map(., content_transformer(removePunctuation)) %>%
  tm_map(., content_transformer(stripWhitespace)) %>%
  tm_map(., content_transformer(removeWords), stopwords("SMART"))





## Same operations with the testing corpus
test_dirs = Sys.glob('../C50/C50test/*')
file_list = NULL
labels_test = NULL
for(author in test_dirs) {
  author_name = substring(author, first=16)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  labels_test = append(labels_test, rep(author_name, length(files_to_add)))
}
all_txt = lapply(file_list, readerPlain) 
names(all_txt) = file_list
names(all_txt) = sub('.txt', '', names(all_txt))


corpus_test = Corpus(VectorSource(all_txt)) 

corpus_test = corpus_test %>% 
  tm_map(., content_transformer(tolower)) %>% 
  tm_map(., content_transformer(removeNumbers)) %>% 
  tm_map(., content_transformer(removePunctuation)) %>%
  tm_map(., content_transformer(stripWhitespace)) %>%
  tm_map(., content_transformer(removeWords), stopwords("SMART")) 


###########SKIP#################
# create training and testing feature matrices
DTM_train = DocumentTermMatrix(corpus_train)
DTM_train # some basic summary statistics


# restrict test-set vocabulary to the terms in DTM_train
DTM_test = DocumentTermMatrix(corpus_test,
                              control = list(dictionary=Terms(DTM_train)))

# outcome vector
y_train = 0 + {labels_train=='TheresePoletti'}
y_test = 0 + {labels_test=='TheresePoletti'}


# lasso logistic regression for document classification
logit1 = cv.gamlr(DTM_train, y_train, family='binomial', nfold=10)
coef(logit1, select='min') 
plot(coef(logit1))

yhat_test = predict(logit1, DTM_test, type='response')
xtabs(~ {yhat_test > 0.5} + y_test)
boxplot(as.numeric(yhat_test) ~ y_test)
##############SKIP##############




DTM_train = DocumentTermMatrix(corpus_train, control = list(weighting = weightTfIdf, bounds = list(global = c(1, Inf))))
DTM_train = removeSparseTerms(DTM_train, 0.95)
DTM_train
DTM_train = as.matrix(DTM_train)
DTM_train = as.data.frame(DTM_train)


DTM_test = DocumentTermMatrix(corpus_test, control=list( weighting = weightTfIdf, bounds = list(global = c(1, Inf))))
DTM_test = removeSparseTerms(DTM_test, 0.95)
DTM_test
DTM_test = as.matrix(DTM_test)
DTM_test = as.data.frame(DTM_test)



DTM_train = cbind(DTM_train,labels_train)
DTM_test = cbind(DTM_test,labels_test)
naive_bayes = naiveBayes(as.factor(labels_train) ~ DTM_test, data = DTM_train)
pred = predict(naive_bayes, DTM_test[,-ncol(DTM_test)])

results = data.frame(table(DTM_test$labels_test, pred))

results = as.matrix(table(DTM_test$labels_test, pred))
sum(DTM_test$labels_test == pred)

naive
# Model Accuracy
confusionMatrix(as.factor(DTM_test$labels_test), pred)$overall['Accuracy']









DTM_train = DocumentTermMatrix(corpus_train, control=list(weighting=weightTfIdf, bounds = list(global = c(5, Inf))))
DTM_train = removeSparseTerms(DTM_train, 0.90)
DTM_train
tfidf_DTM_train = weightTfIdf(DTM_train)



DTM_test = DocumentTermMatrix(corpus_test,control=list(weighting=weightTfIdf, bounds = list(global = c(5, Inf))))
DTM_test = removeSparseTerms(DTM_test, 0.90)
DTM_test

#Clustering of DTM Train



# the full set of cosine similarities
# two helper functions that use some linear algebra for the calculations
cosine_dtm_docs = function(dtm) {
  crossprod_simple_triplet_matrix(t(dtm))/(sqrt(col_sums(t(dtm)^2) %*% t(col_sums(t(dtm)^2))))
}

# use the function to compute pairwise cosine similarity for all documents
cosine_dtm_mat_train = cosine_dtm_docs(tfidf_DTM_train)


# looks like document 16 has the highest cosine similarity
sort(cosine_dtm_mat_train, decreasing=TRUE)

cosine_dist_mat = proxy::dist(as.matrix(cosine_dtm_mat_train), method='cosine')
tree_dtm_train = hclust(cosine_dist_mat)
plot(tree_dtm)
clust_cutree_train = cutree(tree_dtm_train, k=50)
table(clust_cutree_train)
which(clust_cutree == 3)
clust_train = as.matrix(clust_cutree_train)



#clustering of DTM Test
tfidf_DTM_test = weightTfIdf(DTM_test)

cosine_dtm_docs = function(tfidf_DTM_test) {
  crossprod_simple_triplet_matrix(t(tfidf_DTM_test))/(sqrt(col_sums(t(tfidf_DTM_test)^2) %*% t(col_sums(t(tfidf_DTM_test)^2))))
}

# use the function to compute pairwise cosine similarity for all documents
cosine_dtm_mat_test = cosine_dtm_docs(tfidf_DTM_test)


# looks like document 16 has the highest cosine similarity
sort(cosine_dtm_mat_test, decreasing=TRUE)

cosine_dist_mat_test = proxy::dist(as.matrix(cosine_dtm_mat_test), method='cosine')
tree_dtm_test = hclust(cosine_dist_mat_test)
plot(tree_dtm)
clust_cutree_test = cutree(tree_dtm_test, k=50)
table(clust_cutree_test)
which(clust_cutree == 3)
clust_test = as.matrix(clust_cutree_test)


DTM_train<-cbind(DTM_train,clust_train)
DTM_test<-cbind(DTM_test,clust_test)

DTM_train<-cbind(DTM_train,labels_train)
DTM_test<-cbind(DTM_test,labels_test)



clust_table <-data.frame(table(clust_test, clust_train))
sum(clust_table$clust_train == clust_table$clust_test)

#### How accurate was this model?
confusionMatrix(as.factor(clust_test), as.factor(clust_train))$overall['Accuracy']


X = as.matrix(DTM_train)


#running a pca on x 
pc_dtm_train = prcomp(X, scale=TRUE)

#PCA using the 11 chemical properties 
train_data = pc_wine$x[,1:11]
pcr1 = lm(y ~ winedata)

#plots fitted values of PCAs
plot1 = plot(fitted(pcr1), y)


