# Check these links: http://cran.r-project.org/web/packages/tm/vignettes/tm.pdf, http://onepager.togaware.com/TextMiningO.pdf

#Install packages ----

# Uncoment lines to install packages

#install.packages("wordcloud")
#install.packages("tm")
#install.packages("RColorBrewer")
#install.packages("SnowballC")
#install.packages("ggplot2")

# RGraphviz
#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")

# Load libraries ----
library("tm")
library("SnowballC")
library("Rgraphviz")
library("wordcloud")
library("RColorBrewer")
library("ggplot2")


# Import data file ----
capa  <- read.table("categorizacao.csv", header = T, fill=TRUE, sep=",")
capa  <- capa[,-c(3,4)]

# Weak points ----

# Define Corpus
corpusWeak  <- VCorpus(VectorSource(capa$pontosfrageis))

# Remove blank spaces, transforming to Lower, remove punctuation, remove stopwords and stem doc.
corpusWeak  <- tm_map(corpusWeak, content_transformer(stripWhitespace))
corpusWeak  <- tm_map(corpusWeak, content_transformer(tolower))
corpusWeak  <- tm_map(corpusWeak, content_transformer(removePunctuation))
corpusWeak  <- tm_map(corpusWeak, content_transformer(removeNumbers))
corpusWeak  <- tm_map(corpusWeak, removeWords, stopwords("portuguese"))
corpusWeak  <- tm_map(corpusWeak, stemDocument, "portuguese")

# Inspect corpusFrageis
dtmWeak <- TermDocumentMatrix(corpusWeak)

# Find frequent words
findFreqTerms(dtmWeak, lowfreq =200)
freqWords  <- findFreqTerms(dtmWeak, lowfreq =200)

# Find more correlated words
findAssocs(dtmWeak, "drog", corlimit=0.2)

# plot
graph.par(list(edges=list(fontsize=16, texCol="darkred")))
plot(dtmWeak, terms = findFreqTerms(dtmWeak, lowfreq= 150), corThreshold=.3)

# Clustering ---- 

dtm2Weak <- removeSparseTerms(dtmWeak, sparse=0.90)

# convert the sparse term-document matrix to a standard data frame
mydata.df <- as.data.frame(inspect(dtm2Weak))

# Scale
mydata.df.scale <- scale(mydata.df)
d <- dist(mydata.df.scale, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
plot(fit, main="") # display dendogram?

groups <- cutree(fit, k=10) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=10, border="red")

# Create wordcloud
wordcloud(corpusWeak, random.order = F, min.freq = 50, colors = brewer.pal(5, "Dark2"))


# Strenghts ----

#definindo corpus
corpusStrong  <- VCorpus(VectorSource(capa$pontosfortes))

#removendo espaços em branco, formatando as letras em minúsculas e removendo a pontuação
corpusStrong  <- tm_map(corpusStrong, content_transformer(stripWhitespace))
corpusStrong  <- tm_map(corpusStrong, content_transformer(tolower))
corpusStrong  <- tm_map(corpusStrong, content_transformer(removePunctuation))
corpusStrong  <- tm_map(corpusStrong, content_transformer(removeNumbers))
corpusStrong  <- tm_map(corpusStrong, removeWords, stopwords("portuguese"))
corpusStrong  <- tm_map(corpusStrong, stemDocument, "portuguese")

# Inspect corpusFortes
dtmStrong <- TermDocumentMatrix(corpusStrong)


# Find frequent words
findFreqTerms(dtmStrong, lowfreq =250)

# Find more correlated words
findAssocs(dtmStrong, "drog", corlimit=0.2)

# plot
graph.par(list(edges=list(fontsize=16, texCol="darkred")))
plot(dtmStrong, terms = findFreqTerms(dtmStrong, lowfreq= 150), corThreshold=.3)

# Clustering ---- 

dtm2Strong <- removeSparseTerms(dtmStrong, sparse=0.90)

# convert the sparse term-document matrix to a standard data frame
mydata.df <- as.data.frame(inspect(dtm2Strong))

# Scale
mydata.df.scale <- scale(mydata.df)
d <- dist(mydata.df.scale, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
plot(fit, main="") # display dendogram?

groups <- cutree(fit, k=10) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=10, border="red")

# Create wordcloud
wordcloud(corpusStrong, random.order = F, min.freq = 50, colors = brewer.pal(5, "Dark2"))


