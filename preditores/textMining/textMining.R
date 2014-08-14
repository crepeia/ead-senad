# Check this links: http://cran.r-project.org/web/packages/tm/vignettes/tm.pdf, http://onepager.togaware.com/TextMiningO.pdf

#instalando os pacotes wordcloud, tm e RColorBrewer
#install.packages("wordcloud")
#install.packages("tm")
#install.packages("RColorBrewer")
#install.packages("SnowballC")

#carregando pacotes
library("wordcloud")
library("tm")
library("RColorBrewer")
library("SnowballC")

# OPCIONAL - Escolhendo o diretório de trabalho (setwd - Working Directory) - Descomente para utilizar
setwd("Área de Trabalho/")

#selecionando planilha com dados
capa  <- read.table("categorizacao.csv", header = T, fill=TRUE, sep=",")
capa  <- capa[,-c(3,4)]

# Pontos Frageis ----

#definindo corpus
corpusFrageis  <- VCorpus(VectorSource(capa$pontosfrageis))

#removendo espaços em branco, formatando as letras em minúsculas e removendo a pontuação
corpusFrageis  <- tm_map(corpusFrageis, content_transformer(stripWhitespace))
corpusFrageis  <- tm_map(corpusFrageis, content_transformer(tolower))
corpusFrageis  <- tm_map(corpusFrageis, content_transformer(removePunctuation))
corpusFrageis  <- tm_map(corpusFrageis, content_transformer(removeNumbers))
corpusFrageis  <- tm_map(corpusFrageis, removeWords, stopwords("portuguese"))
corpusFrageis  <- tm_map(corpusFrageis, stemDocument, "portuguese")

# Inspect corpusFrageis
dtm <- TermDocumentMatrix(corpusFrageis)

# Document Term Matrix
freq <- colSums(as.matrix(dtm))

# Check how many words were used
length(freq)

# Order the words in ocurrence
ord  <- order(freq)

# Least frequent items
freq[head(ord)]

# More frequent items
freq[tail(ord, 40)]


# Find frequent words
findFreqTerms(dtm, lowfreq= 400)

# Find more correlated words
findAssocs(dtm, "drog", corlimit=0.3)

# plot
plot(dtm, terms = findFreqTerms(dtm, lowfreq= 300), corThreshold=.2)



# Create wordcloud
wordcloud(corpusFrageis, random.order = F, colors = brewer.pal(5, "Dark2"))


# Pontos fortes ----

#definindo corpus
corpusFortes  <- VCorpus(VectorSource(capa$pontosfortes))

#removendo espaços em branco, formatando as letras em minúsculas e removendo a pontuação
corpusFortes  <- tm_map(corpusFortes, content_transformer(stripWhitespace))
corpusFortes  <- tm_map(corpusFortes, content_transformer(tolower))
corpusFortes  <- tm_map(corpusFortes, content_transformer(removePunctuation))
corpusFortes  <- tm_map(corpusFortes, content_transformer(removeNumbers))
corpusFortes  <- tm_map(corpusFortes, removeWords, stopwords("portuguese"))
corpusFortes  <- tm_map(corpusFortes, stemDocument, "portuguese")

# Inspect corpusFortes
dtm <- TermDocumentMatrix(corpusFortes)

# Document Term Matrix
freq <- colSums(as.matrix(dtm))

# Check how many words were used
length(freq)

# Order the words in ocurrence
ord  <- order(freq)

# Least frequent items
freq[head(ord)]

# More frequent items
freq[tail(ord, 40)]


# Find frequent words
findFreqTerms(dtm, lowfreq= 300)

# Find more correlated words
findAssocs(dtm, "ambient", corlimit=0.3)

# Create wordcloud
wordcloud(corpusFortes, random.order = F, colors = brewer.pal(5, "Dark2"))



