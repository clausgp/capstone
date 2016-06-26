# modelling

library(quanteda)
library(text2vec)
library(stringi)
library(data.table)
library(dplyr)

library(pryr)
library(rbenchmark)

setwd("Rwork\\coursera data science specialization\\capstone")

# only using blogs for now
blogs.file <- readLines("final/en_US/en_US.blogs.txt", encoding="UTF-8")
blogs.file <- blogs.file[stri_count_words(blogs.file)>=5]
blogs.file <- iconv(blogs.file, from="UTF-8", to="ASCII", sub="")

# twitter
twitter.file <- readLines("final/en_US/en_US.twitter.txt", skipNul=TRUE, encoding="UTF-8")
twitter.file <- twitter.file[stri_count_words(twitter.file)>=5]
twitter.file <- iconv(twitter.file, from="UTF-8", to="ASCII", sub="")

# news
tmp <- file("final/en_US/en_US.news.txt", open="rb")
news.file <- readLines(tmp, encoding="UTF-8")
close(tmp)
news.file <- news.file[stri_count_words(news.file)>=5]
news.file <- iconv(news.file, from="UTF-8", to="ASCII", sub="")

pct <- 0.5
set.seed(1234)
blogs <- sample(blogs.file, pct*length(blogs.file))

blogs.corp <- corpus(blogs)

# unigram
bdfm <- dfm(blogs.corp, ngrams=1, concatenator=" ")
nf <- bdfm@Dim[2]
bf <- topfeatures(bdfm, n=nf)
sf <- sum(bf)
udt <- data.table(f=attributes(bf)$names, n=as.integer(bf))
setkey(udt, f, n)
udt$pct <- udt$n/sf

# bigram
bdfm <- dfm(blogs.corp, ngrams=2, concatenator=" ")
nf <- bdfm@Dim[2]
bf <- topfeatures(bdfm, n=nf)
sf <- sum(bf)
bdt <- data.table(f=attributes(bf)$names, n=as.integer(bf))
bdt$first <- stri_extract_first(bdt$f, regex="^\\S+(?= )")
bdt$last <- stri_extract_last(bdt$f, regex="(?<= )\\S+$")
setkey(bdt, f, first, last, n)
bdt$s <- bdt$n/udt[bdt$first,n]

# trigram
bdfm <- dfm(blogs.corp, ngrams=3, concatenator=" ")
nf <- bdfm@Dim[2]
bf <- topfeatures(bdfm, n=nf)
sf <- sum(bf)
tdt <- data.table(f=attributes(bf)$names, n=as.integer(bf))
tdt$first <- stri_extract_first(tdt$f, regex="^\\S+ \\S+(?= )")
tdt$last <- stri_extract_last(tdt$f, regex="(?<= )\\S+$")
setkey(tdt, f, first, last, n)
tdt$s <- tdt$n/bdt[tdt$first, n]

# rewrite this!!
# testing
tdt.test <- tdt[1:100]
benchmark(t1 <- tdt.test$n/sapply(tdt.test$first, function(x) bdt$n[bdt$f==x]), t2 <- tdt.test[,n]/bdt[tdt.test[,first],n])
tdt$s <- tdt$n/sapply(tdt$first, function(x) bdt$n[bdt$f==x])




# text2vec
# tokenize from quanteda to clean before 
blogs.q <- tokenize(blogs, removeNumbers=TRUE, removePunct=TRUE, removeSymbols=TRUE, removeTwitter=TRUE, removeHyphens=TRUE)
it <- itoken(blogs.q, 
             preprocess_function = tolower, 
             tokenizer = word_tokenizer)
vocab <- create_vocabulary(it, ngram=c(2L, 3L))
vocab.dt <- data.table(vocab$vocab)
# setkey(vocab.dt, terms_counts)
setorder(vocab.dt, -terms_counts)


vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(it, vectorizer)

# try 2
#blogs.sent <- unlist(stri_split_boundaries(blogs, type="sentence", simplify=TRUE))
blogs.sent <- blogs.file %>%
    stri_split_boundaries(type="sentence") %>% unlist() %>% 
    stri_replace_all("", regex="[^a-zA-Z ]") %>%
    stri_replace_all(" ", regex=" +") %>% 
    tolower() %>% stri_trim_right()
it <- itoken(blogs.sent)
vocab <- create_vocabulary(it, ngram=c(1L, 1L))
b.udt <- vocab$vocab
setorder(b.udt, -terms_counts)

vocab$vocab <- vocab$vocab[doc_counts>1]
vocab$vocab[1:30]

vocab.dt <- vocab$vocab[doc_counts>4]

# twitter
twit.sent <- twitter.file %>%
    stri_split_boundaries(type="sentence") %>% unlist() %>% 
    stri_replace_all("", regex="[^a-zA-Z ]") %>%
    stri_replace_all(" ", regex=" +") %>% 
    tolower() %>% stri_trim_right()
it <- itoken(twit.sent)
vocab <- create_vocabulary(it, ngram=c(3L, 3L))
t.tdt <- vocab$vocab
setorder(t.tdt, -terms_counts)

# news
news.sent <- news.file %>%
    stri_split_boundaries(type="sentence") %>% unlist() %>% 
    stri_replace_all("", regex="[^a-zA-Z ]") %>%
    stri_replace_all(" ", regex=" +") %>% 
    tolower() %>% stri_trim_right()
it <- itoken(news.sent)
vocab <- create_vocabulary(it, ngram=c(3L, 3L))
n.tdt <- vocab$vocab
setorder(n.tdt, -terms_counts)



head(udt,10)
object_size(udt)

head(bdt,10)
object_size(bdt)

head(tdt,10)
object_size(tdt)

# bf <- topfeatures(bdfm, n=bdfm@Dim[2])
# dbf <- data.frame(f=attributes(bf), n=bf, row.names=NULL)
# dbf <- data.table(f=attributes(bf)$names, n=bf)
# dbf$first <- stri_extract_first(dbf$f, regex="^\\S+ \\S+(?= )")
# dbf$last <- stri_extract_last(dbf$f, regex="(?<= )\\S+$")

# blogs
save(b.udt, file="budt.Rda")
save(b.bdt, file="bbdt.Rda")
save(b.tdt, file="btdt.Rda")

# twitter
save(t.udt, file="tudt.Rda")
save(t.bdt, file="tbdt.Rda")
save(t.tdt, file="ttdt.Rda")

# news
save(n.udt, file="nudt.Rda")
save(n.bdt, file="nbdt.Rda")
save(n.tdt, file="ntdt.Rda")
