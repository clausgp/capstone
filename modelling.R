# modelling - ngrams for each file blogs/twitter/news are generated

library(text2vec)
library(stringi)
library(data.table)
library(dplyr)

library(pryr)
library(rbenchmark)

# setwd("Rwork\\coursera data science specialization\\capstone")

# blogs
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

# split into train and test
pct <- 0.9
set.seed(1234)
# blogs
sample_size <- floor(pct*length(blogs.file))
train_ind <- sample(seq_len(length(blogs.file)), sample_size)
blogs.train <- blogs.file[train_ind]
blogs.test <- blogs.file[-train_ind]
save(blogs.train, file="blogs_train.Rda")
save(blogs.test, file="blogs_test.Rda")
# twitter
sample_size <- floor(pct*length(twitter.file))
train_ind <- sample(seq_len(length(twitter.file)), sample_size)
twitter.train <- twitter.file[train_ind]
twitter.test <- twitter.file[-train_ind]
save(twitter.train, file="twitter_train.Rda")
save(twitter.test, file="twitter_test.Rda")
# news
sample_size <- floor(pct*length(news.file))
train_ind <- sample(seq_len(length(news.file)), sample_size)
news.train <- news.file[train_ind]
news.test <- news.file[-train_ind]
save(news.train, file="news_train.Rda")
save(news.test, file="news_test.Rda")

# terminate R/close open R again, and jump to this part

# ngrams are made 1 at a time, terminate and restart R when memory runs low

# text2vec
load("blogs_train.Rda")
blogs <- blogs.train %>%
    # stri_split_boundaries(type="sentence") %>% unlist() %>% 
    stri_replace_all("", regex="[^a-zA-Z ]") %>%
    stri_replace_all("", regex="\\b(http|www).+\\b") %>% 
    stri_replace_all(" ", regex=" +") %>%
    tolower() %>% stri_trim_both()
rm(blogs.train)
# manually change ngram, and b.*dt at each ngram generation. doc_counts>1 from ngram>=3
it <- itoken(blogs)
vocab <- create_vocabulary(it, ngram=c(5L, 5L))
vocab$vocab <- vocab$vocab[doc_counts>1]
b.qdt <- vocab$vocab
setorder(b.qdt, terms)

# twitter
load("twitter_train.Rda")
twit <- twitter.train %>%
    # stri_split_boundaries(type="sentence") %>% unlist() %>% 
    stri_replace_all("", regex="[^a-zA-Z ]") %>%
    stri_replace_all("", regex="\\b(http|www).+\\b") %>%
    stri_replace_all(" ", regex=" +") %>% 
    tolower() %>% stri_trim_both()
rm(twitter.train)
# manually change ngram, and t.*dt at each ngram generation. doc_counts>1 from ngram>=3
it <- itoken(twit)
vocab <- create_vocabulary(it, ngram=c(5L, 5L))
vocab$vocab <- vocab$vocab[doc_counts>1]
t.qdt <- vocab$vocab
setorder(t.qdt, terms)

# news
load("news_train.Rda")
news <- news.train %>%
    # stri_split_boundaries(type="sentence") %>% unlist() %>% 
    stri_replace_all("", regex="[^a-zA-Z ]") %>%
    stri_replace_all("", regex="\\b(http|www).+\\b") %>%
    stri_replace_all(" ", regex=" +") %>% 
    tolower() %>% stri_trim_both()
rm(news.train)
# manually change ngram, and n.*dt at each ngram generation. doc_counts>1 from ngram>=3
it <- itoken(news)
vocab <- create_vocabulary(it, ngram=c(5L, 5L))
vocab$vocab <- vocab$vocab[doc_counts>1]
n.qdt <- vocab$vocab
setorder(n.qdt, terms)

head(udt,10)
object_size(udt)

head(bdt,10)
object_size(bdt)

head(tdt,10)
object_size(tdt)

# blogs
save(b.udt, file="budt.Rda")
save(b.bdt, file="bbdt.Rda")
save(b.tdt, file="btdt.Rda")
save(b.fdt, file="bfdt.Rda")
save(b.qdt, file="bqdt.Rda")

# twitter
save(t.udt, file="tudt.Rda")
save(t.bdt, file="tbdt.Rda")
save(t.tdt, file="ttdt.Rda")
save(t.fdt, file="tfdt.Rda")
save(t.qdt, file="tqdt.Rda")

# news
save(n.udt, file="nudt.Rda")
save(n.bdt, file="nbdt.Rda")
save(n.tdt, file="ntdt.Rda")
save(n.fdt, file="nfdt.Rda")
save(n.qdt, file="nqdt.Rda")