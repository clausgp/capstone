# testing the trained model from the 10% test data

source("predict.R")

load("blogs_test.Rda")
load("twitter_test.Rda")
load("news_test.Rda")

# blogs.test
# blogs.test <- blogs.test[1:100]
b.len <- length(blogs.test)
blogs.df <- data.frame(text=blogs.test, last=rep("", b.len), pred=rep("", b.len),
                 s1=rep(FALSE, b.len), s3=rep(FALSE, b.len), s5=rep(FALSE, b.len),
                 stringsAsFactors = FALSE)
for (i in seq_len(b.len)){
    txt <- clean(blogs.df$text[i])
    txt.len <- length(txt)
    blogs.df$last[i] <- txt[txt.len]
    predicts <- predict.next(txt[-txt.len], nr=5)
    blogs.df$pred[i] <- predicts[1]
    blogs.df$s1[i] <- blogs.df$last[i] %in% predicts[1]
    blogs.df$s3[i] <- blogs.df$last[i] %in% predicts[1:3]
    blogs.df$s5[i] <- blogs.df$last[i] %in% predicts[1:5]
}
sum(blogs.df$s1)/b.len
sum(blogs.df$s3)/b.len
sum(blogs.df$s5)/b.len


# twitter.test
t.len <- length(twitter.test)
twit.df <- data.frame(text=twitter.test, last=rep("", t.len), pred=rep("", t.len),
                 s1=rep(FALSE, t.len), s3=rep(FALSE, t.len), s5=rep(FALSE, t.len),
                 stringsAsFactors = FALSE)
for (i in seq_len(t.len)){
    txt <- clean(twit.df$text[i])
    txt.len <- length(txt)
    twit.df$last[i] <- txt[txt.len]
    predicts <- predict.next(txt[-txt.len], nr=5)
    twit.df$pred[i] <- predicts[1]
    twit.df$s1[i] <- twit.df$last[i] %in% predicts[1]
    twit.df$s3[i] <- twit.df$last[i] %in% predicts[1:3]
    twit.df$s5[i] <- twit.df$last[i] %in% predicts[1:5]
}
sum(twit.df$s1)/t.len
sum(twit.df$s3)/t.len
sum(twit.df$s5)/t.len

# news.test
n.len <- length(news.test)
news.df <- data.frame(text=news.test, last=rep("", n.len), pred=rep("", n.len),
                      s1=rep(FALSE, n.len), s3=rep(FALSE, n.len), s5=rep(FALSE, n.len),
                      stringsAsFactors = FALSE)
for (i in seq_len(n.len)){
    txt <- clean(news.df$text[i])
    txt.len <- length(txt)
    news.df$last[i] <- txt[txt.len]
    predicts <- predict.next(txt[-txt.len], nr=5)
    news.df$pred[i] <- predicts[1]
    news.df$s1[i] <- news.df$last[i] %in% predicts[1]
    news.df$s3[i] <- news.df$last[i] %in% predicts[1:3]
    news.df$s5[i] <- news.df$last[i] %in% predicts[1:5]
}
sum(news.df$s1)/n.len
sum(news.df$s3)/n.len
sum(news.df$s5)/n.len

test.stats2 <- data.frame(type=c("blogs", "twitter", "news"),
                         test.size=c(b.len, t.len, n.len),
                         s1=c(sum(blogs.df$s1), sum(twit.df$s1), sum(news.df$s1)),
                         s3=c(sum(blogs.df$s3), sum(twit.df$s3), sum(news.df$s3)),
                         s5=c(sum(blogs.df$s5), sum(twit.df$s5), sum(news.df$s5)),
                         stringsAsFactors = FALSE)
test.stats2$s1.pct <- test.stats2$s1/test.stats2$test.size
test.stats2$s3.pct <- test.stats2$s3/test.stats2$test.size
test.stats2$s5.pct <- test.stats2$s5/test.stats2$test.size

save(test.stats2, file="test_stats2.Rda")