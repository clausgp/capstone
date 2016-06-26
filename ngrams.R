
library(dplyr)
library(data.table)
library(stringi)
library(pryr)

# join uni-grams

load("budt.Rda")
load("nudt.Rda")
load("tudt.Rda")

udt <- b.udt %>% rbind(n.udt) %>% rbind(t.udt) %>% select(terms, terms_counts) %>% group_by(terms) %>%
    summarise(terms_counts = sum(terms_counts))
nu.terms <- sum(udt$terms_counts)
setorder(udt, -terms_counts)

udt <- udt[terms_counts>2]

udt$p <- udt$terms_counts/nu.terms
udt$logp <- log(udt$p)

setorder(udt, terms)

save(udt, file="udt.Rda")


# join bi-grams

load("bbdt.Rda")
load("nbdt.Rda")
load("tbdt.Rda")

bdt <- b.bdt %>% rbind(n.bdt) %>%
#    rbind(t.bdt) %>%
    select(terms, terms_counts) %>% group_by(terms) %>%
    summarise(terms_counts = sum(terms_counts))
nb.terms <- sum(bdt$terms_counts)
setorder(bdt, -terms_counts)

bdt <- bdt[terms_counts>2]

bdt$p <- bdt$terms_counts/nb.terms
bdt$logp <- log(bdt$p)

setorder(bdt, terms)

save(bdt, file="bdt.Rda")

# join 3 grams

load("btdt.Rda")
load("ntdt.Rda")
load("ttdt.Rda")

tdt <- b.tdt %>% rbind(n.tdt) %>%
    #    rbind(t.tdt) %>%
    select(terms, terms_counts) %>% group_by(terms) %>%
    summarise(terms_counts = sum(terms_counts))
nt.terms <- sum(tdt$terms_counts)
setorder(tdt, -terms_counts)

tdt <- tdt[terms_counts>2]

tdt$first <- stri_extract_first(tdt$terms, regex="^\\S+_\\S+(?=_)")
tdt$last <- stri_extract_last(tdt$terms, regex="(?<=_)\\S+$")

tdt$p <- tdt$terms_counts/nt.terms
tdt$logp <- log(tdt$p)

setorder(tdt, terms)

save(tdt, file="tdt.Rda")