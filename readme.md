# Word suggestion

### Capstone project for the Johns Hopkins University data science specialization

Stupid backoff ngram model used to suggest next/current word in a sentence

Presentation : [rpubs.com/clausgp/wordsuggestion](http://rpubs.com/clausgp/wordsuggestion)  
Shiny-app : [khlavus.shinyapps.io/capstone](https://khlavus.shinyapps.io/capstone/)

testdata : [HC Corpora data](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip)

#### Modelling steps

* modelling.R - has to manually be changed when creating ngrams as memory allows
* ngrams.R - combine ngrams
* predict.R - stupid-backoff prediction model
* test.R - test

#### Other sources

* eda.R - milestone report, exploratory data analyzis
* app.R - shiny app
* cappres.Rpres - Rpresentation

Heavely usage of saved r datastructues not included, these has to be generated in the modelling steps.