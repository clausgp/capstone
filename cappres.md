<style>
.small-txt {
  font-size: 1em;
}
</style>

Word suggestion
========================================================
author: Claus Gaarde Pedersen
date: Juli 2016
autosize: true
transition: zoom
transition-speed: slow
font-family: "Lato"

Capstone project for the Johns Hopkins University Data science specialization.

Introduction
========================================================
type: section
navigation: section
<small>In this project natural language processing will be used to build a model that can predict what the next word in a sentence will likely be.  
For this an app is build, that tries to mimick what the Swiftkey app we know from mobile phones are doing. When you are typing in text in a textbox it will suggest what word to write.</small>

<small>To do this not only will the next word be suggested, but also the current word being typed will be suggested.</small>

<small>For fun the app is also using the model to generate random text. Maybe you can use it for your next twitter message?</small>

<small>If possible try to use the app from a tablet or laptop with touchscreen.</small>

Algorithm
========================================================
type: section
<small>The model build is a N-gram model with stupid backoff.  
The model functions by looking up the longest ngram to see if the last n-1 words exists. If it does the most likely of the next word is used. If not it will backoff to the next lower ngram and look up there, until it reaches 1-gram where it takes the most likely from there.  
In my implementaion 1-grams to 8-grams are created from 90% of the supplied blogs, twitter and news documents.  
10% of the supplied documents are used to test the models predictions.</small>

<small>I have implemented the algorithm with a practical approach focussing mostly on ranking the mostly likely prediction instead of a prober statistical distribution. The app dosnt do any calculations it just looks up the suggested words so its fast. It will take some time to load because it includes around 150 mb of model-data, so its not really 'mobile-phone-ready'  
Stats for the ngram model and test-results can be seen in the apps notes.</small>

Description of shiny app
========================================================
type: section

shiny-app : [https://khlavus.shinyapps.io/capstone/](https://khlavus.shinyapps.io/capstone/)
<small>In the App panel you can have your try at the app.
When you type in text in the text box the app automaticly decide if it should suggest the next word or the current word being typed.  
This decision is done by checking if the last letter is a space, if so 5 words will be suggested as the next word. If the last letter is not a space up to 5 words will be suggested as the word currently being typed.  
I both cases buttoms will be shown under the text box, with the leftmost being the most likely.</small>

<small>In the model-prose panel, you can use the model to generate prose.  
Just click the buttons and drop your jaw in befuddlement.  
Be warned there is no profanity filter in the model as i deliberately left that out.</small>

Ressources
========================================================
type: prompt
<small>
The model have been built with R in [Rstudio](http:www.rstudio.com).  
Source can be found on [github.com/clausgp](https://github.com/clausgp/capstone)

Packages used :  
 - text2vec
 - stringi
 - data.table
 - dplyr
 - shiny
 - shinyjs
 - knitr
 - pryr
 - rbenchmark</small>
