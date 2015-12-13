__Performing sentimental analysis on twitter feeds for various airline carriers to determine the customer satifaction__

We generate a sentiment index for all customer tweets and compare them across all airline carriers.

Importing all required libraries
```{r}
library("twitteR")
library("plyr")
library("ROAuth")
library("ggplot2")
library("tm")
library("wordcloud")
```

Authenticating Twitter app credentials for consuming the twitter API
```{r}
setup_twitter_oauth(consumer_key = "j0fASC3m9cnBYlmxBjWYhiw6u",
                    consumer_secret = "QODywdBRD7YvYJIlxoObDqaOrgzmIyazyi5y2pbv9FRzoE5FvR",
                    access_token = "1135344139-fY2OaxZy4O9eWbWps0mhKuUYUmwitfb4DXpFIGH",
                    access_secret = "PyHheRVoehspvD85ycVDhRme3WA8WfRYrUPR9Kp6diRiV"
)
```

Importing the collection of positive and negative words for sentimental analysis
```{r}
pos = scan('positive-words.txt',what='character',comment.char=';')
neg = scan('negative-words.txt',what='character',comment.char=';')
```

Implementing the sentiment scoring function 
```{r}
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    # Cleaning up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = gsub("&amp", "", sentence)
    sentence = gsub("(RT|via)((?:\b\\W*@\\w+)+)", "", sentence)
    sentence = gsub("@\\w+", "", sentence)
    sentence = gsub("[[:punct:]]", "", sentence)
    sentence = gsub("[[:digit:]]", "", sentence)
    sentence = gsub("http\\w+", "", sentence)
    sentence = gsub("[ t]{2,}", "", sentence)
    sentence = gsub("^\\s+|\\s+$", "", sentence)
    sentence=str_replace_all(sentence,"[^[:graph:]]", " ") 
    
    # Converting the sentence to lower case
    sentence = tolower(sentence)
    
    # Splitting the sentence into words
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    
    # Excluding stop words that will not play a role in determing the sentiment of a tweets
    exc.words=c(stopwords("english"),stopwords("SMART"))
    check <- match(words,exc.words)
    exc.list <-!is.na(check)
    words <-words[!exc.list]
    
    # Comparing the sentence words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    #Returns the position of the matches term or NA
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    #Generating a score for based on the number of matches for positive and negative words in the text
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}
```

Consuming the Twitter API to get tweets related to various airline carriers. These tweets are then passed to the scoring function above to generate a sentiment score for all the tweets for the airline carrier.
```{r}
deltatweets = searchTwitter("#Delta", n=500) #Consuming the Twitter API for 'Delta Airways'
deltatweets.text = laply(deltatweets, function(t)t$getText()) #Extracting the text from the tweets
delta.analysis = score.sentiment(deltatweets.text,pos,neg) #Passing the tweet text to the scoring function
delta.analysis$airline='Delta'

americantweets = searchTwitter("#AmericanAir", n=500)
americantweets.text = laply(americantweets, function(t)t$getText())
american.analysis = score.sentiment(americantweets.text,pos,neg)
american.analysis$airline='American'

SouthwestAirtweets = searchTwitter("#SouthwestAir", n=500)
SouthwestAirtweets.text = laply(SouthwestAirtweets, function(t)t$getText())
SouthwestAir.analysis = score.sentiment(SouthwestAirtweets.text,pos,neg)
SouthwestAir.analysis$airline='SouthwestAir'

JetBluetweets = searchTwitter("#JetBlue", n=500)
JetBluetweets.text = laply(JetBluetweets, function(t)t$getText())
JetBlue.analysis = score.sentiment(JetBluetweets.text,pos,neg)
JetBlue.analysis$airline='JetBlue'

Unitedtweets = searchTwitter("#United", n=500)
Unitedtweets.text = laply(Unitedtweets, function(t)t$getText())
United.analysis = score.sentiment(Unitedtweets.text,pos,neg)
United.analysis$airline='United'

VirginAmericatweets = searchTwitter("#VirginAmerica", n=500)
VirginAmericatweets.text = laply(VirginAmericatweets, function(t)t$getText())
VirginAmerica.analysis = score.sentiment(VirginAmericatweets.text,pos,neg)
VirginAmerica.analysis$airline='VirginAmerica'

USAirwaystweets = searchTwitter("#USAirways", n=500)
USAirwaystweets.text = laply(USAirwaystweets, function(t)t$getText())
USAirways.analysis = score.sentiment(USAirwaystweets.text,pos,neg)
USAirways.analysis$airline='USAirways'

HawaiianAirtweets = searchTwitter("#HawaiianAir", n=500)
HawaiianAirtweets.text = laply(HawaiianAirtweets, function(t)t$getText())
HawaiianAir.analysis = score.sentiment(HawaiianAirtweets.text,pos,neg)
HawaiianAir.analysis$airline='HawaiianAir'

AlaskaAirtweets = searchTwitter("#AlaskaAir", n=500)
AlaskaAirtweets.text = laply(AlaskaAirtweets, function(t)t$getText())
AlaskaAir.analysis = score.sentiment(AlaskaAirtweets.text,pos,neg)
AlaskaAir.analysis$airline='AlaskaAir'
```

Binding all the analysis results of different carriers into a single variable for analysis and visualization purposes
```{r}
all.analysis = rbind(american.analysis, delta.analysis,SouthwestAir.analysis,AlaskaAir.analysis,
                     HawaiianAir.analysis, USAirways.analysis, VirginAmerica.analysis, United.analysis,
                     JetBlue.analysis)
```

Plotting the results to compare the sentiment index of various carriers
```{r}
g = ggplot(data=all.analysis, mapping=aes(x=score, fill=airline))
g = g + geom_bar(binwidth=1)
g = g + facet_grid(airline~.)
g
```

The above visual plots a score for tweets of each carrier. These scores fall within a range of -15 to +7 (approximately). Scores below 0 denote unhappy sentiments and scores above 0 denote happy sentiments. 
Airline carriers 'United' and 'JetBlue' shows a good proportion of customers being satisfied with their services whereas the plot for 'USAirways' shows a greater proportion of customers being unhappy.
These results cannot be coined as an accurate measure for determing the customer satisfaction as the analysis score can vary based on the number of tweets for each carrier.


__Determing a trend in the topics being discussed by customers for various airline carriers__
Here, we try to determine the trending topics being discussed by customers. This information can be vital for the airline industry as they can acquire a fair idea on the operations that need improvement within their business. As this document attempts to analyse flight delays, this query can be used to learn if customers are concerned about flight delays or no.

Concatenating the tweets of all airline carriers
```{r}
tweets <- c(deltatweets, americantweets, SouthwestAirtweets, JetBluetweets, Unitedtweets, 
            VirginAmericatweets, USAirwaystweets, HawaiianAirtweets, AlaskaAirtweets)
```

Extracting the text from the tweets for analysis
```{r}
tweets.text = laply(tweets,function(t)t$getText())
```

The text from the tweets has might consist of invalid characters that might cause the 'tm' library functions to crash and hence we cleanse the text by removing them.
```{r}
clean.text <- function(some_txt)
{
  some_txt = gsub("&amp", "", some_txt)
  some_txt = gsub("(RT|via)((?:\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  # define "tolower error handling" function
  
  exc.words=c(stopwords("english"),stopwords("SMART"),stopwords("catalan"))
  # exclude stop words
  check <- match(some_txt,exc.words)
  exc.list <-!is.na(check)
  some_txt <-some_txt[!exc.list]
  
  try.tolower = function(x) 
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y) 
  }
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
}
clean_text = clean.text(tweets.text)
```

This clean text is added to a Corpus that a structure for saving collections of text documents.
```{r}
tweet_corpus = Corpus(VectorSource(clean_text))
```

The Corpus is further transformed into a Term-document Matrix which describes the frequency of terms that occur in a collection of documents.
```{r}
tdm = TermDocumentMatrix(tweet_corpus, control = list(removePunctuation = TRUE,stopwords = c(stopwords("english"),stopwords("SMART"),stopwords("catalan")), removeNumbers = TRUE, tolower = TRUE))
```

Defining the term-document-matrix as a matrix
```{r}
m = as.matrix(tdm)
```

Sorting the words in descending order based on their occurance and creating the dataset for visualization.
```{r}
word_freqs = sort(rowSums(m), decreasing=TRUE)
dm = data.frame(word=names(word_freqs), freq=word_freqs) 
```

Visualzing the result in the form of a word cloud
```{r}
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
```

Based on the word cloud, it is quite clear that people are more concerned about in-flight service and experience more than flight delays. This inference is based on tweets from customers that generally expect better customer service from airline staff. Flight delay is a common phenomenon; which might be one of the reasons for less customer discussions or tweets around this topic.
