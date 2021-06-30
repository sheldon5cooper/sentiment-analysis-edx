#Case Study: Trump Tweets
##====================================##===========================================
#Problem Statement: 
#During the 2016 US presidential election, 
#then-candidate Donald J. Trump used his Twitter account as a way to communicate 
#with potential voters.  On August 6, 2016 Todd Vaziri tweeted 
#about Trump that "Every non-hyperbolic tweet is from iPhone (his staff). 
#Every hyperbolic tweet is from Android (from him)."  Data scientist David Robinson
#conducted an analysis to determine if data supported this assertion. 
#Here we go through David's analysis to learn some of the basics of text mining.
##=================================================##=============================
#libraries to uses
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
library(dslabs)
library(tidytext)
library(textdata)
#=============================================##==================================
#we use the data "Trump tweets" available in "dslabs" package.
set.seed(1)
data(trump_tweets)
head(trump_tweets)#taking a quick look at the dataset
names(trump_tweets)#taking a look at variable names
trump_tweets%>%select(text)%>% head #tweets are shown by "text" variable
trump_tweets%>%count(source)%>%arrange(desc(n))
#we watch the no. of tweet smade from different platforms
trump_tweets%>%
  extract(source,"source","Twitter for (.*)")%>%
  count(source)%>%arrange(desc(n))
#here we extracted using extract and regex tweets from group "twitter for (any platform)"
campaign_tweets<- trump_tweets%>%
  extract(source,"source","Twitter for (.*)") %>% #same function as above
  filter(source %in% c("Android","iPhone") & #taking in onlt tweets made by android of iPhone
           created_at >=ymd("2015-06-17") & #announcement of trump campaign
           created_at < ymd("2016-11-08")) %>% #date of election
  filter(!is_retweet) %>% #filtering out retweets
  arrange(created_at)
head(campaign_tweets) #examing campaign_tweets dataset
ds_theme_set()
?count()
campaign_tweets%>%
  mutate(hour=hour(with_tz(created_at,"EST"))) %>% #hour column with EST Time Zone
  count(source,hour) %>% #counting by source and hour
  group_by(source) %>% #grouping by source
  mutate(percent=n/sum(n))%>%
  ungroup()%>%
  ggplot(aes(hour,percent,color=source))+
  geom_line()+
  geom_point()+
  scale_y_continuous(labels = percent_format())+
  labs(x="Hour of the Day(EST)",
       y="% of the tweets",
       color="")
#We see a clear time difference in the way tweets from different phones are shared from.
##=================================##========================================##========
##Now performing a text sentiment analysis from available tweets
#taking a quick example to understand the function unnest_tokens()
i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word) #as we see that unnest_tokens() does work but doesn't do it properly
#due to special characters in twitter such as "#" and "@".
#so we'll create a REGEX pattern to better the tokens
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
#The pattern appears complex but all we are defining is a patter that 
#starts with @, # or neither and is followed by 
#any combination of letters or digits:
campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)#we use unnest_tokens() now with REGEX pattern

campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>% 
  #we use this step to remove the links to pictures in the tweet
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)
tweet_words <- campaign_tweets %>% #now we extract all the words in all the tweets
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) 
head(tweet_words)
tweet_words %>% 
  count(word) %>%
  arrange(desc(n)) #we count which are the most used words

#as we see a lot of these words are just commonly used words,
#tidytext package has a special database consisting of these words known as
# "stop_Words"
#so we filter out these words from text tokens
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word )

tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n)) #we end with a informative set of top 10 used words

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  #removing the pictures link
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  #unnesting words(tokens) based on the pattern
  filter(!word %in% stop_words$word & #removing words in stop_words
           !str_detect(word, "^\\d+$")) %>% #removing words that are just years
  mutate(word = str_replace(word, "^'", "")) #removing (') from words string that start as a quote(')
tweet_words

android_iphone_or <- tweet_words %>%
  count(word, source) %>% #counting the no of times a word appaears in tweets from each source
  spread(source, n, fill = 0) %>% #spreading the source coulumn into two columns with "Android" and "Iphone" with "n" denoting the no. of times the word comes in tweets from each device.
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
#creating the column "or" to denote an odds ratio to show how likely a words is to
#be tweeted from "Android" or "iPhone", the bigger the answer denotes more instances of Android,
#the smaller the answer denotes the higher likeliness of iPhone being used.
# "0.5" is added there to prevent from zero proportions as there are "0" values in the columns.
android_iphone_or %>% arrange(desc(or)) #shows the bigger values, words tweeted most from Android
android_iphone_or %>% arrange(or) #shows the smaller values, words tweeted most from iPhone.
android_iphone_or
android_iphone_or<- android_iphone_or %>% filter(Android+iPhone > 100)
#removing which are very low frequency in total appearance in tweets from both devices

android_iphone_or%>% arrange(desc(or))
android_iphone_or%>%arrange(or)
 #we can already see a particular trend in the kind of words most used in android tweets and iPhone tweets

#but we are looking for more hyperbolic statements from Android than iPhone in 
#particular, so now we perform Sentiment Analysis.

##=========================================###==========================================
##=========================================###==========================================
#Sentiment Analysis
#The first step in sentiment analysis is to assign a sentiment to each word.
#first we learn about certain lexicons used in Tidytext package to assign sentiments to words
##1: "Bing" Lexicon divides words into positive or negative.
get_sentiments("bing")
##2: "AFINN" Lexicon asssigns a no. between -5 and 5, 5 being most positive and -5 being most negative.
get_sentiments("afinn")
##3: The "loughran" and "nrc" lexicons provide several different sentiments:
get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)
###======================================##================================================
#For the analysis here we are interested in exploring the different sentiments of each tweet, 
#so we will use the nrc lexicon:

nrc <- get_sentiments("nrc") %>%
  select(word, sentiment)
nrc #getting words and sentiments available in "nrc"

#We can combine the words and sentiments using inner_join(), 
#which will only keep words associated with a sentiment

tweet_words %>% inner_join(nrc, by = "word") %>% 
  select(source, word, sentiment)%>%sample_n(10)

#now we see count and compare the frequencies of each sentiment appears for each device.


sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>% #joins the sentiment column with sentiment for words that have a sentiment attached to them, and "na" for words with no sentiment
  count(source, sentiment) %>% #count the no. of times a sentiment shows up based on the device
  spread(source, n) %>% #dividing up the source column into different column for each device, with the value of no. of time a sentiment appears in them
  mutate(sentiment = replace_na(sentiment, replace = "none")) #changing the sentiment column to show "none" (no sentiments) for the words which had no sentiments.

sentiment_counts

#for each sentiment we can compute the odds of being in the device: 
#proportion of words with sentiment versus proportion of words without and 
#then compute the odds ratio comparing the two devices:

sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android) , #proportion of no. of words with a particular sentiment divided by words without that sentiment in Android
         iPhone = iPhone / (sum(iPhone) - iPhone),  #proportion of no. of words with a particular sentiment divided by words without that sentiment in iPhone
         or = Android/iPhone) %>% #ratio of proportion of ANdroid to Iphone
  arrange(desc(or)) #basically the bigger the ratio, the more the sentiment is represented in Android Tweet


log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          #we create the same "or" object as earlier but this time we take the log value of it
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          #then we take the "se"(Standard error) of it
          conf.low = log_or - qnorm(0.975)*se, #lower confidence interval
          conf.high = log_or + qnorm(0.975)*se) %>% #higher confidence interval
  arrange(desc(log_or)) #arranging based on log value

log_or

log_or %>%
  mutate(sentiment = reorder(sentiment, log_or),) %>% #reordering sentiment column based on log_or value
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
#creating gg plot visualization with setting conf.low and conf.high as lower and upper y limits
    geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip() 

android_iphone_or

android_iphone_or %>% inner_join(nrc) %>% #using inner join to assign sentiment to words in android_iPhone_or
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
  arrange(desc(or))

android_iphone_or %>% inner_join(nrc, by = "word") %>% #using inner_join to add a sentiment column to android_iPhone_or
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  #using factors function on sentiment column 
  mutate(log_or = log(or)) %>%
  #adding log of or as a column
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  #keeping only words which are used more than 10 times and have and absolute log_or value of more than 1
  mutate(word = reorder(word, log_or)) %>%
  #reordering words based on log_or
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  #using facet wrap to create different bar graphs based on different sentiment
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
##=================================================###========================================
#Project End
