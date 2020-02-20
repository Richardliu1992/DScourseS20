---
title: "Question3"
author: "Ruichun Liu"
date: "2/18/2020"
output: html_document
---

```{r}
# Load packages
library(rvest)
library(stringr)
library(dplyr)
library(lubridate)
library(readr)
# Read web page
# Here I just scraped data on page1, top51-top100. I need to learn how to scrape data for multiple pages.
webpage <- read_html("https://www.rollingstone.com/music/music-lists/100-greatest-country-songs-of-all-time-11200/100-brad-paisley-welcome-to-the-future-2009-214233/")
# Extract records info
results <- webpage %>% html_nodes(".c-list__header")

# Building the dataset

records <- vector("list", length = length(results))
for (i in seq_along(results)) {

    rank <- results[i] %>% html_nodes("span") %>% html_text(trim = TRUE)
    
    detail <- xml_contents(results[i])[3] %>% html_text(trim = TRUE)

    records[[i]] <- data_frame(rank = rank, detail = detail)
}
df <- bind_rows(records)

# Export to csv
write_csv(df, "countrymusics.csv")
```

#4 Worked with GAO
install.packages("twitteR")
library(twitteR)
requestURL = "https://api.twitter.com/oauth/request_token"
accessURL = "https://api.twitter.com/oauth/access_token"
authURL = "https://api.twitter.com/oauth/authorize"
consumerKey = "I2j9G106bfnJt88rSjrteTRGj"
consumerSecret = "1gxYmadllk95mJXyId0zoGoyHB8QJdHxy0eUey8s6EeFIIdrd7"
accessToken = "1055216750562820099-AnnRWiuJYV400GgPTh8zvcs1Vvlhat"
accessSecret = "h5Gk3w3nfrG7tk9GzMPaoHArXt52piBLTtuaH9dKAR32z"
setup_twitter_oauth(consumerKey,
                    consumerSecret,
                    accessToken,
                    accessSecret)

tweets <- searchTwitter('#coronavirus',
                        geocode='35.2225685120,-97.4394760132,500mi', 
                        n=5000, retryOnRateLimit=1)

tweets.df <- twListToDF(tweets) 
View(tweets.df)
head(tweets.df$text)

length(which(tweets.df$isRetweet == TRUE))
