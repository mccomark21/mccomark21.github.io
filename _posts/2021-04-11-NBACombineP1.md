---
layout: post
title: "NBA Draft Combine Analysis"
subtitle: "Collecting, Cleaning, and Imputing Data"
background: '/img/posts/NBA_Combine/NBA_Combine_Background.jpg'
---

## Introduction

The NBA draft Combine annually collects data pertaining to potential NBA
prospects. The data collected focuses on measurements of a player’s
height, weight, wingspan, and many different measurements of
athleticism. Within this post, we will collect, clean, adn impute data
regarding the history of the NBA Combine dating back to the year 2000.

To get started, we’ll first need to load our necessary libraries which
will help with our analysis.

``` r
library(tidyverse)
library(plotly)
library(nbastatR)
library(stats)
library(VIM)
library(naniar)
library(visdat)
library(UpSetR) 
```

## Data Collection: nbastatR

The primary library we’ll be using in this analysis is called “nbastatR”
(further documentation of the package can be found here). The developers
of this library created a collection of functions that interact with the
NBA API. For our purposes, we’ll be using the “draft\_combines”
function. The two lines of code below will generate a data frame
containing NBA draft combine data dating back to the year 2000.

``` r
# Generate today's date for "years" parameter
CD <- as.numeric(format(as.Date(Sys.Date(), format="%Y-%m-%d"),"%Y"))+1 # current date

# Execute function to collect draft combine data to date
Combine_df <- draft_combines(years = 2000:CD) # Select range of draft years desired

print(head(Combine_df,10))
```

## Data Cleaning

Although we got off to a quick start thanks to the developers from
NBAstatR it’s still important that we evaluate the cleanliness of our
data. We need to examine the size of our data frame as well as the
information contained within it.

There are 116 columns/features and 1,395 records/rows within this data
frame. Using the “vis\_miss” function from the “visdat” library I can
see that 73.2% of my data frame is missing data. Unfortunately, this is
a common flaw in many data sets so it’s up to us to clean it.To trim
down this sparse data set we will only keep columns that have less than
20% of missing values. This will bring our data frame down to 18
columns/features.

``` r
print(dim(Combine_df))
```

    ## [1] 1395  116

``` r
vis_miss(Combine_df)
```
<img src="/img/posts/NBA_Combine/vis_miss_1.png" alt="Missing Values">

``` r
# Missing data as a percentage per column
missing_df <- data.frame(colMeans(is.na(Combine_df)))
colnames(missing_df) <- "percent_missing"

# Exclude any columns that are missing more than 20% of values
missing_df <- cbind(ColNames = rownames(missing_df), missing_df)
rownames(missing_df) <- 1:nrow(missing_df)

missing_df2 <- missing_df %>% 
  select(ColNames,percent_missing) %>% 
  filter(percent_missing < 0.2)
         
keeps <- as.character(unique(missing_df2$ColNames))

Combine_df <- Combine_df[keeps]

vis_miss(Combine_df)
```

![](img/posts/NBA_Combine/vis_miss_2.png)<!-- -->

## Data Imputation

Next, there are still a number of records which contain missing data. In
these cases a prospect may decide not to participate in specific
workouts or measurements due to the impact it may have on their draft
value. We’ll use the “gg\_miss\_upset” function from the “naniar”
library to visualize where our missing data resides. Just from a quick
glance we can see 202 of our missing values stem from the “pctBodyFat”
column. Additionally, these specific records are only missing data in
this specific column. We’ll create a column that counts how many columns
of missing data there are per record. Then we’ll filter for only records
that are missing at most 1 column of data.

Now, we’ve entered a pivotal point in the data cleaning process. We’ve
already dropped a number of columns that didn’t have any data. There are
data scientists who might simply exclude any records with missing values
but in our situation we are already limited to just over 1,000 records.
Sadly, that’s just not enough samples for us to truly gauge a
distribution. Personally, I highly value the information in each of
these records and made the decision to impute measurements where
possible. In the case of this data set, there were roughly 210 records
that I found reasonable to impute. I utilized the “knn” function from
the “VIM” library. In this method, an aggregation of the nearest
neighbors is used to impute the missing value. Below you can see I’ve
set k equal to 5 which means I’m looking at the 5 closest data points to
assist in the imputation.

``` r
gg_miss_upset(Combine_df)
```

![](img/posts/NBA_Combine/gg_miss_upset.png)<!-- -->

``` r
Combine_df$na_count <- apply(Combine_df, 1, function(x) sum(is.na(x)))

Combine_df <- Combine_df %>%
  filter(na_count<2)

# Impute missing data using knn
Combine_df <- kNN(Combine_df, k = 5)

# Exclude impute label columns
Combine_df <- select(Combine_df, -contains("_imp"))
```

## Next Steps

Now that we have a nice clean data frame we can start to consider where
we’ll go next with this project. I have a few ideas in mind and will
share that work in a follow up post. Thanks for reading\!

``` r
print(head(Combine_df,10))
```

    ##    yearCombine idPlayer nameFirst nameLast     namePlayer slugPosition
    ## 1         2001     2124     Malik    Allen    Malik Allen         PF-C
    ## 2         2001    12020    Lamont   Barnes  Lamont Barnes         PF-C
    ## 3         2001    12131     Mario    Bland    Mario Bland           PF
    ## 4         2001     2056    Primoz   Brezec  Primoz Brezec            C
    ## 5         2001     2049    Speedy  Claxton Speedy Claxton           PG
    ## 6         2001    12132      Eric    Coley     Eric Coley        SG-SF
    ## 7         2001    12133        Ed     Cota        Ed Cota           PG
    ## 8         2001    12134     Schea   Cotton   Schea Cotton           SF
    ## 9         2001    12017   Caswell    Cyrus  Caswell Cyrus           PF
    ## 10        2001     2064    Khalid  El-Amin Khalid El-Amin           PG
    ##    heightWOShoesInches heightWOShoes weightLBS wingspanInches wingspan
    ## 1                80.25     6' 8.25''     271.0           86.5 7' 2.5''
    ## 2                80.50      6' 8.5''     235.5           87.5 7' 3.5''
    ## 3                77.50      6' 5.5''     287.0           84.0   7' 0''
    ## 4                84.75     7' 0.75''     243.0           86.0   7' 2''
    ## 5                70.50     5' 10.5''     166.0           72.0   6' 0''
    ## 6                76.00        6' 4''     205.0           83.0  6' 11''
    ## 7                72.25     6' 0.25''     189.0           76.0   6' 4''
    ## 8                76.25     6' 4.25''     219.5           79.0   6' 7''
    ## 9                79.75     6' 7.75''     220.5           86.0   7' 2''
    ## 10               69.00        5' 9''     200.0           73.5 6' 1.5''
    ##    reachStandingInches reachStandingO verticalLeapStandingInches
    ## 1                109.0         9' 1''                       25.5
    ## 2                108.0         9' 0''                       28.0
    ## 3                103.0         8' 7''                       27.0
    ## 4                110.0         9' 2''                       26.0
    ## 5                 94.5      7' 10.5''                       36.0
    ## 6                102.0         8' 6''                       35.0
    ## 7                 96.5       8' 0.5''                       26.5
    ## 8                100.0         8' 4''                       34.0
    ## 9                107.5      8' 11.5''                       33.5
    ## 10                93.0         7' 9''                       27.5
    ##    verticalLeapMaxInches timeLaneAgility timeThreeQuarterCourtSprint pctBodyFat
    ## 1                   29.0           11.83                        3.38       10.5
    ## 2                   29.5           12.30                        3.40        8.0
    ## 3                   31.0           13.04                        3.47       10.5
    ## 4                   29.5           11.53                        3.55       10.2
    ## 5                   42.5           10.48                        3.06        5.9
    ## 6                   38.0           11.40                        3.15        4.5
    ## 7                   33.5           10.98                        3.24       11.3
    ## 8                   38.0           11.55                        3.00        7.2
    ## 9                   37.5           12.12                        3.16        6.0
    ## 10                  32.0           10.59                        3.32        7.3
    ##    na_count
    ## 1         1
    ## 2         1
    ## 3         1
    ## 4         1
    ## 5         1
    ## 6         1
    ## 7         1
    ## 8         1
    ## 9         1
    ## 10        1
