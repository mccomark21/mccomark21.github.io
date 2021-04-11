---
layout: post
title: "NBA Draft Combine Analysis - Part 1"
subtitle: "Collecting, Cleaning, and Imputing Data"
background: ''
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

print(Combine_df)
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

![](NBAstatR_Draft_Combine_Analysis_Part_1_files/figure-gfm/Identify%20and%20remove%20sparse%20columns-1.png)<!-- -->

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

![](NBAstatR_Draft_Combine_Analysis_Part_1_files/figure-gfm/Identify%20and%20remove%20sparse%20columns-2.png)<!-- -->
\#\# Data Imputation

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

![](NBAstatR_Draft_Combine_Analysis_Part_1_files/figure-gfm/Remove%20Sparse%20columns%20and%20Impute%20remaining%20missing%20values-1.png)<!-- -->

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
print(Combine_df)
```

    ##      yearCombine   idPlayer  nameFirst         nameLast
    ## 1           2001       2124      Malik            Allen
    ## 2           2001      12020     Lamont           Barnes
    ## 3           2001      12131      Mario            Bland
    ## 4           2001       2056     Primoz           Brezec
    ## 5           2001       2049     Speedy          Claxton
    ## 6           2001      12132       Eric            Coley
    ## 7           2001      12133         Ed             Cota
    ## 8           2001      12134      Schea           Cotton
    ## 9           2001      12017    Caswell            Cyrus
    ## 10          2001       2064     Khalid          El-Amin
    ## 11          2001      12018     Vassil          Evtimov
    ## 12          2001       2238    Antonis           Fotsis
    ## 13          2001       2109      Eddie             Gill
    ## 14          2001      12136     Marcus            Goree
    ## 15          2001      12137       A.J.          Granger
    ## 16          2001       2062       A.J.           Guyton
    ## 17          2001       2110        Ron             Hale
    ## 18          2001       2078      Jason             Hart
    ## 19          2001      12138     Johnny          Hemsley
    ## 20          2001      12139    Michael           Hermon
    ## 21          2001       2083       Cory        Hightower
    ## 22          2001       2067      Eddie            House
    ## 23          2001       2111     Jimmie           Hunter
    ## 24          2001      12141      Jacob           Jaacks
    ## 25          2001       2060      Marko            Jaric
    ## 26          2001      12142       Nate          Johnson
    ## 27          2001      12143     Kenyon            Jones
    ## 28          2001       2077       Mark          Karcher
    ## 29          2001       2061        Dan           Langhi
    ## 30          2001       2090     Justin             Love
    ## 31          2001       2058       Mark           Madsen
    ## 32          2001       2082        Dan       McClintock
    ## 33          2001       2087       Pete          Mickeal
    ## 34          2001      12145      Brian        Montonati
    ## 35          2001       2508       Gabe          Muoneke
    ## 36          2001      12146     Ndongo          N'diaye
    ## 37          2001       2059    Eduardo           Najera
    ## 38          2001      12147    Matthew          Nielsen
    ## 39          2001       2084      Chris           Porter
    ## 40          2001       2068      Lavor          Postell
    ## 41          2001       2080       Igor        Rakocevic
    ## 42          2001      12148       Reed         Rawlings
    ## 43          2001       2072    Michael             Redd
    ## 44          2001      12149      Damon             Reed
    ## 45          2001      12021     Aubrey            Reese
    ## 46          2001      12022 Julius Doc         Robinson
    ## 47          2001      12023       Pepe          Sanchez
    ## 48          2001      12024       Matt       Santangelo
    ## 49          2001       2637       Alex           Scales
    ## 50          2001      12025      Karim          Shabazz
    ## 51          2001      12026       Nick         Sheppard
    ## 52          2001       2065       Mike            Smith
    ## 53          2001       2074     Jabari            Smith
    ## 54          2001      12027    Jarrett         Stephens
    ## 55          2001      12028     Bootsy         Thornton
    ## 56          2001      12029     Jaquay            Walls
    ## 57          2001      12030     Jameel          Watkins
    ## 58          2002      12033       Adam       Allenspach
    ## 59          2002       2240    Gilbert           Arenas
    ## 60          2002       2220    Brandon        Armstrong
    ## 61          2002       2203      Shane          Battier
    ## 62          2002      12034     Cookie          Belcher
    ## 63          2002       2294    Charlie             Bell
    ## 64          2002       2257      Ruben  Boumtje-Boumtje
    ## 65          2002      12035     Calvin           Bowman
    ## 66          2002       2214    Michael          Bradley
    ## 67          2002       2249    Jamison           Brewer
    ## 68          2002       2198      Kwame            Brown
    ## 69          2002       2245     Damone            Brown
    ## 70          2002      12031 SirValiant            Brown
    ## 71          2002      12032       Ryan          Carroll
    ## 72          2002       2199      Tyson         Chandler
    ## 73          2002       2251       Eric        Chenowith
    ## 74          2002       2439        Sam           Clancy
    ## 75          2002      12036      Gyasi      Cline-Heard
    ## 76          2002       2260     Jarron          Collins
    ## 77          2002       2215      Jason          Collins
    ## 78          2002       2241       Omar             Cook
    ## 79          2002       2201       Eddy            Curry
    ## 80          2002      12037       Tate           Decker
    ## 81          2002       2205   DeSagana             Diop
    ## 82          2002      12038       Robb           Dryden
    ## 83          2002       2230    Maurice            Evans
    ## 84          2002      12039    Anthony            Evans
    ## 85          2002      12150   Benjamin              Eze
    ## 86          2002       2386     Kimani          Ffriend
    ## 87          2002       2228      Alton             Ford
    ## 88          2002       2218     Joseph            Forte
    ## 89          2002         -1      Jason          Gardner
    ## 90          2002      12151      Jerry            Green
    ## 91          2002      12152      Kenny          Gregory
    ## 92          2002       2204      Eddie          Griffin
    ## 93          2002       2239    Trenton          Hassell
    ## 94          2002       2213       Kirk           Haston
    ## 95          2002       2217    Brendan          Haywood
    ## 96          2002      12154    Michael            Hicks
    ## 97          2002       2212     Steven           Hunter
    ## 98          2002      12155      Andre           Hutson
    ## 99          2002      12156       Nate            James
    ## 100         2002       2210    Richard        Jefferson
    ## 101         2002       2798     Horace          Jenkins
    ## 102         2002      12157    Darrell            Johns
    ## 103         2002       2256        Ken          Johnson
    ## 104         2002       2207        Joe          Johnson
    ## 105         2002      12158     Darren            Kelly
    ## 106         2002       2253       Sean          Lampley
    ## 107         2002      12159       Zach          Marbury
    ## 108         2002      12160       Mike        Mardesich
    ## 109         2002      12161   DeMarcus            Minor
    ## 110         2002     200081    Jamario             Moon
    ## 111         2002       2211       Troy           Murphy
    ## 112         2002      12162       Troy           Ostler
    ## 113         2002      40046    Lazaros     Papadopoulos
    ## 114         2002      12164     Rashad         Phillips
    ## 115         2002      12165  Demetrius           Porter
    ## 116         2002      12166     Martin           Rancik
    ## 117         2002       2202      Jason       Richardson
    ## 118         2002       2369     Norman       Richardson
    ## 119         2002       2219      Jeryl           Sasser
    ## 120         2002      12167      Kenny      Satterfield
    ## 121         2002       2243      Brian       Scalabrine
    ## 122         2002      12168        Lee          Scruggs
    ## 123         2002       2250      Bobby          Simmons
    ## 124         2002       2226       Will          Solomon
    ## 125         2002      12169       Greg        Stevenson
    ## 126         2002      12170     Damone         Thornton
    ## 127         2002       2224     Jamaal          Tinsley
    ## 128         2002      12171       Tory           Walker
    ## 129         2002      12172 Souleymane             Wane
    ## 130         2002       2248       Earl           Watson
    ## 131         2002       2206     Rodney            White
    ## 132         2002       2254      Loren            Woods
    ## 133         2002      12040      Brent           Wright
    ## 134         2002      12041    Michael           Wright
    ## 135         2003       2403       Nene                 
    ## 136         2003       2425     Robert        Archibald
    ## 137         2003       2486    Maurice            Baker
    ## 138         2003       2440       Matt           Barnes
    ## 139         2003      12286      Lubos           Barton
    ## 140         2003       2437      Lonny           Baxter
    ## 141         2003      12287        Lee           Benson
    ## 142         2003       2430     Carlos           Boozer
    ## 143         2003       2414     Curtis        Borchardt
    ## 144         2003       2452       J.R.           Bremer
    ## 145         2003      12288      Brian            Brown
    ## 146         2003      12289    Sylvere            Bryan
    ## 147         2003      12290      Chris          Burgess
    ## 148         2003       2406      Caron           Butler
    ## 149         2003       2446     Rasual           Butler
    ## 150         2003      12291       Mire          Chatman
    ## 151         2003      12042      Chris   Christoffersen
    ## 152         2003      12044      Craig           Dawson
    ## 153         2003       2399       Mike         Dunleavy
    ## 154         2003      12045      Teddy            Dupay
    ## 155         2003       2451    Corsley          Edwards
    ## 156         2003      12046       Andy            Ellis
    ## 157         2003       2408     Melvin              Ely
    ## 158         2003       2501     Reggie            Evans
    ## 159         2003       2400       Drew           Gooden
    ## 160         2003      12066      David           Graves
    ## 161         2003       2696       Lynn            Greer
    ## 162         2003     101219    Anthony           Grundy
    ## 163         2003       2409     Marcus          Haislip
    ## 164         2003      12068      Damon          Hancock
    ## 165         2003      12069       Greg       Harrington
    ## 166         2003       2617     Udonis           Haslem
    ## 167         2003      12070    Cordell            Henry
    ## 168         2003       2450      Randy          Holcomb
    ## 169         2003       2415       Ryan         Humphrey
    ## 170         2003       2407      Jared         Jeffries
    ## 171         2003      12049      Jason         Jennings
    ## 172         2003       2495     Lonnie            Jones
    ## 173         2003       2410       Fred            Jones
    ## 174         2003      12050       Sean          Kennedy
    ## 175         2003      12051       Kris             Lang
    ## 176         2003      12052   Muhammed           Lasege
    ## 177         2003       2677      Kevin             Lyde
    ## 178         2003       2432       Tito           Maddox
    ## 179         2003      12054        Kei          Madison
    ## 180         2003      12057      Elvin             Mims
    ## 181         2003      12058      Byron           Mouton
    ## 182         2003       2436     Ronald           Murray
    ## 183         2003      12059       Uche           Okafor
    ## 184         2003       2457    Jannero            Pargo
    ## 185         2003       2470      Smush           Parker
    ## 186         2003       2458       Luke           Recker
    ## 187         2003      12061      Rolan          Roberts
    ## 188         2003      12062     Travis         Robinson
    ## 189         2003       2416     Kareem             Rush
    ## 190         2003      12063     Brooks            Sales
    ## 191         2003       2422       John          Salmons
    ## 192         2003       2453    Predrag          Savovic
    ## 193         2003     201324     Julian          Sensley
    ## 194         2003      12065    Preston         Shumpert
    ## 195         2003       2447      Tamar             Slay
    ## 196         2003       2443     Darius         Songaila
    ## 197         2003       2405     Amar'e       Stoudemire
    ## 198         2003       2445     Marcus           Taylor
    ## 199         2003      12297      Jobey           Thomas
    ## 200         2003       2402     Dajuan           Wagner
    ## 201         2003       2404      Chris           Wilcox
    ## 202         2003       2421      Frank         Williams
    ## 203         2003       2398        Jay         Williams
    ## 204         2003       2465     George         Williams
    ## 205         2003       2417     Qyntel            Woods
    ## 206         2003       2428    Vincent        Yarbrough
    ## 207         2004     200995   Aloysius         Anagonye
    ## 208         2004      12072       Rick         Anderson
    ## 209         2004       2546    Carmelo          Anthony
    ## 210         2004       2602     Jerome          Beasley
    ## 211         2004       2559       Troy             Bell
    ## 212         2004      12073     LaVell        Blanchard
    ## 213         2004       2586      Keith           Bogans
    ## 214         2004       2588       Matt           Bonner
    ## 215         2004       2547      Chris             Bosh
    ## 216         2004       2651   Jermaine          Boyette
    ## 217         2004      12295 Souleymane           Camara
    ## 218         2004       2679       Matt          Carroll
    ## 219         2004       2555       Nick         Collison
    ## 220         2004       2567      Brian             Cook
    ## 221         2004      12074       Joel         Cornette
    ## 222         2004       2605    Marquis          Daniels
    ## 223         2004      12075 Aleksander           Djuric
    ## 224         2004      12076      Ruben          Douglas
    ## 225         2004       2648     Ronald           Dupree
    ## 226         2004       2606       Carl          English
    ## 227         2004      12077        Ebi              Ere
    ## 228         2004      12078    Marquis           Estill
    ## 229         2004       2551       T.J.             Ford
    ## 230         2004      12079      Jason          Gardner
    ## 231         2004       2584     Willie            Green
    ## 232         2004      12080     Justin         Hamilton
    ## 233         2004       2580     Travis           Hansen
    ## 234         2004      12081     Trevor           Harvey
    ## 235         2004       2618     Marcus           Hatten
    ## 236         2004       2550       Kirk          Hinrich
    ## 237         2004      12082      Jerry           Holman
    ## 238         2004       2572       Josh           Howard
    ## 239         2004       2599    Brandon           Hunter
    ## 240         2004      12084       Sani          Ibrahim
    ## 241         2004      12085    Michael         Ignerski
    ## 242         2004      12086     Robert          Jackson
    ## 243         2004       2639    Britton          Johnsen
    ## 244         2004       2563    Dahntay            Jones
    ## 245         2004       2592      James            Jones
    ## 246         2004       2549      Chris            Kaman
    ## 247         2004       2574      Jason           Kapono
    ## 248         2004      12087      Jason             Keep
    ## 249         2004      12088    Bernard             King
    ## 250         2004       2594       Kyle           Korver
    ## 251         2004       2591      James             Lang
    ## 252         2004      12089     Donald           Little
    ## 253         2004      12090      Chris           Massie
    ## 254         2004      12091       Will         McDonald
    ## 255         2004       2545      Darko          Milicic
    ## 256         2004      12092       Jeff           Newton
    ## 257         2004      12093       Uche     Nsonwu-Amadi
    ## 258         2004      12094     Ugonna          Okyekwe
    ## 259         2004     101248     Marlon           Parmer
    ## 260         2004      12095   Stephane            Pelle
    ## 261         2004       2632       Kirk           Penney
    ## 262         2004       2750      Pavel        Podkolzin
    ## 263         2004       2694       Josh           Powell
    ## 264         2004      12096     Hollis            Price
    ## 265         2004       2557       Luke          Ridnour
    ## 266         2004      12097        Joe            Shipp
    ## 267         2004      12098        Ron             Slay
    ## 268         2004       2604     Theron            Smith
    ## 269         2004       2596      Tommy            Smith
    ## 270         2004      12099     Marvin            Stone
    ## 271         2004       2552    Michael         Sweetney
    ## 272         2004       2548     Dwyane             Wade
    ## 273         2004      12101      Wayne          Wallace
    ## 274         2004       2561      David             West
    ## 275         2004      12102     Wesley           Wilson
    ## 276         2004      12103       Doug            Wrenn
    ## 277         2004       2583    Derrick        Zimmerman
    ## 278         2005       2754       Tony            Allen
    ## 279         2005       2772     Trevor            Ariza
    ## 280         2005       2857      Andre          Barrett
    ## 281         2005     101138    Brandon             Bass
    ## 282         2005      12173      Brian        Boddicker
    ## 283         2005      12174        Tim           Bowers
    ## 284         2005       2810      Andre            Brown
    ## 285         2005       2866     Jackie           Butler
    ## 286         2005      12175 Aleksandar            Capin
    ## 287         2005      12176       Ales             Chan
    ## 288         2005       2735       Josh        Childress
    ## 289         2005     200555         TJ         Cummings
    ## 290         2005       2845       Erik          Daniels
    ## 291         2005       2736       Luol             Deng
    ## 292         2005      12177     Marcus          Douthit
    ## 293         2005       2768      Chris            Duhon
    ## 294         2005       2824     Desmon           Farmer
    ## 295         2005       2784       Luis           Flores
    ## 296         2005       2782       Matt           Freije
    ## 297         2005      12178      Chris          Garnett
    ## 298         2005     101155       Ryan            Gomes
    ## 299         2005       2732        Ben           Gordon
    ## 300         2005       2734      Devin           Harris
    ## 301         2005       2730     Dwight           Howard
    ## 302         2005      12179    Rolando           Howell
    ## 303         2005       2743       Kris        Humphries
    ## 304         2005       2738      Andre         Iguodala
    ## 305         2005      12180     Martin              Iti
    ## 306         2005       2739       Luke          Jackson
    ## 307         2005       2744         Al        Jefferson
    ## 308         2005       2865     Arthur          Johnson
    ## 309         2005      12181       Ivan         Koljevic
    ## 310         2005       2733      Shaun       Livingston
    ## 311         2005      12184      Jaime          Lloreda
    ## 312         2005      12000     Bryant         Matthews
    ## 313         2005      12001     Marcus           Melvin
    ## 314         2005       2822       Rich           Melzer
    ## 315         2005       2777      Ricky           Minard
    ## 316         2005      12003     Marcus            Moore
    ## 317         2005      12002      James            Moore
    ## 318         2005      12004     Michel        Morandais
    ## 319         2005       2749     Jameer           Nelson
    ## 320         2005      12005      Misan       Nikagbatse
    ## 321         2005       2731      Emeka           Okafor
    ## 322         2005      12006    Randall              Orr
    ## 323         2005      12007      Dylan             Page
    ## 324         2005         -1      Drago          Pasalic
    ## 325         2005      12008     Rickey         Paulding
    ## 326         2005      12009     Kelvin             Pena
    ## 327         2005      12010       Omar         Quintero
    ## 328         2005       2762      Peter            Ramos
    ## 329         2005       2876      Jared           Reiner
    ## 330         2005     101160   Lawrence          Roberts
    ## 331         2005       2774    Bernard         Robinson
    ## 332         2005     101126       Nate         Robinson
    ## 333         2005      12011     Aerick          Sanders
    ## 334         2005       2781     Romain             Sato
    ## 335         2005      12012    Blagota          Sekulic
    ## 336         2005       2746       Josh            Smith
    ## 337         2005       2747       J.R.            Smith
    ## 338         2005       2745       Kirk           Snyder
    ## 339         2005       2776       Pape              Sow
    ## 340         2005      12014      Tiago         Splitter
    ## 341         2005      12016        Tom      Timmerrmans
    ## 342         2005      12296      Marko            Tomas
    ## 343         2005       2761    Jackson           Vroman
    ## 344         2005       2753    Delonte             West
    ## 345         2005       2863     Damien          Wilkins
    ## 346         2005      12292       Mike         Williams
    ## 347         2005      12293       Nate         Williams
    ## 348         2005      12294     Rashad           Wright
    ## 349         2006     101165       Alex            Acker
    ## 350         2006     101224       Deji         Akindele
    ## 351         2006     101187       Alan         Anderson
    ## 352         2006     101149   Martynas  Andriuskevicius
    ## 353         2006     101252       Sean            Banks
    ## 354         2006     101188      Eddie           Basden
    ## 355         2006      12105       Mike             Bell
    ## 356         2006     101106     Andrew            Bogut
    ## 357         2006     101198       Will            Bynum
    ## 358         2006     101215       Will           Conroy
    ## 359         2006      12107     Taylor       Coppenrath
    ## 360         2006     101143     Travis           Diener
    ## 361         2006     101113        Ike            Diogu
    ## 362         2006      12108      Daryl           Dorsey
    ## 363         2006     101145      Monta            Ellis
    ## 364         2006     101109    Raymond           Felton
    ## 365         2006     201164       Rudy        Fernandez
    ## 366         2006      12109       D'Or          Fischer
    ## 367         2006      12110       Eddy            Fobbs
    ## 368         2006     101213    Sharrod             Ford
    ## 369         2006     101112   Channing             Frye
    ## 370         2006     101184       Deng              Gai
    ## 371         2006     101128  Francisco           Garcia
    ## 372         2006      12111       John        Gilchrist
    ## 373         2006     101162     Marcin           Gortat
    ## 374         2006     101121       Joey           Graham
    ## 375         2006     101122      Danny          Granger
    ## 376         2006     101123     Gerald            Green
    ## 377         2006     101236    Charles            Hayes
    ## 378         2006     101129     Luther             Head
    ## 379         2006     101125     Julius            Hodge
    ## 380         2006     101141      Ersan         Ilyasova
    ## 381         2006     101127    Jarrett             Jack
    ## 382         2006     101204     Dwayne            Jones
    ## 383         2006      12113  Mindaugas        Katelynas
    ## 384         2006     201318      Jason            Klotz
    ## 385         2006         -1       Carl          Krauser
    ## 386         2006     101247      Keith         Langford
    ## 387         2006     101135      David              Lee
    ## 388         2006     101249       John        Lucas III
    ## 389         2006     101185      Rawle         Marshall
    ## 390         2006     101118       Sean              May
    ## 391         2006     101119     Rashad          McCants
    ## 392         2006     200829       Ivan         McFarlin
    ## 393         2006      12115       Juan           Mendez
    ## 394         2006     101223      Aaron            Miles
    ## 395         2006      12116      Ellis            Myles
    ## 396         2006      12117      Larry         O'Bannon
    ## 397         2006      12118      Drago          Pasalic
    ## 398         2006     101108      Chris             Paul
    ## 399         2006     201225     Carlos           Powell
    ## 400         2006     101179     Ronnie            Price
    ## 401         2006     101194    Anthony         Roberson
    ## 402         2006     201575    Brandon             Rush
    ## 403         2006     101195       Luke       Schenscher
    ## 404         2006     101134      Wayne           Simien
    ## 405         2006      12120        Tre          Simmons
    ## 406         2006      12121      David            Simon
    ## 407         2006     101147      Chris             Taft
    ## 408         2006      12122      Chris           Thomas
    ## 409         2006      12119       Omar           Thomas
    ## 410         2006     101142      Ronny           Turiaf
    ## 411         2006     101111    Charlie       Villanueva
    ## 412         2006     101110    Martell          Webster
    ## 413         2006     101156     Robert           Whaley
    ## 414         2006     101114      Deron         Williams
    ## 415         2006     101214      Jawad         Williams
    ## 416         2006     101107     Marvin         Williams
    ## 417         2006     101152     Bracey           Wright
    ## 418         2006     101120    Antoine           Wright
    ## 419         2007      12123      Kenny          Adeleke
    ## 420         2007     200772    Maurice             Ager
    ## 421         2007     200746   LaMarcus         Aldridge
    ## 422         2007     201165     Morris           Almond
    ## 423         2007     200811        Lou         Amundson
    ## 424         2007     202151     Rashad         Anderson
    ## 425         2007     200756     Hilton        Armstrong
    ## 426         2007     200788      James        Augustine
    ## 427         2007     200764    Renaldo          Balkman
    ## 428         2007      12124       J.P.          Batista
    ## 429         2007     200807       Will          Blalock
    ## 430         2007     200847    Brandon           Bowman
    ## 431         2007     200758     Ronnie           Brewer
    ## 432         2007     200787     Denham            Brown
    ## 433         2007      12125       Brad          Buckman
    ## 434         2007      12126        Nik     Caner-Medley
    ## 435         2007     200760     Rodney           Carney
    ## 436         2007      12127    Keydren            Clark
    ## 437         2007     200776      Mardy          Collins
    ## 438         2007      12128     Taquan             Dean
    ## 439         2007      12129    Terence            Dials
    ## 440         2007      12130       Sean          Dockery
    ## 441         2007     200770     Jordan           Farmar
    ## 442         2007     200751      Randy             Foye
    ## 443         2007      12185      Torin          Francis
    ## 444         2007     200752       Rudy              Gay
    ## 445         2007      12186       Nick           George
    ## 446         2007      12187        Taj             Gray
    ## 447         2007      12188        Dan         Grunfeld
    ## 448         2007      12189       Matt          Haryasz
    ## 449         2007      12190       Eric            Hicks
    ## 450         2007      12191     Tedric             Hill
    ## 451         2007     200812     Daniel           Horton
    ## 452         2007     200780    Solomon            Jones
    ## 453         2007     200784      Bobby            Jones
    ## 454         2007      12192     Viktor            Keyru
    ## 455         2007      12193      Marco    Killingsworth
    ## 456         2007     200814    Tarence           Kinsey
    ## 457         2007      12194       Carl          Krauser
    ## 458         2007     200840      Chris           McCray
    ## 459         2007     201019      Gerry         McNamara
    ## 460         2007     200822       Pops     Mensah-Bonsu
    ## 461         2007     200859       Paul           Miller
    ## 462         2007     200794       Paul          Millsap
    ## 463         2007     201383     Dwayne         Mitchell
    ## 464         2007     200747       Adam         Morrison
    ## 465         2007      12195       Yemi        Nicholson
    ## 466         2007     200786      David             Noel
    ## 467         2007     200779      Steve            Novak
    ## 468         2007     200753    Patrick         O'Bryant
    ## 469         2007      12196     Danilo          Pinnock
    ## 470         2007     200809      Chris            Quinn
    ## 471         2007     200810      Allan              Ray
    ## 472         2007     200755         JJ           Redick
    ## 473         2007     201015   Antywane         Robinson
    ## 474         2007     200750    Brandon              Roy
    ## 475         2007         -1      Blake           Schilb
    ## 476         2007     200754   Mouhamed             Sene
    ## 477         2007     200759     Cedric          Simmons
    ## 478         2007     201201     Marcus        Slaughter
    ## 479         2007     200783      Craig            Smith
    ## 480         2007     200848     Steven            Smith
    ## 481         2007      23033    Michael        Southhall
    ## 482         2007      12199      Frans            Steyn
    ## 483         2007     201028     Curtis          Stinson
    ## 484         2007     200748      Tyrus           Thomas
    ## 485         2007      12200       Joah           Tucker
    ## 486         2007      12201        Ian        Vouyoukas
    ## 487         2007     200827     Darius       Washington
    ## 488         2007     201228         CJ           Watson
    ## 489         2007     200818     Justin         Williams
    ## 490         2007     200749    Shelden         Williams
    ## 491         2007     200761     Shawne         Williams
    ## 492         2007      12203       Eric         Williams
    ## 493         2007     200766     Marcus         Williams
    ## 494         2007     202124     Curtis          Withers
    ## 495         2008      12204    Mohamed           Abukar
    ## 496         2008      12205      Mario           Boggan
    ## 497         2008      12206      Craig         Bradshaw
    ## 498         2008     201147      Corey           Brewer
    ## 499         2008     201166      Aaron           Brooks
    ## 500         2008     201628      Bobby            Brown
    ## 501         2008     201469    Russell           Carter
    ## 502         2008      12207    Coleman          Collins
    ## 503         2008     201144       Mike           Conley
    ## 504         2008     201161    Daequan             Cook
    ## 505         2008      12208      Ryvon           Covile
    ## 506         2008     201159    Javaris       Crittenton
    ## 507         2008     201176   Jermareo         Davidson
    ## 508         2008      12210     Zabian          Dowdell
    ## 509         2008     201162      Jared           Dudley
    ## 510         2008     201142      Kevin           Durant
    ## 511         2008      12211    Rashaun          Freeman
    ## 512         2008     201189      Aaron             Gray
    ## 513         2008     201145       Jeff            Green
    ## 514         2008     201192    Taurean            Green
    ## 515         2008      12212      Caleb            Green
    ## 516         2008     201150    Spencer            Hawes
    ## 517         2008      12214    Brandon            Heath
    ## 518         2008     201195    Herbert             Hill
    ## 519         2008     201143         Al          Horford
    ## 520         2008      12215    Quinton           Hosley
    ## 521         2008      12216      James           Hughes
    ## 522         2008      12217     Jeremy             Hunt
    ## 523         2008      12218      Ekene           Ibekwe
    ## 524         2008      12219    Dominic            James
    ## 525         2008     201234       Trey          Johnson
    ## 526         2008      12220     Rashad   Jones-Jennings
    ## 527         2008     201185      Jared           Jordan
    ## 528         2008     201207       Coby             Karl
    ## 529         2008      12221    Antanas     Kavaliauskas
    ## 530         2008     201171       Carl           Landry
    ## 531         2008     201186   Stephane            Lasme
    ## 532         2008     201151       Acie              Law
    ## 533         2008      12222      Marko            Lekic
    ## 534         2008      12223        Ron            Lewis
    ## 535         2008     201858    Cartier           Martin
    ## 536         2008     201187    Dominic          McGuire
    ## 537         2008     201177       Josh        McRoberts
    ## 538         2008     201197      Sammy            Mejia
    ## 539         2008      12224       Brad           Newley
    ## 540         2008     201193   Demetris          Nichols
    ## 541         2008     201149     Joakim             Noah
    ## 542         2008     201141       Greg             Oden
    ## 543         2008      12225       Ivan        Radenovic
    ## 544         2008      12226         JR         Reynolds
    ## 545         2008     201181      Chris          Richard
    ## 546         2008      12227     Dustin        Salisbery
    ## 547         2008      12228      Blake           Schilb
    ## 548         2008      12229   Renaldas         Seibutis
    ## 549         2008     201196      Ramon         Sessions
    ## 550         2008     201203    Mustafa           Shakur
    ## 551         2008     201160      Jason            Smith
    ## 552         2008     201199         DJ       Strawberry
    ## 553         2008     201155     Rodney          Stuckey
    ## 554         2008      12230     Curtis          Sumpter
    ## 555         2008     201180        Yue              Sun
    ## 556         2008     201249     Jamaal            Tatum
    ## 557         2008      12232   Reyshawn            Terry
    ## 558         2008     201154         Al         Thornton
    ## 559         2008     201229    Anthony         Tolliver
    ## 560         2008      12233        Ali           Traore
    ## 561         2008      12234       Kyle           Visser
    ## 562         2008     201208     Darryl          Watkins
    ## 563         2008      12235      Major          Wingate
    ## 564         2008      12236    DaShaun             Wood
    ## 565         2008     201153     Julian           Wright
    ## 566         2008     201148    Brandan           Wright
    ## 567         2008      12237       Avis            Wyatt
    ## 568         2008     201152   Thaddeus            Young
    ## 569         2008     201156       Nick            Young
    ## 570         2009     201570        Joe        Alexander
    ## 571         2009     201589    Darrell           Arthur
    ## 572         2009     201571       D.J.         Augustin
    ## 573         2009     201573     Jerryd          Bayless
    ## 574         2009     201563    Michael          Beasley
    ## 575         2009      12239      Ramel          Bradley
    ## 576         2009      12240     Tyrone        Brazelton
    ## 577         2009      12241     Takais            Brown
    ## 578         2009     201812      Keith        Brumbaugh
    ## 579         2009     201868    Stanley          Burrell
    ## 580         2009      12242      Brian            Butch
    ## 581         2009      12245        Joe         Crawford
    ## 582         2009      12246      Chris          Daniels
    ## 583         2009     201595       Joey           Dorsey
    ## 584         2009      12247     Marcus             Dove
    ## 585         2009      12248       Josh           Duncan
    ## 586         2009      12249      Frank           Elegar
    ## 587         2009     201607    Patrick        Ewing Jr.
    ## 588         2009     201814       Gary           Forbes
    ## 589         2009      12250       Shan           Foster
    ## 590         2009      12251      James             Gist
    ## 591         2009      12252   Vladimir        Golubovic
    ## 592         2009     201569       Eric           Gordon
    ## 593         2009     201863   Kentrell       Gransberry
    ## 594         2009     201590      Donte           Greene
    ## 595         2009     201612      Malik         Hairston
    ## 596         2009     201614      DeVon           Hardin
    ## 597         2009     201613    Richard          Hendrix
    ## 598         2009     201588     George             Hill
    ## 599         2009      12254       Kyle            Hines
    ## 600         2009      12255       Jiri          Hubalek
    ## 601         2009     201991     Lester           Hudson
    ## 602         2009     201629    Othello           Hunter
    ## 603         2009     201616    Darnell          Jackson
    ## 604         2009      12257      Davon        Jefferson
    ## 605         2009      12258     Joseph            Jones
    ## 606         2009     201599    DeAndre           Jordan
    ## 607         2009      12253      Sasha             Kaun
    ## 608         2009      12260   Marcelus             Kemp
    ## 609         2009      12261     Maarty           Leunen
    ## 610         2009     201572      Brook            Lopez
    ## 611         2009     201567      Kevin             Love
    ## 612         2009      12262      Aleks            Maric
    ## 613         2009     201564       O.J.             Mayo
    ## 614         2009     201651      James             Mays
    ## 615         2009     201601        Luc     Mbah a Moute
    ## 616         2009     201580     JaVale            McGee
    ## 617         2009      12263       Drew          Neitzel
    ## 618         2009     201634   DeMarcus           Nelson
    ## 619         2009     201624      David          Padgett
    ## 620         2009      12264     Jeremy            Pargo
    ## 621         2009      12265      Trent         Plaisted
    ## 622         2009      12266       Quan          Prowell
    ## 623         2009      12267      Shaun           Pruitt
    ## 624         2009     201576    Anthony         Randolph
    ## 625         2009      12268    Charles           Rhodes
    ## 626         2009      12269      Brian          Roberts
    ## 627         2009     201781    Russell         Robinson
    ## 628         2009     201565    Derrick             Rose
    ## 629         2009      12271     Ronald           Steele
    ## 630         2009      12272      Bryce           Taylor
    ## 631         2009     201446       Mike           Taylor
    ## 632         2009     201872       Mark          Tyndale
    ## 633         2009      12273     Robert            Vaden
    ## 634         2009     201622      Deron       Washington
    ## 635         2009     201603      Sonny            Weems
    ## 636         2009     201566    Russell        Westbrook
    ## 637         2009        199     Reggie         Williams
    ## 638         2010     202399       Jeff           Adrien
    ## 639         2010     201965       Jeff            Ayres
    ## 640         2010     201958   Rodrigue         Beaubois
    ## 641         2010     201971     DeJuan            Blair
    ## 642         2010     201974    Derrick            Brown
    ## 643         2010     201978      Chase         Budinger
    ## 644         2010     201956       Omri           Casspi
    ## 645         2010      12275     Dionte        Christmas
    ## 646         2010     201947       Earl            Clark
    ## 647         2010     201954     Darren         Collison
    ## 648         2010     201967      Dante       Cunningham
    ## 649         2010     201939    Stephen            Curry
    ## 650         2010     201948     Austin             Daye
    ## 651         2010     201942      DeMar          DeRozan
    ## 652         2010     201962      Toney          Douglas
    ## 653         2010     201961      Wayne        Ellington
    ## 654         2010     201936     Tyreke            Evans
    ## 655         2010     201938      Jonny            Flynn
    ## 656         2010     201959        Taj           Gibson
    ## 657         2010     201980      Danny            Green
    ## 658         2010     201933      Blake          Griffin
    ## 659         2010     201946      Tyler       Hansbrough
    ## 660         2010     201935      James           Harden
    ## 661         2010     201945     Gerald        Henderson
    ## 662         2010      12278       Josh         Heytvelt
    ## 663         2010     201941     Jordan             Hill
    ## 664         2010     201950       Jrue          Holiday
    ## 665         2010      12279        Joe           Ingles
    ## 666         2010     201949      James          Johnson
    ## 667         2010     201951         Ty           Lawson
    ## 668         2010     201953       Eric           Maynor
    ## 669         2010      12282       Jack        McClinton
    ## 670         2010      12283      Jerel           McNeal
    ## 671         2010     201975      Jodie            Meeks
    ## 672         2010     201988      Patty            Mills
    ## 673         2010     201957      Byron          Mullens
    ## 674         2010     201985       A.J.            Price
    ## 675         2010      12284      Tyler            Smith
    ## 676         2010     201969     DaJuan          Summers
    ## 677         2010     201966   Jermaine           Taylor
    ## 678         2010     201952       Jeff           Teague
    ## 679         2010     201977     Marcus         Thornton
    ## 680         2010     201944   Terrence         Williams
    ## 681         2010     201970        Sam            Young
    ## 682         2011     202374    Solomon            Alabi
    ## 683         2011     202332       Cole          Aldrich
    ## 684         2011     202329  Al-Farouq            Aminu
    ## 685         2011     202341      James         Anderson
    ## 686         2011     202337       Luke          Babbitt
    ## 687         2011     202344     Trevor           Booker
    ## 688         2011     202342      Craig         Brackins
    ## 689         2011     202340      Avery          Bradley
    ## 690         2011     202382    Derrick         Caracter
    ## 691         2011     202395    Sherron          Collins
    ## 692         2011     202326   DeMarcus          Cousins
    ## 693         2011     202348     Jordan         Crawford
    ## 694         2011     202334         Ed            Davis
    ## 695         2011     202365      Devin           Ebanks
    ## 696         2011     202324    Derrick           Favors
    ## 697         2011      12316      Keith           Gallon
    ## 698         2011      12317    Charles           Garcia
    ## 699         2011     202376       Luke        Harangody
    ## 700         2011     202330     Gordon          Hayward
    ## 701         2011     202351      Lazar          Hayward
    ## 702         2011     202333     Xavier            Henry
    ## 703         2011     202359  Darington           Hobson
    ## 704         2011     202345     Damion            James
    ## 705         2011     202356      Armon          Johnson
    ## 706         2011     202325     Wesley          Johnson
    ## 707         2011     202346  Dominique            Jones
    ## 708         2011      12330     Sylven       Landesberg
    ## 709         2011     202371       Gani            Lawal
    ## 710         2011     202328       Greg           Monroe
    ## 711         2011     202350     Daniel            Orton
    ## 712         2011      12334    Artsiom      Parakhouski
    ## 713         2011     202335    Patrick        Patterson
    ## 714         2011     202360       Andy          Rautins
    ## 715         2011      12338       Ryan         Richards
    ## 716         2011     202383    Stanley         Robinson
    ## 717         2011     202336      Larry          Sanders
    ## 718         2011     202362      Lance       Stephenson
    ## 719         2011      12343    Mikhail         Torrance
    ## 720         2011     202323       Evan           Turner
    ## 721         2011     202327       Ekpe             Udoh
    ## 722         2011      12346     Jarvis          Varnado
    ## 723         2011     202322       John             Wall
    ## 724         2011     202378     Willie           Warren
    ## 725         2011     202358    Terrico            White
    ## 726         2011     202355     Hassan        Whiteside
    ## 727         2012     202728      Keith           Benson
    ## 728         2012     202705    Marshon           Brooks
    ## 729         2012     202692       Alec            Burks
    ## 730         2012     202710      Jimmy           Butler
    ## 731         2012     202708     Norris             Cole
    ## 732         2012     202739        Jon          Diebler
    ## 733         2012      12406    Michael          Dunigan
    ## 734         2012      12407 LaceDarius             Dunn
    ## 735         2012     202702    Kenneth           Faried
    ## 736         2012      12409      James         Fredette
    ## 737         2012     202726     Andrew        Goudelock
    ## 738         2012     202706     Jordan         Hamilton
    ## 739         2012     202712     Justin           Harper
    ## 740         2012     202699     Tobias           Harris
    ## 741         2012     202715      Tyler        Honeycutt
    ## 742         2012      12415     Scotty           Hopson
    ## 743         2012      12417       Rick          Jackson
    ## 744         2012     202724    Charles          Jenkins
    ## 745         2012     202707     JaJuan          Johnson
    ## 746         2012     202709       Cory           Joseph
    ## 747         2012     202683       Enes           Kanter
    ## 748         2012     202688    Brandon           Knight
    ## 749         2012     202723    Malcolm              Lee
    ## 750         2012     202695      Kawhi          Leonard
    ## 751         2012     202727     Travis           Leslie
    ## 752         2012     202720        Jon            Leuer
    ## 753         2012     202732    DeAndre          Liggins
    ## 754         2012      12428      David           Lighty
    ## 755         2012     202714    Shelvin             Mack
    ## 756         2012      12430    Demetri          McCamey
    ## 757         2012     202734    E'Twaun            Moore
    ## 758         2012     202693   Markieff           Morris
    ## 759         2012     202694     Marcus           Morris
    ## 760         2012     202721     Darius           Morris
    ## 761         2012     202718   Chandler          Parsons
    ## 762         2012      12436     Jereme         Richmond
    ## 763         2012     202729       Josh            Selby
    ## 764         2012     202697       Iman         Shumpert
    ## 765         2012     202713       Kyle          Singler
    ## 766         2012     202698      Chris        Singleton
    ## 767         2012      12441      Jamie            Skeen
    ## 768         2012      12442    Gregory            Smith
    ## 769         2012     202701      Nolan            Smith
    ## 770         2012      12445    Malcolm           Thomas
    ## 771         2012     202738     Isaiah           Thomas
    ## 772         2012     202717       Trey        Thompkins
    ## 773         2012     202691       Klay         Thompson
    ## 774         2012     202684    Tristan         Thompson
    ## 775         2012     202719     Jeremy            Tyler
    ## 776         2012     202696     Nikola          Vucevic
    ## 777         2012     202689      Kemba           Walker
    ## 778         2012     202682    Derrick         Williams
    ## 779         2012     202716     Jordan         Williams
    ## 780         2013     203112     Quincy              Acy
    ## 781         2013     203084   Harrison           Barnes
    ## 782         2013     203115       Will           Barton
    ## 783         2013     203078    Bradley             Beal
    ## 784         2013      12458    J'Covan            Brown
    ## 785         2013     203733    William           Buford
    ## 786         2013     203109        Jae          Crowder
    ## 787         2013     203134     Marcus           Denmon
    ## 788         2013     203083      Andre         Drummond
    ## 789         2013     203119        Kim          English
    ## 790         2013     203105     Festus            Ezeli
    ## 791         2013      12467       Drew           Gordon
    ## 792         2013     203254   JaMychal            Green
    ## 793         2013     203110   Draymond            Green
    ## 794         2013     203090    Maurice         Harkless
    ## 795         2013     203189         Tu         Holloway
    ## 796         2013     203133     Robbie           Hummel
    ## 797         2013     203108    Bernard            James
    ## 798         2013     203098       John          Jenkins
    ## 799         2013     203111    Orlando          Johnson
    ## 800         2013     203130     Darius     Johnson-Odom
    ## 801         2013     203093   Terrence            Jones
    ## 802         2013     203103      Perry            Jones
    ## 803         2013     203158      Kevin            Jones
    ## 804         2013     203126       Kris           Joseph
    ## 805         2013     203077    Michael   Kidd-Gilchrist
    ## 806         2013     203087     Jeremy             Lamb
    ## 807         2013     203117      Doron             Lamb
    ## 808         2013     203086     Meyers          Leonard
    ## 809         2013     203081     Damian          Lillard
    ## 810         2013     203159      Scott          Machado
    ## 811         2013     203088    Kendall         Marshall
    ## 812         2013     203097        Fab             Melo
    ## 813         2013     203114      Khris        Middleton
    ## 814         2013     203121     Darius           Miller
    ## 815         2013     203113     Quincy           Miller
    ## 816         2013     203183       Tony         Mitchell
    ## 817         2013     203102     Arnett         Moultrie
    ## 818         2013     203122      Kevin           Murphy
    ## 819         2013     203094     Andrew        Nicholson
    ## 820         2013     203124       Kyle          O'Quinn
    ## 821         2013     203101      Miles          Plumlee
    ## 822         2013     203085     Austin           Rivers
    ## 823         2013     203080     Thomas         Robinson
    ## 824         2013     203082   Terrence             Ross
    ## 825         2013     203118       Mike            Scott
    ## 826         2013     203156      Henry             Sims
    ## 827         2013     203096      Jared        Sullinger
    ## 828         2013     203106       Jeff           Taylor
    ## 829         2013     203116    Tyshawn           Taylor
    ## 830         2013      12507     Jordan           Taylor
    ## 831         2013     203104    Marquis           Teague
    ## 832         2013     203138     Hollis         Thompson
    ## 833         2013     203100       Tony           Wroten
    ## 834         2013     203092      Tyler           Zeller
    ## 835         2014     203500     Steven            Adams
    ## 836         2014     203505     Vander             Blue
    ## 837         2014     203485    Lorenzo            Brown
    ## 838         2014     203493     Reggie          Bullock
    ## 839         2014     203504       Trey            Burke
    ## 840         2014     203484 Kentavious    Caldwell-Pope
    ## 841         2014     203477     Isaiah           Canaan
    ## 842         2014     203478     Jackie       Carmichael
    ## 843         2014     203487    Michael  Carter-Williams
    ## 844         2014      12524       Will          Clyburn
    ## 845         2014     203496     Robert        Covington
    ## 846         2014     203459      Allen           Crabbe
    ## 847         2014     203561    Brandon           Davies
    ## 848         2014     203473    Dewayne           Dedmon
    ## 849         2014     203516      James            Ennis
    ## 850         2014     203467    Carrick            Felix
    ## 851         2014     203497       Rudy           Gobert
    ## 852         2014     203462     Archie          Goodwin
    ## 853         2014     203501        Tim     Hardaway Jr.
    ## 854         2014     203524    Solomon             Hill
    ## 855         2014     203470     Colton          Iverson
    ## 856         2014     203511      Grant          Jerrett
    ## 857         2014     203509       Myck          Kabongo
    ## 858         2014     203483      Kenny            Kadji
    ## 859         2014     203499      Shane           Larkin
    ## 860         2014     203495      Ricky             Ledo
    ## 861         2014     203466         CJ           Leslie
    ## 862         2014     203522     Trevor           Mbakwe
    ## 863         2014     203492        Ray         McCallum
    ## 864         2014     203468         CJ         McCollum
    ## 865         2014     203463        Ben         McLemore
    ## 866         2014     203502       Tony         Mitchell
    ## 867         2014     203498    Shabazz         Muhammad
    ## 868         2014     203513       Erik           Murphy
    ## 869         2014     203488       Mike          Muscala
    ## 870         2014     203506     Victor          Oladipo
    ## 871         2014     203482      Kelly           Olynyk
    ## 872         2014     203464    Brandon             Paul
    ## 873         2014     203658     Norvel            Pelle
    ## 874         2014     203486      Mason          Plumlee
    ## 875         2014     203490       Otto           Porter
    ## 876         2014     203515       Phil          Pressey
    ## 877         2014     203318       Glen             Rice
    ## 878         2014     203460      Andre         Roberson
    ## 879         2014     203471     Dennis         Schroder
    ## 880         2014     203491     Peyton             Siva
    ## 881         2014     203503       Tony            Snell
    ## 882         2014     203480      James      Southerland
    ## 883         2014     203472    Deshaun           Thomas
    ## 884         2014     203519     Adonis           Thomas
    ## 885         2014     203481       Jeff           Withey
    ## 886         2014     203469       Cody           Zeller
    ## 887         2015     203919     Jordan            Adams
    ## 888         2015     203648   Thanasis    Antetokounmpo
    ## 889         2015     203947     Jordan        Bachynski
    ## 890         2015     203946    Cameron         Bairstow
    ## 891         2015     203920       Khem            Birch
    ## 892         2015     203938       Alec            Brown
    ## 893         2015     203913     Jabari            Brown
    ## 894         2015     203900     Markel            Brown
    ## 895         2015     203907     Deonte           Burton
    ## 896         2015     203902      Semaj         Christon
    ## 897         2015     203903     Jordan         Clarkson
    ## 898         2015     203905      Aaron            Craft
    ## 899         2015     203908    DeAndre          Daniels
    ## 900         2015     203921 Cleanthony            Early
    ## 901         2015     203929     Melvin             Ejim
    ## 902         2015     203898      Tyler            Ennis
    ## 903         2015     203957      Dante             Exum
    ## 904         2015     203927       C.J.             Fair
    ## 905         2015     203932      Aaron           Gordon
    ## 906         2015     203798       P.J.         Hairston
    ## 907         2015     203925        Joe           Harris
    ## 908         2015     203918     Rodney             Hood
    ## 909         2015     203928       Cory        Jefferson
    ## 910         2015     203910       Nick          Johnson
    ## 911         2015     203945       Alex             Kirk
    ## 912         2015     203897       Zach           LaVine
    ## 913         2015     203906      Devyn           Marble
    ## 914         2015     203949      James           McAdoo
    ## 915         2015     203909       K.J.        McDaniels
    ## 916         2015     203926       Doug        McDermott
    ## 917         2015     203895     Jordan            McRae
    ## 918         2015     203894    Shabazz           Napier
    ## 919         2015     203948     Johnny     O?Bryant III
    ## 920         2015     203934      Lamar        Patterson
    ## 921         2015     203901     Elfrid           Payton
    ## 922         2015     203939     Dwight           Powell
    ## 923         2015     203944     Julius           Randle
    ## 924         2015     203922      Glenn     Robinson III
    ## 925         2015     203941  LaQuinton             Ross
    ## 926         2015     203935     Marcus            Smart
    ## 927         2015     203893       Russ            Smith
    ## 928         2015     203917        Nik         Stauskas
    ## 929         2015     203950    Jarnell           Stokes
    ## 930         2015     203904     Xavier           Thames
    ## 931         2015     203943       Noah           Vonleh
    ## 932         2015     203933       T.J.           Warren
    ## 933         2015     203912       C.J.           Wilcox
    ## 934         2015     203899    Kendall         Williams
    ## 935         2015     203942     Patric            Young
    ## 936         2016    1626147     Justin         Anderson
    ## 937         2016    1626165    Brandon           Ashley
    ## 938         2016    1626207       Ryan        Boatright
    ## 939         2016    1626164      Devin           Booker
    ## 940         2016    1626148    Anthony            Brown
    ## 941         2016    1626176     Rakeem        Christmas
    ## 942         2016    1626192        Pat      Connaughton
    ## 943         2016    1626188      Quinn             Cook
    ## 944         2016    1626183    Branden           Dawson
    ## 945         2016    1626155        Sam           Dekker
    ## 946         2016    1626187    Michael       Frazier II
    ## 947         2016    1626203    Treveon           Graham
    ## 948         2016    1626152    Olivier           Hanlan
    ## 949         2016    1626150     Andrew         Harrison
    ## 950         2016    1626151      Aaron         Harrison
    ## 951         2016    1626227      Tyler           Harvey
    ## 952         2016    1626186      Corey          Hawkins
    ## 953         2016    1626178     Rondae Hollis-Jefferson
    ## 954         2016    1626182   Jonathan           Holmes
    ## 955         2016    1626158    Richaun           Holmes
    ## 956         2016    1626154       R.J.           Hunter
    ## 957         2016    1626205      Vince           Hunter
    ## 958         2016    1626177     Dakari          Johnson
    ## 959         2016    1626145       Tyus            Jones
    ## 960         2016    1626185     Jarell           Martin
    ## 961         2016     204456         TJ        McConnell
    ## 962         2016    1626175     Jordan           Mickey
    ## 963         2016       2006      Larry            Nance
    ## 964         2016    1626162      Kelly            Oubre
    ## 965         2016    1626166    Cameron            Payne
    ## 966         2016    1626189     Terran         Petteway
    ## 967         2016    1626171      Bobby           Portis
    ## 968         2016    1626181     Norman           Powell
    ## 969         2016    1626180    Michael           Qualls
    ## 970         2016    1626184    Chasson           Randle
    ## 971         2016    1626179      Terry           Rozier
    ## 972         2016    1626208     Keifer            Sykes
    ## 973         2016    1626194     Marcus         Thornton
    ## 974         2016    1626160       J.P.           Tokoto
    ## 975         2016    1626173     Rashad           Vaughn
    ## 976         2016    1626217      Chris           Walker
    ## 977         2016    1627361        Dez            Wells
    ## 978         2016    1626210       Alan         Williams
    ## 979         2016    1626153      Delon           Wright
    ## 980         2017    1627758        Ron            Baker
    ## 981         2017    1627735       Wade       Baldwin IV
    ## 982         2017    1627761    DeAndre           Bembry
    ## 983         2017    1627791        Ben           Bentil
    ## 984         2017    1628417      Jaron      Blossomgame
    ## 985         2017    1627762       Joel         Bolomboy
    ## 986         2017    1627763    Malcolm          Brogdon
    ## 987         2017    1627737   Marquese           Chriss
    ## 988         2017    1627766     Isaiah          Cousins
    ## 989         2017    1627767     Cheick           Diallo
    ## 990         2017    1627768      Perry            Ellis
    ## 991         2017    1627770        Kay           Felder
    ## 992         2017    1627827     Dorian     Finney-Smith
    ## 993         2017    1627771    Michael          Gbinije
    ## 994         2017    1627772     Daniel         Hamilton
    ## 995         2017    1628404       Josh             Hart
    ## 996         2017    1628502      Nigel            Hayes
    ## 997         2017     204211     Justin          Jackson
    ## 998         2017    1627743  Demetrius          Jackson
    ## 999         2017    1627744      Brice          Johnson
    ## 1000        2017    1627745     Damian            Jones
    ## 1001        2017    1627774       Jake           Layman
    ## 1002        2017    1629159     Marcus              Lee
    ## 1003        2017    1627748       Thon            Maker
    ## 1004        2017    1627775    Patrick            McCaw
    ## 1005        2017    1629005      Malik           Newman
    ## 1006        2017    1627777    Georges            Niang
    ## 1007        2017    1627778    Chinanu           Onuaku
    ## 1008        2017    1627779     Marcus            Paige
    ## 1009        2017    1627780       Gary        Payton II
    ## 1010        2017    1627752    Taurean           Prince
    ## 1011        2017    1627753       Zhou               Qi
    ## 1012        2017    1627781    Malachi       Richardson
    ## 1013        2017    1627783     Pascal           Siakam
    ## 1014        2017    1627754    Diamond            Stone
    ## 1015        2017    1628403      Caleb         Swanigan
    ## 1016        2017    1628453       Melo          Trimble
    ## 1017        2017    1627755      Tyler             Ulis
    ## 1018        2017    1627784     Jarrod           Uthoff
    ## 1019        2017    1627756     Denzel        Valentine
    ## 1020        2017    1627785     Isaiah        Whitehead
    ## 1021        2017    1627786       Troy         Williams
    ## 1022        2017    1627787       Kyle          Wiltjer
    ## 1023        2017    1627757    Stephen        Zimmerman
    ## 1024        2018    1628389     Edrice          Adebayo
    ## 1025        2018    1628959      Rawle           Alkins
    ## 1026        2018    1628386    Jarrett            Allen
    ## 1027        2018    1628443     Kadeem            Allen
    ## 1028        2018    1628387        Ike         Anigbogu
    ## 1029        2018    1628503      Jamel            Artis
    ## 1030        2018    1628407     Dwayne            Bacon
    ## 1031        2018    1628431       V.J.          Beachem
    ## 1032        2018    1628395     Jordan             Bell
    ## 1033        2018    1628417      Jaron      Blossomgame
    ## 1034        2018    1628396       Tony          Bradley
    ## 1035        2018    1628515     Isaiah          Briscoe
    ## 1036        2018    1628415     Dillon           Brooks
    ## 1037        2018    1628418     Thomas           Bryant
    ## 1038        2018    1628381       John          Collins
    ## 1039        2018    1628977    Hamidou           Diallo
    ## 1040        2018    1628416      Tyler           Dorsey
    ## 1041        2018    1628422    Damyean           Dotson
    ## 1042        2018    1628408         PJ           Dozier
    ## 1043        2018    1628393      Jawun            Evans
    ## 1044        2018    1628390   Terrance         Ferguson
    ## 1045        2018    1628385      Harry            Giles
    ## 1046        2018    1628404       Josh             Hart
    ## 1047        2018    1628502      Nigel            Hayes
    ## 1048        2018    1628439     Isaiah            Hicks
    ## 1049        2018    1628411     Wesley           Iwundu
    ## 1050        2018    1628402      Frank          Jackson
    ## 1051        2018    1628992     Justin    Jackson (UMD)
    ## 1052        2018    1628382     Justin    Jackson (UNC)
    ## 1053        2018    1628514      Peter              Jok
    ## 1054        2018    1628398       Kyle            Kuzma
    ## 1055        2018    1628388         TJ             Leaf
    ## 1056        2018    1628399      Tyler            Lydon
    ## 1057        2018    1628412      Frank        Mason III
    ## 1058        2018    1628452    Kennedy            Meeks
    ## 1059        2018    1628378    Donovan         Mitchell
    ## 1060        2018    1628420      Monte           Morris
    ## 1061        2018    1629004        Svi       Mykhailiuk
    ## 1062        2018    1628400       Semi          Ojeleye
    ## 1063        2018    1628419    Cameron           Oliver
    ## 1064        2018    1628383     Justin           Patton
    ## 1065        2018    1628397       Ivan             Rabb
    ## 1066        2018    1628432      Davon             Reed
    ## 1067        2018    1628421      Devin         Robinson
    ## 1068        2018    1628424       Kobi          Simmons
    ## 1069        2018    1628414  Sindarius        Thornwell
    ## 1070        2018    1628453       Melo          Trimble
    ## 1071        2018    1629021     Moritz           Wagner
    ## 1072        2018    1628476    Derrick       Walton Jr.
    ## 1073        2018    1628401    Derrick            White
    ## 1074        2018    1628430      Nigel    Williams-Goss
    ## 1075        2018 1962937123       Omer        Yurtseven
    ## 1076        2019    1628959      Rawle           Alkins
    ## 1077        2019    1628960    Grayson            Allen
    ## 1078        2019    1628961     Kostas    Antetokounmpo
    ## 1079        2019    1628962      Udoka         Azubuike
    ## 1080        2019    1628965     Jaylen          Barford
    ## 1081        2019    1628966      Keita       Bates-Diop
    ## 1082        2019    1628967       Tyus           Battle
    ## 1083        2019    1628968      Brian            Bowen
    ## 1084        2019    1628971      Bruce            Brown
    ## 1085        2019    1628972       Troy            Brown
    ## 1086        2019    1628973      Jalen          Brunson
    ## 1087        2019    1628974       Tony             Carr
    ## 1088        2019    1628975      Jevon           Carter
    ## 1089        2019    1628978      Donte       DiVincenzo
    ## 1090        2019    1628977    Hamidou           Diallo
    ## 1091        2019    1628979     Trevon            Duval
    ## 1092        2019    1629035     Carsen          Edwards
    ## 1093        2019    1628980      Jacob            Evans
    ## 1094        2019    1628981      Bruno         Fernando
    ## 1095        2019    1628982     Melvin          Frazier
    ## 1096        2019    1628984    Devonte           Graham
    ## 1097        2019    1628985      Devon             Hall
    ## 1098        2019    1628986     Jaylen            Hands
    ## 1099        2019    1628987      Kevin           Hervey
    ## 1100        2019    1628988      Aaron          Holiday
    ## 1101        2019    1628989      Kevin          Huerter
    ## 1102        2019    1628993      Alize          Johnson
    ## 1103        2019    1628994     George             King
    ## 1104        2019    1628996     Sagaba           Konate
    ## 1105        2019    1628997      Caleb           Martin
    ## 1106        2019    1628998       Cody           Martin
    ## 1107        2019    1628999      Yante            Maten
    ## 1108        2019    1629000    Brandon            McCoy
    ## 1109        2019    1629001  DeAnthony           Melton
    ## 1110        2019    1629002   Chimezie             Metu
    ## 1111        2019    1629003      Shake           Milton
    ## 1112        2019    1629004 Sviatoslav       Mykhailiuk
    ## 1113        2019    1629005      Malik           Newman
    ## 1114        2019    1629006       Josh           Okogie
    ## 1115        2019    1629033       Theo           Pinson
    ## 1116        2019    1629007     Jontay           Porter
    ## 1117        2019    1629009      Billy          Preston
    ## 1118        2019    1629013     Landry           Shamet
    ## 1119        2019    1629014   Anfernee           Simons
    ## 1120        2019    1629015     Zhaire            Smith
    ## 1121        2019    1629034        Ray         Spalding
    ## 1122        2019    1629016      Omari         Spellman
    ## 1123        2019    1629017      Khyri           Thomas
    ## 1124        2019    1629018       Gary            Trent
    ## 1125        2019    1629019    Allonzo            Trier
    ## 1126        2019    1629021     Moritz           Wagner
    ## 1127        2019    1629022     Lonnie           Walker
    ## 1128        2019    1629023         PJ       Washington
    ## 1129        2019    1629024     Austin            Wiley
    ## 1130        2019    1629025       Kris           Wilkes
    ## 1131        2019    1629026    Kenrich         Williams
    ## 1132        2020    1629646    Charles           Bassey
    ## 1133        2020    1629647     Darius           Bazley
    ## 1134        2020    1629648     Jordan             Bone
    ## 1135        2020    1628968      Brian         Bowen II
    ## 1136        2020    1629065         Ky           Bowman
    ## 1137        2020    1629649      Ignas       Brazdeikis
    ## 1138        2020    1629052     O'Shae         Brissett
    ## 1139        2020    1629650      Moses            Brown
    ## 1140        2020    1629634    Brandon           Clarke
    ## 1141        2020    1629651    Nicolas          Claxton
    ## 1142        2020    1629076      Tyler             Cook
    ## 1143        2020    1629056    Terence            Davis
    ## 1144        2020    1629652   Luguentz             Dort
    ## 1145        2020    1629653      Devon           Dotson
    ## 1146        2020    1629654     Carsen          Edwards
    ## 1147        2020    1629605      Tacko             Fall
    ## 1148        2020    1628981      Bruno         Fernando
    ## 1149        2020    1629655     Daniel          Gafford
    ## 1150        2020    1629656    Quentin           Grimes
    ## 1151        2020    1629657       Kyle              Guy
    ## 1152        2020    1628986     Jaylen            Hands
    ## 1153        2020    1629607      Jared           Harper
    ## 1154        2020    1629637     Jaxson            Hayes
    ## 1155        2020    1629608      Dewan        Hernandez
    ## 1156        2020    1629658      Jalen            Hoard
    ## 1157        2020    1629659      Talen    Horton-Tucker
    ## 1158        2020    1629660         Ty           Jerome
    ## 1159        2020    1629661    Cameron          Johnson
    ## 1160        2020    1629662    Mfiondu        Kabengele
    ## 1161        2020    1629663      Louis             King
    ## 1162        2020    1629664     Dedric           Lawson
    ## 1163        2020    1629665      Jalen           Lecque
    ## 1164        2020    1629642     Nassir           Little
    ## 1165        2020    1629611    Terance             Mann
    ## 1166        2020    1628998       Cody           Martin
    ## 1167        2020    1629666    Charles         Matthews
    ## 1168        2020    1629667      Jalen        McDaniels
    ## 1169        2020    1629668       Zach      Norvell Jr.
    ## 1170        2020    1629669     Jaylen           Nowell
    ## 1171        2020    1629644         KZ           Okpala
    ## 1172        2020    1629671       Miye              Oni
    ## 1173        2020    1629672       Eric         Paschall
    ## 1174        2020    1629617     Reggie            Perry
    ## 1175        2020    1629044   Shamorie            Ponds
    ## 1176        2020    1629673     Jordan            Poole
    ## 1177        2020    1629645      Kevin       Porter Jr.
    ## 1178        2020    1629674    Neemias            Queta
    ## 1179        2020    1629675        Naz             Reid
    ## 1180        2020    1629676     Isaiah             Roby
    ## 1181        2020    1629677       Luka          Samanic
    ## 1182        2020    1629678    Admiral        Schofield
    ## 1183        2020    1629621     Marial           Shayok
    ## 1184        2020    1629679   Simisola           Shittu
    ## 1185        2020    1629682    Tremont           Waters
    ## 1186        2020    1629683  Quinndary     Weatherspoon
    ## 1187        2020    1629025       Kris           Wilkes
    ## 1188        2020    1629684      Grant         Williams
    ## 1189        2020    1629685      Dylan          Windler
    ## 1190        2021    1630234    Ty-Shon        Alexander
    ## 1191        2021    1628962      Udoka         Azubuike
    ## 1192        2021    1630189      Tyler              Bey
    ## 1193        2021    1630213      Yoeli           Childs
    ## 1194        2021    1629603     Mamadi          Diakite
    ## 1195        2021    1629653      Devon           Dotson
    ## 1196        2021    1630220       Paul            Eboua
    ## 1197        2021    1629604         CJ           Elleby
    ## 1198        2021    1630235      Trent          Forrest
    ## 1199        2021    1630182       Josh            Green
    ## 1200        2021    1630204     Ashton           Hagans
    ## 1201        2021    1630221     Joshua             Hall
    ## 1202        2021    1630223      Jalen           Harris
    ## 1203        2021    1630210     Markus           Howard
    ## 1204        2021    1630222      Mason            Jones
    ## 1205        2021    1630233     Nathan           Knight
    ## 1206        2021    1630211      Karim             Mane
    ## 1207        2021    1630230       Naji         Marshall
    ## 1208        2021    1630231     Kenyon       Martin Jr.
    ## 1209        2021    1630219     Skylar             Mays
    ## 1210        2021    1630192       Zeke            Nnaji
    ## 1211        2021    1629670     Jordan            Nwora
    ## 1212        2021    1629617     Reggie            Perry
    ## 1213        2021    1630193   Immanuel         Quickley
    ## 1214        2021    1630194       Paul         Reed Jr.
    ## 1215        2021    1630208       Nick         Richards
    ## 1216        2021    1630203      Grant           Riller
    ## 1217        2021    1630199    Cassius          Stanley
    ## 1218        2021    1630205      Lamar          Stevens
    ## 1219        2021    1630179     Tyrell            Terry
    ## 1220        2021    1629681    Killian           Tillie
    ## 1221        2021    1630214     Xavier      Tillman Sr.
    ## 1222        2021    1629700      Kaleb           Wesson
    ## 1223        2021    1630232     Kahlil          Whitney
    ## 1224        2021    1630216    Cassius          Winston
    ## 1225        2021    1630218     Robert       Woodard II
    ##                    namePlayer slugPosition heightWOShoesInches heightWOShoes
    ## 1                 Malik Allen         PF-C               80.25     6' 8.25''
    ## 2               Lamont Barnes         PF-C               80.50      6' 8.5''
    ## 3                 Mario Bland           PF               77.50      6' 5.5''
    ## 4               Primoz Brezec            C               84.75     7' 0.75''
    ## 5              Speedy Claxton           PG               70.50     5' 10.5''
    ## 6                  Eric Coley        SG-SF               76.00        6' 4''
    ## 7                     Ed Cota           PG               72.25     6' 0.25''
    ## 8                Schea Cotton           SF               76.25     6' 4.25''
    ## 9               Caswell Cyrus           PF               79.75     6' 7.75''
    ## 10             Khalid El-Amin           PG               69.00        5' 9''
    ## 11             Vassil Evtimov           PF               81.75     6' 9.75''
    ## 12             Antonis Fotsis           SF               81.25     6' 9.25''
    ## 13                 Eddie Gill           PG               71.75    5' 11.75''
    ## 14               Marcus Goree        SF-PF               79.50      6' 7.5''
    ## 15               A.J. Granger           SF               79.50      6' 7.5''
    ## 16                A.J. Guyton        PG-SG               72.75     6' 0.75''
    ## 17                   Ron Hale        SG-SF               79.50      6' 7.5''
    ## 18                 Jason Hart           PG               73.75     6' 1.75''
    ## 19             Johnny Hemsley        SG-SF               74.75     6' 2.75''
    ## 20             Michael Hermon        SG-PG               74.75     6' 2.75''
    ## 21             Cory Hightower        SG-SF               77.50      6' 5.5''
    ## 22                Eddie House        SG-PG               71.75    5' 11.75''
    ## 23              Jimmie Hunter        SG-PG               75.00        6' 3''
    ## 24               Jacob Jaacks           PF               81.25     6' 9.25''
    ## 25                Marko Jaric        SG-SF               78.50      6' 6.5''
    ## 26               Nate Johnson           SF               78.50      6' 6.5''
    ## 27               Kenyon Jones         PF-C               81.00        6' 9''
    ## 28               Mark Karcher           SG               76.00        6' 4''
    ## 29                 Dan Langhi           SF               81.00        6' 9''
    ## 30                Justin Love        PG-SG               73.50      6' 1.5''
    ## 31                Mark Madsen           PF               80.00        6' 8''
    ## 32             Dan McClintock         C-PF               83.00       6' 11''
    ## 33               Pete Mickeal        SG-SF               77.00        6' 5''
    ## 34            Brian Montonati           PF               80.75     6' 8.75''
    ## 35               Gabe Muoneke        PF-SF               78.25     6' 6.25''
    ## 36             Ndongo N'diaye            C               83.50     6' 11.5''
    ## 37             Eduardo Najera        PF-SF               78.50      6' 6.5''
    ## 38            Matthew Nielsen           PF               81.00        6' 9''
    ## 39               Chris Porter        SF-PF               77.50      6' 5.5''
    ## 40              Lavor Postell           SG               76.50      6' 4.5''
    ## 41             Igor Rakocevic           PG               74.50      6' 2.5''
    ## 42              Reed Rawlings        SG-SF               78.25     6' 6.25''
    ## 43               Michael Redd        SG-SF               76.75     6' 4.75''
    ## 44                 Damon Reed         PF-C               80.50      6' 8.5''
    ## 45               Aubrey Reese           PG               70.75    5' 10.75''
    ## 46        Julius Doc Robinson           PG               72.00        6' 0''
    ## 47               Pepe Sanchez           PG               75.00        6' 3''
    ## 48            Matt Santangelo           PG               72.00        6' 0''
    ## 49                Alex Scales        SG-PG               74.25     6' 2.25''
    ## 50              Karim Shabazz            C               83.50     6' 11.5''
    ## 51              Nick Sheppard         PF-C               81.50      6' 9.5''
    ## 52                 Mike Smith           SF               78.50      6' 6.5''
    ## 53               Jabari Smith         PF-C               81.75     6' 9.75''
    ## 54           Jarrett Stephens           PF               77.25     6' 5.25''
    ## 55            Bootsy Thornton           SG               75.50      6' 3.5''
    ## 56               Jaquay Walls           PG               73.25     6' 1.25''
    ## 57             Jameel Watkins         PF-C               81.50      6' 9.5''
    ## 58            Adam Allenspach            C               83.50     6' 11.5''
    ## 59             Gilbert Arenas           SG               74.25     6' 2.25''
    ## 60          Brandon Armstrong           SG               75.50      6' 3.5''
    ## 61              Shane Battier        SF-PF               80.25     6' 8.25''
    ## 62             Cookie Belcher        SG-PG               75.00        6' 3''
    ## 63               Charlie Bell           PG               74.50      6' 2.5''
    ## 64      Ruben Boumtje-Boumtje            C               83.50     6' 11.5''
    ## 65              Calvin Bowman           PF               80.75     6' 8.75''
    ## 66            Michael Bradley           PF               81.50      6' 9.5''
    ## 67             Jamison Brewer           PG               74.50      6' 2.5''
    ## 68                Kwame Brown         PF-C               82.00       6' 10''
    ## 69               Damone Brown           SF               80.25     6' 8.25''
    ## 70           SirValiant Brown           PG               71.00       5' 11''
    ## 71               Ryan Carroll        SF-SG               76.00        6' 4''
    ## 72             Tyson Chandler        SF-PF               83.50     6' 11.5''
    ## 73             Eric Chenowith            C               85.00        7' 1''
    ## 74                 Sam Clancy           PF               78.25     6' 6.25''
    ## 75          Gyasi Cline-Heard           PF               78.25     6' 6.25''
    ## 76             Jarron Collins         C-PF               81.25     6' 9.25''
    ## 77              Jason Collins         PF-C               82.25    6' 10.25''
    ## 78                  Omar Cook           PG               71.50     5' 11.5''
    ## 79                 Eddy Curry            C               82.50     6' 10.5''
    ## 80                Tate Decker           SF               81.25     6' 9.25''
    ## 81              DeSagana Diop            C               82.00       6' 10''
    ## 82                Robb Dryden            C               85.00        7' 1''
    ## 83              Maurice Evans           SG               76.25     6' 4.25''
    ## 84              Anthony Evans           PF               78.00        6' 6''
    ## 85               Benjamin Eze         C-PF               80.75     6' 8.75''
    ## 86             Kimani Ffriend         C-PF               81.50      6' 9.5''
    ## 87                 Alton Ford           PF               80.25     6' 8.25''
    ## 88               Joseph Forte           SG               75.50      6' 3.5''
    ## 89              Jason Gardner           PG               69.75     5' 9.75''
    ## 90                Jerry Green        PG-SG               73.50      6' 1.5''
    ## 91              Kenny Gregory           SF               75.50      6' 3.5''
    ## 92              Eddie Griffin        PF-SF               81.25     6' 9.25''
    ## 93            Trenton Hassell        SG-SF               76.25     6' 4.25''
    ## 94                Kirk Haston        PF-SF               80.00        6' 8''
    ## 95            Brendan Haywood            C               83.75    6' 11.75''
    ## 96              Michael Hicks           SG               76.00        6' 4''
    ## 97              Steven Hunter         C-PF               82.25    6' 10.25''
    ## 98               Andre Hutson           PF               80.25     6' 8.25''
    ## 99                 Nate James           SF               76.75     6' 4.75''
    ## 100         Richard Jefferson           SF               79.25     6' 7.25''
    ## 101            Horace Jenkins           PG               71.75    5' 11.75''
    ## 102             Darrell Johns            C               83.25    6' 11.25''
    ## 103               Ken Johnson            C               82.75    6' 10.75''
    ## 104               Joe Johnson        SF-SG               78.75     6' 6.75''
    ## 105              Darren Kelly        SG-PG               74.00        6' 2''
    ## 106              Sean Lampley           SF               77.50      6' 5.5''
    ## 107              Zach Marbury        PG-SG               73.25     6' 1.25''
    ## 108            Mike Mardesich         C-PF               82.25    6' 10.25''
    ## 109            DeMarcus Minor           SG               75.25     6' 3.25''
    ## 110              Jamario Moon           SF               78.50      6' 6.5''
    ## 111               Troy Murphy           PF               81.75     6' 9.75''
    ## 112               Troy Ostler        PF-SF               80.75     6' 8.75''
    ## 113      Lazaros Papadopoulos         C-PF               82.25    6' 10.25''
    ## 114           Rashad Phillips           PG               69.00        5' 9''
    ## 115          Demetrius Porter           PG               70.75    5' 10.75''
    ## 116             Martin Rancik        PF-SF               80.75     6' 8.75''
    ## 117          Jason Richardson        SF-SG               76.75     6' 4.75''
    ## 118         Norman Richardson           SG               75.50      6' 3.5''
    ## 119              Jeryl Sasser        PG-SG               77.50      6' 5.5''
    ## 120         Kenny Satterfield           PG               73.00        6' 1''
    ## 121          Brian Scalabrine        PF-SF               80.75     6' 8.75''
    ## 122               Lee Scruggs        PF-SF               82.25    6' 10.25''
    ## 123             Bobby Simmons           SF               78.25     6' 6.25''
    ## 124              Will Solomon        PG-SG               71.75    5' 11.75''
    ## 125            Greg Stevenson           SF               75.75     6' 3.75''
    ## 126           Damone Thornton           PF               78.75     6' 6.75''
    ## 127            Jamaal Tinsley           PG               72.75     6' 0.75''
    ## 128               Tory Walker           SF               78.75     6' 6.75''
    ## 129           Souleymane Wane            C               80.50      6' 8.5''
    ## 130               Earl Watson           PG               72.00        6' 0''
    ## 131              Rodney White           SF               79.50      6' 7.5''
    ## 132               Loren Woods         C-PF               84.75     7' 0.75''
    ## 133              Brent Wright        PF-SF               79.75     6' 7.75''
    ## 134            Michael Wright           PF               79.00        6' 7''
    ## 135                     Nene            PF               81.25     6' 9.25''
    ## 136          Robert Archibald         PF-C               82.00       6' 10''
    ## 137             Maurice Baker           PG               71.75    5' 11.75''
    ## 138               Matt Barnes           SF               78.75     6' 6.75''
    ## 139              Lubos Barton        SF-SG               78.50      6' 6.5''
    ## 140              Lonny Baxter         PF-C               78.25     6' 6.25''
    ## 141                Lee Benson        SF-PF               80.25     6' 8.25''
    ## 142             Carlos Boozer           PF               79.75     6' 7.75''
    ## 143          Curtis Borchardt            C               82.75    6' 10.75''
    ## 144               J.R. Bremer        SG-PG               73.25     6' 1.25''
    ## 145               Brian Brown        PG-SG               74.50      6' 2.5''
    ## 146             Sylvere Bryan           PF               81.00        6' 9''
    ## 147             Chris Burgess            C               82.00       6' 10''
    ## 148              Caron Butler        SF-SG               77.25     6' 5.25''
    ## 149             Rasual Butler           SF               78.75     6' 6.75''
    ## 150              Mire Chatman        PG-SG               72.00        6' 0''
    ## 151      Chris Christoffersen            C               84.75     7' 0.75''
    ## 152              Craig Dawson        SG-SF               75.50      6' 3.5''
    ## 153             Mike Dunleavy           SF               80.00        6' 8''
    ## 154               Teddy Dupay           PG               69.50      5' 9.5''
    ## 155           Corsley Edwards           PF               79.25     6' 7.25''
    ## 156                Andy Ellis           PF               81.25     6' 9.25''
    ## 157                Melvin Ely           PF               81.00        6' 9''
    ## 158              Reggie Evans           PF               79.50      6' 7.5''
    ## 159               Drew Gooden           PF               80.75     6' 8.75''
    ## 160              David Graves           SG               76.50      6' 4.5''
    ## 161                Lynn Greer           PG               72.25     6' 0.25''
    ## 162            Anthony Grundy        PG-SG               72.75     6' 0.75''
    ## 163            Marcus Haislip           PF               80.50      6' 8.5''
    ## 164             Damon Hancock        SG-PG               74.75     6' 2.75''
    ## 165           Greg Harrington           PG               73.00        6' 1''
    ## 166             Udonis Haslem           PF               79.75     6' 7.75''
    ## 167             Cordell Henry           PG               68.00        5' 8''
    ## 168             Randy Holcomb        SF-PF               79.00        6' 7''
    ## 169             Ryan Humphrey        PF-SF               78.00        6' 6''
    ## 170            Jared Jeffries           PF               82.00       6' 10''
    ## 171            Jason Jennings            C               83.25    6' 11.25''
    ## 172              Lonnie Jones         C-PF               82.25    6' 10.25''
    ## 173                Fred Jones           SG               74.25     6' 2.25''
    ## 174              Sean Kennedy           PG               72.75     6' 0.75''
    ## 175                 Kris Lang         C-PF               81.25     6' 9.25''
    ## 176           Muhammed Lasege         C-PF               80.75     6' 8.75''
    ## 177                Kevin Lyde         C-PF               80.75     6' 8.75''
    ## 178               Tito Maddox           PG               75.00        6' 3''
    ## 179               Kei Madison           SF               79.75     6' 7.75''
    ## 180                Elvin Mims        SF-SG               76.25     6' 4.25''
    ## 181              Byron Mouton        SG-SF               76.50      6' 4.5''
    ## 182             Ronald Murray           SG               75.00        6' 3''
    ## 183               Uche Okafor         PF-C               81.00        6' 9''
    ## 184             Jannero Pargo           PG               72.25     6' 0.25''
    ## 185              Smush Parker        PG-SG               74.50      6' 2.5''
    ## 186               Luke Recker        SG-SF               76.75     6' 4.75''
    ## 187             Rolan Roberts           PF               77.75     6' 5.75''
    ## 188           Travis Robinson           SF               77.25     6' 5.25''
    ## 189               Kareem Rush        SG-SF               76.75     6' 4.75''
    ## 190              Brooks Sales         PF-C               81.00        6' 9''
    ## 191              John Salmons        SF-SG               77.50      6' 5.5''
    ## 192           Predrag Savovic           SG               76.75     6' 4.75''
    ## 193            Julian Sensley        PF-SF               79.00        6' 7''
    ## 194          Preston Shumpert           SF               78.00        6' 6''
    ## 195                Tamar Slay           SF               78.75     6' 6.75''
    ## 196           Darius Songaila           PF               80.00        6' 8''
    ## 197         Amar'e Stoudemire           PF               80.50      6' 8.5''
    ## 198             Marcus Taylor           PG               73.00        6' 1''
    ## 199              Jobey Thomas           SG               74.75     6' 2.75''
    ## 200             Dajuan Wagner        PG-SG               72.75     6' 0.75''
    ## 201              Chris Wilcox         PF-C               80.25     6' 8.25''
    ## 202            Frank Williams           PG               73.50      6' 1.5''
    ## 203              Jay Williams           PG               72.25     6' 0.25''
    ## 204           George Williams        PF-SF               77.75     6' 5.75''
    ## 205              Qyntel Woods        SF-SG               79.00        6' 7''
    ## 206         Vincent Yarbrough           SF               77.50      6' 5.5''
    ## 207         Aloysius Anagonye           PF               78.75     6' 6.75''
    ## 208             Rick Anderson        SF-PF               80.25     6' 8.25''
    ## 209           Carmelo Anthony           SF               78.25     6' 6.25''
    ## 210            Jerome Beasley        SF-PF               81.00        6' 9''
    ## 211                 Troy Bell           PG               72.25     6' 0.25''
    ## 212          LaVell Blanchard           SF               78.00        6' 6''
    ## 213              Keith Bogans           SG               76.25     6' 4.25''
    ## 214               Matt Bonner        PF-SF               80.50      6' 8.5''
    ## 215                Chris Bosh           PF               82.25    6' 10.25''
    ## 216          Jermaine Boyette        PG-SG               73.00        6' 1''
    ## 217         Souleymane Camara        PF-SF               81.75     6' 9.75''
    ## 218              Matt Carroll        SG-SF               77.00        6' 5''
    ## 219             Nick Collison           PF               80.75     6' 8.75''
    ## 220                Brian Cook        PF-SF               81.50      6' 9.5''
    ## 221             Joel Cornette        PF-SF               80.25     6' 8.25''
    ## 222           Marquis Daniels        SG-PG               77.25     6' 5.25''
    ## 223         Aleksander Djuric         C-PF               82.00       6' 10''
    ## 224             Ruben Douglas           SG               75.50      6' 3.5''
    ## 225             Ronald Dupree        SF-SG               78.00        6' 6''
    ## 226              Carl English        SG-PG               76.00        6' 4''
    ## 227                   Ebi Ere           SG               75.50      6' 3.5''
    ## 228            Marquis Estill         PF-C               80.50      6' 8.5''
    ## 229                 T.J. Ford           PG               71.00       5' 11''
    ## 230             Jason Gardner           PG               69.75     5' 9.75''
    ## 231              Willie Green        SG-PG               74.50      6' 2.5''
    ## 232           Justin Hamilton        PG-SG               75.00        6' 3''
    ## 233             Travis Hansen           SG               76.50      6' 4.5''
    ## 234             Trevor Harvey         C-PF               81.50      6' 9.5''
    ## 235             Marcus Hatten           PG               72.75     6' 0.75''
    ## 236              Kirk Hinrich        PG-SG               74.75     6' 2.75''
    ## 237              Jerry Holman           PF               80.75     6' 8.75''
    ## 238               Josh Howard        SF-SG               77.25     6' 5.25''
    ## 239            Brandon Hunter           PF               78.50      6' 6.5''
    ## 240              Sani Ibrahim         C-PF               80.50      6' 8.5''
    ## 241          Michael Ignerski        PF-SF               81.50      6' 9.5''
    ## 242            Robert Jackson         PF-C               79.75     6' 7.75''
    ## 243           Britton Johnsen        SF-PF               82.00       6' 10''
    ## 244             Dahntay Jones        SG-SF               76.75     6' 4.75''
    ## 245               James Jones           SF               79.25     6' 7.25''
    ## 246               Chris Kaman         C-PF               83.50     6' 11.5''
    ## 247              Jason Kapono        SG-SF               78.50      6' 6.5''
    ## 248                Jason Keep         C-PF               81.75     6' 9.75''
    ## 249              Bernard King        SG-PG               75.75     6' 3.75''
    ## 250               Kyle Korver           SF               78.25     6' 6.25''
    ## 251                James Lang         C-PF               80.25     6' 8.25''
    ## 252             Donald Little         PF-C               81.50      6' 9.5''
    ## 253              Chris Massie         PF-C               78.50      6' 6.5''
    ## 254             Will McDonald         C-PF               81.50      6' 9.5''
    ## 255             Darko Milicic           PF               83.50     6' 11.5''
    ## 256               Jeff Newton           PF               80.00        6' 8''
    ## 257         Uche Nsonwu-Amadi         PF-C               79.75     6' 7.75''
    ## 258            Ugonna Okyekwe           SF               79.25     6' 7.25''
    ## 259             Marlon Parmer           PG               72.75     6' 0.75''
    ## 260            Stephane Pelle        PF-SF               79.00        6' 7''
    ## 261               Kirk Penney        SG-SF               77.00        6' 5''
    ## 262           Pavel Podkolzin            C               87.50      7' 3.5''
    ## 263               Josh Powell        PF-SF               80.00        6' 8''
    ## 264              Hollis Price        PG-SG               71.50     5' 11.5''
    ## 265              Luke Ridnour           PG               73.00        6' 1''
    ## 266                 Joe Shipp           SG               76.00        6' 4''
    ## 267                  Ron Slay        SF-PF               78.00        6' 6''
    ## 268              Theron Smith        PF-SF               78.50      6' 6.5''
    ## 269               Tommy Smith        PF-SF               81.00        6' 9''
    ## 270              Marvin Stone         C-PF               81.50      6' 9.5''
    ## 271          Michael Sweetney           PF               79.25     6' 7.25''
    ## 272               Dwyane Wade        SG-SF               75.75     6' 3.75''
    ## 273             Wayne Wallace           PF               79.25     6' 7.25''
    ## 274                David West        PF-SF               80.25     6' 8.25''
    ## 275             Wesley Wilson         C-PF               82.25    6' 10.25''
    ## 276                Doug Wrenn        SF-PF               77.75     6' 5.75''
    ## 277         Derrick Zimmerman           PG               73.00        6' 1''
    ## 278                Tony Allen           SG               75.50      6' 3.5''
    ## 279              Trevor Ariza        SG-SF               79.00        6' 7''
    ## 280             Andre Barrett           PG               68.75     5' 8.75''
    ## 281              Brandon Bass           PF               78.25     6' 6.25''
    ## 282           Brian Boddicker        SF-PF               79.50      6' 7.5''
    ## 283                Tim Bowers        PG-SG               72.50      6' 0.5''
    ## 284               Andre Brown         PF-C               80.00        6' 8''
    ## 285             Jackie Butler            C               80.00        6' 8''
    ## 286          Aleksandar Capin           PG               72.00        6' 0''
    ## 287                 Ales Chan            C               84.00        7' 0''
    ## 288            Josh Childress        SG-SF               77.75     6' 5.75''
    ## 289               TJ Cummings           SF               79.50      6' 7.5''
    ## 290              Erik Daniels        SF-PF               78.25     6' 6.25''
    ## 291                 Luol Deng           SF               79.00        6' 7''
    ## 292            Marcus Douthit         C-PF               81.50      6' 9.5''
    ## 293               Chris Duhon           PG               72.00        6' 0''
    ## 294             Desmon Farmer           SG               76.50      6' 4.5''
    ## 295               Luis Flores        PG-SG               72.50      6' 0.5''
    ## 296               Matt Freije        SF-PF               80.25     6' 8.25''
    ## 297             Chris Garnett         C-PF               80.25     6' 8.25''
    ## 298                Ryan Gomes        SF-PF               78.50      6' 6.5''
    ## 299                Ben Gordon        PG-SG               73.00        6' 1''
    ## 300              Devin Harris           PG               73.75     6' 1.75''
    ## 301             Dwight Howard         PF-C               81.00        6' 9''
    ## 302            Rolando Howell           PF               79.50      6' 7.5''
    ## 303            Kris Humphries           PF               80.25     6' 8.25''
    ## 304            Andre Iguodala        SG-SF               77.75     6' 5.75''
    ## 305                Martin Iti         C-PF               80.50      6' 8.5''
    ## 306              Luke Jackson        SF-SG               78.00        6' 6''
    ## 307              Al Jefferson        PF-SF               80.50      6' 8.5''
    ## 308            Arthur Johnson         PF-C               79.00        6' 7''
    ## 309             Ivan Koljevic           PG               73.00        6' 1''
    ## 310          Shaun Livingston        PG-SG               78.25     6' 6.25''
    ## 311             Jaime Lloreda           PF               78.75     6' 6.75''
    ## 312           Bryant Matthews        SF-SG               77.75     6' 5.75''
    ## 313             Marcus Melvin        SF-PF               78.75     6' 6.75''
    ## 314               Rich Melzer        PF-SF               79.50      6' 7.5''
    ## 315              Ricky Minard        PG-SG               75.25     6' 3.25''
    ## 316              Marcus Moore        PG-SG               76.00        6' 4''
    ## 317               James Moore        PF-SF               78.50      6' 6.5''
    ## 318          Michel Morandais        SG-SF               75.75     6' 3.75''
    ## 319             Jameer Nelson           PG               71.00       5' 11''
    ## 320          Misan Nikagbatse        SG-PG               75.50      6' 3.5''
    ## 321              Emeka Okafor         C-PF               80.75     6' 8.75''
    ## 322               Randall Orr        PF-SF               81.00        6' 9''
    ## 323                Dylan Page           SF               79.50      6' 7.5''
    ## 324             Drago Pasalic           PF               81.25     6' 9.25''
    ## 325           Rickey Paulding           SG               74.75     6' 2.75''
    ## 326               Kelvin Pena        SG-PG               74.25     6' 2.25''
    ## 327             Omar Quintero           PG               69.75     5' 9.75''
    ## 328               Peter Ramos            C               85.75     7' 1.75''
    ## 329              Jared Reiner         C-PF               82.00       6' 10''
    ## 330          Lawrence Roberts           PF               79.50      6' 7.5''
    ## 331          Bernard Robinson        SG-SF               77.25     6' 5.25''
    ## 332             Nate Robinson           PG               67.75     5' 7.75''
    ## 333            Aerick Sanders           PF               78.50      6' 6.5''
    ## 334               Romain Sato           SG               74.00        6' 2''
    ## 335           Blagota Sekulic        SF-PF               81.25     6' 9.25''
    ## 336                Josh Smith        SG-SF               79.00        6' 7''
    ## 337                J.R. Smith        SG-SF               77.50      6' 5.5''
    ## 338               Kirk Snyder        SG-SF               78.00        6' 6''
    ## 339                  Pape Sow         PF-C               80.50      6' 8.5''
    ## 340            Tiago Splitter         C-PF               82.25    6' 10.25''
    ## 341           Tom Timmerrmans         PF-C               82.50     6' 10.5''
    ## 342               Marko Tomas        SG-PG               78.50      6' 6.5''
    ## 343            Jackson Vroman         PF-C               81.25     6' 9.25''
    ## 344              Delonte West        PG-SG               73.50      6' 1.5''
    ## 345            Damien Wilkins        SG-SF               76.50      6' 4.5''
    ## 346             Mike Williams           SF               77.50      6' 5.5''
    ## 347             Nate Williams        PF-SF               80.50      6' 8.5''
    ## 348             Rashad Wright           PG               72.50      6' 0.5''
    ## 349                Alex Acker           SG               75.75     6' 3.75''
    ## 350             Deji Akindele         C-PF               81.50      6' 9.5''
    ## 351             Alan Anderson        SG-SF               76.50      6' 4.5''
    ## 352  Martynas Andriuskevicius         C-PF               85.25     7' 1.25''
    ## 353                Sean Banks           SF               77.75     6' 5.75''
    ## 354              Eddie Basden        SG-SF               76.25     6' 4.25''
    ## 355                 Mike Bell        SF-PF               80.75     6' 8.75''
    ## 356              Andrew Bogut         C-PF               83.00       6' 11''
    ## 357                Will Bynum        PG-SG               70.50     5' 10.5''
    ## 358               Will Conroy           PG               73.00        6' 1''
    ## 359         Taylor Coppenrath        PF-SF               79.75     6' 7.75''
    ## 360             Travis Diener        PG-SG               71.75    5' 11.75''
    ## 361                 Ike Diogu           PF               78.50      6' 6.5''
    ## 362              Daryl Dorsey           PG               71.25    5' 11.25''
    ## 363               Monta Ellis        SG-PG               74.25     6' 2.25''
    ## 364            Raymond Felton           PG               71.50     5' 11.5''
    ## 365            Rudy Fernandez           SG               76.75     6' 4.75''
    ## 366              D'Or Fischer         C-PF               81.25     6' 9.25''
    ## 367                Eddy Fobbs         C-PF               82.25    6' 10.25''
    ## 368              Sharrod Ford           PF               79.25     6' 7.25''
    ## 369             Channing Frye         PF-C               81.50      6' 9.5''
    ## 370                  Deng Gai         PF-C               79.75     6' 7.75''
    ## 371          Francisco Garcia        SF-SG               77.75     6' 5.75''
    ## 372            John Gilchrist           PG               73.25     6' 1.25''
    ## 373             Marcin Gortat         PF-C               83.00       6' 11''
    ## 374               Joey Graham           SF               77.25     6' 5.25''
    ## 375             Danny Granger                            79.50      6' 7.5''
    ## 376              Gerald Green           SF               78.00        6' 6''
    ## 377             Charles Hayes        SF-PF               77.50      6' 5.5''
    ## 378               Luther Head        PG-SG               74.00        6' 2''
    ## 379              Julius Hodge        SG-SF               78.00        6' 6''
    ## 380            Ersan Ilyasova           SF               79.75     6' 7.75''
    ## 381              Jarrett Jack           PG               74.50      6' 2.5''
    ## 382              Dwayne Jones         C-PF               80.50      6' 8.5''
    ## 383       Mindaugas Katelynas        SF-PF               79.50      6' 7.5''
    ## 384               Jason Klotz         C-PF               80.50      6' 8.5''
    ## 385              Carl Krauser        PG-SG               71.50     5' 11.5''
    ## 386            Keith Langford        SG-SF               74.25     6' 2.25''
    ## 387                 David Lee           PF               79.75     6' 7.75''
    ## 388            John Lucas III           PG               69.25     5' 9.25''
    ## 389            Rawle Marshall        SG-SF               76.50      6' 4.5''
    ## 390                  Sean May           PF               79.00        6' 7''
    ## 391            Rashad McCants           SG               74.75     6' 2.75''
    ## 392             Ivan McFarlin           PF               77.50      6' 5.5''
    ## 393               Juan Mendez        PF-SF               77.25     6' 5.25''
    ## 394               Aaron Miles           PG               72.00        6' 0''
    ## 395               Ellis Myles           PF               78.00        6' 6''
    ## 396            Larry O'Bannon           SG               75.00        6' 3''
    ## 397             Drago Pasalic        SF-PF               81.50      6' 9.5''
    ## 398                Chris Paul           PG               71.75    5' 11.75''
    ## 399             Carlos Powell        SF-PF               77.25     6' 5.25''
    ## 400              Ronnie Price           PG               72.75     6' 0.75''
    ## 401          Anthony Roberson           PG               72.75     6' 0.75''
    ## 402              Brandon Rush        SG-SF               77.00        6' 5''
    ## 403           Luke Schenscher            C               83.75    6' 11.75''
    ## 404              Wayne Simien           PF               79.25     6' 7.25''
    ## 405               Tre Simmons        SG-SF               76.00        6' 4''
    ## 406               David Simon         C-PF               80.00        6' 8''
    ## 407                Chris Taft           PF               80.50      6' 8.5''
    ## 408              Chris Thomas           PG               71.75    5' 11.75''
    ## 409               Omar Thomas        SF-SG               75.25     6' 3.25''
    ## 410              Ronny Turiaf           PF               80.00        6' 8''
    ## 411        Charlie Villanueva        PF-SF               81.00        6' 9''
    ## 412           Martell Webster           SF               78.00        6' 6''
    ## 413             Robert Whaley         PF-C               80.00        6' 8''
    ## 414            Deron Williams           PG               73.75     6' 1.75''
    ## 415            Jawad Williams           SF               79.00        6' 7''
    ## 416           Marvin Williams        PF-SF               79.00        6' 7''
    ## 417             Bracey Wright        SG-PG               73.25     6' 1.25''
    ## 418            Antoine Wright        SF-SG               77.25     6' 5.25''
    ## 419             Kenny Adeleke         PF-C               78.50      6' 6.5''
    ## 420              Maurice Ager        SG-PG               75.25     6' 3.25''
    ## 421         LaMarcus Aldridge         PF-C               82.00       6' 10''
    ## 422             Morris Almond        SG-SF               76.75     6' 4.75''
    ## 423              Lou Amundson           PF               79.20      6' 7.2''
    ## 424           Rashad Anderson           SG               75.25     6' 3.25''
    ## 425          Hilton Armstrong         C-PF               81.50      6' 9.5''
    ## 426           James Augustine           PF               80.25     6' 8.25''
    ## 427           Renaldo Balkman        SF-PF               77.25     6' 5.25''
    ## 428              J.P. Batista           PF               79.50      6' 7.5''
    ## 429              Will Blalock           PG               71.25    5' 11.25''
    ## 430            Brandon Bowman           SF               78.75     6' 6.75''
    ## 431             Ronnie Brewer        SG-PG               77.75     6' 5.75''
    ## 432              Denham Brown        SG-SF               76.00        6' 4''
    ## 433              Brad Buckman           PF               79.25     6' 7.25''
    ## 434          Nik Caner-Medley           SF               78.50      6' 6.5''
    ## 435             Rodney Carney        SG-SF               76.50      6' 4.5''
    ## 436             Keydren Clark           PG               69.00        5' 9''
    ## 437             Mardy Collins        PG-SG               76.25     6' 4.25''
    ## 438               Taquan Dean        SG-PG               74.00        6' 2''
    ## 439             Terence Dials         PF-C               79.75     6' 7.75''
    ## 440              Sean Dockery        PG-SG               72.75     6' 0.75''
    ## 441             Jordan Farmar           PG               72.75     6' 0.75''
    ## 442                Randy Foye        SG-PG               74.25     6' 2.25''
    ## 443             Torin Francis         PF-C               81.00        6' 9''
    ## 444                  Rudy Gay        SF-PF               79.00        6' 7''
    ## 445               Nick George        SF-SG               76.00        6' 4''
    ## 446                  Taj Gray        PF-SF               79.25     6' 7.25''
    ## 447              Dan Grunfeld        SF-SG               76.75     6' 4.75''
    ## 448              Matt Haryasz         PF-C               81.75     6' 9.75''
    ## 449                Eric Hicks           PF               76.75     6' 4.75''
    ## 450               Tedric Hill        SF-PF               79.75     6' 7.75''
    ## 451             Daniel Horton        PG-SG               72.50      6' 0.5''
    ## 452             Solomon Jones         PF-C               80.50      6' 8.5''
    ## 453               Bobby Jones        SF-SG               77.75     6' 5.75''
    ## 454              Viktor Keyru        SG-PG               77.25     6' 5.25''
    ## 455       Marco Killingsworth           PF               77.50      6' 5.5''
    ## 456            Tarence Kinsey        SF-SG               76.75     6' 4.75''
    ## 457              Carl Krauser           PG               71.75    5' 11.75''
    ## 458              Chris McCray           SG               75.00        6' 3''
    ## 459            Gerry McNamara        PG-SG               71.75    5' 11.75''
    ## 460         Pops Mensah-Bonsu         PF-C               79.50      6' 7.5''
    ## 461               Paul Miller         PF-C               80.50      6' 8.5''
    ## 462              Paul Millsap           PF               78.25     6' 6.25''
    ## 463           Dwayne Mitchell        SG-SF               74.00        6' 2''
    ## 464             Adam Morrison           SF               78.50      6' 6.5''
    ## 465            Yemi Nicholson         PF-C               79.75     6' 7.75''
    ## 466                David Noel        SF-SG               76.50      6' 4.5''
    ## 467               Steve Novak           SF               80.00        6' 8''
    ## 468          Patrick O'Bryant            C               83.00       6' 11''
    ## 469            Danilo Pinnock        PG-SG               74.75     6' 2.75''
    ## 470               Chris Quinn        PG-SG               72.50      6' 0.5''
    ## 471                 Allan Ray           SG               73.00        6' 1''
    ## 472                 JJ Redick           SG               76.00        6' 4''
    ## 473         Antywane Robinson        SF-PF               78.25     6' 6.25''
    ## 474               Brandon Roy        SG-SF               77.25     6' 5.25''
    ## 475              Blake Schilb        PG-SG               77.25     6' 5.25''
    ## 476             Mouhamed Sene            C               83.00       6' 11''
    ## 477            Cedric Simmons         PF-C               80.25     6' 8.25''
    ## 478          Marcus Slaughter        PF-SF               79.00        6' 7''
    ## 479               Craig Smith           PF               77.50      6' 5.5''
    ## 480              Steven Smith        SF-PF               79.50      6' 7.5''
    ## 481         Michael Southhall            C               81.50      6' 9.5''
    ## 482               Frans Steyn            C               85.25     7' 1.25''
    ## 483            Curtis Stinson           SG               73.75     6' 1.75''
    ## 484              Tyrus Thomas        PF-SF               79.25     6' 7.25''
    ## 485               Joah Tucker        SF-SG               75.75     6' 3.75''
    ## 486             Ian Vouyoukas         C-PF               80.50      6' 8.5''
    ## 487         Darius Washington           PG               71.50     5' 11.5''
    ## 488                 CJ Watson           PG               72.50      6' 0.5''
    ## 489           Justin Williams         C-PF               79.25     6' 7.25''
    ## 490          Shelden Williams         PF-C               79.50      6' 7.5''
    ## 491           Shawne Williams        SF-PF               79.25     6' 7.25''
    ## 492             Eric Williams         C-PF               79.25     6' 7.25''
    ## 493           Marcus Williams           PG               74.00        6' 2''
    ## 494            Curtis Withers        PF-SF               78.00        6' 6''
    ## 495            Mohamed Abukar           SF               80.00        6' 8''
    ## 496              Mario Boggan           PF               77.00        6' 5''
    ## 497            Craig Bradshaw           PF               80.00        6' 8''
    ## 498              Corey Brewer        SF-SG               78.00        6' 6''
    ## 499              Aaron Brooks           PG               70.00       5' 10''
    ## 500               Bobby Brown           PG               71.00       5' 11''
    ## 501            Russell Carter           SG               75.00        6' 3''
    ## 502           Coleman Collins           PF               80.00        6' 8''
    ## 503               Mike Conley           PG               71.00       5' 11''
    ## 504              Daequan Cook           SG               75.00        6' 3''
    ## 505              Ryvon Covile         C-PF               79.00        6' 7''
    ## 506        Javaris Crittenton           PG               75.00        6' 3''
    ## 507         Jermareo Davidson           PF               81.00        6' 9''
    ## 508            Zabian Dowdell           PG               73.00        6' 1''
    ## 509              Jared Dudley           SF               77.00        6' 5''
    ## 510              Kevin Durant        SF-PF               81.00        6' 9''
    ## 511           Rashaun Freeman           PF               78.00        6' 6''
    ## 512                Aaron Gray            C               84.00        7' 0''
    ## 513                Jeff Green           SF               79.00        6' 7''
    ## 514             Taurean Green           PG               71.00       5' 11''
    ## 515               Caleb Green           PF               78.00        6' 6''
    ## 516             Spencer Hawes           PF               82.00       6' 10''
    ## 517             Brandon Heath           PG               74.00        6' 2''
    ## 518              Herbert Hill            C               79.00        6' 7''
    ## 519                Al Horford           PF               80.00        6' 8''
    ## 520            Quinton Hosley        SF-SG               77.00        6' 5''
    ## 521              James Hughes            C               81.00        6' 9''
    ## 522               Jeremy Hunt           SG               74.00        6' 2''
    ## 523              Ekene Ibekwe           PF               80.00        6' 8''
    ## 524             Dominic James           PG               70.00       5' 10''
    ## 525              Trey Johnson           SG               75.00        6' 3''
    ## 526     Rashad Jones-Jennings           PF               78.00        6' 6''
    ## 527              Jared Jordan           PG               72.00        6' 0''
    ## 528                 Coby Karl        SG-PG               75.00        6' 3''
    ## 529      Antanas Kavaliauskas           PF               81.00        6' 9''
    ## 530               Carl Landry           PF               79.00        6' 7''
    ## 531            Stephane Lasme           PF               77.00        6' 5''
    ## 532                  Acie Law           PG               74.00        6' 2''
    ## 533               Marko Lekic            C               81.00        6' 9''
    ## 534                 Ron Lewis           SG               74.00        6' 2''
    ## 535            Cartier Martin           SF               76.00        6' 4''
    ## 536           Dominic McGuire           SF               79.00        6' 7''
    ## 537            Josh McRoberts           PF               80.00        6' 8''
    ## 538               Sammy Mejia           SG               77.00        6' 5''
    ## 539               Brad Newley           SG               77.00        6' 5''
    ## 540          Demetris Nichols           SF               78.00        6' 6''
    ## 541               Joakim Noah           PF               82.00       6' 10''
    ## 542                 Greg Oden            C               83.00       6' 11''
    ## 543            Ivan Radenovic           PF               81.00        6' 9''
    ## 544               JR Reynolds           PG               72.00        6' 0''
    ## 545             Chris Richard           PF               79.00        6' 7''
    ## 546          Dustin Salisbery           SG               75.00        6' 3''
    ## 547              Blake Schilb           SG               77.00        6' 5''
    ## 548         Renaldas Seibutis           SG               76.00        6' 4''
    ## 549            Ramon Sessions           PG               74.00        6' 2''
    ## 550            Mustafa Shakur           PG               75.00        6' 3''
    ## 551               Jason Smith           PF               82.75    6' 10.75''
    ## 552             DJ Strawberry           SG               75.75     6' 3.75''
    ## 553            Rodney Stuckey        SG-PG               75.75     6' 3.75''
    ## 554            Curtis Sumpter           SF               78.00        6' 6''
    ## 555                   Yue Sun           SF               79.75     6' 7.75''
    ## 556              Jamaal Tatum           PG               71.50     5' 11.5''
    ## 557            Reyshawn Terry           SF               78.75     6' 6.75''
    ## 558               Al Thornton           SF               77.75     6' 5.75''
    ## 559          Anthony Tolliver           PF               79.00        6' 7''
    ## 560                Ali Traore         C-PF               80.75     6' 8.75''
    ## 561               Kyle Visser            C               82.00       6' 10''
    ## 562            Darryl Watkins            C               80.75     6' 8.75''
    ## 563             Major Wingate            C               80.50      6' 8.5''
    ## 564              DaShaun Wood           PG               70.75    5' 10.75''
    ## 565             Julian Wright        SF-PF               78.50      6' 6.5''
    ## 566            Brandan Wright           PF               80.75     6' 8.75''
    ## 567                Avis Wyatt           PF               80.75     6' 8.75''
    ## 568            Thaddeus Young           SF               77.75     6' 5.75''
    ## 569                Nick Young        SG-SF               77.00        6' 5''
    ## 570             Joe Alexander        SF-PF               79.25     6' 7.25''
    ## 571            Darrell Arthur           PF               79.50      6' 7.5''
    ## 572             D.J. Augustin           PG               70.00       5' 10''
    ## 573            Jerryd Bayless        PG-SG               73.75     6' 1.75''
    ## 574           Michael Beasley        PF-SF               79.00        6' 7''
    ## 575             Ramel Bradley        PG-SG               73.25     6' 1.25''
    ## 576          Tyrone Brazelton           PG               70.00       5' 10''
    ## 577              Takais Brown           PF               79.25     6' 7.25''
    ## 578           Keith Brumbaugh        SF-PF               79.50      6' 7.5''
    ## 579           Stanley Burrell           PG               73.50      6' 1.5''
    ## 580               Brian Butch         C-PF               81.50      6' 9.5''
    ## 581              Joe Crawford           SG               75.00        6' 3''
    ## 582             Chris Daniels         C-PF               80.25     6' 8.25''
    ## 583               Joey Dorsey         PF-C               78.25     6' 6.25''
    ## 584               Marcus Dove        SF-PF               78.75     6' 6.75''
    ## 585               Josh Duncan           PF               80.00        6' 8''
    ## 586              Frank Elegar           PF               79.25     6' 7.25''
    ## 587         Patrick Ewing Jr.        SF-PF               78.00        6' 6''
    ## 588               Gary Forbes           SG               76.50      6' 4.5''
    ## 589               Shan Foster           SG               76.00        6' 4''
    ## 590                James Gist           PF               79.00        6' 7''
    ## 591        Vladimir Golubovic            C               83.25    6' 11.25''
    ## 592               Eric Gordon           SG               74.00        6' 2''
    ## 593       Kentrell Gransberry            C               78.75     6' 6.75''
    ## 594              Donte Greene        SF-PF               80.25     6' 8.25''
    ## 595            Malik Hairston           SG               76.25     6' 4.25''
    ## 596              DeVon Hardin         C-PF               81.50      6' 9.5''
    ## 597           Richard Hendrix           PF               78.75     6' 6.75''
    ## 598               George Hill           PG               73.25     6' 1.25''
    ## 599                Kyle Hines        SF-PF               75.75     6' 3.75''
    ## 600              Jiri Hubalek         C-PF               81.50      6' 9.5''
    ## 601             Lester Hudson           PG               71.75    5' 11.75''
    ## 602            Othello Hunter        PF-SF               78.50      6' 6.5''
    ## 603           Darnell Jackson           PF               79.00        6' 7''
    ## 604           Davon Jefferson           SF               77.25     6' 5.25''
    ## 605              Joseph Jones           PF               79.00        6' 7''
    ## 606            DeAndre Jordan            C               81.75     6' 9.75''
    ## 607                Sasha Kaun         C-PF               81.00        6' 9''
    ## 608             Marcelus Kemp           SG               75.00        6' 3''
    ## 609             Maarty Leunen           SF               79.25     6' 7.25''
    ## 610               Brook Lopez            C               83.25    6' 11.25''
    ## 611                Kevin Love           PF               79.75     6' 7.75''
    ## 612               Aleks Maric            C               81.75     6' 9.75''
    ## 613                 O.J. Mayo        SG-PG               75.25     6' 3.25''
    ## 614                James Mays           PF               78.75     6' 6.75''
    ## 615          Luc Mbah a Moute           SF               77.75     6' 5.75''
    ## 616              JaVale McGee         C-PF               83.00       6' 11''
    ## 617              Drew Neitzel           PG               71.75    5' 11.75''
    ## 618           DeMarcus Nelson        PG-SG               73.00        6' 1''
    ## 619             David Padgett         C-PF               82.25    6' 10.25''
    ## 620              Jeremy Pargo           PG               72.50      6' 0.5''
    ## 621            Trent Plaisted           PF               81.25     6' 9.25''
    ## 622              Quan Prowell        SF-PF               78.75     6' 6.75''
    ## 623              Shaun Pruitt         C-PF               80.25     6' 8.25''
    ## 624          Anthony Randolph        PF-SF               81.00        6' 9''
    ## 625            Charles Rhodes           PF               79.25     6' 7.25''
    ## 626             Brian Roberts           PG               72.25     6' 0.25''
    ## 627          Russell Robinson           PG               72.50      6' 0.5''
    ## 628              Derrick Rose           PG               73.50      6' 1.5''
    ## 629             Ronald Steele           PG               72.25     6' 0.25''
    ## 630              Bryce Taylor           SG               74.75     6' 2.75''
    ## 631               Mike Taylor        SG-PG               72.75     6' 0.75''
    ## 632              Mark Tyndale        SG-SF               74.75     6' 2.75''
    ## 633              Robert Vaden           SG               75.50      6' 3.5''
    ## 634          Deron Washington           SF               77.25     6' 5.25''
    ## 635               Sonny Weems        SG-SF               76.00        6' 4''
    ## 636         Russell Westbrook           PG               74.25     6' 2.25''
    ## 637           Reggie Williams           SG               75.25     6' 3.25''
    ## 638               Jeff Adrien           PF               77.25     6' 5.25''
    ## 639                Jeff Ayres           PF               80.75     6' 8.75''
    ## 640         Rodrigue Beaubois           PG               73.25     6' 1.25''
    ## 641              DeJuan Blair           PF               77.25     6' 5.25''
    ## 642             Derrick Brown        SF-PF               79.50      6' 7.5''
    ## 643            Chase Budinger        SF-SG               78.25     6' 6.25''
    ## 644               Omri Casspi           SF               79.75     6' 7.75''
    ## 645          Dionte Christmas           SG               76.25     6' 4.25''
    ## 646                Earl Clark        SF-PF               80.50      6' 8.5''
    ## 647           Darren Collison           PG               72.25     6' 0.25''
    ## 648          Dante Cunningham           PF               79.00        6' 7''
    ## 649             Stephen Curry        PG-SG               74.00        6' 2''
    ## 650               Austin Daye        SF-PF               81.75     6' 9.75''
    ## 651             DeMar DeRozan        SG-SF               77.50      6' 5.5''
    ## 652             Toney Douglas           PG               73.00        6' 1''
    ## 653           Wayne Ellington           SG               76.25     6' 4.25''
    ## 654              Tyreke Evans        PG-SG               76.00        6' 4''
    ## 655               Jonny Flynn           PG               71.25    5' 11.25''
    ## 656                Taj Gibson           PF               80.50      6' 8.5''
    ## 657               Danny Green        SG-SF               77.25     6' 5.25''
    ## 658             Blake Griffin           PF               80.50      6' 8.5''
    ## 659          Tyler Hansbrough           PF               80.25     6' 8.25''
    ## 660              James Harden           SG               76.00        6' 4''
    ## 661          Gerald Henderson        SG-SF               76.00        6' 4''
    ## 662             Josh Heytvelt         PF-C               82.00       6' 10''
    ## 663               Jordan Hill         PF-C               81.25     6' 9.25''
    ## 664              Jrue Holiday        PG-SG               75.25     6' 3.25''
    ## 665                Joe Ingles           SF               79.75     6' 7.75''
    ## 666             James Johnson        SF-PF               79.00        6' 7''
    ## 667                 Ty Lawson           PG               71.25    5' 11.25''
    ## 668               Eric Maynor           PG               74.25     6' 2.25''
    ## 669            Jack McClinton        SG-PG               71.75    5' 11.75''
    ## 670              Jerel McNeal           SG               73.50      6' 1.5''
    ## 671               Jodie Meeks           SG               75.00        6' 3''
    ## 672               Patty Mills           PG               71.25    5' 11.25''
    ## 673             Byron Mullens            C               83.75    6' 11.75''
    ## 674                A.J. Price           PG               72.50      6' 0.5''
    ## 675               Tyler Smith           SF               77.25     6' 5.25''
    ## 676            DaJuan Summers           SF               79.25     6' 7.25''
    ## 677           Jermaine Taylor           SG               75.50      6' 3.5''
    ## 678               Jeff Teague           PG               72.25     6' 0.25''
    ## 679           Marcus Thornton           SG               74.75     6' 2.75''
    ## 680         Terrence Williams        SG-SF               77.00        6' 5''
    ## 681                 Sam Young        SF-SG               77.25     6' 5.25''
    ## 682             Solomon Alabi            C               83.50     6' 11.5''
    ## 683              Cole Aldrich            C               81.00        6' 9''
    ## 684           Al-Farouq Aminu           SF               79.25     6' 7.25''
    ## 685            James Anderson           SF               76.75     6' 4.75''
    ## 686              Luke Babbitt        SF-PF               79.50      6' 7.5''
    ## 687             Trevor Booker           PF               78.25     6' 6.25''
    ## 688            Craig Brackins        PF-SF               80.50      6' 8.5''
    ## 689             Avery Bradley        SG-PG               74.00        6' 2''
    ## 690          Derrick Caracter           PF               80.25     6' 8.25''
    ## 691           Sherron Collins           PG               70.25    5' 10.25''
    ## 692          DeMarcus Cousins            C               81.50      6' 9.5''
    ## 693           Jordan Crawford           SG               75.00        6' 3''
    ## 694                  Ed Davis           PF               81.00        6' 9''
    ## 695              Devin Ebanks           SF               79.00        6' 7''
    ## 696            Derrick Favors           PF               80.75     6' 8.75''
    ## 697              Keith Gallon         PF-C               80.50      6' 8.5''
    ## 698            Charles Garcia           SF               80.25     6' 8.25''
    ## 699            Luke Harangody           PF               78.00        6' 6''
    ## 700            Gordon Hayward           SF               78.75     6' 6.75''
    ## 701             Lazar Hayward        SG-SF               76.50      6' 4.5''
    ## 702              Xavier Henry           SG               77.25     6' 5.25''
    ## 703          Darington Hobson        SG-SF               77.25     6' 5.25''
    ## 704              Damion James           SF               78.25     6' 6.25''
    ## 705             Armon Johnson        PG-SG               74.00        6' 2''
    ## 706            Wesley Johnson           SF               78.25     6' 6.25''
    ## 707           Dominique Jones           SG               75.25     6' 3.25''
    ## 708         Sylven Landesberg           SG               76.75     6' 4.75''
    ## 709                Gani Lawal           PF               79.50      6' 7.5''
    ## 710               Greg Monroe         PF-C               81.75     6' 9.75''
    ## 711              Daniel Orton            C               80.75     6' 8.75''
    ## 712       Artsiom Parakhouski            C               82.25    6' 10.25''
    ## 713         Patrick Patterson           PF               80.00        6' 8''
    ## 714              Andy Rautins           SG               76.25     6' 4.25''
    ## 715             Ryan Richards           PF               82.50     6' 10.5''
    ## 716          Stanley Robinson           SF               78.50      6' 6.5''
    ## 717             Larry Sanders         PF-C               81.25     6' 9.25''
    ## 718          Lance Stephenson           SG               76.50      6' 4.5''
    ## 719          Mikhail Torrance           PG               76.00        6' 4''
    ## 720               Evan Turner           SG               77.75     6' 5.75''
    ## 721                 Ekpe Udoh           PF               80.75     6' 8.75''
    ## 722            Jarvis Varnado         PF-C               80.25     6' 8.25''
    ## 723                 John Wall           PG               74.75     6' 2.75''
    ## 724             Willie Warren        SG-PG               74.50      6' 2.5''
    ## 725             Terrico White        SG-PG               75.75     6' 3.75''
    ## 726          Hassan Whiteside         C-PF               82.50     6' 10.5''
    ## 727              Keith Benson         C-PF               82.00       6' 10''
    ## 728            Marshon Brooks           SG               76.25     6' 4.25''
    ## 729                Alec Burks           SG               77.00        6' 5''
    ## 730              Jimmy Butler           SF               78.00        6' 6''
    ## 731               Norris Cole           PG               72.25     6' 0.25''
    ## 732               Jon Diebler           SG               77.25     6' 5.25''
    ## 733           Michael Dunigan         PF-C               80.50      6' 8.5''
    ## 734           LaceDarius Dunn           SG               75.00        6' 3''
    ## 735            Kenneth Faried           PF               78.00        6' 6''
    ## 736            James Fredette           PG               72.75     6' 0.75''
    ## 737          Andrew Goudelock           PG               73.25     6' 1.25''
    ## 738           Jordan Hamilton           SF               78.75     6' 6.75''
    ## 739             Justin Harper        PF-SF               80.00        6' 8''
    ## 740             Tobias Harris        SF-PF               78.50      6' 6.5''
    ## 741           Tyler Honeycutt           SF               78.75     6' 6.75''
    ## 742             Scotty Hopson           SG               77.75     6' 5.75''
    ## 743              Rick Jackson           PF               80.25     6' 8.25''
    ## 744           Charles Jenkins        PG-SG               73.50      6' 1.5''
    ## 745            JaJuan Johnson           PF               81.00        6' 9''
    ## 746               Cory Joseph           PG               74.00        6' 2''
    ## 747               Enes Kanter         PF-C               81.75     6' 9.75''
    ## 748            Brandon Knight           PG               73.50      6' 1.5''
    ## 749               Malcolm Lee           SG               76.00        6' 4''
    ## 750             Kawhi Leonard           SF               78.00        6' 6''
    ## 751             Travis Leslie           SG               75.00        6' 3''
    ## 752                 Jon Leuer           PF               82.00       6' 10''
    ## 753           DeAndre Liggins        SF-SG               77.00        6' 5''
    ## 754              David Lighty           SG               76.75     6' 4.75''
    ## 755              Shelvin Mack           PG               73.00        6' 1''
    ## 756           Demetri McCamey           PG               73.75     6' 1.75''
    ## 757             E'Twaun Moore           SG               74.50      6' 2.5''
    ## 758           Markieff Morris           PF               79.75     6' 7.75''
    ## 759             Marcus Morris        PF-SF               79.00        6' 7''
    ## 760             Darius Morris        PG-SG               75.25     6' 3.25''
    ## 761          Chandler Parsons           SF               80.75     6' 8.75''
    ## 762           Jereme Richmond           SF               78.00        6' 6''
    ## 763                Josh Selby        SG-PG               73.75     6' 1.75''
    ## 764             Iman Shumpert        PG-SG               76.25     6' 4.25''
    ## 765              Kyle Singler           SF               79.50      6' 7.5''
    ## 766           Chris Singleton           SF               79.75     6' 7.75''
    ## 767               Jamie Skeen           PF               78.50      6' 6.5''
    ## 768             Gregory Smith         PF-C               80.00        6' 8''
    ## 769               Nolan Smith           PG               73.50      6' 1.5''
    ## 770            Malcolm Thomas           PF               79.50      6' 7.5''
    ## 771             Isaiah Thomas           PG               68.75     5' 8.75''
    ## 772            Trey Thompkins           PF               80.50      6' 8.5''
    ## 773             Klay Thompson           SG               77.75     6' 5.75''
    ## 774          Tristan Thompson           PF               79.50      6' 7.5''
    ## 775              Jeremy Tyler         PF-C               81.00        6' 9''
    ## 776            Nikola Vucevic         PF-C               82.25    6' 10.25''
    ## 777              Kemba Walker           PG               71.50     5' 11.5''
    ## 778          Derrick Williams        PF-SF               79.25     6' 7.25''
    ## 779           Jordan Williams           PF               79.75     6' 7.75''
    ## 780                Quincy Acy           PF               78.50      6' 6.5''
    ## 781           Harrison Barnes        SF-SG               79.00        6' 7''
    ## 782               Will Barton        SG-SF               77.00        6' 5''
    ## 783              Bradley Beal           SG               75.25     6' 3.25''
    ## 784             J'Covan Brown           PG               73.00        6' 1''
    ## 785            William Buford        SG-SF               76.00        6' 4''
    ## 786               Jae Crowder           SF               76.75     6' 4.75''
    ## 787             Marcus Denmon           SG               74.25     6' 2.25''
    ## 788            Andre Drummond            C               81.75     6' 9.75''
    ## 789               Kim English           SG               76.50      6' 4.5''
    ## 790              Festus Ezeli         C-PF               81.75     6' 9.75''
    ## 791               Drew Gordon           PF               80.00        6' 8''
    ## 792            JaMychal Green           PF               80.00        6' 8''
    ## 793            Draymond Green        PF-SF               77.75     6' 5.75''
    ## 794          Maurice Harkless           SF               79.25     6' 7.25''
    ## 795               Tu Holloway           PG               70.50     5' 10.5''
    ## 796             Robbie Hummel           SF               79.25     6' 7.25''
    ## 797             Bernard James           PF               80.75     6' 8.75''
    ## 798              John Jenkins           SG               75.25     6' 3.25''
    ## 799           Orlando Johnson           SG               75.75     6' 3.75''
    ## 800       Darius Johnson-Odom           SG               73.50      6' 1.5''
    ## 801            Terrence Jones        PF-SF               80.25     6' 8.25''
    ## 802               Perry Jones        PF-SF               82.25    6' 10.25''
    ## 803               Kevin Jones           PF               78.25     6' 6.25''
    ## 804               Kris Joseph           SG               78.00        6' 6''
    ## 805    Michael Kidd-Gilchrist           SF               77.75     6' 5.75''
    ## 806               Jeremy Lamb           SG               76.00        6' 4''
    ## 807                Doron Lamb        SG-PG               75.25     6' 3.25''
    ## 808            Meyers Leonard            C               83.75    6' 11.75''
    ## 809            Damian Lillard           PG               73.75     6' 1.75''
    ## 810             Scott Machado           PG               73.00        6' 1''
    ## 811          Kendall Marshall           PG               75.25     6' 3.25''
    ## 812                  Fab Melo            C               82.75    6' 10.75''
    ## 813           Khris Middleton           SF               78.75     6' 6.75''
    ## 814             Darius Miller        SF-SG               78.00        6' 6''
    ## 815             Quincy Miller           SF               80.75     6' 8.75''
    ## 816             Tony Mitchell           SG               77.25     6' 5.25''
    ## 817           Arnett Moultrie           PF               81.50      6' 9.5''
    ## 818              Kevin Murphy           SG               77.00        6' 5''
    ## 819          Andrew Nicholson           PF               80.50      6' 8.5''
    ## 820              Kyle O'Quinn           PF               80.50      6' 8.5''
    ## 821             Miles Plumlee         PF-C               82.50     6' 10.5''
    ## 822             Austin Rivers        SG-PG               75.50      6' 3.5''
    ## 823           Thomas Robinson           PF               79.75     6' 7.75''
    ## 824             Terrence Ross        SG-SF               78.00        6' 6''
    ## 825                Mike Scott           PF               79.25     6' 7.25''
    ## 826                Henry Sims         C-PF               82.00       6' 10''
    ## 827           Jared Sullinger           PF               79.75     6' 7.75''
    ## 828               Jeff Taylor        SF-SG               78.00        6' 6''
    ## 829            Tyshawn Taylor           PG               74.75     6' 2.75''
    ## 830             Jordan Taylor           PG               73.00        6' 1''
    ## 831            Marquis Teague           PG               73.00        6' 1''
    ## 832           Hollis Thompson           SF               78.75     6' 6.75''
    ## 833               Tony Wroten           PG               76.75     6' 4.75''
    ## 834              Tyler Zeller         PF-C               83.25    6' 11.25''
    ## 835              Steven Adams            C               82.75    6' 10.75''
    ## 836               Vander Blue           SG               75.50      6' 3.5''
    ## 837             Lorenzo Brown           PG               76.00        6' 4''
    ## 838            Reggie Bullock        SG-SF               77.75     6' 5.75''
    ## 839                Trey Burke           PG               71.75    5' 11.75''
    ## 840  Kentavious Caldwell-Pope           SG               76.50      6' 4.5''
    ## 841             Isaiah Canaan           PG               71.00       5' 11''
    ## 842         Jackie Carmichael           PF               79.50      6' 7.5''
    ## 843   Michael Carter-Williams           PG               76.75     6' 4.75''
    ## 844              Will Clyburn        SF-SG               77.75     6' 5.75''
    ## 845          Robert Covington        SF-PF               78.25     6' 6.25''
    ## 846              Allen Crabbe           SG               77.25     6' 5.25''
    ## 847            Brandon Davies           PF               80.50      6' 8.5''
    ## 848            Dewayne Dedmon            C               82.00       6' 10''
    ## 849               James Ennis           SF               77.75     6' 5.75''
    ## 850             Carrick Felix        SF-SG               76.75     6' 4.75''
    ## 851               Rudy Gobert            C               84.50      7' 0.5''
    ## 852            Archie Goodwin           SG               75.75     6' 3.75''
    ## 853          Tim Hardaway Jr.           SG               76.50      6' 4.5''
    ## 854              Solomon Hill           SF               77.50      6' 5.5''
    ## 855            Colton Iverson            C               82.50     6' 10.5''
    ## 856             Grant Jerrett           PF               80.75     6' 8.75''
    ## 857              Myck Kabongo           PG               73.25     6' 1.25''
    ## 858               Kenny Kadji         PF-C               80.75     6' 8.75''
    ## 859              Shane Larkin           PG               70.25    5' 10.25''
    ## 860                Ricky Ledo           SG               76.75     6' 4.75''
    ## 861                 CJ Leslie        PF-SF               79.50      6' 7.5''
    ## 862             Trevor Mbakwe           PF               78.75     6' 6.75''
    ## 863              Ray McCallum           PG               72.25     6' 0.25''
    ## 864               CJ McCollum        PG-SG               74.25     6' 2.25''
    ## 865              Ben McLemore           SG               75.50      6' 3.5''
    ## 866             Tony Mitchell        PF-SF               79.50      6' 7.5''
    ## 867          Shabazz Muhammad        SF-SG               76.75     6' 4.75''
    ## 868               Erik Murphy           PF               80.75     6' 8.75''
    ## 869              Mike Muscala         C-PF               82.25    6' 10.25''
    ## 870            Victor Oladipo           SG               75.25     6' 3.25''
    ## 871              Kelly Olynyk         PF-C               82.75    6' 10.75''
    ## 872              Brandon Paul           SG               74.75     6' 2.75''
    ## 873              Norvel Pelle         PF-C               81.50      6' 9.5''
    ## 874             Mason Plumlee         C-PF               83.25    6' 11.25''
    ## 875               Otto Porter           SF               79.50      6' 7.5''
    ## 876              Phil Pressey           PG               69.50      5' 9.5''
    ## 877                 Glen Rice        SF-SG               76.50      6' 4.5''
    ## 878            Andre Roberson        PF-SF               78.25     6' 6.25''
    ## 879           Dennis Schroder           PG               73.00        6' 1''
    ## 880               Peyton Siva           PG               71.50     5' 11.5''
    ## 881                Tony Snell        SF-SG               78.00        6' 6''
    ## 882         James Southerland        SF-PF               79.00        6' 7''
    ## 883            Deshaun Thomas           SF               77.00        6' 5''
    ## 884             Adonis Thomas           SF               76.75     6' 4.75''
    ## 885               Jeff Withey            C               82.75    6' 10.75''
    ## 886               Cody Zeller         C-PF               82.75    6' 10.75''
    ## 887              Jordan Adams           SG               75.50      6' 3.5''
    ## 888    Thanasis Antetokounmpo           SF               77.25     6' 5.25''
    ## 889          Jordan Bachynski           PF               85.25     7' 1.25''
    ## 890          Cameron Bairstow           PF               80.75     6' 8.75''
    ## 891                Khem Birch           PF               79.50      6' 7.5''
    ## 892                Alec Brown           PF               84.25     7' 0.25''
    ## 893              Jabari Brown           SG               75.00        6' 3''
    ## 894              Markel Brown           SG               74.50      6' 2.5''
    ## 895             Deonte Burton           PG               72.00        6' 0''
    ## 896            Semaj Christon           PG               73.50      6' 1.5''
    ## 897           Jordan Clarkson           PG               75.25     6' 3.25''
    ## 898               Aaron Craft           PG               72.75     6' 0.75''
    ## 899           DeAndre Daniels           SF               79.25     6' 7.25''
    ## 900          Cleanthony Early           SF               78.00        6' 6''
    ## 901               Melvin Ejim           SF               77.25     6' 5.25''
    ## 902               Tyler Ennis           PG               73.00        6' 1''
    ## 903                Dante Exum           SG               76.50      6' 4.5''
    ## 904                 C.J. Fair           SF               78.50      6' 6.5''
    ## 905              Aaron Gordon           PF               79.50      6' 7.5''
    ## 906             P.J. Hairston           SG               76.25     6' 4.25''
    ## 907                Joe Harris           SG               76.75     6' 4.75''
    ## 908               Rodney Hood           SF               79.25     6' 7.25''
    ## 909            Cory Jefferson           PF               79.75     6' 7.75''
    ## 910              Nick Johnson           SG               73.50      6' 1.5''
    ## 911                 Alex Kirk           PF               81.25     6' 9.25''
    ## 912               Zach LaVine           PG               76.50      6' 4.5''
    ## 913              Devyn Marble           SG               77.00        6' 5''
    ## 914              James McAdoo           PF               79.50      6' 7.5''
    ## 915            K.J. McDaniels           SF               76.50      6' 4.5''
    ## 916            Doug McDermott           SF               78.25     6' 6.25''
    ## 917              Jordan McRae           SG               75.75     6' 3.75''
    ## 918            Shabazz Napier           PG               71.00       5' 11''
    ## 919       Johnny O?Bryant III           PF               79.25     6' 7.25''
    ## 920           Lamar Patterson           SG               76.00        6' 4''
    ## 921             Elfrid Payton           PG               74.50      6' 2.5''
    ## 922             Dwight Powell           PF               81.50      6' 9.5''
    ## 923             Julius Randle           PF               79.75     6' 7.75''
    ## 924        Glenn Robinson III           SF               77.50      6' 5.5''
    ## 925            LaQuinton Ross           SF               78.50      6' 6.5''
    ## 926              Marcus Smart           PG               74.00        6' 2''
    ## 927                Russ Smith           PG               71.50     5' 11.5''
    ## 928              Nik Stauskas           SG               77.25     6' 5.25''
    ## 929            Jarnell Stokes           PF               79.00        6' 7''
    ## 930             Xavier Thames           PG               74.25     6' 2.25''
    ## 931               Noah Vonleh           PF               80.00        6' 8''
    ## 932               T.J. Warren           SF               79.00        6' 7''
    ## 933               C.J. Wilcox           SG               75.50      6' 3.5''
    ## 934          Kendall Williams           PG               74.25     6' 2.25''
    ## 935              Patric Young           PF               80.50      6' 8.5''
    ## 936           Justin Anderson        SF-SG               77.25     6' 5.25''
    ## 937            Brandon Ashley           PF               79.25     6' 7.25''
    ## 938            Ryan Boatright           PG               70.00       5' 10''
    ## 939              Devin Booker           SG               76.50      6' 4.5''
    ## 940             Anthony Brown        SF-SG               77.75     6' 5.75''
    ## 941          Rakeem Christmas         PF-C               80.25     6' 8.25''
    ## 942           Pat Connaughton           SG               76.00        6' 4''
    ## 943                Quinn Cook           PG               72.25     6' 0.25''
    ## 944            Branden Dawson           PF               77.50      6' 5.5''
    ## 945                Sam Dekker           SF               79.75     6' 7.75''
    ## 946        Michael Frazier II           SG               75.75     6' 3.75''
    ## 947            Treveon Graham           SF               76.00        6' 4''
    ## 948            Olivier Hanlan        PG-SG               75.25     6' 3.25''
    ## 949           Andrew Harrison           PG               76.50      6' 4.5''
    ## 950            Aaron Harrison           SG               76.50      6' 4.5''
    ## 951              Tyler Harvey           SG               74.75     6' 2.75''
    ## 952             Corey Hawkins        SG-PG               72.75     6' 0.75''
    ## 953   Rondae Hollis-Jefferson           SF               77.50      6' 5.5''
    ## 954           Jonathan Holmes        PF-SF               80.25     6' 8.25''
    ## 955            Richaun Holmes           PF               80.25     6' 8.25''
    ## 956               R.J. Hunter           SG               76.50      6' 4.5''
    ## 957              Vince Hunter           PF               78.75     6' 6.75''
    ## 958            Dakari Johnson            C               83.00       6' 11''
    ## 959                Tyus Jones           PG               72.50      6' 0.5''
    ## 960             Jarell Martin           PF               80.00        6' 8''
    ## 961              TJ McConnell           PG               72.50      6' 0.5''
    ## 962             Jordan Mickey           PF               79.00        6' 7''
    ## 963               Larry Nance           PF               79.50      6' 7.5''
    ## 964               Kelly Oubre        SF-SG               77.75     6' 5.75''
    ## 965             Cameron Payne           PG               72.75     6' 0.75''
    ## 966           Terran Petteway           SG               76.75     6' 4.75''
    ## 967              Bobby Portis           PF               81.50      6' 9.5''
    ## 968             Norman Powell           SG               75.00        6' 3''
    ## 969            Michael Qualls        SG-SF               76.00        6' 4''
    ## 970            Chasson Randle           PG               72.25     6' 0.25''
    ## 971              Terry Rozier        PG-SG               72.50      6' 0.5''
    ## 972              Keifer Sykes           PG               70.50     5' 10.5''
    ## 973           Marcus Thornton        PG-SG               73.75     6' 1.75''
    ## 974               J.P. Tokoto        SF-SG               77.25     6' 5.25''
    ## 975             Rashad Vaughn           SG               75.75     6' 3.75''
    ## 976              Chris Walker           PF               80.75     6' 8.75''
    ## 977                 Dez Wells           SG               75.00        6' 3''
    ## 978             Alan Williams           PF               79.25     6' 7.25''
    ## 979              Delon Wright           PG               76.25     6' 4.25''
    ## 980                 Ron Baker           SG               75.25     6' 3.25''
    ## 981           Wade Baldwin IV           PG               74.50      6' 2.5''
    ## 982            DeAndre Bembry        SG-SF               76.25     6' 4.25''
    ## 983                Ben Bentil           PF               79.25     6' 7.25''
    ## 984         Jaron Blossomgame           SF               78.25     6' 6.25''
    ## 985             Joel Bolomboy           PF               79.50      6' 7.5''
    ## 986           Malcolm Brogdon           SG               75.75     6' 3.75''
    ## 987           Marquese Chriss           PF               80.75     6' 8.75''
    ## 988            Isaiah Cousins           PG               75.25     6' 3.25''
    ## 989             Cheick Diallo           PF               79.50      6' 7.5''
    ## 990               Perry Ellis           PF               79.00        6' 7''
    ## 991                Kay Felder           PG               68.25     5' 8.25''
    ## 992       Dorian Finney-Smith           SF               78.50      6' 6.5''
    ## 993           Michael Gbinije           SG               77.50      6' 5.5''
    ## 994           Daniel Hamilton        SG-SF               78.25     6' 6.25''
    ## 995                 Josh Hart           SG               76.00        6' 4''
    ## 996               Nigel Hayes        SF-PF               78.25     6' 6.25''
    ## 997            Justin Jackson           SF               79.00        6' 7''
    ## 998         Demetrius Jackson           PG               72.00        6' 0''
    ## 999             Brice Johnson           PF               81.00        6' 9''
    ## 1000             Damian Jones            C               82.25    6' 10.25''
    ## 1001              Jake Layman           SF               80.00        6' 8''
    ## 1002               Marcus Lee         PF-C               81.00        6' 9''
    ## 1003               Thon Maker           PF               83.75    6' 11.75''
    ## 1004            Patrick McCaw        PG-SG               77.25     6' 5.25''
    ## 1005             Malik Newman           SG               74.25     6' 2.25''
    ## 1006            Georges Niang           PF               78.75     6' 6.75''
    ## 1007           Chinanu Onuaku            C               81.00        6' 9''
    ## 1008             Marcus Paige           PG               72.50      6' 0.5''
    ## 1009           Gary Payton II           PG               74.00        6' 2''
    ## 1010           Taurean Prince           SF               78.50      6' 6.5''
    ## 1011                  Zhou Qi         PF-C               85.25     7' 1.25''
    ## 1012       Malachi Richardson        SG-SF               76.75     6' 4.75''
    ## 1013            Pascal Siakam           PF               80.25     6' 8.25''
    ## 1014            Diamond Stone            C               81.00        6' 9''
    ## 1015           Caleb Swanigan           PF               79.50      6' 7.5''
    ## 1016             Melo Trimble           PG               73.25     6' 1.25''
    ## 1017               Tyler Ulis           PG               68.75     5' 8.75''
    ## 1018            Jarrod Uthoff           PF               80.50      6' 8.5''
    ## 1019         Denzel Valentine        SG-SF               76.00        6' 4''
    ## 1020         Isaiah Whitehead        PG-SG               75.25     6' 3.25''
    ## 1021            Troy Williams           SF               77.75     6' 5.75''
    ## 1022             Kyle Wiltjer           PF               80.75     6' 8.75''
    ## 1023        Stephen Zimmerman            C               82.25    6' 10.25''
    ## 1024           Edrice Adebayo         PF-C               80.75     6' 8.75''
    ## 1025             Rawle Alkins        SG-SF               74.50      6' 2.5''
    ## 1026            Jarrett Allen            C               81.00        6' 9''
    ## 1027             Kadeem Allen           PG               73.00        6' 1''
    ## 1028             Ike Anigbogu            C               80.50      6' 8.5''
    ## 1029              Jamel Artis        SG-SF               77.50      6' 5.5''
    ## 1030             Dwayne Bacon        SG-SF               76.75     6' 4.75''
    ## 1031             V.J. Beachem        SG-SF               78.25     6' 6.25''
    ## 1032              Jordan Bell           PF               79.00        6' 7''
    ## 1033        Jaron Blossomgame           SF               77.75     6' 5.75''
    ## 1034             Tony Bradley            C               81.25     6' 9.25''
    ## 1035           Isaiah Briscoe           PG               73.25     6' 1.25''
    ## 1036            Dillon Brooks        SG-SF               77.00        6' 5''
    ## 1037            Thomas Bryant         PF-C               81.50      6' 9.5''
    ## 1038             John Collins           PF               80.25     6' 8.25''
    ## 1039           Hamidou Diallo        SG-SF               75.75     6' 3.75''
    ## 1040             Tyler Dorsey           SG               75.00        6' 3''
    ## 1041           Damyean Dotson        SG-SF               76.50      6' 4.5''
    ## 1042                PJ Dozier           SG               77.25     6' 5.25''
    ## 1043              Jawun Evans           PG               70.75    5' 10.75''
    ## 1044        Terrance Ferguson        SG-SF               77.50      6' 5.5''
    ## 1045              Harry Giles           PF               81.25     6' 9.25''
    ## 1046                Josh Hart        SG-SF               75.75     6' 3.75''
    ## 1047              Nigel Hayes           PF               78.25     6' 6.25''
    ## 1048             Isaiah Hicks           PF               79.25     6' 7.25''
    ## 1049            Wesley Iwundu        SG-SF               77.50      6' 5.5''
    ## 1050            Frank Jackson           PG               74.00        6' 2''
    ## 1051     Justin Jackson (UMD)           SF               77.75     6' 5.75''
    ## 1052     Justin Jackson (UNC)           SF               79.00        6' 7''
    ## 1053                Peter Jok        SG-SF               76.25     6' 4.25''
    ## 1054               Kyle Kuzma        SF-SG               80.00        6' 8''
    ## 1055                  TJ Leaf           PF               80.75     6' 8.75''
    ## 1056              Tyler Lydon           PF               80.25     6' 8.25''
    ## 1057          Frank Mason III           PG               71.00       5' 11''
    ## 1058            Kennedy Meeks            C               80.75     6' 8.75''
    ## 1059         Donovan Mitchell           SG               73.25     6' 1.25''
    ## 1060             Monte Morris           PG               73.25     6' 1.25''
    ## 1061           Svi Mykhailiuk        SG-SF               78.50      6' 6.5''
    ## 1062             Semi Ojeleye        SF-SG               77.25     6' 5.25''
    ## 1063           Cameron Oliver           PF               79.00        6' 7''
    ## 1064            Justin Patton            C               82.00       6' 10''
    ## 1065                Ivan Rabb           PF               80.75     6' 8.75''
    ## 1066               Davon Reed        SG-SF               76.50      6' 4.5''
    ## 1067           Devin Robinson           SF               79.00        6' 7''
    ## 1068             Kobi Simmons           PG               75.25     6' 3.25''
    ## 1069      Sindarius Thornwell        SG-SF               75.50      6' 3.5''
    ## 1070             Melo Trimble           PG               73.25     6' 1.25''
    ## 1071            Moritz Wagner           PF               82.00       6' 10''
    ## 1072       Derrick Walton Jr.           PG               71.00       5' 11''
    ## 1073            Derrick White           PG               75.25     6' 3.25''
    ## 1074      Nigel Williams-Goss           PG               73.50      6' 1.5''
    ## 1075           Omer Yurtseven            C               82.50     6' 10.5''
    ## 1076             Rawle Alkins           SG               74.75     6' 2.75''
    ## 1077            Grayson Allen           SG               75.00        6' 3''
    ## 1078     Kostas Antetokounmpo           PF               81.00        6' 9''
    ## 1079           Udoka Azubuike            C               82.00       6' 10''
    ## 1080           Jaylen Barford        PG-SG               73.25     6' 1.25''
    ## 1081         Keita Bates-Diop        SG-SF               79.25     6' 7.25''
    ## 1082              Tyus Battle           SG               77.00        6' 5''
    ## 1083              Brian Bowen        SG-SF               78.25     6' 6.25''
    ## 1084              Bruce Brown        SG-PG               75.50      6' 3.5''
    ## 1085               Troy Brown           SF               77.75     6' 5.75''
    ## 1086            Jalen Brunson           PG               73.00        6' 1''
    ## 1087                Tony Carr           PG               75.00        6' 3''
    ## 1088             Jevon Carter           PG               72.25     6' 0.25''
    ## 1089         Donte DiVincenzo           SG               75.50      6' 3.5''
    ## 1090           Hamidou Diallo           SG               76.25     6' 4.25''
    ## 1091             Trevon Duval           PG               73.50      6' 1.5''
    ## 1092           Carsen Edwards           PG               70.75    5' 10.75''
    ## 1093              Jacob Evans        SG-SF               76.25     6' 4.25''
    ## 1094           Bruno Fernando            C               80.75     6' 8.75''
    ## 1095           Melvin Frazier        SF-SG               76.50      6' 4.5''
    ## 1096           Devonte Graham           PG               72.25     6' 0.25''
    ## 1097               Devon Hall           SG               76.25     6' 4.25''
    ## 1098             Jaylen Hands           PG               73.50      6' 1.5''
    ## 1099             Kevin Hervey           PF               78.50      6' 6.5''
    ## 1100            Aaron Holiday           PG               71.75    5' 11.75''
    ## 1101            Kevin Huerter           SG               78.25     6' 6.25''
    ## 1102            Alize Johnson           PF               79.25     6' 7.25''
    ## 1103              George King        SF-SG               76.75     6' 4.75''
    ## 1104            Sagaba Konate            C               78.50      6' 6.5''
    ## 1105             Caleb Martin        SF-SG               76.75     6' 4.75''
    ## 1106              Cody Martin        SG-SF               76.75     6' 4.75''
    ## 1107              Yante Maten         PF-C               79.25     6' 7.25''
    ## 1108            Brandon McCoy            C               83.50     6' 11.5''
    ## 1109         DeAnthony Melton        SG-PG               74.25     6' 2.25''
    ## 1110            Chimezie Metu         PF-C               80.50      6' 8.5''
    ## 1111             Shake Milton        SG-PG               76.50      6' 4.5''
    ## 1112    Sviatoslav Mykhailiuk        SG-SF               78.50      6' 6.5''
    ## 1113             Malik Newman           SG               74.50      6' 2.5''
    ## 1114              Josh Okogie           SG               75.00        6' 3''
    ## 1115              Theo Pinson        SG-SF               77.00        6' 5''
    ## 1116            Jontay Porter         C-PF               82.00       6' 10''
    ## 1117            Billy Preston           PF               80.75     6' 8.75''
    ## 1118            Landry Shamet           PG               76.00        6' 4''
    ## 1119          Anfernee Simons        SG-PG               74.25     6' 2.25''
    ## 1120             Zhaire Smith        SF-SG               74.75     6' 2.75''
    ## 1121             Ray Spalding           PF               81.00        6' 9''
    ## 1122           Omari Spellman         PF-C               80.00        6' 8''
    ## 1123             Khyri Thomas           SG               74.50      6' 2.5''
    ## 1124               Gary Trent           SG               76.25     6' 4.25''
    ## 1125            Allonzo Trier           SG               75.75     6' 3.75''
    ## 1126            Moritz Wagner         C-PF               82.50     6' 10.5''
    ## 1127            Lonnie Walker           SG               75.75     6' 3.75''
    ## 1128            PJ Washington           PF               78.50      6' 6.5''
    ## 1129             Austin Wiley            C               80.25     6' 8.25''
    ## 1130              Kris Wilkes           SF               78.50      6' 6.5''
    ## 1131         Kenrich Williams           SF               78.00        6' 6''
    ## 1132           Charles Bassey            C               80.75     6' 8.75''
    ## 1133            Darius Bazley           PF               79.75     6' 7.75''
    ## 1134              Jordan Bone           SG               73.50      6' 1.5''
    ## 1135           Brian Bowen II           SF               78.25     6' 6.25''
    ## 1136                Ky Bowman           PG               73.00        6' 1''
    ## 1137         Ignas Brazdeikis           SF               77.75     6' 5.75''
    ## 1138          O'Shae Brissett        SF-PF               79.00        6' 7''
    ## 1139              Moses Brown            C               85.25     7' 1.25''
    ## 1140           Brandon Clarke           SF               79.25     6' 7.25''
    ## 1141          Nicolas Claxton            C               82.00       6' 10''
    ## 1142               Tyler Cook           PF               79.25     6' 7.25''
    ## 1143            Terence Davis           SG               75.00        6' 3''
    ## 1144            Luguentz Dort           PG               74.75     6' 2.75''
    ## 1145             Devon Dotson           PG               72.00        6' 0''
    ## 1146           Carsen Edwards           PG               70.75    5' 10.75''
    ## 1147               Tacko Fall            C               89.25     7' 5.25''
    ## 1148           Bruno Fernando            C               80.75     6' 8.75''
    ## 1149           Daniel Gafford           PF               81.25     6' 9.25''
    ## 1150           Quentin Grimes           SG               76.00        6' 4''
    ## 1151                 Kyle Guy           SG               72.75     6' 0.75''
    ## 1152             Jaylen Hands           PG               73.50      6' 1.5''
    ## 1153             Jared Harper           PG               69.75     5' 9.75''
    ## 1154             Jaxson Hayes           PF               82.25    6' 10.25''
    ## 1155          Dewan Hernandez            C               81.00        6' 9''
    ## 1156              Jalen Hoard           PF               79.25     6' 7.25''
    ## 1157      Talen Horton-Tucker           SG               74.50      6' 2.5''
    ## 1158                Ty Jerome           PG               76.25     6' 4.25''
    ## 1159          Cameron Johnson           SG               79.00        6' 7''
    ## 1160        Mfiondu Kabengele           PF               80.75     6' 8.75''
    ## 1161               Louis King           SF               78.75     6' 6.75''
    ## 1162            Dedric Lawson           PF               79.00        6' 7''
    ## 1163             Jalen Lecque           PG               74.50      6' 2.5''
    ## 1164            Nassir Little           SF               76.50      6' 4.5''
    ## 1165             Terance Mann        SG-SF               76.75     6' 4.75''
    ## 1166              Cody Martin        PG-SG               76.75     6' 4.75''
    ## 1167         Charles Matthews           SG               77.00        6' 5''
    ## 1168          Jalen McDaniels           SF               80.25     6' 8.25''
    ## 1169         Zach Norvell Jr.           SG               76.50      6' 4.5''
    ## 1170            Jaylen Nowell           SG               75.25     6' 3.25''
    ## 1171                KZ Okpala           SF               79.25     6' 7.25''
    ## 1172                 Miye Oni           SG               76.25     6' 4.25''
    ## 1173            Eric Paschall           SF               78.00        6' 6''
    ## 1174             Reggie Perry        SF-PF               80.00        6' 8''
    ## 1175           Shamorie Ponds           PG               71.50     5' 11.5''
    ## 1176             Jordan Poole           SG               75.50      6' 3.5''
    ## 1177         Kevin Porter Jr.           SG               76.00        6' 4''
    ## 1178            Neemias Queta            C               82.75    6' 10.75''
    ## 1179                 Naz Reid           PF               80.25     6' 8.75''
    ## 1180              Isaiah Roby           PF               79.25     6' 7.25''
    ## 1181             Luka Samanic           PF               81.50      6' 9.5''
    ## 1182        Admiral Schofield           SF               76.00        6' 4''
    ## 1183            Marial Shayok        SF-PF               76.50      6' 4.5''
    ## 1184          Simisola Shittu           PF               80.50      6' 8.5''
    ## 1185           Tremont Waters           PG               69.50      5' 9.5''
    ## 1186   Quinndary Weatherspoon           SG               75.00        6' 3''
    ## 1187              Kris Wilkes           SF               78.25     6' 6.25''
    ## 1188           Grant Williams           PF               77.75     6' 5.75''
    ## 1189            Dylan Windler           SF               78.25     6' 6.25''
    ## 1190        Ty-Shon Alexander           SG               74.00      6'2.00''
    ## 1191           Udoka Azubuike            C               82.00     6'10.00''
    ## 1192                Tyler Bey           PF               78.00      6'6.00''
    ## 1193             Yoeli Childs           PF               78.00      6'6.00''
    ## 1194           Mamadi Diakite           PF               80.50      6'8.50''
    ## 1195             Devon Dotson           PG               72.50      6'0.50''
    ## 1196               Paul Eboua        SF-PF               78.00      6'6.00''
    ## 1197                CJ Elleby        SG-SF               77.50      6'5.50''
    ## 1198            Trent Forrest           PG               75.50      6'3.50''
    ## 1199               Josh Green        SG-SF               76.50      6'4.50''
    ## 1200            Ashton Hagans           PG               73.50      6'1.50''
    ## 1201              Joshua Hall           SF               78.00      6'6.00''
    ## 1202             Jalen Harris           SG               74.75      6'2.75''
    ## 1203            Markus Howard           PG               69.50      5'9.50''
    ## 1204              Mason Jones           SG               75.75      6'3.75''
    ## 1205            Nathan Knight         PF-C               80.00      6'8.00''
    ## 1206               Karim Mane        PG-SG               77.00      6'5.00''
    ## 1207            Naji Marshall           SF               77.75      6'5.75''
    ## 1208        Kenyon Martin Jr.        SF-PF               78.00      6'6.00''
    ## 1209              Skylar Mays           SG               75.00      6'3.00''
    ## 1210               Zeke Nnaji         PF-C               81.25      6'9.25''
    ## 1211             Jordan Nwora        SF-PF               77.75      6'5.75''
    ## 1212             Reggie Perry         PF-C               80.25      6'8.25''
    ## 1213        Immanuel Quickley           SG               74.00      6'2.00''
    ## 1214            Paul Reed Jr.           PF               80.50      6'8.50''
    ## 1215            Nick Richards            C               82.50     6'10.50''
    ## 1216             Grant Riller        PG-SG               72.25      6'0.25''
    ## 1217          Cassius Stanley           SG               77.00      6'5.00''
    ## 1218            Lamar Stevens        SF-PF               77.75      6'5.75''
    ## 1219             Tyrell Terry           PG               73.50      6'1.50''
    ## 1220           Killian Tillie           PF               80.75      6'8.75''
    ## 1221       Xavier Tillman Sr.         PF-C               79.50      6'7.50''
    ## 1222             Kaleb Wesson            C               81.25      6'9.25''
    ## 1223           Kahlil Whitney           SF               77.50      6'5.50''
    ## 1224          Cassius Winston           PG               72.50      6'0.50''
    ## 1225        Robert Woodard II           SF               77.50      6'5.50''
    ##      weightLBS wingspanInches   wingspan reachStandingInches reachStandingO
    ## 1        271.0          86.50   7' 2.5''              109.00         9' 1''
    ## 2        235.5          87.50   7' 3.5''              108.00         9' 0''
    ## 3        287.0          84.00     7' 0''              103.00         8' 7''
    ## 4        243.0          86.00     7' 2''              110.00         9' 2''
    ## 5        166.0          72.00     6' 0''               94.50      7' 10.5''
    ## 6        205.0          83.00    6' 11''              102.00         8' 6''
    ## 7        189.0          76.00     6' 4''               96.50       8' 0.5''
    ## 8        219.5          79.00     6' 7''              100.00         8' 4''
    ## 9        220.5          86.00     7' 2''              107.50      8' 11.5''
    ## 10       200.0          73.50   6' 1.5''               93.00         7' 9''
    ## 11       262.0          82.50  6' 10.5''              106.50      8' 10.5''
    ## 12       219.0          80.50   6' 8.5''              106.00        8' 10''
    ## 13       176.0          76.00     6' 4''               93.00         7' 9''
    ## 14       239.0          84.50   7' 0.5''              105.50       8' 9.5''
    ## 15       227.0          81.50   6' 9.5''              103.00         8' 7''
    ## 16       175.0          75.75  6' 3.75''               97.50       8' 1.5''
    ## 17       202.0          83.00    6' 11''              104.50       8' 8.5''
    ## 18       181.0          78.00     6' 6''               97.50       8' 1.5''
    ## 19       200.0          80.00     6' 8''              100.50       8' 4.5''
    ## 20       195.0          78.25  6' 6.25''               98.50       8' 2.5''
    ## 21       183.0          83.00    6' 11''              101.50       8' 5.5''
    ## 22       176.5          75.50   6' 3.5''               95.00        7' 11''
    ## 23       170.0          80.00     6' 8''              100.00         8' 4''
    ## 24       238.0          83.00    6' 11''              105.50       8' 9.5''
    ## 25       210.0          81.00     6' 9''              104.00         8' 8''
    ## 26       211.5          80.50   6' 8.5''              104.00         8' 8''
    ## 27       282.0          85.00     7' 1''              108.00         9' 0''
    ## 28       218.0          80.00     6' 8''              102.50       8' 6.5''
    ## 29       197.5          80.00     6' 8''              104.00         8' 8''
    ## 30       205.5          76.50   6' 4.5''               97.50       8' 1.5''
    ## 31       236.5          84.50   7' 0.5''              104.50       8' 8.5''
    ## 32       250.0          85.50   7' 1.5''              107.00        8' 11''
    ## 33       222.0          81.50   6' 9.5''              103.50       8' 7.5''
    ## 34       215.0          82.50  6' 10.5''              106.00        8' 10''
    ## 35       244.0          84.50   7' 0.5''              105.00         8' 9''
    ## 36       208.0          87.50   7' 3.5''              112.50       9' 4.5''
    ## 37       234.5          83.50  6' 11.5''              104.50       8' 8.5''
    ## 38       223.0          82.50  6' 10.5''              106.00        8' 10''
    ## 39       214.0          81.50   6' 9.5''              103.50       8' 7.5''
    ## 40       205.0          80.50   6' 8.5''              101.50       8' 5.5''
    ## 41       184.0          77.25  6' 5.25''              100.00         8' 4''
    ## 42       218.0          81.25  6' 9.25''              104.00         8' 8''
    ## 43       214.0          81.00     6' 9''              102.00         8' 6''
    ## 44       228.0          81.50   6' 9.5''              104.50       8' 8.5''
    ## 45       189.0          77.50   6' 5.5''               94.50      7' 10.5''
    ## 46       184.0          75.75  6' 3.75''               95.50      7' 11.5''
    ## 47       194.0          75.50   6' 3.5''               96.00         8' 0''
    ## 48       177.0          75.00     6' 3''               94.00        7' 10''
    ## 49       184.5          81.00     6' 9''               97.50       8' 1.5''
    ## 50       224.0          87.00     7' 3''              111.50       9' 3.5''
    ## 51       273.0          83.00    6' 11''              107.50      8' 11.5''
    ## 52       198.0          80.50   6' 8.5''              103.00         8' 7''
    ## 53       240.5          86.50   7' 2.5''              109.50       9' 1.5''
    ## 54       258.0          81.25  6' 9.25''              103.00         8' 7''
    ## 55       194.5          78.50   6' 6.5''              101.50       8' 5.5''
    ## 56       165.5          74.50   6' 2.5''               96.50       8' 0.5''
    ## 57       244.5          90.00     7' 6''              110.00         9' 2''
    ## 58       259.0          84.50   7' 0.5''              107.00        8' 11''
    ## 59       199.0          81.50   6' 9.5''               99.50       8' 3.5''
    ## 60       188.0          81.50   6' 9.5''               99.50       8' 3.5''
    ## 61       229.0          82.50  6' 10.5''              105.00         8' 9''
    ## 62       206.0          80.50   6' 8.5''               99.00         8' 3''
    ## 63       200.0          79.50   6' 7.5''               97.50       8' 1.5''
    ## 64       245.0          88.00     7' 4''              111.00         9' 3''
    ## 65       214.0          83.50  6' 11.5''              104.00         8' 8''
    ## 66       227.0          85.50   7' 1.5''              107.00        8' 11''
    ## 67       178.0          80.75  6' 8.75''              102.50       8' 6.5''
    ## 68       243.0          85.00     7' 1''              108.00         9' 0''
    ## 69       202.0          83.75 6' 11.75''              104.50       8' 8.5''
    ## 70       176.0          76.00     6' 4''               92.50       7' 8.5''
    ## 71       202.0          80.25  6' 8.25''               99.00         8' 3''
    ## 72       224.0          87.00     7' 3''              110.00         9' 2''
    ## 73       264.0          88.00     7' 4''              111.00         9' 3''
    ## 74       242.0          86.75  7' 2.75''              107.00        8' 11''
    ## 75       231.0          83.50  6' 11.5''              103.00         8' 7''
    ## 76       252.0          87.00     7' 3''              104.00         8' 8''
    ## 77       251.0          88.50   7' 4.5''              110.00         9' 2''
    ## 78       189.0          77.50   6' 5.5''               92.00         7' 8''
    ## 79       301.0          90.50   7' 6.5''              111.00         9' 3''
    ## 80       218.0          83.75 6' 11.75''              107.00        8' 11''
    ## 81       314.0          90.50   7' 6.5''              113.00         9' 5''
    ## 82       241.0          86.50   7' 2.5''              110.20      9' 2.25''
    ## 83       221.0          80.00     6' 8''              101.00         8' 5''
    ## 84       267.0          83.50  6' 11.5''              101.50       8' 5.5''
    ## 85       232.0          90.00     7' 6''              109.00         9' 1''
    ## 86       228.0          82.75 6' 10.75''              108.50       9' 0.5''
    ## 87       271.0          82.50  6' 10.5''              102.50       8' 6.5''
    ## 88       193.0          78.50   6' 6.5''              100.50       8' 4.5''
    ## 89       188.0          73.25  6' 1.25''               92.50       7' 8.5''
    ## 90       179.0          78.25  6' 6.25''               99.00         8' 3''
    ## 91       200.0          83.25 6' 11.25''              101.00         8' 5''
    ## 92       222.0          87.00     7' 3''              110.00         9' 2''
    ## 93       205.0          80.00     6' 8''               99.50       8' 3.5''
    ## 94       242.0          84.25  7' 0.25''              106.00        8' 10''
    ## 95       266.0          90.50   7' 6.5''              113.50       9' 5.5''
    ## 96       197.0          81.25  6' 9.25''              100.50       8' 4.5''
    ## 97       224.0          88.00     7' 4''              108.00         9' 0''
    ## 98       244.0          83.75 6' 11.75''              105.50       8' 9.5''
    ## 99       196.0          82.75 6' 10.75''              103.00         8' 7''
    ## 100      223.0          84.00     7' 0''              103.00         8' 7''
    ## 101      171.0          77.25  6' 5.25''               97.00         8' 1''
    ## 102      278.0          89.00     7' 5''              111.50       9' 3.5''
    ## 103      227.0          88.00     7' 4''              109.50       9' 1.5''
    ## 104      226.0          81.00     6' 9''              105.00         8' 9''
    ## 105      178.0          78.75  6' 6.75''              100.00         8' 4''
    ## 106      213.0          81.25  6' 9.25''              101.00         8' 5''
    ## 107      183.0          75.50   6' 3.5''               96.50       8' 0.5''
    ## 108      247.0          84.50   7' 0.5''              108.50       9' 0.5''
    ## 109      207.0          80.25  6' 8.25''               99.50       8' 3.5''
    ## 110      185.0          83.50  6' 11.5''              103.00         8' 7''
    ## 111      230.0          83.00    6' 11''              105.50       8' 9.5''
    ## 112      200.0          85.50   7' 1.5''              106.50      8' 10.5''
    ## 113      227.0          86.50   7' 2.5''              110.50       9' 2.5''
    ## 114      166.0          73.50   6' 1.5''               92.00         7' 8''
    ## 115      178.0          74.75  6' 2.75''               94.00        7' 10''
    ## 116      230.0          80.00     6' 8''              102.50       8' 6.5''
    ## 117      213.0          83.50  6' 11.5''              102.50       8' 6.5''
    ## 118      180.0          76.50   6' 4.5''               99.00         8' 3''
    ## 119      194.0          82.50  6' 10.5''              103.50       8' 7.5''
    ## 120      176.0          76.50   6' 4.5''               98.00         8' 2''
    ## 121      241.0          81.75  6' 9.75''              105.50       8' 9.5''
    ## 122      209.0          89.75  7' 5.75''              110.50       9' 2.5''
    ## 123      235.0          84.25  7' 0.25''              105.00         8' 9''
    ## 124      172.0          81.50   6' 9.5''               99.50       8' 3.5''
    ## 125      215.0          84.50   7' 0.5''              102.00         8' 6''
    ## 126      235.0          85.75  7' 1.75''              103.70      8' 7.75''
    ## 127      199.0          79.50   6' 7.5''               97.50       8' 1.5''
    ## 128      187.0          83.25 6' 11.25''              104.50       8' 8.5''
    ## 129      235.0          89.00     7' 5''              109.00         9' 1''
    ## 130      184.0          78.75  6' 6.75''               97.00         8' 1''
    ## 131      243.0          83.50  6' 11.5''              104.50       8' 8.5''
    ## 132      246.0          89.50   7' 5.5''              113.00         9' 5''
    ## 133      236.0          85.50   7' 1.5''              105.50       8' 9.5''
    ## 134      247.0          83.50  6' 11.5''              106.50      8' 10.5''
    ## 135      253.0          88.50   7' 4.5''              109.00         9' 1''
    ## 136      250.0          83.75 6' 11.75''              109.50       9' 1.5''
    ## 137      178.0          76.00     6' 4''               96.00         8' 0''
    ## 138      226.0          81.00     6' 9''               96.50       8' 0.5''
    ## 139      221.0          84.00     7' 0''              107.00        8' 11''
    ## 140      264.0          85.50   7' 1.5''              107.50      8' 11.5''
    ## 141      217.0          85.75  7' 1.75''              109.00         9' 1''
    ## 142      258.0          86.25  7' 2.25''              108.50       9' 0.5''
    ## 143      238.0          86.50   7' 2.5''              112.00         9' 4''
    ## 144      188.0          74.25  6' 2.25''               96.00         8' 0''
    ## 145      194.0          79.00     6' 7''               99.00         8' 3''
    ## 146      226.0          88.50   7' 4.5''              109.50       9' 1.5''
    ## 147      246.0          83.50  6' 11.5''              109.00         9' 1''
    ## 148      222.0          83.50  6' 11.5''              103.50       8' 7.5''
    ## 149      198.0          84.00     7' 0''              105.50       8' 9.5''
    ## 150      174.0          78.00     6' 6''               98.00         8' 2''
    ## 151      291.0          87.50   7' 3.5''              109.00         9' 1''
    ## 152      200.0          78.25  6' 6.25''              102.00         8' 6''
    ## 153      230.0          81.00     6' 9''              106.00        8' 10''
    ## 154      178.0          70.75 5' 10.75''               90.50       7' 6.5''
    ## 155      285.0          82.75 6' 10.75''              106.50      8' 10.5''
    ## 156      220.0          82.25 6' 10.25''              102.00         8' 6''
    ## 157      240.0          88.00     7' 4''              111.00         9' 3''
    ## 158      250.0          86.50   7' 2.5''              107.50      8' 11.5''
    ## 159      227.0          84.50   7' 0.5''              106.50      8' 10.5''
    ## 160      212.0          79.00     6' 7''              101.00         8' 5''
    ## 161      170.0          73.00     6' 1''               94.50      7' 10.5''
    ## 162      167.0          79.75  6' 7.75''               99.00         8' 3''
    ## 163      221.0          84.00     7' 0''              107.00        8' 11''
    ## 164      203.0          78.00     6' 6''               99.50       8' 3.5''
    ## 165      183.0          75.75  6' 3.75''               97.00         8' 1''
    ## 166      250.0          83.00    6' 11''              107.00        8' 11''
    ## 167      155.0          73.00     6' 1''               92.00         7' 8''
    ## 168      225.0          85.00     7' 1''              105.00         8' 9''
    ## 169      223.0          84.00     7' 0''              104.50       8' 8.5''
    ## 170      230.0          84.50   7' 0.5''              109.00         9' 1''
    ## 171      249.0          91.00     7' 7''              111.00         9' 3''
    ## 172      230.0          86.25  7' 2.25''              112.00         9' 4''
    ## 173      218.0          84.00     7' 0''              100.00         8' 4''
    ## 174      182.0          74.50   6' 2.5''               96.00         8' 0''
    ## 175      242.0          83.75 6' 11.75''              109.00         9' 1''
    ## 176      228.0          86.00     7' 2''              108.50       9' 0.5''
    ## 177      278.0          86.00     7' 2''              109.50       9' 1.5''
    ## 178      200.0          77.00     6' 5''               99.00         8' 3''
    ## 179      216.0          84.50   7' 0.5''              107.00        8' 11''
    ## 180      211.0          85.50   7' 1.5''              105.50       8' 9.5''
    ## 181      213.0          82.25 6' 10.25''              102.00         8' 6''
    ## 182      197.0          79.50   6' 7.5''              100.50       8' 4.5''
    ## 183      229.0          89.50   7' 5.5''              112.00         9' 4''
    ## 184      168.0          74.50   6' 2.5''               96.00         8' 0''
    ## 185      179.0          82.00    6' 10''              107.50      8' 11.5''
    ## 186      198.0          79.00     6' 7''              101.00         8' 5''
    ## 187      241.0          85.00     7' 1''              105.50       8' 9.5''
    ## 188      211.0          82.50  6' 10.5''              104.50       8' 8.5''
    ## 189      199.0          81.75  6' 9.75''              103.50       8' 7.5''
    ## 190      232.0          82.00    6' 10''              108.00         9' 0''
    ## 191      207.0          82.25 6' 10.25''              104.00         8' 8''
    ## 192      212.0          79.00     6' 7''              102.00         8' 6''
    ## 193      228.0          83.50  6' 11.5''              105.50       8' 9.5''
    ## 194      197.0          80.50   6' 8.5''              103.50       8' 7.5''
    ## 195      203.0          83.50  6' 11.5''              104.00         8' 8''
    ## 196      240.0          85.00     7' 1''              108.50       9' 0.5''
    ## 197      233.0          85.75  7' 1.75''              108.50       9' 0.5''
    ## 198      181.0          76.50   6' 4.5''               98.50       8' 2.5''
    ## 199      183.0          77.75  6' 5.75''               99.50       8' 3.5''
    ## 200      193.0          74.00     6' 2''               99.00         8' 3''
    ## 201      218.0          85.00     7' 1''              107.50      8' 11.5''
    ## 202      212.0          80.00     6' 8''              100.50       8' 4.5''
    ## 203      197.0          75.50   6' 3.5''               97.00         8' 1''
    ## 204      214.0          84.50   7' 0.5''              103.00         8' 7''
    ## 205      213.0          82.00    6' 10''              105.00         8' 9''
    ## 206      202.0          84.50   7' 0.5''              104.50       8' 8.5''
    ## 207      257.0          86.50   7' 2.5''              106.00        8' 10''
    ## 208      216.0          83.00    6' 11''              106.50      8' 10.5''
    ## 209      233.0          84.00     7' 0''              105.50       8' 9.5''
    ## 210      237.0          84.50   7' 0.5''              107.00        8' 11''
    ## 211      178.0          77.00     6' 5''               97.50       8' 1.5''
    ## 212      205.0          78.00     6' 6''              101.50       8' 5.5''
    ## 213      213.0          81.25  6' 9.25''              101.00         8' 5''
    ## 214      242.0          80.75  6' 8.75''              105.50       8' 9.5''
    ## 215      225.0          87.50   7' 3.5''              109.00         9' 1''
    ## 216      187.0          76.00     6' 4''               96.00         8' 0''
    ## 217      211.0          86.25  7' 2.25''              109.00         9' 1''
    ## 218      207.0          78.50   6' 6.5''               98.50       8' 2.5''
    ## 219      255.0          85.50   7' 1.5''              108.00         9' 0''
    ## 220      234.0          86.00     7' 2''              109.00         9' 1''
    ## 221      239.0          81.25  6' 9.25''              105.50       8' 9.5''
    ## 222      198.0          82.00    6' 10''              103.50       8' 7.5''
    ## 223      253.0          84.00     7' 0''              108.00         9' 0''
    ## 224      193.0          79.25  6' 7.25''              100.50       8' 4.5''
    ## 225      209.0          80.25  6' 8.25''              101.00         8' 5''
    ## 226      201.0          77.25  6' 5.25''               98.50       8' 2.5''
    ## 227      210.0          82.50  6' 10.5''              102.00         8' 6''
    ## 228      257.0          88.00     7' 4''              108.00         9' 0''
    ## 229      162.0          71.50  5' 11.5''               93.50       7' 9.5''
    ## 230      194.0          73.00     6' 1''               92.00         7' 8''
    ## 231      201.0          79.25  6' 7.25''              100.00         8' 4''
    ## 232      217.0          83.00    6' 11''              103.00         8' 7''
    ## 233      200.0          78.50   6' 6.5''              100.50       8' 4.5''
    ## 234      245.0          85.00     7' 1''              109.50       9' 1.5''
    ## 235      163.0          78.25  6' 6.25''               98.00         8' 2''
    ## 236      186.0          78.00     6' 6''               98.50       8' 2.5''
    ## 237      232.0          87.25  7' 3.25''              109.50       9' 1.5''
    ## 238      202.0          86.00     7' 2''              105.50       8' 9.5''
    ## 239      266.0          86.75  7' 2.75''              105.50       8' 9.5''
    ## 240      229.0          85.00     7' 1''              109.50       9' 1.5''
    ## 241      232.0          82.75 6' 10.75''              107.00        8' 11''
    ## 242      254.0          84.00     7' 0''              107.50      8' 11.5''
    ## 243      212.0          82.75 6' 10.75''              107.50      8' 11.5''
    ## 244      214.0          81.00     6' 9''              102.50       8' 6.5''
    ## 245      218.0          86.50   7' 2.5''              107.50      8' 11.5''
    ## 246      252.0          83.75 6' 11.75''              110.50       9' 2.5''
    ## 247      215.0          78.50   6' 6.5''              102.00         8' 6''
    ## 248      270.0          84.50   7' 0.5''              106.50      8' 10.5''
    ## 249      200.0          77.50   6' 5.5''               98.50       8' 2.5''
    ## 250      211.0          81.50   6' 9.5''              102.50       8' 6.5''
    ## 251      316.0          90.25  7' 6.25''              110.00         9' 2''
    ## 252      229.0          87.00     7' 3''               97.00         8' 1''
    ## 253      259.0          85.00     7' 1''              106.50      8' 10.5''
    ## 254      258.0          87.50   7' 3.5''              109.50       9' 1.5''
    ## 255      250.0          89.00     7' 5''              111.50       9' 3.5''
    ## 256      199.0          85.00     7' 1''              106.00        8' 10''
    ## 257      246.0          87.75  7' 3.75''              106.50      8' 10.5''
    ## 258      228.0          87.00     7' 3''              109.00         9' 1''
    ## 259      184.0          75.50   6' 3.5''               97.00         8' 1''
    ## 260      246.0          86.00     7' 2''              107.00        8' 11''
    ## 261      218.0          77.25  6' 5.25''               97.00         8' 1''
    ## 262      303.0          89.75  7' 5.75''              116.00         9' 8''
    ## 263      227.0          85.00     7' 1''              107.50      8' 11.5''
    ## 264      165.0          76.50   6' 4.5''               94.50      7' 10.5''
    ## 265      167.0          75.00     6' 3''               95.50      7' 11.5''
    ## 266      224.0          82.00    6' 10''              103.00         8' 7''
    ## 267      236.0          82.00    6' 10''              103.50       8' 7.5''
    ## 268      234.0          82.25 6' 10.25''              104.50       8' 8.5''
    ## 269      200.0          86.50   7' 2.5''              107.50      8' 11.5''
    ## 270      261.0          87.50   7' 3.5''              109.50       9' 1.5''
    ## 271      262.0          85.00     7' 1''              107.50      8' 11.5''
    ## 272      212.0          82.75 6' 10.75''              102.00         8' 6''
    ## 273      187.0          86.00     7' 2''              108.00         9' 0''
    ## 274      226.0          88.25  7' 4.25''              108.25      9' 0.25''
    ## 275      257.0          87.00     7' 3''              110.50       9' 2.5''
    ## 276      220.0          91.50   7' 7.5''              100.50       8' 4.5''
    ## 277      190.0          81.00     6' 9''               96.25       8' .25''
    ## 278      214.0          81.00     6' 9''              102.00         8' 6''
    ## 279      201.0          86.00     7' 2''              107.50      8' 11.5''
    ## 280      171.0          73.00     6' 1''               92.00         7' 8''
    ## 281      246.0          86.50   7' 2.5''              106.50      8' 10.5''
    ## 282      230.0          82.00    6' 10''              105.50       8' 9.5''
    ## 283      181.0          81.00     6' 9''               98.00         8' 2''
    ## 284      236.0          83.00    6' 11''              107.00        8' 11''
    ## 285      263.0          85.50   7' 1.5''              109.00         9' 1''
    ## 286      176.0          73.50   6' 1.5''               94.00        7' 10''
    ## 287      255.0          87.00     7' 3''              113.00         9' 5''
    ## 288      196.0          83.00    6' 11''              105.00         8' 9''
    ## 289      215.0          81.50   6' 9.5''              106.50      8' 10.5''
    ## 290      223.0          83.00    6' 11''              106.00        8' 10''
    ## 291      220.0          84.50   7' 0.5''              108.50       9' 0.5''
    ## 292      223.0          88.00     7' 4''              111.50       9' 3.5''
    ## 293      193.0          78.50   6' 6.5''               98.00         8' 2''
    ## 294      214.0          80.50   6' 8.5''              103.00         8' 7''
    ## 295      202.0          79.50   6' 7.5''               98.50       8' 2.5''
    ## 296      234.0          85.00     7' 1''              108.50       9' 0.5''
    ## 297      260.0          85.00     7' 1''              108.00         9' 0''
    ## 298      248.0          86.00     7' 2''              106.50      8' 10.5''
    ## 299      192.0          80.50   6' 8.5''               99.00         8' 3''
    ## 300      170.0          79.50   6' 7.5''               98.50       8' 2.5''
    ## 301      240.0          88.50   7' 4.5''              111.50       9' 3.5''
    ## 302      229.0          84.50   7' 0.5''              107.00        8' 11''
    ## 303      238.0          84.50   7' 0.5''              106.50      8' 10.5''
    ## 304      217.0          83.00    6' 11''              105.50       8' 9.5''
    ## 305      233.0          89.00     7' 5''              111.50       9' 3.5''
    ## 306      212.0          80.50   6' 8.5''              103.00         8' 7''
    ## 307      263.0          86.50   7' 2.5''              110.00         9' 2''
    ## 308      262.0          87.00     7' 3''              109.00         9' 1''
    ## 309      172.0          74.50   6' 2.5''               95.00        7' 11''
    ## 310      186.0          83.00    6' 11''              105.50       8' 9.5''
    ## 311      247.0          81.50   6' 9.5''              107.00        8' 11''
    ## 312      206.0          84.50   7' 0.5''              105.50       8' 9.5''
    ## 313      241.0          84.00     7' 0''              105.50       8' 9.5''
    ## 314      229.0          86.00     7' 2''              107.00        8' 11''
    ## 315      198.0          79.00     6' 7''              101.50       8' 5.5''
    ## 316      202.0          81.00     6' 9''              101.50       8' 5.5''
    ## 317      198.0          84.50   7' 0.5''              107.00        8' 11''
    ## 318      205.0          83.50  6' 11.5''              102.00         8' 6''
    ## 319      199.0          74.50   6' 2.5''               95.00        7' 11''
    ## 320      223.0          82.00    6' 10''              102.00         8' 6''
    ## 321      257.0          88.00     7' 4''              110.50       9' 2.5''
    ## 322      193.0          82.50  6' 10.5''              107.50      8' 11.5''
    ## 323      239.0          79.00     6' 7''              105.00         8' 9''
    ## 324      231.0          83.50  6' 11.5''              108.00         9' 0''
    ## 325      210.0          81.50   6' 9.5''              101.50       8' 5.5''
    ## 326      182.0          78.50   6' 6.5''               99.50       8' 3.5''
    ## 327      170.0          71.50  5' 11.5''               92.50       7' 8.5''
    ## 328      266.0          89.50   7' 5.5''              111.50       9' 3.5''
    ## 329      246.0          84.50   7' 0.5''              109.00         9' 1''
    ## 330      233.0          81.50   6' 9.5''              106.50      8' 10.5''
    ## 331      208.0          82.00    6' 10''              102.00         8' 6''
    ## 332      181.0          73.00     6' 1''               91.50       7' 7.5''
    ## 333      205.0          83.50  6' 11.5''              106.00        8' 10''
    ## 334      204.0          83.00    6' 11''              102.50       8' 6.5''
    ## 335      234.0          83.00    6' 11''              107.00        8' 11''
    ## 336      221.0          84.00     7' 0''              106.50      8' 10.5''
    ## 337      227.0          82.00    6' 10''              104.00         8' 8''
    ## 338      228.0          81.00     6' 9''              104.00         8' 8''
    ## 339      233.0          86.00     7' 2''              108.50       9' 0.5''
    ## 340      233.0          86.00     7' 2''              109.50       9' 1.5''
    ## 341      272.0          83.00    6' 11''              107.50      8' 11.5''
    ## 342      198.0          79.00     6' 7''              103.50       8' 7.5''
    ## 343      226.0          84.00     7' 0''              105.50       8' 9.5''
    ## 344      183.0          78.00     6' 6''               99.50       8' 3.5''
    ## 345      222.0          84.00     7' 0''              106.00        8' 10''
    ## 346      197.0          83.50  6' 11.5''              105.50       8' 9.5''
    ## 347      240.0          85.50   7' 1.5''              109.50       9' 1.5''
    ## 348      190.0          82.00    6' 10''               98.50       8' 2.5''
    ## 349      183.0          84.00     7' 0''              102.50       8' 6.5''
    ## 350      236.4          87.00     7' 3''              108.50       9' 0.5''
    ## 351      223.2          81.50   6' 9.5''              104.00         8' 8''
    ## 352      227.8          83.25 6' 11.25''              112.50       9' 4.5''
    ## 353      206.4          85.00     7' 1''              108.00         9' 0''
    ## 354      210.0          82.25 6' 10.25''              101.00         8' 5''
    ## 355      206.2          85.50   7' 1.5''              104.50       8' 8.5''
    ## 356      250.6          87.00     7' 3''              110.50       9' 2.5''
    ## 357      188.8          75.50   6' 3.5''               93.00         7' 9''
    ## 358      188.6          75.00     6' 3''               96.50       8' 0.5''
    ## 359      252.4          83.50  6' 11.5''              106.50      8' 10.5''
    ## 360      165.2          71.75 5' 11.75''               93.50       7' 9.5''
    ## 361      255.4          87.50   7' 3.5''              109.00         9' 1''
    ## 362      160.8          71.50  5' 11.5''               94.00        7' 10''
    ## 363      176.6          74.75  6' 2.75''               98.00         8' 2''
    ## 364      199.4          76.25  6' 4.25''               98.00         8' 2''
    ## 365      172.0          79.50   6' 7.5''              101.50       8' 5.5''
    ## 366      242.4          90.00     7' 6''              110.50       9' 2.5''
    ## 367      235.2          90.75  7' 6.75''              111.50       9' 3.5''
    ## 368      212.8          85.50   7' 1.5''              106.00        8' 10''
    ## 369      243.6          86.50   7' 2.5''              110.50       9' 2.5''
    ## 370      214.0          88.50   7' 4.5''              110.50       9' 2.5''
    ## 371      189.6          82.75 6' 10.75''              103.00         8' 7''
    ## 372      195.2          81.25  6' 9.25''               96.50       8' 0.5''
    ## 373      225.2          87.50   7' 3.5''              110.00         9' 2''
    ## 374      216.6          80.00     6' 8''              104.00         8' 8''
    ## 375      225.4          85.50   7' 1.5''              103.00         8' 7''
    ## 376      192.0          81.75  6' 9.75''              104.00         8' 8''
    ## 377      232.4          82.00    6' 10''              104.50       8' 8.5''
    ## 378      178.8          77.25  6' 5.25''               98.50       8' 2.5''
    ## 379      202.2          84.50   7' 0.5''              107.50      8' 11.5''
    ## 380      208.8          85.25  7' 1.25''              109.50       9' 1.5''
    ## 381      197.6          79.50   6' 7.5''              100.00         8' 4''
    ## 382      242.2          86.75  7' 2.75''              108.50       9' 0.5''
    ## 383      217.0          83.00    6' 11''              105.50       8' 9.5''
    ## 384      249.4          82.00    6' 10''              107.50      8' 11.5''
    ## 385      193.4          77.50   6' 5.5''               98.50       8' 2.5''
    ## 386      203.2          81.00     6' 9''              101.00         8' 5''
    ## 387      229.5          84.00     7' 0''              106.50      8' 10.5''
    ## 388      154.4          72.00     6' 0''               92.00         7' 8''
    ## 389      186.6          85.00     7' 1''              104.00         8' 8''
    ## 390      258.8          85.25  7' 1.25''              105.00         8' 9''
    ## 391      201.0          82.75 6' 10.75''              103.50       8' 7.5''
    ## 392      228.6          81.00     6' 9''              105.50       8' 9.5''
    ## 393      231.4          84.00     7' 0''              106.50      8' 10.5''
    ## 394      174.6          75.50   6' 3.5''               94.00        7' 10''
    ## 395      255.2          81.75  6' 9.75''              105.50       8' 9.5''
    ## 396      196.0          78.00     6' 6''               99.50       8' 3.5''
    ## 397      238.8          83.75 6' 11.75''              108.50       9' 0.5''
    ## 398      178.0          76.25  6' 4.25''               93.00         7' 9''
    ## 399      221.8          82.75 6' 10.75''              105.00         8' 9''
    ## 400      184.8          77.50   6' 5.5''               96.50       8' 0.5''
    ## 401      178.8          74.75  6' 2.75''               95.00        7' 11''
    ## 402      211.4          83.25 6' 11.25''              104.50       8' 8.5''
    ## 403      241.6          89.00     7' 5''              111.50       9' 3.5''
    ## 404      255.8          84.00     7' 0''              107.00        8' 11''
    ## 405      194.2          79.50   6' 7.5''              102.50       8' 6.5''
    ## 406      258.8          87.50   7' 3.5''              108.00         9' 0''
    ## 407      261.0          85.75  7' 1.75''              109.00         9' 1''
    ## 408      176.2          75.00     6' 3''               95.00        7' 11''
    ## 409      200.0          80.00     6' 8''              102.00         8' 6''
    ## 410      237.8          85.50   7' 1.5''              106.50      8' 10.5''
    ## 411      236.6          84.25  7' 0.25''              109.00         9' 1''
    ## 412      229.6          83.00    6' 11''              106.00        8' 10''
    ## 413      269.4          86.00     7' 2''              108.00         9' 0''
    ## 414      202.4          78.25  6' 6.25''               98.00         8' 2''
    ## 415      218.0          85.25  7' 1.25''              106.50      8' 10.5''
    ## 416      228.2          87.50   7' 3.5''              108.00         9' 0''
    ## 417      186.8          82.00    6' 10''              100.50       8' 4.5''
    ## 418      202.6          80.75  6' 8.75''              104.50       8' 8.5''
    ## 419      250.0          82.25 6' 10.25''              104.00         8' 8''
    ## 420      203.0          79.75  6' 7.75''              101.50       8' 5.5''
    ## 421      234.0          88.75  7' 4.75''              110.00         9' 2''
    ## 422      214.0          82.00    6' 10''              102.50       8' 6.5''
    ## 423      221.0          83.50  6' 11.5''              103.50       8' 7.5''
    ## 424      224.0          79.00     6' 7''              100.00         8' 4''
    ## 425      240.0          88.00     7' 4''              109.00         9' 1''
    ## 426      227.0          84.25  7' 0.25''              104.00         8' 8''
    ## 427      206.0          85.00     7' 1''              104.50       8' 8.5''
    ## 428      265.0          82.00    6' 10''              103.50       8' 7.5''
    ## 429      194.0          78.00     6' 6''               96.50       8' 0.5''
    ## 430      209.0          83.50  6' 11.5''              103.00         8' 7''
    ## 431      223.0          83.25 6' 11.25''              103.50       8' 7.5''
    ## 432      218.0          81.50   6' 9.5''              102.50       8' 6.5''
    ## 433      238.0          84.00     7' 0''              107.50      8' 11.5''
    ## 434      234.0          81.00     6' 9''              103.00         8' 7''
    ## 435      204.0          82.00    6' 10''              103.50       8' 7.5''
    ## 436      184.0          74.50   6' 2.5''               92.50       7' 8.5''
    ## 437      224.0          82.00    6' 10''              103.00         8' 7''
    ## 438      183.0          78.75  6' 6.75''               98.00         8' 2''
    ## 439      253.0          84.00     7' 0''              106.00        8' 10''
    ## 440      182.0          76.50   6' 4.5''               95.50      7' 11.5''
    ## 441      171.0          75.00     6' 3''               94.50      7' 10.5''
    ## 442      212.0          78.25  6' 6.25''               97.00         8' 1''
    ## 443      247.0          85.00     7' 1''              105.50       8' 9.5''
    ## 444      222.0          87.00     7' 3''              107.50      8' 11.5''
    ## 445      201.0          82.00    6' 10''              103.00         8' 7''
    ## 446      231.0          84.75  7' 0.75''              107.00        8' 11''
    ## 447      216.0          78.25  6' 6.25''               99.00         8' 3''
    ## 448      219.0          84.25  7' 0.25''              107.50      8' 11.5''
    ## 449      238.0          85.50   7' 1.5''              105.50       8' 9.5''
    ## 450      236.0          85.00     7' 1''              107.00        8' 11''
    ## 451      198.0          79.00     6' 7''               96.50       8' 0.5''
    ## 452      224.0          88.75  7' 4.75''              109.00         9' 1''
    ## 453      211.0          81.50   6' 9.5''              101.50       8' 5.5''
    ## 454      202.0          81.25  6' 9.25''              104.00         8' 8''
    ## 455      266.0          87.00     7' 3''              104.00         8' 8''
    ## 456      179.0          78.75  6' 6.75''              100.50       8' 4.5''
    ## 457      190.0          78.25  6' 6.25''               97.00         8' 1''
    ## 458      187.0          77.75  6' 5.75''               96.50       8' 0.5''
    ## 459      177.0          72.75  6' 0.75''               92.50       7' 8.5''
    ## 460      232.0          84.00     7' 0''              105.50       8' 9.5''
    ## 461      251.0          84.00     7' 0''              101.50       8' 5.5''
    ## 462      258.0          85.50   7' 1.5''              105.50       8' 9.5''
    ## 463      208.0          78.50   6' 6.5''               98.50       8' 2.5''
    ## 464      198.0          82.00    6' 10''              105.00         8' 9''
    ## 465      258.0          88.25  7' 4.25''              107.00        8' 11''
    ## 466      223.0          81.50   6' 9.5''              100.00         8' 4''
    ## 467      216.0          81.50   6' 9.5''              103.00         8' 7''
    ## 468      249.0          89.75  7' 5.75''              113.00         9' 5''
    ## 469      204.0          79.50   6' 7.5''              100.50       8' 4.5''
    ## 470      175.0          77.00     6' 5''               94.00        7' 10''
    ## 471      204.0          80.50   6' 8.5''               99.50       8' 3.5''
    ## 472      190.0          75.25  6' 3.25''               97.50       8' 1.5''
    ## 473      217.0          86.50   7' 2.5''              105.50       8' 9.5''
    ## 474      207.0          80.00     6' 8''              101.00         8' 5''
    ## 475      209.0          80.75  6' 8.75''              102.00         8' 6''
    ## 476      237.0          92.50   7' 8.5''              113.00         9' 5''
    ## 477      223.0          88.25  7' 4.25''              108.50       9' 0.5''
    ## 478      220.0          83.50  6' 11.5''              104.50       8' 8.5''
    ## 479      259.0          81.50   6' 9.5''              103.50       8' 7.5''
    ## 480      238.0          84.50   7' 0.5''              104.00         8' 8''
    ## 481      241.0          88.25  7' 4.25''              107.50      8' 11.5''
    ## 482      280.0          86.25  7' 2.25''              109.00         9' 1''
    ## 483      208.0          77.00     6' 5''               97.50       8' 1.5''
    ## 484      217.0          87.00     7' 3''              108.00         9' 0''
    ## 485      224.0          78.50   6' 6.5''               99.50       8' 3.5''
    ## 486      267.0          84.75  7' 0.75''              108.50       9' 0.5''
    ## 487      195.0          76.00     6' 4''               96.00         8' 0''
    ## 488      171.0          78.00     6' 6''               96.00         8' 0''
    ## 489      211.0          85.50   7' 1.5''              109.00         9' 1''
    ## 490      258.0          88.25  7' 4.25''              104.00         8' 8''
    ## 491      227.0          87.00     7' 3''              104.00         8' 8''
    ## 492      285.0          88.25  7' 4.25''              106.50      8' 10.5''
    ## 493      215.0          79.00     6' 7''               97.50       8' 1.5''
    ## 494      227.0          82.00    6' 10''              101.00         8' 5''
    ## 495      210.0          81.00     6' 9''              103.00         8' 7''
    ## 496      240.0          86.00     7' 2''              107.00        8' 11''
    ## 497      240.0          85.50   7' 1.5''              106.50      8' 10.5''
    ## 498      185.0          80.25  6' 8.25''              103.00         8' 7''
    ## 499      161.0          76.00     6' 4''               94.00        7' 10''
    ## 500      170.0          76.00     6' 4''               95.50      7' 11.5''
    ## 501      221.0          80.50   6' 8.5''               97.50       8' 1.5''
    ## 502      236.0          86.00     7' 2''              107.00        8' 11''
    ## 503      175.0          77.75  6' 5.75''               94.50      7' 10.5''
    ## 504      203.0          80.25  6' 8.25''              101.50       8' 5.5''
    ## 505      250.0          85.25  7' 1.25''              107.50      8' 11.5''
    ## 506      194.0          77.50   6' 5.5''               98.00         8' 2''
    ## 507      230.0          88.00     7' 4''              109.50       9' 1.5''
    ## 508      191.0          82.00    6' 10''              100.00         8' 4''
    ## 509      219.0          79.00     6' 7''              102.00         8' 6''
    ## 510      215.0          88.75  7' 4.75''              110.00         9' 2''
    ## 511      244.0          83.50  6' 11.5''              104.00         8' 8''
    ## 512      271.0          87.25  7' 3.25''              108.50       9' 0.5''
    ## 513      228.0          85.25  7' 1.25''              103.00         8' 7''
    ## 514      173.0          74.25  6' 2.25''               96.00         8' 0''
    ## 515      238.0          83.75 6' 11.75''              103.00         8' 7''
    ## 516      244.0          84.50   7' 0.5''              110.00         9' 2''
    ## 517      181.0          79.00     6' 7''               94.00        7' 10''
    ## 518      232.0          86.25  7' 2.25''              108.50       9' 0.5''
    ## 519      246.0          84.75  7' 0.75''              107.00        8' 11''
    ## 520      215.0          83.25 6' 11.25''              105.00         8' 9''
    ## 521      209.0          87.00     7' 3''              111.00         9' 3''
    ## 522      198.0          80.25  6' 8.25''              101.50       8' 5.5''
    ## 523      208.0          90.00     7' 6''              104.00         8' 8''
    ## 524      175.0          74.00     6' 2''               93.00         7' 9''
    ## 525      216.0          77.00     6' 5''               98.00         8' 2''
    ## 526      235.0          84.00     7' 0''              105.50       8' 9.5''
    ## 527      183.0          72.00     6' 0''               94.00        7' 10''
    ## 528      204.0          83.00    6' 11''               99.50       8' 3.5''
    ## 529      237.0          82.50  6' 10.5''              106.50      8' 10.5''
    ## 530      248.0          83.00    6' 11''              102.50       8' 6.5''
    ## 531      213.0          86.00     7' 2''              107.00        8' 11''
    ## 532      186.0          78.50   6' 6.5''               98.00         8' 2''
    ## 533      267.0          85.00     7' 1''              108.50       9' 0.5''
    ## 534      194.0          78.50   6' 6.5''               99.00         8' 3''
    ## 535      218.0          82.00    6' 10''              103.50       8' 7.5''
    ## 536      220.0          82.50  6' 10.5''              104.00         8' 8''
    ## 537      240.0          85.00     7' 1''              106.50      8' 10.5''
    ## 538      218.0          83.00    6' 11''              105.00         8' 9''
    ## 539      201.0          80.00     6' 8''              102.50       8' 6.5''
    ## 540      211.0          84.25  7' 0.25''              105.00         8' 9''
    ## 541      223.0          85.25  7' 1.25''              106.50      8' 10.5''
    ## 542      257.0          88.25  7' 4.25''              112.00         9' 4''
    ## 543      226.0          83.75 6' 11.75''              106.00        8' 10''
    ## 544      188.0          75.25  6' 3.25''               95.00        7' 11''
    ## 545      252.0          88.50   7' 4.5''              110.50       9' 2.5''
    ## 546      208.0          80.00     6' 8''              100.50       8' 4.5''
    ## 547      214.0          81.00     6' 9''              102.50       8' 6.5''
    ## 548      185.0          80.75  6' 8.75''              101.00         8' 5''
    ## 549      185.0          76.25  6' 4.25''               97.50       8' 1.5''
    ## 550      185.0          77.50   6' 5.5''               99.00         8' 3''
    ## 551      233.0          82.75 6' 10.75''              106.00        8' 10''
    ## 552      199.0          78.50   6' 6.5''              101.00         8' 5''
    ## 553      207.0          79.25  6' 7.25''              100.50       8' 4.5''
    ## 554      227.0          81.50   6' 9.5''              104.00         8' 8''
    ## 555      212.0          81.00     6' 9''              105.00         8' 9''
    ## 556      168.0          74.50   6' 2.5''               93.50       7' 9.5''
    ## 557      222.0          82.00    6' 10''              103.50       8' 7.5''
    ## 558      221.0          85.00     7' 1''              104.00         8' 8''
    ## 559      242.0          86.50   7' 2.5''              107.50      8' 11.5''
    ## 560      249.0          88.00     7' 4''              108.50       9' 0.5''
    ## 561      242.0          83.00    6' 11''              108.00         9' 0''
    ## 562      241.0          87.00     7' 3''              110.00         9' 2''
    ## 563      242.0          88.00     7' 4''              109.00         9' 1''
    ## 564      167.0          72.75  6' 0.75''               92.00         7' 8''
    ## 565      211.0          86.25  7' 2.25''              108.00         9' 0''
    ## 566      200.0          87.75  7' 3.75''              108.50       9' 0.5''
    ## 567      212.0          85.00     7' 1''              107.00        8' 11''
    ## 568      210.0          83.50  6' 11.5''              106.00        8' 10''
    ## 569      206.0          84.00     7' 0''              100.50       8' 4.5''
    ## 570      220.0          83.50  6' 11.5''              106.00        8' 10''
    ## 571      216.0          82.75 6' 10.75''              107.00        8' 11''
    ## 572      171.5          75.50   6' 3.5''               94.50      7' 10.5''
    ## 573      204.0          75.50   6' 3.5''               97.00         8' 1''
    ## 574      239.0          84.25  7' 0.25''              107.00        8' 11''
    ## 575      184.0          77.00     6' 5''               98.00         8' 2''
    ## 576      166.0          73.75  6' 1.75''               94.00        7' 10''
    ## 577      252.0          82.00    6' 10''              106.00        8' 10''
    ## 578      190.0          79.25  6' 7.25''              104.50       8' 8.5''
    ## 579      201.0          78.25  6' 6.25''               97.50       8' 1.5''
    ## 580      229.6          83.25 6' 11.25''              108.00         9' 0''
    ## 581      213.0          77.75  6' 5.75''               99.50       8' 3.5''
    ## 582      256.0          84.50   7' 0.5''              109.00         9' 1''
    ## 583      265.0          85.75  7' 1.75''              107.00        8' 11''
    ## 584      208.0          83.50  6' 11.5''              105.00         8' 9''
    ## 585      230.0          83.50  6' 11.5''              104.50       8' 8.5''
    ## 586      210.0          87.25  7' 3.25''              106.00        8' 10''
    ## 587      224.0          84.25  7' 0.25''               97.50       8' 1.5''
    ## 588      220.5          83.25 6' 11.25''              104.50       8' 8.5''
    ## 589      194.5          83.50  6' 11.5''              103.50       8' 7.5''
    ## 590      220.0          88.00     7' 4''              107.50      8' 11.5''
    ## 591      258.0          86.50   7' 2.5''              111.50       9' 3.5''
    ## 592      222.0          81.00     6' 9''               99.00         8' 3''
    ## 593      290.0          82.25 6' 10.25''              106.00        8' 10''
    ## 594      221.0          82.00    6' 10''              108.00         9' 0''
    ## 595      204.0          81.00     6' 9''              103.50       8' 7.5''
    ## 596      235.0          87.00     7' 3''              109.50       9' 1.5''
    ## 597      250.0          87.00     7' 3''              108.00         9' 0''
    ## 598      181.0          81.00     6' 9''               97.50       8' 1.5''
    ## 599      234.0          85.25  7' 1.25''              104.50       8' 8.5''
    ## 600      226.0          82.75 6' 10.75''              109.00         9' 1''
    ## 601      196.0          80.75  6' 8.75''               98.50       8' 2.5''
    ## 602      220.0          87.00     7' 3''              109.50       9' 1.5''
    ## 603      240.5          83.25 6' 11.25''              106.50      8' 10.5''
    ## 604      213.0          81.50   6' 9.5''              105.00         8' 9''
    ## 605      240.0          84.25  7' 0.25''              107.00        8' 11''
    ## 606      250.0          90.00     7' 6''              113.50       9' 5.5''
    ## 607      247.0          88.00     7' 4''              110.00         9' 2''
    ## 608      221.0          80.50   6' 8.5''              103.50       8' 7.5''
    ## 609      222.0          80.00     6' 8''              102.50       8' 6.5''
    ## 610      258.0          89.50   7' 5.5''              113.00         9' 5''
    ## 611      255.0          83.25 6' 11.25''              106.00        8' 10''
    ## 612      273.0          85.00     7' 1''              109.50       9' 1.5''
    ## 613      200.0          78.00     6' 6''               99.50       8' 3.5''
    ## 614      227.0          85.75  7' 1.75''              105.00         8' 9''
    ## 615      221.0          86.50   7' 2.5''              103.50       8' 7.5''
    ## 616      241.0          90.00     7' 6''              114.50       9' 6.5''
    ## 617      175.5          74.50   6' 2.5''               95.00        7' 11''
    ## 618      198.0          82.00    6' 10''               98.50       8' 2.5''
    ## 619      239.5          85.00     7' 1''              109.50       9' 1.5''
    ## 620      208.0          77.00     6' 5''               96.00         8' 0''
    ## 621      240.0          82.50  6' 10.5''              105.50       8' 9.5''
    ## 622      225.5          82.50  6' 10.5''              103.50       8' 7.5''
    ## 623      242.2          85.00     7' 1''              107.50      8' 11.5''
    ## 624      197.0          87.00     7' 3''              109.00         9' 1''
    ## 625      244.0          83.25 6' 11.25''              107.50      8' 11.5''
    ## 626      173.0          73.25  6' 1.25''               95.00        7' 11''
    ## 627      196.0          80.00     6' 8''              100.50       8' 4.5''
    ## 628      196.0          80.00     6' 8''               98.50       8' 2.5''
    ## 629      183.5          76.25  6' 4.25''               97.50       8' 1.5''
    ## 630      210.0          81.00     6' 9''               99.50       8' 3.5''
    ## 631      166.2          79.25  6' 7.25''               98.00         8' 2''
    ## 632      217.0          83.00    6' 11''              103.50       8' 7.5''
    ## 633      199.0          80.00     6' 8''              101.00         8' 5''
    ## 634      198.5          81.00     6' 9''              102.50       8' 6.5''
    ## 635      193.0          82.00    6' 10''              104.00         8' 8''
    ## 636      192.0          79.75  6' 7.75''              100.00         8' 4''
    ## 637      199.0          81.25  6' 9.25''              103.00         8' 7''
    ## 638      236.2          86.00     7' 2''              107.50      8' 11.5''
    ## 639      239.6          85.00     7' 1''              107.00        8' 11''
    ## 640      182.4          81.75  6' 9.75''              100.00         8' 4''
    ## 641      276.6          86.00     7' 2''              106.50      8' 10.5''
    ## 642      224.6          86.50   7' 2.5''              107.50      8' 11.5''
    ## 643      206.4          79.00     6' 7''              101.00         8' 5''
    ## 644      211.2          81.25  6' 9.25''              106.50      8' 10.5''
    ## 645      210.8          81.00     6' 9''              102.50       8' 6.5''
    ## 646      228.4          86.50   7' 2.5''              109.50       9' 1.5''
    ## 647      166.2          75.00     6' 3''               96.50       8' 0.5''
    ## 648      227.4          83.00    6' 11''              106.50      8' 10.5''
    ## 649      181.0          75.50   6' 3.5''               97.00         8' 1''
    ## 650      191.8          86.75  7' 2.75''              110.00         9' 2''
    ## 651      211.2          81.00     6' 9''              102.50       8' 6.5''
    ## 652      183.4          78.00     6' 6''               97.50       8' 1.5''
    ## 653      202.4          78.50   6' 6.5''              100.00         8' 4''
    ## 654      220.6          83.25 6' 11.25''              104.00         8' 8''
    ## 655      195.6          76.00     6' 4''               95.50      7' 11.5''
    ## 656      214.4          88.00     7' 4''              109.00         9' 1''
    ## 657      208.0          82.00    6' 10''              103.00         8' 7''
    ## 658      248.4          83.25 6' 11.25''              105.00         8' 9''
    ## 659      234.2          83.50  6' 11.5''              106.00        8' 10''
    ## 660      222.0          82.75 6' 10.75''              103.50       8' 7.5''
    ## 661      214.6          82.25 6' 10.25''              102.50       8' 6.5''
    ## 662      246.2          85.25  7' 1.25''              108.00         9' 0''
    ## 663      232.4          85.50   7' 1.5''              108.00         9' 0''
    ## 664      199.0          79.00     6' 7''              100.50       8' 4.5''
    ## 665      209.2          82.25 6' 10.25''              105.00         8' 9''
    ## 666      257.2          84.75  7' 0.75''              105.50       8' 9.5''
    ## 667      196.6          72.75  6' 0.75''               94.50      7' 10.5''
    ## 668      163.6          74.50   6' 2.5''               97.00         8' 1''
    ## 669      185.2          74.50   6' 2.5''               96.00         8' 0''
    ## 670      190.4          79.25  6' 7.25''               99.50       8' 3.5''
    ## 671      211.4          76.50   6' 4.5''               98.00         8' 2''
    ## 672      175.4          74.00     6' 2''               95.00        7' 11''
    ## 673      258.2          85.50   7' 1.5''              111.00         9' 3''
    ## 674      192.8          75.75  6' 3.75''               97.00         8' 1''
    ## 675      212.0          81.75  6' 9.75''              104.50       8' 8.5''
    ## 676      243.0          84.75  7' 0.75''              106.50      8' 10.5''
    ## 677      207.4          80.75  6' 8.75''              101.00         8' 5''
    ## 678      175.2          79.50   6' 7.5''               98.50       8' 2.5''
    ## 679      194.4          77.00     6' 5''               99.00         8' 3''
    ## 680      213.2          81.00     6' 9''              103.50       8' 7.5''
    ## 681      222.8          82.75 6' 10.75''              105.50       8' 9.5''
    ## 682      237.4          89.00     7' 5''              113.00         9' 5''
    ## 683      236.0          88.75  7' 4.75''              111.50       9' 3.5''
    ## 684      216.0          87.25  7' 3.25''              108.50       9' 0.5''
    ## 685      207.6          80.50   6' 8.5''              104.00         8' 8''
    ## 686      217.8          83.25 6' 11.25''              104.50       8' 8.5''
    ## 687      235.8          81.75  6' 9.75''              106.00        8' 10''
    ## 688      229.0          84.00     7' 0''              109.00         9' 1''
    ## 689      180.4          79.25  6' 7.25''               98.50       8' 2.5''
    ## 690      280.0          84.25  7' 0.25''              107.50      8' 11.5''
    ## 691      217.2          74.50   6' 2.5''               94.00        7' 10''
    ## 692      291.8          89.75  7' 5.75''              113.00         9' 5''
    ## 693      198.4          79.00     6' 7''              101.00         8' 5''
    ## 694      226.6          84.00     7' 0''              108.00         9' 0''
    ## 695      207.8          84.25  7' 0.25''              107.50      8' 11.5''
    ## 696      245.2          88.00     7' 4''              110.00         9' 2''
    ## 697      301.8          88.50   7' 4.5''              109.50       9' 1.5''
    ## 698      231.6          85.75  7' 1.75''              107.50      8' 11.5''
    ## 699      240.2          81.50   6' 9.5''              106.00        8' 10''
    ## 700      211.0          79.75  6' 7.75''              103.00         8' 7''
    ## 701      225.8          84.75  7' 0.75''              102.00         8' 6''
    ## 702      210.2          83.25 6' 11.25''              105.00         8' 9''
    ## 703      203.6          81.25  6' 9.25''              104.50       8' 8.5''
    ## 704      226.6          84.75  7' 0.75''              107.00        8' 11''
    ## 705      195.2          80.00     6' 8''               99.50       8' 3.5''
    ## 706      206.4          85.00     7' 1''              106.00        8' 10''
    ## 707      216.0          81.25  6' 9.25''              101.00         8' 5''
    ## 708      210.0          81.00     6' 9''              100.50       8' 4.5''
    ## 709      233.4          84.00     7' 0''              107.50      8' 11.5''
    ## 710      246.6          86.25  7' 2.25''              108.50       9' 0.5''
    ## 711      269.2          88.25  7' 4.25''              110.50       9' 2.5''
    ## 712      268.2          85.00     7' 1''              109.50       9' 1.5''
    ## 713      239.6          85.25  7' 1.25''              107.00        8' 11''
    ## 714      191.6          79.00     6' 7''              100.00         8' 4''
    ## 715      230.2          85.50   7' 1.5''              110.00         9' 2''
    ## 716      213.2          84.00     7' 0''              107.50      8' 11.5''
    ## 717      222.4          89.75  7' 5.75''              112.00         9' 4''
    ## 718      227.0          82.50  6' 10.5''              103.00         8' 7''
    ## 719      208.8          79.50   6' 7.5''              103.00         8' 7''
    ## 720      213.8          80.00     6' 8''              103.50       8' 7.5''
    ## 721      237.4          88.50   7' 4.5''              106.50      8' 10.5''
    ## 722      210.2          87.50   7' 3.5''              109.50       9' 1.5''
    ## 723      195.6          81.25  6' 9.25''              101.50       8' 5.5''
    ## 724      208.4          78.00     6' 6''               99.50       8' 3.5''
    ## 725      202.6          81.00     6' 9''              101.00         8' 5''
    ## 726      226.6          91.00     7' 7''              113.00         9' 5''
    ## 727      217.0          87.75  7' 3.75''              109.50       9' 1.5''
    ## 728      194.6          85.00     7' 1''              101.00         8' 5''
    ## 729      192.6          82.00    6' 10''              103.50       8' 7.5''
    ## 730      222.2          79.50   6' 7.5''              101.50       8' 5.5''
    ## 731      174.4          74.25  6' 2.25''               95.50      7' 11.5''
    ## 732      196.6          78.00     6' 6''               99.00         8' 3''
    ## 733      237.6          87.25  7' 3.25''              107.50      8' 11.5''
    ## 734      187.8          78.25  6' 6.25''              101.00         8' 5''
    ## 735      224.6          84.00     7' 0''              108.00         9' 0''
    ## 736      196.0          76.50   6' 4.5''               96.50       8' 0.5''
    ## 737      197.6          76.25  6' 4.25''               96.50       8' 0.5''
    ## 738      228.4          81.50   6' 9.5''              104.00         8' 8''
    ## 739      228.0          83.75 6' 11.75''              106.00        8' 10''
    ## 740      222.8          83.00    6' 11''              103.50       8' 7.5''
    ## 741      186.8          81.00     6' 9''              105.00         8' 9''
    ## 742      204.6          82.50  6' 10.5''              103.50       8' 7.5''
    ## 743      241.8          86.00     7' 2''              106.50      8' 10.5''
    ## 744      216.4          79.50   6' 7.5''               96.50       8' 0.5''
    ## 745      219.8          86.00     7' 2''              107.50      8' 11.5''
    ## 746      186.2          77.50   6' 5.5''               99.00         8' 3''
    ## 747      259.2          85.50   7' 1.5''              109.50       9' 1.5''
    ## 748      176.8          78.75  6' 6.75''               98.50       8' 2.5''
    ## 749      198.2          81.75  6' 9.75''              101.50       8' 5.5''
    ## 750      227.4          87.00     7' 3''              106.00        8' 10''
    ## 751      205.0          82.50  6' 10.5''              103.00         8' 7''
    ## 752      223.2          84.00     7' 0''              105.50       8' 9.5''
    ## 753      202.2          83.00    6' 11''              103.50       8' 7.5''
    ## 754      216.2          80.00     6' 8''              101.00         8' 5''
    ## 755      209.0          79.50   6' 7.5''               96.50       8' 0.5''
    ## 756      204.0          78.25  6' 6.25''               99.50       8' 3.5''
    ## 757      191.4          81.50   6' 9.5''               99.50       8' 3.5''
    ## 758      240.8          82.75 6' 10.75''              106.50      8' 10.5''
    ## 759      229.6          82.00    6' 10''              105.50       8' 9.5''
    ## 760      190.0          79.50   6' 7.5''              102.00         8' 6''
    ## 761      221.2          81.50   6' 9.5''              104.50       8' 8.5''
    ## 762      207.2          82.25 6' 10.25''              106.50      8' 10.5''
    ## 763      194.6          77.25  6' 5.25''               98.00         8' 2''
    ## 764      221.8          81.50   6' 9.5''               99.50       8' 3.5''
    ## 765      228.0          82.10  6' 10.1''              106.00        8' 10''
    ## 766      230.2          85.00     7' 1''              103.50       8' 7.5''
    ## 767      241.6          85.25  7' 1.25''              106.50      8' 10.5''
    ## 768      252.8          86.50   7' 2.5''              106.50      8' 10.5''
    ## 769      187.6          77.50   6' 5.5''               99.00         8' 3''
    ## 770      223.0          84.50   7' 0.5''              107.50      8' 11.5''
    ## 771      186.0          73.75  6' 1.75''               91.50       7' 7.5''
    ## 772      239.0          85.00     7' 1''              107.50      8' 11.5''
    ## 773      205.6          81.00     6' 9''              103.50       8' 7.5''
    ## 774      227.4          85.25  7' 1.25''              108.50       9' 0.5''
    ## 775      262.4          89.00     7' 5''              110.50       9' 2.5''
    ## 776      259.8          88.50   7' 4.5''              112.50       9' 4.5''
    ## 777      184.0          75.50   6' 3.5''               91.50       7' 7.5''
    ## 778      248.4          85.50   7' 1.5''              108.00         9' 0''
    ## 779      247.2          84.25  7' 0.25''              106.50      8' 10.5''
    ## 780      223.8          86.75  7' 2.75''              106.50      8' 10.5''
    ## 781      227.8          83.25 6' 11.25''              101.50       8' 5.5''
    ## 782      174.4          81.75  6' 9.75''              102.50       8' 6.5''
    ## 783      201.8          80.00     6' 8''              100.00         8' 4''
    ## 784      202.4          77.25  6' 5.25''               96.00         8' 0''
    ## 785      214.6          81.50   6' 9.5''              102.00         8' 6''
    ## 786      241.2          81.25  6' 9.25''               99.50       8' 3.5''
    ## 787      188.2          77.00     6' 5''               94.50      7' 10.5''
    ## 788      278.6          90.25  7' 6.25''              109.50       9' 1.5''
    ## 789      192.0          78.50   6' 6.5''               98.00         8' 2''
    ## 790      264.2          89.75  7' 5.75''              108.00         9' 0''
    ## 791      239.0          83.50  6' 11.5''              103.50       8' 7.5''
    ## 792      217.4          86.25  7' 2.25''              106.50      8' 10.5''
    ## 793      235.6          85.25  7' 1.25''              105.00         8' 9''
    ## 794      206.6          84.00     7' 0''              102.50       8' 6.5''
    ## 795      186.8          77.50   6' 5.5''               94.50      7' 10.5''
    ## 796      218.0          80.50   6' 8.5''              102.00         8' 6''
    ## 797      229.8          87.00     7' 3''              108.50       9' 0.5''
    ## 798      212.0          80.50   6' 8.5''               98.00         8' 2''
    ## 799      223.8          83.25 6' 11.25''               99.00         8' 3''
    ## 800      212.0          79.00     6' 7''               96.00         8' 0''
    ## 801      252.0          86.25  7' 2.25''              107.00        8' 11''
    ## 802      233.8          85.75  7' 1.75''              107.00        8' 11''
    ## 803      251.2          85.50   7' 1.5''              105.00         8' 9''
    ## 804      215.0          83.00    6' 11''              104.00         8' 8''
    ## 805      232.8          84.00     7' 0''              104.50       8' 8.5''
    ## 806      179.2          83.00    6' 11''              102.00         8' 6''
    ## 807      199.4          78.75  6' 6.75''               98.50       8' 2.5''
    ## 808      249.8          87.00     7' 3''              108.00         9' 0''
    ## 809      188.8          79.75  6' 7.75''               95.50      7' 11.5''
    ## 810      205.8          76.00     6' 4''               94.50      7' 10.5''
    ## 811      198.4          77.50   6' 5.5''               96.00         8' 0''
    ## 812      255.0          86.50   7' 2.5''              110.00         9' 2''
    ## 813      216.2          82.75 6' 10.75''              103.00         8' 7''
    ## 814      233.4          81.00     6' 9''              101.00         8' 5''
    ## 815      218.7          85.25  7' 1.25''              105.00         8' 9''
    ## 816      216.4          82.00    6' 10''              100.00         8' 4''
    ## 817      232.8          86.25  7' 2.25''              106.50      8' 10.5''
    ## 818      194.0          78.75  6' 6.75''               99.00         8' 3''
    ## 819      234.0          88.00     7' 4''              107.00        8' 11''
    ## 820      240.8          88.75  7' 4.75''              106.50      8' 10.5''
    ## 821      252.4          84.75  7' 0.75''              105.50       8' 9.5''
    ## 822      202.8          79.25  6' 7.25''               96.50       8' 0.5''
    ## 823      244.2          87.25  7' 3.25''              106.00        8' 10''
    ## 824      196.6          79.25  6' 7.25''              100.50       8' 4.5''
    ## 825      240.8          82.75 6' 10.75''              104.00         8' 8''
    ## 826      241.2          88.00     7' 4''              108.00         9' 0''
    ## 827      268.2          85.25  7' 1.25''              105.50       8' 9.5''
    ## 828      212.8          78.25  6' 6.25''              101.50       8' 5.5''
    ## 829      177.0          78.25  6' 6.25''               97.50       8' 1.5''
    ## 830      192.8          75.00     6' 3''               92.50       7' 8.5''
    ## 831      179.8          79.25  6' 7.25''               96.50       8' 0.5''
    ## 832      206.0          81.50   6' 9.5''              102.50       8' 6.5''
    ## 833      203.2          81.00     6' 9''              101.00         8' 5''
    ## 834      247.4          84.00     7' 0''              104.50       8' 8.5''
    ## 835      254.5          88.50   7' 4.5''              109.50       9' 1.5''
    ## 836      197.4          78.00     6' 6''              100.50       8' 4.5''
    ## 837      189.2          79.00     6' 7''              100.00         8' 4''
    ## 838      199.8          80.75  6' 8.75''              102.00         8' 6''
    ## 839      187.0          77.50   6' 5.5''               97.50       8' 1.5''
    ## 840      203.6          80.00     6' 8''              100.50       8' 4.5''
    ## 841      188.4          76.50   6' 4.5''               94.50      7' 10.5''
    ## 842      241.2          85.50   7' 1.5''              107.00        8' 11''
    ## 843      184.4          79.25  6' 7.25''              101.00         8' 5''
    ## 844      210.4          84.25  7' 0.25''              105.00         8' 9''
    ## 845      208.6          85.75  7' 1.75''              106.00        8' 10''
    ## 846      197.4          83.25 6' 11.25''              103.50       8' 7.5''
    ## 847      241.8          85.50   7' 1.5''              108.50       9' 0.5''
    ## 848      238.8          88.00     7' 4''              109.00         9' 1''
    ## 849      200.8          83.50  6' 11.5''              104.50       8' 8.5''
    ## 850      202.6          81.25  6' 9.25''              103.50       8' 7.5''
    ## 851      237.6          92.50   7' 8.5''              115.00         9' 7''
    ## 852      189.0          81.50   6' 9.5''              102.00         8' 6''
    ## 853      199.4          79.00     6' 7''              101.00         8' 5''
    ## 854      226.0          81.00     6' 9''              103.00         8' 7''
    ## 855      262.8          86.00     7' 2''              110.00         9' 2''
    ## 856      232.0          86.00     7' 2''              109.00         9' 1''
    ## 857      180.4          78.25  6' 6.25''               98.00         8' 2''
    ## 858      241.6          87.00     7' 3''              106.50      8' 10.5''
    ## 859      170.8          70.75 5' 10.75''               89.50       7' 5.5''
    ## 860      197.2          79.25  6' 7.25''              102.50       8' 6.5''
    ## 861      209.2          86.25  7' 2.25''              104.50       8' 8.5''
    ## 862      235.8          88.00     7' 4''              106.50      8' 10.5''
    ## 863      191.4          75.25  6' 3.25''               96.50       8' 0.5''
    ## 864      197.0          78.25  6' 6.25''               96.50       8' 0.5''
    ## 865      189.2          79.75  6' 7.75''              100.50       8' 4.5''
    ## 866      236.4          86.50   7' 2.5''              106.50      8' 10.5''
    ## 867      221.8          83.00    6' 11''              104.50       8' 8.5''
    ## 868      239.8          82.50  6' 10.5''              108.00         9' 0''
    ## 869      230.2          85.00     7' 1''              108.00         9' 0''
    ## 870      213.2          81.25  6' 9.25''              100.50       8' 4.5''
    ## 871      234.0          81.75  6' 9.75''              108.00         9' 0''
    ## 872      201.0          82.25 6' 10.25''               98.00         8' 2''
    ## 873      207.2          86.50   7' 2.5''              109.00         9' 1''
    ## 874      238.2          83.00    6' 11''              108.00         9' 0''
    ## 875      197.6          85.50   7' 1.5''              105.50       8' 9.5''
    ## 876      177.0          74.25  6' 2.25''               92.00         7' 8''
    ## 877      210.8          81.25  6' 9.25''              101.50       8' 5.5''
    ## 878      205.8          83.00    6' 11''              104.50       8' 8.5''
    ## 879      164.8          79.75  6' 7.75''               98.00         8' 2''
    ## 880      180.6          75.00     6' 3''               93.00         7' 9''
    ## 881      198.2          83.50  6' 11.5''              105.50       8' 9.5''
    ## 882      220.6          85.00     7' 1''              106.00        8' 10''
    ## 883      219.8          82.00    6' 10''              104.00         8' 8''
    ## 884      231.8          85.00     7' 1''               99.00         8' 3''
    ## 885      221.8          86.00     7' 2''              110.50       9' 2.5''
    ## 886      230.0          82.75 6' 10.75''              106.00        8' 10''
    ## 887      208.8          82.00    6' 10''              102.00         8' 6''
    ## 888      205.2          84.00     7' 0''              104.50       8' 8.5''
    ## 889      254.1          88.00     7' 4''              108.50       9' 0.5''
    ## 890      252.4          84.75  7' 0.75''              107.00        8' 11''
    ## 891      208.8          85.00     7' 1''              107.00        8' 11''
    ## 892      231.2          85.50   7' 1.5''              109.00         9' 1''
    ## 893      201.9          80.25  6' 8.25''              100.00         8' 4''
    ## 894      184.4          80.75  6' 8.75''              100.00         8' 4''
    ## 895      192.8          79.00     6' 7''               97.50       8' 1.5''
    ## 896      186.0          78.50   6' 6.5''               99.00         8' 3''
    ## 897      186.3          80.00     6' 8''               98.00         8' 2''
    ## 898      191.6          74.00     6' 2''               94.50      7' 10.5''
    ## 899      196.0          86.00     7' 2''              106.50      8' 10.5''
    ## 900      209.4          82.75 6' 10.75''              101.50       8' 5.5''
    ## 901      218.5          83.25 6' 11.25''              103.50       8' 7.5''
    ## 902      181.5          79.25  6' 7.25''               98.00         8' 2''
    ## 903      196.3          81.50   6' 9.5''              103.00         8' 7''
    ## 904      218.1          81.50   6' 9.5''              104.00         8' 8''
    ## 905      220.1          83.75 6' 11.75''              105.00         8' 9''
    ## 906      228.8          81.00     6' 9''              101.00         8' 5''
    ## 907      215.0          78.00     6' 6''              100.00         8' 4''
    ## 908      208.2          80.50   6' 8.5''              103.00         8' 7''
    ## 909      218.4          84.50   7' 0.5''              105.00         8' 9''
    ## 910      198.0          79.25  6' 7.25''               96.50       8' 0.5''
    ## 911      252.0          87.50   7' 3.5''              109.50       9' 1.5''
    ## 912      180.9          80.25  6' 8.25''              100.00         8' 4''
    ## 913      192.4          81.00     6' 9''              102.00         8' 6''
    ## 914      227.6          86.25  7' 2.25''              106.50      8' 10.5''
    ## 915      195.6          83.25 6' 11.25''              102.00         8' 6''
    ## 916      218.0          81.25  6' 9.25''              103.00         8' 7''
    ## 917      179.4          84.50   7' 0.5''              104.00         8' 8''
    ## 918      175.2          75.25  6' 3.25''               93.00         7' 9''
    ## 919      256.8          86.25  7' 2.25''              105.00         8' 9''
    ## 920      226.0          83.00    6' 11''              101.00         8' 5''
    ## 921      185.4          80.00     6' 8''               98.50       8' 2.5''
    ## 922      234.4          84.50   7' 0.5''              105.00         8' 9''
    ## 923      249.7          84.00     7' 0''              105.50       8' 9.5''
    ## 924      211.4          82.00    6' 10''              100.50       8' 4.5''
    ## 925      238.7          85.75  7' 1.75''              106.00        8' 10''
    ## 926      227.2          81.25  6' 9.25''               99.00         8' 3''
    ## 927      160.0          75.50   6' 3.5''               95.00        7' 11''
    ## 928      206.9          79.75  6' 7.75''              102.00         8' 6''
    ## 929      262.8          85.25  7' 1.25''              103.50       8' 7.5''
    ## 930      187.0          76.00     6' 4''               96.00         8' 0''
    ## 931      247.1          88.25  7' 4.25''              108.00         9' 0''
    ## 932      220.2          82.25 6' 10.25''              104.00         8' 8''
    ## 933      201.0          81.75  6' 9.75''              100.50       8' 4.5''
    ## 934      183.0          77.50   6' 5.5''               99.00         8' 3''
    ## 935      246.8          85.75  7' 1.75''              103.50       8' 7.5''
    ## 936      230.6          83.75 6' 11.75''              101.00         8' 5''
    ## 937      228.0          87.75  7' 3.75''              105.00         8' 9''
    ## 938      169.6          72.50   6' 0.5''               92.50       7' 8.5''
    ## 939      205.8          80.25  6' 8.25''              102.50       8' 6.5''
    ## 940      211.0          83.00    6' 11''              104.50       8' 8.5''
    ## 941      242.6          89.25  7' 5.25''              110.50       9' 2.5''
    ## 942      215.0          80.00     6' 8''               96.00         8' 0''
    ## 943      179.0          76.00     6' 4''               96.50       8' 0.5''
    ## 944      230.2          83.00    6' 11''              105.00         8' 9''
    ## 945      218.6          83.50  6' 11.5''              106.00        8' 10''
    ## 946      199.4          80.00     6' 8''              101.50       8' 5.5''
    ## 947      219.4          82.50  6' 10.5''              103.00         8' 7''
    ## 948      185.8          78.00     6' 6''              100.00         8' 4''
    ## 949      213.2          81.00     6' 9''              100.00         8' 4''
    ## 950      209.2          80.25  6' 8.25''              100.50       8' 4.5''
    ## 951      181.4          77.50   6' 5.5''               98.50       8' 2.5''
    ## 952      191.8          80.00     6' 8''               99.50       8' 3.5''
    ## 953      210.8          86.00     7' 2''              104.00         8' 8''
    ## 954      242.2          83.50  6' 11.5''              107.50      8' 11.5''
    ## 955      242.8          85.50   7' 1.5''              108.00         9' 0''
    ## 956      185.0          82.50  6' 10.5''              104.00         8' 8''
    ## 957      208.4          83.00    6' 11''              104.50       8' 8.5''
    ## 958      264.6          86.00     7' 2''              112.00         9' 4''
    ## 959      184.6          77.00     6' 5''               97.00         8' 1''
    ## 960      238.6          81.00     6' 9''              106.50      8' 10.5''
    ## 961      187.8          74.00     6' 2''               96.50       8' 0.5''
    ## 962      238.0          87.25  7' 3.25''              106.00        8' 10''
    ## 963      226.6          85.50   7' 1.5''              108.00         9' 0''
    ## 964      202.8          86.25  7' 2.25''              102.50       8' 6.5''
    ## 965      182.8          79.25  6' 7.25''               97.50       8' 1.5''
    ## 966      209.2          83.25 6' 11.25''              104.50       8' 8.5''
    ## 967      246.2          86.00     7' 2''              108.50       9' 0.5''
    ## 968      215.4          82.75 6' 10.75''              102.50       8' 6.5''
    ## 969      201.4          84.25  7' 0.25''              104.50       8' 8.5''
    ## 970      178.8          79.00     6' 7''               96.50       8' 0.5''
    ## 971      190.2          80.25  6' 8.25''               98.50       8' 2.5''
    ## 972      167.2          74.00     6' 2''               93.00         7' 9''
    ## 973      177.4          79.75  6' 7.75''               97.50       8' 1.5''
    ## 974      195.8          82.00    6' 10''              100.50       8' 4.5''
    ## 975      198.6          79.00     6' 7''              101.00         8' 5''
    ## 976      208.0          86.75  7' 2.75''              108.00         9' 0''
    ## 977      208.8          81.50   6' 9.5''              100.50       8' 4.5''
    ## 978      261.4          85.75  7' 1.75''              106.50      8' 10.5''
    ## 979      181.4          79.50   6' 7.5''              101.50       8' 5.5''
    ## 980      212.4          81.75  6' 9.75''               97.50       8' 1.5''
    ## 981      201.8          83.25 6' 11.25''              100.00         8' 4''
    ## 982      206.6          81.25  6' 9.25''              101.00         8' 5''
    ## 983      229.2          85.50   7' 1.5''              105.00         8' 9''
    ## 984      213.6          82.00    6' 10''              102.00         8' 6''
    ## 985      224.4          85.75  7' 1.75''              103.50       8' 7.5''
    ## 986      223.0          82.50  6' 10.5''               98.00         8' 2''
    ## 987      233.0          84.25  7' 0.25''              105.00         8' 9''
    ## 988      194.0          78.25  6' 6.25''               98.50       8' 2.5''
    ## 989      218.6          88.50   7' 4.5''              107.50      8' 11.5''
    ## 990      217.8          82.25 6' 10.25''              102.00         8' 6''
    ## 991      177.2          74.50   6' 2.5''               88.50       7' 4.5''
    ## 992      212.6          83.75 6' 11.75''              105.50       8' 9.5''
    ## 993      204.6          79.50   6' 7.5''              101.00         8' 5''
    ## 994      197.0          81.00     6' 9''              103.50       8' 7.5''
    ## 995      203.6          80.50   6' 8.5''               99.50       8' 3.5''
    ## 996      245.0          87.00     7' 3''              107.50      8' 11.5''
    ## 997      193.4          82.75 6' 10.75''              103.50       8' 7.5''
    ## 998      193.8          77.50   6' 5.5''               93.00         7' 9''
    ## 999      208.6          84.50   7' 0.5''              106.50      8' 10.5''
    ## 1000     243.6          87.75  7' 3.75''              107.00        8' 11''
    ## 1001     208.6          81.25  6' 9.25''              102.00         8' 6''
    ## 1002     211.0          87.00     7' 3''              107.50      8' 11.5''
    ## 1003     216.0          87.00     7' 3''              110.50       9' 2.5''
    ## 1004     180.8          82.00    6' 10''              100.50       8' 4.5''
    ## 1005     182.2          77.75  6' 5.75''               97.50       8' 1.5''
    ## 1006     230.8          82.00    6' 10''              103.00         8' 7''
    ## 1007     245.2          86.75  7' 2.75''              108.50       9' 0.5''
    ## 1008     163.8          78.25  6' 6.25''               97.00         8' 1''
    ## 1009     183.8          80.00     6' 8''               99.50       8' 3.5''
    ## 1010     220.2          83.50  6' 11.5''              102.00         8' 6''
    ## 1011     218.2          91.75  7' 7.75''              112.50       9' 4.5''
    ## 1012     199.6          84.00     7' 0''              101.50       8' 5.5''
    ## 1013     226.6          87.25  7' 3.25''              107.50      8' 11.5''
    ## 1014     254.4          86.75  7' 2.75''              108.50       9' 0.5''
    ## 1015     246.6          87.50   7' 3.5''              107.00        8' 11''
    ## 1016     191.8          74.00     6' 2''               92.50       7' 8.5''
    ## 1017     149.2          74.00     6' 2''               88.50       7' 4.5''
    ## 1018     214.4          83.50  6' 11.5''              104.00         8' 8''
    ## 1019     210.4          82.75 6' 10.75''              102.00         8' 6''
    ## 1020     210.0          80.75  6' 8.75''              100.00         8' 4''
    ## 1021     217.8          80.25  6' 8.25''              102.00         8' 6''
    ## 1022     242.8          84.25  7' 0.25''              102.50       8' 6.5''
    ## 1023     234.0          87.25  7' 3.25''              108.50       9' 0.5''
    ## 1024     242.6          86.75  7' 2.75''              108.00         9' 0''
    ## 1025     222.6          80.75  6' 8.75''               99.00         8' 3''
    ## 1026     233.6          89.25  7' 5.25''              109.50       9' 1.5''
    ## 1027     192.4          81.25  6' 9.25''               99.50       8' 3.5''
    ## 1028     252.2          90.25  7' 6.25''              110.50       9' 2.5''
    ## 1029     213.2          82.00    6' 10''              105.00         8' 9''
    ## 1030     221.6          82.00    6' 10''              103.50       8' 7.5''
    ## 1031     193.4          82.25 6' 10.25''              104.50       8' 8.5''
    ## 1032     223.6          83.75 6' 11.75''              104.50       8' 8.5''
    ## 1033     218.6          82.00    6' 10''              104.50       8' 8.5''
    ## 1034     248.8          89.00     7' 5''              112.50       9' 4.5''
    ## 1035     222.4          81.00     6' 9''               99.50       8' 3.5''
    ## 1036     220.2          78.00     6' 6''              100.50       8' 4.5''
    ## 1037     247.8          90.00     7' 6''              112.50       9' 4.5''
    ## 1038     225.2          83.25 6' 11.25''              106.50      8' 10.5''
    ## 1039     197.4          83.25 6' 11.25''              101.50       8' 5.5''
    ## 1040     182.6          77.25  6' 5.25''               99.50       8' 3.5''
    ## 1041     205.2          81.00     6' 9''              100.00         8' 4''
    ## 1042     200.6          83.00    6' 11''              101.00         8' 5''
    ## 1043     185.2          77.50   6' 5.5''               95.00        7' 11''
    ## 1044     184.4          80.75  6' 8.75''              103.00         8' 7''
    ## 1045     232.0          87.25  7' 3.25''              109.50       9' 1.5''
    ## 1046     209.2          80.25  6' 8.25''              101.00         8' 5''
    ## 1047     254.2          87.25  7' 3.25''              104.50       8' 8.5''
    ## 1048     233.4          84.50   7' 0.5''              105.00         8' 9''
    ## 1049     193.4          85.00     7' 1''              105.50       8' 9.5''
    ## 1050     201.6          79.50   6' 7.5''               98.00         8' 2''
    ## 1051     219.4          87.25  7' 3.25''              107.00        8' 11''
    ## 1052     200.8          83.00    6' 11''              104.50       8' 8.5''
    ## 1053     202.4          80.00     6' 8''              102.00         8' 6''
    ## 1054     222.6          84.25  7' 0.25''              107.50      8' 11.5''
    ## 1055     222.2          83.00    6' 11''              107.00        8' 11''
    ## 1056     215.0          84.00     7' 0''              107.50      8' 11.5''
    ## 1057     188.8          75.25  6' 3.25''               94.00        7' 10''
    ## 1058     276.6          85.00     7' 1''              106.00        8' 10''
    ## 1059     211.4          82.00    6' 10''               97.00         8' 1''
    ## 1060     175.2          76.00     6' 4''               96.50       8' 0.5''
    ## 1061     219.6          77.00     6' 5''              100.00         8' 4''
    ## 1062     241.4          81.75  6' 9.75''              102.00         8' 6''
    ## 1063     238.6          85.25  7' 1.25''              106.50      8' 10.5''
    ## 1064     229.0          87.00     7' 3''              111.50       9' 3.5''
    ## 1065     219.6          85.50   7' 1.5''              109.00         9' 1''
    ## 1066     206.2          84.00     7' 0''              102.00         8' 6''
    ## 1067     189.6          84.75  7' 0.75''              106.00        8' 10''
    ## 1068     166.2          78.00     6' 6''               99.00         8' 3''
    ## 1069     211.6          82.00    6' 10''              103.00         8' 7''
    ## 1070     195.2          74.00     6' 2''               95.50      7' 11.5''
    ## 1071     231.2          84.00     7' 0''              108.00         9' 0''
    ## 1072     188.6          74.50   6' 2.5''               95.00        7' 11''
    ## 1073     189.8          79.50   6' 7.5''              101.50       8' 5.5''
    ## 1074     190.2          79.25  6' 7.25''               99.00         8' 3''
    ## 1075     248.4          85.00     7' 1''              108.50       9' 0.5''
    ## 1076     217.4          80.75  6' 8.75''               99.00         8' 3''
    ## 1077     198.0          79.25  6' 7.25''               97.00         8' 1''
    ## 1078     194.8          86.25  7' 2.25''              110.00         9' 2''
    ## 1079     273.8          91.00     7' 7''              112.50       9' 4.5''
    ## 1080     207.6          75.50   6' 3.5''               96.50       8' 0.5''
    ## 1081     223.8          87.25  7' 3.25''              106.50      8' 10.5''
    ## 1082     200.2          81.00     6' 9''              102.00         8' 6''
    ## 1083     202.0          82.25 6' 10.25''              103.50       8' 7.5''
    ## 1084     194.6          81.00     6' 9''               98.50       8' 2.5''
    ## 1085     208.0          82.25 6' 10.25''              105.00         8' 9''
    ## 1086     198.4          76.00     6' 4''               96.00         8' 0''
    ## 1087     198.8          80.25  6' 8.25''              100.00         8' 4''
    ## 1088     196.2          76.25  6' 4.25''               95.00        7' 11''
    ## 1089     200.6          78.00     6' 6''               97.50       8' 1.5''
    ## 1090     197.0          83.50  6' 11.5''              103.00         8' 7''
    ## 1091     191.0          80.25  6' 8.25''               97.50       8' 1.5''
    ## 1092     195.6          78.25  6' 6.25''               94.50      7' 10.5''
    ## 1093     199.6          81.25  6' 9.25''              102.50       8' 6.5''
    ## 1094     233.2          88.25  7' 4.25''              109.00         9' 1''
    ## 1095     198.2          85.75  7' 1.75''              105.00         8' 9''
    ## 1096     186.4          78.25  6' 6.25''               96.00         8' 0''
    ## 1097     206.4          79.75  6' 7.75''              102.00         8' 6''
    ## 1098     179.0          77.50   6' 5.5''               98.00         8' 2''
    ## 1099     211.6          87.50   7' 3.5''              107.00        8' 11''
    ## 1100     187.0          79.50   6' 7.5''               97.00         8' 1''
    ## 1101     194.0          79.50   6' 7.5''              101.50       8' 5.5''
    ## 1102     216.6          80.75  6' 8.75''              103.00         8' 7''
    ## 1103     218.6          83.50  6' 11.5''              101.50       8' 5.5''
    ## 1104     246.0          84.00     7' 0''              106.50      8' 10.5''
    ## 1105     195.8          82.00    6' 10''              101.50       8' 5.5''
    ## 1106     188.6          82.25 6' 10.25''              102.50       8' 6.5''
    ## 1107     246.2          84.50   7' 0.5''              107.50      8' 11.5''
    ## 1108     250.4          86.00     7' 2''              110.50       9' 2.5''
    ## 1109     193.2          80.50   6' 8.5''               99.50       8' 3.5''
    ## 1110     219.6          84.50   7' 0.5''              108.00         9' 0''
    ## 1111     207.2          84.75  7' 0.75''               99.50       8' 3.5''
    ## 1112     211.6          76.75  6' 4.75''              100.00         8' 4''
    ## 1113     189.2          77.50   6' 5.5''               98.50       8' 2.5''
    ## 1114     210.8          84.00     7' 0''              102.00         8' 6''
    ## 1115     211.2          82.75 6' 10.75''              103.50       8' 7.5''
    ## 1116     236.0          84.25  7' 0.25''              109.00         9' 1''
    ## 1117     222.4          86.00     7' 2''              108.00         9' 0''
    ## 1118     188.6          78.75  6' 6.75''              100.00         8' 4''
    ## 1119     183.2          81.25  6' 9.25''               99.50       8' 3.5''
    ## 1120     198.6          81.75  6' 9.75''              100.00         8' 4''
    ## 1121     215.4          88.75  7' 4.75''              109.00         9' 1''
    ## 1122     253.8          86.00     7' 2''              107.50      8' 11.5''
    ## 1123     198.8          82.50  6' 10.5''              101.00         8' 5''
    ## 1124     204.2          80.75  6' 8.75''               98.00         8' 2''
    ## 1125     198.0          78.75  6' 6.75''               98.00         8' 2''
    ## 1126     241.4          84.00     7' 0''              108.00         9' 0''
    ## 1127     196.2          82.25 6' 10.25''               99.50       8' 3.5''
    ## 1128     223.0          86.50   7' 2.5''              107.00        8' 11''
    ## 1129     249.4          89.00     7' 5''              111.00         9' 3''
    ## 1130     205.0          82.25 6' 10.25''              103.50       8' 7.5''
    ## 1131     200.4          79.25  6' 7.25''              103.50       8' 7.5''
    ## 1132     239.0          87.50   7' 3.5''              109.50       9' 1.5''
    ## 1133     208.4          84.00     7' 0''              107.00        8' 11''
    ## 1134     179.0          75.25  6' 3.25''               95.00        7' 11''
    ## 1135     200.0          82.00    6' 10''              103.00         8' 7''
    ## 1136     181.2          79.00     6' 7''               98.00         8' 2''
    ## 1137     220.8          81.25  6' 9.25''              102.00         8' 6''
    ## 1138     203.2          84.00     7' 0''              104.00         8' 8''
    ## 1139     237.2          88.75  7' 4.75''              113.00         9' 5''
    ## 1140     207.2          80.25  6' 8.25''              102.00         8' 6''
    ## 1141     216.6          86.50   7' 2.5''              110.00         9' 2''
    ## 1142     245.4          84.50   7' 0.5''              106.50      8' 10.5''
    ## 1143     192.4          80.75  6' 8.75''              101.00         8' 5''
    ## 1144     222.2          80.50   6' 8.5''               99.50       8' 3.5''
    ## 1145     178.6          75.25  6' 3.25''               95.50      7' 11.5''
    ## 1146     199.4          78.00     6' 6''               94.50      7' 10.5''
    ## 1147     288.8          98.25  8' 2.25''              122.50      10' 2.5''
    ## 1148     237.0          87.25  7' 3.25''              110.00         9' 2''
    ## 1149     237.6          86.50  7' 2.25''              110.00         9' 2''
    ## 1150     209.6          79.75  6' 7.75''              100.50       8' 4.5''
    ## 1151     167.8          76.50   6' 4.5''               95.50      7' 11.5''
    ## 1152     180.4          77.50   6' 5.5''               97.00         8' 1''
    ## 1153     169.2          77.50   6' 5.5''               94.00        7' 10''
    ## 1154     218.6          87.50   7' 3.5''              110.50       9' 2.5''
    ## 1155     232.8          85.75  7' 1.75''              107.00        8' 11''
    ## 1156     212.6          84.75  7' 0.75''              105.50       8' 9.5''
    ## 1157     235.4          85.25  7' 1.25''              103.00         8' 7''
    ## 1158     194.4          76.00     6' 4''               98.00         8' 2''
    ## 1159     205.2          82.00    6' 10''              103.00         8' 7''
    ## 1160     256.2          87.00     7' 3''              109.50       9' 1.5''
    ## 1161     195.2          84.25  7' 0.25''              104.50       8' 8.5''
    ## 1162     233.0          86.25  7' 2.25''              107.50      8' 11.5''
    ## 1163     185.2          80.50   6' 8.5''               98.50       8' 2.5''
    ## 1164     224.2          85.25  7' 1.25''              104.50       8' 8.5''
    ## 1165     204.6          79.75  6' 7.75''              101.00         8' 5''
    ## 1166     192.2          82.25 6' 10.25''              102.50       8' 6.5''
    ## 1167     194.8          81.50   6' 9.5''              103.00         8' 7''
    ## 1168     191.6          84.25  7' 0.25''              105.50       8' 9.5''
    ## 1169     206.4          78.00     6' 6''              101.00         8' 5''
    ## 1170     202.4          79.25  6' 7.25''               98.00         8' 2''
    ## 1171     209.6          85.75  7' 1.75''              106.50      8' 10.5''
    ## 1172     205.6          82.75 6' 10.75''              100.50       8' 4.5''
    ## 1173     254.4          87.75 6' 11.75''              103.50       8' 7.5''
    ## 1174     250.8          84.50   7' 0.5''              106.00        8' 10''
    ## 1175     180.0          75.50   6' 3.5''               96.50       8' 0.5''
    ## 1176     190.8          78.75  6' 6.75''               99.50       8' 3.5''
    ## 1177     212.6          81.00     6' 9''              103.00         8' 7''
    ## 1178     225.8          88.25  7' 4.25''              111.50       9' 3.5''
    ## 1179     255.8          87.25  7' 3.25''              109.00         9' 1''
    ## 1180     214.0          85.00     7' 1''              106.00        8' 10''
    ## 1181     227.2          82.50  6' 10.5''              107.00        8' 11''
    ## 1182     240.6          81.75  6' 9.75''              102.50       8' 6.5''
    ## 1183     196.8          84.25  7' 0.25''              103.50       8' 7.5''
    ## 1184     227.2          85.25  7' 1.25''              106.00        8' 10''
    ## 1185     172.4          74.25  6' 2.25''               93.50       7' 9.5''
    ## 1186     206.6          81.00     6' 9''              100.00         8' 4''
    ## 1187     208.8          82.75 6' 10.75''              103.00         8' 7''
    ## 1188     240.2          81.75  6' 9.75''              104.50       8' 8.5''
    ## 1189     195.8          82.00    6' 10''              104.50       8' 8.5''
    ## 1190     200.8          80.00   6'8.00''              100.00       8'4.00''
    ## 1191     259.5          91.25   7'7.25''              109.00       9'1.00''
    ## 1192     212.8          85.25   7'1.25''              105.50       8'9.50''
    ## 1193     226.4          83.50  6'11.50''              106.00      8'10.00''
    ## 1194     222.1          87.75   7'3.75''              107.00      8'11.00''
    ## 1195     185.2          75.00   6'3.00''               95.00      7'11.00''
    ## 1196     222.2          87.50   7'3.50''              108.00       9'0.00''
    ## 1197     198.6          79.25   6'7.25''              100.50       8'4.50''
    ## 1198     203.0          79.50   6'7.50''              102.50       8'6.50''
    ## 1199     214.0          81.75   6'9.75''              101.50       8'5.50''
    ## 1200     193.0          80.00   6'8.00''               99.50       8'3.50''
    ## 1201     196.6          83.00  6'11.00''              104.50       8'8.50''
    ## 1202     193.4          79.00   6'7.00''               97.50       8'1.50''
    ## 1203     170.8          72.25   6'0.25''               91.50       7'7.50''
    ## 1204     207.2          79.00   6'7.00''              102.50       8'6.50''
    ## 1205     235.0          86.00   7'2.00''              109.50       9'1.50''
    ## 1206     196.0          83.00  6'11.00''              102.00       8'6.00''
    ## 1207     233.5          84.75   7'0.75''              102.50       8'6.50''
    ## 1208     213.2          79.25   6'7.25''              102.00       8'6.00''
    ## 1209     204.0          78.00   6'6.00''               99.00       8'3.00''
    ## 1210     246.8          86.00   7'2.00''              105.50       8'9.50''
    ## 1211     223.0          82.25  6'10.25''              104.00       8'8.00''
    ## 1212     249.6          84.25   7'0.25''              106.50      8'10.50''
    ## 1213     185.8          80.25   6'8.25''               99.00       8'3.00''
    ## 1214     219.2          86.00   7'2.00''              109.50       9'1.50''
    ## 1215     241.6          86.25   7'2.25''              110.00       9'2.00''
    ## 1216     178.4          77.25   6'5.25''               96.50       8'0.50''
    ## 1217     201.6          79.00   6'7.00''               99.50       8'3.50''
    ## 1218     228.0          81.00   6'9.00''              101.00       8'5.00''
    ## 1219     170.0          73.75   6'1.75''               96.00       8'0.00''
    ## 1220     221.6          80.00   6'8.00''              106.00      8'10.00''
    ## 1221     266.5          86.00   7'2.00''              106.00      8'10.00''
    ## 1222     252.6          87.50   7'3.50''              111.50       9'3.50''
    ## 1223     205.0          84.25   7'0.25''              105.50       8'9.50''
    ## 1224     196.0          78.00   6'6.00''               97.00       8'1.00''
    ## 1225     230.2          86.00   7'2.00''              105.50       8'9.50''
    ##      verticalLeapStandingInches verticalLeapMaxInches timeLaneAgility
    ## 1                         25.50                  29.0           11.83
    ## 2                         28.00                  29.5           12.30
    ## 3                         27.00                  31.0           13.04
    ## 4                         26.00                  29.5           11.53
    ## 5                         36.00                  42.5           10.48
    ## 6                         35.00                  38.0           11.40
    ## 7                         26.50                  33.5           10.98
    ## 8                         34.00                  38.0           11.55
    ## 9                         33.50                  37.5           12.12
    ## 10                        27.50                  32.0           10.59
    ## 11                        21.00                  28.5           11.75
    ## 12                        27.00                  30.0           11.60
    ## 13                        35.00                  38.5           11.12
    ## 14                        29.50                  33.5           12.00
    ## 15                        30.00                  34.0           10.73
    ## 16                        33.00                  37.5           10.55
    ## 17                        29.50                  34.5           12.58
    ## 18                        29.50                  33.5           11.70
    ## 19                        31.50                  35.0           11.50
    ## 20                        29.00                  33.0           11.79
    ## 21                        29.50                  34.5           11.92
    ## 22                        29.50                  38.5           11.12
    ## 23                        29.00                  36.0           11.23
    ## 24                        26.00                  32.0           11.48
    ## 25                        27.50                  33.0           11.32
    ## 26                        27.00                  33.5           11.90
    ## 27                        28.50                  30.0           12.44
    ## 28                        27.50                  30.5           11.51
    ## 29                        31.00                  34.5           10.85
    ## 30                        31.00                  38.5           10.99
    ## 31                        30.50                  33.5           12.12
    ## 32                        33.00                  35.0           11.52
    ## 33                        28.00                  34.5           10.83
    ## 34                        25.00                  29.0           11.85
    ## 35                        30.50                  34.0           11.00
    ## 36                        24.50                  27.5           13.16
    ## 37                        28.00                  35.0           11.38
    ## 38                        26.00                  29.0           11.28
    ## 39                        28.50                  35.0           10.93
    ## 40                        33.50                  36.5           11.79
    ## 41                        27.50                  34.0           10.40
    ## 42                        28.50                  33.5           11.10
    ## 43                        27.00                  31.0           12.06
    ## 44                        27.50                  31.5           11.21
    ## 45                        31.50                  37.5           11.13
    ## 46                        30.00                  37.5           12.54
    ## 47                        26.00                  31.0           11.62
    ## 48                        33.50                  43.0           10.66
    ## 49                        38.50                  42.5           10.98
    ## 50                        24.50                  28.0           12.72
    ## 51                        23.50                  25.5           12.14
    ## 52                        33.00                  21.0           11.32
    ## 53                        24.50                  30.0           12.45
    ## 54                        27.00                  29.0           11.98
    ## 55                        26.50                  31.0           12.08
    ## 56                        28.50                  35.5           10.75
    ## 57                        28.00                  30.5           12.38
    ## 58                        28.50                  31.0           11.90
    ## 59                        31.50                  36.0           11.18
    ## 60                        30.00                  37.0           10.91
    ## 61                        29.50                  33.0           10.95
    ## 62                        35.00                  41.5           11.26
    ## 63                        31.50                  38.0           10.59
    ## 64                        26.50                  31.0           11.84
    ## 65                        28.50                  34.5           11.96
    ## 66                        27.00                  30.0           11.11
    ## 67                        26.00                  32.5            9.65
    ## 68                        30.00                  33.0           11.59
    ## 69                        32.00                  37.0           11.81
    ## 70                        31.00                  39.5           11.23
    ## 71                        34.50                  39.0           11.00
    ## 72                        31.00                  33.5           12.13
    ## 73                        23.50                  27.5           12.50
    ## 74                        31.00                  34.0           11.47
    ## 75                        31.50                  34.5           11.40
    ## 76                        23.00                  29.0           11.65
    ## 77                        25.50                  28.5           12.10
    ## 78                        31.00                  35.5           10.77
    ## 79                        28.00                  31.0           12.25
    ## 80                        25.50                  28.5           12.19
    ## 81                        21.50                  25.5           12.37
    ## 82                        27.00                  30.0           12.47
    ## 83                        34.50                  37.5           10.08
    ## 84                        31.50                  33.0           12.56
    ## 85                        29.00                  32.0           13.05
    ## 86                        32.00                  33.0           11.46
    ## 87                        34.50                  36.5           11.70
    ## 88                        26.50                  33.0           11.51
    ## 89                        34.50                  40.0           10.88
    ## 90                        28.50                  35.0           11.70
    ## 91                        39.50                  45.5           11.33
    ## 92                        26.50                  33.0           11.77
    ## 93                        35.50                  37.5           11.57
    ## 94                        27.00                  31.5           12.25
    ## 95                        27.00                  28.5           12.87
    ## 96                        33.00                  40.5           11.38
    ## 97                        30.50                  38.5           11.99
    ## 98                        29.00                  32.5           11.25
    ## 99                        26.00                  31.0           11.42
    ## 100                       33.00                  38.5           11.19
    ## 101                       34.50                  41.0           10.97
    ## 102                       23.50                  30.0           12.41
    ## 103                       32.50                  35.0           11.27
    ## 104                       32.50                  36.5           12.05
    ## 105                       30.50                  34.5           11.38
    ## 106                       36.00                  40.0           11.06
    ## 107                       33.00                  36.5           11.53
    ## 108                       24.00                  30.0           11.65
    ## 109                       29.00                  41.0           10.57
    ## 110                       33.00                  43.0           11.64
    ## 111                       28.50                  34.5           11.22
    ## 112                       26.50                  29.5           13.42
    ## 113                       23.00                  26.0           12.00
    ## 114                       27.00                  32.0           11.93
    ## 115                       26.00                  31.0           11.50
    ## 116                       32.00                  37.0           10.77
    ## 117                       32.50                  39.5           11.75
    ## 118                       30.50                  35.5           10.97
    ## 119                       29.00                  34.0           11.47
    ## 120                       28.50                  33.0           12.05
    ## 121                       27.00                  30.5           11.75
    ## 122                       23.50                  29.5           13.28
    ## 123                       27.50                  34.0           11.69
    ## 124                       28.50                  35.0           11.30
    ## 125                       30.00                  35.5           11.29
    ## 126                       33.50                  37.5           11.17
    ## 127                       28.00                  37.5           11.96
    ## 128                       28.00                  33.0           11.95
    ## 129                       29.00                  30.5           12.02
    ## 130                       32.50                  37.5           10.97
    ## 131                       27.50                  33.0           12.07
    ## 132                       24.50                  28.0           11.83
    ## 133                       23.00                  29.0           12.25
    ## 134                       27.50                  33.5           11.37
    ## 135                       30.00                  34.0           10.73
    ## 136                       24.00                  27.5           11.34
    ## 137                       29.50                  33.0           12.19
    ## 138                       35.50                  40.5           10.68
    ## 139                       25.50                  31.5           11.20
    ## 140                       28.00                  32.5           11.77
    ## 141                       27.50                  32.5           11.44
    ## 142                       26.50                  28.5           10.77
    ## 143                       24.00                  27.0           12.56
    ## 144                       30.00                  35.5           10.93
    ## 145                       26.00                  33.0           11.43
    ## 146                       28.50                  29.5           12.03
    ## 147                       24.50                  31.0           11.58
    ## 148                       27.00                  31.0           12.15
    ## 149                       31.50                  37.5           11.54
    ## 150                       29.00                  33.0           11.12
    ## 151                       28.00                  31.0           12.55
    ## 152                       26.00                  30.0           12.06
    ## 153                       24.50                  29.0           11.55
    ## 154                       28.00                  33.0           10.53
    ## 155                       28.50                  33.0           12.18
    ## 156                       32.00                  35.5           11.69
    ## 157                       26.00                  29.0           11.96
    ## 158                       26.00                  29.5           12.73
    ## 159                       29.00                  33.0           11.81
    ## 160                       26.00                  29.5           11.20
    ## 161                       29.50                  36.5           11.24
    ## 162                       27.50                  32.0           11.10
    ## 163                       33.50                  36.5           11.35
    ## 164                       32.00                  35.0           11.62
    ## 165                       28.50                  34.0           12.06
    ## 166                       28.50                  33.5           11.80
    ## 167                       33.00                  37.5           11.30
    ## 168                       29.50                  35.5           12.85
    ## 169                       31.50                  37.5           11.75
    ## 170                       26.00                  30.0           11.73
    ## 171                       29.00                  32.0           11.92
    ## 172                       30.50                  31.5           11.80
    ## 173                       33.50                  37.5           11.12
    ## 174                       22.50                  26.5           11.64
    ## 175                       28.00                  29.0           11.36
    ## 176                       27.50                  30.0           12.96
    ## 177                       27.50                  30.5           12.97
    ## 178                       27.00                  32.5           12.20
    ## 179                       27.50                  34.5           11.74
    ## 180                       32.00                  38.0           11.17
    ## 181                       24.00                  30.0           11.84
    ## 182                       31.00                  36.5           11.13
    ## 183                       24.50                  25.0           14.45
    ## 184                       25.50                  29.5           11.11
    ## 185                       21.00                  28.5           12.30
    ## 186                       25.50                  31.0           10.78
    ## 187                       32.50                  37.5           11.36
    ## 188                       26.00                  33.0           11.96
    ## 189                       26.00                  30.0           11.86
    ## 190                       26.00                  29.5           11.05
    ## 191                       30.50                  34.5           11.35
    ## 192                       22.50                  28.5           10.94
    ## 193                       27.50                  31.5           11.85
    ## 194                       28.50                  29.0           11.03
    ## 195                       32.50                  38.0           10.94
    ## 196                       24.50                  29.5           11.69
    ## 197                       32.00                  35.5           11.19
    ## 198                       27.50                  33.5           11.25
    ## 199                       26.00                  32.0           11.46
    ## 200                       28.00                  35.0           12.03
    ## 201                       31.00                  34.5           11.43
    ## 202                       26.00                  31.0           11.73
    ## 203                       28.50                  36.0           10.34
    ## 204                       33.50                  39.0           11.21
    ## 205                       33.50                  34.5           11.94
    ## 206                       30.00                  34.0           11.70
    ## 207                       31.00                  34.5           12.93
    ## 208                       26.00                  30.5           11.23
    ## 209                       30.50                  33.5           11.40
    ## 210                       27.00                  32.0           11.36
    ## 211                       34.50                  41.0           10.89
    ## 212                       33.50                  36.5           11.64
    ## 213                       29.50                  35.0           11.43
    ## 214                       27.00                  32.0           11.52
    ## 215                       30.50                  33.0           11.80
    ## 216                       29.00                  35.5           11.20
    ## 217                       28.50                  34.5           11.51
    ## 218                       29.00                  33.5           11.11
    ## 219                       28.00                  33.0           11.62
    ## 220                       25.00                  29.0           11.87
    ## 221                       26.00                  32.5           11.27
    ## 222                       30.50                  35.0           11.62
    ## 223                       23.50                  27.5           11.97
    ## 224                       28.50                  35.0           11.95
    ## 225                       37.50                  41.5           11.09
    ## 226                       28.00                  34.0           11.77
    ## 227                       29.50                  33.5           11.38
    ## 228                       30.00                  36.0           12.92
    ## 229                       32.50                  39.5           11.45
    ## 230                       30.50                  35.0           10.62
    ## 231                       28.50                  28.5           11.86
    ## 232                       30.00                  37.5           11.18
    ## 233                       31.50                  35.0           10.72
    ## 234                       29.50                  32.0           11.85
    ## 235                       29.50                  37.5           11.50
    ## 236                       29.00                  33.5           10.98
    ## 237                       33.00                  37.5           11.54
    ## 238                       33.00                  36.5           10.89
    ## 239                       32.50                  35.0           11.13
    ## 240                       26.00                  30.5           13.02
    ## 241                       26.50                  33.0           11.21
    ## 242                       23.50                  26.0           11.26
    ## 243                       26.50                  32.5           10.67
    ## 244                       30.00                  39.5           10.91
    ## 245                       30.50                  33.0           11.18
    ## 246                       23.50                  30.5           11.33
    ## 247                       23.50                  30.0           11.04
    ## 248                       29.50                  34.0           11.18
    ## 249                       28.00                  33.5           11.37
    ## 250                       25.50                  31.5           11.42
    ## 251                       24.50                  32.0           14.01
    ## 252                       33.00                  37.0           11.18
    ## 253                       26.50                  33.0           12.48
    ## 254                       25.00                  32.5           11.83
    ## 255                       26.50                  32.5           11.30
    ## 256                       27.50                  34.0           11.18
    ## 257                       27.50                  29.0           11.83
    ## 258                       32.00                  36.5           11.43
    ## 259                       28.00                  32.0           11.59
    ## 260                       28.50                  33.0           11.37
    ## 261                       32.00                  35.5           10.44
    ## 262                       19.50                  22.5           13.40
    ## 263                       32.50                  38.5           11.72
    ## 264                       27.50                  33.0           10.72
    ## 265                       26.00                  32.5           11.00
    ## 266                       29.00                  34.5           11.75
    ## 267                       28.50                  31.5           12.01
    ## 268                       28.50                  32.0           10.99
    ## 269                       30.00                  33.0           11.54
    ## 270                       25.50                  27.5           12.10
    ## 271                       26.00                  31.5           12.90
    ## 272                       31.50                  35.0           10.56
    ## 273                       29.50                  36.0           11.85
    ## 274                       28.50                  31.5           12.18
    ## 275                       27.00                  30.5           12.19
    ## 276                       34.50                  41.0           11.38
    ## 277                       34.00                  40.5           11.09
    ## 278                       31.50                  36.5           10.70
    ## 279                       29.50                  32.0           11.63
    ## 280                       29.00                  34.5           10.63
    ## 281                       31.50                  33.0           11.61
    ## 282                       28.00                  30.0           10.93
    ## 283                       37.00                  43.5           11.24
    ## 284                       32.00                  35.5           11.70
    ## 285                       23.00                  25.0           13.32
    ## 286                       23.00                  29.0           11.50
    ## 287                       21.00                  29.0           12.90
    ## 288                       29.00                  36.0           11.95
    ## 289                       27.00                  31.0           11.58
    ## 290                       31.00                  32.0           11.38
    ## 291                       27.50                  31.5           11.46
    ## 292                       25.50                  31.5           11.64
    ## 293                       31.50                  35.0           10.45
    ## 294                       24.00                  31.0           10.89
    ## 295                       29.00                  32.0           10.97
    ## 296                       29.50                  31.0           11.15
    ## 297                       27.00                  31.5           11.75
    ## 298                       26.50                  30.5           12.49
    ## 299                       31.50                  37.5           11.28
    ## 300                       30.50                  37.0           11.03
    ## 301                       30.50                  35.5           11.21
    ## 302                       28.50                  34.5           11.91
    ## 303                       29.50                  36.0           11.33
    ## 304                       30.50                  34.5           11.17
    ## 305                       26.50                  32.5           12.28
    ## 306                       26.50                  36.0           11.10
    ## 307                       28.50                  30.0           13.08
    ## 308                       26.50                  28.5           11.81
    ## 309                       26.00                  28.5           11.07
    ## 310                       26.50                  30.0           10.72
    ## 311                       25.50                  29.5           12.93
    ## 312                       29.00                  34.5           11.08
    ## 313                       25.00                  30.5           11.81
    ## 314                       24.50                  28.5           11.74
    ## 315                       27.50                  33.0           10.94
    ## 316                       25.00                  30.0           11.31
    ## 317                       28.50                  32.5           11.01
    ## 318                       30.50                  33.5           11.45
    ## 319                       28.50                  33.5           10.95
    ## 320                       27.50                  32.5           11.27
    ## 321                       31.50                  34.0           12.32
    ## 322                       26.50                  33.0           11.18
    ## 323                       26.00                  30.0           10.99
    ## 324                       25.50                  30.0           12.97
    ## 325                       30.00                  36.0           11.14
    ## 326                       27.50                  32.5           11.50
    ## 327                       25.00                  29.0           11.37
    ## 328                       24.00                  26.5           12.61
    ## 329                       22.00                  26.0           12.65
    ## 330                       27.50                  33.0           12.52
    ## 331                       28.50                  34.5           10.91
    ## 332                       35.50                  43.5           10.75
    ## 333                       24.50                  30.5           11.98
    ## 334                       31.50                  35.5           11.27
    ## 335                       23.50                  31.5           10.93
    ## 336                       32.50                  39.5           11.43
    ## 337                       29.00                  35.5           10.93
    ## 338                       30.00                  35.0           10.79
    ## 339                       28.00                  31.5           12.13
    ## 340                       26.00                  31.5           11.65
    ## 341                       23.00                  26.5           13.13
    ## 342                       32.00                  31.5           11.80
    ## 343                       29.50                  33.5           10.75
    ## 344                       30.00                  35.0           11.20
    ## 345                       26.50                  31.0           11.92
    ## 346                       23.50                  27.5           11.59
    ## 347                       23.50                  26.0           12.60
    ## 348                       28.50                  32.5           11.15
    ## 349                       28.00                  32.0           11.67
    ## 350                       30.50                  32.5           12.58
    ## 351                       26.50                  30.5           10.32
    ## 352                       24.00                  27.5           11.94
    ## 353                       25.00                  31.5           11.74
    ## 354                       33.00                  36.5           10.52
    ## 355                       33.50                  39.5           11.73
    ## 356                       27.50                  33.5           12.06
    ## 357                       33.00                  40.5           11.12
    ## 358                       30.50                  36.5           10.51
    ## 359                       24.50                  27.0           11.51
    ## 360                       24.50                  31.0           10.85
    ## 361                       28.00                  31.0           11.94
    ## 362                       25.00                  30.5           11.41
    ## 363                       26.50                  31.5           12.13
    ## 364                       29.00                  33.5           10.50
    ## 365                       26.50                  35.5           10.48
    ## 366                       28.00                  30.5           12.53
    ## 367                       29.50                  34.0           11.53
    ## 368                       28.50                  37.0           11.92
    ## 369                       27.50                  31.0           11.60
    ## 370                       31.50                  35.5           12.51
    ## 371                       27.50                  31.5           10.63
    ## 372                       30.50                  36.5           11.48
    ## 373                       29.50                  36.0           11.48
    ## 374                       30.00                  36.0           10.62
    ## 375                       31.00                  34.0           10.84
    ## 376                       31.50                  39.0           11.20
    ## 377                       30.00                  33.0           10.70
    ## 378                       31.50                  38.5           11.12
    ## 379                       24.50                  29.0           10.89
    ## 380                       22.00                  30.0           11.59
    ## 381                       26.00                  28.5           10.87
    ## 382                       28.00                  31.5           11.87
    ## 383                       31.50                  35.0           11.60
    ## 384                       21.50                  27.0           13.13
    ## 385                       26.00                  31.0           11.62
    ## 386                       32.00                  29.5           11.38
    ## 387                       30.50                  32.5           10.80
    ## 388                       29.00                  34.0           10.44
    ## 389                       30.50                  37.0           11.64
    ## 390                       30.00                  33.0           11.04
    ## 391                       32.50                  34.5           10.39
    ## 392                       26.50                  32.0           10.64
    ## 393                       29.00                  33.0           11.70
    ## 394                       32.00                  37.5           10.69
    ## 395                       26.00                  28.5           12.80
    ## 396                       31.50                  36.5           11.44
    ## 397                       25.00                  30.5           12.12
    ## 398                       32.00                  38.0           11.09
    ## 399                       27.50                  34.5           11.54
    ## 400                       31.50                  39.0           10.49
    ## 401                       31.50                  37.0           11.17
    ## 402                       32.50                  35.0           11.33
    ## 403                       23.50                  26.5           12.40
    ## 404                       27.00                  27.5           11.05
    ## 405                       27.00                  31.5           10.80
    ## 406                       28.00                  33.0           11.98
    ## 407                       28.50                  33.0           11.26
    ## 408                       28.50                  34.0           10.84
    ## 409                       25.00                  29.5           11.11
    ## 410                       30.50                  33.0           11.50
    ## 411                       26.00                  31.0           10.86
    ## 412                       28.00                  30.5           11.39
    ## 413                       28.50                  33.5           12.11
    ## 414                       30.00                  35.0           10.83
    ## 415                       28.00                  34.0           11.39
    ## 416                       29.50                  35.0           11.11
    ## 417                       30.00                  36.0           11.58
    ## 418                       26.50                  29.5           11.45
    ## 419                       28.50                  30.5           11.57
    ## 420                       29.50                  35.0           11.73
    ## 421                       26.50                  34.0           12.02
    ## 422                       31.00                  35.5           11.45
    ## 423                       32.00                  35.5           10.58
    ## 424                       27.50                  32.5           11.77
    ## 425                       28.50                  31.5           12.28
    ## 426                       30.00                  32.5           10.54
    ## 427                       30.50                  35.0           11.58
    ## 428                       26.00                  30.5           11.62
    ## 429                       35.00                  40.0           11.08
    ## 430                       31.00                  34.0           11.41
    ## 431                       35.00                  41.0           11.32
    ## 432                       28.00                  34.5           11.42
    ## 433                       25.50                  30.5           12.26
    ## 434                       28.50                  35.5           10.57
    ## 435                       32.00                  38.5           10.57
    ## 436                       31.00                  34.0           11.03
    ## 437                       31.50                  37.5           12.00
    ## 438                       30.50                  34.5           10.53
    ## 439                       26.50                  30.0           11.54
    ## 440                       29.00                  33.0           10.88
    ## 441                       33.50                  42.0           11.07
    ## 442                       32.00                  38.0           10.53
    ## 443                       26.50                  32.0           11.97
    ## 444                       33.00                  40.5           11.03
    ## 445                       30.50                  38.0           11.69
    ## 446                       28.50                  33.0           11.02
    ## 447                       28.00                  33.5           11.30
    ## 448                       27.50                  31.5           11.34
    ## 449                       30.00                  34.0           12.06
    ## 450                       30.00                  36.5           12.23
    ## 451                       30.00                  36.5           10.35
    ## 452                       32.00                  39.0           11.27
    ## 453                       28.50                  34.5           11.23
    ## 454                       29.00                  34.5           11.40
    ## 455                       28.00                  32.0           11.42
    ## 456                       29.00                  35.5           10.67
    ## 457                       26.00                  31.0           11.62
    ## 458                       30.50                  39.0           11.46
    ## 459                       27.00                  32.0           11.28
    ## 460                       31.50                  36.0           11.47
    ## 461                       29.00                  35.5           12.25
    ## 462                       28.50                  32.5           11.67
    ## 463                       37.50                  41.5           11.45
    ## 464                       25.50                  30.5           11.46
    ## 465                       26.50                  26.0           12.57
    ## 466                       34.50                  38.5           10.54
    ## 467                       24.50                  31.0           11.11
    ## 468                       26.50                  30.0           12.68
    ## 469                       29.50                  35.5           11.91
    ## 470                       32.00                  37.0           11.25
    ## 471                       28.50                  35.5           11.01
    ## 472                       27.50                  33.0           10.94
    ## 473                       29.00                  32.5           11.73
    ## 474                       34.00                  40.5           11.13
    ## 475                       28.50                  34.5           11.25
    ## 476                       28.50                  31.0           12.52
    ## 477                       30.50                  35.0           11.05
    ## 478                       30.00                  34.0           11.56
    ## 479                       30.00                  29.5           11.59
    ## 480                       29.50                  34.5           12.08
    ## 481                       31.50                  37.0           12.29
    ## 482                       28.50                  29.5           11.67
    ## 483                       28.00                  31.5           10.95
    ## 484                       34.00                  39.5           11.36
    ## 485                       29.00                  34.0           11.58
    ## 486                       23.00                  28.0           13.70
    ## 487                       29.00                  37.5           10.71
    ## 488                       31.50                  34.0           11.61
    ## 489                       26.50                  31.5           11.90
    ## 490                       29.00                  33.5           11.53
    ## 491                       32.00                  31.0           10.69
    ## 492                       28.50                  29.0           11.71
    ## 493                       24.50                  28.0           11.30
    ## 494                       35.00                  36.5           11.54
    ## 495                       30.50                  35.0           11.78
    ## 496                       24.00                  26.5           12.18
    ## 497                       26.00                  28.0           11.88
    ## 498                       30.50                  36.5           11.69
    ## 499                       32.50                  39.5           10.57
    ## 500                       32.50                  41.0           11.27
    ## 501                       33.00                  38.5           11.61
    ## 502                       30.00                  35.0           11.47
    ## 503                       35.50                  40.5           11.63
    ## 504                       30.00                  34.5           11.12
    ## 505                       25.00                  30.5           12.52
    ## 506                       32.50                  38.0           11.60
    ## 507                       27.00                  30.0           12.63
    ## 508                       30.00                  33.5           11.13
    ## 509                       28.00                  32.0           11.32
    ## 510                       26.00                  33.5           12.33
    ## 511                       28.00                  32.0           11.43
    ## 512                       27.00                  32.5           12.07
    ## 513                       33.50                  38.0           12.00
    ## 514                       29.00                  32.5           11.30
    ## 515                       28.50                  36.0           11.72
    ## 516                       26.00                  29.0           11.88
    ## 517                       32.00                  36.0           11.39
    ## 518                       30.50                  34.0           12.31
    ## 519                       31.00                  35.5           12.15
    ## 520                       28.50                  34.0           12.47
    ## 521                       31.00                  33.0           13.31
    ## 522                       30.50                  33.0           12.11
    ## 523                       34.50                  39.0           11.00
    ## 524                       33.50                  38.5           10.94
    ## 525                       26.50                  35.0           11.78
    ## 526                       25.00                  30.0           12.18
    ## 527                       24.50                  28.5           11.02
    ## 528                       32.00                  34.5           11.57
    ## 529                       25.50                  30.0           11.19
    ## 530                       31.50                  36.5           11.35
    ## 531                       29.50                  36.0           11.73
    ## 532                       29.00                  34.0           11.00
    ## 533                       24.00                  30.5           11.96
    ## 534                       33.00                  38.0           11.59
    ## 535                       27.00                  29.5           12.10
    ## 536                       28.50                  34.5           10.95
    ## 537                       27.50                  31.5           11.70
    ## 538                       27.00                  31.5           11.87
    ## 539                       29.50                  31.5           11.77
    ## 540                       28.50                  32.5           10.39
    ## 541                       32.50                  37.5           11.79
    ## 542                       32.00                  34.0           11.67
    ## 543                       23.00                  28.0           11.61
    ## 544                       27.50                  34.5           11.69
    ## 545                       26.50                  30.5           11.39
    ## 546                       34.00                  38.0           11.07
    ## 547                       26.50                  33.5           11.63
    ## 548                       24.00                  32.5           11.33
    ## 549                       29.50                  33.5           11.65
    ## 550                       28.50                  35.0           11.82
    ## 551                       33.50                  37.5           10.96
    ## 552                       28.50                  35.0           10.87
    ## 553                       30.50                  35.0           11.34
    ## 554                       27.50                  32.5           11.30
    ## 555                       29.00                  34.0           10.68
    ## 556                       28.50                  33.0           11.51
    ## 557                       31.00                  37.5           11.76
    ## 558                       35.00                  41.0           11.96
    ## 559                       28.50                  30.0           11.49
    ## 560                       25.00                  29.0           12.16
    ## 561                       26.00                  29.0           12.60
    ## 562                       26.50                  31.0           11.65
    ## 563                       27.50                  30.5           12.14
    ## 564                       31.50                  34.5           11.40
    ## 565                       28.50                  33.5           11.48
    ## 566                       30.50                  35.5           11.76
    ## 567                       27.50                  32.0           11.46
    ## 568                       34.50                  37.0           11.06
    ## 569                       39.50                  40.5           11.00
    ## 570                       32.50                  38.5           11.33
    ## 571                       28.50                  30.0           12.18
    ## 572                       28.50                  35.0           11.27
    ## 573                       31.00                  38.0           11.26
    ## 574                       30.00                  35.0           11.06
    ## 575                       24.50                  29.5           10.68
    ## 576                       30.00                  34.0           11.59
    ## 577                       29.00                  31.0           12.47
    ## 578                       26.00                  27.0           11.65
    ## 579                       29.50                  34.5           11.19
    ## 580                       24.00                  26.5           11.87
    ## 581                       31.00                  36.0           11.67
    ## 582                       26.00                  28.5           12.72
    ## 583                       27.50                  33.0           11.84
    ## 584                       27.50                  33.5           11.59
    ## 585                       28.00                  32.0           12.47
    ## 586                       33.50                  37.5           11.86
    ## 587                       35.00                  42.0           11.77
    ## 588                       27.50                  33.5           11.45
    ## 589                       30.00                  33.0           12.30
    ## 590                       30.50                  35.0           11.37
    ## 591                       21.50                  27.0           12.40
    ## 592                       32.00                  40.0           10.81
    ## 593                       24.50                  27.5           12.16
    ## 594                       27.50                  31.5           11.21
    ## 595                       30.50                  38.0           11.33
    ## 596                       29.50                  32.0           11.49
    ## 597                       26.50                  28.0           10.62
    ## 598                       34.00                  37.5           12.20
    ## 599                       28.50                  33.0           12.17
    ## 600                       20.50                  26.0           12.26
    ## 601                       30.50                  36.0           11.15
    ## 602                       28.00                  34.5           11.51
    ## 603                       27.00                  31.0           11.75
    ## 604                       27.00                  32.0           12.02
    ## 605                       25.50                  31.0           11.61
    ## 606                       26.00                  30.5           12.30
    ## 607                       29.00                  32.0           12.06
    ## 608                       26.00                  30.0           11.79
    ## 609                       29.50                  34.5           11.57
    ## 610                       27.50                  30.5           12.77
    ## 611                       29.50                  35.0           11.17
    ## 612                       23.00                  28.0           11.76
    ## 613                       30.50                  41.0           11.04
    ## 614                       29.50                  35.0           11.23
    ## 615                       32.00                  38.5           11.71
    ## 616                       27.00                  32.5           12.75
    ## 617                       25.50                  32.0           11.19
    ## 618                       34.50                  38.5           10.54
    ## 619                       24.00                  28.0           12.45
    ## 620                       32.00                  37.0           10.65
    ## 621                       30.50                  35.5           11.32
    ## 622                       32.00                  36.0           11.79
    ## 623                       27.00                  32.5           11.30
    ## 624                       29.00                  35.0           11.86
    ## 625                       26.50                  33.5           11.51
    ## 626                       31.50                  36.5           10.65
    ## 627                       25.00                  31.5           11.30
    ## 628                       34.50                  40.0           11.69
    ## 629                       25.00                  29.5           11.08
    ## 630                       34.50                  41.0           11.08
    ## 631                       31.00                  39.0           10.98
    ## 632                       27.00                  33.5           12.52
    ## 633                       26.00                  29.5           11.97
    ## 634                       34.00                  40.0           11.03
    ## 635                       29.00                  36.5           10.58
    ## 636                       30.00                  36.5           10.98
    ## 637                       31.50                  36.5           11.32
    ## 638                       27.50                  31.5           11.34
    ## 639                       31.00                  35.0           11.98
    ## 640                       29.50                  39.0           10.49
    ## 641                       26.00                  33.0           11.50
    ## 642                       30.50                  35.5           11.26
    ## 643                       29.50                  38.5           11.08
    ## 644                       25.50                  32.5           11.15
    ## 645                       27.00                  33.5           10.83
    ## 646                       28.50                  33.0           11.17
    ## 647                       30.50                  33.5           10.45
    ## 648                       28.00                  35.0           11.18
    ## 649                       29.50                  35.5           11.07
    ## 650                       25.00                  28.0           12.11
    ## 651                       29.00                  38.5           11.88
    ## 652                       28.00                  32.5           10.63
    ## 653                       31.50                  38.0           11.14
    ## 654                       28.50                  34.0           11.81
    ## 655                       33.00                  40.0           10.86
    ## 656                       25.50                  30.0           11.56
    ## 657                       29.00                  33.0           11.30
    ## 658                       32.00                  35.5           10.95
    ## 659                       27.50                  34.0           11.12
    ## 660                       31.50                  37.0           11.10
    ## 661                       31.50                  35.0           11.17
    ## 662                       28.50                  34.0           11.67
    ## 663                       31.00                  35.0           12.23
    ## 664                       28.50                  34.0           10.64
    ## 665                       26.00                  31.0           10.66
    ## 666                       30.50                  35.0           11.21
    ## 667                       29.00                  36.5           10.98
    ## 668                       28.50                  31.5           10.78
    ## 669                       30.00                  36.5           10.44
    ## 670                       31.50                  36.5           10.61
    ## 671                       28.50                  37.0           10.96
    ## 672                       27.50                  33.0           10.87
    ## 673                       28.50                  32.5           11.10
    ## 674                       26.00                  31.0           10.99
    ## 675                       29.00                  33.5           10.69
    ## 676                       29.50                  34.5           10.94
    ## 677                       34.00                  37.5           11.54
    ## 678                       30.50                  36.5           11.05
    ## 679                       31.00                  33.0           10.73
    ## 680                       30.50                  37.0           11.15
    ## 681                       27.50                  33.0           11.47
    ## 682                       22.50                  26.0           13.20
    ## 683                       23.00                  28.0           11.48
    ## 684                       27.00                  33.5           11.29
    ## 685                       30.00                  35.5           11.86
    ## 686                       29.50                  37.5           10.98
    ## 687                       31.00                  36.0           11.15
    ## 688                       26.00                  35.0           11.65
    ## 689                       31.50                  37.5           11.47
    ## 690                       25.00                  30.5           12.78
    ## 691                       27.50                  33.0           12.31
    ## 692                       23.50                  27.5           11.40
    ## 693                       31.50                  34.5           11.03
    ## 694                       31.00                  36.0           11.70
    ## 695                       23.50                  32.0           11.69
    ## 696                       31.50                  35.5           11.74
    ## 697                       23.50                  28.5           13.44
    ## 698                       24.50                  30.5           11.65
    ## 699                       24.00                  28.5           11.83
    ## 700                       30.50                  34.5           11.73
    ## 701                       31.00                  36.0           10.87
    ## 702                       28.50                  36.5           11.10
    ## 703                       29.00                  34.0           11.68
    ## 704                       29.00                  33.0           10.89
    ## 705                       31.50                  38.5           11.25
    ## 706                       32.00                  37.0           11.43
    ## 707                       26.00                  32.5           10.88
    ## 708                       28.00                  32.0           11.59
    ## 709                       27.00                  31.5           11.61
    ## 710                       25.00                  29.0           12.10
    ## 711                       24.00                  30.5           12.32
    ## 712                       25.50                  26.5           12.07
    ## 713                       28.50                  33.5           11.14
    ## 714                       23.50                  30.5           11.27
    ## 715                       25.00                  28.5           11.33
    ## 716                       31.00                  37.5           11.65
    ## 717                       25.50                  28.0           12.49
    ## 718                       27.00                  33.0           11.39
    ## 719                       23.00                  32.0           11.43
    ## 720                       27.50                  34.5           11.06
    ## 721                       31.00                  33.5           11.15
    ## 722                       29.50                  32.5           11.61
    ## 723                       30.00                  39.0           10.84
    ## 724                       23.00                  31.5           11.21
    ## 725                       31.00                  40.0           11.38
    ## 726                       27.00                  31.5           11.83
    ## 727                       31.50                  36.0           11.88
    ## 728                       34.00                  38.5           10.74
    ## 729                       29.00                  36.0           10.96
    ## 730                       32.00                  39.0           11.12
    ## 731                       29.50                  38.5           10.07
    ## 732                       28.00                  35.0           10.94
    ## 733                       29.50                  34.5           11.85
    ## 734                       26.50                  31.0           11.45
    ## 735                       30.50                  35.0           11.35
    ## 736                       28.00                  33.0           10.42
    ## 737                       32.50                  37.0           10.33
    ## 738                       25.00                  32.0           11.47
    ## 739                       28.50                  33.0           11.92
    ## 740                       31.50                  37.5           10.96
    ## 741                       30.00                  37.0           11.68
    ## 742                       32.50                  36.0           10.47
    ## 743                       28.50                  33.0           11.51
    ## 744                       29.50                  34.0           11.68
    ## 745                       33.50                  38.0           11.21
    ## 746                       27.50                  35.0           10.75
    ## 747                       26.00                  32.5           11.30
    ## 748                       29.00                  37.5           10.74
    ## 749                       30.00                  35.5           11.20
    ## 750                       25.50                  32.0           11.45
    ## 751                       33.00                  40.5           11.00
    ## 752                       29.50                  36.5           10.82
    ## 753                       27.00                  31.0           11.18
    ## 754                       28.50                  33.0           10.81
    ## 755                       32.00                  39.0           11.23
    ## 756                       27.00                  33.0           11.55
    ## 757                       32.00                  34.5           11.12
    ## 758                       27.00                  31.5           11.85
    ## 759                       29.00                  33.0           12.28
    ## 760                       24.50                  32.0           11.00
    ## 761                       25.50                  31.5           11.05
    ## 762                       27.00                  33.5           11.74
    ## 763                       29.50                  42.0           11.13
    ## 764                       36.50                  42.0           11.10
    ## 765                       23.00                  30.0           11.22
    ## 766                       30.50                  37.5           11.33
    ## 767                       26.00                  30.5           12.43
    ## 768                       29.50                  36.0           12.43
    ## 769                       28.50                  34.0           11.05
    ## 770                       32.00                  36.5           11.31
    ## 771                       31.50                  40.0           10.49
    ## 772                       27.50                  30.5           11.72
    ## 773                       26.50                  31.5           10.99
    ## 774                       30.00                  35.0           10.92
    ## 775                       28.00                  33.5           11.26
    ## 776                       23.50                  25.0           12.02
    ## 777                       32.00                  39.5           10.87
    ## 778                       29.00                  34.5           11.03
    ## 779                       25.00                  30.5           12.74
    ## 780                       32.00                  37.0           10.48
    ## 781                       38.00                  39.5           10.93
    ## 782                       30.00                  34.0           12.50
    ## 783                       33.00                  39.0           10.95
    ## 784                       26.00                  29.5           11.68
    ## 785                       27.50                  32.5           11.40
    ## 786                       31.00                  34.5           11.45
    ## 787                       33.50                  40.0           10.82
    ## 788                       31.50                  33.5           10.83
    ## 789                       29.00                  36.5           10.59
    ## 790                       34.00                  33.5           12.35
    ## 791                       29.00                  34.0           11.35
    ## 792                       29.50                  32.5           10.63
    ## 793                       28.00                  33.0           11.01
    ## 794                       32.50                  37.0           10.87
    ## 795                       28.00                  35.0           11.00
    ## 796                       27.50                  30.0           11.56
    ## 797                       30.00                  32.5           11.84
    ## 798                       30.50                  36.5           11.36
    ## 799                       32.00                  39.5           10.98
    ## 800                       33.50                  41.5           10.76
    ## 801                       29.50                  34.5           11.57
    ## 802                       33.00                  38.5           11.31
    ## 803                       27.50                  32.5           11.94
    ## 804                       28.50                  35.0           11.61
    ## 805                       32.00                  35.5           11.77
    ## 806                       31.50                  38.0           10.98
    ## 807                       29.00                  33.5           11.12
    ## 808                       30.00                  32.5           11.34
    ## 809                       34.50                  39.5           11.15
    ## 810                       27.00                  32.5           11.26
    ## 811                       31.00                  37.0           12.03
    ## 812                       29.50                  31.0           12.14
    ## 813                       28.00                  31.0           11.45
    ## 814                       33.00                  37.0           11.34
    ## 815                       30.50                  36.0           11.05
    ## 816                       31.50                  40.0           11.72
    ## 817                       33.50                  37.5           11.32
    ## 818                       31.00                  36.5           10.97
    ## 819                       28.50                  30.5           11.89
    ## 820                       31.50                  31.5           11.65
    ## 821                       34.00                  40.5           10.64
    ## 822                       32.50                  37.5           11.54
    ## 823                       28.50                  35.5           11.96
    ## 824                       31.00                  37.5           11.78
    ## 825                       27.00                  32.5           11.59
    ## 826                       26.50                  29.5           12.33
    ## 827                       29.50                  31.0           12.77
    ## 828                       33.50                  40.0           10.87
    ## 829                       32.00                  36.5           11.48
    ## 830                       33.00                  36.5           10.57
    ## 831                       32.50                  40.5           10.65
    ## 832                       28.50                  36.5           11.72
    ## 833                       30.00                  35.5           10.75
    ## 834                       30.00                  34.0           11.13
    ## 835                       28.50                  33.0           11.85
    ## 836                       28.00                  37.5           10.40
    ## 837                       28.00                  34.0           11.78
    ## 838                       31.00                  36.5           11.33
    ## 839                       29.50                  36.5           11.20
    ## 840                       29.00                  34.5           10.60
    ## 841                       33.00                  40.5           10.98
    ## 842                       30.00                  32.0           11.83
    ## 843                       31.50                  41.0           10.68
    ## 844                       28.00                  34.5           11.68
    ## 845                       29.50                  36.0           11.69
    ## 846                       30.50                  36.0           10.67
    ## 847                       26.00                  31.5           11.22
    ## 848                       28.00                  32.5           12.75
    ## 849                       29.50                  36.5           11.55
    ## 850                       32.00                  38.5           11.16
    ## 851                       25.00                  29.0           12.85
    ## 852                       30.00                  36.0           10.75
    ## 853                       31.50                  37.5           10.68
    ## 854                       29.50                  37.5           10.77
    ## 855                       26.50                  30.0           11.52
    ## 856                       28.50                  34.0           11.85
    ## 857                       27.50                  33.5           10.64
    ## 858                       34.00                  38.0           11.47
    ## 859                       34.50                  44.0           10.64
    ## 860                       27.00                  33.5           10.72
    ## 861                       32.50                  40.5           10.19
    ## 862                       30.00                  36.5           12.01
    ## 863                       30.50                  40.0           10.82
    ## 864                       32.00                  38.5           11.02
    ## 865                       32.50                  42.0           11.87
    ## 866                       33.50                  38.0           11.82
    ## 867                       29.50                  37.0           10.99
    ## 868                       24.50                  29.5           12.19
    ## 869                       28.00                  33.5           11.76
    ## 870                       33.00                  42.0           10.69
    ## 871                       24.50                  29.5           11.42
    ## 872                       33.50                  39.5           11.16
    ## 873                       31.00                  34.5           11.49
    ## 874                       30.50                  36.0           10.89
    ## 875                       27.00                  36.0           11.25
    ## 876                       33.00                  38.5           10.86
    ## 877                       33.00                  40.5           10.99
    ## 878                       30.00                  36.5           11.36
    ## 879                       30.00                  34.0           11.09
    ## 880                       33.50                  41.5           10.59
    ## 881                       30.00                  36.5           11.36
    ## 882                       29.00                  32.0           10.90
    ## 883                       28.50                  32.0           12.94
    ## 884                       34.50                  40.5           11.64
    ## 885                       26.50                  29.0           12.49
    ## 886                       35.50                  37.5           10.82
    ## 887                       24.50                  29.5           12.13
    ## 888                       31.50                  39.5           11.39
    ## 889                       29.00                  33.0           12.15
    ## 890                       31.00                  33.5           11.39
    ## 891                       30.50                  35.5           11.60
    ## 892                       27.00                  30.0           12.05
    ## 893                       28.00                  32.0           11.60
    ## 894                       36.50                  43.5           12.62
    ## 895                       29.00                  39.5           11.28
    ## 896                       30.50                  36.5           11.55
    ## 897                       33.00                  38.5           10.76
    ## 898                       29.00                  32.0           10.78
    ## 899                       26.50                  32.0           11.46
    ## 900                       34.00                  40.0           11.15
    ## 901                       29.50                  35.0           11.55
    ## 902                       29.00                  36.0           11.12
    ## 903                       31.50                  34.5           10.75
    ## 904                       27.00                  35.0           11.00
    ## 905                       32.50                  39.0           10.81
    ## 906                       32.00                  37.0           11.12
    ## 907                       28.50                  33.5           11.11
    ## 908                       29.00                  36.0           11.21
    ## 909                       33.00                  37.5           12.63
    ## 910                       33.50                  41.5           10.80
    ## 911                       28.50                  31.5           11.20
    ## 912                       33.50                  41.5           10.42
    ## 913                       29.00                  35.0           11.32
    ## 914                       29.50                  34.0           11.61
    ## 915                       33.00                  37.0           12.71
    ## 916                       28.50                  36.5           11.10
    ## 917                       27.50                  34.5           11.07
    ## 918                       30.50                  37.5           11.60
    ## 919                       31.00                  35.5           11.57
    ## 920                       27.50                  32.5           11.63
    ## 921                       31.50                  35.5           11.06
    ## 922                       28.50                  35.0           11.28
    ## 923                       29.00                  35.5           11.45
    ## 924                       36.50                  41.5           11.22
    ## 925                       25.00                  31.0           11.97
    ## 926                       33.00                  36.0           10.82
    ## 927                       29.50                  34.0           11.57
    ## 928                       29.00                  35.5           10.79
    ## 929                       31.00                  36.0           11.92
    ## 930                       29.00                  32.5           11.48
    ## 931                       31.00                  37.0           11.45
    ## 932                       27.00                  35.5           11.10
    ## 933                       31.00                  37.5           11.30
    ## 934                       27.00                  33.0           10.83
    ## 935                       32.00                  37.5           11.18
    ## 936                       38.00                  43.0           11.13
    ## 937                       31.50                  35.5           11.21
    ## 938                       32.00                  41.0           10.71
    ## 939                       27.50                  34.5           10.27
    ## 940                       27.50                  34.5           10.94
    ## 941                       29.00                  32.0           11.60
    ## 942                       37.50                  44.0           10.58
    ## 943                       24.50                  31.0           11.44
    ## 944                       28.50                  34.5           12.22
    ## 945                       25.50                  34.5           10.64
    ## 946                       27.00                  35.0           10.66
    ## 947                       28.50                  31.5           11.64
    ## 948                       28.00                  34.0           11.41
    ## 949                       26.50                  33.0           11.20
    ## 950                       28.50                  34.5           11.49
    ## 951                       26.00                  28.5           10.56
    ## 952                       28.50                  34.0           10.81
    ## 953                       32.00                  38.0           10.41
    ## 954                       25.50                  31.0           10.95
    ## 955                       32.50                  36.0           11.24
    ## 956                       27.50                  33.5           10.87
    ## 957                       33.50                  37.5           11.19
    ## 958                       22.50                  25.0           11.76
    ## 959                       26.50                  32.5           11.84
    ## 960                       28.50                  35.0           11.20
    ## 961                       26.00                  31.5           10.95
    ## 962                       33.00                  37.5           11.72
    ## 963                       29.00                  37.5           10.93
    ## 964                       34.50                  37.0           11.09
    ## 965                       29.75                  35.5           10.90
    ## 966                       25.00                  31.0           10.91
    ## 967                       25.00                  31.5           11.78
    ## 968                       32.50                  40.5           10.69
    ## 969                       34.00                  39.5           11.88
    ## 970                       33.50                  39.5           10.63
    ## 971                       33.00                  38.0           10.84
    ## 972                       34.00                  43.0           10.47
    ## 973                       34.50                  43.0           10.78
    ## 974                       34.50                  40.0           10.97
    ## 975                       28.00                  34.0           11.40
    ## 976                       30.50                  37.0           11.25
    ## 977                       31.50                  39.0           10.56
    ## 978                       25.50                  28.5           12.26
    ## 979                       29.50                  31.0           11.16
    ## 980                       30.00                  35.5           10.77
    ## 981                       32.50                  38.0           10.45
    ## 982                       32.00                  38.0           10.90
    ## 983                       29.00                  33.0           11.16
    ## 984                       35.00                  41.0           11.53
    ## 985                       37.50                  40.5           10.26
    ## 986                       30.50                  35.5           10.77
    ## 987                       32.50                  38.5           11.93
    ## 988                       30.50                  37.0           10.79
    ## 989                       31.00                  35.0           11.24
    ## 990                       32.00                  38.0           10.73
    ## 991                       35.50                  44.0           11.52
    ## 992                       32.50                  35.5           11.01
    ## 993                       33.50                  37.5           11.00
    ## 994                       26.00                  29.0           11.48
    ## 995                       29.50                  38.5           11.15
    ## 996                       25.50                  28.5           12.11
    ## 997                       29.50                  34.5           11.32
    ## 998                       37.50                  43.5           10.81
    ## 999                       33.50                  38.0           11.88
    ## 1000                      36.00                  37.0           11.35
    ## 1001                      33.00                  39.5           11.35
    ## 1002                      28.50                  35.5           11.68
    ## 1003                      32.00                  36.5           11.15
    ## 1004                      30.00                  38.0           11.01
    ## 1005                      30.00                  35.5           10.88
    ## 1006                      25.00                  31.0           11.64
    ## 1007                      27.50                  33.5           11.65
    ## 1008                      33.50                  39.5           10.82
    ## 1009                      31.50                  38.5           11.57
    ## 1010                      34.00                  36.0           10.96
    ## 1011                      31.50                  32.0           11.27
    ## 1012                      33.00                  38.0           10.56
    ## 1013                      30.50                  36.5           11.25
    ## 1014                      27.00                  29.5           12.02
    ## 1015                      26.00                  29.0           12.80
    ## 1016                      26.50                  35.5           10.57
    ## 1017                      32.50                  38.0           10.80
    ## 1018                      29.00                  34.5           11.52
    ## 1019                      27.00                  32.0           10.51
    ## 1020                      27.50                  35.5           11.65
    ## 1021                      30.00                  36.0           10.72
    ## 1022                      28.50                  35.5           11.35
    ## 1023                      26.00                  31.0           12.08
    ## 1024                      33.50                  38.5           11.94
    ## 1025                      31.50                  40.5           11.99
    ## 1026                      31.50                  35.5           11.82
    ## 1027                      33.00                  35.5           11.24
    ## 1028                      29.50                  32.5           12.52
    ## 1029                      23.50                  31.0           11.62
    ## 1030                      30.00                  36.5           11.70
    ## 1031                      30.00                  37.0           11.18
    ## 1032                      31.50                  38.0           10.63
    ## 1033                      31.00                  35.5           11.47
    ## 1034                      24.50                  27.5           11.79
    ## 1035                      28.00                  33.5           10.95
    ## 1036                      31.00                  37.5           11.14
    ## 1037                      25.50                  33.0           11.42
    ## 1038                      33.00                  37.5           11.66
    ## 1039                      34.50                  44.5           10.88
    ## 1040                      30.50                  36.0           10.55
    ## 1041                      31.50                  38.0           11.18
    ## 1042                      34.00                  39.0           10.84
    ## 1043                      27.50                  33.5           11.34
    ## 1044                      28.50                  38.0           11.16
    ## 1045                      27.00                  32.5           11.08
    ## 1046                      27.50                  35.5           11.15
    ## 1047                      29.00                  33.5           11.81
    ## 1048                      31.50                  37.0           12.31
    ## 1049                      31.50                  36.0           11.25
    ## 1050                      34.50                  42.0           10.94
    ## 1051                      26.50                  31.5           11.78
    ## 1052                      29.50                  35.5           10.80
    ## 1053                      26.50                  31.0           11.34
    ## 1054                      27.00                  34.0           10.72
    ## 1055                      29.00                  34.5           11.55
    ## 1056                      29.50                  33.5           11.68
    ## 1057                      34.00                  41.0           11.41
    ## 1058                      28.00                  30.5           12.91
    ## 1059                      36.50                  40.5           11.53
    ## 1060                      28.00                  33.5           11.00
    ## 1061                      27.00                  33.0           12.40
    ## 1062                      35.00                  40.5           10.58
    ## 1063                      28.00                  39.5           12.17
    ## 1064                      26.00                  30.5           11.80
    ## 1065                      28.50                  32.5           12.68
    ## 1066                      28.50                  36.0           11.37
    ## 1067                      35.50                  41.5           12.85
    ## 1068                      30.50                  38.5           11.15
    ## 1069                      27.00                  30.5           11.48
    ## 1070                      24.50                  32.0           10.75
    ## 1071                      25.50                  32.5           12.00
    ## 1072                      26.00                  32.5           11.28
    ## 1073                      31.00                  36.5           10.84
    ## 1074                      27.50                  34.5           10.42
    ## 1075                      26.00                  31.5           12.85
    ## 1076                      32.50                  40.5           11.50
    ## 1077                      32.50                  40.5           10.31
    ## 1078                      29.50                  35.0           12.48
    ## 1079                      31.00                  31.5           12.97
    ## 1080                      27.00                  33.0           11.39
    ## 1081                      30.50                  35.0           11.20
    ## 1082                      32.00                  37.5           11.04
    ## 1083                      30.50                  34.0           11.58
    ## 1084                      32.50                  38.0           11.11
    ## 1085                      26.00                  33.0           11.51
    ## 1086                      29.00                  37.0           10.59
    ## 1087                      25.00                  31.5           11.89
    ## 1088                      29.00                  36.5           11.04
    ## 1089                      34.50                  42.0           10.72
    ## 1090                      32.50                  40.5           10.53
    ## 1091                      34.00                  41.5           10.93
    ## 1092                      28.50                  34.5           11.59
    ## 1093                      28.00                  35.5           11.11
    ## 1094                      29.00                  35.0           12.19
    ## 1095                      31.00                  40.5           11.24
    ## 1096                      31.00                  37.0           11.28
    ## 1097                      25.00                  31.0           10.95
    ## 1098                      31.50                  39.5           12.11
    ## 1099                      27.50                  32.0           11.28
    ## 1100                      25.50                  33.0           10.96
    ## 1101                      31.00                  38.0           10.89
    ## 1102                      25.00                  31.0           11.83
    ## 1103                      31.00                  39.0           10.75
    ## 1104                      31.50                  35.0           12.28
    ## 1105                      29.00                  36.5           10.43
    ## 1106                      29.00                  36.5           10.64
    ## 1107                      27.00                  31.5           11.35
    ## 1108                      27.50                  32.0           11.77
    ## 1109                      31.00                  36.5           11.48
    ## 1110                      29.50                  36.0           11.89
    ## 1111                      30.00                  33.0           11.07
    ## 1112                      30.50                  37.0           11.25
    ## 1113                      27.50                  33.5           10.90
    ## 1114                      33.00                  42.0           11.08
    ## 1115                      27.00                  34.0           11.53
    ## 1116                      25.50                  31.0           11.90
    ## 1117                      25.00                  33.0           11.26
    ## 1118                      31.00                  39.5           11.26
    ## 1119                      32.00                  41.5           11.27
    ## 1120                      33.00                  41.5           11.02
    ## 1121                      26.00                  32.0           11.48
    ## 1122                      28.50                  35.5           12.28
    ## 1123                      30.00                  35.0           10.96
    ## 1124                      33.50                  39.5           11.37
    ## 1125                      32.00                  40.0           11.02
    ## 1126                      27.00                  34.0           11.48
    ## 1127                      31.50                  40.0           10.87
    ## 1128                      27.00                  34.0           11.39
    ## 1129                      28.00                  33.5           12.79
    ## 1130                      28.00                  35.5           11.29
    ## 1131                      28.00                  33.0           10.86
    ## 1132                      29.00                  33.0           12.65
    ## 1133                      30.50                  37.0           11.09
    ## 1134                      36.00                  42.5            9.97
    ## 1135                      32.00                  35.5           10.98
    ## 1136                      27.50                  33.0           11.11
    ## 1137                      31.00                  36.5           11.06
    ## 1138                      34.00                  37.5           11.30
    ## 1139                      27.00                  31.0           11.95
    ## 1140                      34.00                  40.5           10.61
    ## 1141                      31.50                  36.5           11.26
    ## 1142                      27.50                  32.0           11.32
    ## 1143                      29.50                  34.0           10.69
    ## 1144                      32.50                  38.0           10.79
    ## 1145                      30.00                  38.5           10.63
    ## 1146                      28.50                  34.0           10.53
    ## 1147                      22.00                  26.5           13.01
    ## 1148                      29.50                  33.5           11.29
    ## 1149                      32.50                  36.5           11.61
    ## 1150                      27.50                  36.0           10.83
    ## 1151                      30.50                  36.5           10.48
    ## 1152                      33.00                  41.5           10.73
    ## 1153                      33.50                  40.5           10.77
    ## 1154                      27.00                  34.5           11.74
    ## 1155                      30.50                  35.5           11.46
    ## 1156                      27.50                  35.5           11.44
    ## 1157                      26.00                  34.5           11.34
    ## 1158                      26.00                  31.5           11.21
    ## 1159                      30.50                  36.5           10.52
    ## 1160                      28.00                  35.5           11.21
    ## 1161                      29.00                  33.0           10.99
    ## 1162                      23.50                  26.0           11.80
    ## 1163                      35.00                  43.0           11.09
    ## 1164                      31.00                  38.5           12.15
    ## 1165                      32.50                  38.5           11.51
    ## 1166                      28.00                  35.5           10.44
    ## 1167                      29.50                  37.5           11.26
    ## 1168                      29.50                  33.5           11.54
    ## 1169                      24.00                  30.0           11.60
    ## 1170                      32.00                  38.5           10.76
    ## 1171                      30.50                  37.0           10.88
    ## 1172                      30.50                  38.5           11.71
    ## 1173                      33.00                  38.0           11.63
    ## 1174                      28.50                  31.5           12.46
    ## 1175                      29.00                  37.0           11.01
    ## 1176                      28.00                  35.5           10.59
    ## 1177                      27.00                  34.0           10.82
    ## 1178                      24.00                  32.5           12.04
    ## 1179                      26.00                  32.5           12.03
    ## 1180                      32.50                  35.5           10.63
    ## 1181                      27.00                  38.0           11.01
    ## 1182                      30.00                  34.0           10.77
    ## 1183                      29.50                  36.5           11.16
    ## 1184                      27.00                  35.5           11.85
    ## 1185                      30.50                  40.5           10.76
    ## 1186                      31.00                  38.0           11.57
    ## 1187                      27.50                  35.0           10.39
    ## 1188                      26.00                  31.5           10.83
    ## 1189                      29.00                  37.5           10.70
    ## 1190                      28.00                  33.0           11.33
    ## 1191                      37.00                  41.0           11.82
    ## 1192                      37.00                  43.5           11.35
    ## 1193                      34.00                  38.5           10.87
    ## 1194                      28.00                  37.0           11.95
    ## 1195                      31.50                  40.5           10.44
    ## 1196                      33.00                  35.5           10.79
    ## 1197                      30.50                  36.5           11.25
    ## 1198                      28.00                  34.5           11.04
    ## 1199                      30.50                  39.5           10.64
    ## 1200                      29.50                  36.5           10.82
    ## 1201                      31.50                  37.5           12.16
    ## 1202                      37.50                  42.5           10.59
    ## 1203                      30.50                  35.5           11.21
    ## 1204                      24.00                  31.0           10.60
    ## 1205                      23.00                  31.5           11.56
    ## 1206                      31.00                  36.0           11.52
    ## 1207                      28.50                  35.0           11.77
    ## 1208                      31.50                  38.5           11.93
    ## 1209                      29.00                  37.5           10.65
    ## 1210                      33.00                  38.0           10.94
    ## 1211                      29.00                  35.0           11.29
    ## 1212                      30.00                  33.5           11.26
    ## 1213                      29.50                  35.5           11.21
    ## 1214                      26.50                  37.5           12.29
    ## 1215                      30.00                  34.0           11.42
    ## 1216                      32.50                  39.5           11.17
    ## 1217                      34.50                  44.0           11.40
    ## 1218                      35.00                  41.0           10.35
    ## 1219                      30.00                  34.0           10.78
    ## 1220                      27.50                  32.0           11.60
    ## 1221                      26.00                  32.5           11.80
    ## 1222                      21.50                  27.0           11.73
    ## 1223                      29.00                  39.0           11.17
    ## 1224                      24.00                  28.0           11.15
    ## 1225                      32.00                  38.0           11.10
    ##      timeThreeQuarterCourtSprint pctBodyFat na_count
    ## 1                           3.38      10.50        1
    ## 2                           3.40       8.00        1
    ## 3                           3.47      10.50        1
    ## 4                           3.55      10.20        1
    ## 5                           3.06       5.90        1
    ## 6                           3.15       4.50        1
    ## 7                           3.24      11.30        1
    ## 8                           3.00       7.20        1
    ## 9                           3.16       6.00        1
    ## 10                          3.32       7.30        1
    ## 11                          3.52      12.00        1
    ## 12                          3.44       9.90        1
    ## 13                          3.18       6.70        1
    ## 14                          3.25       6.00        1
    ## 15                          3.25       5.40        1
    ## 16                          3.22       5.30        1
    ## 17                          3.20       6.70        1
    ## 18                          3.34       7.30        1
    ## 19                          3.19       6.20        1
    ## 20                          3.25       8.00        1
    ## 21                          3.37       9.30        1
    ## 22                          3.27       5.90        1
    ## 23                          3.35       6.70        1
    ## 24                          3.58      12.40        1
    ## 25                          3.28       6.00        1
    ## 26                          3.33       6.00        1
    ## 27                          3.60       8.00        1
    ## 28                          3.40       7.60        1
    ## 29                          3.24       8.00        1
    ## 30                          3.31       5.40        1
    ## 31                          3.46       6.70        1
    ## 32                          3.33       6.20        1
    ## 33                          3.06       6.05        1
    ## 34                          3.53      12.40        1
    ## 35                          3.40       7.00        1
    ## 36                          3.69      11.70        1
    ## 37                          3.45       6.00        1
    ## 38                          3.35       8.00        1
    ## 39                          3.20       6.00        1
    ## 40                          3.30       6.20        1
    ## 41                          3.10       6.70        1
    ## 42                          3.27       6.20        1
    ## 43                          3.34       6.70        1
    ## 44                          3.31       8.00        1
    ## 45                          3.25       5.90        1
    ## 46                          3.41       8.00        1
    ## 47                          3.44       9.30        1
    ## 48                          3.06       5.90        1
    ## 49                          3.21       5.30        1
    ## 50                          3.36      12.40        1
    ## 51                          3.52      12.40        1
    ## 52                          3.22       6.10        1
    ## 53                          3.43      12.40        1
    ## 54                          3.46       8.00        1
    ## 55                          3.33       5.70        1
    ## 56                          3.23       8.00        1
    ## 57                          3.45       8.00        1
    ## 58                          3.40      12.40        0
    ## 59                          3.25       5.30        1
    ## 60                          3.20       9.30        0
    ## 61                          3.30       9.30        0
    ## 62                          2.91       5.30        0
    ## 63                          3.12       6.70        0
    ## 64                          3.33       6.00        0
    ## 65                          3.21       8.60        0
    ## 66                          3.35       9.30        0
    ## 67                          3.15       6.70        0
    ## 68                          3.45       8.90        0
    ## 69                          3.27       6.70        0
    ## 70                          3.00       6.70        0
    ## 71                          3.10       8.00        0
    ## 72                          3.36       6.70        0
    ## 73                          3.59      12.90        0
    ## 74                          3.13       8.00        0
    ## 75                          3.23       8.00        0
    ## 76                          3.60      15.20        0
    ## 77                          3.54      12.90        0
    ## 78                          3.20       5.50        0
    ## 79                          3.53      16.50        0
    ## 80                          3.47      13.60        0
    ## 81                          3.56      21.00        0
    ## 82                          3.46       8.00        0
    ## 83                          3.12       4.00        0
    ## 84                          3.31      12.90        0
    ## 85                          3.36       3.90        0
    ## 86                          3.09      11.70        0
    ## 87                          3.16       8.00        0
    ## 88                          3.19       8.00        1
    ## 89                          2.92       8.00        0
    ## 90                          3.17       5.50        0
    ## 91                          3.10       6.70        0
    ## 92                          3.21       4.00        0
    ## 93                          3.29       5.30        0
    ## 94                          3.35      10.50        0
    ## 95                          3.50      12.90        0
    ## 96                          3.19       7.30        0
    ## 97                          3.15       5.30        0
    ## 98                          3.30      11.70        0
    ## 99                          3.31       6.00        0
    ## 100                         3.15       6.70        0
    ## 101                         3.10       6.00        0
    ## 102                         3.59      19.10        0
    ## 103                         3.12       9.90        0
    ## 104                         3.40       6.70        0
    ## 105                         3.21       6.70        0
    ## 106                         3.12       8.00        0
    ## 107                         3.10       6.70        0
    ## 108                         3.45      13.60        0
    ## 109                         3.15       6.70        0
    ## 110                         3.08       5.30        0
    ## 111                         3.43      11.70        0
    ## 112                         3.50       8.00        0
    ## 113                         3.70      11.70        0
    ## 114                         3.27      12.40        0
    ## 115                         3.34      12.40        0
    ## 116                         3.19       8.00        0
    ## 117                         3.12       5.30        0
    ## 118                         3.15       6.00        0
    ## 119                         3.22       6.70        0
    ## 120                         3.20       6.70        0
    ## 121                         3.47      13.60        0
    ## 122                         3.55      10.50        0
    ## 123                         3.41       6.70        0
    ## 124                         3.22       4.00        0
    ## 125                         3.23       7.30        0
    ## 126                         3.20       8.00        0
    ## 127                         3.20      13.60        0
    ## 128                         3.25       6.70        0
    ## 129                         3.46      11.20        0
    ## 130                         3.19       4.00        0
    ## 131                         3.47       9.30        0
    ## 132                         3.48       9.30        0
    ## 133                         3.30      12.40        0
    ## 134                         3.21       9.30        0
    ## 135                         3.19       6.80        1
    ## 136                         3.33      10.50        1
    ## 137                         3.29       7.30        1
    ## 138                         3.28       5.70        1
    ## 139                         3.31       6.50        1
    ## 140                         3.21       7.10        1
    ## 141                         3.29       7.10        1
    ## 142                         3.31       6.65        1
    ## 143                         3.63      12.90        1
    ## 144                         3.10       6.70        1
    ## 145                         3.29       8.00        1
    ## 146                         3.20       8.00        1
    ## 147                         3.37       6.70        1
    ## 148                         3.33       9.30        1
    ## 149                         3.29       7.30        1
    ## 150                         3.18       7.30        1
    ## 151                         3.39      13.60        1
    ## 152                         3.25       8.00        1
    ## 153                         3.30      10.50        1
    ## 154                         3.19       7.30        1
    ## 155                         3.31       6.00        1
    ## 156                         3.34       6.70        1
    ## 157                         3.35       9.30        1
    ## 158                         3.26      14.80        1
    ## 159                         3.19       6.70        1
    ## 160                         3.27       8.00        1
    ## 161                         3.16       6.70        1
    ## 162                         3.39       6.80        1
    ## 163                         3.21       8.30        1
    ## 164                         3.03       6.70        1
    ## 165                         3.32       7.30        1
    ## 166                         3.31       8.90        1
    ## 167                         3.04       6.60        1
    ## 168                         3.22       8.00        1
    ## 169                         3.20       4.80        1
    ## 170                         3.40       9.30        1
    ## 171                         3.62       9.30        1
    ## 172                         3.33       8.00        1
    ## 173                         3.13       5.30        1
    ## 174                         3.28       9.30        1
    ## 175                         3.24       7.30        1
    ## 176                         3.29       9.90        1
    ## 177                         3.36       9.90        1
    ## 178                         3.24       6.70        1
    ## 179                         3.22       6.70        1
    ## 180                         3.22       6.70        1
    ## 181                         3.10       8.00        1
    ## 182                         3.16       6.70        1
    ## 183                         3.34      10.50        1
    ## 184                         3.20       7.30        1
    ## 185                         3.26       8.30        1
    ## 186                         3.27       6.70        1
    ## 187                         3.33       6.70        1
    ## 188                         3.43       6.70        1
    ## 189                         3.22       6.70        1
    ## 190                         3.29       8.00        1
    ## 191                         3.25       6.00        1
    ## 192                         3.38       6.70        1
    ## 193                         3.32       7.10        1
    ## 194                         3.24       9.30        1
    ## 195                         3.09       6.50        1
    ## 196                         3.52       8.60        1
    ## 197                         3.25       9.60        1
    ## 198                         3.31       5.70        1
    ## 199                         3.30       8.60        1
    ## 200                         3.22       8.60        1
    ## 201                         3.16       8.00        1
    ## 202                         3.29       6.20        1
    ## 203                         3.09       6.70        1
    ## 204                         3.24       5.30        1
    ## 205                         3.23       6.20        1
    ## 206                         3.32       6.00        1
    ## 207                         3.30      10.50        0
    ## 208                         3.40       9.90        0
    ## 209                         3.15       8.00        0
    ## 210                         3.25       9.90        0
    ## 211                         3.06       5.30        0
    ## 212                         3.38      11.70        0
    ## 213                         3.25       8.00        0
    ## 214                         3.32      13.60        0
    ## 215                         3.30       8.00        0
    ## 216                         3.07       7.30        0
    ## 217                         3.20       9.90        0
    ## 218                         3.35       6.70        0
    ## 219                         3.28      17.00        0
    ## 220                         3.45      10.50        0
    ## 221                         3.14      12.90        0
    ## 222                         3.25       4.00        0
    ## 223                         3.65      16.30        0
    ## 224                         3.27       6.00        0
    ## 225                         3.23       6.70        0
    ## 226                         3.32       8.00        0
    ## 227                         3.16      14.10        0
    ## 228                         3.31      11.70        0
    ## 229                         3.20       6.70        0
    ## 230                         3.17       9.30        0
    ## 231                         3.19       8.00        0
    ## 232                         3.18       8.00        0
    ## 233                         3.18       6.00        0
    ## 234                         3.32      10.50        0
    ## 235                         3.15       4.00        0
    ## 236                         3.10       5.30        0
    ## 237                         3.24       6.00        0
    ## 238                         3.09       6.00        0
    ## 239                         3.21       8.00        0
    ## 240                         3.40       9.30        0
    ## 241                         3.13      10.50        0
    ## 242                         3.25      15.90        0
    ## 243                         3.08       7.30        0
    ## 244                         3.11       2.60        0
    ## 245                         3.19       6.70        0
    ## 246                         3.37      16.30        0
    ## 247                         3.38      11.70        0
    ## 248                         3.30       8.60        0
    ## 249                         3.18      12.90        0
    ## 250                         3.47       6.70        0
    ## 251                         3.55      21.40        0
    ## 252                         3.20       7.30        0
    ## 253                         3.22      14.80        0
    ## 254                         3.42      13.60        0
    ## 255                         3.36       8.00        0
    ## 256                         3.23       5.30        0
    ## 257                         3.25       6.00        0
    ## 258                         3.20       4.30        0
    ## 259                         3.42       9.30        1
    ## 260                         3.27      12.40        0
    ## 261                         3.04       8.00        0
    ## 262                         3.80      16.30        0
    ## 263                         3.20       8.00        0
    ## 264                         3.25       7.30        0
    ## 265                         3.28       5.30        0
    ## 266                         3.20      11.70        0
    ## 267                         3.43      16.30        0
    ## 268                         3.10       9.30        0
    ## 269                         3.14       6.70        0
    ## 270                         3.50      17.40        0
    ## 271                         3.28      14.10        0
    ## 272                         3.08       6.70        0
    ## 273                         3.24       4.60        0
    ## 274                         3.43       8.00        0
    ## 275                         3.30      12.40        0
    ## 276                         3.17       7.30        0
    ## 277                         3.10       5.30        0
    ## 278                         3.19       6.70        0
    ## 279                         3.29       8.00        0
    ## 280                         3.08       6.70        0
    ## 281                         3.24       9.30        0
    ## 282                         3.25      11.20        0
    ## 283                         3.21       2.60        0
    ## 284                         3.22       6.00        0
    ## 285                         3.40      16.30        0
    ## 286                         3.27       9.30        0
    ## 287                         3.62      14.80        0
    ## 288                         3.38       9.30        0
    ## 289                         3.12       6.70        0
    ## 290                         3.19      18.50        0
    ## 291                         3.34       6.70        0
    ## 292                         3.34      13.70        0
    ## 293                         3.14       6.70        0
    ## 294                         3.20       5.30        0
    ## 295                         3.10       6.00        0
    ## 296                         3.25      10.80        0
    ## 297                         3.36      14.80        0
    ## 298                         3.24       8.00        1
    ## 299                         3.19       5.30        0
    ## 300                         3.19       5.30        0
    ## 301                         3.14      10.50        0
    ## 302                         3.28      11.70        0
    ## 303                         3.20       6.70        0
    ## 304                         3.18       6.70        0
    ## 305                         3.41      10.50        0
    ## 306                         3.24       8.00        0
    ## 307                         3.29      10.50        0
    ## 308                         3.38      16.30        0
    ## 309                         3.28       6.70        0
    ## 310                         3.23       6.70        0
    ## 311                         3.23       9.90        0
    ## 312                         3.13       6.70        0
    ## 313                         3.33      10.50        0
    ## 314                         3.39      12.40        0
    ## 315                         3.10       8.00        0
    ## 316                         3.34      14.80        0
    ## 317                         3.21       8.00        0
    ## 318                         3.22       8.60        0
    ## 319                         3.14       8.00        0
    ## 320                         3.32      12.90        0
    ## 321                         3.15       5.30        0
    ## 322                         3.27       6.70        0
    ## 323                         3.40      12.30        0
    ## 324                         3.56      12.90        0
    ## 325                         3.17       5.30        0
    ## 326                         3.24       8.60        0
    ## 327                         3.25      14.10        0
    ## 328                         3.60       6.70        0
    ## 329                         3.56      12.90        0
    ## 330                         3.30       9.30        0
    ## 331                         3.17       6.00        0
    ## 332                         2.96       6.70        0
    ## 333                         3.37       6.70        0
    ## 334                         3.18       6.00        0
    ## 335                         3.39       9.30        0
    ## 336                         3.24       6.70        0
    ## 337                         3.21       6.70        0
    ## 338                         3.13       9.30        0
    ## 339                         3.27       9.30        0
    ## 340                         3.38       5.30        0
    ## 341                         3.63      15.20        0
    ## 342                         3.23       9.30        0
    ## 343                         3.25       6.70        0
    ## 344                         3.15       6.70        0
    ## 345                         3.15       7.30        0
    ## 346                         3.42       9.30        0
    ## 347                         3.35      12.40        0
    ## 348                         3.28       8.00        0
    ## 349                         3.35       8.00        1
    ## 350                         3.35       9.20        1
    ## 351                         3.27       6.00        1
    ## 352                         3.42      10.20        1
    ## 353                         3.35       6.90        1
    ## 354                         3.21       8.00        1
    ## 355                         3.29       6.70        1
    ## 356                         3.51      10.20        1
    ## 357                         3.00       4.80        1
    ## 358                         3.28       8.35        1
    ## 359                         3.44       7.95        1
    ## 360                         3.26       7.00        1
    ## 361                         3.45      12.40        1
    ## 362                         3.24       7.30        1
    ## 363                         3.31       7.30        1
    ## 364                         3.06       6.70        1
    ## 365                         3.33       4.50        1
    ## 366                         3.50       6.90        1
    ## 367                         3.28       9.70        1
    ## 368                         3.20       7.10        1
    ## 369                         3.38       8.50        1
    ## 370                         3.27       7.05        1
    ## 371                         3.33       5.10        1
    ## 372                         3.15       8.00        1
    ## 373                         3.33       8.00        1
    ## 374                         3.05       6.30        1
    ## 375                         3.34       7.60        1
    ## 376                         3.21       6.00        1
    ## 377                         3.34       6.20        1
    ## 378                         3.21       9.30        1
    ## 379                         3.18       7.00        1
    ## 380                         3.56       9.30        1
    ## 381                         3.24       8.00        1
    ## 382                         3.25       8.00        1
    ## 383                         3.38       6.20        1
    ## 384                         3.68      10.20        1
    ## 385                         3.38       9.30        1
    ## 386                         3.23       6.20        1
    ## 387                         3.19       6.20        1
    ## 388                         3.14       7.30        1
    ## 389                         3.21       6.00        1
    ## 390                         3.29       6.20        1
    ## 391                         3.11       5.30        1
    ## 392                         3.27      10.80        1
    ## 393                         3.24       7.80        1
    ## 394                         3.22       6.10        1
    ## 395                         3.42      11.30        1
    ## 396                         3.18       6.30        1
    ## 397                         3.48       7.80        1
    ## 398                         3.22       5.90        1
    ## 399                         3.21       8.00        1
    ## 400                         3.29       9.70        1
    ## 401                         3.19       5.90        1
    ## 402                         3.31       4.90        1
    ## 403                         3.47      12.10        1
    ## 404                         3.35      15.80        1
    ## 405                         3.18       5.60        1
    ## 406                         3.44      10.20        1
    ## 407                         3.27      10.50        1
    ## 408                         3.33       6.10        1
    ## 409                         3.27       6.00        1
    ## 410                         3.23       7.50        1
    ## 411                         3.30       7.40        1
    ## 412                         3.39      10.50        1
    ## 413                         3.25       8.60        1
    ## 414                         3.25       8.00        1
    ## 415                         3.23       5.30        1
    ## 416                         3.17       6.20        1
    ## 417                         3.27       6.90        1
    ## 418                         3.41       6.10        1
    ## 419                         3.47      10.50        0
    ## 420                         3.22       8.00        0
    ## 421                         3.43       8.70        0
    ## 422                         3.19       6.90        0
    ## 423                         3.20       6.80        0
    ## 424                         3.48      17.30        0
    ## 425                         3.53       6.30        0
    ## 426                         3.25       5.50        0
    ## 427                         3.22       4.90        0
    ## 428                         3.38      12.00        0
    ## 429                         3.18       7.20        0
    ## 430                         3.31       8.40        0
    ## 431                         3.14       5.00        0
    ## 432                         3.25       9.00        0
    ## 433                         3.54      10.50        0
    ## 434                         3.21       9.60        0
    ## 435                         3.06       5.30        0
    ## 436                         3.11      11.60        0
    ## 437                         3.27      10.00        0
    ## 438                         3.20       3.90        0
    ## 439                         3.50      15.80        0
    ## 440                         3.34       7.30        0
    ## 441                         3.17       4.40        0
    ## 442                         3.23      10.00        0
    ## 443                         3.51      12.40        0
    ## 444                         3.32       5.70        0
    ## 445                         3.25       6.00        0
    ## 446                         3.45       6.80        0
    ## 447                         3.46      10.40        0
    ## 448                         3.46       9.80        0
    ## 449                         3.34       7.10        0
    ## 450                         3.24       7.30        0
    ## 451                         3.16       8.00        0
    ## 452                         3.30       7.40        0
    ## 453                         3.28       8.00        0
    ## 454                         3.24       5.50        0
    ## 455                         3.34      19.00        0
    ## 456                         3.30       4.10        0
    ## 457                         3.38       8.00        0
    ## 458                         3.34       9.60        0
    ## 459                         3.40      12.80        0
    ## 460                         3.29       5.80        0
    ## 461                         3.66      13.80        0
    ## 462                         3.30       9.70        0
    ## 463                         3.16       4.50        0
    ## 464                         3.37       6.80        0
    ## 465                         3.65      17.20        0
    ## 466                         3.07       6.10        0
    ## 467                         3.37       8.10        0
    ## 468                         3.63      10.00        0
    ## 469                         3.08       4.60        0
    ## 470                         3.25       5.90        0
    ## 471                         3.20       9.80        0
    ## 472                         3.29       7.00        0
    ## 473                         3.40       6.90        0
    ## 474                         3.27       7.60        0
    ## 475                         3.43      11.30        0
    ## 476                         3.38       6.60        0
    ## 477                         3.31       5.60        0
    ## 478                         3.40       8.50        0
    ## 479                         3.40      12.20        0
    ## 480                         3.40       7.80        0
    ## 481                         3.51       7.90        0
    ## 482                         3.38      12.80        0
    ## 483                         3.27       6.00        0
    ## 484                         3.20       5.60        0
    ## 485                         3.31       7.00        0
    ## 486                         3.72      14.80        0
    ## 487                         3.26       9.70        0
    ## 488                         3.15       5.20        0
    ## 489                         3.31       6.50        0
    ## 490                         3.59      11.40        0
    ## 491                         3.30       8.00        0
    ## 492                         3.56      13.00        0
    ## 493                         3.40      12.40        0
    ## 494                         3.10       5.00        0
    ## 495                         3.37       8.90        0
    ## 496                         3.45       9.20        0
    ## 497                         3.54      12.10        0
    ## 498                         3.22       4.20        0
    ## 499                         3.20       2.70        0
    ## 500                         3.25       6.60        0
    ## 501                         3.29       6.00        0
    ## 502                         3.27       6.20        1
    ## 503                         3.09       4.20        0
    ## 504                         3.22       7.00        0
    ## 505                         3.37       8.00        0
    ## 506                         3.32       4.50        0
    ## 507                         3.53       6.90        0
    ## 508                         3.29       5.10        0
    ## 509                         3.45       7.10        0
    ## 510                         3.45       6.60        0
    ## 511                         3.41      11.50        0
    ## 512                         3.70      10.80        0
    ## 513                         3.34       3.70        0
    ## 514                         3.26       4.60        0
    ## 515                         3.46      10.90        0
    ## 516                         3.51      13.00        0
    ## 517                         3.28       9.00        0
    ## 518                         3.39       5.60        0
    ## 519                         3.37       9.10        0
    ## 520                         3.32       5.70        0
    ## 521                         3.37       3.50        0
    ## 522                         3.29       5.20        0
    ## 523                         3.28       3.20        0
    ## 524                         3.11       4.80        0
    ## 525                         3.31       5.40        0
    ## 526                         3.29       5.90        0
    ## 527                         3.39       7.00        0
    ## 528                         3.41       5.40        0
    ## 529                         3.42       7.70        0
    ## 530                         3.29       7.50        0
    ## 531                         3.22       4.90        0
    ## 532                         3.22       5.60        0
    ## 533                         3.43      11.40        0
    ## 534                         3.22       4.50        0
    ## 535                         3.43       5.60        0
    ## 536                         3.25       4.70        0
    ## 537                         3.47      13.70        0
    ## 538                         3.49       8.30        0
    ## 539                         3.29       8.00        0
    ## 540                         3.38       6.20        0
    ## 541                         3.47       4.80        0
    ## 542                         3.27       7.80        0
    ## 543                         3.60       9.80        0
    ## 544                         3.24       8.20        0
    ## 545                         3.29       7.50        0
    ## 546                         3.12       6.60        0
    ## 547                         3.36      10.00        0
    ## 548                         3.39       6.10        0
    ## 549                         3.27       6.20        0
    ## 550                         3.27       4.10        0
    ## 551                         3.29       7.50        0
    ## 552                         3.14       4.50        0
    ## 553                         3.11       6.10        0
    ## 554                         3.22       5.30        0
    ## 555                         3.45       5.10        0
    ## 556                         3.22       5.00        0
    ## 557                         3.26       8.90        0
    ## 558                         3.16       5.80        0
    ## 559                         3.29       5.70        0
    ## 560                         3.52      12.10        0
    ## 561                         3.39      10.20        0
    ## 562                         3.56       6.90        0
    ## 563                         3.47       6.70        0
    ## 564                         3.23       5.40        0
    ## 565                         3.36       6.10        0
    ## 566                         3.31       5.80        0
    ## 567                         3.23       3.30        0
    ## 568                         3.19       4.90        0
    ## 569                         3.25       6.80        0
    ## 570                         2.99       5.80        0
    ## 571                         3.14       7.30        0
    ## 572                         3.07       8.10        0
    ## 573                         3.07       4.70        0
    ## 574                         3.24       7.70        0
    ## 575                         3.24       5.70        0
    ## 576                         3.25       8.10        0
    ## 577                         3.25       9.20        0
    ## 578                         3.28       4.00        0
    ## 579                         3.19       5.60        0
    ## 580                         3.52      10.80        0
    ## 581                         3.22       7.60        0
    ## 582                         3.56      16.10        0
    ## 583                         3.20       9.00        0
    ## 584                         3.26       5.30        0
    ## 585                         3.26       5.40        0
    ## 586                         3.17       4.50        0
    ## 587                         3.43       9.40        0
    ## 588                         3.25       6.50        0
    ## 589                         3.24       6.30        0
    ## 590                         3.14       4.80        0
    ## 591                         3.33       6.90        0
    ## 592                         3.01       8.20        0
    ## 593                         3.32      17.40        0
    ## 594                         3.20       7.60        0
    ## 595                         3.08       6.90        0
    ## 596                         3.33       5.80        0
    ## 597                         3.27       9.20        0
    ## 598                         3.07       3.00        0
    ## 599                         3.40       3.60        0
    ## 600                         3.54       8.80        0
    ## 601                         3.16       3.40        0
    ## 602                         3.18       6.40        0
    ## 603                         3.26      12.50        0
    ## 604                         3.36      12.00        0
    ## 605                         3.33      12.20        0
    ## 606                         3.27       7.50        0
    ## 607                         3.20       9.60        0
    ## 608                         3.39       9.00        0
    ## 609                         3.36       8.90        0
    ## 610                         3.57       6.30        0
    ## 611                         3.22      12.90        0
    ## 612                         3.35      10.10        0
    ## 613                         3.14       6.30        0
    ## 614                         3.19       8.80        0
    ## 615                         3.17       7.00        0
    ## 616                         3.25       5.30        0
    ## 617                         3.20       6.20        0
    ## 618                         3.13       4.50        0
    ## 619                         3.45       9.00        0
    ## 620                         3.07       7.60        0
    ## 621                         3.17      11.00        0
    ## 622                         3.14       6.50        0
    ## 623                         3.40      13.80        0
    ## 624                         3.26       4.70        0
    ## 625                         3.13      10.70        0
    ## 626                         3.05       3.60        0
    ## 627                         3.15       7.40        0
    ## 628                         3.05       4.60        0
    ## 629                         3.26       8.10        0
    ## 630                         3.19       8.30        0
    ## 631                         3.08       3.80        0
    ## 632                         3.14       6.70        0
    ## 633                         3.40       6.20        0
    ## 634                         3.09       7.20        0
    ## 635                         2.96       4.40        0
    ## 636                         3.08       4.80        0
    ## 637                         3.07       5.30        0
    ## 638                         3.32       7.90        0
    ## 639                         3.14       5.50        0
    ## 640                         3.15       7.90        0
    ## 641                         3.45      12.00        0
    ## 642                         3.13       8.60        0
    ## 643                         3.24      10.00        0
    ## 644                         3.28       8.60        0
    ## 645                         3.21       7.10        0
    ## 646                         3.35       5.20        0
    ## 647                         3.10       5.70        0
    ## 648                         3.26       4.80        0
    ## 649                         3.28       5.70        0
    ## 650                         3.55       5.50        0
    ## 651                         3.31       4.90        0
    ## 652                         3.03       5.20        0
    ## 653                         3.20       5.50        0
    ## 654                         3.17       7.10        0
    ## 655                         3.23       6.30        0
    ## 656                         3.41       6.60        0
    ## 657                         3.30       5.60        0
    ## 658                         3.28       8.20        0
    ## 659                         3.27       8.50        0
    ## 660                         3.13      10.10        0
    ## 661                         3.14       4.40        0
    ## 662                         3.38      10.80        0
    ## 663                         3.30       6.00        0
    ## 664                         3.21       6.30        0
    ## 665                         3.30      10.10        0
    ## 666                         3.23      12.00        0
    ## 667                         3.12       6.60        0
    ## 668                         3.19       5.40        0
    ## 669                         3.21       6.20        0
    ## 670                         3.25       5.40        0
    ## 671                         3.10       8.00        0
    ## 672                         3.10       6.90        0
    ## 673                         3.45       8.50        0
    ## 674                         3.22      12.40        0
    ## 675                         3.24       7.00        0
    ## 676                         3.17       6.60        0
    ## 677                         3.32       5.80        0
    ## 678                         3.18       4.50        0
    ## 679                         3.28       4.80        0
    ## 680                         3.18       5.10        0
    ## 681                         3.45       4.90        0
    ## 682                         3.68       5.00        0
    ## 683                         3.35       8.50        0
    ## 684                         3.30       8.20        0
    ## 685                         3.19       7.90        0
    ## 686                         3.40       5.60        0
    ## 687                         3.10       7.30        0
    ## 688                         3.39       7.90        0
    ## 689                         3.14       4.00        0
    ## 690                         3.61      12.80        0
    ## 691                         3.24       9.20        0
    ## 692                         3.55      16.40        0
    ## 693                         3.37       4.90        0
    ## 694                         3.21      10.00        0
    ## 695                         3.44       7.90        0
    ## 696                         3.25       6.50        0
    ## 697                         3.70      15.10        0
    ## 698                         3.23       7.00        0
    ## 699                         3.41      11.10        0
    ## 700                         3.22       6.90        0
    ## 701                         3.31       7.50        0
    ## 702                         3.18       4.70        0
    ## 703                         3.25       6.20        0
    ## 704                         3.20       9.60        0
    ## 705                         3.19       5.60        0
    ## 706                         3.14       4.60        0
    ## 707                         3.31       5.90        0
    ## 708                         3.36      11.60        0
    ## 709                         3.24       6.30        0
    ## 710                         3.35      11.20        0
    ## 711                         3.39      13.80        0
    ## 712                         3.33       6.00        0
    ## 713                         3.25       5.30        0
    ## 714                         3.49       6.20        0
    ## 715                         3.37       8.00        0
    ## 716                         3.23       8.30        1
    ## 717                         3.27       4.60        0
    ## 718                         3.38       9.30        0
    ## 719                         3.17       7.00        0
    ## 720                         3.27       8.60        0
    ## 721                         3.29       8.00        0
    ## 722                         3.37       9.50        0
    ## 723                         3.14       5.60        0
    ## 724                         3.50       7.60        0
    ## 725                         3.15       3.70        0
    ## 726                         3.54       5.50        0
    ## 727                         3.35       6.10        0
    ## 728                         3.09       5.20        0
    ## 729                         3.17       5.10        0
    ## 730                         3.15       5.40        0
    ## 731                         3.22       4.70        0
    ## 732                         3.37       8.50        0
    ## 733                         3.27       8.00        0
    ## 734                         3.27       5.50        0
    ## 735                         3.26       6.30        0
    ## 736                         3.21       7.70        0
    ## 737                         3.10       9.90        0
    ## 738                         3.30       9.80        0
    ## 739                         3.30       4.00        0
    ## 740                         3.17       8.40        0
    ## 741                         3.07       5.30        0
    ## 742                         3.18       4.80        0
    ## 743                         3.26       8.30        0
    ## 744                         3.18       4.30        0
    ## 745                         3.14       7.50        0
    ## 746                         3.27       9.70        0
    ## 747                         3.26       5.90        0
    ## 748                         3.07       4.20        0
    ## 749                         3.09       5.90        0
    ## 750                         3.15       5.40        0
    ## 751                         3.13       8.10        0
    ## 752                         3.36       5.40        0
    ## 753                         3.33       5.00        1
    ## 754                         3.25       6.50        0
    ## 755                         3.18       7.10        0
    ## 756                         3.19       6.10        0
    ## 757                         3.31       7.60        0
    ## 758                         3.40      10.20        1
    ## 759                         3.15       8.00        1
    ## 760                         3.20       5.30        0
    ## 761                         3.29       7.00        0
    ## 762                         3.02       5.50        0
    ## 763                         3.20       7.90        0
    ## 764                         3.18       6.50        0
    ## 765                         3.21      10.00        0
    ## 766                         3.09       7.40        0
    ## 767                         3.46       9.80        0
    ## 768                         3.36       7.40        0
    ## 769                         3.17       5.60        0
    ## 770                         3.29       5.90        0
    ## 771                         3.14       6.70        0
    ## 772                         3.41      15.50        0
    ## 773                         3.24       8.00        0
    ## 774                         3.26       6.20        0
    ## 775                         3.29      13.40        0
    ## 776                         3.27       6.10        0
    ## 777                         3.16       5.90        0
    ## 778                         3.23      10.80        0
    ## 779                         3.45      12.10        0
    ## 780                         3.28       7.10        0
    ## 781                         3.16       9.60        0
    ## 782                         3.60       4.10        0
    ## 783                         3.28       6.00        0
    ## 784                         3.47      12.50        0
    ## 785                         3.43      11.30        0
    ## 786                         3.37       9.00        0
    ## 787                         3.19       6.60        0
    ## 788                         3.39       7.50        0
    ## 789                         3.33       4.30        0
    ## 790                         3.29       5.50        0
    ## 791                         3.49       9.10        0
    ## 792                         3.38       9.80        0
    ## 793                         3.40      11.30        0
    ## 794                         3.25       5.30        0
    ## 795                         3.42       8.50        0
    ## 796                         3.41       9.40        0
    ## 797                         3.40       5.00        0
    ## 798                         3.37       7.10        0
    ## 799                         3.25       7.00        0
    ## 800                         3.21       7.00        0
    ## 801                         3.40       7.70        0
    ## 802                         3.19       4.60        0
    ## 803                         3.60      11.20        0
    ## 804                         3.46       8.60        1
    ## 805                         3.18       7.00        0
    ## 806                         3.25       4.50        0
    ## 807                         3.41       6.70        0
    ## 808                         3.41       5.70        0
    ## 809                         3.34       5.90        0
    ## 810                         3.30       5.80        0
    ## 811                         3.23       7.80        0
    ## 812                         3.44       9.20        0
    ## 813                         3.47       7.80        0
    ## 814                         3.31       7.50        0
    ## 815                         3.48       7.80        0
    ## 816                         3.47       5.70        0
    ## 817                         3.22       7.70        0
    ## 818                         3.42       5.80        0
    ## 819                         3.67       7.60        0
    ## 820                         3.53       8.00        0
    ## 821                         3.36      11.40        0
    ## 822                         3.32       6.20        0
    ## 823                         3.17       5.00        0
    ## 824                         3.28       3.20        0
    ## 825                         3.44      10.60        0
    ## 826                         3.44       7.70        0
    ## 827                         3.81      10.70        0
    ## 828                         3.29       4.20        0
    ## 829                         3.20       4.20        0
    ## 830                         3.19       5.50        0
    ## 831                         3.19       6.30        0
    ## 832                         3.38       6.00        0
    ## 833                         3.24       5.20        0
    ## 834                         3.40       6.40        0
    ## 835                         3.40       6.65        0
    ## 836                         3.14       4.60        0
    ## 837                         3.40       3.95        0
    ## 838                         3.31       6.95        0
    ## 839                         3.16       6.90        0
    ## 840                         3.12       6.45        0
    ## 841                         3.22       5.90        0
    ## 842                         3.35       5.80        0
    ## 843                         3.22       4.40        0
    ## 844                         3.38       6.85        0
    ## 845                         3.34       4.70        0
    ## 846                         3.32       4.65        0
    ## 847                         3.50       8.60        0
    ## 848                         3.40       8.35        0
    ## 849                         3.40       4.50        0
    ## 850                         3.25       3.30        0
    ## 851                         3.57       4.40        0
    ## 852                         3.27       4.55        0
    ## 853                         3.25       6.05        0
    ## 854                         3.19       7.60        0
    ## 855                         3.52       9.60        0
    ## 856                         3.51      10.05        0
    ## 857                         3.44       6.00        0
    ## 858                         3.44       7.45        0
    ## 859                         3.08       3.80        0
    ## 860                         3.32      10.25        0
    ## 861                         3.10       7.95        0
    ## 862                         3.32       5.90        0
    ## 863                         3.25       7.30        0
    ## 864                         3.32       8.60        0
    ## 865                         3.27       5.00        0
    ## 866                         3.52       5.55        0
    ## 867                         3.32       9.00        0
    ## 868                         3.57       7.95        0
    ## 869                         3.44       6.80        0
    ## 870                         3.25       6.55        0
    ## 871                         3.59       6.65        0
    ## 872                         3.37       5.25        0
    ## 873                         3.32       5.15        0
    ## 874                         3.29       6.15        0
    ## 875                         3.40       6.65        0
    ## 876                         3.13       4.70        0
    ## 877                         3.25       8.45        0
    ## 878                         3.34       7.05        0
    ## 879                         3.21       4.10        0
    ## 880                         3.16       6.05        0
    ## 881                         3.25       4.85        0
    ## 882                         3.38       8.05        0
    ## 883                         3.53       9.10        0
    ## 884                         3.32       7.50        0
    ## 885                         3.47       8.65        0
    ## 886                         3.15       4.75        0
    ## 887                         3.50      10.80        0
    ## 888                         3.18       6.05        0
    ## 889                         3.36       7.45        0
    ## 890                         3.37       8.70        0
    ## 891                         3.26       6.00        0
    ## 892                         3.48       7.70        0
    ## 893                         3.50       6.05        0
    ## 894                         3.18       4.20        0
    ## 895                         3.28       7.65        0
    ## 896                         3.32       5.25        0
    ## 897                         3.28       4.30        0
    ## 898                         3.26       6.10        0
    ## 899                         3.44       3.85        0
    ## 900                         3.18       4.45        0
    ## 901                         3.14       6.45        0
    ## 902                         3.30       6.20        0
    ## 903                         3.19       6.40        0
    ## 904                         3.32       5.05        0
    ## 905                         3.27       5.05        0
    ## 906                         3.32       8.20        0
    ## 907                         3.31       6.20        0
    ## 908                         3.38       7.45        0
    ## 909                         3.36       5.25        0
    ## 910                         3.27       6.05        0
    ## 911                         3.48      13.60        0
    ## 912                         3.19       4.70        0
    ## 913                         3.28       6.90        0
    ## 914                         3.28       6.15        0
    ## 915                         3.10       4.45        0
    ## 916                         3.29       7.10        0
    ## 917                         3.39       5.20        0
    ## 918                         3.20       7.35        0
    ## 919                         3.45      10.75        0
    ## 920                         3.45      10.75        0
    ## 921                         3.23       5.65        0
    ## 922                         3.20       6.20        0
    ## 923                         3.27       9.40        0
    ## 924                         3.15       5.05        0
    ## 925                         3.44      16.30        0
    ## 926                         3.26      10.55        0
    ## 927                         3.27       3.50        0
    ## 928                         3.27      12.05        0
    ## 929                         3.26       8.20        0
    ## 930                         3.48       6.40        0
    ## 931                         3.28       7.30        0
    ## 932                         3.26       7.95        0
    ## 933                         3.22       6.50        0
    ## 934                         3.22       7.20        1
    ## 935                         3.43       5.45        0
    ## 936                         3.22       5.00        0
    ## 937                         3.58       8.80        0
    ## 938                         3.26       6.60        0
    ## 939                         3.28       8.30        0
    ## 940                         3.18       6.10        0
    ## 941                         3.47       9.60        0
    ## 942                         3.20      10.20        0
    ## 943                         3.38       6.70        0
    ## 944                         3.20       5.80        0
    ## 945                         3.28       7.50        0
    ## 946                         3.32       7.40        0
    ## 947                         3.30       5.50        0
    ## 948                         3.25       4.30        0
    ## 949                         3.34       5.90        0
    ## 950                         3.27       6.00        1
    ## 951                         3.44       9.20        0
    ## 952                         3.51       5.50        0
    ## 953                         3.12       5.00        0
    ## 954                         3.51       7.20        0
    ## 955                         3.32       8.80        0
    ## 956                         3.26       6.00        0
    ## 957                         3.26       4.60        0
    ## 958                         3.62      14.90        0
    ## 959                         3.32       6.50        0
    ## 960                         3.27      10.10        0
    ## 961                         3.30       7.30        0
    ## 962                         3.28       7.20        0
    ## 963                         3.25       6.40        0
    ## 964                         3.32       5.70        0
    ## 965                         3.28       6.30        0
    ## 966                         3.32       4.90        0
    ## 967                         3.56       8.90        0
    ## 968                         3.20       6.40        0
    ## 969                         3.18       4.00        0
    ## 970                         3.20       4.50        0
    ## 971                         3.15       5.60        0
    ## 972                         3.13       5.00        0
    ## 973                         3.02       5.30        0
    ## 974                         3.27       4.10        0
    ## 975                         3.21       6.20        0
    ## 976                         3.35       5.10        0
    ## 977                         3.20       8.10        0
    ## 978                         3.50      12.00        0
    ## 979                         3.29       6.60        0
    ## 980                         3.45       7.85        0
    ## 981                         3.19       5.20        0
    ## 982                         3.17       7.00        0
    ## 983                         3.30       9.70        0
    ## 984                         3.20       5.25        0
    ## 985                         3.17       5.15        0
    ## 986                         3.31       5.40        0
    ## 987                         3.26       8.50        0
    ## 988                         3.24       4.80        0
    ## 989                         3.26       5.60        0
    ## 990                         3.13       4.60        0
    ## 991                         3.15       5.80        0
    ## 992                         3.27       6.45        0
    ## 993                         3.12       5.40        0
    ## 994                         3.40       5.95        0
    ## 995                         3.20       5.75        0
    ## 996                         3.20      10.90        0
    ## 997                         3.26       7.60        0
    ## 998                         3.25       6.50        0
    ## 999                         3.20       7.50        0
    ## 1000                        3.20       6.55        0
    ## 1001                        3.24       7.10        0
    ## 1002                        3.25       5.35        0
    ## 1003                        3.33       6.05        0
    ## 1004                        3.30       4.70        0
    ## 1005                        3.28       5.60        0
    ## 1006                        3.49      10.05        0
    ## 1007                        3.50       8.55        0
    ## 1008                        3.15       4.30        0
    ## 1009                        3.23       3.95        0
    ## 1010                        3.24       6.45        0
    ## 1011                        3.46       4.85        0
    ## 1012                        3.33       7.00        0
    ## 1013                        3.41       5.20        0
    ## 1014                        3.50      12.85        0
    ## 1015                        3.40      12.55        1
    ## 1016                        3.20       8.35        0
    ## 1017                        3.20       5.20        0
    ## 1018                        3.50       6.00        0
    ## 1019                        3.46       9.20        0
    ## 1020                        3.34       9.25        0
    ## 1021                        3.35      10.00        0
    ## 1022                        3.48      15.25        0
    ## 1023                        3.43      11.15        0
    ## 1024                        3.24       5.20        0
    ## 1025                        3.30      11.00        0
    ## 1026                        3.21       7.40        0
    ## 1027                        3.15       6.40        0
    ## 1028                        3.44       5.40        0
    ## 1029                        3.44      11.90        0
    ## 1030                        3.22       7.90        0
    ## 1031                        3.26       6.80        0
    ## 1032                        3.21       5.40        0
    ## 1033                        3.17       5.70        0
    ## 1034                        3.44      12.00        0
    ## 1035                        3.38      11.00        0
    ## 1036                        3.21       7.50        0
    ## 1037                        3.37       9.40        0
    ## 1038                        3.27       5.40        0
    ## 1039                        3.11       5.20        0
    ## 1040                        3.16       4.70        0
    ## 1041                        3.13       5.30        0
    ## 1042                        3.15       6.10        0
    ## 1043                        3.21       9.10        0
    ## 1044                        3.32       5.80        0
    ## 1045                        3.34       5.20        0
    ## 1046                        3.18       5.80        0
    ## 1047                        3.27      11.50        0
    ## 1048                        3.36       7.70        0
    ## 1049                        3.36       4.00        0
    ## 1050                        3.14       6.70        0
    ## 1051                        3.43       7.10        0
    ## 1052                        3.30       8.10        0
    ## 1053                        3.41      11.00        0
    ## 1054                        3.25       9.40        0
    ## 1055                        3.27       6.80        0
    ## 1056                        3.46      13.60        0
    ## 1057                        3.18       4.30        0
    ## 1058                        3.50      11.50        0
    ## 1059                        3.01       5.90        0
    ## 1060                        3.19       6.90        0
    ## 1061                        3.53      11.40        0
    ## 1062                        3.16       5.50        0
    ## 1063                        3.16       7.70        0
    ## 1064                        3.46       7.80        0
    ## 1065                        3.35       6.80        0
    ## 1066                        3.34       6.50        0
    ## 1067                        3.23       3.20        0
    ## 1068                        3.18       5.00        0
    ## 1069                        3.36       7.80        0
    ## 1070                        3.27      10.60        0
    ## 1071                        3.35       6.70        0
    ## 1072                        3.29       5.80        0
    ## 1073                        3.08       6.20        0
    ## 1074                        3.19       5.60        0
    ## 1075                        3.45       7.60        0
    ## 1076                        3.15       8.90        0
    ## 1077                        3.15       5.55        0
    ## 1078                        3.21       5.00        0
    ## 1079                        3.12       7.95        0
    ## 1080                        3.31      11.65        0
    ## 1081                        3.17       5.35        0
    ## 1082                        3.07       4.00        0
    ## 1083                        3.28       7.60        0
    ## 1084                        3.20       3.75        0
    ## 1085                        3.33       7.05        0
    ## 1086                        3.15       6.70        0
    ## 1087                        3.17       6.50        0
    ## 1088                        3.18       4.15        0
    ## 1089                        3.11       5.00        0
    ## 1090                        3.10       4.45        0
    ## 1091                        3.18       5.20        0
    ## 1092                        3.20       6.30        0
    ## 1093                        3.20       6.50        0
    ## 1094                        3.23       5.25        0
    ## 1095                        3.08       7.90        0
    ## 1096                        3.21       4.80        0
    ## 1097                        3.27       8.45        0
    ## 1098                        3.13       5.00        0
    ## 1099                        3.36       5.15        0
    ## 1100                        3.27       6.40        0
    ## 1101                        3.09       8.60        0
    ## 1102                        3.38       8.10        0
    ## 1103                        3.29       7.25        0
    ## 1104                        3.28       7.35        0
    ## 1105                        3.26       7.00        0
    ## 1106                        3.09       5.45        0
    ## 1107                        3.17       8.10        0
    ## 1108                        3.23       8.70        0
    ## 1109                        3.23       5.20        0
    ## 1110                        3.33       5.05        0
    ## 1111                        3.25       6.80        0
    ## 1112                        3.15       8.45        0
    ## 1113                        3.13       6.25        0
    ## 1114                        3.04       5.20        0
    ## 1115                        3.22       7.35        0
    ## 1116                        3.40      13.85        0
    ## 1117                        3.20       5.45        0
    ## 1118                        3.11       4.85        0
    ## 1119                        3.10       7.70        0
    ## 1120                        3.05       6.05        0
    ## 1121                        3.34       5.55        0
    ## 1122                        3.38      13.75        0
    ## 1123                        3.18       6.40        0
    ## 1124                        3.12       7.00        0
    ## 1125                        3.14       4.95        0
    ## 1126                        3.18       6.75        0
    ## 1127                        3.06       5.65        0
    ## 1128                        3.20       6.85        0
    ## 1129                        3.30       5.00        0
    ## 1130                        3.24       5.15        0
    ## 1131                        3.19       5.95        0
    ## 1132                        3.53       8.50        0
    ## 1133                        3.27       3.60        0
    ## 1134                        3.08       5.00        0
    ## 1135                        3.26       6.50        0
    ## 1136                        3.26       4.90        0
    ## 1137                        3.36       6.00        0
    ## 1138                        3.48       2.90        0
    ## 1139                        3.51       7.80        0
    ## 1140                        3.15       4.90        0
    ## 1141                        3.36       4.50        0
    ## 1142                        3.28       6.10        0
    ## 1143                        3.28       4.10        0
    ## 1144                        3.22       5.00        0
    ## 1145                        3.04       5.90        0
    ## 1146                        3.22       5.50        0
    ## 1147                        3.78       6.80        0
    ## 1148                        3.21       5.40        0
    ## 1149                        3.25       5.90        0
    ## 1150                        3.20       8.40        0
    ## 1151                        3.27       5.70        0
    ## 1152                        3.12       5.10        0
    ## 1153                        3.04       3.90        0
    ## 1154                        3.37       5.00        0
    ## 1155                        3.28       6.10        0
    ## 1156                        3.34       6.70        0
    ## 1157                        3.34       8.40        0
    ## 1158                        3.40       6.50        0
    ## 1159                        3.26       5.80        0
    ## 1160                        3.25       5.10        0
    ## 1161                        3.28       4.80        0
    ## 1162                        3.60       8.90        0
    ## 1163                        3.29       4.00        0
    ## 1164                        3.24       5.90        0
    ## 1165                        3.30       4.00        0
    ## 1166                        3.32       5.00        0
    ## 1167                        3.38       3.80        0
    ## 1168                        3.32       5.70        0
    ## 1169                        3.43       7.50        0
    ## 1170                        3.23       6.10        0
    ## 1171                        3.38       4.60        0
    ## 1172                        3.17       4.90        0
    ## 1173                        3.30       6.30        0
    ## 1174                        3.30       6.70        0
    ## 1175                        3.20       9.80        0
    ## 1176                        3.27       7.50        0
    ## 1177                        3.14       5.10        0
    ## 1178                        3.73       6.30        0
    ## 1179                        3.42      14.00        0
    ## 1180                        3.25       3.90        0
    ## 1181                        3.27       5.60        0
    ## 1182                        3.37       6.80        0
    ## 1183                        3.37       4.70        0
    ## 1184                        3.30       6.20        0
    ## 1185                        3.07       6.60        0
    ## 1186                        3.16       6.10        0
    ## 1187                        3.34       4.90        0
    ## 1188                        3.33       5.40        0
    ## 1189                        3.30       4.60        0
    ## 1190                        3.26      12.30        0
    ## 1191                        3.23       9.61        0
    ## 1192                        3.17       5.00        0
    ## 1193                        3.22       8.20        0
    ## 1194                        3.16       7.83        0
    ## 1195                        3.02       4.60        0
    ## 1196                        3.19       3.80        0
    ## 1197                        3.25       4.20        0
    ## 1198                        3.18       5.65        0
    ## 1199                        3.12       6.80        0
    ## 1200                        3.13       4.10        0
    ## 1201                        3.38       7.60        0
    ## 1202                        3.11       4.40        0
    ## 1203                        3.11       9.91        0
    ## 1204                        3.33       8.90        0
    ## 1205                        3.31       8.20        0
    ## 1206                        3.04       3.73        0
    ## 1207                        3.37       8.70        0
    ## 1208                        3.16       5.90        0
    ## 1209                        3.30       7.40        0
    ## 1210                        3.20       5.90        0
    ## 1211                        3.43      11.68        0
    ## 1212                        3.25       8.40        0
    ## 1213                        3.17       8.20        0
    ## 1214                        3.45       6.83        0
    ## 1215                        3.25       8.50        0
    ## 1216                        3.07       5.40        0
    ## 1217                        3.09       5.80        0
    ## 1218                        3.20       6.80        0
    ## 1219                        3.28       7.60        0
    ## 1220                        3.40      12.43        0
    ## 1221                        3.49       8.50        0
    ## 1222                        3.60      12.50        0
    ## 1223                        3.28       5.30        0
    ## 1224                        3.61       9.00        0
    ## 1225                        3.18       4.50        0
