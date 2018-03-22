Untitled
================

Introduction
------------

Being a movie fanatic, I thought it would be interesting to do a network analysis of actors and directors working together. I particularly wanted to see if any actor-director duo have an increased inclination towards working together. Thanks to Kaggle, where I obtained the IMDB 5000 movie dataset, I can find the answer to my question!

Although the original dataset I obtained has been removed by Kaggle, the original dataset can be found here at Dataworld.

Based on the massive movie information available, I wanted to analyze and visualize the network of actors and directors working together and the number of times they have worked together.

Data Exploration/ Loading/
--------------------------

Loading the packages I would need for my analyses.

``` r
# Loading packages
library(dplyr) # data manipulation
library(tidyr) # data tidying
library(stringr)
library(igraph) 
library(visNetwork) # Interactive visualization
```

Now that the packages are loaded, I read and explored the movie dataset. I decided, it is a good idea to replace all blank cells, if any, with NAs while loading the dataset before proceeding with any further analyses. This is to ensure I can easily exclude fields with NAs during my analyses when needed.

``` r
movie.data = read.csv('movie_metadata.csv', na.strings = c("","NA"), header = T)

# Exploring the structure of the dataset
str(movie.data)
```

    ## 'data.frame':    5043 obs. of  28 variables:
    ##  $ color                    : Factor w/ 2 levels " Black and White",..: 2 2 2 2 NA 2 2 2 2 2 ...
    ##  $ director_name            : Factor w/ 2398 levels "A. Raven Cruz",..: 928 800 2026 379 605 108 2029 1651 1227 553 ...
    ##  $ num_critic_for_reviews   : int  723 302 602 813 NA 462 392 324 635 375 ...
    ##  $ duration                 : int  178 169 148 164 NA 132 156 100 141 153 ...
    ##  $ director_facebook_likes  : int  0 563 0 22000 131 475 0 15 0 282 ...
    ##  $ actor_3_facebook_likes   : int  855 1000 161 23000 NA 530 4000 284 19000 10000 ...
    ##  $ actor_2_name             : Factor w/ 3032 levels "50 Cent","A. Michael Baldwin",..: 1407 2217 2488 533 2432 2548 1227 800 2439 652 ...
    ##  $ actor_1_facebook_likes   : int  1000 40000 11000 27000 131 640 24000 799 26000 25000 ...
    ##  $ gross                    : int  760505847 309404152 200074175 448130642 NA 73058679 336530303 200807262 458991599 301956980 ...
    ##  $ genres                   : Factor w/ 914 levels "Action","Action|Adventure",..: 107 101 128 288 754 126 120 308 126 447 ...
    ##  $ actor_1_name             : Factor w/ 2097 levels "50 Cent","A.J. Buckley",..: 304 982 354 1967 527 442 786 222 337 34 ...
    ##  $ movie_title              : Factor w/ 4917 levels "#HorrorÂ ","[Rec] 2Â ",..: 398 2731 3279 3707 3332 1961 3289 3459 399 1631 ...
    ##  $ num_voted_users          : int  886204 471220 275868 1144337 8 212204 383056 294810 462669 321795 ...
    ##  $ cast_total_facebook_likes: int  4834 48350 11700 106759 143 1873 46055 2036 92000 58753 ...
    ##  $ actor_3_name             : Factor w/ 3521 levels "50 Cent","A.J. Buckley",..: 3441 1394 3133 1770 NA 2713 1969 2162 3017 2940 ...
    ##  $ facenumber_in_poster     : int  0 0 1 0 0 1 0 1 4 3 ...
    ##  $ plot_keywords            : Factor w/ 4760 levels "10 year old|dog|florida|girl|supermarket",..: 1319 4282 2075 3483 NA 650 4744 28 1141 2004 ...
    ##  $ movie_imdb_link          : Factor w/ 4919 levels "http://www.imdb.com/title/tt0006864/?ref_=fn_tt_tt_1",..: 2965 2721 4533 3756 4918 2476 2526 2458 4546 2551 ...
    ##  $ num_user_for_reviews     : int  3054 1238 994 2701 NA 738 1902 387 1117 973 ...
    ##  $ language                 : Factor w/ 47 levels "Aboriginal","Arabic",..: 12 12 12 12 NA 12 12 12 12 12 ...
    ##  $ country                  : Factor w/ 65 levels "Afghanistan",..: 64 64 62 64 NA 64 64 64 64 62 ...
    ##  $ content_rating           : Factor w/ 18 levels "Approved","G",..: 9 9 9 9 NA 9 9 8 9 8 ...
    ##  $ budget                   : num  2.37e+08 3.00e+08 2.45e+08 2.50e+08 NA ...
    ##  $ title_year               : int  2009 2007 2015 2012 NA 2012 2007 2010 2015 2009 ...
    ##  $ actor_2_facebook_likes   : int  936 5000 393 23000 12 632 11000 553 21000 11000 ...
    ##  $ imdb_score               : num  7.9 7.1 6.8 8.5 7.1 6.6 6.2 7.8 7.5 7.5 ...
    ##  $ aspect_ratio             : num  1.78 2.35 2.35 2.35 NA 2.35 2.35 1.85 2.35 2.35 ...
    ##  $ movie_facebook_likes     : int  33000 0 85000 164000 0 24000 0 29000 118000 10000 ...

There are 5043 observations and 28 variables. Since I am interested in working with the director\_name and actor\_name variables, I made sure they are in the format I want them to be. The director and actor variables are recognized as categorical, thus showing as factors. To ensure, I do not face any problems during my analysis, I converted them as characters before proceeding.

``` r
# Converting the variables as a character
movie.data$director_name = as.character(movie.data$director_name)
movie.data$actor_1_name = as.character(movie.data$actor_1_name)
movie.data$actor_2_name = as.character(movie.data$actor_2_name)
movie.data$actor_3_name = as.character(movie.data$actor_3_name)
```

Data Cleaning
-------------

In the dataset,there are some duplicate rows. I removed the duplicate rows and worked with the unique ones.

``` r
# Finding total number of duplicated rows
sum(duplicated(movie.data))
```

    ## [1] 45

``` r
# Removing duplicates from the dataset
movie.data = movie.data[!duplicated(movie.data),]
```

Now there are 4998 unique observations left. To further explore the data, I looked for missing values in each variable. As you would remember, while loading the data, I replaced all blank cells with NA. I used the colSums() function which returns the total number of NAs per column to see where and how much data is missing.

``` r
# Finding total number of NAs per column
colSums(sapply(movie.data, is.na)) 
```

    ##                     color             director_name 
    ##                        19                       103 
    ##    num_critic_for_reviews                  duration 
    ##                        49                        15 
    ##   director_facebook_likes    actor_3_facebook_likes 
    ##                       103                        23 
    ##              actor_2_name    actor_1_facebook_likes 
    ##                        13                         7 
    ##                     gross                    genres 
    ##                       874                         0 
    ##              actor_1_name               movie_title 
    ##                         7                         0 
    ##           num_voted_users cast_total_facebook_likes 
    ##                         0                         0 
    ##              actor_3_name      facenumber_in_poster 
    ##                        23                        13 
    ##             plot_keywords           movie_imdb_link 
    ##                       152                         0 
    ##      num_user_for_reviews                  language 
    ##                        21                        12 
    ##                   country            content_rating 
    ##                         5                       301 
    ##                    budget                title_year 
    ##                       487                       107 
    ##    actor_2_facebook_likes                imdb_score 
    ##                        13                         0 
    ##              aspect_ratio      movie_facebook_likes 
    ##                       327                         0

Since I am doing a network analysis between directors and actors, I am more interested in the director\_name and actors\_x\_name variables. There are NAs in the director\_name and actor\_name column. Now for a proper network analysis, I would need both a director\_name and an actor\_name, since technically, there cannot be a relation with a blank/ a relation with a blank in non-existent, unless it's with a ghost.

I could have simply excluded all cases with NA. However, in doing so, I would lose way more entries, since there might be some directors who might have worked with only 2 actors, and since the actor\_3\_name variable would have NA, it would also be omitted, thus affecting the exact count of director\_name. Therefore, I excluded entries with NA only in the director\_name variable.

``` r
movie.data = movie.data %>% 
  drop_na(director_name) 
```

Data Tidying
------------

All the movie titles seemed to have a special character(Ã) at end. Some also have some whitespaces. So I tidied them up using some regular expressions. I got to admit! Understanding regular expressions almost made me climb up the wall. But I think I've got a hang of it now, although, there's still lots to learn.

``` r
# Replacing Ã and whitespaces with blanks using gsub() functions
movie.data$movie_title= gsub('Ã','',movie.data$movie_title)
movie.data$movie_title= gsub('^\\s*$','',movie.data$movie_title)

# Removing the additional space at the end of the movie title
str_trim(movie.data$movie_title,'right') 
```

Since there are about 5000 unique observations, one could imagine the complexity of the network between 5000 directors and the respective actors they have worked with in their movies. So in order to be able to humanely visualize and decipher the network, I selected only the well rated movies, i.e. having an IMDB\_score of at least 8.

``` r
data = subset(movie.data,imdb_score >= 8)
```

Network Analysis
----------------

After all that data cleaning and tidying up, now this was the fun part!

The main aspect of networks are the numerous separate entities and the relation between them. The entities are called nodes or vertices, and the connections between them are called edges or links.

Nodeslist
=========

The package I used to create the network objects for my network analysis is igraph. To begin with, I created lists of distinct directors and actors as a dataframe, which are going to be my network entities (nodes) in the graph. Since there are 3 columns observing the name of actors in a movie, I created a separate list of distinct actors for each actor\_name column, and then used a full\_join() to obatin a list of all unique actors. I created separate lists for directors and actors as I intendeded to make a two-mode network.

The nodeslists contains a "names" column, containing the name of distinct directors and actors.

``` r
# List of distinct directors (Nodes Set 1)
directors = data %>% 
  distinct(director_name) %>% 
  rename(names = director_name)


# List of distinct actors (Nodes Set 2)
actors1 = data%>%
  distinct(actor_1_name) %>%
  rename(names=actor_1_name)
actors2 = data%>%
  distinct(actor_2_name) %>%
  rename(names=actor_2_name)
actors3 = data%>%
  distinct(actor_3_name) %>%
  rename(names=actor_3_name)

actors = full_join(actors1,actors2, by="names")
actors = full_join(actors,actors3, by="names")
```

I created 2 separate nodeslist for directors and actors, as I wanted to add/will be adding them separately into the function that will create the network objects. By doing so, I will be able to create a boolean "type" attibute that will indicate the difference between the actors and directors, and result in a bipartite graph object. It is worth mentioning, there are a few directors who have worked as an actor in their own movie. So I removed their names from the actors list to avoid duplication of nodes.

``` r
# List of directors who were also actors in their movies
subset(data, director_name==actor_1_name |director_name==actor_2_name | director_name==actor_3_name, 
       select = c(director_name, actor_1_name,actor_2_name,actor_3_name))
```

    ##          director_name      actor_1_name    actor_2_name     actor_3_name
    ## 1605    Clint Eastwood    Clint Eastwood  Morgan Freeman      Mike Colter
    ## 1783    Jacques Perrin    Jacques Perrin  Philippe Labro             <NA>
    ## 1870    Clint Eastwood    Clint Eastwood   Dreama Walker        Ahney Her
    ## 2761    Clint Eastwood    Clint Eastwood  Morgan Freeman   Frances Fisher
    ## 3288     Michael Moore     Michael Moore Tucker Albrizzi     Bill Clinton
    ## 3890       Woody Allen       Woody Allen      Carol Kane   Shelley Duvall
    ## 4055     Michael Moore     Michael Moore      Dick Clark     Bill Clinton
    ## 4285        Ari Folman        Ari Folman     Ronny Dayag   Zahava Solomon
    ## 4497 Quentin Tarantino Quentin Tarantino   Steve Buscemi       Chris Penn
    ## 4938     Bill Melendez     Peter Robbins   Bill Melendez Christopher Shea

``` r
# List of actors after removal of the directors above
actors = subset(actors, names != 'Ari Folman' &
                   names != 'Clint Eastwood' &
                   names != 'Jacques Perrin' &
                   names != 'Michael Moore' &
                   names != 'Quentin Tarantino' &
                   names != 'Woody Allen' &
                   names != 'Bill Melendez')
```

Edgelist
========

After obtaining the lists of distinct actors and directors, I created an edgelist in a similar fashion as creating the nodeslist of actors. Since there are 3 columns for actors, I created an edgelist for each actor column with directors. I also created a weight column that will count the number of times the actor-director duo have worked together. To create a single dataframe with the director-actor relation, I concatenated the 3 lists together using rbind().

``` r
# relation between actor 1 and director
edgelist_1 = data %>%  
  group_by(director_name,actor_1_name, movie_title) %>%
  summarise(weight = n()) %>%
  ungroup() %>%
  rename(actor_name = actor_1_name)

# relation between actor 2 and director
edgelist_2 = data %>%  
  group_by(director_name, actor_2_name, movie_title) %>%
  summarise(weight = n()) %>%
  ungroup() %>%
  rename(actor_name = actor_2_name)

# relation between actor 3 and director
edgelist_3 = data %>%  
  group_by(director_name, actor_3_name, movie_title) %>%
  summarise(weight = n()) %>%
  ungroup() %>%
  rename(actor_name = actor_3_name)

# Merging the two edgelist together to get final edgelist, using rbind() function.
edgelist = rbind(edgelist_1,edgelist_2,edgelist_3)

# Removing links self 
edgelist = subset(edgelist,director_name != actor_name) 
```

Like in the actors nodeslist, I excluded the relations existing within the same director to omit self edges. Currently the edgelist obtained consists of the names of the directors and actors who have worked together, the number of times they worked together and the movie they worked on. To simplify the edgelist obtained, I grouped the directors and actors to obtain a list with the movie titles in the same field and the weight column giving a total count of the movies.

``` r
edgelist = edgelist %>%
  group_by(director_name, actor_name) %>%
  summarise(movie_title= paste0(movie_title, collapse = ","), weight = n())
edgelist_links = select(edgelist, director_name, actor_name)
```

Creating network objects using igraph
=====================================

I used the graph.empty() function to create the network objetcs. Since I was interested in creating a bipartite network, I introduced a boolean "type" attribute in the nodeslist, which distinguishes the nodes for directors and actors.

``` r
# Creating and introducing "type" attibute in the vertices
bg = make_empty_graph()
bg = add_vertices(bg,nv=length(directors$names), attr = list(name=directors$names, 
                                                               type = rep(TRUE, length(directors$names))))
bg = add_vertices(bg,nv=length(actors$names), attr = list(name=actors$names, 
                                                             type= rep(FALSE, length(actors$names))))

# Adding the edges
edgeListVec <- as.vector(t(as.matrix(data.frame(edgelist_links))))
bg <- add.edges(bg,edgeListVec)

# Confirming if the graph object is bipartite
is.bipartite(bg)
```

    ## [1] TRUE

GitHub Documents
----------------

This is an R Markdown format used for publishing markdown documents to GitHub. When you click the **Knit** button all R code chunks are run and a markdown file (.md) suitable for publishing to GitHub is generated.

Including Code
--------------

You can include R code in the document as follows:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

Including Plots
---------------

You can also embed plots, for example:

![](github2_files/figure-markdown_github/pressure-1.png)

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
