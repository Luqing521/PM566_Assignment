Lab 06 - Text Mining
================

# Learning goals

  - Use `unnest_tokens()` and `unnest_ngrams()` to extract tokens and
    ngrams from text.
  - Use dplyr and ggplot2 to analyze text data

# Lab description

For this lab we will be working with a new dataset. The dataset contains
transcription samples from <https://www.mtsamples.com/>. And is loaded
and “fairly” cleaned at
<https://raw.githubusercontent.com/USCbiostats/data-science-data/master/00_mtsamples/mtsamples.csv>.

This markdown document should be rendered using `github_document`
document.

# Setup the Git project and the GitHub repository

1.  Go to your documents (or wherever you are planning to store the
    data) in your computer, and create a folder for this project, for
    example, “PM566-labs”

2.  In that folder, save [this
    template](https://raw.githubusercontent.com/USCbiostats/PM566/master/content/assignment/06-lab.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository, hopefully of
    the same name that this folder has, i.e., “PM566-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

### Setup packages

You should load in `dplyr`, (or `data.table` if you want to work that
way), `ggplot2` and `tidytext`. If you don’t already have `tidytext`
then you can install with

``` r
install.packages("tidytext")
```

### read in Medical Transcriptions

Loading in reference transcription samples from
<https://www.mtsamples.com/>

``` r
library(readr)
library(dplyr)
library(ggplot2)
library(tidytext)
mt_samples <- read_csv("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/00_mtsamples/mtsamples.csv")
mt_samples <- mt_samples %>%
  select(description, medical_specialty, transcription)

head(mt_samples)
```

    ## # A tibble: 6 x 3
    ##   description                  medical_specialty   transcription                
    ##   <chr>                        <chr>               <chr>                        
    ## 1 A 23-year-old white female … Allergy / Immunolo… "SUBJECTIVE:,  This 23-year-…
    ## 2 Consult for laparoscopic ga… Bariatrics          "PAST MEDICAL HISTORY:, He h…
    ## 3 Consult for laparoscopic ga… Bariatrics          "HISTORY OF PRESENT ILLNESS:…
    ## 4 2-D M-Mode. Doppler.         Cardiovascular / P… "2-D M-MODE: , ,1.  Left atr…
    ## 5 2-D Echocardiogram           Cardiovascular / P… "1.  The left ventricular ca…
    ## 6 Morbid obesity.  Laparoscop… Bariatrics          "PREOPERATIVE DIAGNOSIS: , M…

-----

## Question 1: What specialties do we have?

We can use `count()` from `dplyr` to figure out how many different
catagories do we have? Are these catagories related? overlapping? evenly
distributed?

``` r
mt_samples %>%
  count(medical_specialty, sort = TRUE)
```

    ## # A tibble: 40 x 2
    ##    medical_specialty                 n
    ##    <chr>                         <int>
    ##  1 Surgery                        1103
    ##  2 Consult - History and Phy.      516
    ##  3 Cardiovascular / Pulmonary      372
    ##  4 Orthopedic                      355
    ##  5 Radiology                       273
    ##  6 General Medicine                259
    ##  7 Gastroenterology                230
    ##  8 Neurology                       223
    ##  9 SOAP / Chart / Progress Notes   166
    ## 10 Obstetrics / Gynecology         160
    ## # … with 30 more rows

-----

## Question 2

  - Tokenize the the words in the `transcription` column
  - Count the number of times each token appears
  - Visualize the top 20 most frequent words

Explain what we see from this result. Does it makes sense? What insights
(if any) do we get?

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────── tidyverse 1.3.0 ──

    ## ✓ tibble  3.0.3     ✓ stringr 1.4.0
    ## ✓ tidyr   1.1.2     ✓ forcats 0.5.0
    ## ✓ purrr   0.3.4

    ## ── Conflicts ────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
mt_samples %>% 
  unnest_tokens(token,transcription) %>% 
  count(token,sort= TRUE) %>% 
  top_n(20,n) %>% 
  ggplot(aes(n,fct_reorder(token,n)))+
  geom_col()
```

![](06-lab_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

  - The top 20 most frequent words almost are stop words.

-----

## Question 3

  - Redo visualization but remove stopwords before
  - Bonus points if you remove numbers as well

What do we see know that we have removed stop words? Does it give us a
better idea of what the text is about?

``` r
nums <- as.character(seq(0,100))

mt_samples %>% 
  unnest_tokens(word, transcription) %>%
  filter(!(word %in% nums)) %>% 
  anti_join(stop_words, by= c("word")) %>% 
  count(word, sort = TRUE) %>% 
  top_n(20,n) %>% 
  ggplot(aes(n,fct_reorder(word,n)))+
  geom_col()
```

![](06-lab_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

  - ## Patient is the most frequently used word after removing the stop words and numbers.
    
    # Question 4

repeat question 2, but this time tokenize into bi-grams. how does the
result change if you look at tri-grams?

``` r
# bi-grams
mt_samples %>% 
  unnest_ngrams(token,transcription, 2 ) %>% 
  count(token,sort= TRUE) %>% 
  top_n(20,n) %>% 
  ggplot(aes(n,fct_reorder(token,n)))+
  geom_col()
```

![](06-lab_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# tri-grams
mt_samples %>% 
  unnest_ngrams(token,transcription, 3 ) %>% 
  count(token,sort= TRUE) %>% 
  top_n(20,n) %>% 
  ggplot(aes(n,fct_reorder(token,n)))+
  geom_col()
```

![](06-lab_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

  - ## The phrase are more descriptive with tri-grams.

# Question 5

Using the results you got from questions 4. Pick a word and count the
words that appears after and before it.

``` r
library(tidyr)
#after blood
mt_samples %>% 
  unnest_ngrams(ngram,transcription, 2) %>% 
  separate(ngram, into = c("word1", "word2"), sep = " ") %>% 
  select(word1, word2) %>% 
  anti_join(stop_words, by = c("word1" = "word")) %>% 
  anti_join(stop_words, by = c("word2" = "word")) %>% 
  filter(!(word2 %in% nums)) %>% 
  filter(word1 == "blood") %>% 
  count(word2, sort = TRUE)
```

    ## # A tibble: 115 x 2
    ##    word2           n
    ##    <chr>       <int>
    ##  1 pressure     1265
    ##  2 loss          965
    ##  3 cell          130
    ##  4 cells         112
    ##  5 sugar          91
    ##  6 sugars         79
    ##  7 cultures       53
    ##  8 flow           45
    ##  9 transfusion    42
    ## 10 glucose        37
    ## # … with 105 more rows

``` r
#before blood
mt_samples %>% 
  unnest_ngrams(ngram,transcription, 2) %>% 
  separate(ngram, into = c("word1", "word2"), sep = " ") %>% 
  select(word1, word2) %>% 
  anti_join(stop_words, by = c("word1" = "word")) %>% 
  anti_join(stop_words, by = c("word2" = "word")) %>% 
  filter(!(word1 %in% nums)) %>% 
  filter(word2 == "blood") %>% 
  count(word1, sort = TRUE)
```

    ## # A tibble: 317 x 2
    ##    word1         n
    ##    <chr>     <int>
    ##  1 estimated   754
    ##  2 white       180
    ##  3 signs       170
    ##  4 red         123
    ##  5 pounds       48
    ##  6 cold         29
    ##  7 cord         28
    ##  8 fasting      28
    ##  9 elevated     27
    ## 10 patient's    26
    ## # … with 307 more rows

-----

# Question 6

Which words are most used in each of the specialties. you can use
`group_by()` and `top_n()` from `dplyr` to have the calculations be done
within each specialty. Remember to remove stopwords. How about the most
5 used words?

``` r
mt_samples %>% 
  unnest_tokens(token, transcription) %>% 
  anti_join(stop_words, by = c("token" = "word")) %>% 
  group_by(medical_specialty) %>% 
  count(token) %>% 
  top_n(5,n)
```

    ## # A tibble: 209 x 3
    ## # Groups:   medical_specialty [40]
    ##    medical_specialty    token         n
    ##    <chr>                <chr>     <int>
    ##  1 Allergy / Immunology allergies    21
    ##  2 Allergy / Immunology history      38
    ##  3 Allergy / Immunology nasal        13
    ##  4 Allergy / Immunology noted        23
    ##  5 Allergy / Immunology past         13
    ##  6 Allergy / Immunology patient      22
    ##  7 Autopsy              1            66
    ##  8 Autopsy              anterior     47
    ##  9 Autopsy              inch         59
    ## 10 Autopsy              left         83
    ## # … with 199 more rows

  - For instance, the top 5 frequently used words in Allergy/ Immunology
    specialties are “allergies”, “history”, “nasal”,“noted”,“patient”

# Question 7 - extra

Find your own insight in the data:

Ideas:

  - Interesting ngrams
  - See if certain words are used more in some specialties than others

<!-- end list -->

``` r
mt_samples %>% 
  unnest_tokens(token, transcription) %>% 
  anti_join(stop_words, by = c("token" = "word")) %>% 
  group_by(medical_specialty) %>% 
  count(token, medical_specialty) %>% 
  bind_tf_idf(token,medical_specialty,n) %>% 
  arrange(desc(tf_idf)) %>% 
  top_n(1,n)
```

    ## # A tibble: 41 x 6
    ## # Groups:   medical_specialty [40]
    ##    medical_specialty        token       n     tf    idf   tf_idf
    ##    <chr>                    <chr>   <int>  <dbl>  <dbl>    <dbl>
    ##  1 Lab Medicine - Pathology tumor      35 0.0288 0.470  0.0135  
    ##  2 Sleep Medicine           sleep     143 0.0407 0.223  0.00907 
    ##  3 Ophthalmology            eye       456 0.0278 0.255  0.00710 
    ##  4 Podiatry                 foot      232 0.0193 0.357  0.00688 
    ##  5 Endocrinology            thyroid   129 0.0199 0.255  0.00508 
    ##  6 Lab Medicine - Pathology cm         35 0.0288 0.134  0.00384 
    ##  7 Office Notes             normal    230 0.0299 0.0513 0.00153 
    ##  8 Chiropractic             pain      187 0.0278 0.0253 0.000705
    ##  9 IME-QME-Work Comp etc.   pain      152 0.0180 0.0253 0.000456
    ## 10 Letters                  pain       80 0.0149 0.0253 0.000378
    ## # … with 31 more rows

\-We can see from the table that the frequency of using certain words
depends on medical specialties. For instance, word “eye” have been uses
456 times in ophthalmology which is reliable.
