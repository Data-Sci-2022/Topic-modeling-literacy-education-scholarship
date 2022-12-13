Final project; Data processing - Data Science for Linguists (LING 2340)
Fall 2022
================
Gianina Morales
11/15/2022 (version 1) - 12/1/2022 (version 2)- 12/15/2022 (version 3)

- <a href="#data-processing---progress-report-2-new-replacement-file"
  id="toc-data-processing---progress-report-2-new-replacement-file">Data
  processing - Progress report 2 (new replacement file)</a>
  - <a
    href="#pre-processing-creating-and-tidying-dataframes-from-the-corpus"
    id="toc-pre-processing-creating-and-tidying-dataframes-from-the-corpus">Pre-processing:
    Creating and tidying dataframes from the corpus</a>
    - <a href="#turning-raw-data-on-raw-data-frames"
      id="toc-turning-raw-data-on-raw-data-frames">Turning raw data on raw
      data frames</a>
    - <a href="#turning-raw-data-frames-on-tidy-data-frames-of-tokens"
      id="toc-turning-raw-data-frames-on-tidy-data-frames-of-tokens">Turning
      raw data frames on tidy data frames of tokens</a>
  - <a href="#initial-processing" id="toc-initial-processing">Initial
    processing</a>
    - <a href="#general-sense-of-the-data-by-decade"
      id="toc-general-sense-of-the-data-by-decade">General sense of the data
      by decade</a>
    - <a href="#description-of-my-data-so-far"
      id="toc-description-of-my-data-so-far">Description of my data so far</a>
  - <a href="#next-steps" id="toc-next-steps">Next steps</a>

# Data processing - Progress report 2 (new replacement file)

------------------------------------------------------------------------

## Pre-processing: Creating and tidying dataframes from the corpus

``` r
library(tidytext)
library(tidyverse)
library(tm)
library(stopwords)
library(stringi)
library(topicmodels)
knitr::opts_chunk$set(fig.fullwidth=TRUE, fig.path = "Images/Data_processes1/", cache=TRUE)
```

### Turning raw data on raw data frames

***Note***

*After further discussion with my advisor on the possible results of my
project, the R processing of my initial data, and the confidential
agreement with the publishers of the “leading journal in the field of
Literacy education,” I decided to include data from a second journal in
my corpus. This time, I am considering the journal that publishes the
paper conferences of the annual conference of the same organization in
charge of the “leading journal in the field of Literacy education.” This
movement will have three benefits:*

- *I can share a sample of my raw data (open-access articles).*
- *I will have a more significant corpus to analyze trends over time in
  topics and language present in a community of literacy research (the
  leading association).*
- *I will have more tools to comply with the ethics of avoiding
  individualization in the data analysis*.

I divided the data into decades. It was very difficult to manipulate the
whole data (almost 3,200 `.txt` files), so I created individual data
frames.

**Decade 1. 1969-1979**

*Note: I am considering data from the first year in which the two focal
journals published, to facilitate further comparison with previous
content analysis studies.*

- Reading the `.txt` files

``` r
# read all the content from one decade of .txt files into a data frame.
raw_corpus69_79 <- tibble(file = dir("Private/1969-1979", full.names = TRUE))%>%
    mutate(text = map(file, read_lines, skip = 2, skip_empty_rows = TRUE, n_max = Inf,locale = default_locale(), na = character(), num_threads = readr_threads())) %>%
    transmute(id = basename(file), text) %>%
    unnest(text) %>%
#replace punctuation signs and numbers with spaces in column 'text'
    mutate(text = str_replace_all(text, "[[:punct:]]+|[0-9]+", " ")) %>% 
#add column with decade
  add_column(decade = "decade1") %>% 
  relocate(decade, id, text)   
#replace spaces with underscore and other changes in column id
raw_corpus69_79$id <- stri_replace_all_regex(raw_corpus69_79$id, pattern=c(" ","JLR","Y", "[aeiou]"), replacement=c("-","","", "x"), vectorize=F)
#visualization 
sample_n(raw_corpus69_79, 10) 
```

    ## # A tibble: 10 × 3
    ##    decade  id                   text                                            
    ##    <chr>   <chr>                <chr>                                           
    ##  1 decade1 1973-dxslxndx.txt    "Its extreme simplicity is deceiving  Just try …
    ##  2 decade1 1974-lxtxn.txt       ""                                              
    ##  3 decade1 1977-Rxplxy.txt      "alpha at            or more    p       Further…
    ##  4 decade1 1970-pxlxttxxrx.txt  "Time"                                          
    ##  5 decade1 1977-Clxxr.txt       "cated and facile readers   "                   
    ##  6 decade1 1972-whxtstxnx.txt   "DRT Form"                                      
    ##  7 decade1 1977-Allxngtxn.txt   "     The  reliability  of the  experimental  t…
    ##  8 decade1 1972-xthxy.txt       "again promises to be a long and arduous  but r…
    ##  9 decade1 1969-Bxrgxr.txt      "and  Vechsler Jntelligence Scale for Children "
    ## 10 decade1 1970-fxrtxnbxrry.txt "REFERENCES"

**Decade 2. 1980-1989**

- Reading the `.txt` files

``` r
# read all the content from one decade of .txt files into a data frame.
raw_corpus80_89 <- tibble(file = dir("Private/1980-1989", full.names = TRUE))%>%
    mutate(text = map(file, read_lines, skip = 2, skip_empty_rows = TRUE, n_max = Inf,locale = default_locale(), na = character(), num_threads = readr_threads())) %>%
    transmute(id = basename(file), text) %>%
    unnest(text) %>%
#replace punctuation signs and numbers with spaces in column 'text'
    mutate(text = str_replace_all(text, "[[:punct:]]+|[0-9]+", " ")) %>% 
#add column with decade
  add_column(decade = "decade2") %>% 
  relocate(decade, id, text) 
  #replace spaces with underscore and other changes in column id
raw_corpus80_89$id <- stri_replace_all_regex(raw_corpus80_89$id, pattern=c(" ","JLR","Y", "[aeiou]"), replacement=c("-","","", "x"), vectorize=F)
#visualization 
sample_n(raw_corpus80_89, 10)
```

    ## # A tibble: 10 × 3
    ##    decade  id                 text                                              
    ##    <chr>   <chr>              <chr>                                             
    ##  1 decade2 1980-grxbx.txt     "Low         "                                    
    ##  2 decade2 1988-nxxmxn.txt    "question type  cued vs  noncued  as the other "  
    ##  3 decade2 1989-wxllxnsky.txt "Table  "                                         
    ##  4 decade2 1987-Fxrrxlx.txt   " administered only at the end of first grade  Th…
    ##  5 decade2 1987-Gxmxrrx.txt   "Bachman    L   F          Performance  on  cloze…
    ##  6 decade2 1987-Txxlx.txt     "Downing   J   Ayers   D      Schaefer   B       …
    ##  7 decade2 1988-sxmmxns.txt   "SD =     was significantly less than that of   y…
    ##  8 decade2 1985-Schwxrtz.txt  "without  supporting context  sentences  and  wro…
    ##  9 decade2 1988-pxkxlskx.txt  " "                                               
    ## 10 decade2 1989-kxbby.txt     "a  "

**Decade 3. 1990-1999**

- Reading the `.txt` files

``` r
# read all the content from one decade of .txt files into a data frame.
raw_corpus90_99 <- tibble(file = dir("Private/1990-1999", full.names = TRUE))%>%
    mutate(text = map(file, read_lines, skip = 2, skip_empty_rows = TRUE, n_max = Inf,locale = default_locale(), na = character(), num_threads = readr_threads())) %>%
    transmute(id = basename(file), text) %>%
    unnest(text) %>%
#replace punctuation signs and numbers with spaces in column 'text'
    mutate(text = str_replace_all(text, "[[:punct:]]+|[0-9]+", " ")) %>% 
#add column with decade
  add_column(decade = "decade3") %>% 
  relocate(decade, id, text) 
  #replace spaces with underscore and other changes in column id
raw_corpus90_99$id <- stri_replace_all_regex(raw_corpus90_99$id, pattern=c(" ","JLR","Y", "[aeiou]"), replacement=c("-","","", "x"), vectorize=F)
#visualization 
sample_n(raw_corpus90_99, 10)
```

    ## # A tibble: 10 × 3
    ##    decade  id                 text                                              
    ##    <chr>   <chr>              <chr>                                             
    ##  1 decade3 1994-mcCxrthxy.txt "about each teacher "                             
    ##  2 decade3 1992-mcCxrthxy.txt "Rogoff  B      Adult assistance of children s le…
    ##  3 decade3 1999-mxrtxn.txt    "Electronic Expression"                           
    ##  4 decade3 1995-txdwxll.txt   "Other was adapted to two voices  the Other voice…
    ##  5 decade3 1999-gxx.txt       "young children getting scaffolded socialization …
    ##  6 decade3 1998-drxhxr2.txt   "population  The   fourth graders at School  cons…
    ##  7 decade3 1991-mcLxrxn.txt   "For Freire  the most important sites for resisti…
    ##  8 decade3 1993-schxrxr.txt   "Neutral"                                         
    ##  9 decade3 1995-mcxntyrx.txt  "literature  celebration of texts "               
    ## 10 decade3 1991-flxxd.txt     " "

**Decade 4. 2000-2009**

- Reading the `.txt` files

``` r
# read all the content from one decade of .txt files into a data frame.
raw_corpus00_09 <- tibble(file = dir("Private/2000-2009", full.names = TRUE))%>%
    mutate(text = map(file, read_lines, skip = 2, skip_empty_rows = TRUE, n_max = Inf,locale = default_locale(), na = character(), num_threads = readr_threads())) %>%
    transmute(id = basename(file), text) %>%
    unnest(text) %>%
#replace punctuation signs and numbers with spaces in column 'text'
    mutate(text = str_replace_all(text, "[[:punct:]]+|[0-9]+", " ")) %>% 
#add column with decade
  add_column(decade = "decade4") %>% 
  relocate(decade, id, text) 
  #replace spaces with underscore and other changes in column id
raw_corpus00_09$id <- stri_replace_all_regex(raw_corpus00_09$id, pattern=c(" ","JLR","Y", "[aeiou]"), replacement=c("-","","", "x"), vectorize=F) 
#visualization 
sample_n(raw_corpus00_09, 10)
```

    ## # A tibble: 10 × 3
    ##    decade  id                text                                               
    ##    <chr>   <chr>             <chr>                                              
    ##  1 decade4 2006-xng.txt      "helped promote positive  emotions toward learning…
    ##  2 decade4 2005-Knxbxl.txt   "                                       Montclair …
    ##  3 decade4 2001-phxllxps.txt "changing patterns of verbal response in their dic…
    ##  4 decade4 2004-Smxth2.txt   "        Urbana  IL   National  Council of Teacher…
    ##  5 decade4 2001-lxxdlxw.txt  "structure notions of teaching  learning  within l…
    ##  6 decade4 2009-wxhlwxnd.txt "texts  transforming meanings  improvising practic…
    ##  7 decade4 2001-kxstx.txt    "National Reading Conference Yearbook     pp      "
    ##  8 decade4 2001-clxrk.txt    ""                                                 
    ##  9 decade4 2003-bxrxnx.txt   "Maria  +   Lucero  +   "                          
    ## 10 decade4 2008-zhxng.txt    "Narrows down query words and re searches     "

**Decade 5. 2010-2019**

- Reading the `.txt` files

``` r
# read all the content from one decade of .txt files into a data frame.
raw_corpus10_19 <- tibble(file = dir("Private/2010-2019", full.names = TRUE))%>%
     mutate(text = map(file, read_lines, skip = 2, skip_empty_rows = TRUE, n_max = Inf,locale = default_locale(), na = character(), num_threads = readr_threads())) %>%
    transmute(id = basename(file), text) %>%
    unnest(text) %>%
#replace punctuation signs and numbers with spaces in column 'text'
    mutate(text = str_replace_all(text, "[[:punct:]]+|[0-9]+", " ")) %>% 
#add column with decade
  add_column(decade = "decade5") %>% 
  relocate(decade, id, text) 
  #replace spaces with underscore and other changes in column id
raw_corpus10_19$id <- stri_replace_all_regex(raw_corpus10_19$id, pattern=c(" ","JLR","Y", "[aeiou]"), replacement=c("-","","", "x"), vectorize=F)
#visualization
sample_n(raw_corpus10_19, 10) 
```

    ## # A tibble: 10 × 3
    ##    decade  id                     text                                          
    ##    <chr>   <chr>                  <chr>                                         
    ##  1 decade5 2015-Wxsx_OByrnx.txt   "characteristics associated with presented id…
    ##  2 decade5 2017-mxchxdx.txt       "in examinations of children s literacy  e g …
    ##  3 decade5 2018-rxbxn.txt         "evidence from Jain knowledge  offers the opp…
    ##  4 decade5 2013-dxnnxs.txt        "Abstract "                                   
    ##  5 decade5 2019-bxchhxlz.txt      "When I first started I was just a player and…
    ##  6 decade5 2015-Gxxdmxn.txt       "language appropriately based on social conte…
    ##  7 decade5 2010-hxtchxsxn.txt     "teachers  pedagogical approaches matched the…
    ##  8 decade5 2011-lxwxs.txt         "or cable goes out "                          
    ##  9 decade5 2012-nxgxxrón-Lxx.txt  "  communicate with all of my family who live…
    ## 10 decade5 2016-Lxwxnxg-xt-xl.txt "Written Text"

**Decade 6. 2020-2022**

*This is the remains and is not a decade, but it represents the most
contemporary scholarship, so I am considering this set of articles as a
piece.*

- Reading the `.txt` files

``` r
# read all the content from one decade of .txt files into a data frame.
raw_corpus20_22 <- tibble(file = dir("Private/2020-2022", full.names = TRUE))%>%
    mutate(text = map(file, read_lines, skip = 2, skip_empty_rows = TRUE, n_max = Inf,locale = default_locale(), na = character(), num_threads = readr_threads())) %>%
    transmute(id = basename(file), text) %>%
    unnest(text) %>%
#replace punctuation signs and numbers with spaces in column 'text'
    mutate(text = str_replace_all(text, "[[:punct:]]+|[0-9]+", " ")) %>% 
  #add column with decade
  add_column(decade = "decade6") %>% 
  relocate(decade, id, text) 
  #replace spaces with underscore and other changes in column id
raw_corpus20_22$id <- stri_replace_all_regex(raw_corpus20_22$id, pattern=c(" ","JLR","Y", "[aeiou]"), replacement=c("-","","", "x"), vectorize=F)
#visualization
sample_n(raw_corpus20_22, 10) 
```

    ## # A tibble: 10 × 3
    ##    decade  id                 text                                              
    ##    <chr>   <chr>              <chr>                                             
    ##  1 decade6 2021-wxng.txt      "   I feel like I make friends with people in goo…
    ##  2 decade6 2021-wxssmxn.txt   "Thomas  E  E      Stories still matter  Rethinki…
    ##  3 decade6 2020-mxchxdx.txt   "� "                                              
    ##  4 decade6 2020-kxchxrsky.txt "Barnard  M      Approaches to understanding visu…
    ##  5 decade6 2021-lxvxnx.txt    "Here is an example of UDBW  In one study in a hi…
    ##  6 decade6 2021-pxckxrd.txt   "Manuscript submitted    November   Manuscript ac…
    ##  7 decade6 2021-xmxgxn.txt    "strategically author herself throughout the text…
    ##  8 decade6 2020-kxchxrsky.txt "Dani Kachorsky  College of Education and Human D…
    ##  9 decade6 2020-kxblxr.txt    "�he was tired and he felt he ought to be getting…
    ## 10 decade6 2020-wxllxs.txt    "Now picture this    children in an urban third g…

### Turning raw data frames on tidy data frames of tokens

``` r
# preparing list of the data frames by decade
raw_corpus_all =list(raw_corpus69_79, raw_corpus80_89, raw_corpus90_99, raw_corpus00_09 , raw_corpus10_19, raw_corpus20_22)
#names
names(raw_corpus_all) <- c("tidy_corpus69_79", "tidy_corpus80_89", "tidy_corpus90_99", "tidy_corpus00_09" , "tidy_corpus10_19", "tidy_corpus20_22")
#results
raw_corpus_all %>% str(1)
```

    ## List of 6
    ##  $ tidy_corpus69_79: tibble [219,094 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ tidy_corpus80_89: tibble [199,576 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ tidy_corpus90_99: tibble [266,226 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ tidy_corpus00_09: tibble [238,349 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ tidy_corpus10_19: tibble [195,807 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ tidy_corpus20_22: tibble [23,434 × 3] (S3: tbl_df/tbl/data.frame)

``` r
#Loading custom stopwords
My_stopwords <- read_lines("My_stopwords.csv")
#tokenizing across data frames
tidy_corpus_all <- raw_corpus_all %>% 
  map(~ .x %>% 
    unnest_tokens(word, text))
#Apply stop words
tidy_corpus_all <- tidy_corpus_all %>% 
  map(~ .x %>% 
        anti_join(stop_words %>% 
               filter(lexicon=="onix") %>% 
               rbind(tibble(lexicon = "custom", word = My_stopwords)))) 
```

    ## Joining, by = "word"
    ## Joining, by = "word"
    ## Joining, by = "word"
    ## Joining, by = "word"
    ## Joining, by = "word"
    ## Joining, by = "word"

``` r
  #final tidy corpus object
tidy_corpus_all %>% str(1)
```

    ## List of 6
    ##  $ tidy_corpus69_79: tibble [1,283,443 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ tidy_corpus80_89: tibble [1,411,179 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ tidy_corpus90_99: tibble [2,116,316 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ tidy_corpus00_09: tibble [2,036,098 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ tidy_corpus10_19: tibble [1,840,809 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ tidy_corpus20_22: tibble [446,786 × 3] (S3: tbl_df/tbl/data.frame)

- Saving data frames

``` r
#Individual tidy data frames saved as Rds
##size
tidy_corpus_all %>%
  map_chr(~ .x %>% 
            object.size() %>% 
            format("Mb")) 
```

    ## tidy_corpus69_79 tidy_corpus80_89 tidy_corpus90_99 tidy_corpus00_09 
    ##        "31.9 Mb"        "34.4 Mb"        "51.7 Mb"          "50 Mb" 
    ## tidy_corpus10_19 tidy_corpus20_22 
    ##        "45.2 Mb"        "11.8 Mb"

``` r
##saving individual data frames as Rds  
if (!dir.exists("Private/tidy_corpus_all")) {
  dir.create("Private/tidy_corpus_all")
}
tidy_corpus_all%>%
  iwalk(~ write_rds(.x, paste0("Private/tidy_corpus_all/", .y, ".Rds")))
```

*Note: as files are heavy, I only could share in GitHub the last one*

------------------------------------------------------------------------

## Initial processing

------------------------------------------------------------------------

### General sense of the data by decade

To understand some generalities of the data, I combine the data frames
into one master data frame with all the tokens.

- Master data frame

``` r
# Combining data frames
master_tidy_corpus <- bind_rows(tidy_corpus_all$tidy_corpus69_79, tidy_corpus_all$tidy_corpus80_89, tidy_corpus_all$tidy_corpus90_99, tidy_corpus_all$tidy_corpus00_09 , tidy_corpus_all$tidy_corpus10_19, tidy_corpus_all$tidy_corpus20_22)
head(master_tidy_corpus)
```

    ## # A tibble: 6 × 3
    ##   decade  id             word        
    ##   <chr>   <chr>          <chr>       
    ## 1 decade1 1969-crxwx.txt introduction
    ## 2 decade1 1969-crxwx.txt considerable
    ## 3 decade1 1969-crxwx.txt studies     
    ## 4 decade1 1969-crxwx.txt conducted   
    ## 5 decade1 1969-crxwx.txt nature      
    ## 6 decade1 1969-crxwx.txt memory

``` r
#Saving as Rds
master_tidy_corpus%>%
  write_rds("Private/master_tidy_corpus.Rds")
# Saving the master data frame as an Rds with less than 25 MB to share as a sample in Github
set.seed(170)
  write_rds (master_tidy_corpus %>% 
  sample_n(900000), "Data_product_samples/master_tidycorpus_sample.Rds")
```

- Generalities

``` r
## word count
head(master_tidy_corpus %>% 
    count(word, sort = TRUE), 30)
```

    ## # A tibble: 30 × 2
    ##    word          n
    ##    <chr>     <int>
    ##  1 reading  151923
    ##  2 students  96993
    ##  3 literacy  62494
    ##  4 children  61047
    ##  5 research  59529
    ##  6 teachers  54463
    ##  7 study     45779
    ##  8 teacher   43348
    ##  9 language  41600
    ## 10 text      40838
    ## # … with 20 more rows

``` r
##summary
summary(master_tidy_corpus) 
```

    ##     decade               id                word          
    ##  Length:9134631     Length:9134631     Length:9134631    
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character

- Plot

``` r
master_tidy_corpus %>%
  count(word, sort = TRUE)%>%
  filter(n > 25000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill=word)) +
  geom_col() +
  labs(y = NULL)
```

![](Images/Data_processes1/unnamed-chunk-11-1.png)<!-- -->

### Description of my data so far

After all the processes to tidy my data, I finally have a data frame
useful for applying topic modeling. My master data frame consists of
9,134,631 tokens representing the “cleaning” words from 3,131 articles
published between 1969 and 2022 in two literacy education journals—a
leading research journal and a conference papers journal, both from the
same disciplinary association. The master data frame has 131,328 unique
words. The plot above indicates the words repeated over 25,000 times
across the data frame. In general, it seems to represent the scope of
the focal journals, anchored in literacy education research. More
significance will be identified through the topic modeling analysis.

------------------------------------------------------------------------

## Next steps

1.  Finishing the testing and publishing the general topic modeling
    analysis.

2.  Apply the model to analyze in deep each decade.

3.  Compare results of topics between decades with more detail.

4.  Create the final report of my results.

5.  Discuss the results in relation to my research questions:

- What are the trends in topics of literacy education research and
  scholarship over more than five decades (1969-2021) of the focal
  journals?

- How do the topics have changed over time?

``` r
sessionInfo()
```

    ## R version 4.2.1 (2022-06-23 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19045)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.utf8 
    ## [2] LC_CTYPE=English_United States.utf8   
    ## [3] LC_MONETARY=English_United States.utf8
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.utf8    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] topicmodels_0.2-12 stringi_1.7.8      stopwords_2.3      tm_0.7-9          
    ##  [5] NLP_0.2-1          forcats_0.5.2      stringr_1.4.1      dplyr_1.0.9       
    ##  [9] purrr_0.3.4        readr_2.1.2        tidyr_1.2.0        tibble_3.1.8      
    ## [13] ggplot2_3.3.6      tidyverse_1.3.2    tidytext_0.3.4    
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] httr_1.4.4          bit64_4.0.5         vroom_1.5.7        
    ##  [4] jsonlite_1.8.0      modelr_0.1.9        assertthat_0.2.1   
    ##  [7] highr_0.9           stats4_4.2.1        googlesheets4_1.0.1
    ## [10] cellranger_1.1.0    yaml_2.3.5          slam_0.1-50        
    ## [13] pillar_1.8.1        backports_1.4.1     lattice_0.20-45    
    ## [16] glue_1.6.2          digest_0.6.29       rvest_1.0.3        
    ## [19] colorspace_2.0-3    htmltools_0.5.3     Matrix_1.5-3       
    ## [22] pkgconfig_2.0.3     broom_1.0.1         haven_2.5.1        
    ## [25] scales_1.2.1        tzdb_0.3.0          googledrive_2.0.0  
    ## [28] farver_2.1.1        generics_0.1.3      ellipsis_0.3.2     
    ## [31] withr_2.5.0         cli_3.3.0           magrittr_2.0.3     
    ## [34] crayon_1.5.1        readxl_1.4.1        evaluate_0.16      
    ## [37] tokenizers_0.2.3    janeaustenr_1.0.0   fs_1.5.2           
    ## [40] fansi_1.0.3         SnowballC_0.7.0     xml2_1.3.3         
    ## [43] tools_4.2.1         hms_1.1.2           gargle_1.2.0       
    ## [46] lifecycle_1.0.1     munsell_0.5.0       reprex_2.0.2       
    ## [49] compiler_4.2.1      rlang_1.0.4         grid_4.2.1         
    ## [52] rstudioapi_0.14     labeling_0.4.2      rmarkdown_2.16     
    ## [55] codetools_0.2-18    gtable_0.3.0        DBI_1.1.3          
    ## [58] R6_2.5.1            lubridate_1.8.0     knitr_1.40         
    ## [61] fastmap_1.1.0       bit_4.0.4           utf8_1.2.2         
    ## [64] modeltools_0.2-23   parallel_4.2.1      Rcpp_1.0.9         
    ## [67] vctrs_0.4.1         dbplyr_2.2.1        tidyselect_1.1.2   
    ## [70] xfun_0.32
