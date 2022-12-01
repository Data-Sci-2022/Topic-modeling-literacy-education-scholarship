**Final Project - Data Science for Linguists (LING 2340) Fall 2022**

Gianina Morales

----------------

# Progress report

----------------
## October 8th 2022

I created the repository for my final project [**Topic-modeling-literacy-education-scholarship**](https://github.com/Data-Sci-2022/Topic-modeling-literacy-education-scholarship). I followed the instructions and prepared the required files.

## October 11th and 12th 2022

I finished the documents required for the beginning of the Final project repository: 

  - [Project plan](project_plan.md).
  - [README](README.md)
  - [Progress report](progress_report.md).

For the purpose of the Project plan document, I studied more about topic modeling in my main sources: [Eight to late](https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/) and [Text Mining with R](https://www.tidytextmining.com/index.html). 

------
# First Progress Report

## November 3rd 2022

For this progress report, I collect, pre-processing the data and prepared a data frame ready for the topic modeling analysis. Unfortunately, I had many problems with "parsing", and I could not work with all the data; I had to come back and work with a subgroup to test the procedure to clean the data. The details are in my [Data_processing2](Data_processing2.md) document. I am following the book [Text Mining with R](https://www.tidytextmining.com/index.html) as my base. 

* *sharing plan*

My sharing plan does not have so much change from what I described in my [Project plan](project_plan.md). I still cannot share the data publicly. However, I plan to consult about the possibility of sharing a sample of open access articles in my repository. This plan will be resolved in the next days. In the meantime, I shared a sample of what will be my base data frame in an `.Rds` file inside [Data_samples](/Data_samples).

------
# Second Progress Report

## November 15th 2022

For this progress report, I have finished all the initial [processing of my data](Data_processing_report2.md). As a result, I have a tidy data frame ready for advancing with the topic modeling part of my research.
The pre-processing and initial processing of my data took me an incredibly tremendous amount of time. For that reason, I am still in the early stage of the second part (and more properly "investigative") of my work. 

* *Sharing scheme for the "found" portion of the data*

After consulting to my advisor and deciding doubling the corpus including another source, I feel more confident of sharing the products of my data processing which are basically lists of millions of words. 

I have saved the product of my processing so far in several Rd files. Unfortunately, the files are heavy for GitHub (they are bigger than 25 MB), so I only can share one of them in [Data_product_samples](Data_product_samples). In my future submission, I plan to prepare a pre-processing and processing analysis for a set of open access articles to show the usability of my code. My idea is apply the same codes used to the whole corpus. 

* *Licensing*

I opted for a creative common licence [Attribution-NonCommercial-ShareAlike 4.0 International](LICENSE.md). This licence allows sharing and adapt the material but only for noncommercial purposes and always attributing credit. I selected a semi-restrictive licence because my project is part of a major research project which has its own clauses and licence and I do not want to interfere with that. 

------
# Third Progress Report

## December 1st 2022

For this report, I applied the topic modeling package to identify probabilistic topics and terms by topics in my data. I had many problems with my computer trying to run the model with the master data frame (that comprises the words from all the articles of my corpus). I even applied another stopwords (Onix) sample to diminish the size. I reduced the tokens by 1 million (now, I have more than 9,000,000 tokens). Ultimately, I decided only to run the models for decades. I also had problems knitting, so my report only shows raw data because I could not have time to do written analyses.
Finally, I have applied feedback and corrected my file from the report 2. 

---




