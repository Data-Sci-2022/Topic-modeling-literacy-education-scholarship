**Final Project - Data Science for Linguists (LING 2340) Fall 2022**

Gianina Morales

10/11/2022

----------------

# Project plan

----------------

## Topic modeling of Literacy Education articles: five decades of scholarship

### Summary

Topic modeling is a data mining and machine learning technique that automatically analyzes texts to identify latent topic structures. Topic modeling programs use algorithms to identify probabilistic topics or patterns of words in a corpus (Brett, 2012). The most widely used model in humanities is Latent Dirichlet Allocation&mdash;LDA; more information is available in the blog [Eight to late](https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/).  I will use topic modeling techniques to analyze the trends over time in literacy research and scholarship in one leading journal and a conference papers journal in the field of Literacy education, both pertaining to the same disciplinary association. My research questions are: 

1. What are the trends in topics of literacy education research and scholarship over more than five decades (1969-2022) of the focal journals?
2.  How do the topics have changed over time?

### Data overview

The data for this project corresponds to `.txt` files from all the articles published in the focal journals from 1969 (beginning of the leading journal) to 2022. The data was obtained in the context of a larger research project aimed at applying topic modeling to the analysis of articles from 12 US-based journals in Literacy Education. The agreement with the publisher that gives access to the files implies that files cannot be shared directly with the public, and data results cannot individualize journals until peer-review publication. Therefore, I will share the process analysis and results in the repository without individualizing the source. In the future, my project will supply the development of a journal publication.

In terms of data mining efforts, I have a long way ahead. First, I need to convert the `.txt` files in data frames. That implies several pre-processing actions to clean the data. In terms of actions, I am following the blog [Eight to late](https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/) and the book [Text Mining with R](https://www.tidytextmining.com/index.html) as guides. After the pre-processing phase, I will be able to apply topic modeling techniques to the data. This phase also requires extensive time and effort because the results are not immediate, and it is necessary to run the model repetitively until the results make sense. For this phase, along with the resources mentioned, I am also relying on lectures from [David M. Blei](http://videolectures.net/mlss09uk_blei_tm/#c8302) and other materials published in [Github](https://github.com/trinker/topicmodels_learning). I plan to use the packages: [`tidytext`](https://github.com/juliasilge/tidytext) and [`topicmodels`](https://cran.r-project.org/web/packages/topicmodels/topicmodels.pdf) (or similar) for data mining and topic modeling .

### Analysis overview

I plan to run topic modeling techniques in R to analyze more than five decades (between 1969 and 2022) of scholarship in two important sources in the field of literacy education. The corpus includes all the articles published in the leading focal journal, without distinction of sub-types, plus the conference papers published annually by the same professional association in charge of the leading journal. Therefore, my expected results from the data analysis involve lists of probabilistic topics weighted by the frequency of appearance across the articles and organized over time, for example, by decade (cf. Wang et al., 2017). A situation that may arise in the analysis is the role that *special issues* (collection of articles related to a specific topic) could play in the topic modeling results. However, how the articles are presented in the data makes it difficult to identify the special issues because, except for the last years, the `.txt` files do not detail titles, volumes, and issues. Other elements that contribute to ameliorating the possible *noise* of special issues are the consideration of two focal journals in place of one and the elevated total amount of articles (3,131).  

In term of objectives, the main purpose of my analysis is to identify trends in the topics of literacy research and scholarship, tracing its shifts over time. Secondarily, I pretend to preliminary examine changes in the discourse and vocabulary employed by the authors (literacy scholars). I also have a possible third purpose that will depend on the time employed for the main analysis: to compare the results with topic analysis developed manually by other authors using content analysis methods (e.g., Baldwin et al., 1992; Guthrie et al., 1983; Parsons et al., 2016).

### References

Baldwin, R. S., Readence, J. E., Schumm, J. S., Konopak, J. P., Konopak, B. C., & Klingner, J. K. (1992). Forty Years of NRC Publications: 1952–1991. Journal of Reading Behavior, 24(4), 505–532. https://doi.org/10.1080/10862969209547793

Blei, D. M. (2009, September 1). Topic Models [Lecture]. Machine Learning Summer School, Cambridge, UK. http://videolectures.net/mlss09uk_blei_tm/

Brett, M. (2012, December 12). Topic Modeling: A Basic Introduction Journal of Digital Humanities. Journal of Digital Humanities. http://journalofdigitalhumanities.org/2-1/topic-modeling-a-basic-introduction-by-megan-r-brett/

K. (2015, September 29). A gentle introduction to topic modeling using R. Eight to Late. https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/

Guthrie, J. T., Seifert, M., & Mosberg, L. (1983). Research Synthesis in Reading: Topics, Audiences, and Citation Rates. Reading Research Quarterly, 19(1), 16. https://doi.org/10.2307/747334

Parsons, S. A., Gallagher, M. A., & the George Mason University Content Analysis Team. (2016). A Content Analysis of Nine Literacy Journals, 2009-2014. Journal of Literacy Research, 48(4), 476–502. https://doi.org/10.1177/1086296X16680053

Silge, J. and Robinson, D. (2022). Text Mining with R. https://www.tidytextmining.com/

Wang, Y., Bowers, A. J., & Fikis, D. J. (2017). Automated Text Data Mining Analysis of Five Decades of Educational Leadership Research Literature: Probabilistic Topic Modeling of EAQ Articles From 1965 to 2014. Educational Administration Quarterly, 53(2), 289–323. https://doi.org/10.1177/0013161X16660585

