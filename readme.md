# Covid-19 related studies

This is an ongoing pet project that studies various datasets related to Covid-19, in order to extract some insights regarding the cases and deaths on the one hand and on the other hand the symptoms and how probable it is a person with specific social behaviour and symptoms to be a positive Covid-19 case.

## Requirements
All the analysis is conducted using R, so one should have R and Rstudio installed. The specific packages used in this analysis are mentioned inside the code so there's no need to be already installed.

# Subprojects
## US surveillance over time
The [dataset](https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36) used in this subproject contains information about the Covid-19 cases and deaths in the United States in the time period from 22 Jan. 2020 to 8 Sep. 2021 . It contains 15 columns, with information about the date, state, number of cases and number of deaths, not only the new ones but also the cumulative values from the beginning of the pandemic.

In this subproject I analysed the dataset, by creating multiple plots to study the behaviour of each variable in correlation to time and also to other variables. I also tried to compute the average number of days between the confirmation of a covid case and the probable death. All this analysis is in the directory USCasesDeaths.


## Covid-19 Symptoms
The [dataset](https://www.kaggle.com/hemanthhari/symptoms-and-covid-presence) used in this subproject is one that contains possible covid symptoms and social behaviour of approximately 5000 people and whether they are positive to Covid-19 or not. Specifically it contains 20 columns regarding symptoms, chronic diseases and social behaviour and a column that shows if they have covid, all containing boolean values ('Yes', 'No').

I analysed the dataset, extracting some statistics and similarity between columns and then I trained a classifier that predicts if a person is a positive case or not. In this moment, the classifier is a decision tree but as I said at the beginning, this is an ongoing project and my goal is to try different classifiers (e.g. Random Forest, SVM) in order to find the best model for this case. For more details, see the code inside the directory SymptomsCovid.

<br/>
@Copyright Dimitris Gougousis