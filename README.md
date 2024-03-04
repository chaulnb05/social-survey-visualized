# Do young people who spend different amounts of time on the internet have different lifestyles?

This shiny application visualizes the difference in lifestyles of young people who have different internet usage, based on a survey completed by 1010 Slovakians aged 15 to 30 in 2013.

## User Guide
Users have the choice to explore the data in 2 ways.

 ### *Option 1: "Answer distribution of each statement"*
This option focuses on one statement at a time. Users can scroll through a list of survey questions to pick one to explore. Users can also decide whether they want to see answers from 1010 survey participants, or 100 random participants at a time. If users wish to choose the former option, they can also generate different sets of participants to show on the boxplot by clicking the `Generate Sample` button

 ### *Option 2: "Answer distribution of each statement"*
 This option allows users to explore multiple survey questions at a time and compare how people in different groups of internet usage answer the questions on average. Users can pick up to 5 survey questions by scrolling through the search bar

 ## Building of the app
-  This is a single-file application, which means UI, data processing, and server functions are in one file - `app.R`.
-  The reason why I chose this method is because this is a simple visualization app that can be done in a single-file method. However, for bigger and more complicated applications, having 2 separated files for UI and server are more suitable.

## Data Source
The dataset used to build this application is [Young People Survey](https://www.kaggle.com/datasets/miroslavsabo/young-people-survey) by Miroslav Sabo on Kaggle. The source is also referenced on the website in the `References` tab.


## Note
This is my final project for the **Data Visualization with R** course at RMIT University in May-June 2023.
