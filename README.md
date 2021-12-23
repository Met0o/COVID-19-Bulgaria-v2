# COVID-19-Bulgaria-v2

This repository contains the code of the COVID-19 dashboard as well as the automation scripts for data pre-processing and daily deployment.
 
The Shiny web application (current version) can be accessed from my ShinyApps.io profile - https://metodisimeonov.shinyapps.io/COVID-19-Bulgaria-v2
 
Static HTML render of the dashboard (not updated) can be viewed in my RPubs profile - https://rpubs.com/MetodiSimeonov/COVID-19-Bulgaria-v2

First version of the dashboard containing active & recovered data (now unavailable from the source) can be viewed in my RPubs profile - https://rpubs.com/MetodiSimeonov/COVID-19-Bulgaria 

File structure and execution logic:

1. COVID-19-Bulgaria-v2.Rmd - Rmarkdown application file
2. Updates.R       - Script to update the data source package daily, transform and export .rds artifacts for consumption of the web app
3. taskscheduleR.R - Script to create the automation job on a windows-based machine 
4. vaccines.R      - Script used to develop vaccination data and plots 
5. .rds files      - Data artifacts required for the web app  
