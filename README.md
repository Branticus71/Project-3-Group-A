# Project-3-Group-A
## Purpose
This repo exists to hold separate automated reports for different data channels that provide visualizations and predictive models. The data used to create these reports  is the Online News Popularity Data Set from the UCI Machine Learning Repository.
## Links to Each Analysis
[Lifestyle articles is available here](LifestyleAnalysis.html)

[Business articles is available here](BusinessAnalysis.html)

[Entertainment articles is available here](EntertainmentAnalysis.html)

[Social Media articles is available here](SocmedAnalysis.html)

[Tech articles is available here](TechAnalysis.html)

[World articles is available here](WorldAnalysis.html)
## List of R Packages Used

1. Tidyverse
2. Caret
3. Corrplot
4. Corrr
5. Psych
6. Rmarkdown
7. KableExtra
8. Timereg

## Code Used to Knit Each Report from a Single Markdown File
```
#Create vector of channels
channel <- c("Lifestyle", "Entertainment", "Business", "Socmed", "Tech", "World")
#create filenames
output_file <- paste0(channel, "Analysis.md")
#create a list for each team with just the team name parameter
params = lapply(channel, FUN = function(x){list(channel = x)})
#put into a data frame
reports <- tibble(output_file, params)
#Required Packages
library(rmarkdown)
library(tidyverse)
pwalk(reports, render, input = "./Project-3-Group-A.Rmd")
```
