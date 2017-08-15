# R-Shiny-Project
Real-Time Portfolio Optimization Demo
#### Initial Authors: Li Zou, Xuan Zhou, Yihuan Wang

#### Maintainer: Xuan Zhou

Initially, we deployed it on Amazon Web Services(AWS). Right now, I deploy this on a free RStudio’s hosting service, shinyapps.io:
https://xuanzhou.shinyapps.io/Portfolio-Optimization-Demo/

## Summary:
* Using Shiny in R to create a website(demo)
* Performing portfolio optimization for investors using real-time data from yahoo
* Shiny should contain sever.R and ui.R which is the user interface(Looks like Shiny updates its framework, now sever.R and ui.R can be written in the same file. However, written separately is still available, and to me it is the better way to do it.)

### New Adds:
* Optimize the user interface
* Add google analytics for tracking all the page views and button clickings


### Pending:
* Wrong input value issues
* Tickers missing issues: webpage using tickers from yahoo, probably other website use different tickers
* Missing labels for graphs in each tabPanel

