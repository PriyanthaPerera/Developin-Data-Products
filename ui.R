library("shiny")


shinyUI(pageWithSidebar(
  headerPanel("The Power of Machine Learning Techniques in Detecting Non-Linearity in Data"),
  sidebarPanel(
    h4('A simple 1-D Example'),
    p('This example is run in R  with the following packages:
      shiny,
      ggplot2,
      caret,
      randomForest,
      gbm,
      plyr,
      . The "observed" data is generated using a simple non-linear function with a linear trend corrupted by a mean zero normally distributed error term. '),

    p('The following sample of  Machine Learning techniques are selected and the fits contrasted with that of a Linear Regression: Random Forest, K-Nearest Neighbors and Gradient Boosting. Many other techniwues are available and the reader is encouraged to research them. Goodness of fit will be measured by the RMSE statistic. For this demonstration we have not performed other goodness of fit techniques that are usuallly done to determine the best fit. ' ),

    p('Please select the buttons in order to view the original underlying data and the respective graphs of the fits. The penultimate button will reveal the original underlying non-linear function'),

    p( 'Select the appropriate number in the dropbox and submit. Obsereved Data = 1, Linear Regression = 2, Random Forest = 3, K-Nearest Neighbors = 4, Gradient Boosting = 5, Underlying Function = 2, Side-by-Side Plot = 6'),

    radioButtons("model", "Model",
               c("obs", "lm", "rf", "knn", "gbm", "det", "compare")
  )
    ),

 mainPanel(
    plotOutput("outplot"),
    h4('RMSE'),
    verbatimTextOutput("rmse")
 )
))

