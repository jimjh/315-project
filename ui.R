# install.packages('shiny')
library(shiny)

shinyUI(bootstrapPage(
  
  headerPanel("US Census 2010"),
  
  mainPanel(
    # multiple tabs
    tabsetPanel(
      tabPanel('Plot',
               selectInput(inputId = "n_breaks",
                           label = "Number of bins in histogram (approximate):",
                           choices = c(10, 20, 35, 50),
                           selected = 20),
               plotOutput('hist')
      ),
      tabPanel('Age vs Race',
               selectInput('age.gender', 'Gender: ',
                           list('Male' = 'male', 'Female' = 'female')),
               selectInput('age.race', 'Race: ',
                           list('White' = 'white', 'Black' = 'black',
                                'American Indian/Native' = 'american.indian',
                                'Asian' = 'asian')),
               h2('Bandwidth'),
               sliderInput("age.adjust.x", label="Age",
                           min=2, max=10, value=7.5, step=0.5),
               sliderInput("age.adjust.y", label="Pop. Percentage",
                           min=0.05, max=2, value=0.25, step=0.05),
               plotOutput('age.race')
      ),
      tabPanel('Income vs Race',
               selectInput('income.gender', 'Gender: ',
                           list('Male' = 'male', 'Female' = 'female')),
               selectInput('income.race', 'Race: ',
                           list('White' = 'white', 'Black' = 'black',
                                'American Indian/Native' = 'american.indian',
                                'Asian' = 'asian')),
               h2('Bandwidth'),
               sliderInput("income.adjust.x", label="Income",
                           min=1000, max=30000, value=12000, step=1000),
               sliderInput("income.adjust.y", label="Pop. Percentage",
                           min=0.05, max=2, value=0.25, step=0.05),
               plotOutput('income.race')
      )
    )
  )
  
))