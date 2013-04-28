# install.packages('shiny')
library(shiny)

shinyUI(bootstrapPage(
  
  headerPanel("US Census 2010"),
  
  mainPanel(
    # multiple tabs
    tabsetPanel(
      tabPanel('Race by Year',
               selectInput(inputId = "race.year",
                           label = "Race:",
                           choices = c("White","Black","American Indian/Native","Asian","Hispanic"),
                           selected = "White"),
               plotOutput('plot2')
      ),
      tabPanel('Race by Year (Percentage Change)',
               selectInput(inputId = "race.percent",
                           label = "Race:",
                           choices = c("White","Black","American Indian/Native","Asian","Hispanic"),
                           selected = "White"),
               plotOutput('race.percent.change')
      ),
      tabPanel('Race by Year (Proportion)',
               selectInput(inputId = "race.prop",
                           label = "Race:",
                           choices = c("White","Black","American Indian/Native","Asian","Hispanic"),
                           selected = "White"),
               plotOutput('race.proportion')
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
      ),
      # Tab Panel Containing Age vs. Population Graphs
      tabPanel('Age vs. Population',
               tabsetPanel(
                 tabPanel('Male',
                   h4("Select Age Options"),
                   checkboxInput("age1", "Age 1st Quartile"),
                   checkboxInput("agem", "Age Mean"),
                   checkboxInput("age3", "Age 1rd Quartile"),
                   h4("Select Population Options"),
                   checkboxInput("pop1", "Population 1st Quartile"),
                   checkboxInput("popm", "Population Mean" ),
                   checkboxInput("pop3", "Population 3rd Quartile"),
                   br(),
                   checkboxInput("cont", "Show Contour"),
                   checkboxInput("showLine", "Show Lowess"),
                   sliderInput("age.adjust.f", label="Adjust Lowess Smoother Span",
                               min=0, max=.5, value=.3, step=.1),
                   tabPanel('Male Age vs. Population',
                            plotOutput('scatteragepop')  
                            )        
                 ),
                 tabPanel('Female',
                    h4("Select Age Options"),
                    checkboxInput("feage1", "Age 1st Quartile"),
                    checkboxInput("feagem", "Age Mean"),
                    checkboxInput("feage3", "Age 1rd Quartile"),
                    br(),
                    h4("Select Population Options"),
                    checkboxInput("fepop1", "Population 1st Quartile"),
                    checkboxInput("fepopm", "Population Mean" ),
                    checkboxInput("fepop3", "Population 3rd Quartile"),
                    br(),
                    checkboxInput("fecont", "Show Contour"),
                    checkboxInput("feshowLine", "Show Lowess"),
                    sliderInput("age.adjust.f_female", label="Adjust Lowess Smoother Span",
                                min=0, max=.5, value=.3, step=.1),
                    tabPanel('Female Age vs. Population',
                             plotOutput('scatteragepop_female')  
                    )       
                 )
               )
      )
    )
  )
))