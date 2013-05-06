# install.packages('shiny')
library(shiny)

shinyUI(bootstrapPage(

  headerPanel("US Census 2010"),

  mainPanel(div(class='span12',
    # multiple tabs
    tabsetPanel(
      tabPanel('Race by Year',
               selectInput(inputId = "race.year",
                           label = "Race:",
                           choices = c("White","Black","American Indian/Native","Asian","Hispanic"),
                           selected = "White"),
               checkboxInput('pop_contour', 'Show Population Contour'),
               plotOutput('plot2', height="7.5in")
      ),
      tabPanel('Race by Year (Percentage Change)',
               selectInput(inputId = "race.percent",
                           label = "Race:",
                           choices = c("White","Black","American Indian/Native","Asian","Hispanic"),
                           selected = "White"),
               plotOutput('race.percent.change', height="7.5in")
      ),
      tabPanel('Race by Year (Proportion)',
               selectInput(inputId = "race.prop",
                           label = "Race:",
                           choices = c("White","Black","American Indian/Native","Asian","Hispanic"),
                           selected = "White"),
               plotOutput('race.proportion', height="7.5in")
      ),
      tabPanel('Race by Year (Latitude, Longitude)',
               selectInput(inputId = "race.lat",
                           label = "Race:",
                           choices = c("White","Black","American Indian/Native","Asian","Hispanic"),
                           selected = "White"),
               radioButtons("lat.lon", "Latitude/Longitude:",
                            c("Latitude" = "lat",
                              "Longitude" = "lon")),
               h4('Bandwidth'),
               sliderInput("violin.adjust", label="Adjust",
                           min=1, max=10, value=3, step=0.25),
               plotOutput('race.lat', height="7.5in")
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
                           min=5, max=15, value=9.5, step=0.5),
               sliderInput("age.adjust.y", label="Pop. Percentage",
                           min=0.05, max=2, value=0.25, step=0.05),
               plotOutput('age.race', height="7.5in")
      ),
      tabPanel('Income vs Race',
               h2('Bandwidth'),
               sliderInput("income.adjust.x", label="Income",
                           min=1000, max=30000, value=15000, step=1000),
               sliderInput("income.adjust.y", label="Pop. Percentage",
                           min=0.05, max=2, value=0.25, step=0.05),
               selectInput('income.gender', 'Gender: ',
                           list('Male' = 'male', 'Female' = 'female')),
               selectInput('income.race', 'Race: ',
                           list('White' = 'white', 'Black' = 'black',
                                'American Indian/Native' = 'american.indian',
                                'Asian' = 'asian')),
               plotOutput('income.race', height="7.5in")
      ),
      # Tab Panel Containing Age vs. Population Graphs
      tabPanel('Age vs. Population',
               tabPanel('Age vs. Population',
                        br(),
                        checkboxInput('age_contour', 'Show Population Contour'),
                        br(),
                        plotOutput('age_vs_pop', height="7.5in"),
                        br(),
                        plotOutput('zoomed_in_age')
                        )
      ),
      # Tab Panel Containing Income vs. Population Graphs
      tabPanel('Income vs. Population',
               tabPanel('Income vs. Population',
                        br(),
                        checkboxInput('income_contour', 'Show Population Contour'),
                        br(),
                        plotOutput('income_vs_pop', height="7.5in"),
                        br(),
                        plotOutput('zoomed_in_income')
               )
      ),
        # Tab panel for barplots of household size by race
        tabPanel('Household vs Race',
                  selectInput('hh.race', 'Race: ',
                           list('White' = 'White', 'Black' = 'Black',
                                'Hispanic' = 'Hispanic','Asian' = 'Asian')),
                  plotOutput('hh.race.plot')
                ),
        # Tab panel for race proportion barplots
        tabPanel('Race Proportions',
                  checkboxInput('prop.include', 'Include White and Black', value=TRUE),
                  plotOutput('prop.race.plot')
                )
    )
  )
  )
))
