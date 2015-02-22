library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    titlePanel("Developing Data Products - Project"),
    
    sidebarLayout(
        sidebarPanel(h2("Daily Activity Pattern Exploration"),
                    br(),
                    p("With this app you can plot data from a personal activity monitoring device which collects data in 5-minute intervals during the day to record the number of steps the bearers take [1]."),
                    selectInput("dayname","Choose the day or time period to display:",                                c("Monday"="mon","Tuesday"="tue","Wednesday" ="wed", "Thursday"="thu", "Friday"="fri", "Weekday"="wkday","Weekend"="wkend")),
                    br(),
                    sliderInput("meanguess",label=h4("Guess Mean per Interval"),min=10, max=150,value=75,step=5),
                    br(),
                    numericInput('sumguess', 'Total number of steps taken', 10000, min = 5000, max = 100000, step = 100),
                    #submitButton('Submit'),
                    br(),
                    br(),
                    h5("[1] The data is from the programming assignment for the Reproducible Research class in the Data Science track.")
                    ),
        mainPanel(

            #textOutput("checktext"),
            h2("Instructions:"), 
            p("Select a day to plot the step count throughout the day and the histogram of steps within the day.  You can also select 'weekday' or 'weekend.'"),
            p("Then guess the average number of steps taken during that day or collection of days.  The plots will display your guess and how far off it is from the true mean as the mean square error (MSE)."),
            p("You can also guess the total number of steps depicted in the plot.  The line under the plot will display your guess and how far it is from the true value."),
            br(),
            plotOutput("myplot1"),
            plotOutput("myplot2"),
            textOutput("text1")
            
        )
    )
))
