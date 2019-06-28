library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)

shinyUI(dashboardPage(title = "Recommender system", skin = "red",
    dashboardHeader(title = "Platform dashboard" #,dropdownMenuOutput("msgOutput")
                    ),
    dashboardSidebar(
        sidebarMenu(
        menuItem("Home"),
        menuItem("Dashboard",tabName = "dashboard",icon = icon("dashboard")),
        menuItem("Comparison",tabName = "other"),
        menuItem("Visit-us", icon = icon("send",lib='glyphicon'), href = "https://www.salesforce.com"),
        menuItem("Platform",  radioButtons("pw","Select platform",choices = c("Amazon","Flipkart","Snapdeal")))
    )),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                fluidRow(
                    valueBox(100,"Number of Visits today",icon = icon("bell")),
                    valueBox(100,"Number of Customers",icon = icon("user-tie"),color = "yellow"),
                    valueBox(100,"Today's sales",icon = icon("chart-line"),color = "red")
                ),
                fluidRow(
                    box(title = "Yearly Sales", solidHeader = TRUE, status = "primary",
                        plotlyOutput("graph")
                ),
                box(title = "Retailers with max/min sales", solidHeader = TRUE, status = "primary",
                    sliderInput("mx", "Select number of retailers(max=+ve/min=-ve)", -10,10,0),
                    plotOutput("graph2")
                )
                ),
                
                fluidRow(
                    box(title = "Profits on different items", solidHeader = TRUE, status = "primary",
                        selectInput("ch","Select Category", choices = c("Bottoms","Dresses","Intimates","Jackets","Tops")),
                        plotlyOutput("graph3")
                    ),
                    box(title = "Customer locations", solidHeader = TRUE, status = "primary",
                        plotlyOutput("graph4")
                        )
                )
            ),
            tabItem(tabName = "other",
                    fluidRow(
                        box(title = "Sales comparison across all platforms", solidHeader = TRUE, status = "primary", width = 900, height = 550,
                            plotlyOutput("graph5")
                        )
                    )
            )
      )
    )
)
)