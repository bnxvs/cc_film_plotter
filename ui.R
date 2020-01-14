#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library("ggthemes")
library("openxlsx")
library("readxl")

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Film curves plotting"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            dateInput("test_date", "Date of test:"),
            textInput("film_name", "Film name:"),
            textInput("film_type", "Type of film:"),
            textInput("dev_name", "Developer:"),
            textInput("dev_dil", "Dilution:", "stock"),
            radioButtons("agitation", "Agitation mode:",
                         c("Machine rot." = 1,
                           "Manual" = 2,
                           "Manual contin." = 3,
                           "Stand dev." = 4)),
            numericInput("temperature",
                      "Temperature:",
                      value = 20,
                      min = 15,
                      max = 45),
            radioButtons("ei_d", "Sensitometer setup:",
                        c("~ E.I. 100 (0.00063 lx's)" = "100",
                          "~ E.I. 200 (0.00125 lx's)" = "200",
                          "~ E.I. 400 (0.0025 lx's)" = "400")),
            radioButtons("x-scale", "X scale",
                         c("Log exposure (LogH)" = "LogH",
                           "Exposure (lux per sec.)" = "LuxS")),
            
            textAreaInput("film_annotation", "Add. text on plot:"),
            textInput("test_conductor", "The test was conducted by:"),
            checkboxInput("curve_smooth", "Smooth curves (dot off)"),
            tags$hr(),
            
            fileInput("film_data", "Testing data (*.xlsx):", 
                      accept = c(".xlsx")),
            width = 3
        ),

        # Show a plot of the generated distribution
        mainPanel( "Please set parameters (left panel) and upload *.xlsx datafile.",
                   "You can download a template here: www.itcp.kz/film_templ.xlsx", tags$br(),
                   "Warning! Each film test must be in separate column. The header of each column must contains a time of developing and some short text (if needed).", tags$br(),
          tabsetPanel(
          tabPanel("Family plot", tableOutput("TestResult"), tags$hr(),
                   plotOutput("distPlot", width = 1024, height = 768)),
          tabPanel("Dataset", tableOutput("dataTable"), width = 9)
          )
    )
)))