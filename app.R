#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("ggplot2")
library("ggthemes")
library("openxlsx")
library("readxl")
library("readr")
library('reshape2')
library('sendmailR')
library("rmarkdown")
library("bslib")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = bs_theme(version = 5, bootswatch = 'minty'),
  
  # Application title
  titlePanel("Film curves plotting"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      dateInput("test_date", "Date of test:"),
      textInput("film_name", "Film name:"),
      selectInput("film_type", "Type of film:",
                  c("B&W negative",
                    "B&W Pyro negative",
                    "X-ray film blue",
                    "X-ray film green",
                    "Cinematic ECN-2",
                    "Color negative C-41",
                    "Color positive E-6")
      ),
      textInput("dev_name", "Developer:"),
      textInput("dev_dil", "Dilution:"),
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
                   c("~ E.I. 100 (0.0025 lx's)" = "100",
                     "~ E.I. 200 (0.00125 lx's)" = "200",
                     "~ E.I. 400 (0.00063 lx's)" = "400")),
      radioButtons("x-scale", "X scale",
                   c("Log exposure (LogH)" = "LogH",
                     "Exposure (lux per sec.)" = "LuxS")),
      
      textAreaInput("film_annotation", "Add. text on plot:"),
      textInput("test_conductor", "The test was conducted by:"),
      checkboxInput("curve_smooth", "Smooth curves (dot off)", value = TRUE),
      checkboxInput("fgei","Show E.I. by ΔX (FG, test)",value = FALSE),
      tags$hr(),
      
      fileInput("film_data", "Testing data (*.xlsx):", 
                accept = c(".xlsx")),
      width = 3,
      downloadButton("download_button")
    ),
    
    # Show a plot of the generated distribution
    mainPanel( "Please set parameters (left panel) and upload *.xlsx datafile.",
               "You can download a template here: www.itcp.kz/film_templ.xlsx", tags$br(),
               "Warning! Each film test must be in separate column. The header of each column must contains a time of developing and some short text (if needed).", tags$br(),
               tabsetPanel(
                 tabPanel("Family plot", tableOutput("TestResult"), tags$hr(),
                          plotOutput("distPlot", width = 1024, height = 768), tags$hr(),
                          "This project source code is available on github: https://github.com/bnxvs/cc_film_plotter"),
                 tabPanel("Dataset", tableOutput("dataTable"), width = 9)
               )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  data <- reactive({
    film <- input$film_data
    
    if(is.null(film))
      return(NULL)
    file.rename(film$datapath,
                paste(film$datapath, ".xlsx", sep=""))
    film_data <- read_excel(paste(film$datapath, ".xlsx", sep=""), 1)
    return(film_data)
  })
  
  output$dataTable <- renderTable({
    data()
  })
  
  
  # **********************************************************************  

  round_math <- function(x, digits = 0) {
    posneg <- sign(x)  # Определяем знак числа
    z <- abs(x) * 10^digits
    z <- z + 0.5  # Добавляем 0.5 для округления вверх
    z <- floor(z)
    return(posneg * z / 10^digits)
  }

  
  
  test_res <- function() {
    req(data())
    df <- data()
    CIs <- c()
    ISOs <- c()
    ISOfg <- c()
    ISO09s <- c()
    Ran <- c()
    Dmi <- c()
    Dmx <- c()
    dD <- c()
    Dlb <- c()
    Gamma <- c()
    heads <- c(names(df))
    
    if (input$ei_d == "100") {
      df$D <- seq(-2.60206, 0.40824, length.out=21)
    } else if (input$ei_d == "200") {
      df$D <- seq(-2.90309, 0.10721, length.out=21)
    } else if (input$ei_d == "400") {
      df$D <- seq(-3.20412, -0.19382, length.out=21)
    }
    
    ln <- length(names(df))
    for (i in 2:ln-1) {
      # ********************************************************************      
      axisx <- df[[i]] #какая пленка отображается
      axisy <- df$D
      
      # ********************************************************************
      iso_point <- approx(x = axisx, 
                          y = axisy, 
                          xout = axisx[1] + 0.1)
      x = as.numeric(iso_point[2]) #пишем определенные координаты Х в переменную
      
      iso <- round_math(0.8/10^x, digits = 0) #рассчитываем ISO
      
      # по критерию 0.9
      iso_point_09 <- approx(x = axisx, 
                             y = axisy, 
                             xout = axisx[1] + 0.6)
      x09 = as.numeric(iso_point_09[2])
      #      temp_iso <- 10^x09 #Контрольная проверка ИСО
      
      iso09 <- round_math(10/10^x09, digits = 0)
      
      #определяем координаты точки +1,3Х по оси Y для расчета дельты D
      gamma_point <- approx(x = axisy,
                            y = axisx,
                            xout = as.numeric(iso_point[2]) + 1.3)
      gamma_point_x <- as.numeric(axisx[1] + 0.1) #пишем определенные координаты Y в переменную
      
      g_point <- as.numeric(gamma_point[2]) #приводим к цифровому формату
      
      g <- round_math((g_point - gamma_point_x) / 1.3, digits = 2) #определяем средний градиент
      
      range_point <- approx(x = axisx,
                            y = axisy,
                            xout = as.numeric(iso_point[1]) + 1.2)
      r <- as.numeric(range_point[2])
      
      range_lenght <- round_math(abs(x - r), digits = 2)
      
      dmin <- round_math(axisx[1], digits = 2)
      dmax <- round_math(max(axisx), digits = 2)
      
      gam_start_point <- approx(x = axisy, 
                                y = axisx, 
                                xout = x + 0.1)
      gam_x <- as.numeric(gam_start_point[1]) #пишем определенные координаты Х в переменную
      gam <- round_math((g_point - gamma_point_x) / 
                     (as.numeric(gamma_point[1]) - gam_x), digits = 2)
      
      del_b <- round_math(3.3 / gam, digits = 1)
      
      deltaD <- round_math(g_point - as.numeric(iso_point[1]), digits = 2)
      dD <- c(dD, deltaD)
      
      # чуйка по методике FG
      delta_x <-  round_math(0.83 - ((0.86*deltaD) + (0.24*(deltaD^2))), digits = 2)
      iso_point_fg <- approx(x = axisx, 
                             y = axisy, 
                             xout = axisx[1] + 0.1 - (abs(delta_x)*-1))
      x_fg = as.numeric(iso_point_fg[2]) #пишем определенные координаты Х в переменную
      
      iso_fg <- round_math(1/10^x_fg, digits = 2) #рассчитываем E.I. Fractional Gradient
      
      
      Dmi <- c(Dmi, dmin)
      Dmx <- c(Dmx, dmax)
      Ran <- c(Ran, range_lenght)
      CIs <- c(CIs, g)
      ISOs <- c(ISOs, iso)
      ISOfg <- c(ISOfg, iso_fg)
      ISO09s <- c(ISO09s, iso09)
      Dlb <- c(Dlb, del_b)
      Gamma <- c(Gamma, gam)
      
    }
    if(input$fgei == T) {test_result <- data.frame("Time & note" = heads, "E.I.('iso') b/f + 0.1" = ISOs, "E.I. b/f + 0.6" = ISO09s,"E.I. by ΔX (FG)" = ISOfg,
                                                   "CI (g)" = CIs, "γ" = Gamma, "Range 'L' (ΔD1.2)" = Ran, "∆B" = Dlb,
                                                   "D min." = Dmi, "D max." = Dmx, "ΔD (1.3 LogH)" = dD, check.names=FALSE)}
    else {test_result <- data.frame("Time & note" = heads, "E.I.('iso') b/f + 0.1" = ISOs, "E.I. b/f + 0.6" = ISO09s,
                                    "CI (g)" = CIs, "γ" = Gamma, "Range 'L' (ΔD1.2)" = Ran, "∆B" = Dlb,
                                    "D min." = Dmi, "D max." = Dmx, check.names=FALSE)}
    
  }
  
  output$TestResult <- renderTable({
    test_res()
  })
  
  
  # **********************************************************************
  
  draw_plot <- function() {
    req(data())
    df <- data()
    CIs <- c()
    ISOs <- c()
    iso_x_point <- c()
    iso_y_point <- c()
    if (input$ei_d == "100") {
      df$D <- seq(-2.60206, 0.40824, length.out=21)
    } else if (input$ei_d == "200") {
      df$D <- seq(-2.90309, 0.10721, length.out=21)
    } else if (input$ei_d == "400") {
      df$D <- seq(-3.20412, -0.19382, length.out=21)
    }
    
    melted = melt(df, id.vars="D")
    ln <- length(names(df))
    for (i in 2:ln-1) {
      # ********************************************************************      
      axisx <- df[[i]] #какая пленка отображается
      axisy <- df$D
      
      # ********************************************************************
      iso_point <- approx(x = axisx, 
                          y = axisy, 
                          xout = axisx[1] + 0.1)
      x = as.numeric(iso_point[2]) #пишем определенные координаты Х в переменную
      
      iso_x_point <- c(iso_x_point, iso_point[1])
      iso_y_point <- c(iso_y_point, iso_point[2])
      
      iso <- round_math(0.8/10^x, digits = 0) #рассчитываем ISO
      
      #определяем координаты точки +1,3Х по оси Y для расчета дельты D
      gamma_point <- approx(x = axisy,
                            y = axisx,
                            xout = as.numeric(iso_point[2]) + 1.3)
      gamma_point_x <- as.numeric(axisx[1] + 0.1) #пишем определенные координаты Y в переменную
      
      g_point <- as.numeric(gamma_point[2]) #приводим к цифровому формату
      
      g <- round_math((g_point - gamma_point_x) / 1.3, digits = 2) #определяем средний градиент
      
      CIs <- c(CIs, g)
      ISOs <- c(ISOs, iso)
    }
    
    if (input$agitation == 1) {
      regim <- c("machine processing")
    } else if (input$agitation == 2) {
      regim <- c("manual agitation")
    } else if (input$agitation == 3) {
      regim <- c("manual continuous agitation")
    } else if (input$agitation == 4) {
      regim <- c("without agitation (stand dev.)")
    }
    # ********************************************************************
    if(input$`x-scale` == "LuxS") {
      ggplot() + 
        {if(input$curve_smooth == T) geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), data=melted, 
                                                 aes(x = D, y = value, group = variable, color=variable), se = F)}+
        {if(input$curve_smooth == F) geom_line(data = melted, aes(x = D, y = value, group = variable, 
                                                                  color = variable), se = F)}+
        #        geom_point(data = melted, aes(x = D, y = value, group = variable, color = variable), 
        #                     se = F)+
        coord_fixed()+
        geom_hline(yintercept = as.numeric(iso_x_point), color = "lightgrey", linetype = "dashed")+
        geom_vline(xintercept = as.numeric(iso_y_point), color = "lightgrey", linetype = "dashed")+
        theme_linedraw()+
        ggtitle(paste("Family plot: ", input$film_name, " ", input$film_type, " in ", input$dev_name, " ", input$dev_dil,
                      "\n", regim, ", ", input$temperature,"°C", 
                      "\n", sep = ""))+
        theme(plot.title = element_text(color = "gray33", size = 18), legend.position = "right")+
        xlab("Lux per sec") +
        ylab("Density (D)")+
        scale_colour_discrete(name='Development times:', 
                              labels = paste(names(df), ", CI:", CIs,", EI 0.1:",ISOs)) + 
        
        theme(axis.title.x = element_text(colour = "orange4", size = 14),
              axis.title.y = element_text(colour = "orange4", size = 14),
              axis.text = element_text(size = 12),
              legend.background = element_rect(fill = "honeydew1"),
              legend.text=element_text(size=14))+
        geom_text(aes(-3, 2.4, label=paste(input$film_annotation, "\n", input$test_conductor, " ", input$test_date),
                      hjust = "inward", family="Helvetica", fontface="italic"),
                  colour = "black")+    
        scale_x_continuous(breaks = c(-3.5,-3.2,-2.9,-2.6,-2.3,-2.0,-1.7,-1.4,
                                      -1.1,-0.8,-0.49,-0.19,0.11,0.41),
                           limits = c(-3.5,0.7), 
                           labels = c(0.000315,0.00063,0.00125,0.0025,0.005,0.01,
                                      0.02,0.04,0.08,0.16,0.32,0.63,1.25,2.5),
                           expand = c(0, 0))+
        scale_y_continuous(breaks = seq(0,3.0,0.2),
                           limits = c(0,3.0),
                           expand = c(0, 0))
    } else if(input$`x-scale` == "LogH") {
      ggplot() + 
        {if(input$curve_smooth == T) geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), data=melted, 
                                                 aes(x = D, y = value, group = variable, color=variable), se = F)}+
        {if(input$curve_smooth == F) geom_point(data = melted, aes(x = D, y = value, group = variable, 
                                                                   color = variable), se = F)}+
        {if(input$curve_smooth == F) geom_line(data = melted, aes(x = D, y = value, group = variable, 
                                                                  color = variable), se = F)}+
        #              geom_point(data = melted, aes(x = D, y = value, group = variable, color = variable), 
        #                         se = F)+
        coord_fixed()+
        #        geom_hline(yintercept = as.numeric(iso_x_point), color = "lightgrey", linetype = "dashed")+
        #        geom_vline(xintercept = as.numeric(iso_y_point), color = "lightgrey", linetype = "dashed")+
        theme_linedraw()+
        ggtitle(paste("Family plot: ", input$film_name, " ", input$film_type, " in ", input$dev_name, " ", input$dev_dil,
                      "\n", regim, ", ", input$temperature,"°C", 
                      "\n", sep = ""))+
        theme(plot.title = element_text(color = "gray33", size = 18), legend.position = "right")+
        xlab("Log exposure (logH)") +
        ylab("Density (D)")+
        scale_colour_discrete(name='Development times:', 
                              labels = paste(names(df), ", CI:", CIs,", EI 0.1:",ISOs)) + 
        
        theme(axis.title.x = element_text(colour = "orange4", size = 14),
              axis.title.y = element_text(colour = "orange4", size = 14),
              axis.text = element_text(size = 12),
              legend.background = element_rect(fill = "honeydew1"),
              legend.text=element_text(size=14))+
        geom_text(aes(-3, 2.4, label=paste(input$film_annotation, "\n", input$test_conductor, " ", input$test_date),
                      hjust = "inward", family="Helvetica", fontface="italic"),
                  colour = "black")+    
        scale_x_continuous(breaks = c(-3.5,-3.2,-2.9,-2.6,-2.3,-2.0,-1.7,-1.4,
                                      -1.1,-0.8,-0.49,-0.19,0.11,0.41),
                           limits = c(-3.5,0.7), 
                           #                             labels = c(0.00063,0.00125,0.0025,0.005,0.01,
                           #                                        0.02,0.04,0.08,0.16,0.32,0.63,1.25,2.5),
                           expand = c(0, 0))+
        scale_y_continuous(breaks = seq(0,3.0,0.2),
                           limits = c(0,3.0),
                           expand = c(0, 0))
    }
  }
  
  output$distPlot <- renderPlot({
    draw_plot()
  })
  
  # **********************************************************************
  output$download_button <- downloadHandler(
    filename = function() {
      paste(Sys.Date(),'_',input$film_name,'_',input$dev_name, ".html", sep="")},
    content = function(file) {
      res <- rmarkdown::render(
        "template.Rmd",
        params = list(
          draw_plot = draw_plot,
          test_result = test_res)
      )
      file.rename(res, file)
    }
  )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
