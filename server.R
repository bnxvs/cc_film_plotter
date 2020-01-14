# https://bnxvs.shinyapps.io/cc_film_plot/
#
# rsconnect::deployApp('Shiny/CC_film_Plot')
#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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

#source("Plotting_film.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

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
    
  output$distPlot <- renderPlot({
      req(data())
      df <- data()
      CIs <- c()
      ISOs <- c()
      iso_x_point <- c()
      iso_y_point <- c()
      if (input$ei_d == "100") {
        df$D <- c(-2.60206,-2.45154,-2.30103,-2.15051,
                  -2.00000,-1.84949,-1.69897,-1.54846,
                  -1.39794,-1.24743,-1.09691,-0.94640,
                  -0.79588,-0.64537,-0.49485,-0.34434,
                  -0.19382,-0.04331,0.10721,0.25772,0.40824)
      } else if (input$ei_d == "200") {
        df$D <- c(-2.90309, -2.75257, -2.60206, -2.45154, 
                  -2.30103, -2.15051, -2.00000, -1.84949, 
                  -1.69897, -1.54846, -1.39794, -1.24743, 
                  -1.09691, -0.94640, -0.79588, -0.64537, 
                  -0.49485, -0.34434, -0.19382, -0.04331, 0.10721)
      } else if (input$ei_d == "400") {
        df$D <- c(-3.50515, -3.35463, -3.20412, -3.05360, 
                  -2.90309, -2.75257, -2.60206, -2.45154, 
                  -2.30103, -2.15051, -2.00000, -1.84949, 
                  -1.69897, -1.54846, -1.39794, -1.24743, 
                  -1.09691, -0.94640, -0.79588, -0.64537, -0.49485)
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
      
      iso <- round(0.8/10^x, digits = 0) #рассчитываем ISO
      
      #определяем координаты точки +1,3Х по оси Y для расчета дельты D
      gamma_point <- approx(x = axisy,
                            y = axisx,
                            xout = x + 1.3)
      gamma_point_x <- as.numeric(axisx[1] + 0.1) #пишем определенные координаты Y в переменную
      
      g_point <- as.numeric(gamma_point[2]) #приводим к цифровому формату
      
      g <- round((g_point - gamma_point_x) / 1.3, digits = 2) #определяем средний градиент
      
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
#        geom_hline(yintercept = as.numeric(iso_x_point), color = "lightgrey", linetype = "dashed")+
#        geom_vline(xintercept = as.numeric(iso_y_point), color = "lightgrey", linetype = "dashed")+
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
                      colour = "gray60")+    
        scale_x_continuous(breaks = c(-3.2,-2.9,-2.6,-2.3,-2.0,-1.7,-1.4,
                                      -1.1,-0.8,-0.49,-0.19,0.11,0.41),
                           limits = c(-3.3,0.408), 
                           labels = c(0.00063,0.00125,0.0025,0.005,0.01,
                                      0.02,0.04,0.08,0.16,0.32,0.63,1.25,2.5),
                           expand = c(0, 0))+
        scale_y_continuous(breaks = seq(0,3.0,0.2),
                           limits = c(0,3.0),
                           expand = c(0, 0))
      } else if(input$`x-scale` == "LogH") {
        ggplot() + 
          {if(input$curve_smooth == T) geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), data=melted, 
                                                   aes(x = D, y = value, group = variable, color=variable), se = F)}+
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
                    colour = "gray60")+    
          scale_x_continuous(breaks = c(-3.2,-2.9,-2.6,-2.3,-2.0,-1.7,-1.4,
                                        -1.1,-0.8,-0.49,-0.19,0.11,0.41),
                             limits = c(-3.3,0.408), 
#                             labels = c(0.00063,0.00125,0.0025,0.005,0.01,
#                                        0.02,0.04,0.08,0.16,0.32,0.63,1.25,2.5),
                             expand = c(0, 0))+
          scale_y_continuous(breaks = seq(0,3.0,0.2),
                             limits = c(0,3.0),
                             expand = c(0, 0))
      }
         })
  
# **********************************************************************  
  output$TestResult <- renderTable({
    req(data())
    df <- data()
    CIs <- c()
    ISOs <- c()
    ISO09s <- c()
    Ran <- c()
    Dmi <- c()
    Dmx <- c()
    Dlb <- c()
    Gamma <- c()
    heads <- c(names(df))
    
    if (input$ei_d == "100") {
      df$D <- c(-2.60206,-2.45154,-2.30103,-2.15051,
                -2.00000,-1.84949,-1.69897,-1.54846,
                -1.39794,-1.24743,-1.09691,-0.94640,
                -0.79588,-0.64537,-0.49485,-0.34434,
                -0.19382,-0.04331,0.10721,0.25772,0.40824)
    } else if (input$ei_d == "200") {
      df$D <- c(-2.90309, -2.75257, -2.60206, -2.45154, 
                -2.30103, -2.15051, -2.00000, -1.84949, 
                -1.69897, -1.54846, -1.39794, -1.24743, 
                -1.09691, -0.94640, -0.79588, -0.64537, 
                -0.49485, -0.34434, -0.19382, -0.04331, 0.10721)
    } else if (input$ei_d == "400") {
      df$D <- c(-3.50515, -3.35463, -3.20412, -3.05360, 
                -2.90309, -2.75257, -2.60206, -2.45154, 
                -2.30103, -2.15051, -2.00000, -1.84949, 
                -1.69897, -1.54846, -1.39794, -1.24743, 
                -1.09691, -0.94640, -0.79588, -0.64537, -0.49485)
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
      
      iso <- round(0.8/10^x, digits = 0) #рассчитываем ISO
      
      # по критерию 0.9
      iso_point_09 <- approx(x = axisx, 
                             y = axisy, 
                             xout = axisx[1] + 0.9)
      x09 = as.numeric(iso_point_09[2])
#      temp_iso <- 10^x09 #Контрольная проверка ИСО

      iso09 <- round(10/10^x09, digits = 0)
      
      #определяем координаты точки +1,3Х по оси Y для расчета дельты D
      gamma_point <- approx(x = axisy,
                            y = axisx,
                            xout = x + 1.3)
      gamma_point_x <- as.numeric(axisx[1] + 0.1) #пишем определенные координаты Y в переменную
      
      g_point <- as.numeric(gamma_point[2]) #приводим к цифровому формату
      
      g <- round((g_point - gamma_point_x) / 1.3, digits = 2) #определяем средний градиент
      
      range_point <- approx(x = axisx,
                            y = axisy,
                            xout = as.numeric(iso_point[1]) + 1.2)
      r <- as.numeric(range_point[2])
      
      range_lenght <- round(abs(x - r), digits = 2)
      
      dmin <- round(axisx[1], digits = 2)
      dmax <- round(max(axisx), digits = 2)
      
      gam_start_point <- approx(x = axisy, 
                                y = axisx, 
                                xout = x + 0.1)
      gam_x <- as.numeric(gam_start_point[1]) #пишем определенные координаты Х в переменную
      gam <- round((g_point - gamma_point_x) / 
                     (as.numeric(gamma_point[1]) - gam_x), digits = 2)
      
      del_b <- round(3.3 / gam, digits = 1)
      
      Dmi <- c(Dmi, dmin)
      Dmx <- c(Dmx, dmax)
      Ran <- c(Ran, range_lenght)
      CIs <- c(CIs, g)
      ISOs <- c(ISOs, iso)
      ISO09s <- c(ISO09s, iso09)
      Dlb <- c(Dlb, del_b)
      Gamma <- c(Gamma, gam)
      
    }
    test_result <- data.frame("Time & note" = heads, "E.I.('iso') b/f + 0.1" = ISOs, "E.I. b/f + 0.9" = ISO09s,
                              "CI (g)" = CIs, "γ" = Gamma, "Range 'L' (ΔD1.2)" = Ran, "∆B" = Dlb,
                              "D min." = Dmi, "D max." = Dmx, check.names=FALSE)
  })

})
