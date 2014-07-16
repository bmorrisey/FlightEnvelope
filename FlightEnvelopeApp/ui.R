library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Flight Envelope"),
  
  sidebarLayout(
    
    sidebarPanel(
      
#       h=input$h
#       W=input$W
#       CLmax=input$CLmax
#       Sref=input$Sref
#       DesignLoadFactor=input$DesignLoadFactor
#       Vne=input$Vne
#       Cla=input$Cla
#       mean_geom_chord=input$mean_geom_chord
      
      numericInput("h", 
                   label = "Analysis Altitude (ft)", 
                   value = 0),
      numericInput("W", 
                   label = "Aircraft Weight (lbs)", 
                   value = 10000),
      numericInput("Sref", 
                   label = "Wing Reference Area (sq ft)", 
                   value = 1000),
      numericInput("mean_geom_chord", 
                   label = "Mean Geom Chord (ft)", 
                   value = 12),
      numericInput("Vne", 
                   label = "Limit Airspeed (kts@SL)", 
                   value = 225),
      numericInput("Cla", 
                   label = "Lift Curve Slope (deg-1)", 
                   value = .08),
      sliderInput("CLmax", "CL Envelope",
                  min = -1.0, max = 2.0, step=0.1, value = c(-0.5,1.1)),
      sliderInput("DesignLoadFactor", "Design Load Limits (g)",
                  min = -2.0, max = 5.0, step=0.1, value = c(-1.5,4))
      
      
    ),
    
    mainPanel(
      plotOutput("flightEnvelope")
      
    )
    
  )
  
))


