#########################################################################
####    shiny::runGitHub("vvrzheshch-hw3", "usfviz", subdir = "")    ####
#########################################################################

rm(list = ls())
cat("\014")

library(shiny)
library(plotly)
library(plyr)
library(reshape2)
library(heatmaply)
library(pairsD3)
library(ggplot2)
library(GGally)

# setwd('/Users/vv/Google Drive/MSAN2017/SPRING_2017/MSAN-622-02_Data_and_Information_Visualization/vvrzheshch-hw3/')
df <- read.csv('dataset_Facebook.csv', sep=';')

# Define UI for application that draws a histogram
ui <-pageWithSidebar(
  headerPanel("Valentin Vrzheshch HW3"),
    sidebarPanel(
      selectInput("hm.x", label = h4("Select column for x-axis"), choices = names(df), selected = 'Type'),
      selectInput("hm.y", label = h4("Select column for y-axis"), choices = names(df), selected = 'Paid'),
      selectInput("hm.z", label = h4("Select column for color"), choices = names(df), selected = 'like')
    ),
      
      # Show a plot of the generated distribution
    mainPanel(
      navbarPage("",
        tabPanel("Heatmap",
          plotlyOutput("distPlot"),
          verbatimTextOutput("Summary")),
        tabPanel("Scatterplot Matrix", pairsD3Output("scatterplot", width = '800', height = '450')),
        tabPanel("Parallel Coordinates Plot", plotOutput("par_plot"))
      )
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  sub_df <- reactive({
    acast(
      na.omit(df[, c(input$hm.x, input$hm.y, input$hm.z)]),
      paste0(input$hm.x, ' ~ ', input$hm.y), value.var = input$hm.z)
  })
  
  output$distPlot <- renderPlotly({
    heatmaply(sub_df(), k_col = 2, k_row = 2, na.value = "grey50") %>%
      layout(margin = list(l = 130, b = 40))
    })

   
   output$Summary <- renderPrint({sub_df()})

   output$scatterplot <- renderPairsD3({
     df_2 <- sub_df()
     pairsD3(df_2, group = df_2[,2])
  })
   
   output$par_plot <- renderPlot({
     df_2 <- df[, c(input$hm.x, input$hm.y, input$hm.z)]
     ggparcoord(data = df_2, scale = 'uniminmax', groupColumn = input$hm.z)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

