##Loading the necessary libraries 

library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(plotly)

boAt = read.csv("./../../Big_Frame.csv" )
numeric_boAt <- boAt %>% select_if(is.numeric)

amazon = read.csv("./../../Amazon_earbuds.csv")
amazon$AiredDate <- as.Date(amazon$AiredDate, format = "%d %B %Y")
##########################
##### User interface #####
##########################
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  

  
  navbarPage(
    "Decoding 'boAt' Lifestyle",
    tabPanel(
      "Home",
      id = "Home",
      class = "background-image",
      sidebarLayout(
        sidebarPanel(
          h3("We present here analysis of boAt  dataset.", style = "padding-bottom: 20px")
        ),
        mainPanel(
          h4(" Dataset posted on 05/06/2023, scrapped from the official website"),
          a(href = "https://www.boat-lifestyle.com/", "Click here"),
          h5("Summary of boAt dataset:"),
          tags$li("ProductRating : Average customer satisfaction score"),
          tags$li("ReviewCount : No. of sustomer reviews"),
          tags$li("Sale_Price : Currrent selling price"),
          tags$li("Regular_Price : Originsl retail price without offers"),
          tags$li("Discount_percentage : Percentage by which regular price has been reduced"),
          tags$li("ColourOptions : Colour options available for the product"),
          tags$li("ProductName : Name of the product"),
          tags$li("Type : Specific classification of the product on website"),
          tags$li("Category : Wired or wireless electronic device"),
          tags$li("Warranty : Warranty offered by boAt"),
          tags$li("Charging_Time : Time duration required for the product to get fully charged"),
          tags$li("Charge_Duration : Operational time of the product after getting fully charged "),
          tags$li("Country_Of_Origin : Country where product in manufactured")
          
          
        )
      )
    ),
    tabPanel(
      "Scatterplots",
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "scpl_x",
                      label = "X axis(Numerical):",
                      choices = names(numeric_boAt),
                      selected = "Regular_Price"),
          selectInput(inputId = "scpl_y",
                      label = "Y axis(Numerical): ",
                      choices = names(numeric_boAt),
                      selected = "Sale_Price"),
          fluidRow(
            column(6,
                   radioButtons(inputId = "scpl_size",
                                label = "Size",
                                choices = c("ProductRating",
                                            "ReviewCount",
                                            "Sale_Price",
                                            "Regular_Price",
                                            "Discount_percentage",
                                            "ColourOptions",
                                            "Warranty",
                                            "Charging_Time",
                                            "Charging_Duration"))
            ),
            column(6,
                   radioButtons(inputId = "scpl_color",
                                label = "Color",
                                choices = c("Country_Of_Origin",
                                            "Category",
                                            "Type",
                                            "ProductRating",
                                            "ReviewCount",
                                            "Sale_Price",
                                            "Regular_Price",
                                            "Discount_percentage",
                                            "ColourOptions",
                                            "Warranty",
                                            "Charging_Time",
                                            "Charging_Duration"),
                                selected = "Category"),
                   checkboxInput(inputId = "color.isCategorical",
                                 label = "Categorical",
                                 value = FALSE)
            )
          ),
          radioButtons(inputId = "scpl_shape",
                       label = "Shape",
                       choices = c("Country_Of_Origin",
                                   "Category",
                                   "Type"),
                       selected = "Country_Of_Origin")
        ),
        mainPanel(
          plotOutput(outputId = "scatterplot")
        )
      )
    ),
    tabPanel(
      "Histogram",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "hist_x",
            'Choose a Variable(Numerical):',
            choices = names(numeric_boAt),
            selected = "ProductRating",
            multiple = FALSE,
            selectize = TRUE
          ),
          sliderInput(inputId = "hist_bins",
                      label = "Select bins:",
                      min = 5,
                      max = 40,
                      value = 15),
          textInput(inputId = "hist_col",
                    label = "Enter  color of histogram",
                    value = "pink")
          
        ),
        mainPanel(
          plotOutput(outputId = "histogram")
        )
      )
    ),
    tabPanel("PieCharts" ,
             sidebarLayout(
               sidebarPanel(
                 h4("Categorywise Pie charts and Summary statistics") ##This not working ?!
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Color Options Distribution", plotOutput("pieChart")),
                   tabPanel("Type Distribution", plotOutput("typePieChart")),
                   tabPanel("Summary Statistics", verbatimTextOutput("summaryStats"))
                 )
               )
             )
    )
    
  )
)





# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ##SCATTERPLOT : 
  
  scatterplot_selectedData <- reactive({
    df <- boAt[, c(input$scpl_x, input$scpl_y, input$scpl_size, input$scpl_color, input$scpl_shape), drop = FALSE]
    return(df)
  })
  
  output$scatterplot <- renderPlot({
    scatterplot_data <- scatterplot_selectedData()
    
    ggplot(scatterplot_data, aes(x = scatterplot_data[, 1], y = scatterplot_data[, 2], color = scatterplot_data[, 4], size = scatterplot_data[, 3], shape = scatterplot_data[, 5])) +
      geom_point() +
      labs(x = colnames(scatterplot_data)[1], y = colnames(scatterplot_data)[2]) +
      theme_minimal() +
      xlim(range(scatterplot_data[, 1], na.rm = TRUE)) +
      ylim(range(scatterplot_data[, 2], na.rm = TRUE)) +
      guides(
        shape = guide_legend(title = input$scpl_shape),
        color = guide_legend(title = input$scpl_color),
        size = guide_legend(title = input$scpl_size)
      )
  })
  
  
  
  ## HISTOGRAM : 
  
  # HISTOGRAM
  
  hist_selected <- reactive({
    selected_column <- input$hist_x
    boAt[, selected_column, drop = FALSE]
  })
  
  output$histogram <- renderPlot({
    # Get data for the histogram
    df <- hist_selected()
    
    # Create a ggplot histogram using aes_string
    ggplot(df, aes_string(x = df[,1])) +
      geom_histogram(bins = input$hist_bins, color = input$hist_col) +
      labs(title = paste("Histogram of", input$hist_x),
           x = input$hist_x,
           y = "Frequency") +
      theme_minimal() +
      theme(legend.position = "none")  + # Remove legend for single-variable histograms
      scale_fill_brewer(palette = "Set3")  # Use a color palette for better distinction
  })
  
  
  
  ## Pie Chart 
  output$pieChart <- renderPlot({
    colorops <- table(boAt$ColourOptions)
    labels <- paste(names(colorops), "(", colorops, ")", sep = " ")
    colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#984EA3", "#FFD700")
    
    pie(colorops, labels = labels, col = colors, main = "Color Options Distribution")
  })
  
  # Pie chart for type distribution
  output$typePieChart <- renderPlot({
    type_counts <- table(boAt$Type)
    labels <- paste(names(type_counts), "(", type_counts, ")", sep = " ")
    colors <- rainbow(length(type_counts))
    
    pie(type_counts, labels = labels, col = colors, main = "Type Distribution")
  })
  
  # Summary statistics
  output$summaryStats <- renderText({
    summary_text <- paste("Total Products: ", nrow(boAt))
    summary_text <- paste(summary_text, "\nAverage Discount Percentage: ", round(mean(boAt$Discount_percentage), 2), "%" )
    summary_text <- paste(summary_text, "\nAverage Sale Price: \u20B9", round(mean(boAt$Sale_Price), 2))
    summary_text <- paste(summary_text, "\nMaximum Review Count: ", max(boAt$ReviewCount))
    
    summary_text
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
