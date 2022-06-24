library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("NKE WiSense TD output file to COPAL readable file"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Select NKE .csv file",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # # Input: Checkbox if file has header ----
      # checkboxInput("header", "Header", TRUE),
      # 
      # # Input: Select separator ----
      # radioButtons("sep", "Separator",
      #              choices = c(Comma = ",",
      #                          Semicolon = ";",
      #                          Tab = "\t"),
      #              selected = ","),
      # 
      # # Input: Select quotes ----
      # radioButtons("quote", "Quote",
      #              choices = c(None = "",
      #                          "Double Quote" = '"',
      #                          "Single Quote" = "'"),
      #              selected = ''),

      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")

      # # Horizontal line ----
      # tags$hr(),
      # 
      # # Download button
      # downloadButton("downloadData", "Download")

    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Download data ----
      h4("Table"),
      downloadButton("downloadData", "Download Table"),
      
      # Output: Data file ----
      tableOutput("contents"),

      # Horizontal line ----
      tags$hr(),
      
      # Output: Plot ----
      h4("Plot"),
      plotOutput("plot")
      
    )
    
  )
)


# Define server logic to read selected file ----
server <- function(input, output) {
  
  # Stage 1 function ----
  stage1 <- function(input){
    # Read csv file ----
    df0 <- read.csv(input$file1$datapath,
                    header = T,
                    sep = ",",
                    quote = "",
    )
    # Remove metadata ----
    df1 <- df0[substr(df0[,1], 1, 1) != "#",] 
    # Add columns ----
    df1$Temper <- round(as.numeric(df1$CH0.Temperature.degC.), digits = 2)
    df1$Depth <- round(as.numeric(df1$CH1.Depth.dbar.), digits = 2)
    df1$TimeStamp <- as.POSIXct(df1$Timestamp.Standard., format = "%Y-%m-%d %H:%M:%S")
    df1$Date <- paste(substr(df1$TimeStamp, 9, 10), substr(df1$TimeStamp, 6, 7), substr(df1$TimeStamp, 1, 4), sep="/")
    df1$Time <- substr(df1$TimeStamp, 12, 20)
    # Return df -----
    return(df1)
    
  }
  
  # Stage 2 function ----
  stage2 <- function(df1){
    # Output table
    df2 <- df1[,c("Temper", "Depth", "Date", "Time")]
    # Return df -----
    return(df2)
  }
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially
    req(input$file1)

    # Run stage 1 ----
    df1 <- stage1(input)
    
    # Run stage 2 ----
    df2 <- stage2(df1)
    
    # Choose display ----
    if(input$disp == "head") {
      return(head(df2))
      # return(rbind(head(df2), rep(NA, 4), tail(df2)))
    }
    else {
      return(df2)
    }
    
  })

  # Downloadable txt of selected dataset ----
  output$downloadData <- downloadHandler(
    
    filename = function(){
      
      # Make file output file name ----
      paste(gsub(".csv", "", input$file1), ".txt", sep="")
      
    },
    
    content = function(file){
      
      # Run stage 1 ----
      df1 <- stage1(input)
      
      # Run stage 2 ----
      df2 <- stage2(df1)
      
      # Write table ----
      write.table(df2, file, row.names = FALSE, sep = "\t", dec = ".", quote = F)
      
    }
    
  )
    
  # Plot in main panel ----
  output$plot <- renderPlot({

    req(input$file1)
    
    # Run stage 1 ----
    df1 <- stage1(input)
    
    # Plot data
    plot(df1$TimeStamp, df1$Depth*-1, type="l", lwd = 2, ylim = c(max(df1$Depth)*-1, 0), xlab = "Time", ylab = "Depth (m)", xaxt = "n")
    axis.POSIXct(1, df1$TimeStamp, format = "%H:%M:%S" , lwd.ticks = 1, las = 1)
    title(gsub(".csv", "", input$file1$name))
  })

}

# Create Shiny app ----
shinyApp(ui, server)