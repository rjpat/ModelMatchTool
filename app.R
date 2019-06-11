library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(CleanAF)

ui <- fluidPage(
  tabsetPanel(
    tabPanel('Ingestion',
      sidebarLayout(
        sidebarPanel(
          fileInput('rawData', label = h3('Upload Raw Data'),
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
          ),
          numericInput('matchColumn', label = h4('Target Column'), 1, min = 1, max = 20)
        ),
        mainPanel(
          tableOutput("rawLoadedTable")
        )
      )
    ),
    tabPanel('Data Matching',
      downloadButton("downloadMatched", "Download Output"),
      tableOutput('knownDataTable')
    )
  )
)

server <- function(input, output) {
  testSetCleansing <- function(x) {
    # Name is converted to lower as match is case sensitive
    x$Name <- str_to_lower(x$Name)
    
    #' Everything except alpha numeric characters are stripped out
    #' and characters are split
    
    x %>%
      mutate(Grouped = strsplit(as.character(Name), " ")) %>%
      mutate(cleanedName = CleanAF::name_clean(Name),
             len = sapply(gregexpr("\\W+", cleanedName), length) + 1,
             splitName = CleanAF::name_clean_split(Name))

  }
  
  rawDataLoaded <- reactive({
    rawDataTemp <- input$rawData
    
    if(is.null(rawDataTemp))
      return(NULL)
    
    read.csv(rawDataTemp$datapath)
  })
  
  output$rawLoadedTable <- renderTable({
    rawDataLoaded()
  })
  
  knownModels <- reactive({
    testSetCleansing(read.csv('data/Models2.csv'))
  })
  
  preppedLoadedData <- reactive({
    rawDataLoaded()[input$matchColumn] %>%
      select(Name = 1) %>%
      testSetCleansing()
  })
  
  matchedData <- reactive({
    modelsInt <- knownModels()
    preppedInt <- preppedLoadedData()
    
    # modelsInt <- testSetCleansing(read.csv('Models2.csv'))
    # preppedInt <- testSetCleansing(read.csv('test2.csv', fileEncoding = 'UTF-8-BOM'))[1:15,]

    modelsInt$result <- as.numeric(0)
    preppedInt$rowNumID <- as.numeric(0)
    
    finalOut <- setNames(data.frame(matrix(ncol=4, nrow = 0)), 
                         c('originalName', 'modelId', 'modelName', 'confidence'))
    
    preppedList <- as.list(preppedInt$splitName)
    modelsList <- as.list(modelsInt$splitName)
    
    modelsInt <- modelsInt %>%
      mutate(row = row_number())
    
    # Add progress bar for the long term processing
    withProgress(message = 'Thinking hard', value = 0, {
    
      for(j in 1:nrow(preppedInt)) {
        
        unlistedj <- unlist(preppedList[j])
        
        lengthj <- length(unlistedj)
        
        modelsInt$result <- modelsList %>%
          map_dbl(., function(x) {
            unlistedi <- unlist(x)
            
            resultInt <- sum(unlistedi %in% unlistedj)
            
            (resultInt * 2 / (lengthj + length(unlistedi)))
          })
        
        # Creates the dataframe for the final set out with the test name on the left, matched id in the center, matched name on the right for validation
        finalTemp <- preppedInt[j,] %>%
          mutate(rowNumId = modelsInt %>% filter(result == max(modelsInt$result)) %>% filter(priority == min(priority)) %>% filter(id == min(id)) %>%
                   select(row) %>% filter(rank(row) == 1) %>% as.integer()) %>%
          select(Name, rowNumId) %>%
          rename(originalName = Name) %>%
          mutate(modelId = modelsInt[rowNumId, 1], modelName = modelsInt[rowNumId, 2], confidence = modelsInt[rowNumId, 8]) %>%
          select(-rowNumId)
        
        # Output the data formatted
        finalOut[j,] = finalTemp
  
        incProgress(1/nrow(preppedInt), detail = paste('Completed Row', j))
      }
    
    })  
      
    finalOut
  })

  output$knownDataTable <- renderTable({
    matchedData() %>%
      arrange(desc(confidence))
  })
  
  output$downloadMatched <- downloadHandler(
    filename = function(){'matchedData.csv'},
    content = function(holder){
      write.csv(matchedData(), holder)
    }
  )
  

}

shinyApp(ui = ui, server = server)
