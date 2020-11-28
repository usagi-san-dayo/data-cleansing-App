library(shiny)
library(shinyFiles)
library(DT)
library(rhandsontable)
library(Rfast)
library(shinyjs)
source("dataHandler.R")

options(encoding = "UTF-8")

# Define UI for application that draws a histogram
ui <- navbarPage("Data-Cleansing App",
                 theme = "bootstrap.min.css",
                 navbarMenu("Import and export",
                            tabPanel("Import", fluid = TRUE,
                                     sidebarLayout(
                                       sidebarPanel(
                                         fileInput("file", "Import a csv file", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                         selectInput("encode", "File-encoding", choices = c("Others","UTF-8", "Latin-1")),
                                         shinyDirButton("dir", "Location", "Select the working directory"),
                                         verbatimTextOutput("selectedDir"),
                                         br(),
                                         sliderInput("numFac", "The ratio of the number of levels to the number of rows", min = 0, max = 100, value = 10),
                                         sliderInput("numDate", "The minimum value of Date type objects", min = 0, max = 100, value = 10),
                                         actionButton("mkForm", "Scan data")
                                       ),
                                       # Show a plot of the generated distribution
                                       mainPanel(
                                         fluidRow(
                                           h2("Data"),
                                           dataTableOutput("imported")
                                         )
                                       )
                                     )
                            ),
                            tabPanel("Export", fluid = TRUE,
                                     sidebarLayout(
                                       sidebarPanel(
                                         strong("Whether to append cleansed data"),
                                         checkboxInput("ifAppend", "Appned Data", value = FALSE),
                                         div(style = "display:inline-block", actionButton("mkPreData", "Preview")),
                                         div(style = "display:inline-block", downloadButton("downloadSave", "Download"))
                                       ),
                                       mainPanel(
                                         fluidRow(
                                           h2("Preview"),
                                           dataTableOutput("preview")
                                         )
                                       )
                                     )
                            )
                 ),
                 tabPanel("Numeric", fluid = TRUE,
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("colName", "Choose a colname:", choices = getColNames(cleanser$cleansingForm, "numeric")),
                              textInput("newColName", "Change the colname to", ""),
                              checkboxGroupInput("numeric_NA", "Missing-values replace with the mean", choices = getMissValues(cleanser$cleansingForm, cleanser$cleansingForm$numeric[[1]]$colname), width = "200px"),
                              strong("Whether to categorise numerical values into ≧ the mean and < the mean or not"),
                              checkboxInput("categorise", "Categorise", value = isCategorised(cleanser$cleansingForm, colnames(cleanser$dataset)[1]))
                            ),
                            # Show a plot of the generated distribution
                            mainPanel(
                              fluidRow(
                                style = "margin-top: 15px",
                                h2("Cleansing Form"),
                                rHandsontableOutput("options")
                              ),
                              hr(),
                              fluidRow(
                                h2("Data"),
                                dataTableOutput("view")
                              )
                            )
                          )
                 ),
                 tabPanel("Factor", fluid = TRUE,
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("colNameFactor", "Choose a colname:", choices = getColNames(cleanser$cleansingForm, "factor")),
                              textInput("newColNameFactor", "Change the colname to", ""),
                              checkboxGroupInput("factor_NA", "Choose any levels you want to replace with NA", choices = getLevels(cleanser$cleansingForm, cleanser$cleansingForm$factor[[1]]$colname), width = "200px"),
                              selectInput("pool", "Select any combinations of pooled levels", choices = getPooledLevels(cleanser$cleansingForm, cleanser$cleansingForm$factor[[1]]$colname), multiple = TRUE),
                              selectInput("order", "Select as the 1st level", choices = getRepPoolLevels(cleanser$cleansingForm, cleanser$cleansingForm$factor[[1]]$colname),  multiple = TRUE)
                            ),
                            mainPanel(
                              fluidRow(
                                style = "margin-top: 15px",
                                h2("Cleansing Form"),
                                rHandsontableOutput("optionsFactor")
                              ),
                              hr(),
                              fluidRow(
                                h2("Data"),
                                dataTableOutput("viewFactor")
                              )
                            )
                          )
                 ),
                 tabPanel("Date", fluid = TRUE,
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("colNameDate", "Choose a colname:", choices = getColNames(cleanser$cleansingForm, "Date")),
                              textInput("newColNameDate", "Change the colname to", ""),
                            ),
                            mainPanel(
                              fluidRow(
                                style = "margin-top: 15px",
                                h2("Cleansing Form"),
                                rHandsontableOutput("optionsDate")
                              ),
                              hr(),
                              fluidRow(
                                h2("Data"),
                                dataTableOutput("viewDate")
                              )
                            )
                          )
                 )
)

server <- function(input, output, session) {
  dataset <- data.frame()
  datasetFactor <- data.frame()
  datasetDate <- data.frame()
  options_data <- data.frame()
  options_dataFactor <- data.frame()
  options_dataDate <- data.frame()
  previewData <- data.frame()
  dataValues <- reactiveValues(data = options_data)
  dataValuesFactor <- reactiveValues(data = options_dataFactor)
  dataValuesDate <- reactiveValues(data = options_dataDate)
  preDataValues <- reactiveValues(data = previewData)
  volumes <- getVolumes()
  shinyDirChoose(input, "dir", roots = getVolumes()(), filetypes = c("", "txt"))
  
  session$onSessionEnded(function() {
    stopApp()
  })
  observeEvent(
    input$file,{
      csvFile <- cleanser$readData(gsub("\\.csv", "", input$file$datapath), input$encode)
      updateSliderInput(session, "numFac", "The ratio of the number of levels to the number of rows", min = 0, max = nrow(csvFile), value = 10)
      updateSliderInput(session, "numDate", "The minimum value of Date type objects", min = 0, max = nrow(csvFile), value = 10)
    }
  )
  observeEvent(
    input$mkForm,{
      if (!is.null(input$file)) {
        scanData (input$file, volumes, input$dir, input$numFac, input$numDate, input$encode, session) 
      }
    }
  )
  observeEvent(
    input$mkPreData,{
      preDataValues$data <- mkPreviewData(cleanser$dataset, cleanser$cleansingForm, input$ifAppend)
    }
  )
  observeEvent(
    input$colName,{
      if (!is.null(cleanser$cleansingForm$numeric) & any(dim(cleanser$dataset) != c(0, 0))) {
        updateTextInput(session, "newColName", "Change the colname to", searchColname(cleanser$cleansingForm, input$colName)[1, 2])
        isColChanged_repMissVal <<- TRUE
        updateCheckboxGroupInput(session, "numeric_NA", "Choose any missing-values you want to replace with the mean", choices = getMissValues(cleanser$cleansingForm, input$colName), selected = isRepByMean(cleanser$cleansingForm, input$colName))
        updateCheckboxInput(session, "categorise", "Categorise", value = isCategorised(cleanser$cleansingForm, input$colName))
        dataValues$data <- right_tableManager(dataValues, input$colName)
      }
    },
    ignoreNULL = FALSE
  )
  observeEvent(
    input$newColName,{
      if (!is.null(cleanser$cleansingForm$numeric) & any(dim(cleanser$dataset) != c(0, 0))) {
        changeColName_numeric(input$colName, input$newColName)
        dataValues$data <- right_tableManager(dataValues, input$colName)
      }
    },
    ignoreNULL = FALSE
  )
  observeEvent(
    input$colNameFactor,{
      if (!is.null(cleanser$cleansingForm$factor) & any(dim(cleanser$dataset) != c(0, 0))) {
        updateTextInput(session, "newColNameFactor", "Change the colname to", searchColname(cleanser$cleansingForm, input$colNameFactor)[1, 2])
        isColChanged_repNA <<- TRUE
        updateCheckboxGroupInput(session, "factor_NA", "Choose any levels you want to replace with NA", choices = getLevels(cleanser$cleansingForm, input$colNameFactor), selected = isRepByNA(cleanser$cleansingForm, input$colNameFactor))
        selectedPools <-  unique(mkRepPoolLevels(cleanser$cleansingForm, input$colNameFactor)[grep("[+]", mkRepPoolLevels(cleanser$cleansingForm, input$colNameFactor))])
        updateSelectInput(session, "pool", "Select any combinations of pooled levels", choices = delChoicesPool(cleanser$cleansingForm, input$colNameFactor, selectedPools), selected = selectedPools)
        selectedOrder <- getOrder(cleanser$cleansingForm, input$colNameFactor)
        numOrder <- mkOrdinal(selectedOrder) 
        updateSelectInput(session, "order", paste0("Select as the ", numOrder, " order"), choices = na.omit(getRepPoolLevels(cleanser$cleansingForm, input$colNameFactor)), selected = selectedOrder)
        isColChanged_pool <<- TRUE
        isColChanged_order <<- TRUE
        dataValuesFactor$data <- right_tableManager(dataValuesFactor, input$colNameFactor)
      }
    },
    ignoreNULL = FALSE
  )
  observeEvent(
    input$newColNameFactor,{
      if (!is.null(cleanser$cleansingForm$factor) & any(dim(cleanser$dataset) != c(0, 0))) {
        changeColName_factor(input$colNameFactor, input$newColNameFactor)
        dataValuesFactor$data <- right_tableManager(dataValuesFactor, input$colNameFactor)
      }
    },
    ignoreNULL = FALSE
  )
  observeEvent(
    input$colNameDate,{
      if (!is.null(cleanser$cleansingForm$Date) & any(dim(cleanser$dataset) != c(0, 0))) {
        dataValuesDate$data <- right_tableManager(dataValuesDate, input$colNameDate)
      }
    },
    ignoreNULL = FALSE
  )
  observeEvent(
    input$newColNameDate,{
      if (!is.null(cleanser$cleansingForm$Date) & any(dim(cleanser$dataset) != c(0, 0))) {
        changeColName_Date(input$colNameDate, input$newColNameDate)
        dataValuesDate$data <- right_tableManager(dataValuesDate, input$colNameDate)
      }
    },
    ignoreNULL = FALSE
  )
  observeEvent(
    input$numeric_NA,{
      if (!is.null(cleanser$cleansingForm$numeric) & any(dim(cleanser$dataset) != c(0, 0))) {
        replaceWithMean(input$colName, input$numeric_NA)
        dataValues$data <- right_tableManager(dataValues, input$colName)
      }
    },
    ignoreNULL = FALSE
  )
  observeEvent(
    input$categorise,{
      if (!is.null(cleanser$cleansingForm$numeric) & any(dim(cleanser$dataset) != c(0, 0))) {
        categorise(input$colName, input$categorise)
        dataValues$data <- right_tableManager(dataValues, input$colName)
      }
    },
    ignoreNULL = FALSE
  )
  observeEvent(
    input$factor_NA,{
      row_startTable <- 3
      col_poolTable <- 7
      replaceWithNA(input$colNameFactor, input$factor_NA, input$pool, session)
      table <- searchColname(cleanser$cleansingForm, input$colNameFactor)
      if (!is.null(table)) {
        if (length(input$pool) == 0 & length(input$factor_NA) > 0 & all(table[row_startTable : (nrow(table) - 1), col_poolTable] == "")) {
          updateSelectInput(session, "pool", "Select any combinations of pooled levels", choices = getPooledLevels(cleanser$cleansingForm, input$colNameFactor))
        }
      }
      updateSelectInput(session, "order", paste0("Select as the 1st order"), choices = na.omit(getRepPoolLevels(cleanser$cleansingForm, input$colNameFactor)), selected = character(0))
      dataValuesFactor$data <- right_tableManager(dataValuesFactor, input$colNameFactor)
    },
    ignoreNULL = FALSE
  )
  observeEvent(
    input$pool,{
      updateSelectInput(session, "pool", "Select any combinations of pooled levels", choices = delChoicesPool(cleanser$cleansingForm, input$colNameFactor, input$pool), selected = input$pool)
      poolLevels(input$colNameFactor, input$pool, input$order, session)
      updateSelectInput(session, "order", paste0("Select as the 1st order"), choices = na.omit(getRepPoolLevels(cleanser$cleansingForm, input$colNameFactor)), selected = character(0))
      dataValuesFactor$data <- right_tableManager(dataValuesFactor, input$colNameFactor)
    },
    ignoreNULL = FALSE
  )
  observeEvent(
    input$order,{
      numOrder <- mkOrdinal(input$order) 
      updateSelectInput(session, "order", paste0("Select as the ", numOrder, " order"), choices = na.omit(getRepPoolLevels(cleanser$cleansingForm, input$colNameFactor)), selected = input$order)
      orderLevels(input$colNameFactor, input$order, session)
      dataValuesFactor$data <- right_tableManager(dataValuesFactor, input$colNameFactor)
    },
    ignoreNULL = FALSE
  )
  observeEvent(
    input$options,{
      formHandler_numeric(input$options, input$colName)
      dataValues$data <- right_tableManager(dataValues, input$colName)
    },
    ignoreNULL = FALSE
  )
  observeEvent(
    input$optionsFactor,{
      formHandler_factor(input$optionsFactor, input$colNameFactor)
      dataValuesFactor$data <- right_tableManager(dataValuesFactor, input$colNameFactor)
    },
    ignoreNULL = FALSE
  )
  observeEvent(
    input$optionsDate,{
      formHandler_Date(input$optionsDate, input$colNameDate)
      dataValuesDate$data <- right_tableManager(dataValuesDate, input$colNameDate)
    },
    ignoreNULL = FALSE
  )
  
  output$selectedDir <- renderText({
    if(!is.null(input$dir)){
      dirInfo <- parseDirPath(volumes, input$dir)
    }
  })
  output$imported <- renderDataTable({
    if (!is.null(input$file)) {
      csvFile <- cleanser$readData(gsub("\\.csv", "", input$file$datapath), input$encode)
      importdData <- datatable(csvFile, options = list(scrollX = TRUE, scrollY = "75vh", scrollCollapse = TRUE))
    }
  })
  output$preview <- renderDataTable({
    if (any(dim(cleanser$dataset) != c(0, 0))) {
      previewDataset <- datatable(preDataValues$data, options = list(scrollX = TRUE, scrollY = "75vh", scrollCollapse = TRUE))
    }
  })
  output$options <- renderRHandsontable({ 
    if (!is.null(cleanser$cleansingForm$numeric) & any(dim(cleanser$dataset) != c(0, 0)) & input$colName != "") {
      isColChanged_repMissVal <<- FALSE
      rhandsontable(dataValues$data)
    }
  })
  output$optionsFactor <- renderRHandsontable({
    if (!is.null(cleanser$cleansingForm$factor) & any(dim(cleanser$dataset) != c(0, 0)) & input$colNameFactor != "") {
      isColChanged_repNA <<- FALSE
      isColChanged_pool <<- FALSE
      isColChanged_order <<- FALSE
      rhandsontable(dataValuesFactor$data)
    }
  })
  output$optionsDate <- renderRHandsontable({
    if (!is.null(cleanser$cleansingForm$Date) & any(dim(cleanser$dataset) != c(0, 0)) & input$colNameDate != "") {
      rhandsontable(dataValuesDate$data)
    }
  })
  output$view <- renderDataTable({
    if (!is.null(cleanser$cleansingForm$numeric) & any(dim(cleanser$dataset) != c(0, 0)) & input$colName != "") {
      dataset <- datatable(output_dataNumeric(cleanser$dataset, input$colName, input$newColName, input$numeric_NA, input$options, session), options = list(scrollX = TRUE, scrollY = "60vh", scrollCollapse = TRUE))
    }
  })
  output$viewFactor <- renderDataTable({
    if (!is.null(cleanser$cleansingForm$factor) & any(dim(cleanser$dataset) != c(0, 0)) & input$colNameFactor != "") {
      datasetFactor <- datatable(output_dataFactor(cleanser$dataset, input$colNameFactor, input$newColNameFactor, input$factor_NA, input$pool, input$order, input$optionsFactor, session), options = list(scrollX = TRUE, scrollY = "60vh", scrollCollapse = TRUE))
    }
  })
  output$viewDate <- renderDataTable({
    if (!is.null(cleanser$cleansingForm$Date) & any(dim(cleanser$dataset) != c(0, 0)) & input$colNameDate != "") {
      datasetDate <- datatable(output_dataDate(cleanser$dataset, input$colNameDate, input$newColNameDate, input$optionsDate), options = list(scrollX = TRUE, scrollY = "60vh", scrollCollapse = TRUE))
    }
  })
  output$downloadSave <- downloadHandler(
    filename = paste0("data", ".zip"),
    content = function(file) {
      if (any(dim(cleanser$dataset) != c(0, 0))) {
        output_cleansingForm(file, cleanser$fileInfo, volumes, input$dir, cleanser$cleansingForm, input$encode)
      }
    },
    contentType = "application/zip"
  )
}

# Run the application 
shinyApp(ui = ui, server = server)