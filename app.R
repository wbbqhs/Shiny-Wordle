library(shiny)
library(shinyjs)
library(rhandsontable)
library(wordle)
library(fansi)
source('helper.R')

ui <- function(req) {
  
  fluidPage(
    titlePanel("wordle with helper"),
    tags$head(
      tags$style(HTML(customCSS))
    ),
    tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
    useShinyjs(),
    tabsetPanel(id = "tabs", 
                type = "tabs", 
                tabPanel("Wordle",
                         tags$script('
                          $(document).on("keyup", function(e) {
                            if(e.keyCode == 13){
                              Shiny.onInputChange("enterPressed", Math.random());
                            }
                          });
                        '),
                         fluidRow(
                           column(6, radioButtons("gameMode", "Game Mode",
                                                  choices = list("Daily Challenge" = "daily", "Unlimited" = "unlimited"),
                                                  selected = "daily")),
                           column(6, checkboxInput("hardMode", "Hard mode (to be implemented)"))
                         ),
                         conditionalPanel(condition = "input.gameMode == 'unlimited'",
                                          # Input: Select a file ----
                                          actionButton(inputId = "getNewWord", label = "Play a new word")
                         ),
                         # Horizontal line ----
                         tags$hr(),
                         fluidRow(
                           column(6, 
                                  rHandsontableOutput('InputTable')
                           ),
                           column(6, 
                                  fluidRow(
                                    column(12, 
                                           "Letters Included",
                                           verbatimTextOutput("lettersIncluded"))
                                  ),
                                  fluidRow(
                                    column(6,
                                           "Letters Excluded",
                                           verbatimTextOutput("lettersExcluded"))
                                  )
                           )
                         ),
                         br(),
                         actionButton(inputId = "done", label = "Check"),
                         tags$hr(),
                         htmlOutput("result") 
                ),
                tabPanel("Wordle Helper",
                         tags$hr(),
                         fluidRow(
                           column(6, 'Right click to change color', rHandsontableOutput('helperTable')),
                           column(6, "Suggested Words", textOutput("suggestedWords"))
                         ),
                         fluidRow(
                           column(6, 'For the color impaired or on mobile devices', rHandsontableOutput('colorHelperTable'))
                          )
                )
    )
    
  )
}

server <- function(input, output, session) {
  gameState <- reactiveValues()
  helperState <- reactiveValues()
  helperState$initTable <- inputInit
  helperState$suggestedWords <- ""
  
  
  # make sure there is a word of the day
  wordIndex <- as.numeric(difftime(Sys.Date(), as.Date('2022-01-15'), units = 'days'))
  
  observeEvent(list(input$gameMode, input$getNewWord), {
    # initialize gameState
    if(!is.null(input$InputTable)){
      # exit the edit mode start edit mode on the first cell
      selectFirstCellJS <- paste("HTMLWidgets.getInstance(InputTable).hot.getActiveEditor().cancelChanges()
                                  HTMLWidgets.getInstance(InputTable).hot.selectCell(0,0)
                                  HTMLWidgets.getInstance(InputTable).hot.getActiveEditor().beginEditing()", sep = '')
      shinyjs::runjs(selectFirstCellJS)
    }
    if (input$gameMode == "daily"){
      targetWord <- word_list[wordIndex]
      # targetWord <-  'shire'
    } else {
      targetWord <- sample(word_list, 1)
    }
    shinyjs::enable("done")
    gameState$wordleGame <- WordleGame$new(word_list, target_word = targetWord)
    gameState$lockedRows <- 2:nGuesses
    gameState$inputTable <- inputInit
    gameState$colorMat <- matrix("TBD", nGuesses, nLetters)
    gameState$is_solved <- F
    gameState$failed <- F
    gameState$lettersExcluded <- ''
    gameState$lettersIncluded <- ''
  })
  
  observeEvent({
    # the req line is necessary. otherwise it throws an error 'Error in do.call: second argument must be a list'
    req(input$colorHelperTable)
    list(input$helperTable, input$colorHelperTable)}, {
    helperTable <- hot_to_r(input$helperTable)
    colorHelperTable <- hot_to_r(input$colorHelperTable)
    # need to create a new wordle helper every time anything is updated
    helperState$wordleHelper <- WordleHelper$new(nchar = nLetters)
    for (iRow in 1:nrow(helperTable)){
      attemptedWord <- unlist(helperTable[iRow, ])
      attemptedWord <- attemptedWord[attemptedWord!=" "]
      attempt <- tolower(paste(attemptedWord, collapse = ''))
      lenWord <- nchar(attempt)
      colorVector <- unlist(colorHelperTable[iRow, ])
      colorVector <- colorVector[colorVector!= ' ']
      lenColor <- length(colorVector)
      if(lenWord==5 & lenColor==5){
        helperState$wordleHelper$update(attempt, colorVector)
        helperState$suggestedWords <- paste0(helperState$wordleHelper$words, collapse = ", ")
      }
    }
  })
  
  output$InputTable <- renderRHandsontable({
    # add ability to disable the game (e.g. when deployed on a server, change this so each user can only play it once a day)
    if(F){
      shinyjs::disable("done")
      showNotification('You have already played this today.', duration = 10)
    }
    inputHOT <- rhandsontable(gameState$inputTable) %>%
      hot_row(gameState$lockedRows, readOnly = T) %>%
      hot_cols(renderer = colorRenderer(gameState$colorMat)) %>%
      # hot_validate_character(cols = 1:nLetters, choices = c(LETTERS, letters), allowInvalid = F) %>%
      # this will disallow typing of non-letters or multiple letters in one cell
      hot_cols(validator = paste("
           function (value, callback) {
            setTimeout(function(){
              callback(value.length <= 1 && ", isLetterJS,");
            }, 10)
           }", sep = ''),
               allowInvalid = FALSE)
    return(inputHOT)
  })
  
  output$helperTable <- renderRHandsontable({
    hotHelper <- rhandsontable(helperState$initTable) %>%
      # use customOpts to add options in the right click context menu to change cell colors
      # The JS code set the cell to a class which is defined in the custom css added in the ui
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE, customOpts = helperTableCustomMenu)
    # This will remove the options 'undo', 'redo', 'alignment'
    hotHelper$x$contextMenu$items[c('undo', 'redo', 'alignment')] <- NULL
    hotHelper
  })
  
  output$colorHelperTable <- renderRHandsontable({
    hotHelper <- rhandsontable(helperState$initTable) %>%
      # use customOpts to add options in the right click context menu to change cell colors
      # The JS code set the cell to a class which is defined in the custom css added in the ui
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE, customOpts = helperTableCustomMenu)
    
    for(iCol in 1:nLetters){
      hotHelper <-  hotHelper %>%
        hot_col(iCol, type = "dropdown", source = c("grey", "yellow", "green"))}
    # This will remove the options 'undo', 'redo', 'alignment'
    hotHelper$x$contextMenu$items[c('undo', 'redo', 'alignment')] <- NULL
    hotHelper
  })
  
  observeEvent(input$resetHelper, {
    # initialize helperState
    helperState$wordleHelper <- WordleHelper$new(nchar = nLetters)
    # initialize helperTable
    helperState$initTable <- inputInit
    helperState$suggestedWords <- NULL
  })
  
  # not sure how to hide colorHelperTable but still have it store values
  # shinyjs::hide(id = "colorHelperTable")
  
  # To do: add an event representing clicking the check button or pressing enter after all letters has been entered. 
  
  observeEvent(input$done, {
    inputTable <- hot_to_r(input$InputTable)
    nAttempt <- length(gameState$wordleGame$attempts)
    attemptedWord <- unlist(inputTable[nAttempt+1, ])
    if(nAttempt<=nGuesses-1){
      # read the attempt
      attempt <- tolower(paste(attemptedWord, collapse = ''))
      
      # update game state
      res <- gameState$wordleGame$try(attempt, quiet = T)
      if(is.null(res)){
        # this happens when the attempt is not in the word list
        showNotification('Not a valid word in the word list.')
      } else {
        if(input$hardMode){
          # additional checks if hard mode is on
          # 1. all green letters must be kept in their positions
          # 2. yellow letters must appear the max number of times they have appeared in all previous guesses
          # 3. no excluded letters can appear
          
          # if any of the 3 conditions is not met, do nothing to advance the game
        }
        # update InputTable and colors
        gameState$inputTable[nAttempt+1,] <- attemptedWord
        gameState$colorMat[nAttempt+1,] <- res
        lettersInfo <- getLettersInfo(gameState)
        gameState$lettersExcluded <- lettersInfo$lettersExcluded
        gameState$lettersIncluded <- lettersInfo$lettersIncluded
        # lock guessed rows and unlock one more row if game is not done
        if(gameState$wordleGame$is_solved()){
          showNotification("Success!")
          gameState$lockedRows <- 1:nGuesses
          shinyjs::disable("done")
          gameState$is_solved <- T
          # gameState$wordleGame$share()
        } else {
          if (nAttempt>nGuesses-2){
            # failed after all tries
            gameState$failed <- T
            showNotification(paste("Try again later! The answer is '", gameState$wordleGame$target_word, "'.", sep = ''))
            shinyjs::disable("done")
          } else {
            # game is not done or solved or failed
            # select the first cell of the next row and start edit mode
            selectNextRowJS <- paste("HTMLWidgets.getInstance(InputTable).hot.selectCell(", nAttempt+1, ",0)
                                  HTMLWidgets.getInstance(InputTable).hot.getActiveEditor().beginEditing()", sep = '')
            shinyjs::runjs(selectNextRowJS)
          }
          gameState$lockedRows <- (1:nGuesses)[-(nAttempt+2)]
        }
        # to do: figure out a way to auto select the cell to the right after typing one letter (if possible)
        
      }
    }
  })
  
  output$lettersIncluded <- renderText({ gameState$lettersIncluded })
  output$lettersExcluded <- renderText({ gameState$lettersExcluded })
  output$suggestedWords <- renderText({ helperState$suggestedWords })
  
  resultToShare <- reactive({
    if(gameState$is_solved | gameState$failed) {
      ansi2html(getBlocksToShare(gameState$wordleGame))
    }
    else {
      if(testMode & !is.null(gameState$wordleGame)){
        HTML(paste("Test mode on. The answer is '", gameState$wordleGame$target_word, "'.", sep = ''))
      } else {
        HTML("Colored blocks will be available here.")
      }
    }
  })
  output$result <- renderUI({
    resultToShare()
  })
  # output$debug <- renderPrint(input$keebs)
}

shinyApp(ui = ui, server = server)