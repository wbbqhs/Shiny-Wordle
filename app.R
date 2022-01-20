library(shiny)
library(shinyjs)
# library(shinykeyboard)
library(rhandsontable)
library(wordle)
library(fansi)
source('helper.R')

ui <- function() {
  
  fluidPage(
    useShinyjs(),
    tags$head(
      tags$style(
        HTML(".shiny-notification {
             position:fixed;
             top: calc(10%);
             left: calc(50%);
             }
             "
        )
      )
    ),
    
    radioButtons("gameMode", "Game Mode",
                 choices = list("Daily Challenge" = "daily", "Unlimited" = "unlimited"),
                 selected = "daily"),
    conditionalPanel(condition = "input.gameMode == 'unlimited'",
                     # Input: Select a file ----
                     actionButton(inputId = "getNewWord", label = "Play a new word")
                     ),
    # Horizontal line ----
    tags$hr(),
    fluidRow(
      column(6, "Make your guess", 
             rHandsontableOutput('InputTable')
             # , verbatimTextOutput("debug")
             # , keyboardInput("keebs", color_palette = "sharla1"))
            ),
      column(6, "Game Info", 
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
             # , verbatimTextOutput("debug")
             # , keyboardInput("keebs", color_palette = "sharla1"))
            )
    ),
    br(),
    actionButton(inputId = "done", label = "Check"),
    tags$hr(),
    htmlOutput("result") 
  )
}

server <- function(input, output, session) {
  gameState <- reactiveValues()
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
  
  observeEvent(input$done, {
    inputTable <- hot_to_r(input$InputTable)
    nAttempt <- length(gameState$wordleGame$attempts)
    if(nAttempt<=nGuesses-1){
      # read the attempt
      attempt <- tolower(paste(inputTable[nAttempt+1, ], collapse = ''))
      
      # update game state
      res <- gameState$wordleGame$try(attempt, quiet = T)
      if(is.null(res)){
        # this happens when the attempt is not in the word list
        showNotification('Not a valid word in the word list.')
      } else {
        # update InputTable and colors
        gameState$inputTable[nAttempt+1,] <- inputTable[nAttempt+1, ]
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
          # failed after all tries
          if (nAttempt>nGuesses-2){
            gameState$failed <- T
            showNotification(paste("Try again later! The answer is '", gameState$wordleGame$target_word, "'.", sep = ''))
            shinyjs::disable("done")
          }
          gameState$lockedRows <- (1:nGuesses)[-(nAttempt+2)]
        }
        # to do: figure out a way to auto select the cell to the right after typing one letter (if possible)
        # select the first cell of the next row and start edit mode
        if(!gameState$is_solved){
          selectNextRowJS <- paste("HTMLWidgets.getInstance(InputTable).hot.selectCell(", nAttempt+1, ",0)
                                    HTMLWidgets.getInstance(InputTable).hot.getActiveEditor().beginEditing()", sep = '')
          shinyjs::runjs(selectNextRowJS)
        }
      }
    }
  })
  
  output$lettersIncluded <- renderText({ gameState$lettersIncluded })
  output$lettersExcluded <- renderText({ gameState$lettersExcluded })
  
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