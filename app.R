library(shiny)
library(shinyjs)
# library(shinykeyboard)
library(rhandsontable)
library(wordle)
library(fansi)
# use a randomized version of the official wordle word list
# load('data/wordle_dict_rand.Rda')
# word_list <- wordle_dict_rand
# use a randomized version of the 5000 most common word list
load('data/wordle_dict_common.Rda')
word_list <- wordle_dict_common
nGuesses <- 6
nLetters <- 5
initRow <- rep(" ", nGuesses)
inputInit <- as.data.frame(matrix(' ', nGuesses, nLetters))
colnames(inputInit) <- paste("Letter.", 1:nLetters, sep = '')
testMode <- F
isLetterJS <- paste0("[", paste0(paste0("'", c("", " ", letters, LETTERS), "'"), 
                                 collapse = ","), "].indexOf(value) > -1")

blocks <- c(grey = "\U0001f7eb", yellow = "\U0001f7e8", green = "\U0001f7e9")

getBlocksToShare  <-  function(wordleInstance) {
  blockToShare <- c()
  for (response in wordleInstance$responses) {
    blockToShare <- paste(blockToShare, "\n", paste(blocks[response], collapse = ""))
  }
  return(blockToShare)
}

getLettersInfo<- function(gameState){
  inputTable <- gameState$inputTable
  colorMat <- gameState$colorMat
  allLetters <- unique(inputTable[colorMat != 'TBD'])
  lettersIncluded <- sort(unique(inputTable[colorMat == 'green' | colorMat == 'yellow']))
  # note that a grey letter can be a letter that's green in a different position
  # example: guessing serve for shire will produce green, grey, yellow, grey, green
  lettersExcluded <- sort(setdiff(allLetters, lettersIncluded))
  return(list(lettersExcluded = lettersExcluded, lettersIncluded = lettersIncluded))
}

ansi2html <- function(ansi){
  HTML(sprintf(
    "<pre>%s</pre>",
    gsub("\n", "<br/>", as.character(sgr_to_html(ansi)))
  ))
}

# unfortunately the way to pass extra parameters when creating the rhandsonetable and then accessing it through params doesn't work for me
# This is a workaround to color the cells
colorRenderer <-  function(colorMat){
  greenInds <- which(colorMat == 'green', arr.ind = T)
  rowGreenInds <- greenInds[,1]-1
  colGreenInds <- greenInds[,2]-1
  yellowInds <- which(colorMat == 'yellow', arr.ind = T)
  rowYellowInds <- yellowInds[,1]-1
  colYellowInds <- yellowInds[,2]-1
  greyInds <- which(colorMat == 'grey', arr.ind = T)
  rowGreyInds <- greyInds[,1]-1
  colGreyInds <- greyInds[,2]-1
  
  rowIndJSGreen <- paste("[", paste(rowGreenInds, collapse = ","), "]", sep = "")
  colIndJSGreen <- paste("[", paste(colGreenInds, collapse = ","), "]", sep = "")
  
  rowIndJSYellow <- paste("[", paste(rowYellowInds, collapse = ","), "]", sep = "")
  colIndJSYellow <- paste("[", paste(colYellowInds, collapse = ","), "]", sep = "")
  
  rowIndJSGrey <- paste("[", paste(rowGreyInds, collapse = ","), "]", sep = "")
  colIndJSGrey <- paste("[", paste(colGreyInds, collapse = ","), "]", sep = "")
  
  paste("
          function(instance, td, row, col, prop, value, cellProperties) {
            Handsontable.renderers.TextRenderer.apply(this, arguments);
            rowArrayGreen = ", rowIndJSGreen,"
            colArrayGreen = ", colIndJSGreen,"
            rowArrayYellow = ", rowIndJSYellow,"
            colArrayYellow = ", colIndJSYellow,"
            rowArrayGrey = ", rowIndJSGrey,"
            colArrayGrey = ", colIndJSGrey,"
            for (let i = 0; i < rowArrayGreen.length; i++){
              if (rowArrayGreen[i] == row && colArrayGreen[i] == col) {
                td.style.background = '#D1F981';
                return td;
              }
            }
            for (let i = 0; i < rowArrayYellow.length; i++){
              if (rowArrayYellow[i] == row && colArrayYellow[i] == col) {
                td.style.background = 'yellow';
                return td;
              }
            }
            for (let i = 0; i < rowArrayGrey.length; i++){
              if (rowArrayGrey[i] == row && colArrayGrey[i] == col) {
                td.style.background = '#E9E7E7';
                return td;
              }
            }
            return td;
          }", sep = ''
  )
}

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
  
  observeEvent(input$gameMode, {
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
  
  observeEvent(input$getNewWord, {
    # there should be a way to avoid code duplication but not gonna mess with it for now
    if(!is.null(input$InputTable)){
      # issue: cell (0, 0) will retain the previously rendered background color until it has exit edit mode
      selectFirstCellJS <- paste("HTMLWidgets.getInstance(InputTable).hot.getActiveEditor().cancelChanges()
                                  HTMLWidgets.getInstance(InputTable).hot.selectCell(0,0)
                                  HTMLWidgets.getInstance(InputTable).hot.getActiveEditor().beginEditing()", sep = '')
      shinyjs::runjs(selectFirstCellJS)
    }
    if (input$gameMode == "unlimited"){
      shinyjs::enable("done")
      targetWord <- sample(word_list, 1)
      gameState$wordleGame <- WordleGame$new(word_list, target_word = targetWord)
      gameState$lockedRows <- 2:nGuesses
      gameState$inputTable <- inputInit
      gameState$colorMat <- matrix("TBD", nGuesses, nLetters)
      gameState$is_solved <- F
      gameState$failed <- F
      gameState$lettersExcluded <- ''
      gameState$lettersIncluded <- ''
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