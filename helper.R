# use a randomized version of the official wordle word list
# load('data/wordle_dict_rand.Rda')
# word_list <- wordle_dict_rand
# use a randomized version of the 5000 most common word list
load('data/wordle_dict_common.Rda')
word_list <- wordle_dict_common
nGuesses <- 6
nLetters <- 5
initRow <- rep(" ", nGuesses)
inputInit <- as.data.frame(matrix(' ', nGuesses, nLetters), stringsAsFactors = F)
colnames(inputInit) <- paste("Letter.", 1:nLetters, sep = '')
testMode <- F
isLetterJS <- paste0("[", paste0(paste0("'", c("", " ", letters, LETTERS), "'"), 
                                 collapse = ","), "].indexOf(value) > -1")
# These are Unicode characters
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


customCSS <- ".shiny-notification {
                 position:fixed;
                 top: calc(40%);
                 left: calc(50%);
              }
              .handsontable .grey {
                background: #E9E7E7
              }
              .handsontable .yellow {
                background: #f7ecb7
              }
              .handsontable .green {
                background: #D1F981
              }
              .yellow-block,
              .green-block,
              .grey-block {
                width: 30px;
                height: 15px;
                border-radius: 10%;
                float: right;
                border: 1px solid #aaa;
              }
              
              .yellow-block {
                background: #f7ecb7;
              }

              .green-block {
                background: #ccf7a0;
              }
              
              .grey-block {
                background: #E9E7E7;
              }
              "
helperTableCustomMenu  <-  list(
  makeGrey = list(name = "Grey <div class=\"grey-block\"></div>",
                  callback = htmlwidgets::JS(
                    "function(key, options) {
                        thisInstance = HTMLWidgets.getInstance(helperTable).hot
                        thisInstance.setCellMeta(thisInstance.getSelectedLast()[0], thisInstance.getSelectedLast()[1], 'className', 'grey');
                        thisInstance.render();
                        thisInstance1 = HTMLWidgets.getInstance(colorHelperTable).hot
                        thisInstance1.setDataAtCell(thisInstance.getSelectedLast()[0], thisInstance.getSelectedLast()[1], 'grey')
                      }
                     ")
  ),
  makeYellow = list(name = "Yellow <div class=\"yellow-block\"></div>",
                    callback = htmlwidgets::JS(
                      "function(key, options) {
                        thisInstance = HTMLWidgets.getInstance(helperTable).hot
                        thisInstance.setCellMeta(thisInstance.getSelectedLast()[0], thisInstance.getSelectedLast()[1], 'className', 'yellow');
                        thisInstance.render();
                        thisInstance1 = HTMLWidgets.getInstance(colorHelperTable).hot
                        thisInstance1.setDataAtCell(thisInstance.getSelectedLast()[0], thisInstance.getSelectedLast()[1], 'yellow')
                      }
                     ")
  ),
  makeGreen = list(name = "Green <div class=\"green-block\"></div>",
                   callback = htmlwidgets::JS(
                     "function(key, options) {
                        thisInstance = HTMLWidgets.getInstance(helperTable).hot
                        thisInstance.setCellMeta(thisInstance.getSelectedLast()[0], thisInstance.getSelectedLast()[1], 'className', 'green');
                        thisInstance.render();
                        thisInstance1 = HTMLWidgets.getInstance(colorHelperTable).hot
                        thisInstance1.setDataAtCell(thisInstance.getSelectedLast()[0], thisInstance.getSelectedLast()[1], 'green')
                      }
                     ")
  )
)