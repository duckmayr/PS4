library(rvest)

wikiURL <- 'https://en.wikipedia.org/wiki/'
topic <- 'List_of_United_States_presidential_elections_by_popular_vote_margin'

## Grab the tables from the page and use html_table() to extract the tables:

temp <- paste0(wikiURL, topic) %>%
  read_html %>%
  html_nodes("table")

# The table we want is the second element of temp:
voteTable <- html_table(temp[[2]])
# Only some of the columns are useful, and the first two rows are useless:
voteTable <- voteTable[3:nrow(voteTable), c(2:4, 6:8, 11:13)]
# Now we need some sensible column names:
colnames(voteTable) <- c('electionYear', 'winnerName', 'winnerParty',
                         'elecCollegeVote', 'popVote', 'popVoteMargin',
                         'loserName', 'loserParty', 'turnout')

# All the data we will want as numeric is currently stored as character.
# The following function will take any character representation of a number,
# remove any special characters, and return it as numeric.
stringToNum <- function(string){
  # first remove special characters and convert to numeric:
  result <- as.numeric(gsub('[^0-9\\.]+', '', string))
  # then deal with any negative signs:
  result[grepl('^[−-]', string)] <- -1 * result[grepl('^[−-]', string)]
  # and return the result:
  return(result)
}

# For some reason, the table has names as last name, full name.
# We need to remove the extra last name; this function will do it:
removeExtraName <- function(candidateName){
  return(sapply(strsplit(candidateName, ','), function(x) x[2]))
}

# Now we use the functions created above to clean the data:
voteTable[ , c(1, 4:6, 9)] <- apply(voteTable[ , c(1, 4:6, 9)], 2, stringToNum)
voteTable[ , c(2, 7)] <- apply(voteTable[ , c(2, 7)], 2, removeExtraName)

# Also, it will be convenient later to have the data temporally ordered:
voteTable <- voteTable[order(voteTable$electionYear), ]




## Now we need to scrape another table:

topic <- 'United_States_presidential_election'

temp <- paste0(wikiURL, topic) %>% 
  read_html %>%
  html_nodes("table")

# Unfortunately, html_table() doesn't work:
ECvoteTable <- html_table(temp[[3]]) # inconsistent number of columns
ECvoteTable <- html_table(temp[[3]], fill=TRUE) # subscript out of bounds

# So, I had to write my own functions to parse the table:

fillVector <- function(object, keepIndices, input){
  # This function replaces 0 or more values in a vector; while trivial,
  # defining this function prevents code bloat and breaking in parseTable()
  if(length(keepIndices) == 0){
    return(input)
  }
  object[-keepIndices] <- input
  return(object)
}

getRowspans <- function(rowIn, keepIndices, rowLength){
  # For each row in the table, we need to know if any of the cells will be
  # spread over subsequent rows, i.e. if any <td> contains a rowspan
  chars <- html_attr(html_nodes(rowIn, 'td'), 'rowspan')
  rowspans <- fillVector(rep(NA, rowLength), keepIndices, chars)
  return(as.numeric(ifelse(is.na(rowspans), 0, rowspans)))
}

parseRow <- function(rowIn, keepIndices, rowLength, lastRow){
  # This gets the text from every cell in the row;
  # calling fillVector to fill in values from lastRow lets us use the
  # previous row as default values in case there was a rowspan carrying over
  rowCells <- html_text(html_nodes(rowIn, 'td'), trim=TRUE)
  return(fillVector(lastRow, keepIndices, rowCells))
}

parseTable <- function(tableIn){
  # This function uses the helper functions above to actually parse tables.
  # First we get all the rows from tableIn:
  rows <- html_nodes(tableIn, 'tr')
  # Then we get the header as a character vector:
  header <- html_text(html_nodes(rows[1], 'th'), trim=TRUE)
  # It will be convenient to make a blank dataframe to fill in:
  result <- data.frame(matrix(NA, nrow=length(rows)-1, ncol=length(header)))
  # The counters variable will be used to keep track of rowspans.
  # To start off, we assume that each cell spans only one row:
  counters <- rep(1, length(header))
  # Now we can parse the rows in a loop.
  # I chose a loop approach rather than apply because I need to alter
  # variables in the function's environment as I go.
  for(i in 2:length(rows)){
    # I decrement any counters valued greater than one at each iteration
    # to account for the row the current row takes up in the rowspan:
    counters[which(counters > 1)] <- counters[which(counters > 1)] - 1
    # The keep indices variable tells me which columns have carryover values:
    keepIndices <- which(counters > 1)
    # On the first iteration, there will be no previous row
    if (i == 2) {
      lastRow <- rep(NA, length(header))
    } else { # but on all other rows we use the previous row for default values
      lastRow <- result[i-2, ]
    }
    # Then I use parseRow to get the text from the current row,
    # plus default values if there was carryover from a rowspan,
    # and put those values in the result dataframe
    result[i-1, ] <- parseRow(rows[i], keepIndices, length(header), lastRow)
    # and increase rowspan counters as necessary before the next loop iteration
    counters <- counters + getRowspans(rows[i], keepIndices, length(header))
  }
  # Lastly, it might be nice to have column names from the header:
  colnames(result) <- header
  return(result) # and we return the result
}

ECvoteTable <- parseTable(temp[[3]])