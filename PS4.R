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

# So, we need to use a different package:

library(htmltab)

ECvoteTable <- htmltab(paste0(wikiURL, topic), which = 3)
