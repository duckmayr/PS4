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
colnames(voteTable) <- c('year', 'winnerName', 'winnerParty',
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
voteTable <- voteTable[order(voteTable$year), ]


## Time to plot some trends using these data.

# It's convenient to define the colors we'll use:
highPts <- rgb(1, 0, 0, alpha=0.75)
lowPts <- rgb(0, 0, 1, alpha=0.75)
turnoutCol <- 'green4'
popVoteCol <- 'deepskyblue4'
ECvoteCol <- 'firebrick2'

# And a phrase we'll reuse in plot titles:
titleEnd <- 'in Presidential Elections 1824-1900'

# We're going to save these plots in a pdf:
pdf('electionPlots.pdf', width=4.5, height=8.5)

# There will be three total plots, one on top of another:
layout(matrix(1:3, ncol=1))

# The first plot shows how voter turnout has changed over time.
# Interestingly, voter turnout stays fairly stable except for a period of
# high turnout from 1840 to 1900:
plot(turnout ~ year, voteTable, pch=21, cex=0.9, col='black',
     bg=ifelse(voteTable$year %in% c(1840:1900), highPts, lowPts),
     xlab='Election Year', ylab='Voter Turnout (Percent)',
     main='Voter Turnout in Presidential\nElections From 1824 to 2016')
legend('bottomright', pt.bg=c(highPts, lowPts), bty='n', pch=21,
       legend=c('Elections from 1840-1900', 'Elections pre-1840 and post-1900'))

# The next plot shows how turnout and winners' popular vote trend together over
# time. Often, when we see an increase in voter turnout, we see a corresponding
# increase in the winner's popular vote percentage, except that winners' vote
# share does not increase in the period of high turnout from 1840 to 1900:
plot(NULL, xlim=c(1824, 2016), ylim=c(20, 100), type='n',
     xlab='Election Year', ylab='Percent',
     main=paste('Voter Turnout and Popular Vote', titleEnd, sep='\n'))
lines(turnout ~ year, voteTable, col=turnoutCol)
lines(popVote ~ year, voteTable, col=popVoteCol)
legend('bottomright', col=c(turnoutCol, popVoteCol), lty=1, bty='n',
       legend=c('Voter Turnout', 'Popular Vote (Winner)'))

# Finally, we look at how winners' popular vote percentage and electoral
# college vote percentage trend together over time. Generally speaking, an
# increase in one corresponds to an increase in the other, but the changes
# are much more exaggerated for electoral college vote:
plot(NULL, xlim=c(1824, 2016), ylim=c(20, 100), type='n',
     xlab='Election Year', ylab='Percent',
     main=paste('Electoral College and Popular Vote', titleEnd, sep='\n'))
lines(elecCollegeVote ~ year, voteTable, col=ECvoteCol)
lines(popVote ~ year, voteTable, col=popVoteCol)
legend('bottomright', col=c(ECvoteCol, popVoteCol), lty=1, bty='n',
       legend=c('Electoral College Vote (Winner)', 'Popular Vote (Winner)'))

# Now we turn off the plotting device and reset the graphics parameters:
dev.off()


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

# Which will be able to parse the table (note we only need three columns):
tmpTable <- htmltab(paste0(wikiURL, topic), which = 3)[ , c(1, 3, 7)]
colnames(tmpTable) <- c('year', 'candidate', 'ECvotes')
# Electoral college votes are given as a string of the format
# [number of votes received] / [number of votes possible to receive]
# so we need to get just the number of votes received:
tmpTable$ECvotes <- sapply(strsplit(tmpTable$ECvotes, ' / '),
                           function(x) as.numeric(x[1]))
# And now we can aggregate electoral college votes by year and candidate:
aggList <- list(year=tmpTable$year, candidate=tmpTable$candidate)
tmpTable <- aggregate(tmpTable$ECvotes, by=aggList, FUN=sum)
rm(aggList)
# And get only the top two electoral college vote winners by election:
tmpTable <- tmpTable[order(tmpTable$year, tmpTable$x, decreasing=TRUE), ]
tmpTable <- Reduce(rbind, by(tmpTable, tmpTable['year'], head, n=2))
# It will be convenient to separate the winners from the runners up:
winners <- tmpTable[seq(from=1, to=nrow(tmpTable), by=2), ]
losers <- tmpTable[seq(from=2, to=nrow(tmpTable), by=2), ]
# And now we can merge in this data to the original table we scraped:
resultTable <- merge(voteTable, winners, by='year', all.x=TRUE)
resultTable <- merge(resultTable, losers, by='year', all.x=TRUE)
colnames(resultTable)[10:13] <- c('ECwinner', 'ECwinnerVote',
                                  'ECrunnerup', 'ECrunnerupVote')
resultTable$ECrunnerupVote[13] <- 42 # See FN 57 from the Wikipedia page
save(resultTable, file='presidentialElections.Rdata')