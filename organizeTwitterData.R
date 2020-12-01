## Clear everything
rm(list=ls())

## Load library(s)

## Declare global
# First declare the number of times a variable must be mentioned
mention.count <- 99
date.lower <- as.Date("2020-03-22")
date.upper <- Sys.Date() -1

## Read in the data
# This script will assume that the covid19_twitter repo has been cloned into the wd
# clone the repo from here: https://github.com/thepanacealab/covid19_twitter
# It should be in the same directory as the Rproj file

## Find all of the paths to read
dates.to.read <- list.files(path='./covid19_twitter/dailies/', pattern = "2020*")

# Now make sure we are working in the desiered date range
date.range <- seq(date.lower, date.upper, by="days")
dates.to.read <- dates.to.read[which(as.Date(dates.to.read) %in% date.range)]

## Now loop through them
all.terms <- NULL
for(i in dates.to.read){
  # first declare the full path
  full.path <- paste("./covid19_twitter/dailies/", i, "/", i, "_top1000terms.csv", sep='')
  # now read the csv
  in.data <- read.csv(full.path, header = F)
  # Now create a vector of just the terms used
  vals <- as.character(in.data[,1])
  ## Now append and move on
  all.terms <- c(all.terms, vals)
}
## Now find the terms with > 50 occurnaces
term.count <- table(all.terms)
terms.to.include <- names(which(term.count>mention.count))

## Now go through the terms again, load em, and then only include the values with the desiered terms
# First create the output dataframe to merge everything into
all.twitter.data <- matrix(NA, ncol=length(terms.to.include)+1, nrow=1)
colnames(all.twitter.data) <- c("date", terms.to.include)
all.twitter.data <- data.frame(all.twitter.data)
for(i in dates.to.read){
  # first declare the full path
  full.path <- paste("./covid19_twitter/dailies/", i, "/", i, "_top1000terms.csv", sep='')
  # now read the csv
  in.data <- read.csv(full.path, header = F)
  in.data <- t(in.data)
  # Now add column names to the data
  colnames(in.data) <- in.data[1,]
  # NOw turn in.data into a dataframe
  in.data <- data.frame(in.data)
  # Remove the first row
  in.data <- in.data[-1,]
  # Now isolate cols of interest
  in.data <- in.data[,which(names(in.data) %in% terms.to.include)]
  
  # Add a date column
  in.data$date <- i
  
  ## Now merge these together
  all.twitter.data <- merge(all.twitter.data, in.data, all=T)
}
## Remove the first row because that was used as a place holder
all.twitter.data <- all.twitter.data[-1,]

## Now write the csv
out.name <- paste("includeCount_", mention.count, "_dateLower_", date.lower, "_dateUpper_", date.upper, "TwitterCount.csv", sep='')
write.csv(all.twitter.data, out.name, quote=F, row.names=F)
