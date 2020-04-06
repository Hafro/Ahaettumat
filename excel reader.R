# Script to read in the csv files to edit the river and farmsite data

riverdata <- read.csv('ar.csv',header = TRUE,sep = ',')
riverdata$V <- as.numeric(sub(",", ".", as.character(riverdata$V),fixed = TRUE))
riverdata$N <- as.numeric(sub(",", ".", as.character(riverdata$N),fixed = TRUE))
colnames(riverdata) <- c('RiverName','V','N','position','Stock.Size','SizeCategory')

farmsites <- read.csv('eldisstadir.csv',header = TRUE,sep = ',')
farmsites$V <- as.numeric(sub(",", ".", as.character(farmsites$V),fixed = TRUE))
farmsites$N <- as.numeric(sub(",", ".", as.character(farmsites$N),fixed = TRUE))
farmsites$Stock <- as.numeric(sub(",", ".", as.character(farmsites$Stock),fixed = TRUE))
colnames(farmsites) <- c('SiteName','position','V','N','Stock','max')
save.image('Data.RData')

for( i in rownames(farmsites)){
  eval(parse(text=paste('inputs$tonn',i,"=farmsites[i,'Stock']",sep="")))
  save(inputs,file = "inputs.RData")
}

