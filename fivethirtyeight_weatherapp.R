# This uses fivethirtyeight.com's survey data on methods used to check the weather
data <- read.csv('https://raw.githubusercontent.com/fivethirtyeight/data/master/weather-check/weather-check.csv', skip=1)
data <- data.frame(data)
#rename your factors
names(data) <- c("ID","CheckDaily","CheckMethod","SpecificMethodText","SmartwatchCheck","Age","Gender","HouseholdIncome","USRegion")

require(ggplot2)
require(MASS)
require(reshape)
require(scales)

print(levels(data$SmartwatchCheck))
data$SmartwatchCheck <- factor(data$SmartwatchCheck, levels=c(
  "-","Very unlikely","Somewhat unlikely","Somewhat likely","Very likely"))

print(levels(data$HouseholdIncome))
data$HouseholdIncome <- factor(data$HouseholdIncome, levels=c(
  "-","$0 to $9,999","$10,000 to $24,999","$25,000 to $49,999", 
  "$50,000 to $74,999","$75,000 to $99,999","$100,000 to $124,999", 
  "$125,000 to $149,999","$150,000 to $174,999","$175,000 to $199,999", 
  "$200,000 and up","Prefer not to answer"))


#######
#Autogen a bunch of plots to explore data, look for interesting bits.
#######

#Plot the stacked bar plots for all combinations to explore data.
#These show relative response numbers; ratios in next set of plots.
#Note col5 has not yet been cleaned so those plots are strange.
colnames <- colnames(data)
for (i in 2:dim(data)[2]) {
  for (j in 3:dim(data)[2]) {
    p <- ggplot(data, aes(x=data[[i]], fill=data[[j]])) + geom_bar() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      xlab(colnames[i]) + ylab(colnames[j])
    print(p)
    print(c(i,j))
  }
}

#Plot the stacked percentage plots for all combinations to look for trends.
#Gives all combinations of the ordinal and nominal levels.
#Note col5 has not yet been cleaned so those plots are strange.
colnames <- colnames(data)
for (i in 2:dim(data)[2]) {
  for (j in 3:dim(data)[2]) {
    p <- ggplot(data, aes(x=data[[i]])) +
      geom_bar(aes(fill = data[[j]]), position = 'fill') + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      xlab(colnames[i]) + ylab(colnames[j])
    print(p)
    print(c(i,j))
  }
}


#ChiSq between married and cohabitating: Do married and single but
#cohabitating partners have the same expected distribution of time
#not sleeping together
tbl <- table(data[,3], data[,5])
chisq.test(tbl)  #sig, so they're different--but this is across ALL statuses.

data.statuspartnered <- subset(data, data[,3]=="Married" | 
                                 data[,3]=="Single, but cohabiting with a significant other")
#Drop empty levels so you don't have cells with 0-counts.
data.statuspartnered[,3] <- as.factor(as.character(data.statuspartnered[,3]))
data.statuspartnered[,5] <- as.factor(as.character(data.statuspartnered[,5]))
tbl <- table(data.statuspartnered[,3], data.statuspartnered[,5])
chisq.test(tbl)  #not significantly different; p=0.2114