# This uses fivethirtyeight.com's survey data on methods used to check the weather
data <- read.csv('https://raw.githubusercontent.com/fivethirtyeight/data/master/weather-check/weather-check.csv', skip=1)
data <- data.frame(data)
#rename your factors
names(data) <- c("ID","CheckDaily","CheckMethod","SpecificMethodText","SmartwatchCheck","Age","Gender","HouseholdIncome","USRegion")

require(ggplot2)
require(MASS)
require(gdata)
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

ggplot(data, aes(x=HouseholdIncome, fill=CheckDaily)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data, aes(x=HouseholdIncome, fill=CheckMethod)) + geom_bar(fill=) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data, aes(x=HouseholdIncome, fill=SmartwatchCheck)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = HouseholdIncome)) + 
  geom_bar(aes(fill = CheckMethod), position = 'fill') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("How Different Household Income Brackets Check the Weather")


ggplot(data, aes(x=Gender)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data, aes(x=USRegion)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data, aes(x=HouseholdIncome)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))



#LOOP not working.  Figure that out.
#for variable1 = 1 to last variable
  #for variable2 = 1 to last variable
    #ggplot statement
for (inx in 1:dim(data)[2]) {
  for (iny in 1:dim(data)[2]) {
    
    #if(inx==iny) next()
    
    #showplot1<-function(data, inx, iny) {
      #dat <- data
      p <- ggplot(data, aes(x=data[,inx], y=data[,iny])) +
        geom_bar(aes(fill = data[,iny]), position = 'fill') + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      print(p)
  }
}



#try different plots to visualize
ggplot(data, aes(x=Response.2)) + geom_bar(labels = percent_format())  #+ 
  xlab("Relationship Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylim(0,1000)

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

#seeing if non markdown works to push to git

