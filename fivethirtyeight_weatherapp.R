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
      xlab(colnames[i]) + ylab(colnames[j]) + 
    guides(fill=guide_legend(title=colnames[j]))
    print(p)
    print(c(i,j))
  }
}

#Plot all stacked percentage plots to look for ratio trends.
#Gives all combinations of the ordinal and nominal levels.
#Note col5 has been cleaned (c11) so c5,c10 plots are strange.
colnames <- colnames(data)
for (i in 2:dim(data)[2]) {
  for (j in 3:dim(data)[2]) {
    p <- ggplot(data, aes(x=data[[i]])) +
      geom_bar(aes(fill = data[[j]]), position = 'fill') + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      xlab(colnames[i]) + ylab(colnames[j]) + 
      guides(fill=guide_legend(title=colnames[j])) + 
      ggtitle(paste("% of Responders: ", colnames[i], "vs ", colnames[j]))
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
                              data[,3]=="Single, but cohabiting with 
                              a significant other")
#Drop empty levels so you don't have cells with 0-counts.
data.statuspartnered[,3] <- as.factor(as.character(data.statuspartnered[,3]))
data.statuspartnered[,5] <- as.factor(as.character(data.statuspartnered[,5]))
tbl <- table(data.statuspartnered[,3], data.statuspartnered[,5])
chisq.test(tbl)  #not significantly different; p=0.2114

#Try to collapse the specific method levels into groups.
#Text cleaning first.
require(tm)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
#Convert text to corpus
method.corpus <- Corpus(VectorSource(data$SpecificMethodText))
inspect(method.corpus[1:10])
#Convert to lowercase
method.corpus <- tm_map(method.corpus, content_transformer(tolower))
#Remove punctuation
method.corpus <- tm_map(method.corpus, removePunctuation)
#Strip stopwords - not applicable here, but future stuff consider it.
#Strip white space
method.corpus <- tm_map(method.corpus, stripWhitespace)

test <- data.frame(text=unlist(sapply(method.corpus, `[`, "content")),
                   stringsAsFactors=T)
levels(test$text)
test$text <- as.factor(trim(test$text))
  #the tm commands + trim function got us from 122 levels down to 94.  
#append the 'cleaned' vector back to original dataframe
data$CleanMethodText.temp <- test$text


#manually clean remaining 94 categories
#Some of these are pushing NA observations -- fix that next..
#data$CleanMethodText <- as.character(data$CleanMethodText)
data$CleanMethodText.temp <- as.character(data$CleanMethodText.temp)

data$CleanMethodText <- NULL

data$CleanMethodText[which(data$CleanMethodText.temp == "1 weather")] <- "1weather"
data$CleanMethodText[which(data$CleanMethodText.temp == "1weather")] <- "1weather"
data$CleanMethodText[which(data$CleanMethodText.temp == "accuweather")] <- "accuweather"
data$CleanMethodText[which(data$CleanMethodText.temp == "accuweather app")] <- "accuweather"
data$CleanMethodText[which(data$CleanMethodText.temp == "accuweather weather underground")] <- "accuweather"
data$CleanMethodText[which(data$CleanMethodText.temp == "aol")] <- "aol"
data$CleanMethodText[which(data$CleanMethodText.temp == "app iphone")] <- "default"
data$CleanMethodText[which(data$CleanMethodText.temp == "apple weater")] <- "default"
data$CleanMethodText[which(data$CleanMethodText.temp == "apple weather app")] <- "default"
data$CleanMethodText[which(data$CleanMethodText.temp == "appleprovided site")] <- "default"
data$CleanMethodText[which(data$CleanMethodText.temp == "basic weather app iphone")] <- "default"
data$CleanMethodText[which(data$CleanMethodText.temp == "bing")] <- "bing"
data$CleanMethodText[which(data$CleanMethodText.temp == "chrome app")] <- "vague"
data$CleanMethodText[which(data$CleanMethodText.temp == "dark sky")] <- "dark sky"
data$CleanMethodText[which(data$CleanMethodText.temp == "default iphone app")] <- "default"
data$CleanMethodText[which(data$CleanMethodText.temp == "desktop icon")] <- "vague"
data$CleanMethodText[which(data$CleanMethodText.temp == "directv weather app")] <- "directv"
data$CleanMethodText[which(data$CleanMethodText.temp == "fancyclock app phone accuweathercom")] <- "accuweather"
data$CleanMethodText[which(data$CleanMethodText.temp == "goes west satalite")] <- "goes west"
data$CleanMethodText[which(data$CleanMethodText.temp == "google app")] <- "google"
data$CleanMethodText[which(data$CleanMethodText.temp == "google now")] <- "google"
data$CleanMethodText[which(data$CleanMethodText.temp == "google weather")] <- "google"
data$CleanMethodText[which(data$CleanMethodText.temp == "husband usually informs next days weather")] <- "ask a human"
data$CleanMethodText[which(data$CleanMethodText.temp == "intellicast")] <- "intellicast"
data$CleanMethodText[which(data$CleanMethodText.temp == "intellicast site wunderground app")] <- "intellicast"
data$CleanMethodText[which(data$CleanMethodText.temp == "intellicast storm")] <- "intellicast"
data$CleanMethodText[which(data$CleanMethodText.temp == "internet tv radio newspaper")] <- "vague"
data$CleanMethodText[which(data$CleanMethodText.temp == "iphone")] <- "default"
data$CleanMethodText[which(data$CleanMethodText.temp == "iphone app")] <- "default"
data$CleanMethodText[which(data$CleanMethodText.temp == "iphone weather app")] <- "default"
data$CleanMethodText[which(data$CleanMethodText.temp == "ipod weather")] <- "default"
data$CleanMethodText[which(data$CleanMethodText.temp == "kcra online")] <- "kcra"
data$CleanMethodText[which(data$CleanMethodText.temp == "local newspaper online weather bug noah")] <- "weatherbug"
data$CleanMethodText[which(data$CleanMethodText.temp == "look outside")] <- "look outside"
data$CleanMethodText[which(data$CleanMethodText.temp == "lookfeel weather outside")] <- "look outside"
data$CleanMethodText[which(data$CleanMethodText.temp == "maccuweathercom")] <- "accuweather"
data$CleanMethodText[which(data$CleanMethodText.temp == "myphone")] <- "vague"
data$CleanMethodText[which(data$CleanMethodText.temp == "national weather service")] <- "national weather service"
data$CleanMethodText[which(data$CleanMethodText.temp == "national weather service site")] <- "national weather service"
data$CleanMethodText[which(data$CleanMethodText.temp == "nice")] <- "vague"
data$CleanMethodText[which(data$CleanMethodText.temp == "noaa app")] <- "noaa"
data$CleanMethodText[which(data$CleanMethodText.temp == "noaa weather channel")] <- "noaa"
data$CleanMethodText[which(data$CleanMethodText.temp == "noaagov")] <- "noaa"
data$CleanMethodText[which(data$CleanMethodText.temp == "numerous local weather apps")] <- "vague"
data$CleanMethodText[which(data$CleanMethodText.temp == "one comes iphone")] <- "default"
data$CleanMethodText[which(data$CleanMethodText.temp == "phone")] <- "vague"
data$CleanMethodText[which(data$CleanMethodText.temp == "phone preloaded app")] <- "default"
data$CleanMethodText[which(data$CleanMethodText.temp == "smart phone")] <- "vague"
data$CleanMethodText[which(data$CleanMethodText.temp == "storm team 4")] <- "storm team 4"
data$CleanMethodText[which(data$CleanMethodText.temp == "talk mother")] <- "ask a human"
data$CleanMethodText[which(data$CleanMethodText.temp == "via iphone builtin app")] <- "default"
data$CleanMethodText[which(data$CleanMethodText.temp == "weather app")] <- "vague"
data$CleanMethodText[which(data$CleanMethodText.temp == "weather app iphone")] <- "default"
data$CleanMethodText[which(data$CleanMethodText.temp == "weather bug")] <- "weatherbug"
data$CleanMethodText[which(data$CleanMethodText.temp == "weather bug app")] <- "weatherbug"
data$CleanMethodText[which(data$CleanMethodText.temp == "weather channel")] <- "weather channel"
data$CleanMethodText[which(data$CleanMethodText.temp == "weather channel app")] <- "weather channel"
data$CleanMethodText[which(data$CleanMethodText.temp == "weather channel app android")] <- "weather channel"
data$CleanMethodText[which(data$CleanMethodText.temp == "weather channel app iphone")] <- "weather channel"
data$CleanMethodText[which(data$CleanMethodText.temp == "weather channel app phone")] <- "weather channel"
data$CleanMethodText[which(data$CleanMethodText.temp == "weather channel app phone ipad")] <- "weather channel"
data$CleanMethodText[which(data$CleanMethodText.temp == "weather channel ipad app")] <- "weather channel"
data$CleanMethodText[which(data$CleanMethodText.temp == "weather channel iphone ipad")] <- "weather channel"
data$CleanMethodText[which(data$CleanMethodText.temp == "weather channel local tv kxan")] <- "weather channel"
data$CleanMethodText[which(data$CleanMethodText.temp == "weather channelcom")] <- "weather channel"
data$CleanMethodText[which(data$CleanMethodText.temp == "weather clock widget")] <- "vague"
data$CleanMethodText[which(data$CleanMethodText.temp == "weather kitty")] <- "weather kitty"
data$CleanMethodText[which(data$CleanMethodText.temp == "weather puppy")] <- "weather puppy"
data$CleanMethodText[which(data$CleanMethodText.temp == "weather risk")] <- "weather risk"
data$CleanMethodText[which(data$CleanMethodText.temp == "weather timeline android")] <- "weather timeline"
data$CleanMethodText[which(data$CleanMethodText.temp == "weather underground")] <- "weather underground"
data$CleanMethodText[which(data$CleanMethodText.temp == "weather underground also local tv news half time")] <- "weather underground"
data$CleanMethodText[which(data$CleanMethodText.temp == "weather underground hd widgets")] <- "weather underground"
data$CleanMethodText[which(data$CleanMethodText.temp == "weatherbug")] <- "weatherbug"
data$CleanMethodText[which(data$CleanMethodText.temp == "weatherbug app")] <- "weatherbug"
data$CleanMethodText[which(data$CleanMethodText.temp == "weatherbug phone")] <- "weatherbug"
data$CleanMethodText[which(data$CleanMethodText.temp == "weatherbug storm")] <- "weatherbug"
data$CleanMethodText[which(data$CleanMethodText.temp == "weatherbugcom")] <- "weatherbug"
data$CleanMethodText[which(data$CleanMethodText.temp == "weathercom")] <- "weather channel"
data$CleanMethodText[which(data$CleanMethodText.temp == "weatherforyoucom")] <- "weatherforyou"
data$CleanMethodText[which(data$CleanMethodText.temp == "weathergov")] <- "national weather service"
data$CleanMethodText[which(data$CleanMethodText.temp == "weatherunderground")] <- "weather underground"
data$CleanMethodText[which(data$CleanMethodText.temp == "wunderground")] <- "weather underground"
data$CleanMethodText[which(data$CleanMethodText.temp == "wundergroundcom")] <- "weather underground"
data$CleanMethodText[which(data$CleanMethodText.temp == "wwwweathercom")] <- "weather channel"
data$CleanMethodText[which(data$CleanMethodText.temp == "wwwweathergov")] <- "national weather service"
data$CleanMethodText[which(data$CleanMethodText.temp == "wwwwundergroundcom")] <- "weather underground"
data$CleanMethodText[which(data$CleanMethodText.temp == "yahoo")] <- "yahoo"
data$CleanMethodText[which(data$CleanMethodText.temp == "yahoo weather")] <- "yahoo"
data$CleanMethodText[which(data$CleanMethodText.temp == "yahoo weather app")] <- "yahoo"
data$CleanMethodText[which(data$CleanMethodText.temp == "yahoo weather iphone")] <- "yahoo"
data$CleanMethodText[which(data$CleanMethodText.temp == "yo window")] <- "look outside"
data$CleanMethodText[which(data$CleanMethodText.temp == "yrno")] <- "yr.no"
                                                                                                                                                    test$text=="yrno", "yr.no", NA))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) 
#coerce the cleaned text back ito a factor format.  28 lvls now.

data$CleanMethodText <- as.factor(data$CleanMethodText)

#Subset to incude only participants who use a specific website or app
data.spec <- subset(data, select=c(2,3,5,6,7,8,9,11), 
                    data$CheckMethod == "A specific website or app (please provide the answer)" & 
                    data$CleanMethodText != "default" & 
                    data$CleanMethodText != "vague" &  
                    data$CleanMethodText != "ask a human" & 
                    data$CleanMethodText != "look outside")
#Check table to see if you ruined the factor ordering
table(data.spec$CleanMethodText)

#Reorganize factors decreasing order.  So bar plot looks better.
#levels(data.spec$CleanMethodText)<-levels(data.spec$CleanMethodText)[rev(order(tabulate(data.spec$CleanMethodText)))] 
barplot(sort(table(data.spec$CleanMethodText), decreasing = TRUE))

#Re-do plots, specifically looking at all categories v specific text  
colnames <- colnames(data.spec)
for (i in 1:dim(data.spec)[2]) {
  for (j in 8:dim(data.spec)[2]) {
    p <- ggplot(data.spec, aes(x=data.spec[[i]])) +
      geom_bar(aes(fill = data.spec[[j]]), position = 'fill') + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      xlab(colnames[i]) + ylab(colnames[j]) + 
      guides(fill=guide_legend(title=colnames[j])) + 
      ggtitle(paste("% of Responders: ", colnames[i], "vs ", colnames[j]))
    print(p)
    print(c(i,j))
  }
}

p <- ggplot(data.spec, aes(x=CleanMethodText)) + geom_histogram() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  xlab("Specific App/Website") + ylab("Number of Responses")

