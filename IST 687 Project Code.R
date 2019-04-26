#
#      Course: IST687
#      PROJECT
#      TEAM ANKYLOSAURUS
#     
#      Doug Gerard
#      Tamora Long
#      Tyler MArsh
#      Nicole Novakidis
#


#libraries
install.packages(c("parallel", "MASS", "utils", "ggplot2", "RColorBrewer", "ggcorplot", "e1071", "mir", "kimisc", "installr"))
library(MASS)
library(parallel)
library(utils)
library(ggplot2)
library(ggcorrplot)
library(RColorBrewer)
library(e1071)
library(mlr)
library(kimisc)
library(installr)

theme_set(theme_minimal())

# Team Ankylosaurus colors
#code for colors sourced from https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2
ankylosaurusColors <- c(
  `brightred`  = "#FF0000",
  `red`        = "#a00000",
  `darkred`    = "#400000",
  `orange`     = "#f37735",
  `yellow`     = "#ffc425",
  "brightgreen"= "#00FF00",
  `green`      = "#00b159",
  `darkgreen`  = "#008000",
  "brightblue" = "#0000FF",
  `blue`       = "#00aedb",
  'darkblue'   = "#000080",
  `lightgrey`  = "#A0A0A0",
  `grey`       = "#808080",
  'darkgrey'   = "#303030",
  'black'      = "#000000"
  )

#' Function to extract ankylosaurusColors as hex codes
#'
#' @param ... Character names of ankylosaurusColors 
#'
ankylosaurusCols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (ankylosaurusColors)
  
  ankylosaurusColors[cols]
}

ankylosaurusPalettes <- list(
  `main`  = ankylosaurusCols("red", "orange", "yellow", "green", "blue", "grey", "black"),
  
  `all`  = ankylosaurusCols("brightred", "red", "darkred", "orange", "yellow", "brightgreen", 
                            "green", "darkgeen", "brightblue", "blue", "darkblue",
                            "lightgrey", "grey", "darkgrey", "black"),
  
  
  `cool`  = ankylosaurusCols("blue", "green"),
  
  `hot`   = ankylosaurusCols("yellow", "orange", "red"),
  
  `mixed` = ankylosaurusCols("blue", "green", "yellow", "orange", "red"),
  
  `grey`  = ankylosaurusCols("grey", "black"),
  
  `binary`  = ankylosaurusCols("red", "blue")
)

#' Return function to interpolate a ankylosaurusPalettes color palette
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
AnkylosaurusPal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- ankylosaurusPalettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

#' Color scale constructor for drsimonj colors
#'
#' @param palette Character name of palette in ankylosaurusPalettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
ScaleColorAnkylosaurus <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...)
{
  pal <- AnkylosaurusPal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("ankylosaurus", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


#useful functions
Trim <- function (x) gsub("^\\s+|\\s+$", "", x)
ToLowerCase <- function (x) sapply(x, tolower)
TrimLowerCase <- function (x) Trim(ToLowerCase(x))

Map<- function(x, y)
{
  lowercasey <- TrimLowerCase(y)
  res <- data.frame(data = x);
  res$Map <- match(TrimLowerCase(x), lowercasey)
  missed <- unique(unlist(res$data[is.na(res$Map) == TRUE]))

  if (length(missed) > 0)
  {
    print("Error missed mapped items")
    print(missed)
    warning(c("Error missed mapped items:", missed))
  }
  
  return(res$Map);
}

ChiSquaredTest <- function(x, y, alpha = 0.05)
{
  tbl <- table(x, y)
  print(tbl)
  
  ct <- chisq.test(tbl)
  print(ct)
  
  print(paste("Alpha:", alpha))
  
  print(paste("P value is", 
              ifelse((ct$p.value < alpha), 
                     "lower than alpha, reject null hypothesis.",
                     "higher than alpha, accept null hypothesis.")))
}

NADescription <- function(dataframe)
{
  print("NA summary")
  print(c("Number of rows with NA: ", sum(rowSums(is.na(dataframe)))))
  
  c <- apply(dataframe, 2, function(dataframe) length(which(is.na(dataframe))))
  c <- ifelse(c > 0, c, NA)
  c <- c[!is.na(c)]
  print("Columns with NA")
  print(c)
}

DescribeData <- function(description, dataframe)
{
  print(description)
  print("============================")
  summary(dataframe)
  NADescription(dataframe)
  print("")
}

#set working directory
setwd("D:/Users/Douglas/OneDrive/OneDrive - Jackalope Technologies, Inc/ATHENA/Documents/Syracuse/IST 687/Project")
getwd()

#
# READ in the data set
dataSet <- read.csv(file.choose(), TRUE)

DescribeData("Before processing", dataSet)

#replace NAs with mean
for(i in 1:ncol(dataSet)){
  dataSet[is.na(dataSet[,i]), i] <- mean(dataSet[,i], na.rm = TRUE)
}

#Add a status column to Map BLUE=1, SILVER=2, GOLD=3, PLATINUM=4
statusList <- c("blue", "silver", "gold", "platinum")
dataSet$code.status <- Map(dataSet$Airline.Status, statusList)
dataSet$factor.status <- factor(statusList[dataSet$code.status], levels = statusList)
dataSet$code.BlueStatus <- ifelse(dataSet$code.status == 1, 1, 0)
dataSet$code.SilverStatus <- ifelse(dataSet$code.status == 2, 1, 0)
dataSet$code.GoldStatus <- ifelse(dataSet$code.status == 3, 1, 0)
dataSet$code.PlatniumStatus <- ifelse(dataSet$code.status == 4, 1, 0)

airlineNames <- sort(c("EnjoyFlying Air Services", "FlyFast Airways Inc.", "FlyHere Airways", 
                  "FlyToSun Airlines Inc.", "GoingNorth Airlines Inc.", "West Airways Inc.", 
                  "OnlyJets Airlines Inc.", "Northwest Business Airlines Inc.", "Oursin Airlines Inc.", 
                  "Paul Smith Airlines Inc.", "Sigma Airlines Inc.", "Cheapseats Airlines Inc.", 
                  "Southeast Airlines Co.", "Cool&Young Airlines Inc."))
dataSet$code.airlineMapping <- Map(dataSet$Airline.Name, airlineNames)
dataSet$factor.airlineName <- factor(airlineNames[dataSet$code.airlineMapping], levels = airlineNames)

dataSet$code.EnjoyFlyingAirServices <- ifelse("EnjoyFlying Air Services" == dataSet$factor.airlineName, 1, 0)
dataSet$code.FlyFastAirwaysInc <- ifelse("FlyFast Airways Inc." == dataSet$factor.airlineName, 1, 0)
dataSet$code.FlyHereAirways  <- ifelse("FlyHere Airways" == dataSet$factor.airlineName, 1, 0)
dataSet$code.FlyToSunAirlinesInc <- ifelse("FlyToSun Airlines Inc." == dataSet$factor.airlineName, 1, 0)
dataSet$code.GoingNorthAirlinesInc <- ifelse("GoingNorth Airlines Inc." == dataSet$factor.airlineName, 1, 0)
dataSet$code.WestAirwaysInc <- ifelse("West Airways Inc." == dataSet$factor.airlineName, 1, 0)
dataSet$code.OnlyJetsAirlinesInc <- ifelse("OnlyJets Airlines Inc." == dataSet$factor.airlineName, 1, 0)
dataSet$code.NorthwestBusinessAirlinesInc <- ifelse("Northwest Business Airlines Inc." == dataSet$factor.airlineName, 1, 0)
dataSet$code.OursinAirlinesInc <- ifelse("Oursin Airlines Inc." == dataSet$factor.airlineName, 1, 0)
dataSet$code.PaulSmithAirlinesInc <- ifelse("Paul Smith Airlines Inc." == dataSet$factor.airlineName, 1, 0)
dataSet$code.SigmaAirlinesInc <- ifelse("Sigma Airlines Inc." == dataSet$factor.airlineName, 1, 0)
dataSet$code.CheapseatsAirlinesInc <- ifelse("Cheapseats Airlines Inc." == dataSet$factor.airlineName, 1, 0)
dataSet$code.SoutheastAirlinesCo <- ifelse("Southeast Airlines Co." == dataSet$factor.airlineName, 1, 0)
dataSet$code.CoolYoungAirlinesInc <- ifelse("Cool&Young Airlines Inc." == dataSet$factor.airlineName, 1, 0)

#Map Gender FEMALE=1, MALE=2
gender <- c("female", "male")
dataSet$code.gender <- Map(dataSet$Gender, gender)
dataSet$factor.gender <- factor(gender[dataSet$code.gender], levels = gender)
dataSet$code.female <- (1 - (dataSet$code.gender - 1))
dataSet$code.male <- (dataSet$code.gender - 1)

#add years flying
dataSet$code.yearsFlying <- 2019-dataSet$Year.of.First.Flight

#Map travel type MILEAGE TRAVEL=1, PERSON TRAVEL=2, BUSINESS TRAVEL=3
dataSet$code.travelType <- Map(dataSet$Type.of.Travel, TrimLowerCase(c("Mileage tickets", "Business travel", "Personal Travel")))
dataSet$code.MileageTravel  <- ifelse(dataSet$code.travelType == 1, 1, 0)
dataSet$code.BusinessTravel  <- ifelse(dataSet$code.travelType == 2, 1, 0)
dataSet$code.PersonalTravel  <- ifelse(dataSet$code.travelType == 3, 1, 0)

travelTypeList <- c("Mileage tickets", "Business travel", "Personal Travel")
dataSet$factor.travelType <- factor(travelTypeList[dataSet$code.travelType], levels = travelTypeList)

#Map class FIRST=1, BUSINESS = 2, ECONOMY PLUS=3, Exonmoy
dataSet$code.Class <- Map(dataSet$Class, c("first", "business", "eco plus", "eco"))
dataSet$code.FirstClass  <- ifelse(dataSet$code.Class == 1, 1, 0)
dataSet$code.BusinessClass  <- ifelse(dataSet$code.Class == 2, 1, 0)
dataSet$code.EcoPLusClass  <- ifelse(dataSet$code.Class == 3, 1, 0)
dataSet$code.EconomyClass  <- ifelse(dataSet$code.Class == 4, 1, 0)

classList <- c("first", "business", "eco plus", "eco")
dataSet$factor.class <- factor(classList[dataSet$code.Class], levels = classList)

#price Sensitivity
dataSet$code.Sensitivity.VeryHigh <- ifelse(dataSet$Price.Sensitivity == 5, 1, 0)
dataSet$code.Sensitivity.High <- ifelse(dataSet$Price.Sensitivity == 4, 1, 0)
dataSet$code.Sensitivity.Medium <- ifelse(dataSet$Price.Sensitivity == 3, 1, 0)
dataSet$code.Sensitivity.Low <- ifelse(dataSet$Price.Sensitivity == 2, 1, 0)
dataSet$code.Sensitivity.VeryLow <- ifelse(dataSet$Price.Sensitivity == 1, 1, 0)
dataSet$code.Sensitivity.NotSensitive <- ifelse(dataSet$Price.Sensitivity == 0, 1, 0)

sensativityList <- c("NotSensative", "VeryLow", "Low", "Medium", "High", "VeryHigh")
dataSet$factor.Sensativity <- factor(sensativityList[as.integer(dataSet$Price.Sensitivity) + 1], levels = sensativityList)

#Map flightCanceled yes = 1, no = 0
yesno <- c("no", "yes")
dataSet$code.Cancelled <- Map(dataSet$Flight.cancelled, yesno) - 1
dataSet$factor.Cancelled <- factor(yesno[dataSet$code.Cancelled + 1], levels = yesno)

#Map arrival delay  yes = 1, no = 0
dataSet$code.arrivalDelay <- Map(dataSet$Arrival.Delay.greater.5.Mins, c("no", "yes")) - 1
dataSet$factor.arrivalDelay <- factor(yesno[dataSet$code.arrivalDelay + 1], levels = yesno)

#get statisfaction as an integer...rounding to get rid of and half values.
dataSet$code.Satisfaction <- as.integer(round(as.numeric(dataSet$ï..Satisfaction), 0))

satisfactionList <- sort(unique(dataSet$code.Satisfaction))
dataSet$factor.Satisfaction <- factor(satisfactionList[dataSet$code.Satisfaction], levels = satisfactionList)

#reduce dataset to folks that are un happy
unhappy <- dataSet[dataSet$code.Satisfaction<5,]

#reduce dataset to folks that are un happy
happy <- dataSet[dataSet$code.Satisfaction>5,]

#get data as south east
southeast <- dataSet[Trim(dataSet$Airline.Name) == "Southeast Airlines Co.",]
southeastMean <- mean(southeast$code.Satisfaction)

#get data as south east unhappy
southeastUnhappy <- southeast[southeast$code.Satisfaction<5,]

#get data as south east happy
southeastHappy <- southeast[southeast$code.Satisfaction>5,]

#get data as south east
competition <- dataSet[Trim(dataSet$Airline.Name) != "Southeast Airlines Co.",]

#get data as competition unhappy
competitionUnhappy <- competition[competition$code.Satisfaction<5,]

#get data as competition happy
competitionHappy <- competition[competition$code.Satisfaction>5,]

makePDF <- ask.user.yn.question("Generate PDF file?", GUI=TRUE)
makePlots <- ask.user.yn.question("Generate plots?", GUI=TRUE)

#output to PDF
if(makePDF == TRUE)
  pdf(file='project.pdf', width = 7, height=5)

#distribution of unhappy customer atttributes
PlotSet <- function(data, title)
{
  print(title)
  print("================")
  
  #Satisfaction
  g <- ggplot(data, aes(factor(code.Satisfaction)))
  print(g + geom_bar(aes(fill = Gender)) + theme(legend.position = "top") +
    ggtitle(paste(title, "by Satisfaction")) + 
    labs(x = "Satisfaction", y = "Count", fill="Gender") +
    scale_fill_manual(values = AnkylosaurusPal('binary')(2))    
  )
  
  #Gender
  g <- ggplot(data, aes(Gender))
  print(g + geom_bar(aes(fill = factor(code.Satisfaction)))  + theme(legend.position = "top") +
    ggtitle(paste(title, "by Gender")) + labs(x = "Gender", y = "Count", fill="Satisfaction") +
    scale_fill_manual(values = AnkylosaurusPal()(10))    
  )
  
  # by age
  g <- ggplot(data, aes(Age))
  print(g + geom_bar(binwidth=5,aes(fill = factor(code.Satisfaction)))  + theme(legend.position = "top") +
    ggtitle(paste(title, "by Age")) + labs(x = "Age", y = "Count", fill="Satisfaction") +
    scale_fill_manual(values = AnkylosaurusPal()(10))    
  )
  
  # by age/Female
  sd <- data[data$code.gender==1,]
  g <- ggplot(sd, aes(Age))
  print(g + geom_bar(binwidth=5,aes(fill = factor(code.Satisfaction))) + theme(legend.position = "top") +
    ggtitle(paste(title, "by Female Age")) + labs(x = "Age", y = "Count", fill="Satisfaction") +
    scale_fill_manual(values = AnkylosaurusPal()(10))    
  )
  
  # by age/Male
  sd <- data[data$code.gender != 1,]
  g <- ggplot(sd, aes(Age))
  print(g + geom_bar(binwidth=5,aes(fill = factor(code.Satisfaction))) + theme(legend.position = "top") +
    ggtitle(paste(title, "by Male Age")) + labs(x = "Age", y = "Count", fill="Satisfaction") +
    scale_fill_manual(values = AnkylosaurusPal()(10))    
  )
  
  # ticket class
  g <- ggplot(data, aes(Class))
  print(g + geom_bar(aes(fill = factor(code.Satisfaction))) + theme(legend.position = "top") +
    ggtitle(paste(title, "by Ticket Class")) + labs(x = "Ticket Class", y = "Count", fill="Satisfaction") +
    scale_fill_manual(values = AnkylosaurusPal()(10))    
  )
  
   #Airline status
  g <- ggplot(data, aes(Airline.Status))
  print(g + geom_bar(aes(fill = factor(code.Satisfaction))) + theme(legend.position = "top") +
    ggtitle(paste(title, "by Ticket Class")) + labs(x = "Airline Status", y = "Count", fill="Satisfaction") +
    scale_fill_manual(values = AnkylosaurusPal()(10))    
  )
  
  # Shopping
  sd <- data[data$Shopping.Amount.at.Airport <250, ]
  g <- ggplot(sd, aes(Shopping.Amount.at.Airport))
  print(g + geom_bar(binwidth=25,aes(fill = factor(code.Satisfaction))) + theme(legend.position = "top") +
    ggtitle(paste(title, "by Shopping $$$ $0 < amount < $250")) + labs(x = "Shopping $$$", y = "Count", fill="Satisfaction") +
    scale_fill_manual(values = AnkylosaurusPal()(10))    
  )
  
  # Eating/Drinking
  sd <- data[data$Eating.and.Drinking.at.Airport <250, ]
  g <- ggplot(sd, aes(Eating.and.Drinking.at.Airport)) 
  print(g + geom_bar(binwidth=25,aes(fill = factor(code.Satisfaction))) + theme(legend.position = "top") +
    ggtitle(paste(title, "by Eating/Drinking $$$ $0 < amount < $250")) + labs(x = "Eating/Drinking $$$", y = "Count", fill="Satisfaction") +
    scale_fill_manual(values = AnkylosaurusPal()(10))    
  )
  
  # price Sensitivity by gender
  g <- ggplot(data, aes(factor(Price.Sensitivity)))
  print(g + geom_bar(aes(fill = Gender)) + 
    theme(legend.position = "top") +
    ggtitle(paste(title, "by Price Sensitivity")) + 
    labs(x = "Price Sensitivity", y = "Count", fill="Gender") +
    scale_fill_manual(values = AnkylosaurusPal('binary')(2))    
  )
  
  # price Sensitivity by satisfaction
  g <- ggplot(data, aes(factor(Price.Sensitivity)))
  print(g + geom_bar(aes(fill = factor(code.Satisfaction))) + 
          theme(legend.position = "top") +
          ggtitle(paste(title, "by Price Sensitivity")) + 
          labs(x = "Price Sensitivity", y = "Count", fill="Satisfaction") +
          scale_fill_manual(values = AnkylosaurusPal()(10))    
  )
  
  #travel type  
  g <- ggplot(data, aes(Type.of.Travel))
  print(g + geom_bar(aes(fill = factor(code.Satisfaction))) + theme(legend.position = "top") +
    ggtitle(paste(title, "by Travel Type")) + labs(x = "Airline Status", y = "Count", fill="Satisfaction") +
    scale_fill_manual(values = AnkylosaurusPal()(10))    
  )
  
  #flights per year  
  g <- ggplot(data, aes(No.of.Flights.p.a.))
  print(g + geom_bar(aes(fill = factor(code.Satisfaction))) + theme(legend.position = "top") +
    ggtitle(paste(title, "by flights per year")) + labs(x = "Flights", y = "Count", fill="Satisfaction") +
    scale_fill_manual(values = AnkylosaurusPal()(10))    
  )
  
  # number of other loyalty cards
  g <- ggplot(data, aes(No..of.other.Loyalty.Cards))
  print(g + geom_bar(aes(fill = factor(code.Satisfaction))) + theme(legend.position = "top") +
    ggtitle(paste(title, "by Loyalty Cards")) + labs(x = "Flights", y = "Count", fill="Satisfaction") +
    scale_fill_manual(values = AnkylosaurusPal()(10))    
  )
  
  # numberdeparture delay in minutes
  sd <- dataSet[data$Departure.Delay.in.Minutes > 0, ]
  sd <- sd[is.na(sd$Departure.Delay.in.Minutes) == FALSE, ]
  sd <- sd[sd$Departure.Delay.in.Minutes < (mean(sd$Departure.Delay.in.Minutes, na.rm=TRUE) + (6 * sd(sd$Departure.Delay.in.Minutes, na.rm=TRUE))), ]
  g <- ggplot(sd, aes(Departure.Delay.in.Minutes))
  print(g + geom_bar(binwidth=15, aes(fill = factor(code.Satisfaction))) + theme(legend.position = "top") +
    ggtitle(paste(title, "by Departure Delay")) + labs(x = "Departure Delay", y = "Count", fill="Satisfaction") +
    scale_fill_manual(values = AnkylosaurusPal()(10))    
  )
  
  # by arrival delay
  sd <- dataSet[data$Arrival.Delay.in.Minutes > 0, ]
  sd <- sd[is.na(sd$Arrival.Delay.in.Minutes) == FALSE, ]
  sd <- sd[sd$Arrival.Delay.in.Minutes < (mean(sd$Arrival.Delay.in.Minutes, na.rm=TRUE) + (6 * sd(sd$Arrival.Delay.in.Minutes, na.rm=TRUE))), ]
  g <- ggplot(sd, aes(Arrival.Delay.in.Minutes))
  print(g + geom_bar(binwidth=15, aes(fill = factor(code.Satisfaction))) + theme(legend.position = "top") +
    ggtitle(paste(title, "by Arrival Delay")) + labs(x = "Arrival Delay", y = "Count", fill="Satisfaction") +
    scale_fill_manual(values = AnkylosaurusPal()(10))    
  )
  
  # By airlines
  g <- ggplot(data, aes(Airline.Name))
  print(g + geom_bar(aes(fill = factor(code.Satisfaction))) + theme(legend.position = "top") + coord_flip() +
    ggtitle(paste(title, "by Airline")) + 
    labs(x = "Satisfaction", y = "Count", fill="Satisfaction") +
    scale_fill_manual(values = AnkylosaurusPal('all')(14))    
  )
  
  # Flight.time.in.minutes
  sd <- data[data$Flight.time.in.minutes > 0, ]
  g <- ggplot(sd, aes(Flight.time.in.minutes))
  print(g + geom_bar(binwidth=20, aes(fill = factor(code.Satisfaction)))  + theme(legend.position = "top") +
    ggtitle(paste(title, "by Flight Time")) + labs(x = "Flight.time.in.minutes", y = "Count", fill="Satisfaction") +
    scale_fill_manual(values = AnkylosaurusPal()(10))  +
      scale_x_continuous(expand = c(0,0), breaks=waiver())     
  )
  
  # by years flying
  g <- ggplot(data, aes(code.yearsFlying))
  print(g + geom_bar(bins=10, aes(fill = factor(code.Satisfaction))) + theme(legend.position = "top") +
    ggtitle(paste(title, "by Years Flying")) + labs(x = "Years Flying", y = "Count", fill="Satisfaction") +
    scale_fill_manual(values = AnkylosaurusPal()(10)) +
    scale_x_continuous(expand = c(0,0), breaks=waiver())                        
  )
  
  #
  # BOXPLOTS
  #
  g <- ggplot(data, aes(Airline.Name))
  print(g + geom_boxplot(aes(y=Age)) + 
          coord_flip()  + 
          labs(x = "Airline", y = "Age") )
  print(g + geom_boxplot(aes(y=No.of.Flights.p.a.)) + 
          coord_flip()  + 
          labs(x = "Airline", y = "Flights/year"))
  print(g + geom_boxplot(aes(y=Shopping.Amount.at.Airport)) + 
          coord_flip()  + 
          labs(x = "Airline", y = "Shopping $$$"))
  print(g + geom_boxplot(aes(y=Eating.and.Drinking.at.Airport)) + 
          coord_flip()  + 
          labs(x = "Airline", y = "Eating/Drinking $$$"))
  print(g + geom_boxplot(aes(y=Departure.Delay.in.Minutes)) + 
          coord_flip()  + 
          labs(x = "Airline", y = "Departure delay in mins"))
  print(g + geom_boxplot(aes(y=Arrival.Delay.in.Minutes)) + 
          coord_flip()  + 
          labs(x = "Airline", y = "Arrival delay in mins"))
  print(g + geom_boxplot(aes(y=data$Flight.Distance)) + 
          coord_flip()  + 
          labs(x = "Airline", y = "Flight Distance"))
  print(g + geom_boxplot(aes(y=data$Flight.time.in.minutes)) + 
          coord_flip()  + 
          labs(x = "Airline", y = "Flight time in minutes"))
  
  g <- ggplot(data, aes(factor(code.Satisfaction)))
  print(g + geom_boxplot(aes(y=Age)) + 
            labs(x = "Satisfaction", y = "Age") )
  print(g + geom_boxplot(aes(y=No.of.Flights.p.a.)))
  print(g + geom_boxplot(aes(y=Shopping.Amount.at.Airport)) + 
          labs(x = "Satisfaction", y = "Shopping $$$"))
  print(g + geom_boxplot(aes(y=Eating.and.Drinking.at.Airport)) + 
          labs(x = "Satisfaction", y = "Eating/Drinking $$$"))
  print(g + geom_boxplot(aes(y=Departure.Delay.in.Minutes)) + 
          labs(x = "Satisfaction", y = "Departure delay in mins"))
  print(g + geom_boxplot(aes(y=Arrival.Delay.in.Minutes)) + 
          labs(x = "Satisfaction", y = "Arrival delay in mins"))
  print(g + geom_boxplot(aes(y=data$Flight.Distance)) + 
          labs(x = "Satisfaction", y = "Flight Distance"))
  print(g + geom_boxplot(aes(y=data$Flight.time.in.minutes)) + 
          labs(x = "Satisfaction", y = "Flight time in minutes"))
  
  print(ggplot(data, aes(x=code.Satisfaction, y=Flight.Distance, color=Gender)) + 
          geom_point()  + 
          labs(x = "Flight Distance", y = "Satisfaction"))
  
  
  print(ggplot(data, aes(x=code.Satisfaction, y=Flight.time.in.minutes, color=Gender)) + 
          geom_point()  + 
          labs(x = "Flight Time", y = "Satisfaction"))
}

RelationTests <- function(data, title)
{
  print(paste(title,"Relationship Tests"))
  print("================")
  
  # check for relation...
  ChiSquaredTest(data$Gender, data$code.Satisfaction)
  
  # check for relation...
  ChiSquaredTest(data$Airline.Status, data$code.Satisfaction)

  # check for relation...
  ChiSquaredTest(data$Class, data$code.Satisfaction)
  
  # check for relation...
  ChiSquaredTest(data$Type.of.Travel, data$code.Satisfaction)
  
  # start with satisfaction versus flight time...
  # need ideas on who to better understand this data...
  lm(data$Flight.time.in.minutes~data$code.Satisfaction)
  
  if (makePlots == TRUE)
    print(ggplot(data, aes(code.Satisfaction, Flight.time.in.minutes)) +
      geom_point(shape=1) +    # Use hollow circles
      geom_smooth(method=lm)   # Add linear regression line (by default includes 95% confidence region)
    )
  
  print(lm(data$Eating.and.Drinking.at.Airport~data$code.Satisfaction))
  
  if (makePlots == TRUE)
    print(ggplot(data, aes(code.Satisfaction, Eating.and.Drinking.at.Airport)) +
    geom_point(shape=1) +    # Use hollow circles
    geom_smooth(method=lm)   # Add linear regression line (by default includes 95% confidence region)
  )
  
  print(lm(data$No.of.Flights.p.a.~data$code.Satisfaction))
  
  if (makePlots == TRUE)
    print(ggplot(data, aes(code.Satisfaction, No.of.Flights.p.a.)) +
          geom_point(shape=1) +    # Use hollow circles
          geom_smooth(method=lm)   # Add linear regression line (by default includes 95% confidence region)
  )
  
  
  print(lm(data$Age~data$code.Satisfaction))
  
  if (makePlots == TRUE)
    print(ggplot(data, aes(code.Satisfaction, Age)) +
          geom_point(shape=1) +    # Use hollow circles
          geom_smooth(method=lm)   # Add linear regression line (by default includes 95% confidence region)
  )
}

if(makePlots == TRUE)
  PlotSet(southeast, "Southeast Airlines Co.")
RelationTests(southeast, "Southeast Airlines Co.")

if(makePlots == TRUE)
  PlotSet(competition, "Competition")
RelationTests(competition, "Competition")

if(makePlots == TRUE)
  PlotSet(unhappy, "Unhappy")
RelationTests(unhappy, "Unhappy")

if(makePlots == TRUE)
  PlotSet(happy, "Happy")
RelationTests(happy, "Happy")

if(makePlots == TRUE)
  PlotSet(dataSet, "All Data")
RelationTests(dataSet, "All data")

regressionData <- data.frame(code.Satisfaction = dataSet$code.Satisfaction,
                             code.EnjoyFlyingAirServices = dataSet$code.EnjoyFlyingAirServices,
                             code.FlyFastAirwaysInc = dataSet$code.FlyFastAirwaysInc,
                             code.FlyHereAirways = dataSet$code.FlyHereAirways,
                             code.FlyToSunAirlinesInc = dataSet$code.FlyToSunAirlinesInc,
                             code.GoingNorthAirlinesInc = dataSet$code.GoingNorthAirlinesInc,
                             code.WestAirwaysInc = dataSet$code.WestAirwaysInc,
                             code.OnlyJetsAirlinesInc = dataSet$code.OnlyJetsAirlinesInc,
                             code.NorthwestBusinessAirlinesInc = dataSet$code.NorthwestBusinessAirlinesInc,
                             code.OursinAirlinesInc = dataSet$code.OursinAirlinesInc,
                             code.PaulSmithAirlinesInc = dataSet$code.PaulSmithAirlinesInc,
                             code.SigmaAirlinesInc = dataSet$code.SigmaAirlinesInc,
                             code.CheapseatsAirlinesInc = dataSet$code.CheapseatsAirlinesInc,
                             code.SoutheastAirlinesCo = dataSet$code.SoutheastAirlinesCo,
                             code.CoolYoungAirlinesInc= dataSet$code.CoolYoungAirlinesInc,
                             Age = dataSet$Age,
                             code.female = dataSet$code.female,
                             code.BlueStatus = dataSet$code.BlueStatus,
                             code.SilverStatus = dataSet$code.SilverStatus,
                             code.GoldStatus = dataSet$code.GoldStatus,
                             code.EconomyClass = dataSet$code.EconomyClass,
                             code.EcoPLusClass = dataSet$code.EcoPLusClass,
                             code.BusinessClass = dataSet$code.BusinessClass,
                             code.MileageTravel = dataSet$code.MileageTravel,
                             code.PersonalTravel = dataSet$code.PersonalTravel,
                             code.Sensitivity.VeryLow = dataSet$code.Sensitivity.VeryLow,
                             code.Sensitivity.Low = dataSet$code.Sensitivity.Low,
                             code.Sensitivity.Medium = dataSet$code.Sensitivity.Medium,
                             code.Sensitivity.High = dataSet$code.Sensitivity.High,
                             code.Sensitivity.VeryHigh = dataSet$code.Sensitivity.VeryHigh,
                             code.Cancelled = dataSet$code.Cancelled,
                             code.arrivalDelay = dataSet$code.arrivalDelay,
                             code.yearsFlying = dataSet$code.yearsFlying,
                             No.of.Flights.p.a. = dataSet$No.of.Flights.p.a.,
                             No..of.other.Loyalty.Cards = dataSet$No..of.other.Loyalty.Cards,
                             X..of.Flight.with.other.Airlines = dataSet$X..of.Flight.with.other.Airlines,
                             Eating.and.Drinking.at.Airport = dataSet$Eating.and.Drinking.at.Airport,
                             Departure.Delay.in.Minutes = dataSet$Departure.Delay.in.Minutes,
                             Arrival.Delay.in.Minutes = dataSet$Arrival.Delay.in.Minutes,
                             Flight.time.in.minutes = dataSet$Flight.time.in.minutes,
                             Flight.Distance = dataSet$Flight.Distance
                             )
summary(regressionData)

# correleation in the regression dataset....
corr <- round(cor(regressionData), 1)

# Plot
if(makePlots == TRUE)
  print(ggcorrplot(corr, hc.order = TRUE, 
                   type = "lower", 
                   lab = TRUE, 
                   lab_size = 3, 
                   method="circle", 
                   colors = c("tomato2", "white", "springgreen3"), 
                   title="Correlogram of Airline Data Set", 
                   ggtheme=theme_bw))

## regression on all the data
fit <- lm(data=regressionData, 
          formula=code.Satisfaction ~ Age +
            code.EnjoyFlyingAirServices + code.FlyFastAirwaysInc + code.FlyHereAirways + 
            code.FlyToSunAirlinesInc + code.GoingNorthAirlinesInc + 
            code.WestAirwaysInc + code.OnlyJetsAirlinesInc + code.NorthwestBusinessAirlinesInc +
            code.OursinAirlinesInc + code.PaulSmithAirlinesInc + code.SigmaAirlinesInc +
            code.CheapseatsAirlinesInc + code.SoutheastAirlinesCo +   
           code.female + 
           code.BlueStatus + code.SilverStatus + code.GoldStatus + 
           code.EconomyClass  + code.EcoPLusClass + code.BusinessClass +
           code.MileageTravel + code.PersonalTravel +
           code.Sensitivity.VeryHigh + code.Sensitivity.High + code.Sensitivity.Medium + code.Sensitivity.Low +
           code.Cancelled +
           code.arrivalDelay +
           code.yearsFlying +
           No.of.Flights.p.a. +
           No..of.other.Loyalty.Cards +
           X..of.Flight.with.other.Airlines +
           Eating.and.Drinking.at.Airport +
           Departure.Delay.in.Minutes +
           Arrival.Delay.in.Minutes +
           Flight.time.in.minutes +
           Flight.Distance
    )
summary(fit)    
coefficients(fit)

step(fit, data=dataSet, direction="backward")

# take final regfression builkd its own data frame and run the corellogram....
#Step:  AIC=144197.5
#code.Satisfaction ~ Age + code.GoingNorthAirlinesInc + code.WestAirwaysInc + 
#  code.female + code.BlueStatus + code.SilverStatus + code.GoldStatus + 
#  code.EconomyClass + code.EcoPLusClass + code.MileageTravel + 
#  code.PersonalTravel + code.Sensitivity.VeryHigh + code.Sensitivity.High + 
#  code.Sensitivity.Medium + code.Cancelled + code.arrivalDelay + 
#  code.yearsFlying + No.of.Flights.p.a. + No..of.other.Loyalty.Cards + 
#  Departure.Delay.in.Minutes
fit <- lm(formula = code.Satisfaction ~ Age + code.GoingNorthAirlinesInc + 
            code.WestAirwaysInc + code.female + code.BlueStatus + code.SilverStatus + 
            code.GoldStatus + code.EconomyClass + code.EcoPLusClass + 
            code.MileageTravel + code.PersonalTravel + code.Sensitivity.VeryHigh + 
            code.Sensitivity.High + code.Sensitivity.Medium + code.Cancelled + 
            code.arrivalDelay + code.yearsFlying + No.of.Flights.p.a. + 
            No..of.other.Loyalty.Cards + Departure.Delay.in.Minutes, 
          data = regressionData)
summary(fit)    
coefficients(fit)
sqrt(mean(fit$residuals)^2) #


finalSet <- data.frame(satisfaction = regressionData$code.Satisfaction,
                       age = regressionData$Age,
                       GoingNorthAirlinesInc = regressionData$code.GoingNorthAirlinesInc,
                       WestAirwaysInc = regressionData$code.WestAirwaysInc,
                       female = regressionData$code.female,
                       blueStatus = regressionData$code.BlueStatus,
                       silverStatus = regressionData$code.SilverStatus,
                       goldStatus = regressionData$code.GoldStatus,
                       economy = regressionData$code.EconomyClass,
                       ecoPlus = regressionData$code.EcoPLusClass,
                       milageTravel = regressionData$code.MileageTravel,
                       personalTravel = regressionData$code.PersonalTravel,
                       sensativity.VeryHigh = regressionData$code.Sensitivity.VeryHigh,
                       sensativity.High = regressionData$code.Sensitivity.High,
                       sensativity.Medium = regressionData$code.Sensitivity.Medium,
                       cancelled = regressionData$code.Cancelled,
                       arrivalDelay = regressionData$code.arrivalDelay,
                       yearsFlying = regressionData$code.yearsFlying,
                       flightPerYear = regressionData$No.of.Flights.p.a.,
                       otherLoyaltyCards = regressionData$No..of.other.Loyalty.Cards,
                       departureDelayInMinutes = regressionData$Departure.Delay.in.Minutes
                       )
summary(finalSet)


# correleation in the regression dataset....
corr <- round(cor(finalSet), 1)

# Plot
if(makePlots == TRUE)
  print(ggcorrplot(corr, hc.order = TRUE, 
                 type = "lower", 
                 lab = TRUE, 
                 lab_size = 3, 
                 method="circle", 
                 colors = c("tomato2", "white", "springgreen3"), 
                 title="Correlogram of Airline Data Set", 
                 ggtheme=theme_bw))



if(makePDF == TRUE)
  dev.off()

#make a factor only data set
factorSet <- data.frame(status = dataSet$factor.status,
                        airlineName = dataSet$factor.airlineName,
                        gender = dataSet$factor.gender,
                        arrivalDelay = dataSet$factor.arrivalDelay,
                        cancelled = dataSet$factor.Cancelled,
                        class = dataSet$factor.class,
                        satisfaction = dataSet$factor.Satisfaction,
                        travelType = dataSet$factor.travelType
                        )
summary(factorSet)


nbModel=naiveBayes(satisfaction ~ ., data=factorSet)
nbModel

#Prediction on the dataset
nbPredictions <- predict(nbModel, factorSet)
summary(nbPredictions)

count <- nrow(factorSet)

accuracyPct <- function(table, row, column)
{
  rs <- sum(table[row, ])
  
  if (row == column)
    pct = table[row,column] / rs
  else
    pct = -table[row,column] / rs
  
  return(as.integer(pct * 10000) / 100);
}

results <- table(nbPredictions,factorSet$satisfaction)

#report accuracy by percentage, positive pct is accurate perdiction, negative is inaccurate
accuracy <- results
accuracy
for (i in 1:nrow(results))
{
  for (j in 1:nrow(results))
    accuracy[i,j] = accuracyPct(results, i, j)
}
accuracy


# try with macxhine learning...
trainData = sample.rows(factorSet, size=((nrow(factorSet) * 2) / 3), replace=FALSE)
summary(trainData)

#Create a classification task for learning on  Dataset and specify the target feature
task <- makeClassifTask(data = trainData, target = "satisfaction")

#Initialize the Naive Bayes classifier
selected_model = makeLearner("classif.naiveBayes")

#Train the model
NB_mlr = train(selected_model, task)

#Read the model learned  
NB_mlr$learner.model

trainRow <- as.integer(rownames(trainData))
drops <- c("satisfaction")
actualSatisfaction <- factorSet[ -trainRow, c("satisfaction")]
predictDataNoTarget <- factorSet[ -trainRow, !(names(factorSet) %in% drops)]
summary(predictDataNoTarget)

#Predict on the dataset without passing the target feature
predictions_mlr = as.data.frame(predict(NB_mlr, newdata = predictDataNoTarget))

##Confusion matrix to check accuracy
results <- table(predictions_mlr[,1],actualSatisfaction)
accuracy <- results

for (i in 1:nrow(results))
{
  for (j in 1:ncol(results))
    accuracy[i,j] = accuracyPct(results, i, j)
}

#report accuracy by percentage, positive pct is accurate perdiction, negative is inaccurate
accuracy

#try training on full set and predicting full set....
#Create a classification task for learning on  Dataset and specify the target feature
task <- makeClassifTask(data = factorSet, target = "satisfaction")

#Initialize the Naive Bayes classifier
selected_model = makeLearner("classif.naiveBayes")

#Train the model
NB_mlr = train(selected_model, task)

#Read the model learned  
NB_mlr$learner.model

drops <- c("satisfaction")
actualSatisfaction <- factorSet[ , c("satisfaction")]
predictDataNoTarget <- factorSet[ , !(names(factorSet) %in% drops)]

summary(predictDataNoTarget)

#Predict on the dataset without passing the target feature
predictions_mlr = as.data.frame(predict(NB_mlr, newdata = predictDataNoTarget))

##Confusion matrix to check accuracy
results <- table(predictions_mlr[,1],actualSatisfaction)
accuracy <- results

for (i in 1:nrow(results))
{
  for (j in 1:ncol(results))
    accuracy[i,j] = accuracyPct(results, i, j)
}

#report accuracy by percentage, positive pct is accurate perdiction, negative is inaccurate
accuracy

boolScale <- function(x)
{
  return(ifelse(x[] == 0, -1.0, 1.0));
}

rangeScale <- function(x)
{
  mn <- as.single(min(x))
  mx <- as.single(max(x))
  rn <- (mx - mn) + 1
  
  return(((2 * as.single(x[])) - mx - mn) / rn);
}

continuousScale <- function(x)
{
  std <- sd(as.single(x))
  mn <- mean(as.single(x))

  return((as.single(x) - mn) / std);
}

#try SVM
svmData <- data.frame(code.Satisfaction = rangeScale(dataSet$code.Satisfaction),
                             code.EnjoyFlyingAirServices = boolScale(dataSet$code.EnjoyFlyingAirServices),
                             code.FlyFastAirwaysInc = boolScale(dataSet$code.FlyFastAirwaysInc),
                             code.FlyHereAirways = boolScale(dataSet$code.FlyHereAirways),
                             code.FlyToSunAirlinesInc = boolScale(dataSet$code.FlyToSunAirlinesInc),
                             code.GoingNorthAirlinesInc = boolScale(dataSet$code.GoingNorthAirlinesInc),
                             code.WestAirwaysInc = boolScale(dataSet$code.WestAirwaysInc),
                             code.OnlyJetsAirlinesInc = boolScale(dataSet$code.OnlyJetsAirlinesInc),
                             code.NorthwestBusinessAirlinesInc = boolScale(dataSet$code.NorthwestBusinessAirlinesInc),
                             code.OursinAirlinesInc = boolScale(dataSet$code.OursinAirlinesInc),
                             code.PaulSmithAirlinesInc = boolScale(dataSet$code.PaulSmithAirlinesInc),
                             code.SigmaAirlinesInc = boolScale(dataSet$code.SigmaAirlinesInc),
                             code.CheapseatsAirlinesInc = boolScale(dataSet$code.CheapseatsAirlinesInc),
                             code.SoutheastAirlinesCo = boolScale(dataSet$code.SoutheastAirlinesCo),
                             code.CoolYoungAirlinesInc= boolScale(dataSet$code.CoolYoungAirlinesInc),
                             Age = continuousScale(dataSet$Age),
                             code.female = boolScale(dataSet$code.female),
                             code.BlueStatus = boolScale(dataSet$code.BlueStatus),
                             code.SilverStatus = boolScale(dataSet$code.SilverStatus),
                             code.GoldStatus = boolScale(dataSet$code.GoldStatus),
                             code.EconomyClass = boolScale(dataSet$code.EconomyClass),
                             code.EcoPLusClass = boolScale(dataSet$code.EcoPLusClass),
                             code.BusinessClass = boolScale(dataSet$code.BusinessClass),
                             code.MileageTravel = boolScale(dataSet$code.MileageTravel),
                             code.PersonalTravel = boolScale(dataSet$code.PersonalTravel),
                             code.Sensitivity.VeryLow = boolScale(dataSet$code.Sensitivity.VeryLow),
                             code.Sensitivity.Low = boolScale(dataSet$code.Sensitivity.Low),
                             code.Sensitivity.Medium = boolScale(dataSet$code.Sensitivity.Medium),
                             code.Sensitivity.High = boolScale(dataSet$code.Sensitivity.High),
                             code.Sensitivity.VeryHigh = boolScale(dataSet$code.Sensitivity.VeryHigh),
                             code.Cancelled = boolScale(dataSet$code.Cancelled),
                             code.arrivalDelay = boolScale(dataSet$code.arrivalDelay),
                             code.yearsFlying = continuousScale(dataSet$code.yearsFlying),
                             No.of.Flights.p.a. = rangeScale(dataSet$No.of.Flights.p.a.),
                             No..of.other.Loyalty.Cards = rangeScale(dataSet$No..of.other.Loyalty.Cards),
                             X..of.Flight.with.other.Airlines = rangeScale(dataSet$X..of.Flight.with.other.Airlines),
                             Eating.and.Drinking.at.Airport = continuousScale(dataSet$Eating.and.Drinking.at.Airport),
                             Departure.Delay.in.Minutes = continuousScale(dataSet$Departure.Delay.in.Minutes),
                             Arrival.Delay.in.Minutes = continuousScale(dataSet$Arrival.Delay.in.Minutes),
                             Flight.time.in.minutes = continuousScale(dataSet$Flight.time.in.minutes),
                             Flight.Distance = continuousScale(dataSet$Flight.Distance)
                      )
summary(svmData)

# select traingin data
# try with macxhine learning...
trainData = sample.rows(svmData, size=5000, replace=FALSE)
summary(trainData)

trainRow <- as.integer(rownames(trainData))

drops <- c("satisfaction")

predictData <- svmData[ -trainRow, ]
summary(predictData)


#breakout data
x <- subset(trainData, select=-code.Satisfaction)
y <- trainData$code.Satisfaction

#create model and summerize
svm_model <- svm(x, y)
summary(svm_model)

system.time(pred <- predict(svm_model,subset(predictData, select=-code.Satisfaction)))

comp <- data.frame(actual = predictData$code.Satisfaction,  prediction = pred, diff=predictData$code.Satisfaction - pred)
summary(comp)
comp


dataSet$ï..Satisfaction <- as.numeric(dataSet$ï..Satisfaction)
dataSet$Sat <- ifelse(dataSet$ï..Satisfaction <=4,0,1)
rando <- sample(1:10000)
cutPoint2_3 <- floor(2 * 10000[1]/3)
DataTrain <- dataSet[rando[1:cutPoint2_3],]
DataTest <- dataSet[rando[(cutPoint2_3+1):10000],]
library(kernlab)
dim(DataTrain)
dim(DataTest)
svmNewTrain2 <- ksvm(Sat ~Airline.Status + Age + Gender +Price.Sensitivity
                     +No.of.Flights.p.a. +Type.of.Travel +Class
                     +Departure.Delay.in.Minutes +Arrival.Delay.in.Minutes 
                     + Flight.cancelled + Flight.Distance +Airline.Name,
                     #try to predict binary satisfaction
                     data= DataTrain , #specify that the data to use in the anlysis
                     kernel= "rbfdot", #kernel function that projects low dimension
                     kpar="automatic", #kpar refer to parameters that can be used
                     C= 10, #C refers to "Cost of Constrains"
                     cross = 15, #use 1- fold cross validation in this model
                     prob.model = TRUE ,
                     type ='C-svc'#use probability model in this model
)
?ksvm
predNewTest2 <- predict(svmNewTrain2, DataTest, type="votes")
####testData2 <- DataTest[-3191:-3334,]
dim(DataTest)
dim(predNewTest2)
length(predNewTest2)
colnames(dataSet)
length(predNewTest2)
dim(testData2)
colnames(testData2)
compTable2 <- data.frame(DataTest[,82], predNewTest2[1,])
colnames(compTable2) <- c("test", "Pred")
length(predNewTest2)
dim(testData2)
compTable2$correct <-   ifelse(compTable2$test==compTable2$Pred, "correct", "wrong")

PredictionRate <-length(which(compTable2$correct == "correct"))/3190

#nbpredFrame$correct <- ifelse(nbpredFrame$nbpred==nbpredFrame$testData1.goodOzone, "correct","wrong")


#mean(dataSet$code.yearsFlying)
PredictionRate
summary(svmNewTrain2)
str(svmNewTrain2)
svmNewTrain2
