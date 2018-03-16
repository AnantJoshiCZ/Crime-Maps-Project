library(ggplot2)
library(dplyr)
library(magrittr)
library(ggmap)
library(stringr)
library(sp)
library(googleVis)
library(googleway)
library(plotGoogleMaps)
library(readxl)
#library(xlsx)
#library(XLConnectJars)
#library(XLConnect)
 ######################################### MAKE HOVERVAR

##################################################################################################################
########################################## Importing the database into R
########################################## Preprocessing .xlsx files to .csv for easier importing 


#WorldSexViolenceRate <- read_excel("C:/User/Anant Joshi/Documents/PublicationReports2017_latest.xlsx", sheet = 1, col_names = TRUE)
WorldAssaultRate <- read.csv("C:/Users/Anant Joshi/Documents/CrimeData/Assault.csv", header = TRUE, stringsAsFactors = FALSE)
WorldTheftRate <- read.csv("C:/Users/Anant Joshi/Documents/CrimeData/Theft.csv", header = TRUE, stringsAsFactors = FALSE)
WorldBurglaryRate <- read.csv("C:/Users/Anant Joshi/Documents/CrimeData/Burglary.csv", header = TRUE, stringsAsFactors = FALSE)
WorldDomBurglaryRate <- read.csv("C:/Users/Anant Joshi/Documents/CrimeData/DomBurglary.csv", header = TRUE, stringsAsFactors = FALSE)
WorldKidnapRate <- read.csv("C:/Users/Anant Joshi/Documents/CrimeData/kidnapping.csv", header = TRUE, stringsAsFactors = FALSE)
WorldSexViolenceRate <- read.csv("C:/Users/Anant Joshi/Documents/CrimeData/TotalSexualCrime.csv", header = TRUE, stringsAsFactors = FALSE)
WorldVehicleTheftRate <- read.csv("C:/Users/Anant Joshi/Documents/CrimeData/VehicleTheft.csv", header = TRUE, stringsAsFactors = FALSE)
WorldRobberyRate <- read.csv("C:/Users/Anant Joshi/Documents/CrimeData/robbery.csv", header = TRUE, stringsAsFactors = FALSE)
WorldHomicideRate <- read.csv("C:/Users/Anant Joshi/Documents/CrimeData/Homicide.csv", header = TRUE, stringsAsFactors = FALSE)

##################################################################################################################
########################################## Removing extra data (Data Cleaning)

WorldAssaultRate$Region <- NULL
WorldAssaultRate$Sub.region <- NULL

WorldTheftRate$Region <- NULL
WorldTheftRate$Sub.region <- NULL

WorldBurglaryRate$Region <- NULL
WorldBurglaryRate$Sub.region <- NULL

WorldDomBurglaryRate$Region <- NULL
WorldDomBurglaryRate$Sub.region <- NULL

WorldKidnapRate$Region <- NULL
WorldKidnapRate$Sub.region <- NULL

WorldSexViolenceRate$Region <- NULL
WorldSexViolenceRate$Sub.region <- NULL

WorldVehicleTheftRate$Region <- NULL
WorldVehicleTheftRate$Sub.region <- NULL

WorldRobberyRate$Region <- NULL
WorldRobberyRate$Sub.region <- NULL

WorldHomicideRate$Region <- NULL
WorldHomicideRate$Sub.region <- NULL

##################################################################################################
################################### Data Manipulation
################################### Some countries in the dataset did not have proper recognizable names. The GoogleVis package which 
################################### is used in this project does not recognize the names and hence they must be transformed into something 
################################### more recognizable.


WorldAssaultRate$Country.territory <- sub("Kosovo under UNSCR 1244", "Kosovo", WorldAssaultRate$Country.territory)
WorldAssaultRate$Country.territory <- sub("Macao Special Administrative Region of China", "Macao", WorldAssaultRate$Country.territory)
WorldAssaultRate$Country.territory <- sub("State of Palestine", "Palestine", WorldAssaultRate$Country.territory)
#WorldAssaultRate$Country.territory <- sub("United Kingdom", "", WorldAssaultRate$Country.territory)
WorldAssaultRate$Country.territory <- sub("(England and Wales)", "", WorldAssaultRate$Country.territory)
WorldAssaultRate$Country.territory <- sub("(Northern Ireland)", "Northern Ireland", WorldAssaultRate$Country.territory)
WorldAssaultRate$Country.territory <- sub("(Scotland)", "Scotland", WorldAssaultRate$Country.territory)
WorldAssaultRate$Country.territory <- sub("\\(", "", WorldAssaultRate$Country.territory)
WorldAssaultRate$Country.territory <- sub(")", "", WorldAssaultRate$Country.territory)
WorldAssaultRate$Country.territory <- sub("Hong Kong Special Administrative Region of China", "Hong Kong", WorldAssaultRate$Country.territory)
WorldAssaultRate$Country.territory <- sub("Bolivia Plurinational State of", "Bolivia", WorldAssaultRate$Country.territory)



WorldTheftRate$Country.territory <- sub("Kosovo under UNSCR 1244", "Kosovo", WorldTheftRate$Country.territory)
WorldTheftRate$Country.territory <- sub("Macao Special Administrative Region of China", "Macao", WorldTheftRate$Country.territory)
WorldTheftRate$Country.territory <- sub("State of Palestine", "Palestine", WorldTheftRate$Country.territory)
#WorldTheftRate$Country.territory <- sub("United Kingdom", "", WorldTheftRate$Country.territory)
WorldTheftRate$Country.territory <- sub("(England and Wales)", "", WorldTheftRate$Country.territory)
WorldTheftRate$Country.territory <- sub("(Northern Ireland)", "Northern Ireland", WorldTheftRate$Country.territory)
WorldTheftRate$Country.territory <- sub("(Scotland)", "Scotland", WorldTheftRate$Country.territory)
WorldTheftRate$Country.territory <- sub("\\(", "", WorldTheftRate$Country.territory)
WorldTheftRate$Country.territory <- sub(")", "", WorldTheftRate$Country.territory)
WorldTheftRate$Country.territory <- sub("Hong Kong Special Administrative Region of China", "Hong Kong", WorldTheftRate$Country.territory)
WorldTheftRate$Country.territory <- sub("Bolivia Plurinational State of", "Bolivia", WorldTheftRate$Country.territory)


WorldBurglaryRate$Country.territory <- sub("Kosovo under UNSCR 1244", "Kosovo", WorldBurglaryRate$Country.territory)
WorldBurglaryRate$Country.territory <- sub("Macao Special Administrative Region of China", "Macao", WorldBurglaryRate$Country.territory)
WorldBurglaryRate$Country.territory <- sub("State of Palestine", "Palestine", WorldBurglaryRate$Country.territory)
#WorldBurglaryRate$Country.territory <- sub("United Kingdom", "", WorldBurglaryRate$Country.territory)
WorldBurglaryRate$Country.territory <- sub("(England and Wales)", "", WorldBurglaryRate$Country.territory)
WorldBurglaryRate$Country.territory <- sub("(Northern Ireland)", "Northern Ireland", WorldBurglaryRate$Country.territory)
WorldBurglaryRate$Country.territory <- sub("(Scotland)", "Scotland", WorldBurglaryRate$Country.territory)
WorldBurglaryRate$Country.territory <- sub("\\(", "", WorldBurglaryRate$Country.territory)
WorldBurglaryRate$Country.territory <- sub(")", "", WorldBurglaryRate$Country.territory)
WorldBurglaryRate$Country.territory <- sub("Hong Kong Special Administrative Region of China", "Hong Kong", WorldBurglaryRate$Country.territory)
WorldBurglaryRate$Country.territory <- sub("Bolivia Plurinational State of", "Bolivia", WorldBurglaryRate$Country.territory)


WorldDomBurglaryRate$Country.territory <- sub("Kosovo under UNSCR 1244", "Kosovo", WorldDomBurglaryRate$Country.territory)
WorldDomBurglaryRate$Country.territory <- sub("Macao Special Administrative Region of China", "Macao", WorldDomBurglaryRate$Country.territory)
WorldDomBurglaryRate$Country.territory <- sub("State of Palestine", "Palestine", WorldDomBurglaryRate$Country.territory)
#WorldDomBurglaryRate$Country.territory <- sub("United Kingdom", "", WorldDomBurglaryRate$Country.territory)
WorldDomBurglaryRate$Country.territory <- sub("(England and Wales)", "", WorldDomBurglaryRate$Country.territory)
WorldDomBurglaryRate$Country.territory <- sub("(Northern Ireland)", "Northern Ireland", WorldDomBurglaryRate$Country.territory)
WorldDomBurglaryRate$Country.territory <- sub("(Scotland)", "Scotland", WorldDomBurglaryRate$Country.territory)
WorldDomBurglaryRate$Country.territory <- sub("\\(", "", WorldDomBurglaryRate$Country.territory)
WorldDomBurglaryRate$Country.territory <- sub(")", "", WorldDomBurglaryRate$Country.territory)
WorldDomBurglaryRate$Country.territory <- sub("Hong Kong Special Administrative Region of China", "Hong Kong", WorldDomBurglaryRate$Country.territory)
WorldDomBurglaryRate$Country.territory <- sub("Bolivia Plurinational State of", "Bolivia", WorldDomBurglaryRate$Country.territory)


WorldKidnapRate$Country.territory <- sub("Kosovo under UNSCR 1244", "Kosovo", WorldKidnapRate$Country.territory)
WorldKidnapRate$Country.territory <- sub("Macao Special Administrative Region of China", "Macao", WorldKidnapRate$Country.territory)
WorldKidnapRate$Country.territory <- sub("State of Palestine", "Palestine", WorldKidnapRate$Country.territory)
#WorldKidnapRate$Country.territory <- sub("United Kingdom", "", WorldKidnapRate$Country.territory)
WorldKidnapRate$Country.territory <- sub("(England and Wales)", "", WorldKidnapRate$Country.territory)
WorldKidnapRate$Country.territory <- sub("(Northern Ireland)", "Northern Ireland", WorldKidnapRate$Country.territory)
WorldKidnapRate$Country.territory <- sub("(Scotland)", "Scotland", WorldKidnapRate$Country.territory)
WorldKidnapRate$Country.territory <- sub("\\(", "", WorldKidnapRate$Country.territory)
WorldKidnapRate$Country.territory <- sub(")", "", WorldKidnapRate$Country.territory)
WorldKidnapRate$Country.territory <- sub("Hong Kong Special Administrative Region of China", "Hong Kong", WorldKidnapRate$Country.territory)
WorldKidnapRate$Country.territory <- sub("Bolivia Plurinational State of", "Bolivia", WorldKidnapRate$Country.territory)


WorldSexViolenceRate$Country.territory <- sub("Kosovo under UNSCR 1244", "Kosovo", WorldSexViolenceRate$Country.territory)
WorldSexViolenceRate$Country.territory <- sub("Macao Special Administrative Region of China", "Macao", WorldSexViolenceRate$Country.territory)
WorldSexViolenceRate$Country.territory <- sub("State of Palestine", "Palestine", WorldSexViolenceRate$Country.territory)
#WorldSexViolenceRate$Country.territory <- sub("United Kingdom", "", WorldSexViolenceRate$Country.territory)
WorldSexViolenceRate$Country.territory <- sub("(England and Wales)", "", WorldSexViolenceRate$Country.territory)
WorldSexViolenceRate$Country.territory <- sub("(Northern Ireland)", "Northern Ireland", WorldSexViolenceRate$Country.territory)
WorldSexViolenceRate$Country.territory <- sub("(Scotland)", "Scotland", WorldSexViolenceRate$Country.territory)
WorldSexViolenceRate$Country.territory <- sub("\\(", "", WorldSexViolenceRate$Country.territory)
WorldSexViolenceRate$Country.territory <- sub(")", "", WorldSexViolenceRate$Country.territory)
WorldSexViolenceRate$Country.territory <- sub("Hong Kong Special Administrative Region of China", "Hong Kong", WorldSexViolenceRate$Country.territory)
WorldSexViolenceRate$Country.territory <- sub("Bolivia Plurinational State of", "Bolivia", WorldSexViolenceRate$Country.territory)


WorldVehicleTheftRate$Country.territory <- sub("Kosovo under UNSCR 1244", "Kosovo", WorldVehicleTheftRate$Country.territory)
WorldVehicleTheftRate$Country.territory <- sub("Macao Special Administrative Region of China", "Macao", WorldVehicleTheftRate$Country.territory)
WorldVehicleTheftRate$Country.territory <- sub("State of Palestine", "Palestine", WorldVehicleTheftRate$Country.territory)
#WorldVehicleTheftRate$Country.territory <- sub("United Kingdom", "", WorldVehicleTheftRate$Country.territory)
WorldVehicleTheftRate$Country.territory <- sub("(England and Wales)", "", WorldVehicleTheftRate$Country.territory)
WorldVehicleTheftRate$Country.territory <- sub("(Northern Ireland)", "Northern Ireland", WorldVehicleTheftRate$Country.territory)
WorldVehicleTheftRate$Country.territory <- sub("(Scotland)", "Scotland", WorldVehicleTheftRate$Country.territory)
WorldVehicleTheftRate$Country.territory <- sub("\\(", "", WorldVehicleTheftRate$Country.territory)
WorldVehicleTheftRate$Country.territory <- sub(")", "", WorldVehicleTheftRate$Country.territory)
WorldVehicleTheftRate$Country.territory <- sub("Hong Kong Special Administrative Region of China", "Hong Kong", WorldVehicleTheftRate$Country.territory)
WorldVehicleTheftRate$Country.territory <- sub("Bolivia Plurinational State of", "Bolivia", WorldVehicleTheftRate$Country.territory)


WorldRobberyRate$Country.territory <- sub("Kosovo under UNSCR 1244", "Kosovo", WorldRobberyRate$Country.territory)
WorldRobberyRate$Country.territory <- sub("Macao Special Administrative Region of China", "Macao", WorldRobberyRate$Country.territory)
WorldRobberyRate$Country.territory <- sub("State of Palestine", "Palestine", WorldRobberyRate$Country.territory)
#WorldRobberyRate$Country.territory <- sub("United Kingdom", "", WorldRobberyRate$Country.territory)
WorldRobberyRate$Country.territory <- sub("(England and Wales)", "", WorldRobberyRate$Country.territory)
WorldRobberyRate$Country.territory <- sub("(Northern Ireland)", "Northern Ireland", WorldRobberyRate$Country.territory)
WorldRobberyRate$Country.territory <- sub("(Scotland)", "Scotland", WorldRobberyRate$Country.territory)
WorldRobberyRate$Country.territory <- sub("\\(", "", WorldRobberyRate$Country.territory)
WorldRobberyRate$Country.territory <- sub(")", "", WorldRobberyRate$Country.territory)
WorldRobberyRate$Country.territory <- sub("Hong Kong Special Administrative Region of China", "Hong Kong", WorldRobberyRate$Country.territory)
WorldRobberyRate$Country.territory <- sub("Bolivia Plurinational State of", "Bolivia", WorldRobberyRate$Country.territory)


WorldHomicideRate$Country.territory <- sub("Kosovo under UNSCR 1244", "Kosovo", WorldHomicideRate$Country.territory)
WorldHomicideRate$Country.territory <- sub("Macao Special Administrative Region of China", "Macao", WorldHomicideRate$Country.territory)
WorldHomicideRate$Country.territory <- sub("State of Palestine", "Palestine", WorldHomicideRate$Country.territory)
#WorldHomicideRate$Country.territory <- sub("United Kingdom", "", WorldHomicideRate$Country.territory)
WorldHomicideRate$Country.territory <- sub("(England and Wales)", "", WorldHomicideRate$Country.territory)
WorldHomicideRate$Country.territory <- sub("(Northern Ireland)", "Northern Ireland", WorldHomicideRate$Country.territory)
WorldHomicideRate$Country.territory <- sub("(Scotland)", "Scotland", WorldHomicideRate$Country.territory)
WorldHomicideRate$Country.territory <- sub("\\(", "", WorldHomicideRate$Country.territory)
WorldHomicideRate$Country.territory <- sub(")", "", WorldHomicideRate$Country.territory)
WorldHomicideRate$Country.territory <- sub("Hong Kong Special Administrative Region of China", "Hong Kong", WorldHomicideRate$Country.territory)
WorldHomicideRate$Country.territory <- sub("Bolivia Plurinational State of", "Bolivia", WorldHomicideRate$Country.territory)


string <- "Total No. of Crime Instances "

########################################################################################


register_google(key = 'AIzaSyBz9xahrp4DlrHAUwnmfFdjcUCPp34eKBs')



########################################################################################
########################################### Assault Crime Maps


#MapCountriesGvis <- na.omit(MapCountriesGvis)
MapCountriesGvis <- data.frame(WorldAssaultRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldAssaultRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Assault in 2008` <- WorldAssaultRate$X2008.1
string <- "Total Assault Crimes in 2008"
MapCountriesGvis$`Total Assault in 2008`<- WorldAssaultRate$X2008
MapCountriesGvis$`Total Assault in 2008` <-paste(string, MapCountriesGvis$`Total Assault in 2008`,  sep = "=" )
MapCountriesGvis$`Per Capita Assault in 2008` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Assault in 2008`))
AGeo2008 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Assault in 2008', 
                         hovervar = 'Total Assault in 2008',
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 1500],
                                      colors:[\'red', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(AGeo2008)
print(AGeo2008, file="AGeo2008.html")

MapCountriesGvis <- data.frame(WorldAssaultRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldAssaultRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Assault in 2009` <- WorldAssaultRate$X2009.1
string <- "Total Assault Crimes in 2009"
MapCountriesGvis$`Total Assault in 2009`<- WorldAssaultRate$X2009
MapCountriesGvis$`Total Assault in 2009` <-paste(string, MapCountriesGvis$`Total Assault in 2009`,  sep = "=" )
MapCountriesGvis$`Per Capita Assault in 2009` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Assault in 2009`))
AGeo2009 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Assault in 2009', 
                         hovervar = 'Total Assault in 2009',
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 1500],
                                      colors:[\'red', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(AGeo2009)
print(AGeo2009, file="AGeo2009.html")

MapCountriesGvis <- data.frame(WorldAssaultRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldAssaultRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Assault in 2010` <- WorldAssaultRate$X2010.1
string <- "Total Assault Crimes in 2010"
MapCountriesGvis$`Total Assault in 2010`<- WorldAssaultRate$X2010
MapCountriesGvis$`Total Assault in 2010` <-paste(string, MapCountriesGvis$`Total Assault in 2010`,  sep = "=" )
MapCountriesGvis$`Per Capita Assault in 2010` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Assault in 2010`))
AGeo2010 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Assault in 2010', 
                         hovervar = 'Total Assault in 2010',
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 1500],
                                      colors:[\'red', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(AGeo2010)
print(AGeo2010, file="AGeo2010.html")

MapCountriesGvis <- data.frame(WorldAssaultRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldAssaultRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Assault in 2011` <- WorldAssaultRate$X2011.1
string <- "Total Assault Crimes in 2011"
MapCountriesGvis$`Total Assault in 2011`<- WorldAssaultRate$X2011
MapCountriesGvis$`Total Assault in 2011` <-paste(string, MapCountriesGvis$`Total Assault in 2011`,  sep = "=" )
MapCountriesGvis$`Per Capita Assault in 2011` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Assault in 2011`))
AGeo2011 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Assault in 2011', 
                         hovervar = 'Total Assault in 2011',
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 1500],
                                      colors:[\'red', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(AGeo2011)
print(AGeo2011, file="AGeo2011.html")

MapCountriesGvis <- data.frame(WorldAssaultRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldAssaultRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Assault in 2012` <- WorldAssaultRate$X2012.1
string <- "Total Assault Crimes in 2012"
MapCountriesGvis$`Total Assault in 2012`<- WorldAssaultRate$X2012
MapCountriesGvis$`Total Assault in 2012` <-paste(string, MapCountriesGvis$`Total Assault in 2012`,  sep = "=" )
MapCountriesGvis$`Per Capita Assault in 2012` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Assault in 2012`))
AGeo2012 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Assault in 2012', 
                         hovervar = 'Total Assault in 2012',
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 1500],
                                      colors:[\'red', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(AGeo2012)
print(AGeo2012, file="AGeo2012.html")

MapCountriesGvis <- data.frame(WorldAssaultRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldAssaultRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Assault in 2013` <- WorldAssaultRate$X2013.1
string <- "Total Assault Crimes in 2013"
MapCountriesGvis$`Total Assault in 2013`<- WorldAssaultRate$X2013
MapCountriesGvis$`Total Assault in 2013` <-paste(string, MapCountriesGvis$`Total Assault in 2013`,  sep = "=" )
MapCountriesGvis$`Per Capita Assault in 2013` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Assault in 2013`))
AGeo2013 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Assault in 2013', 
                         hovervar = 'Total Assault in 2013',
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 1500],
                                      colors:[\'red', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(AGeo2013)
print(AGeo2013, file="AGeo2013.html")

MapCountriesGvis <- data.frame(WorldAssaultRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldAssaultRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Assault in 2014` <- WorldAssaultRate$X2014.1
string <- "Total Assault Crimes in 2014"
MapCountriesGvis$`Total Assault in 2014`<- WorldAssaultRate$X2014
MapCountriesGvis$`Total Assault in 2014` <-paste(string, MapCountriesGvis$`Total Assault in 2014`,  sep = "=" )
MapCountriesGvis$`Per Capita Assault in 2014` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Assault in 2014`))
AGeo2014 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Assault in 2014', 
                         hovervar = 'Total Assault in 2014',
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 1500],
                                      colors:[\'red', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(AGeo2014)
print(AGeo2014, file="AGeo2014.html")

MapCountriesGvis <- data.frame(WorldAssaultRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldAssaultRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Assault in 2015` <- WorldAssaultRate$X2015.1
string <- "Total Assault Crimes in 2015"
MapCountriesGvis$`Total Assault in 2015`<- WorldAssaultRate$X2015
MapCountriesGvis$`Total Assault in 2015` <-paste(string, MapCountriesGvis$`Total Assault in 2015`,  sep = "=" )
MapCountriesGvis$`Per Capita Assault in 2015` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Assault in 2015`))
AGeo2015 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Assault in 2015', 
                         hovervar = 'Total Assault in 2015',
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 1500],
                                      colors:[\'red', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(AGeo2015)
print(AGeo2015, file="AGeo2015.html")

####################################################################################################
################################################ Sexual Violence Crime Maps

MapCountriesGvis <- data.frame(WorldSexViolenceRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldSexViolenceRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita SexViolence in 2008` <- WorldSexViolenceRate$X2008.1
MapCountriesGvis$`Total SexViolence in 2008`<- WorldSexViolenceRate$X2008
string <- "Total SexViolence Crimes in 2008"
MapCountriesGvis$`Total SexViolence in 2008` <-paste(string, MapCountriesGvis$`Total SexViolence in 2008`,  sep = "=" )
MapCountriesGvis$`Per Capita SexViolence in 2008` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita SexViolence in 2008`))
SGeo2008 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita SexViolence in 2008', 
                         hovervar = 'Total SexViolence in 2008',
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 110],
                                      colors:[\'purple', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(SGeo2008)
print(SGeo2008, file="SGeo2008.html")

MapCountriesGvis <- data.frame(WorldSexViolenceRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldSexViolenceRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita SexViolence in 2009` <- WorldSexViolenceRate$X2009.1
MapCountriesGvis$`Total SexViolence in 2009`<- WorldSexViolenceRate$X2009
string <- "Total SexViolence Crimes in 2009"
MapCountriesGvis$`Total SexViolence in 2009` <-paste(string, MapCountriesGvis$`Total SexViolence in 2009`,  sep = "=" )
MapCountriesGvis$`Per Capita SexViolence in 2009` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita SexViolence in 2009`))
SGeo2009 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita SexViolence in 2009', 
                         hovervar = 'Total SexViolence in 2009',
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 110],
                                      colors:[\'purple', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(SGeo2009)
print(SGeo2009, file="SGeo2009.html")

MapCountriesGvis <- data.frame(WorldSexViolenceRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldSexViolenceRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita SexViolence in 2010` <- WorldSexViolenceRate$X2010.1
MapCountriesGvis$`Total SexViolence in 2010`<- WorldSexViolenceRate$X2010
string <- "Total SexViolence Crimes in 2010"
MapCountriesGvis$`Total SexViolence in 2010` <-paste(string, MapCountriesGvis$`Total SexViolence in 2010`,  sep = "=" )
MapCountriesGvis$`Per Capita SexViolence in 2010` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita SexViolence in 2010`))
SGeo2010 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita SexViolence in 2010', 
                         hovervar = 'Total SexViolence in 2010',
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 110],
                                      colors:[\'purple', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(SGeo2010)
print(SGeo2010, file="SGeo2010.html")

MapCountriesGvis <- data.frame(WorldSexViolenceRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldSexViolenceRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita SexViolence in 2011` <- WorldSexViolenceRate$X2011.1
MapCountriesGvis$`Total SexViolence in 2011`<- WorldSexViolenceRate$X2011
string <- "Total SexViolence Crimes in 2011"
MapCountriesGvis$`Total SexViolence in 2011` <-paste(string, MapCountriesGvis$`Total SexViolence in 2011`,  sep = "=" )
MapCountriesGvis$`Per Capita SexViolence in 2011` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita SexViolence in 2011`))
SGeo2011 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita SexViolence in 2011', 
                         hovervar = 'Total SexViolence in 2011',
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 110],
                                      colors:[\'purple', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(SGeo2011)
print(SGeo2011, file="SGeo2011.html")

MapCountriesGvis <- data.frame(WorldSexViolenceRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldSexViolenceRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita SexViolence in 2012` <- WorldSexViolenceRate$X2012.1
MapCountriesGvis$`Total SexViolence in 2012`<- WorldSexViolenceRate$X2012
string <- "Total SexViolence Crimes in 2012"
MapCountriesGvis$`Total SexViolence in 2012` <-paste(string, MapCountriesGvis$`Total SexViolence in 2012`,  sep = "=" )
MapCountriesGvis$`Per Capita SexViolence in 2012` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita SexViolence in 2012`))
SGeo2012 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita SexViolence in 2012', 
                         hovervar = 'Total SexViolence in 2012',
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 110],
                                      colors:[\'purple', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(SGeo2012)
print(SGeo2012, file="SGeo2012.html")

MapCountriesGvis <- data.frame(WorldSexViolenceRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldSexViolenceRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita SexViolence in 2013` <- WorldSexViolenceRate$X2013.1
MapCountriesGvis$`Total SexViolence in 2013`<- WorldSexViolenceRate$X2013
string <- "Total SexViolence Crimes in 2013"
MapCountriesGvis$`Total SexViolence in 2013` <-paste(string, MapCountriesGvis$`Total SexViolence in 2013`,  sep = "=" )
MapCountriesGvis$`Per Capita SexViolence in 2013` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita SexViolence in 2013`))
SGeo2013 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita SexViolence in 2013', 
                         hovervar = 'Total SexViolence in 2013',
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 110],
                                      colors:[\'purple', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(SGeo2013)
print(SGeo2013, file="SGeo2013.html")

MapCountriesGvis <- data.frame(WorldSexViolenceRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldSexViolenceRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita SexViolence in 2014` <- WorldSexViolenceRate$X2014.1
MapCountriesGvis$`Total SexViolence in 2014`<- WorldSexViolenceRate$X2014
string <- "Total SexViolence Crimes in 2014"
MapCountriesGvis$`Total SexViolence in 2014` <-paste(string, MapCountriesGvis$`Total SexViolence in 2014`,  sep = "=" )
MapCountriesGvis$`Per Capita SexViolence in 2014` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita SexViolence in 2014`))
SGeo2014 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita SexViolence in 2014', 
                         hovervar = 'Total SexViolence in 2014',
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 110],
                                      colors:[\'purple', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(SGeo2014)
print(SGeo2014, file="SGeo2014.html")

MapCountriesGvis <- data.frame(WorldSexViolenceRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldSexViolenceRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita SexViolence in 2015` <- WorldSexViolenceRate$X2015.1
MapCountriesGvis$`Total SexViolence in 2015`<- WorldSexViolenceRate$X2015
string <- "Total SexViolence Crimes in 2015"
MapCountriesGvis$`Total SexViolence in 2015` <-paste(string, MapCountriesGvis$`Total SexViolence in 2015`,  sep = "=" )
MapCountriesGvis$`Per Capita SexViolence in 2015` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita SexViolence in 2015`))
SGeo2015 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita SexViolence in 2015', 
                         hovervar = 'Total SexViolence in 2015',
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 110],
                                      colors:[\'purple', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(SGeo2015)
print(SGeo2015, file="SGeo2015.html")


#################################################################################################################
########################################################## Burglary Crime Maps

MapCountriesGvis <- data.frame(WorldBurglaryRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldBurglaryRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Burglary in 2008` <- WorldBurglaryRate$X2008.1
MapCountriesGvis$`Total Burglary in 2008`<- WorldBurglaryRate$X2008
string <- "Total Burglary Crimes in 2008"
MapCountriesGvis$`Total Burglary in 2008` <-paste(string, MapCountriesGvis$`Total Burglary in 2008`,  sep = "=" )
MapCountriesGvis$`Per Capita Burglary in 2008` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Burglary in 2008`))
BGeo2008 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Burglary in 2008',
                         hovervar = 'Total Burglary in 2008' ,
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 2000],
                                      colors:[\'green', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(BGeo2008)
print(BGeo2008, file="BGeo2008.html")

MapCountriesGvis <- data.frame(WorldBurglaryRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldBurglaryRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Burglary in 2009` <- WorldBurglaryRate$X2009.1
MapCountriesGvis$`Total Burglary in 2009`<- WorldBurglaryRate$X2009
string <- "Total Burglary Crimes in 2009"
MapCountriesGvis$`Total Burglary in 2009` <-paste(string, MapCountriesGvis$`Total Burglary in 2009`,  sep = "=" )
MapCountriesGvis$`Per Capita Burglary in 2009` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Burglary in 2009`))
BGeo2009 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Burglary in 2009',
                         hovervar = 'Total Burglary in 2009' ,
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 2000],
                                      colors:[\'green', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(BGeo2009)
print(BGeo2009, file="BGeo2009.html")

MapCountriesGvis <- data.frame(WorldBurglaryRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldBurglaryRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Burglary in 2010` <- WorldBurglaryRate$X2010.1
MapCountriesGvis$`Total Burglary in 2010`<- WorldBurglaryRate$X2010
string <- "Total Burglary Crimes in 2010"
MapCountriesGvis$`Total Burglary in 2010` <-paste(string, MapCountriesGvis$`Total Burglary in 2010`,  sep = "=" )
MapCountriesGvis$`Per Capita Burglary in 2010` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Burglary in 2010`))
BGeo2010 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Burglary in 2010',
                         hovervar = 'Total Burglary in 2010' ,
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 2000],
                                      colors:[\'green', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(BGeo2010)
print(BGeo2010, file="BGeo2010.html")

MapCountriesGvis <- data.frame(WorldBurglaryRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldBurglaryRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Burglary in 2011` <- WorldBurglaryRate$X2011.1
MapCountriesGvis$`Total Burglary in 2011`<- WorldBurglaryRate$X2011
string <- "Total Burglary Crimes in 2011"
MapCountriesGvis$`Total Burglary in 2011` <-paste(string, MapCountriesGvis$`Total Burglary in 2011`,  sep = "=" )
MapCountriesGvis$`Per Capita Burglary in 2011` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Burglary in 2011`))
BGeo2011 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Burglary in 2011',
                         hovervar = 'Total Burglary in 2011' ,
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 2000],
                                      colors:[\'green', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(BGeo2011)
print(BGeo2011, file="BGeo2011.html")

MapCountriesGvis <- data.frame(WorldBurglaryRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldBurglaryRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Burglary in 2012` <- WorldBurglaryRate$X2012.1
MapCountriesGvis$`Total Burglary in 2012`<- WorldBurglaryRate$X2012
string <- "Total Burglary Crimes in 2012"
MapCountriesGvis$`Total Burglary in 2012` <-paste(string, MapCountriesGvis$`Total Burglary in 2012`,  sep = "=" )
MapCountriesGvis$`Per Capita Burglary in 2012` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Burglary in 2012`))
BGeo2012 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Burglary in 2012',
                         hovervar = 'Total Burglary in 2012' ,
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 2000],
                                      colors:[\'green', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(BGeo2012)
print(BGeo2012, file="BGeo2012.html")


MapCountriesGvis <- data.frame(WorldBurglaryRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldBurglaryRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Burglary in 2013` <- WorldBurglaryRate$X2013.1
MapCountriesGvis$`Total Burglary in 2013`<- WorldBurglaryRate$X2013
string <- "Total Burglary Crimes in 2013"
MapCountriesGvis$`Total Burglary in 2013` <-paste(string, MapCountriesGvis$`Total Burglary in 2013`,  sep = "=" )
MapCountriesGvis$`Per Capita Burglary in 2013` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Burglary in 2013`))
BGeo2013 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Burglary in 2013',
                         hovervar = 'Total Burglary in 2013' ,
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 2000],
                                      colors:[\'green', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(BGeo2013)
print(BGeo2013, file="BGeo2013.html")


MapCountriesGvis <- data.frame(WorldBurglaryRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldBurglaryRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Burglary in 2014` <- WorldBurglaryRate$X2014.1
MapCountriesGvis$`Total Burglary in 2014`<- WorldBurglaryRate$X2014
string <- "Total Burglary Crimes in 2014"
MapCountriesGvis$`Total Burglary in 2014` <-paste(string, MapCountriesGvis$`Total Burglary in 2014`,  sep = "=" )
MapCountriesGvis$`Per Capita Burglary in 2014` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Burglary in 2014`))
BGeo2014 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Burglary in 2014',
                         hovervar = 'Total Burglary in 2014' ,
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 2000],
                                      colors:[\'green', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(BGeo2014)
print(BGeo2014, file="BGeo2014.html")

MapCountriesGvis <- data.frame(WorldBurglaryRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldBurglaryRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Burglary in 2015` <- WorldBurglaryRate$X2015.1
MapCountriesGvis$`Total Burglary in 2015`<- WorldBurglaryRate$X2015
string <- "Total Burglary Crimes in 2015"
MapCountriesGvis$`Total Burglary in 2015` <-paste(string, MapCountriesGvis$`Total Burglary in 2015`,  sep = "=" )
MapCountriesGvis$`Per Capita Burglary in 2015` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Burglary in 2015`))
BGeo2015 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Burglary in 2015',
                         hovervar = 'Total Burglary in 2015' ,
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 2000],
                                      colors:[\'green', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(BGeo2015)
print(BGeo2015, file="BGeo2015.html")


#################################################################################################################
########################################################## Kidnap crime maps

MapCountriesGvis <- data.frame(WorldKidnapRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldKidnapRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Kidnap in 2008` <- WorldKidnapRate$X2008.1
MapCountriesGvis$`Total Kidnap in 2008`<- WorldKidnapRate$X2008
string <- "Total Kidnap Crimes in 2008 "
MapCountriesGvis$`Total Kidnap in 2008` <-paste(string, MapCountriesGvis$`Total Kidnap in 2008`,  sep = "=" )
MapCountriesGvis$`Per Capita Kidnap in 2008` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Kidnap in 2008`))
KGeo2008 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Kidnap in 2008',
                         hovervar = 'Total Kidnap in 2008' ,
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 25],
                                      colors:[\'#000080', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(KGeo2008)
print(KGeo2008, file="KGeo2008.html")

MapCountriesGvis <- data.frame(WorldKidnapRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldKidnapRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Kidnap in 2009` <- WorldKidnapRate$X2009.1
MapCountriesGvis$`Total Kidnap in 2009`<- WorldKidnapRate$X2009
string <- "Total Kidnap Crimes in 2009 "
MapCountriesGvis$`Total Kidnap in 2009` <-paste(string, MapCountriesGvis$`Total Kidnap in 2009`,  sep = "=" )
MapCountriesGvis$`Per Capita Kidnap in 2009` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Kidnap in 2009`))
KGeo2009 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Kidnap in 2009',
                         hovervar = 'Total Kidnap in 2009' ,
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 25],
                                      colors:[\'#000080', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(KGeo2009)
print(KGeo2009, file="KGeo2009.html")

MapCountriesGvis <- data.frame(WorldKidnapRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldKidnapRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Kidnap in 2010` <- WorldKidnapRate$X2010.1
MapCountriesGvis$`Total Kidnap in 2010`<- WorldKidnapRate$X2010
string <- "Total Kidnap Crimes in 2010 "
MapCountriesGvis$`Total Kidnap in 2010` <-paste(string, MapCountriesGvis$`Total Kidnap in 2010`,  sep = "=" )
MapCountriesGvis$`Per Capita Kidnap in 2010` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Kidnap in 2010`))
KGeo2010 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Kidnap in 2010',
                         hovervar = 'Total Kidnap in 2010' ,
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 25],
                                      colors:[\'#000080', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(KGeo2010)
print(KGeo2010, file="KGeo2010.html")

MapCountriesGvis <- data.frame(WorldKidnapRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldKidnapRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Kidnap in 2011` <- WorldKidnapRate$X2011.1
MapCountriesGvis$`Total Kidnap in 2011`<- WorldKidnapRate$X2011
string <- "Total Kidnap Crimes in 2011 "
MapCountriesGvis$`Total Kidnap in 2011` <-paste(string, MapCountriesGvis$`Total Kidnap in 2011`,  sep = "=" )
MapCountriesGvis$`Per Capita Kidnap in 2011` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Kidnap in 2011`))
KGeo2011 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Kidnap in 2011',
                         hovervar = 'Total Kidnap in 2011' ,
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 25],
                                      colors:[\'#000080', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(KGeo2011)
print(KGeo2011, file="KGeo2011.html")

MapCountriesGvis <- data.frame(WorldKidnapRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldKidnapRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Kidnap in 2012` <- WorldKidnapRate$X2012.1
MapCountriesGvis$`Total Kidnap in 2012`<- WorldKidnapRate$X2012
string <- "Total Kidnap Crimes in 2012 "
MapCountriesGvis$`Total Kidnap in 2012` <-paste(string, MapCountriesGvis$`Total Kidnap in 2012`,  sep = "=" )
MapCountriesGvis$`Per Capita Kidnap in 2012` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Kidnap in 2012`))
KGeo2012 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Kidnap in 2012',
                         hovervar = 'Total Kidnap in 2012' ,
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 25],
                                      colors:[\'#000080', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(KGeo2012)
print(KGeo2012, file="KGeo2012.html")


MapCountriesGvis <- data.frame(WorldKidnapRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldKidnapRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Kidnap in 2013` <- WorldKidnapRate$X2013.1
MapCountriesGvis$`Total Kidnap in 2013`<- WorldKidnapRate$X2013
string <- "Total Kidnap Crimes in 2013 "
MapCountriesGvis$`Total Kidnap in 2013` <-paste(string, MapCountriesGvis$`Total Kidnap in 2013`,  sep = "=" )
MapCountriesGvis$`Per Capita Kidnap in 2013` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Kidnap in 2013`))
KGeo2013 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Kidnap in 2013',
                         hovervar = 'Total Kidnap in 2013' ,
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 25],
                                      colors:[\'#000080', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(KGeo2013)
print(KGeo2013, file="KGeo2013.html")



MapCountriesGvis <- data.frame(WorldKidnapRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldKidnapRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Kidnap in 2014` <- WorldKidnapRate$X2014.1
MapCountriesGvis$`Total Kidnap in 2014`<- WorldKidnapRate$X2014
string <- "Total Kidnap Crimes in 2014 "
MapCountriesGvis$`Total Kidnap in 2014` <-paste(string, MapCountriesGvis$`Total Kidnap in 2014`,  sep = "=" )
MapCountriesGvis$`Per Capita Kidnap in 2014` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Kidnap in 2014`))
KGeo2014 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Kidnap in 2014',
                         hovervar = 'Total Kidnap in 2014' ,
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 25],
                                      colors:[\'#000080', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(KGeo2014)
print(KGeo2014, file="KGeo2014.html")

MapCountriesGvis <- data.frame(WorldKidnapRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldKidnapRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Kidnap in 2015` <- WorldKidnapRate$X2015.1
MapCountriesGvis$`Total Kidnap in 2015`<- WorldKidnapRate$X2015
string <- "Total Kidnap Crimes in 2015 "
MapCountriesGvis$`Total Kidnap in 2015` <-paste(string, MapCountriesGvis$`Total Kidnap in 2015`,  sep = "=" )
MapCountriesGvis$`Per Capita Kidnap in 2015` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Kidnap in 2015`))
KGeo2015 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Kidnap in 2015',
                         hovervar = 'Total Kidnap in 2015' ,
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 25],
                                      colors:[\'#000080', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                         ))
plot(KGeo2015)
print(KGeo2015, file="KGeo2015.html")


#################################################################################################################
########################################### Homicide Crime Maps



#################################################################################################################
MapCountriesGvis <- data.frame(WorldBurglaryRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldBurglaryRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Burglary in 2010` <- WorldBurglaryRate$X2010.1
MapCountriesGvis$`Per Capita SexViolence in 2013` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita SexViolence in 2013`))
BGeo2010 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Burglary in 2010', 
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 1000],
                                      colors:[\'green', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                                      , enableRegionInteractivity = TRUE
                         ))
plot(BGeo2010)
print(BGeo2010, file="BGeo2010.html")

MapCountriesGvis <- data.frame(WorldKidnapRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldKidnapRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Kidnap in 2010` <- WorldKidnapRate$X2010.1
MapCountriesGvis$`Per Capita Kidnap in 2010` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Kidnap in 2010`))
KGeo2010 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Kidnap in 2010', 
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 20],
                                      colors:[\'blue', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                                      , enableRegionInteractivity = TRUE
                         ))
plot(KGeo2010)
print(KGeo2010, file="KGeo2010.html")

MapCountriesGvis <- data.frame(WorldHomicideRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldHomicideRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Homicide in 2010` <- WorldHomicideRate$X2010.1
MapCountriesGvis$`Per Capita Homicide in 2010` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Homicide in 2010`))
MapCountriesGvis <- na.omit(MapCountriesGvis)
HGeo2010 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Homicide in 2010', 
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 85],
                                      colors:[\'brown', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                                      , enableRegionInteractivity = TRUE
                         ))
plot(HGeo2010)
print(HGeo2010, file="HGeo2010.html")

MapCountriesGvis <- data.frame(WorldTheftRate$Country.territory)
colnames(MapCountriesGvis)[colnames(MapCountriesGvis) == 'WorldTheftRate.Country.territory'] <- 'Country'
MapCountriesGvis$`Per Capita Theft in 2010` <- WorldTheftRate$X2010.1
MapCountriesGvis$`Total Theft in 2010`<- WorldTheftRate$X2010
MapCountriesGvis$`Total Theft in 2010` <-paste(string, MapCountriesGvis$`Total Theft in 2010`,  sep = "=" )
MapCountriesGvis$`Per Capita Theft in 2010` <- as.numeric(gsub(",","",MapCountriesGvis$`Per Capita Theft in 2010`))
TGeo2010 <- gvisGeoChart(MapCountriesGvis, 
                         locationvar = "Country" , 
                         colorvar = 'Per Capita Theft in 2010', 
                         hovervar = 'Total Theft in 2010' ,
                         options=list(width='1280',
                                      height = '720',
                                      gvis.editor = TRUE,
                                      colorAxis="{values:[1, 5000],
                                      colors:[\'pink', \'black']}"
                                      , backgroundColor = "white"
                                      , datalessRegionColor = "lightgrey"
                                      , defaultColor = "lightgrey"
                                      , domain = 'IN'
                                      #                                 , region = 154
                                      , enableRegionInteractivity = TRUE
                         ))
plot(TGeo2010)
print(TGeo2010, file="TGeo2010.html")

########################################################################################

#for(i in 1:nrow(origAddress))
#{
# result <- geocode(MapCountries$[i], output = "latlona", source = "google")
# origAddress$lon[i] <- as.numeric(result[1])
# origAddress$lat[i] <- as.numeric(result[2])
# origAddress$geoAddress[i] <- as.character(result[3])
#}

########################################################################################



########################################################################################



  