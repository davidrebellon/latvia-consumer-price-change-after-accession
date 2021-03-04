########################################################################################################
#
#
##################################### David Rebellon ###################################################
#
#
# # # # # # # # # # Change in Consumer Prices in Latvia from Joining the EU # # # # # # # # # # # # # #
#
#
#
########################################################################################################



rm(list = ls(all = TRUE)) # This command gives you a clean clear R Session to work in

# Will be looking at monthly CPI data from Latvia
## Sources: Central Statistical Bureau of Latvia, World Bank, OECD


## PACKAGES ###
# install.packages("reshape2")
library(reshape2)



########################################################################################################




### Monthly CPI data is from 1996 - 2015. All data sets were subsetted to match time frame 1996-2015 ###

# Monthly CPI 1996 - 2015
latviaMonthlyCPI <- read.table(file= 'Data/LatviaTotalCPIMonthly.csv', header = TRUE , sep = ';', na.strings = c("..."))

head(latviaMonthlyCPI)
summary(latviaMonthlyCPI)

latviaMonthlyCPI9615 <- latviaMonthlyCPI[1:240,]
head(latviaMonthlyCPI9615)
tail(latviaMonthlyCPI9615)
### csb.gov.lv


# # GDP per Capita PPP Annual from 1995 to 2016
# annualGDPperCapitaPPP <- read.table(file='data/GDPperCapitaPPP.csv', skip = 4 , header = TRUE , sep = ",")
# 
# head(annualGDPperCapitaPPP)
# 
# summary(annualGDPperCapitaPPP)
# 
# annualGDPperCapPPPLatvia <- subset(annualGDPperCapitaPPP, Country.Name == "Latvia")
# head(annualGDPperCapPPPLatvia)
# 
# annualGDPpCapPPPLatvia1995 <- subset(annualGDPperCapPPPLatvia, select = c(1:4, 40:61))
# head(annualGDPpCapPPPLatvia1995)
# ### World Bank


# Personal Income Tax Rate Annual 1996 - 2015
incomeTaxAnnual <- read.table(file="data/IncomeTaxRateLatvia_Annual.csv", header = TRUE, sep=',')
head(incomeTaxAnnual)
summary(incomeTaxAnnual)

incomeTaxAnnual9615 <- subset(incomeTaxAnnual, TIME < 2016 & TIME > 1995)
summary(incomeTaxAnnual9615)
### data.OECD.org


# Adult Pop with Tertiary Education % Annual 1996 - 2015
eduLvlAnnual <- read.table(file="data/Latvia_EducationLevel_Tertiary_Annual.csv", header = TRUE, sep = ',')
head(eduLvlAnnual)
summary(eduLvlAnnual)

eduLvlAnnual9615 <- subset(eduLvlAnnual, TIME < 2016)
summary(eduLvlAnnual9615)
### data.OECD.org


# GDP Growth Quarterly  ### 1995Q2 - 2017Q4 ###
gdpGrowthQ <- read.table(file="data/Latvia_GDP_Growth_Q.csv", header = TRUE, sep = ',')
head(gdpGrowthQ)
summary(gdpGrowthQ)

gdpGrowthQ9615 <- gdpGrowthQ[4:83,]
head(gdpGrowthQ9615)
tail(gdpGrowthQ9615)
### data.OECD.org



# # Unemployment Quarterly 2002Q1 - 2017Q4
# unemployQ <- read.table(file="data/Latvia_Unemployment_Q.csv", header = TRUE, sep = ',')
# head(unemployQ)
# summary(unemployQ)
# unemployQ$COUNTRY <- unemployQ$?..LOCATION
# head(unemployQ)
# 
# unemployLatQ <- subset(unemployQ, COUNTRY == 'LVA')
# head(unemployLatQ)
# ### data.OECD.org

# Unemployment Annual 1995 - 2015

unemployAnnual <- read.table(file="data/UnemploymentRate_Annual.csv", skip = 4 , header = TRUE , sep = ",")
head(unemployAnnual)
summary(unemployAnnual)

unemploymentLatviaAnnual9615 <- subset(unemployAnnual, Country.Name == "Latvia", select = c(1:3, 42:61))
head(unemploymentLatviaAnnual9615)
### World Bank Data

 
# # Long Term Interest Rates Quarterly 2001Q1 - 2017Q4
# interestLTQ <- read.table(file="data/LongTermInterestRateLatvia_Q.csv", header = TRUE, sep = ',')
# head(interestLTQ)
# interestLTLatQ <- subset(interestLTQ, ?..LOCATION == 'LVA')
# head(interestLTLatQ)
# summary(interestLTLatQ)
# ### data.OECD.org
# 
# 
# # Short Term Interest Rates Quarterly 
# interestSTQ <- read.table(file="data/ShortTermInterestRateLatvia_Q.csv", header = TRUE, sep = ',')
# head(interestSTQ)  
# interestSTLatQ <- subset(interestSTQ, ?..LOCATION == 'LVA')
# head(interestSTLatQ)
# summary(interestSTLatQ)
# ### data.OECD.org


####################################################################################################
#
## Latvia dates to notice:
###  ** Apply to EU   : 27 Oct 1995  
###  ** Negotiations - 
####         - Stage1 : 03 Apr 1998 
####         - Stage2 : Mar-May 1999
###  *  Join the EU   : 01 May 2004 = 2004M05
###  *  Join eurozone : 01 Jan 2014 = 2014M01
###  *  Join Schengen : 21 Dec 2007 = 2007M12 & 2008M01
#
#### *  == From europa.eu
#### ** == From mfa.gov.lv
#
####################################################################################################



#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #



####################################################################################################


####################################################################################################

# Need to make non-quarterly data quarterly.
### Annual data objects : eduLvlAnnual9615 , incomeTaxAnnual9615 , unemploymentLatviaAnnual9615
### Monthly data objects: latviaMonthlyCPI9615

####################################################################################################

# First I will create a dataFrame with only Year, Quarter and Year-Quarter.
#   This will be used with all the yearly data conversions.


yearQuarter <- data.frame( TIME=rep( c(1996:2015), 4 ), QUARTER=c( rep("Q1",2016-1996), rep("Q2",2016-1996), rep("Q3",2016-1996), rep("Q4",2016-1996))  )


#### Education Level #####
  #   Merge the Data.
  #   NOTE: The yearly average is used as the quaterly value for all quarters in a given year.
  eduLvlAnnual9615 <- merge(x = yearQuarter, y = eduLvlAnnual9615, by = "TIME")

  # Rename TIME to YEAR
  names(eduLvlAnnual9615)[1] <- "YEAR" 

  # Rename TIME to YEAR
  names(eduLvlAnnual9615)[8] <- "EDUCATION" 
  
  # Create a TIME variable that combines YEAR and QUARTER
  # eduLvlAnnual9615$TIME <- paste(eduLvlAnnual9615$YEAR,"-",eduLvlAnnual9615$QUARTER, sep="")
  
  # Drop all uneeded variables
  eduLvlAnnual9615 <- subset(x = eduLvlAnnual9615, select = c("YEAR", "QUARTER", "EDUCATION") )
  

  
  
  
#### Income Tax #####  
  #   Merge the Data
  #   NOTE: The yearly average is used as the quaterly value for all quarters in a given year.
  incomeTaxAnnual9615 <- merge(x = yearQuarter, y = incomeTaxAnnual9615, by = "TIME")
  
  # Rename TIME to YEAR
  names(incomeTaxAnnual9615)[1] <- "YEAR"
  
  # Rename Value to 
  names(incomeTaxAnnual9615)[8] <- "TAX"
  
  # Drop all uneeded variables
  incomeTaxAnnual9615 <- subset(x = incomeTaxAnnual9615, select = c("YEAR", "QUARTER", "TAX") )
  
  
#### Unemployment ##### 
  # Drop Everything but the colums with unemplyment data
  unemploymentLatviaAnnual9615 <- unemploymentLatviaAnnual9615[4:ncol(unemploymentLatviaAnnual9615)]
  
  # Make the current variable names a row
  unemploymentLatviaAnnual9615 <- rbind(unemploymentLatviaAnnual9615, names(unemploymentLatviaAnnual9615))
  
  # Delete the current variable names
  names(unemploymentLatviaAnnual9615) <- NULL
  
  # Delete all the X's that are in front of the years.
  unemploymentLatviaAnnual9615[2, ] <- gsub(pattern = "X", replacement = "", x = unemploymentLatviaAnnual9615[2, ])
  
  # Transpose the dataframe
  unemploymentLatviaAnnual9615 <- data.frame( t(unemploymentLatviaAnnual9615) )
  
  # Rename the variables
  names(unemploymentLatviaAnnual9615) <- c("UNEMPLOYMENT", "TIME")
  
  
  #   Merge the Data
  #   NOTE: The yearly average is used as the quaterly value for all quarters in a given year.
  unemploymentLatviaAnnual9615 <- merge(x = yearQuarter, y = unemploymentLatviaAnnual9615, by = "TIME")
  
  # Rename TIME to YEAR
  names(unemploymentLatviaAnnual9615)[1] <- "YEAR"
  
  
### PRICE LEVEL ###
  # Drop all the un-needed variables
  latviaMonthlyCPI9615 <- subset(x = latviaMonthlyCPI9615, select = c("ï..Year..Month", "X2000.100", "X2005.100") )

  # Rename the Variables
  names(latviaMonthlyCPI9615) <- c("YEARMONTH", "CPI2000", "CPI2005")
  
  # Split YEARMONTH into YEAR and MONTH
  latviaMonthlyCPI9615 <- cbind(latviaMonthlyCPI9615, colsplit(string = latviaMonthlyCPI9615$YEARMONTH, pattern = "M",names=c("YEAR","MONTH") ) )
  
  # Asign Quarters based on month.
  latviaMonthlyCPI9615$QUARTER <- NA
  latviaMonthlyCPI9615$QUARTER <- ifelse(latviaMonthlyCPI9615$MONTH < 4, "Q1", latviaMonthlyCPI9615$QUARTER)
  latviaMonthlyCPI9615$QUARTER <- ifelse(latviaMonthlyCPI9615$MONTH > 3 & latviaMonthlyCPI9615$MONTH < 7, "Q2", latviaMonthlyCPI9615$QUARTER)
  latviaMonthlyCPI9615$QUARTER <- ifelse(latviaMonthlyCPI9615$MONTH > 6 & latviaMonthlyCPI9615$MONTH < 10, "Q3", latviaMonthlyCPI9615$QUARTER)
  latviaMonthlyCPI9615$QUARTER <- ifelse(latviaMonthlyCPI9615$MONTH > 9, "Q4", latviaMonthlyCPI9615$QUARTER)
  
  # Create YEARQUARTER so the quarterly means can be found
  latviaMonthlyCPI9615$YEARQUARTER <- paste(latviaMonthlyCPI9615$YEAR,"-",latviaMonthlyCPI9615$QUARTER, sep = "")
  
  # Find the Quarterly Means
  latviaMonthlyCPI9615 <-  aggregate(latviaMonthlyCPI9615[ ,2:3], list(latviaMonthlyCPI9615$YEARQUARTER), mean)
  
  #Split YEARQUARTER back apart
  latviaMonthlyCPI9615 <- cbind(latviaMonthlyCPI9615, colsplit(string = latviaMonthlyCPI9615$Group.1, pattern = "-",names=c("YEAR","QUARTER") ) )
  
  # Drop the unneeded Group.1 varaible
  latviaMonthlyCPI9615 <- latviaMonthlyCPI9615[ ,2:5]
  

  
  
####################################################################################################

# Need to merge data
# Objects: eduLvlAnnual9615 , gdpGrowthQ9615 , incomeTaxAnnual9615 , latviaMonthlyCPI9615 , 
#          unemploymentLatviaAnnual9615

####################################################################################################

# Drop unneeded variables in the GDP data  
  gdpGrowthQ9615 <- subset(x = gdpGrowthQ9615, select = c("TIME", "Value"))
  
# Split TIME into YEAR and QUARTER  
  gdpGrowthQ9615 <- cbind(gdpGrowthQ9615, colsplit(string = gdpGrowthQ9615$TIME, pattern = "-",names=c("YEAR","QUARTER") ) )
    
# Drop TIME
  gdpGrowthQ9615 <- gdpGrowthQ9615[ ,2:4]
  
# Rename "Value" to GDPGROWTH  
  names(gdpGrowthQ9615)[1] <- "GDPGROWTH"

  
  
##############################
# MERGE ALL THE DATA FRAMES #  
############################
  
finalDataFrame <- merge(x = eduLvlAnnual9615, y = incomeTaxAnnual9615, by = c("YEAR", "QUARTER") )  
  
  finalDataFrame <- merge(x = finalDataFrame, y = unemploymentLatviaAnnual9615, by = c("YEAR", "QUARTER") )  
  
    finalDataFrame <- merge(x = finalDataFrame, y = latviaMonthlyCPI9615, by = c("YEAR", "QUARTER") )  
    
      finalDataFrame <- merge(x = finalDataFrame, y = gdpGrowthQ9615, by = c("YEAR", "QUARTER") )  
  
      
      
      ########################################
      # STORE THE CLEAN DATA AS A CSV FILE #  
      ########################################
  
      write.csv(x = finalDataFrame, file = "data/cleanLatviaData")

      
      ################################################
      # Read the Data Back in to Make Sure it Works #  
      ##############################################

    checkTheDataFrame <- read.table(file = "data/cleanLatviaData", header = TRUE, sep = ",")      
    
    summary(checkTheDataFrame)
    
    
    #################################
    # ADD DUMMY VARIABLES FOR YEARS #
    ##################################
    
    checkTheDataFrame$STAGE1 <- ifelse(checkTheDataFrame$X >= 10 & checkTheDataFrame$X <= 13, 1 , 0)
    
    checkTheDataFrame$STAGE2 <- ifelse(checkTheDataFrame$X >= 14 & checkTheDataFrame$X <= 33, 1 , 0)
    
    checkTheDataFrame$EU     <- ifelse(checkTheDataFrame$X >= 34, 1 , 0)    
    
    checkTheDataFrame$EUROZONE <- ifelse(checkTheDataFrame$X >= 73, 1 , 0)    
    
    checkTheDataFrame$SCHENGEN <- ifelse(checkTheDataFrame$X >= 49, 1 , 0)    
    
    summary(checkTheDataFrame)    
    
    checkTheDataFrame$STAGE1 <- factor(checkTheDataFrame$STAGE1)    
    checkTheDataFrame$STAGE2 <- factor(checkTheDataFrame$STAGE2) 
    checkTheDataFrame$EU <- factor(checkTheDataFrame$EU) 
    checkTheDataFrame$EUROZONE <- factor(checkTheDataFrame$EUROZONE) 
    checkTheDataFrame$SCHENGEN <- factor(checkTheDataFrame$SCHENGEN) 
    
    summary(checkTheDataFrame)    
    
    checkTheDataFrame <- subset(checkTheDataFrame, select = c("YEAR", "QUARTER" , "EDUCATION","TAX","UNEMPLOYMENT","CPI2000","CPI2005","GDPGROWTH","STAGE1","STAGE2","EU","EUROZONE","SCHENGEN"))
    summary(checkTheDataFrame)
    
    #################################
    # STORE CLEAN DATA WITH DUMMIES #
    #################################
    
    write.csv(x = checkTheDataFrame, file = "data/cleanLatviaData")

    cleanData <- read.table(file = "data/cleanLatviaData", header = TRUE, sep = ",")  

    summary(cleanData)    
    