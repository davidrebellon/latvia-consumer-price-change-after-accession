#########################################################################################################
# # DAVID REBELLON # #                                                                                  #
#                                                                                                       #
#                                                                                                       #
# # # # # # # # # # # ~~~ Latvia Joining the EU and Consumer Price Changes ~~~~ # # # # # # # # # # # # #
#                                                                                                       #
#     Analysis                                                                                          #
#                                                                                                       #
#########################################################################################################




rm(list = ls(all = TRUE)) # This command gives you a clean clear R Session to work in

################
# Read in Data #
################

latviaData <- read.table(file= 'data/cleanLatviaData', header = TRUE , sep = ',')

head(latviaData)
summary(latviaData)

# need to make STAGE1 , STAGE2 , EU, EUROZONE , and SCHENGEN factors

latviaData$STAGE1 <- factor(latviaData$STAGE1)
latviaData$STAGE2 <- factor(latviaData$STAGE2)
latviaData$EU <- factor(latviaData$EU)
latviaData$EUROZONE <- factor(latviaData$EUROZONE)
latviaData$SCHENGEN <- factor(latviaData$SCHENGEN)

summary(latviaData)



### ANALYSIS ###

    #  EU on CPI 2005  #

plot(latviaData$EU, latviaData$CPI2005)

olsEU2005 <- lm(formula = latviaData$CPI2005 ~ latviaData$EU)
summary(olsEU2005)

abline(olsEU2005)

olsEU2000 <- lm(formula = latviaData$CPI2000 ~ latviaData$EU)
summary(olsEU2000)
    ### EU variable is statistically significant at the .0001 level
    ### There is less standard error on CPI 2005 data, will use that as base for rest of analysis




### control for just year ###

olsControlYear <- lm(formula = latviaData$CPI2005 ~ latviaData$EU + latviaData$YEAR)

summary(olsControlYear)

    ### remains statistically significant to the 99th degree but decrease from *** to *

### control for variables ###

olsControlled <- lm(formula = latviaData$CPI2005 ~ latviaData$EU + latviaData$YEAR + latviaData$EDUCATION + latviaData$TAX + latviaData$UNEMPLOYMENT + latviaData$GDPGROWTH + latviaData$EUROZONE + latviaData$STAGE1 + latviaData$STAGE2 + latviaData$SCHENGEN)

summary(olsControlled)

    ### EU no longer statistically significant
    ### maybe there's high collinearity?
  
## will need this package to run VIF coefficient test
# install.packages(AER)
library(AER)

vif(olsControlled)    

# VIF for EU == 20.178 , YEAR == 76.389 , EDUCATION == 65.484 , SCHENGEN == 17.343 
# all others relatively low


auxRegSchengen <- lm(formula = latviaData$EU ~ latviaData$SCHENGEN)
      # doesn't allow factors
latviaNoFactors <- read.table(file= 'data/cleanLatviaData', header = TRUE , sep = ',')

auxRegSchengen <- lm(formula = latviaNoFactors$EU ~ latviaNoFactors$SCHENGEN)
summary(auxRegSchengen)

plot(jitter(latviaNoFactors$EU) , jitter(latviaNoFactors$SCHENGEN))

    ### almost all dots that are 0 in EU are 0 in SCHENGEN and same for 1

auxRegEducation <- lm(formula = latviaData$EDUCATION ~ latviaData$EU)
plot(latviaData$EU, latviaData$EDUCATION)
summary(auxRegEducation)
    ### large increase in education after joining the EU

######
# due to high VIF on education will run ols without EDUCATION
######
olsControlledNoEdu <- lm(formula = latviaData$CPI2005 ~ latviaData$EU + latviaData$YEAR  + latviaData$TAX + latviaData$UNEMPLOYMENT + latviaData$GDPGROWTH + latviaData$EUROZONE + latviaData$STAGE1 + latviaData$STAGE2 + latviaData$SCHENGEN)

summary(olsControlledNoEdu)
    ### results did not change by much so will include EDUCATION to avoid bias
 


 ######################
 # Control for all variables but also control for year as a factor
 ######################
 
 latviaYearAsFactor <- latviaData

 latviaYearAsFactor$YEAR <- factor(latviaYearAsFactor$YEAR) 

 olsYearAsFactorControlAll <- lm( formula = latviaYearAsFactor$CPI2005 ~ latviaYearAsFactor$EU + latviaYearAsFactor$YEAR + latviaYearAsFactor$EDUCATION + latviaYearAsFactor$TAX + latviaYearAsFactor$UNEMPLOYMENT + latviaYearAsFactor$GDPGROWTH + latviaYearAsFactor$STAGE1 + latviaYearAsFactor$STAGE2 + latviaYearAsFactor$EUROZONE + latviaYearAsFactor$SCHENGEN) 

 summary(olsYearAsFactorControlAll) 
 
 ## EU not statistically significant
 
 
 ############
 # Testing only Year as a facotr
 
 olsJustYearFactor <- lm(formula = latviaYearAsFactor$CPI2005 ~ latviaYearAsFactor$EU + latviaYearAsFactor$YEAR)
summary(olsJustYearFactor) 

  ## EU not statistically significant



# 
# ##########
# # run as log-linear
# 
# olsLogLinearFactor <- lm( formula = log(latviaYearAsFactor$CPI2005) ~ latviaYearAsFactor$EU + latviaYearAsFactor$YEAR + latviaYearAsFactor$EDUCATION + latviaYearAsFactor$TAX + latviaYearAsFactor$UNEMPLOYMENT + latviaYearAsFactor$GDPGROWTH + latviaYearAsFactor$STAGE1 + latviaYearAsFactor$STAGE2 + latviaYearAsFactor$EUROZONE + latviaYearAsFactor$SCHENGEN)
# 
# summary(olsLogLinearFactor)
# plot(latviaYearAsFactor$EU, latviaYearAsFactor$CPI2005)
# abline(olsLogLinearFactor[1],olsLogLinearFactor[2])
# 
#     ### not statistically significant but drastically reduced SE ###
# 
# ###########
# # run as log-linear without years as factors
# 
# olsLogLinear <- lm(formula = log(latviaData$CPI2005) ~ latviaData$EU + latviaData$YEAR + latviaData$EDUCATION + latviaData$TAX + latviaData$UNEMPLOYMENT + latviaData$GDPGROWTH + latviaData$EUROZONE + latviaData$STAGE1 + latviaData$STAGE2 + latviaData$SCHENGEN)
# summary(olsLogLinear)
# 
# plot(latviaData$EU , latviaData$CPI2005)
#  abline(olsLogLinear) 
#     ### statistically significant again at the .001 level.
#  



 ######################
 # Conlcusion
 ######################

# After controlling for all variables, EU estimate lost significance.
# Will use the olsYearAsFactorControlAll as final regression



###########################################################
#
# plots for paper
#
###########################################################

plot(latviaData$YEAR , latviaData$CPI2005)

plot(latviaData$YEAR , latviaData$UNEMPLOYMENT)

plot(latviaData$YEAR , latviaData$TAX)

plot(latviaData$YEAR , latviaData$GDPGROWTH)

plot(latviaData$YEAR , latviaData$EDUCATION)

plot(latviaData$EU , latviaData$CPI2005)
abline(olsYearAsFactorControlAll)
summary(olsControlled)

vif(olsYearAsFactorControlAll)

write.csv(latviaData, file = "data/latviaDataTable.csv")
