#Install Package
library(ggplot2)

Data <- read.csv("~/Desktop/Taiwan/3rd Semester/GLM/Canada_Fuel_Consumption_Rating.csv")
#removing "NA" data
Data <- na.omit(Data)


#Extract Variable
Engine <- Data$ENGINE_SIZE
Cylinders <- Data$CYLINDERS
Fuel <- Data$FUEL_TYPE
City <- Data$CITY
HWY  <- Data$HWY
COMB <- Data$COMB
COMB_mpg   <- Data$COMB_mpg
CO2_Emission   <- Data$CO2_EMISSIONS

#replace Outlier
func <- function(x){
  quantiles <- quantile( x, c(.05, .95 ) )
  b <- boxplot(x, plot = FALSE)
  x[ x %in% b$out]  <- quantiles[2]
  x
}

City <- func(City)
CO2_Emission <- func(CO2_Emission)
COMB <- func(COMB)
Cylinders <- func(Cylinders)
COMB_mpg <- func(COMB_mpg)
HWY <- func(HWY)

#plot
plot(CO2_Emission~Cylinders)
lines(lowess(CO2_Emission,Cylinders), col="blue")
library(car)
scatterplot(CO2_Emission ~ Cylinders, data = Data)

#Regression
library(rsm)
Surface_response <- rsm(CO2_Emission ~ SO(Engine, Cylinders), data = Data)
summary(Surface_response)

a <- lm(CO2_Emission~ Engine + Cylinders + Engine:Cylinders, data = Data); 
summary(a)
b <- lm(CO2_Emission~ Engine + Cylinders + I(Cylinders^2) + Engine:Cylinders, data = Data)
summary(b)


sse <- sum( (Data$CO2_EMISSIONS - fitted(Surface_response))^2)
sse

#find ssr
ssr <- sum((fitted(Surface_response) - mean(Data$CO2_EMISSIONS))^2)
ssr

# find SST
sst <- sse + ssr
sst

