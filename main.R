################################################################################
#
#       Title:  A example script for Zero Coupon Curve Calculation
#       Autor:  Heudes Emmanuel Ochoa Parra
#       Date: 26/04/2016
#       Desc: A example about how tu use the package "termstrc" to perform
#       a estimation of the zero coupon curve, given a bond data set.
#       The models available to adjust the data are: 
#         - "ns": Nelson Siegel 
#         - "sv": Nelson Siegel-Svenson
#         - "asv": Nelson Siegel - Adjusted Svenson
#
#       Data Sets:
#         - MotherTable.csv: A table containing all the information needed
#           from de bonds to study.
#         - YTM.csv: Data from the "Yield to Maturity" curve.
#         - CF.csv: Cash Flow
#         - CFYTM.csv: Cash Flow used in the YTM.
#       
#       Instructions to run:
#         - Put all the data sets file in the working directory with the
#           "functionszc.R" inside.
#         - Install the needed packages
#         - Execute the code
#
################################################################################

#Install Needed packages

# install.packages("termstrc")   
# install.packages("devtools")       
# install.packages("knitr")
# install.packages("R.utils")

# Sourcing funtions

source('functionszc.R')

# Loading data:

bonds <- bond.data("MotherTable.csv","CF.csv")
ytm <- ytm.data("YTM.csv","CFYTM.csv")

# Fitting models:

fitted.models <- zc.curve(Bonds = bonds)