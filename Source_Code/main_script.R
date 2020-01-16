##==============================================================================
##
## Script for automatic executaton of all other scripts
##
## Authors: Mahkameh Zarekarizi (mahkameh.zare@gmail.com) 
##          and Klaus Keller (klaus@psu.edu)
##==============================================================================
## Copyright 2019 Mahkameh Zarekarizi
## This file is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This file is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this file.  If not, see <http://www.gnu.org/licenses/>.
##==============================================================================
## Instructions to run:
## 1. As far as you are running this script within its containing folder,
##    you do not need to do anything. Just run the code
##    This script automatically reproduces all the data and figures in the paper 
##    Since geenrating 10,000 houses is coputationally time consuming, the default 
##    number of houses is reduced to 100. Also, for the same reason, the default 
##    number of strategies in reduced too. Therefore, running this script with 
##    the default arguments will not generate the exact similar figures as the 
##    paper. TO reproduce paper figures, please increase the number of strategies 
##    to 100 and increase the number of houses to 10,000.
##    To increase number of houses, change the first argument of S16 below and 
##    to increase the number of strategies, change the second argument in the 
##    same function.
## 2. To Run:
##      Click on source
## 3. Outputs:
##      1. Figures are saved in Figures directory under the main folder
##      2. Data are saved in the Results_RData folder under the main directory
##==============================================================================

rm(list=ls()) #Just in case, remove any variable that is already loaded 
graphics.off() #to make sure the user does not have an open figure
use_prepared_data=TRUE
if(use_prepared_data){
  load_path='Pre_Generated_Output_Data'
}else{
  load_path='Output_Data'
}

# Create the folders for storing output data and figures
tmp <- paste0(getwd(), "/Figures/")
if(dir.exists(tmp)==F){dir.create(tmp, recursive=T)}

tmp <- paste0(getwd(), "/Output_Data/")
if(dir.exists(tmp)==F){dir.create(tmp, recursive=T)}

tmp <- paste0(getwd(), "/Output_Data/House_case_objectives/")
if(dir.exists(tmp)==F){dir.create(tmp, recursive=T)}

tmp <- paste0(getwd(), "/Output_Data/sobol_outputs/")
if(dir.exists(tmp)==F){dir.create(tmp, recursive=T)}


'
# List of packages you will need to install (uncomment if you have not them installed)
# If on a server, you may need to try this: install.packages("<name of package>", repos="http://cran.r-project.org", lib="~/local/R_libs/")  
install.packages("lattice")
install.packages("Kendall")
install.packages("ismev")
install.packages("evdbayes")
install.packages("evir")
install.packages("evd")
install.packages("lhs")
install.packages("fields")
install.packages("plotrix")
install.packages("lhs")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("DEoptim")

install.packages("prim") # You may needed to go to a third party site and download a compatibility package for my Mac
# You do not need this package for the main program. You will need it for classification and a figure in the suplementary materials. 

install.packages("truncnorm")
install.packages("sdtoolkit")
install.packages("sensitivity")
install.packages("pracma")
'

# Start running the functions 
source('./Source_Code/S01_XLRM.R') ## This script generates the XLRM diagram 
print('Step1 DONE: XLRM diagram was saved')

source("./Source_Code/S02_Depth_Damage_Function.R") ## Script plots depth-damage function
print('Step2 DONE: Depth-Damage diagram was saved')

source("./Source_Code/S03_Construction_Cost.R") ## Script plots the costruction cost plot 
print('Step3 DONE: Construction cost plot was saved')

source("./Source_Code/S04_Streamflow_Raw_Data_Analysis.R") ## Script analyzes streamflow data
print('Step4 DONE: Streamflow data were analyzed')

source("./Source_Code/S05_Rating_Curve.R") ## Script for drawing the rating curve downloaded from USGS 
print('Step5 DONE: Rating curve plot was saved')

source("./Source_Code/S06_Convert_Streamflow_To_Gage_Height.R") ## Script for converting streamflow data to Gage Height 
print('Step6 DONE: Streamflow was converted to stage using the rating curve')

source("./Source_Code/S07_Gage_Height_Timeseries.R") ## Script plots the timseries of water levels.
print('Step7 DONE: Water level timeseries were plotted')

source("./Source_Code/S08_Gage_Height_Annual_Maximum.R")  ## Script analyzes the annual maximum water level
print('Step8 DONE: Annual maximum water level data were extracted, saved, and plotted')

source("./Source_Code/S09_Estimate_GEV_MLE.R") ## Script estimates the parameters of a stationary GEV 
print('Step9 DONE: Parameters of the GEV distribution were estimated using MLE and saved')

source("./Source_Code/S10_Estimate_MCMC.R") ## Script estimates GEV parameters using MCMC
print('Step10 DONE: Parameters of the GEV distribution were estimated using MCMC and saved')

source("./Source_Code/S11_Return_Level.R") ## Script to compare return levels with and without uncertainty quantification 
print('Step11 DONE: Return level plot was saved')

source("./Source_Code/S12_Didactic_Value_Uncertainty.R") ## Script plots the figure in the paper to showcase the value of uncertainty 
print('Step12 DONE: A plot was saved to showcase the added value of uncertainty quantification')

source("./Source_Code/S13_Lifetime_Deep_Uncertainty.R") ## plots the lifetime uncertainty figure in paper 
print('Step13 DONE: Objectives for a typical house were calculated')

source("./Source_Code/S14_One_House_CBA.R") ## Script calculates objectives for one house
print('Step14 DONE: A parallel axes plot to show the trade-offs between objectives was saved')

source("./Source_Code/S15_Parallel_Axes.R") ## Script for parallel axes plot of all objectives for one house
print('Step15 DONE: Trade-offs between upfront cost and expected damages was saved')

source("./Source_Code/S16_Tradeoff_investment_damages.R") ## Script for a 2D plot of trade-offs between upfront investment and expected damages
print('Step16 DONE: A pool of houses was created')

source("./Source_Code/S17_Tradeoff_reliability_cost.R") ## Script for a 2D plot of trade-offs between upfront investment and reliability 
print('Step17 DONE: Plots to compare strategies were saved')

if(!use_prepared_data){ #If you are planning to use the pre-generated data, do not run this script. This script takes a long time to run.
  source("./Source_Code/S18_Many_Houses_CBA.R") ## Script generates many houses and analyzes objectives for each of those houses. 
  print('Step18 DONE: Houses that are recommended for elevating are classified with CART')
}

source("./Source_Code/S19_Community_Analysis.R") ## Script creates plots with for comparing FEMA, with-uncertainty, and without-uncertainty strategies 
print('Step19 DONE: Houses in which FEMA does not pass B/C test are classified with CART')

source("./Source_Code/S20_CART_FEMA.R") ## Script aims at classifying houses in which FEMA's recommendation does not pass the benefit-to-cost test (using CART).
print('Step20 DONE: Houses that are recommended for elevating are classified with PRIM')

source("./Source_Code/S21_Discount_Rate_Timeseries.R") 
print('Step21 DONE: Houses in which FEMA does not pass B/C test are classified with PRIM')

source("./Source_Code/S22_Disc_Rate_on_Damage_Comparision.R")
print('Step 22 DONE: Comparison of damages under different discount rate models')

source("./Source_Code/S23_Disc_Factor_Uncertainty.R")
print('Step 23 DONE: Comparison of discount factors under different discount rate models')

if(!use_prepared_data){ #If you are planning to use the pre-generated data, do not run this script. This script takes a long time to run.
  source("./Source_Code/S24_SA_estimate_saltelli_mostlikely_scenario.R")
  print('Step 24 DONE: Estimate Sobol indices for the most likely scenario (HAZUS-Drift)')
}

if(!use_prepared_data){ #If you are planning to use the pre-generated data, do not run this script. This script takes a long time to run.
  source("./Source_Code/S25_SA_estimate_saltelli_deep.R")
  print('Step 25 DONE: Estimate indices for the deep uncertainties')
}

if(!use_prepared_data){ #If you are planning to use the pre-generated data, do not run this script. This script takes a long time to run.
  source("./Source_Code/S26_SA_estimate_saltelli_dr_range.R")
  print('Step 26 DONE: Estimate sobol indices when discount rate uncertainty is quantified by sampling from a range')
}

if(!use_prepared_data){ #If you are planning to use the pre-generated data, do not run this script. This script takes a long time to run.
  source("./Source_Code/S27_SA_estimate_scenario_map.R")
  print('Step 27 DONE: Estimate Sobol indices for all the possible scenarios other than the most likely one')
}

if(!use_prepared_data){ #If you are planning to use the pre-generated data, do not run this script. This script takes a long time to run.
  source("./Source_Code/S28_SA_estimate_vulnerability_scenario_map.R")
  print('Step 28 DONE: Estimate Sobol indices for all combinations of house vulnerability factors (8 different combinations)')
}

source("./Source_Code/S29_SA_radialPlot_mostlikely_scenario.R")
print('Step 29 DONE: Plot the radial plot for the most likely scenario')

source("./Source_Code/S30_SA_radialPlot_deep_uncertainties.R")
print('Step 30 DONE: Plot the radial plot for deep uncertainties')

source("./Source_Code/S31_SA_radialPlot_scenario_map.R")
print('Step 31 DONE: Plot the figure with radial plots of different scenarios (four combinations)')

source("./Source_Code/S32_SA_radialPlot_vulnerability_factor_map.R")
print('Step 32 DONE: Plot the figure with radial plots of different vulnerability factors')

source("./Source_Code/S33_SA_radialPlot_dr_range.R")
print('Step 33 DONE: Plot the figure with radial plot when discount rate is sampled from a range')
