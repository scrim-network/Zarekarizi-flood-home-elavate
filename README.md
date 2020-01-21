# Neglecting Uncertainties Leads to Suboptimal Decisions About Home-Owners Flood Risk Management

Authors: Mahkameh Zarekarizi (1)*, Vivek Srikrishnan (1), and Klaus Keller (1,2)

1 Earth and Environmental Systems Institute, the Pennsylvania State University, University Park, PA, USA
2 Department of Geosciences, the Pennsylvania State University, University Park, PA, USA

* Corresponding author

Mahkameh Zarekarizi*(1), Vivek Srikrishnan (1), and Klaus Keller (1,2)

1 Earth and Environmental Systems Institute, the Pennsylvania State University, 
University Park, PA, USA
2 Department of Geosciences, the Pennsylvania State University, University Park, PA, USA

* Corresponding author 

# Abstract
Homeowners around the world elevate houses to manage flood risks. Deciding how high to elevate the house poses a nontrivial decision problem. The U.S. Federal Emergency Management Agency (FEMA) recommends elevating a house to the Base Flood Elevation (the elevation of the 100-yr flood) plus a freeboard. This recommendation neglects many uncertainties. Here we use a multi-objective robust decision-making framework to analyze this decision in the face of deep uncertainties. We find strong interactions between the economic, engineering, and Earth science uncertainties, illustrating the need for an integrated analysis. We show that considering deep uncertainties surrounding flood hazards, the discount rate, the house lifetime, and the fragility increases the economically optimal house elevation to values well above FEMA’s recommendation. An improved decision-support for home-owners has the potential to drastically improve decisions and outcomes. 

# Citing the code
Until the paper is published, please cite our preprint at this link:https://arxiv.org/abs/2001.06457  

# System requirement 
To run this program you will need R and the following packages: lattice,Kendall,ismev,evdbayes,evir,evd,lhs,fields,plotrix,lhs,rpart,rpart.plot,DEoptim,prim,truncnorm,sdtoolkit,sensitivity,pracma

# How to run the code 
General description:

This rep is a package of multiple scripts indicated in the order they will be needed. For example, S1_....R indicates step 1.
The entire package is controled by main_script.R. This script contains a swtich that gives you freedom to run the entire package locally on your machine or use the prepared data that was used in the paper.
To use the prepared data, set use_prepared_data=TRUE (this is the default option). To run the code yourself locally, set use_prepared_data=FALSE
On a regular desktop computer, the entire program if (use_prepared_data=FALSE) should take around 10 hours.
List of packages that you need to install before running the code is provided below.

To Run:
1. Set the working directory to the main folder (Zarekarizi-flood-flood-home-elevate).
2. set use_prepared_data
To do so:
1. If on RStudio, open the README.md file. Then on the menu bar, go to
Session-->Set Working Directory-->To Source File Location
2. If on RStudio, on the lower right box, open "Zarekarizi-Home-Elevation"
Then, click on More --> Set as Working Directory
3. On console, type the following command:
setwd("~/.../../../../Zarekarizi-Home-Elevation")
4. Run (by clicking on Source in Rstudio)


What happens next:

R will go through all the scripts one by one (S01_XLRM.R all the way to  S33_SA_radialPlot_dr_range.R)
After each script is done, there will be a message om screen reporting that script is done.
The scripts will use the input data saved in the folder called "Input_Data"

Outputs:

1. Figures are saved in Figures directory under the main folder
2. Data are saved in the Output_Data folder under the main directory
Requirements before running


# Contacts 
Mahkameh Zarekarizi mxz414@psu.edu and mahkameh.zare@gmail.com

Vivek Srikrishnan vxs914@psu.edu

Klaus Keller kzk10@psu.edu

# License
Copyright 2019 Mahkameh Zarekarizi

These files are free software: you can redistribute them and/or modify them under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

These files are distributed in the hope that they will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with these files. If not, see http://www.gnu.org/licenses/.
 
