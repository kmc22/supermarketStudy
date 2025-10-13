# Citation
Meijaard Emily M, Kimberly M Carlson, Douglas Sheil, Syahmi Zaini, Erik Meijaard. 2025. Does Palm Oil Really Rule the Supermarket? An Assessment of Three Western Supermarket Chains. *Environmental Research: Food Systems.*


# Licensing
The R code is covered under the MIT License.

The following data in the "input" folder were generated for this project and are covered by a CC0 (Creative Commons Zero) license:  
categoryTotals.xlsx  
productsSampled.xlsx

# Data
All the input data are in the "input" folder. All files written by the code are saved to the "output" folder.

# Code
Open the supermarketStudy.Rproj file to ensure a reproducible environment.  

fao analysis.R - downloads and analyzes data from FAOStat  
Please run the following .R files in the order specified by the numeric file names since 2 is dependent on 1:  
1_bootstrap.R - undertakes the bootstrap analysis  
2_figures.R - produces the mean and confidence interval figures  
