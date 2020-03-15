# SRvival

SRvival is a user-friendly survival analysis toolbox created using the R Shiny platform which allows customised evaluation of prognostic markers. Users can investigate associations between protein expression and patient survival in breast cancer and its molecular subtypes, and can also upload their own datasets for analysis. SRvival allows separate analysis across four survival endpoints - overall survival, disease-free survival, progression-free interval and disease-free interval. Data can be stratified into LuminalA, LuminalB, HER2+, Basal and Normal-like subtypes if desired.

SRvival plots are based on data from the publically available TCGA Breast dataset , which contains survival and protein expression information for 221 proteins in 874 patients. Protein expression was quantified using reverse phase protein arrays. Data is stratified into high and low expression groups based on median expression for each protein. Kaplan Meier survival models and log rank p-values are calculated using the R package, survival.

# Uploading Files

Files in .csv format can be uploaded in the Custom Data section. Each predictor variable (the genes or conditions of interest) should be represented in a column, and each individual represented by a row. Time and endpoint columns should be so named, as in the sample table below:

patient 	endpoint 	time 	gene1 	gene2 	gene3
TCGA-3C-AALI 	0 	4005 	low 	low 	low
TCGA-3C-AALK 	0 	1448 	low 	low 	low
TCGA-4H-AAAK 	0 	348 	low 	low 	low
TCGA-5L-AAT1 	0 	1471 	high 	low 	low
TCGA-5T-A9QA 	0 	303 	high 	high 	high
TCGA-A1-A0SF 	0 	1463 	low 	high 	high

You can browse your data table under the My Data tab. Entering a variable name in the selection box will automatically generate a Kaplan Meier plot based on your sample data. Check for a difference in your survival curves under the My Summary tab, where you will find the chi-squared statistic and log-rank p-value for your survival model.

# Interpreting Survival Plots

The Kaplan Meier method describes the probability of survival at a specified timepoint. At time = 0, the probability of survival is 1, and decreases as time progresses. A steeper slope of the survival function corresponds to a poorer prognosis. The effect of gene expression (or any other predictor variable) on patient prognosis can be evaluated by visualising the differences in survival curves for high (red) and low (blue) expression. The log rank p-value for difference in survival curves can be found under the Model Summary tab. A log rank p-value < 0.05 indicates that the protein of interest is significantly associated with patient survival in the selected cohort.

