
# Mass Univariate Analysis 

A 3D computational modelling of left ventricular geometry in the UK Biobank participants with whole exome sequencing. The output models represent the surface of the
left ventricle on which standardised beta coefficients are calculated and mapped to show the strength of associations between genotype status and wall thickness
adjusted for age, gender, race, body surface area and systolic blood pressure. The models can be plotted with [3D_plot](https://github.com/ImperialCollegeLondon/HCM_expressivity/blob/master/3D_regression_analysis/3D_plot.R).  

## [functions](https://github.com/ImperialCollegeLondon/HCM_expressivity/tree/master/3D_regression_analysis/functions)

Consists of : 

* [multiply.cpp](https://github.com/ImperialCollegeLondon/HCM_expressivity/tree/master/3D_regression_analysis/functions/multiply.cpp) function uses "eigenMapMatMult" for faster matrices multiplication.

* [murq.R](https://github.com/ImperialCollegeLondon/HCM_expressivity/tree/master/3D_regression_analysis/functions/murq.R) for the mass univariate regression using the QR decomposition of the matrix into an orthogonal matrix and a triangular matrix.

* [permFL_fast](https://github.com/ImperialCollegeLondon/HCM_expressivity/tree/master/3D_regression_analysis/functions/permFL_fast.R) for the faster mass univariate regression analysis computing TFCE derived p-values using Freedman-Lane procedure.

This function first computes the nuisance matrix (Z) and the residual-forming matrix (Rz) using "eigenMapMatMult" for faster matrix multiplication. 

The procedure is then similar to permFL.R function from the package [mutools3D](https://github.com/UK-Digital-Heart-Project/mutools3D).

* TFCE and mur functions are also used from the package [mutools3D](https://github.com/UK-Digital-Heart-Project/mutools3D).

## [data](https://github.com/ImperialCollegeLondon/HCM_expressivity/tree/master/3D_regression_analysis/data)

Consists of the data used in the [Mass_univaraite_analysis_UKBB](https://github.com/ImperialCollegeLondon/HCM_expressivity/blob/master/3D_regression_analysis/Mass_univariate_analysis_UKBB.R).


# Significance testing for GENSCAN

A code for computing the z-statistics from Wilcoxon test of visit 1 and visit 2 in the GENSCAN population using TFCE on the z-statistics from premZ.R code.

# Check and move files 

A data analysis code for:
* loading all foldernames from directory with bad segmentations
* producing a matrix with phenotypes (WT ir S2S) of this folder
* checking the position where the outliers occur
* When they are corrected use the code to move them to the directory with the good segmentations.

The codes has been fully documented and descriptions are available within.