### ShinyApp Codes ##########

## CardiacExplorer

ui.r - is the user interface object

server.r - is the server function

In order the code to work:
* a folder "2D" should be created containing a csv file with all 2D association.
* a folder "27k" should be created including all subfolders with the names of the association e.g. Age, BSA, SBP etc
containing files like Age_beta_WT.txt and Age_BHpvaluesTFCE_WT.txt

These folders should be at the same directory as ui.r and server.r and all these should be included in a folder named e.g."CardiacExplorer".

The shiny app could then be called from RStudio as runApp("CardiacExplorer")

The Shiny application enviroment can found here: https://digitalheart.shinyapps.io/CardiacExplorer/

## Mass Univariate Analysis for shiny 

A 3D computational modelling of left ventricular geometry in the UK Biobank participants with whole exome sequencing. The output models represent the surface of the
left ventricle on which standardised beta coefficients are calculated and mapped to show the strength of associations between genotype status and wall thickness
adjusted for age, gender, race, body surface area, systolic blood pressure and diastolic blood pressure. The models can be plotted with the shinyApp.  

Functions and data can be found in /cardiac drive (/cardiac/Experiments_of_Maria

The code has been fully documented and descriptions are available within.