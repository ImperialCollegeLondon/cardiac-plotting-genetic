### ShinyApp Codes ##########

ui.r - is the user interface object

server.r - is the server function

In order the code to work a folder "24k" should be created including all subfolders with the names of the association e.g. Age, BSA, SBP etc
conatining filesd like Age_beta_WT.txt and Age_BHpvaluesTFCE_WT.txt

The 24k folder should be at the same directory as ui.r and server.r and all these should be included in a folder named e.g."CardiacExplorer".

The shiny app could then be called from RStudio as runApp("CardiacExplorer")