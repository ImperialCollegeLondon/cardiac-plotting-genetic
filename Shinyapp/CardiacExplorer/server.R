######################################################
################# server.R ###########################
######################################################
######################################################
#rm(list = ls(all = TRUE))  
#start with an empty environment

# install.packages("shiny")
# install.packages("plotly")
# install.packages("data.table")
# install.packages("dplyr")
# install.packages("remotes")
# library(remotes)
# remotes::install_github("rstudio/d3heatmap", force=TRUE)
# install.packages("plotly")
# install.packages("visNetwork")
# install.packages("igraph")
# install all packages required

#setwd("~/cardiac/Experiments_of_Maria/ShinyApp/")
library(shiny)
library(plotly)
library(data.table)
library(dplyr)
library(d3heatmap)
library(visNetwork)
library(igraph)

choices2 <-c("Age","Sex F0/M1","BSA","BMI", "Weight", "Height", "Body fat (%)", "Whole body fat mass", "Waist circumference", 
             "Hip circumference","SBP", "DBP", "Pulse rate", "Pulse wave stiffness","Impedance of whole body",
             "Smoking status","Alcohol drinker status", "High blood pressure" ,"Angina", "Stroke", 
             "Heart attack","Asthma","Bronchitis", "Hypertension","Serious medical condition or disability diagnosed by doctor", 
             "High cholesterol","Hypertrophic cardiomyopathy", "Heart failure", "Cardiac disease", 
             "Pulmonary embolism", "Rheumatoid arthritis","COPD", "Peripheral vascular disease", "Depression", "Dementia",
             "Parkinson's disease","Cancer diagnosed by doctor", "Diabetes diagnosed by doctor","Number of medication taken",
             "Blood pressure medication uptake","Cholesterol medication uptake", "Insulin medication uptake" ,
             "Duration of moderate activity (in mins/day)", "Duration of vigorous activity (in mins/day)", "Assessment centre")
extractname<-c("Age","Sex","BSA","BMI", "Weight", "Height", "body_fat_perc", "Whole_body_fat_mass", "Waist_circumference", 
                 "Hip_circumference","SBP", "DBP", "Pulse_rate", "Pulse_wave_stiffness","Impedance_whole_body",
                 "Smoking","Alcohol", "High_blood_pressure" ,"Angina", "Stroke", 
                 "Heart_attack","Asthma","Bronchitis", "Hypertension","Disability", 
                 "High_cholesterol","HCM", "HF", "Cardiac_disease", 
                 "Pulmonary_embolism", "Rheumatoid_arthritis","COPD", "PVD", "Depression", "Dementia",
                 "Parkinsons","Cancer", "Diabetes","Medication",
                 "BP_med","Cholesterol_med", "Insulin_med" ,
                 "Moderate", "Vigorous", "Assessment_centre_2")
points <- c("Endocardial", "Epicardial","Full shape")
pv_names <- c("pvalues","BHpvalues","BHpvaluesTFCE")

function(input, output,session) {
  cova <- reactive(input$cov)
  plotmod <- reactive(input$plotmod)
  v <- reactiveValues(doPlot = FALSE, dotext=" ")
  observeEvent(input$cov, {
    v$doPlot <- FALSE
    v$dotext<-" "
  })
  observeEvent(input$plotmod, {
    v$doPlot <- FALSE
    v$dotext<-" "
  })
  
  observe({

    data_all<-read.csv("2D/2d_association.csv")
    data_all<-data_all[,-1]
    colnames(data_all)[c(21:23,17:19,30)]<-c("Blood_pressure_medication","Cholesterol_medication", "Insulin_medication","Duration_of_moderate_activity (min/day)", 
                                       "Duration_of_vigorous_activity (min/day)", "Assessment_centre","Heart_failure")
    colnames(data_all)[47:131]<-c("Longitudinal_PDSR (s-1)","Radial_PDSR (s-1)", "Ecc_AHA_1 (%)","Ecc_AHA_2 (%)", "Ecc_AHA_3 (%)",
                                  "Ecc_AHA_4 (%)","Ecc_AHA_5 (%)","Ecc_AHA_6 (%)","Ecc_AHA_7 (%)","Ecc_AHA_8 (%)","Ecc_AHA_9 (%)",
                                  "Ecc_AHA_10 (%)","Ecc_AHA_11 (%)","Ecc_AHA_12 (%)","Ecc_AHA_13 (%)","Ecc_AHA_14 (%)","Ecc_AHA_15 (%)",
                                  "Ecc_AHA_16 (%)","Ecc_Global (%)", "Err_AHA_1 (%)", "Err_AHA_2 (%)","Err_AHA_3 (%)","Err_AHA_4 (%)",
                                  "Err_AHA_5 (%)","Err_AHA_6 (%)","Err_AHA_7 (%)","Err_AHA_8 (%)","Err_AHA_9 (%)","Err_AHA_10 (%)","Err_AHA_11 (%)",
                                  "Err_AHA_12 (%)","Err_AHA_13 (%)","Err_AHA_14 (%)","Err_AHA_15 (%)","Err_AHA_16 (%)","Err_Global (%)",                  
                                  "Ell_1 (%)","Ell_2 (%)","Ell_3 (%)","Ell_4 (%)","Ell_5 (%)","Ell_6 (%)", "Ell_Global (%)","WT_AHA_1 (mm)",
                                  "WT_AHA_2 (mm)","WT_AHA_3 (mm)","WT_AHA_4 (mm)","WT_AHA_5 (mm)","WT_AHA_6 (mm)","WT_AHA_7 (mm)","WT_AHA_8 (mm)",                   
                                  "WT_AHA_9 (mm)","WT_AHA_10 (mm)","WT_AHA_11 (mm)","WT_AHA_12 (mm)","WT_AHA_13 (mm)","WT_AHA_14 (mm)",                  
                                  "WT_AHA_15 (mm)","WT_AHA_16 (mm)","WT_Global (mm)","AAo distensibility (10-3 mmHg-1)", "DAo distensibility (10-3 mmHg-1)",
                                  "AAo max area (mm2)","AAo min area (mm2)","DAo max area (mm2)","DAo min area (mm2)","LVEDVi (mL/m2)","LVESVi (mL/m2)",
                                  "LVSVi (mL/m2)","LVMi (g/m2)","LAVi max (mL/m2)","LAVi min (mL/m2)","LASVi (mL/m2)","LVEF (%)","LVCO (L/min)","LVCI (L/min/m2)",
                                  "LAEF (%)","RVEDVi (mL/m2)","RVESVi (mL/m2)","RVSVi (mL/m2)","RAVi max (mL/m2)","RAVi min (mL/m2)",                    
                                  "RASVi (mL/m2)","RVEF (%)","RAEF (%)")
    rownames(data_all)<-colnames(data_all)
    
    output$downloadData2 <- downloadHandler(
      filename = function() {
        paste("2d_association.csv")
      },
      content = function(file) {
        write.csv(data_all, file, col.names = TRUE, row.names = TRUE)
      }
    )
    
    isolate({
      output$heatmap <- renderD3heatmap({
        col <- colorRampPalette(c("#4477AA", "#77AADD", "#FFFFFF", "#EE9988", "#BB4444"))
        d3heatmap(data_all, dendrogram="row",scale = "row", colors=scales::col_quantile(col(200),NULL,5), 
                  cexRow = 0.65, cexCol = 0.55,
                  show_grid = T)
        
      })
    })
    bnew<-as.matrix(data_all)
    diag(bnew)<-0
    colnames(bnew)<-NULL;rownames(bnew)<-NULL
    g1<-graph_from_adjacency_matrix(bnew, weighted = TRUE, mode = "undirected")
    isolate({
      output$network <- renderVisNetwork({
        # Data Preparation --------------------------------------------------------
        # visIgraph(g1)
        igraph_net<-toVisNetworkData(g1)
        nodes<-igraph_net[["nodes"]]
        edges<-igraph_net[["edges"]]
        #Nodes
        nodes <- as.data.frame(igraph_net[1])
        colnames(nodes) <- c("id", "label")
        nodes$label<-colnames(data_all)
        #Edges
        # edges <- as.data.frame(igraph_net[2])
        bnew<-as.matrix(data_all);diag(bnew)<-0
        edges <- data.frame(to = rep(rownames(bnew), times = ncol(bnew)),
                            from = rep(colnames(bnew), each = nrow(bnew)),
                            value = as.vector(bnew),
                            stringsAsFactors = FALSE)
        edges<-edges[,c(2,1,3)]
        colnames(edges) <- c("from", "to", "value")
        e_from<-match(edges$from,colnames(data_all))
        e_to<-match(edges$to,colnames(data_all))
        edges$from<-e_from;edges$to<-e_to
        value<-edges$value
        val<-edges$value
        p1<-which(sign(val)==-1)
        val[p1]<-(-val[p1])
        edges$value<-val
        
        edges$color<- "#6DAC4FFF"
        p2<-which(edges$from==47)
        edges$color[p2[1]:nrow(edges)]<-"#FF7070"
        edges$length<-800
        title_from<-colnames(data_all)[edges$from]
        title_to<-colnames(data_all)[edges$to]
        edges$title<-paste(title_from, "~", title_to,":",round(value,3))
        edges$label<-round(value,3)
        cl<-matrix("Non-imaging Phenotypes",nrow=1, ncol =46)
        Ph<-matrix("Imaging Phenotypes", nrow = 1, ncol = 85)
        group <-as.character(cbind(cl,Ph))
        nodes$group<-group
        col1<-matrix("#6DAC4FFF",nrow=1, ncol = 46)
        col2<-matrix("#FF7070",nrow=1, ncol = 85)
        color<-as.character(cbind(col1,col2))
        nodes$color<-color
        nodes$font.size<-35
        pfind<-which(cova()==colnames(data_all))
        p<-which(edges$from==pfind)
        p_all<-c(p); p_all<-unique(p_all)
        
        edges1<-edges[p_all,]
        pe<-which(edges1$value==0)
        edges1<-edges1[-pe,]
        if (plotmod()=="Smooth"){
          visNetwork(nodes, edges1) %>%
            # visIgraphLayout(smooth=TRUE)%>%
            # visGroups(groupname = "Biomarkers", color = "magenta") %>%
            visGroups(groupname = "Non-imaging Phenotypes", color = "#6DAC4FFF") %>%
            visGroups(groupname = "Imaging Phenotypes", color = "#FF7070") %>%
            visLegend()%>%
            visNodes(
              shape = "dot", size=30,
              color = list(
                background = "#0085AF",
                border = "#013848",
                highlight = "#FF8000"
              ),
              shadow = list(enabled = TRUE, size = 10)
            ) %>%
            visEdges(
              shadow = FALSE,
              color = list(color = "#0085AF", highlight = "#C62F4B"),scaling = list(min=1)
            ) %>%
            visPhysics(solver="forceAtlas2Based")%>%
            # visHierarchicalLayout(direction="LR", levelSeparation = 500)%>%
            visOptions(highlightNearest = T,
                       nodesIdSelection = list(enabled = T, main="Associations with covariates",
                                               selected="1",
                                               values= c(1:133),
                                               style='width: 200px; height: 26px;
                                                                        outline:none;
                                                                        border:none;'))%>%
            # selectedBy = "id") 
            visLegend()%>%
            addFontAwesome()
        }
        else if (plotmod()=="Circle"){
          visNetwork(nodes, edges1, width="50%") %>%
            #visIgraphLayout()%>%
            visIgraphLayout(layout="layout_in_circle")%>%
            # visGroups(groupname = "Biomarkers", color = "magenta") %>%
            visGroups(groupname = "Non-imaging Phenotypes", color = "#6DAC4FFF") %>%
            visGroups(groupname = "Imaging Phenotypes", color = "#FF7070") %>%
            visLegend()%>%
            visNodes(
              shape = "dot", size=30,
              color = list(
                background = "#0085AF",
                border = "#013848",
                highlight = "#FF8000"
              ),
              shadow = list(enabled = TRUE, size = 10)
            ) %>%
            visEdges(
              shadow = FALSE, 
              color = list(color = "#0085AF", highlight = "#C62F4B"),scaling = list(min=1)
            ) %>%
            visPhysics(solver="forceAtlas2Based")%>%
            # visHierarchicalLayout(direction="LR", levelSeparation = 500)%>% 
            visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled = T, main="Associations with covariates",
                                                                        selected="1",
                                                                        values= c(1:133),
                                                                        style='width: 200px; height: 26px;
                                                                        outline:none;
                                                                        border:none;'))%>%#,
            # selectedBy = "group") %>%
            visLegend()%>%
            addFontAwesome()
        }
      })
    })
  })
  # give names to all inputs
  phen <- reactive(input$phen)
  whichpoint <- reactive({input$whichee})
  Sel2 <- reactive(input$typeInput2)
  datasetInput <- reactive(input$dataset)
  Adj <- reactive(input$checkbox)
  BH <- reactive(input$checkbox1)
  TFCE <- reactive(input$checkbox2)
  
  endoEpi <- read.table("endo_epi.txt")
  vert2print <- list(which(endoEpi[,4]==0),which(endoEpi[,4]==1),1:length(endoEpi[,4]))
  
  observeEvent(input$helpButton, {
    # on click, send custom message to start help
    showModal(modalDialog(
      title = "CardiacExplorer Help",
      HTML("<ul>3D parametric modelling of left ventricular (LV) geometry in UK biobank participants.<br>
      The model represent the surface of the LV on which standardised beta coefficients are mapped to show the 
      regional strength of associations between the imaging phenotypes and the non-imaging phenotypes.<br><br>
      <li><li>Select Wall thickness (WT) or Surface to surface (S2S) imaging phenotype adjuted for cofounders.<br><br>
      </li><li>Select the non-imaging covariates for the 3D mass univariate regression analysis.<br><br> 
      </li><li>Tick <b>'Adjust for cofounders'</b> checkbox to adjust for other non-imaging phenotypes. <br><br>
      </li><li>Choose points files (Epicardial with 27623 points, Endocardial with 19185 points and Full mesh with 46808 points)<br><br>
      </li><li>Tick <b>'BH'</b> checkbox to use Benjamini-Hochberg procedure.<br><br>
      </li><li>Tick <b>'TFCE'</b> chechbox to use the threshold-free cluster enhancement technique.<br><br>
      </li><li>Press <b>'Plot'</b> every time input parameters are altered.<br><br> 
      <ul><li>Plots might take a few seconds before displayed.<br><br>
      </li><li>Red areas indicate positive associations, blue areas indicate negative associations
           and grey areas indicate non significant correlations.<br><br>
      </li><li>You can download the full mesh coordinates with the beta coefficients or the <br>
           p-values after choosing the dataset to download and press <b>'Download data'</b>.<br>"
      ),
      easyClose = TRUE)
    )})
  v <- reactiveValues(doPlot = FALSE, dotext=" ")
  
  observeEvent(input$typeInput2, {
    v$doPlot <- FALSE
    v$dotext<-" "
  })
  observeEvent(input$checkbox, {
    v$doPlot <- FALSE
    v$dotext<-" "
  })
  observeEvent(input$checkbox1, {
    v$doPlot <- FALSE
    v$dotext<-" "
  })
  observeEvent(input$checkbox2, {
    v$doPlot <- FALSE
    v$dotext<-" "
  })
  observeEvent(input$phen, {
    v$doPlot <- FALSE
    v$dotext<-" "
  })
  observeEvent(input$whichee, {
    v$doPlot <- FALSE
    v$dotext<-" "
  })
  
  observeEvent(input$goButton, {
    v$doPlot <- input$goButton
    v$dotext <- input$goButton
  })
  observe({
    ##### 3D plot ########
    ######################
    output$coolplot <- renderPlotly({
      if (v$doPlot == FALSE) return()
      if (Sel2()==choices2[1]){here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=21003'")}
      if (Sel2()==choices2[3]){here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=22427'")}
      if (Sel2()==choices2[4]){here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=21001'")}
      if (Sel2()==choices2[5]){here<-c("href='httpshttps://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=21002'")}
      if (Sel2()==choices2[6]){here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=12144'")}
      if (Sel2()==choices2[11]){here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=4080'")}
      if (Sel2()==choices2[12]){here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=4079'")}
      if (Sel2()==choices2[22]){here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=20002'")}
      if (Sel2()==choices2[23]){here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=20002'")}
      if (Sel2()==choices2[24]){here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=20002'")}
      if (Sel2()==choices2[32]){here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=20002'")}
      if (Sel2()==choices2[34]){here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=20002'")}
      if (Sel2()==choices2[35]){here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=20002'")}
      if (Sel2()==choices2[2]){
        choices =extractname
        Sel2<-"Sex"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=31'")
        ns<-c("26,483")
      } else if (Sel2()==choices2[7]){
          choices =extractname
          Sel2<-"body_fat_perc"
          here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=23099'")
          ns<-c("26,483")
      } else if (Sel2()==choices2[8]){
        choices =extractname
        Sel2<-"Whole_body_fat_mass"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=23100'")
        ns<-c("26,483")
      } else if (Sel2()==choices2[9]){
        choices =extractname
        Sel2<-"Waist_circumference"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=48'")
        ns<-c("26,483")
      } else if (Sel2()==choices2[10]){
        choices =extractname
        Sel2<-"Hip_circumference"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=49'")
        ns<-c("26,483")
      } else if (Sel2()==choices2[13]){
        choices =extractname
        Sel2<-"Pulse_rate"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=4194'")
        ns<-c("26,483")
      } else if (Sel2()==choices2[14]){
        choices =extractname
        Sel2<-"Pulse_wave_stiffness"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=21021'")
        ns<-c("26,483")
      } else if (Sel2()==choices2[15]){
        choices =extractname
        Sel2<-"Impedance_whole_body"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=23106'")
        ns<-c("26,483")
      } else if (Sel2()==choices2[16]){
        choices =extractname
        Sel2<-"Smoking"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=20116'")
        ns<-c("26,396")
      } else if (Sel2()==choices2[17]){
        choices =extractname
        Sel2<-"Alcohol"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=20117'")
        ns<-c("26,475")
      } else if (Sel2()==choices2[18]){
        choices =extractname
        Sel2<-"High_blood_pressure"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=6150'")
        ns<-c("26,465")
      } else if (Sel2()==choices2[19]){
        choices =extractname
        Sel2<-"Angina"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=6150'")
        ns<-c("26,465")
      } else if (Sel2()==choices2[20]){
        choices =extractname
        Sel2<-"Stroke"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=6150'")
        ns<-c("26,465")
      } else if (Sel2()==choices2[21]){
        choices =extractname
        Sel2<-"Heart_attack"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=6150'")
        ns<-c("26,465")
      } else if (Sel2()==choices2[25]){
        choices =extractname
        Sel2<-"Disability"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=2473'")
        ns<-c("26,186")
      } else if (Sel2()==choices2[26]){
        choices =extractname
        Sel2<-"High_cholesterol"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=20002'")
        ns<-c("26,483")
      }else if (Sel2()==choices2[27]){
        choices =extractname
        Sel2<-"HCM"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=20002'")
        ns<-c("26,483")
      }else if (Sel2()==choices2[28]){
        choices =extractname
        Sel2<-"HF"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=20002'")
        ns<-c("26,483")
      } else if (Sel2()==choices2[29]){
        choices =extractname
        Sel2<-"Cardiac_disease"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=20002'")
        ns<-c("26,483")
      } else if (Sel2()==choices2[30]){
        choices =extractname
        Sel2<-"Pulmonary_embolism"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=20002'")
        ns<-c("26,483")
      } else if (Sel2()==choices2[31]){
        choices =extractname
        Sel2<-"Rheumatoid_arthritis"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=20002'")
        ns<-c("26,483")
      } else if (Sel2()==choices2[33]){
        choices =extractname
        Sel2<-"PVD"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=20002'")
        ns<-c("26,483")
      } else if (Sel2()==choices2[36]){
        choices =extractname
        Sel2<-"Parkinsons"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=20002'")
        ns<-c("26,483")
      } else if (Sel2()==choices2[37]){
        choices =extractname
        Sel2<-"Cancer"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=2453'")
        ns<-c("26,407")
      } else if (Sel2()==choices2[38]){
        choices =extractname
        Sel2<-"Diabetes"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=2443'")
        ns<-c("26,431")
      } else if (Sel2()==choices2[39]){
        choices =extractname
        Sel2<-"Medication"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=137'")
        ns<-c("26,483")
      } else if (Sel2()==choices2[40]){
        choices =extractname
        Sel2<-"BP_med"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=6177'")
        ns<-c("26,405")
      } else if (Sel2()==choices2[41]){
        choices =extractname
        Sel2<-"Cholesterol_med"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=6177'")
        ns<-c("26,404")
      } else if (Sel2()==choices2[42]){
        choices =extractname
        Sel2<-"Insulin_med"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=6177'")
        ns<-c("26,404")
      } else if (Sel2()==choices2[43]){
        choices =extractname
        Sel2<-"Moderate"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=894'")
        ns<-c("26,483")
      }else if (Sel2()==choices2[44]){
        choices =extractname
        Sel2<-"Vigorous"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=914'")
        ns<-c("26,483")
      }else if (Sel2()==choices2[45]){
        choices =extractname
        Sel2<-"Assessment_centre_2"
        here<-c("href='https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=54'")
        ns<-c("26,483")
      } else{
        choices=extractname 
                  
        Sel2<-input$typeInput2
      }
      if (Sel2()==choices2[1] || Sel2()==choices2[3] || Sel2()==choices2[4] || Sel2()==choices2[5] || Sel2()==choices2[6] || 
          Sel2()==choices2[11] || Sel2()==choices2[12] || Sel2()==choices2[22] || Sel2()==choices2[23] || Sel2()==choices2[24] 
          || Sel2()==choices2[32] || Sel2()==choices2[34] || Sel2()==choices2[35]){ns<-c("26,483")}
      
      num_sel<-which(choices==Sel2)
      datafile<-"27k" 
      # }
      if (Adj() == TRUE){
        filepath<-paste(datafile,choices[num_sel],choices[num_sel],sep = "/")
      } else if (Adj() == FALSE) {
        if (Sel2() == "Age" || Sel2() == "BSA" || Sel2() == "SBP" || Sel2() == "Pulse rate" || Sel2() == "Number of medication taken" || Sel2() == "Duration of moderate activity (in mins/day)" || Sel2() == "Duration of vigorous activity (in mins/day)"){
          adjpath<-paste("Adjust_0",choices[num_sel], sep = "/")
          filepath<-paste(datafile,choices[num_sel],adjpath,sep = "/")
          
        } else {
          showModal(modalDialog(
            HTML("<h5>You have unchecked 'Adjust for cofounders' when choosing a categorical variable
                   or this option is not available for the specific phenotype.<br>
                  <br>This will be automatically checked. Please press 'Plot'. "),
            easyClose = TRUE)
          )
          updateCheckboxInput(session,"checkbox", label = "Adjust for cofounders.", value = TRUE)
          filepath<-paste(datafile,choices[num_sel],choices[num_sel],sep = "/")
        }
      }
      
      isolate({
        if (TFCE() == FALSE & BH() == FALSE) {
          Data <- read.table(paste(filepath,"_beta_",phen,".txt", sep = ""), quote="\"", comment.char="")
          colnames(Data) <- c("x", "y", "z", "beta")
          Data_pvalues <- read.table(paste(filepath,"_pvalues_",phen(),".txt",sep = ""), quote="\"", comment.char="")
          colnames(Data_pvalues) <- c("x","y","z","pvalues")
          
          if (whichpoint()==points[1]){
            Data <- Data[vert2print[[1]],]
            Data_pvalues <- Data_pvalues[vert2print[[1]],]
          } else if (whichpoint()==points[2]){
            
            Data <- Data[vert2print[[2]],]
            Data_pvalues <- Data_pvalues[vert2print[[2]],]
          } else if (whichpoint()==points[3]){
            
            Data <- Data[vert2print[[3]],]
            Data_pvalues <- Data_pvalues[vert2print[[3]],]
          }
          observeEvent(input$dataset, {
            if (datasetInput() == "Beta coefficients") {
              output$downloadData <- downloadHandler(
                filename = function() {
                  paste(choices[num_sel],"_beta_",phen(),".txt", sep = "")
                },
                content = function(file) {
                  write.table(Data, file, col.names = TRUE, row.names = FALSE)
                }
              )
            } else if (datasetInput() == "P-values") {
              output$downloadData <- downloadHandler(
                filename = function() {
                  paste(choices[num_sel],"_pvalues_",phen(),".txt", sep = "")
                },
                content = function(file) {
                  write.table(Data_pvalues, file, col.names = TRUE, row.names = FALSE)
                }
              )
            }
          })
        } else if (TFCE() == FALSE & BH() == TRUE){
          Data <- read.table(paste(filepath,"_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
          colnames(Data) <- c("x", "y", "z", "beta")
          Data_pvalues <- read.table(paste(filepath,"_BHpvalues_",phen(),".txt",sep = ""), quote="\"", comment.char="")
          colnames(Data_pvalues) <- c("x","y","z","pvalues")
          
          if (whichpoint()==points[1]){
            Data <- Data[vert2print[[1]],]
            Data_pvalues <- Data_pvalues[vert2print[[1]],]
          } else if (whichpoint()==points[2]){
            
            Data <- Data[vert2print[[2]],]
            Data_pvalues <- Data_pvalues[vert2print[[2]],]
          } else if (whichpoint()==points[3]){
            
            Data <- Data[vert2print[[3]],]
            Data_pvalues <- Data_pvalues[vert2print[[3]],]
          }
          observeEvent(input$dataset, {
            
            if (datasetInput() == "Beta coefficients") {
              output$downloadData <- downloadHandler(
                filename = function() {
                  paste(choices[num_sel],"_beta_",phen(),".txt", sep = "")
                },
                content = function(file) {
                  write.table(Data, file, col.names = TRUE, row.names = FALSE)
                }
              )
            } else if (datasetInput() == "P-values") {
              output$downloadData <- downloadHandler(
                filename = function() {
                  paste(choices[num_sel],"_BHpvalues_",phen(),".txt", sep = "")
                },
                content = function(file) {
                  write.table(Data_pvalues, file, col.names = TRUE, row.names = FALSE)
                }
              )
            }
          })
          
        } else if (TFCE() == TRUE & BH() == TRUE) {
          Data <- read.table(paste(filepath,"_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
          colnames(Data) <- c("x", "y", "z", "beta")
          Data_pvalues <- read.table(paste(filepath,"_BHpvaluesTFCE_",phen(),".txt",sep = ""), quote="\"", comment.char="")
          colnames(Data_pvalues) <- c("x","y","z","pvalues")
          
          if (whichpoint()==points[1]){
            Data <- Data[vert2print[[1]],]
            Data_pvalues <- Data_pvalues[vert2print[[1]],]
          } else if (whichpoint()==points[2]){
            
            Data <- Data[vert2print[[2]],]
            Data_pvalues <- Data_pvalues[vert2print[[2]],]
          } else if (whichpoint()==points[3]){
            
            Data <- Data[vert2print[[3]],]
            Data_pvalues <- Data_pvalues[vert2print[[3]],]
          }
          observeEvent(input$dataset, {
            if (datasetInput() == "Beta coefficients") {
              output$downloadData <- downloadHandler(
                filename = function() {
                  paste(choices[num_sel],"_beta_",phen(),".txt", sep = "")
                },
                content = function(file) {
                  write.table(Data, file, col.names = TRUE, row.names = FALSE)
                }
              )
            } else if (datasetInput() == "P-values") {
              output$downloadData <- downloadHandler(
                filename = function() {
                  paste(choices[num_sel],"_BHpvaluesTFCE_",phen(),".txt", sep = "")
                },
                content = function(file) {
                  write.table(Data_pvalues, file, col.names = TRUE, row.names = FALSE)
                }
              )
            }
          })
        } else if (TFCE() == TRUE & BH() == FALSE) {
          Data <- read.table(paste(filepath,"_beta_",phen(),".txt", sep = ""), quote="\"", comment.char="")
          colnames(Data) <- c("x", "y", "z", "beta")
          
          Data_pvalues <- read.table(paste(filepath,"_pvaluesTFCE_",phen(),".txt",sep = ""), quote="\"", comment.char="")
          colnames(Data_pvalues) <- c("x","y","z","pvalues")
          
          if (whichpoint()==points[1]){
            Data <- Data[vert2print[[1]],]
            Data_pvalues <- Data_pvalues[vert2print[[1]],]
          } else if (whichpoint()==points[2]){
            
            Data <- Data[vert2print[[2]],]
            Data_pvalues <- Data_pvalues[vert2print[[2]],]
          } else if (whichpoint()==points[3]){
            
            Data <- Data[vert2print[[3]],]
            Data_pvalues <- Data_pvalues[vert2print[[3]],]
          }
          
          observeEvent(input$dataset, {
            
            if (datasetInput() == "Beta coefficients") {
              output$downloadData <- downloadHandler(
                filename = function() {
                  paste(choices[num_sel],"_beta_",phen(),".txt", sep = "")
                },
                content = function(file) {
                  write.table(Data, file, col.names = TRUE, row.names = FALSE)
                }
              )
            } else if (datasetInput() == "P-values") {
              output$downloadData <- downloadHandler(
                filename = function() {
                  paste(choices[num_sel],"_pvaluesTFCE_",phen(),".txt", sep = "")
                },
                content = function(file) {
                  write.table(Data_pvalues, file, col.names = TRUE, row.names = FALSE)
                }
              )
            }
          })
        }
        pos<-which(Data_pvalues$pvalues < 0.05)
        sig <- (length(pos)/nrow(Data))*100
        significance_area<-format(round(sig, 0), nsmall = 0)
        average_beta <- format(round(mean(Data$beta[pos]), 3), nsmall = 2)
        
        if (length(pos) == 0) {
          output$text <- renderUI({
            if (v$dotext == " ") return()
            str1 <- paste("<br><h4>Regional strength of associations between ",phen()," and ", Sel2(),"in UKBB subjects, n = ",ns,". Not significant associations.<br>")
            str2 <- paste("<br><h4> The data code for <em>",Sel2(),"</em> is described <a",here,">here.</a>")
            HTML(paste(str1,str2))
          })
          ax <- list(
            title = "",
            zeroline = F,
            showline = F,
            showticklabels = F,
            showgrid = F)
          p<-plot_ly(Data, x = ~x , y = ~y , z = ~z, height = 650,# width = 1200,
                     marker = list(color = ~beta,colorbar=list(title='beta coefficient - (a.u.)',len = 0.55, x = 1.13, y = 0.4), 
                                   colorscale = c('#1972A4', '#FF7070'),
                                   cauto = F,
                                   cmin = -max(abs(Data$beta)),
                                   cmax = max(abs(Data$beta)),showscale = T)) %>%
            add_markers() %>%
            layout(scene = list(xaxis = ax,yaxis = ax, zaxis = ax, dragmode="turntable",
                                camera = list(eye = list(x = cos(3.3)*2, y = sin(3.3)*2, z= 0.23))))
          p
        } else {
          
          output$text <- renderUI({
            if (v$dotext == " ") return()
            str1 <- paste("<br><h4>Regional strength of associations between",phen()," and ", Sel2(),"with average beta coefficient = ",average_beta, "and significant area = ", significance_area, "%, in UKBB subjects, n = ",ns,".<br>")
            str2 <- paste("<br><h4> The data code for <em>",Sel2(),"</em> is described <a",here,">here.</a>")
            HTML(paste(str1,str2))
          })
          ax <- list(
            title = "",
            zeroline = F,
            showline = F,
            showticklabels = F,
            showgrid = F)
          p<-plot_ly(Data, x = ~x , y = ~y , z = ~z, height = 650, #width = 1200,
                     marker = list(color = "lightgrey",colorbar=list(title='beta coefficient - (a.u.)',len = 0.55, x = 1.00, y = 0.8), 
                                   colorscale = c('#1972A4', '#FF7070'),
                                   cauto = F,
                                   cmin = -max(abs(Data$beta))*6,
                                   cmax = max(abs(Data$beta))*6,showscale = F)) %>%
            add_markers() %>%
            layout(scene = list(xaxis = ax,yaxis = ax, zaxis = ax, dragmode="turntable",
                                camera = list(eye = list(x = cos(3.3)*2, y = sin(3.3)*2, z= 0.23))))
          
          p<- add_trace(p = p,
                        z = Data[pos,3],
                        x = Data[pos,1],
                        y = Data[pos,2],type = "scatter3d", mode = "markers",inherit = T,
                        marker = list(color =  Data[pos,4],colorbar=list(title='beta coefficient - (a.u.)',len = 0.55, x = 1.13, y = 0.4),
                                      colorscale = c('#1972A4', '#FF7070'),
                                      cauto= F,
                                      cmin = -max(abs(Data[pos,4])),
                                      cmax = max(abs(Data[pos,4])), showscale = T))%>%
            layout(showlegend = F)
          p
        }
      })
    })
  })
}

