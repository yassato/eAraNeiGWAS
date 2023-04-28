#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    
    titlePanel("eAraNeiGWAS (Otsu, Japan)"),
    
    sidebarLayout(  
      sidebarPanel( 
        textInput("id",  
                  label = "Enter AGI code to display",  
                  value = "AT1G01920"),
        helpText(HTML("
        <i>Please note that some results may not be available for low-expressed genes. Error messages will occur for genes whose results are unavailable.</i> <br/>
                      ")
                 )
      ),
      mainPanel(   
        tabsetPanel(type = "tabs",
                    tabPanel("Summary", 
                             DT::dataTableOutput("table")),  
                    tabPanel("rpm",   
                             plotOutput("hist"),
                             downloadButton(outputId="data",label="Data")),
                    tabPanel("PVE",   
                             plotOutput("pve",height="500px",width = "80%")),
                    tabPanel("MR_obs", 
                             DT::dataTableOutput("coexp_obs")),
                    tabPanel("MR_J0", 
                             DT::dataTableOutput("coexp_s0")),
                    tabPanel("MR_J4", 
                             DT::dataTableOutput("coexp_s1")), 
                    tabPanel("MR_J12", 
                             DT::dataTableOutput("coexp_s2")),  
                    tabPanel("Insect",   
                             plotOutput("insect",height="600px")),
                    tabPanel("eGWAS",
                             textOutput("gwas_f"), 
                             imageOutput("gwas")),  
                    tabPanel("Candidates", 
                             DT::dataTableOutput("candidate")),
                    tabPanel("README", 
                             HTML("
        <b>Summary:</b> Summary table showing the average log2(rpm+1) among individuals (mean); its standard deviation (SD); PVE_0,1,2 corresponds to those at J = 0 (= genomic heritability, h^2), 4 (the first nearest neighbors), 12 (the second nearest neighbors); and Spearman's rank correlations with observed leaf damage (cor_dam), external feeders (cor_ex), or internal feeders (cor_in). <br/>
        <b>rpm:</b> Histogram of reads per milion. Click 'Data' button to download the data in a binary format (.rds), which can be reloaded using readRDS(), and contains log2(rpm+1) values as 'log2rpm1.' <br/>
        <b>PVE:</b> Proportion of expression variation explained at each scale. J = 0 corresponds to normal GWAS. Not displayed for genes with average log2(rpm+1)<1. <br/>
        <b>MR_obs:</b> Genes with the top 30 mutual ranks (MR) in the observed coexpressions. Not displayed for genes with average log2(rpm+1)<1. <br/>
        <b>MR_J0:</b> Genes with the top 30 mutual ranks (MR) in the co-expressions based on BLUPs at J = 0 (the same as normal GWAS). Not displayed for genes with average log2(rpm+1)<1. <br/>
        <b>MR_J4:</b> Genes with the top 30 mutual ranks (MR) in the co-expressions based on BLUPs at J = 4 (the nearest neighbors). Not displayed for genes with average log2(rpm+1)<1. <br/>
        <b>MR_J12:</b> Genes with the top 30 mutual ranks (MR) in the co-expressions based on BLUPs at J = 12 (the second nearest neighbors). Not displayed for genes with average log2(rpm+1)<1. <br/>
        <b>Insect:</b> Biplots showing the relationships between gene expression levels (observed or predicted levels with BLUPs) and insect herbivory. Spearman's rank correlation, rho, is shown above each panel. Not displayed for genes with average log2(rpm+1)<1. <br/>
        <b>eGWAS:</b> The y-axis indicates observed -log10(p). The x-axis of the Manhattan plot indicates the genomic position from chr1 to chr5. The x-axis of the QQ plot indicates the expected -log10(p). The upper panels show the results of the focal genotype effects at J = 0 (= normal GWAS). The lower panels show the results of neighbor effects (J = 4 or 12). Displayed only for genes with the top 1% PVE. <br/>
        <b>Candidate:</b> List of candidate genes 10-kb near the top 0.01% SNPs of the eGWAS. The results at J = 4 or 12 are shown. Displayed only for genes with the top 1% PVE.
                      "))

        )
      )
    )
  ))
