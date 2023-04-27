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
                             plotOutput("hist")),  
                    tabPanel("PVE",   
                             plotOutput("pve",height="500px",width = "80%")),
                    tabPanel("MR_obs", 
                             DT::dataTableOutput("coexp_obs")),
                    tabPanel("MR_J0", 
                             DT::dataTableOutput("coexp_s0")),
                    tabPanel("MR_J4", 
                             DT::dataTableOutput("coexp_s1")), 
                    tabPanel("MR_J8", 
                             DT::dataTableOutput("coexp_s2")),  
                    tabPanel("Insect",   
                             plotOutput("insect",height="600px")),
                    tabPanel("eGWAS",
                             textOutput("gwas_f"), 
                             imageOutput("gwas")),  
                    tabPanel("Candidates", 
                             DT::dataTableOutput("candidate")),
                    tabPanel("Help", 
                             HTML("
        <b>Summary:</b> Summary table showing average of log2(rpm+1) among individuals (mean); its standard deviation (SD); PVE_0,1,2 corresponds to those at J = 0 (= genomic heritability, h^2), 4 (the first nearest neighbors), 8 (the second nearest neighbors); and Spearman's rank correlations with observed leaf damage (cor_dam), external feeders (cor_ex), or internal feeders (cor_in). <br/>
        <b>rpm:</b> histogram of reads per milion. <br/>
        <b>PVE:</b> Proportion of expression variation explained at each scale. J = 0 corresponds to normal GWAS. <br/>
        <b>MR_obs:</b> Genes having the top 30 mutual rank (MR) in the observed coexpressions. <br/>
        <b>MR_J0:</b> Genes having the top 30 mutual rank (MR) in the coexpressions based on BLUPs at J = 0 (the same as normal GWAS). <br/>
        <b>MR_J4:</b> Genes having the top 30 mutual rank (MR) in the coexpressions based on BLUPs at J = 4 (the nearest neighbors). <br/>
        <b>MR_J8:</b> Genes having the top 30 mutual rank (MR) in the coexpressions based on BLUPs at J = 8 (the second nearest neighbors). <br/>
        <b>Insect:</b> Biplots showing relationships between gene expression levels (observed levels or predicted levels with BLUPs) and insect herbivory. Spearman's rank correlation, rho, is shown above each panel. <br/>
        <b>eGWAS:</b> Y-axes indicate observed -log10(p). X-axis of the Manhattan plot indicates genomic position from chr1 to chr5. X-axis of the QQ-plot indicates expected -log10(p). Upper panels show the results of self-genotype effects at J = 0 (= normal GWAS). Lower panels show the results of neighbor effects (J = 4 or 8). <br/>
        <b>Candidate:</b> List of candidate genes 10-kb near the top 0.01% SNPs of eGWAS. Results at J = 4 or 8 are shown.
                      "))

        )
      )
    )
  ))
