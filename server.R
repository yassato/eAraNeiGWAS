#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(ggplot2)
library(patchwork)

# load top table
tab <- readRDS("./data/index_Japan.rds")
tab[,1:8] <- round(tab[,1:8],3)

# load description file
des = readRDS("./data/Araport11_genes.201606.transcript.rep_ERCC_Virus7457_GFP_GUS.rds")

# Define server logic required to show results of each gene
shinyServer(
  function(input, output) {

    output$table <- DT::renderDataTable( 
        tab, 
        filter="top",
        options=list(pageLength=50)
      )

    output$hist <- renderPlot({
      d <- readRDS(paste0("./data/",input$id,".rds"))
      p <- ggplot(d$rpmBLUP, aes(x=log2rpm1)) + 
        geom_histogram() + xlab(expression('log'[2]*'(rpm+1)')) + ylab("No. of individuals") +
        theme_classic()
      p
    })

    output$pve <- renderPlot({
      d <- readRDS(paste0("./data/",input$id,".rds"))
      p <- ggplot(as.data.frame(d$PVE), aes(x=factor(c(0,4,8)),y=pves)) + 
        geom_col() + xlab("No. of neighbors, J") + ylab("PVE") + ylim(0,1) +
        theme_classic()
      p
    })
    
    output$coexp_obs <- DT::renderDataTable({
      d <- readRDS(paste0("./data/",input$id,".rds"))
      obs = cbind(d$MR[,4],des[d$AGI_MR[,4],c(6,3)])
      colnames(obs)[1] = c("MR")
      obs }, 
      filter="top",
      options=list(pageLength=10)
    )
    
    output$coexp_s0 <- DT::renderDataTable({
      d <- readRDS(paste0("./data/",input$id,".rds"))
      s0 = cbind(d$MR[,1],des[d$AGI_MR[,1],c(6,3)])
      colnames(s0)[1] = c("MR")
      s0 }, 
      filter="top",
      options=list(pageLength=10)
    )
    
    output$coexp_s1 <- DT::renderDataTable({
      d <- readRDS(paste0("./data/",input$id,".rds"))
      s1 = cbind(d$MR[,2],des[d$AGI_MR[,2],c(6,3)])
      colnames(s1)[1] = c("MR")
      s1 }, 
      filter="top",
      options=list(pageLength=10)
    )
    
    output$coexp_s2 <- DT::renderDataTable({
      d <- readRDS(paste0("./data/",input$id,".rds"))
      s2 = cbind(d$MR[,3],des[d$AGI_MR[,3],c(6,3)])
      colnames(s2)[1] = c("MR")
      s2 }, 
      filter="top",
      options=list(pageLength=10)
    )
    
    output$insect <- renderPlot({
      d <- readRDS(paste0("./data/",input$id,".rds"))
      
      # load pheno
      pheno_2 = readRDS("./data/pheno.rds")
      
      # plots
      p = list()
      d2 = data.frame(d$rpmBLUP,Score=pheno_2[,"Score"],chewer=pheno_2[,"chewer"],sucker=pheno_2[,"sucker"])
      rho = cor(d2$log2rpm1,d2$Score,method="spearman")
      tmp = ggplot(d2,aes(x=log2rpm1,y=Score)) + labs(title="obs.",subtitle=paste0("rho = ",round(rho,3))) + geom_point(alpha=0.25) + 
        ylab("Leaf area loss") + xlab(expression(log[2]*(rpm+1))) + theme_classic()
      p = append(p,list(tmp))
      rho = cor(d2$log2rpm1,log(d2$chewer+1),method="spearman")
      tmp = ggplot(d2,aes(x=log2rpm1,y=log(chewer+1))) + labs(subtitle=paste0("rho = ",round(rho,3))) + geom_point(alpha=0.25) + 
        ylab("ln(No. of external feeders + 1)") + xlab(expression(log[2]*(rpm+1))) + theme_classic()
      p = append(p,list(tmp))
      rho = cor(d2$log2rpm1,log(d2$sucker+1),method="spearman")
      tmp = ggplot(d2,aes(x=log2rpm1,y=log(sucker+1))) + labs(subtitle=paste0("rho = ",round(rho,3))) + geom_point(alpha=0.25) + 
        ylab("ln(No. of internal feeders + 1)") + xlab(expression(log[2]*(rpm+1))) + theme_classic()
      p = append(p,list(tmp))
      
      for(i in colnames(d2)[2:4]) {
        rho = cor(d2[,i],d2$Score,method="spearman")
        tmp = ggplot(d2,aes_string(x=i,y=d2$Score)) + labs(subtitle=paste0("rho = ",round(rho,3))) + geom_point(alpha=0.25) + 
          ylab("Leaf area loss") + xlab("BLUP") + theme_classic()
        p = append(p,list(tmp))
        rho = cor(d2[,i],log(d2$chewer+1),method="spearman")
        tmp = ggplot(d2,aes_string(x=i,y=log(d2$chewer+1))) + labs(subtitle=paste0("rho = ",round(rho,3))) + geom_point(alpha=0.25) + 
          ylab("ln(No. of external feeders + 1)") + xlab("BLUP") + theme_classic()
        p = append(p,list(tmp))
        rho = cor(d2[,i],log(d2$sucker+1),method="spearman")
        tmp = ggplot(d2,aes_string(x=i,y=log(d2$sucker+1))) + labs(subtitle=paste0("rho = ",round(rho,3))) + geom_point(alpha=0.25) + 
          ylab("ln(No. of internal feeders + 1)") + xlab("BLUP") + theme_classic()
        p = append(p,list(tmp))
        
      }
      p[[1]]+(p[[4]]+ggtitle("J = 0"))+(p[[7]]+ggtitle("J = 4"))+(p[[10]]+ggtitle("J = 8"))+p[[2]]+p[[5]]+p[[8]]+p[[11]]+p[[3]]+p[[6]]+p[[9]]+p[[12]]
    })
    
    output$gwas_f <- renderText({
      f = try(system(paste0("ls ./data/figs/",input$id,"*.png"),intern=TRUE))
      name = try(substr(f[1],13,regexpr(".png",f)[1]-1))
      s = try(substr(f[1],regexpr(".png",f)[1]-1,regexpr(".png",f)[1]-1))
      try(switch(s,
                 "0" = s <- 0,
                 "1" = s <- 4,
                 "2" = s <- 8))
      name = try(substr(name,1,nchar(name)-2))
      name = try(paste0(name,"J",s))
      print(try(name))
    })
    
    output$gwas <- renderImage({
      f = try(system(paste0("ls ./data/figs/",input$id,"*.png"),intern=TRUE))
      list(src=f,
           contentType='image/png',
           height="600px")
    },deleteFile=FALSE)
    
    output$candidate <- DT::renderDataTable({
      f = try(system(paste0("ls ./data/*/top001_",input$id,".csv"),intern=TRUE))
      tab = try(read.delim(f,sep="\t",header=TRUE,as.is=TRUE))
      try(as.data.frame(tab[,2:9]))}, 
      filter="top",
      options=list(pageLength=50)
    )
    
})
