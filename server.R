library(ggplot2)
library(plotly)
library(shiny)
library(DT)
library(statmod)

load("shinyPack.RData")
load("shinyPackTitration.RData")
load("metabTitr.RData")

myTheme <- theme(panel.background = element_blank(),
                 axis.line = element_line(color = "black"),
                 text = element_text(color = 'black', size = 12),
                 legend.key=element_blank())

function(input, output, session){
  
  makeDEtab <- reactive({
    
    comparison <- try(get(paste(as.character(input$DE_group1))))
    
    if (inherits(comparison, "try-error")) {
      return(get(
        paste(as.character(input$DE_group1))
      ))
    } else {
      return(get(
        paste(as.character(input$DE_group1))
      ))
    }
    
  })
  
  
  output$volcanoOut <- renderPlotly({
    p <- ggplot(data = makeDEtab(),
                aes(x = logFC, y = -log10(adj.P.Val),
                    text = paste("Gene:", make.names(symbol)),
                    key = genes,
                    color = adj.P.Val < 0.05)) +
      geom_point(alpha = 0.35) +
      scale_color_manual(values = c("#FFF294", "#2f2ff5")) +
      labs(x = "Log2 fold change", y = "-log10 q value", color = "Sig") + myTheme
    
    p <- ggplotly(p, source = "nVolcano")
    
    print(p)
  })
  
  output$DEbarplot1 <- renderPlotly({
    
    event.data <- event_data("plotly_click",
                             source = "nVolcano")
    if (is.null(event.data)){
      p <- ggplot(cpmX, aes(x = dietGroup, y = ENSMUSG00000028603, fill = strainDietGroup)) +
        geom_boxplot() + labs(x = "Diet") + facet_grid(~sexGroup+strainGroup) +
        scale_fill_manual(values = c("#565A5C", "#F26F17", "#5AB5DA",
                                     "#FFFFFF", "#FE0000", "#0088AA")) +
        myTheme + guides(fill = F)
      p <- ggplotly(p)
    } else {
      p <- ggplot(cpmX, aes_string(x = "dietGroup", y = event.data$key, fill = "strainDietGroup")) +
        geom_boxplot() + labs(x = "Diet") + facet_grid(~sexGroup+strainGroup) +
        scale_fill_manual(values = c("#565A5C", "#F26F17", "#5AB5DA",
                                     "#FFFFFF", "#FE0000", "#0088AA")) +
        myTheme + guides(fill = F)
      p <- ggplotly(p)
    }
    
  })
  
  output$DEbarplot2 <- renderPlotly({
    
    selFrame <- subset(tab_diet07, symbol == input$geneChoose)
    selEns <- selFrame$genes
    
    p <- ggplot(cpmX, aes_string(x = "dietGroup", y = selEns, fill = "strainDietGroup")) +
      geom_boxplot() + labs(x = "Diet") + facet_grid(~sexGroup+strainGroup) +
      scale_fill_manual(values = c("#565A5C", "#F26F17", "#5AB5DA",
                                   "#FFFFFF", "#FE0000", "#0088AA")) +
      myTheme + guides(fill = F)
    
  })
  
  output$diffTable <- DT::renderDataTable(DT::datatable({
    
    makeDEtab()
    
  }))
  
  makeDEtab2 <- reactive({
    
    comparison <- try(get(paste(as.character(input$DE_group3),
                                "_", 
                                as.character(input$DE_group4),
                                "_top", sep = "")))
    
    if (inherits(comparison, "try-error")) {
      return(get(
        paste(as.character(input$DE_group4),
              "_", 
              as.character(input$DE_group3),
              "_top", sep = "")
      ))
    } else {
      return(get(
        paste(as.character(input$DE_group3),
              "_", 
              as.character(input$DE_group4),
              "_top", sep = "")
      ))
    }
    
  })
  
  output$volcanoOut2 <- renderPlotly({
    p <- ggplot(data = makeDEtab2(),
                aes(x = logFC, y = -log10(adj.P.Val),
                    text = paste("Gene:", make.names(symbol)),
                    key = genes,
                    color = adj.P.Val < 0.05)) +
      geom_point(alpha = 0.35) +
      scale_color_manual(values = c("#FFF294", "#2f2ff5")) +
      labs(x = "Log2 fold change", y = "-log10 q value", color = "Sig") + myTheme
    
    p <- ggplotly(p, source = "nVolcano2")
    
    print(p)
  })
  
  output$DEbarplot3 <- renderPlotly({
    
    event.data <- event_data("plotly_click",
                             source = "nVolcano2")
    if (is.null(event.data)){
      p <- ggplot(cpmFrameTitr, aes(x = txGroup, y = ENSMUSG00000028603, fill = txGroup)) +
        geom_boxplot() + labs(x = "Diet") +
        scale_fill_brewer(palette = "RdYlBu", direction = -1) +
        myTheme + guides(fill = F)
      p <- ggplotly(p)
    } else {
      p <- ggplot(cpmFrameTitr, aes_string(x = "txGroup", y = event.data$key, fill = "txGroup")) +
        geom_boxplot() + labs(x = "Diet") +
        scale_fill_brewer(palette = "RdYlBu", direction = -1) +
        myTheme + guides(fill = F)
      p <- ggplotly(p)
    }
    
  })
  
  output$DEbarplot4 <- renderPlotly({
    
    selFrame <- subset(p18_p00_top, symbol == input$geneChoose2)
    selEns <- selFrame$genes
    
    p <- ggplot(cpmFrameTitr, aes_string(x = "txGroup", y = selEns, fill = "txGroup")) +
      geom_boxplot() + labs(x = "Diet") +
      scale_fill_brewer(palette = "RdYlBu", direction = -1) +
      myTheme + guides(fill = F)
    
  })
  
  output$diffTable2 <- DT::renderDataTable(DT::datatable({
    
    makeDEtab2()
    
  }))
  
  output$metabPlot <- renderPlot({
    
    p <- ggplot(data = dataNorm, aes_string(x="expGroup", y=input$metab, group = "expGroup")) + 
      geom_boxplot(aes_string(fill = "expGroup")) +
      labs(x = NULL) + myTheme + 
      scale_fill_brewer(palette = "RdYlBu", direction = -1) 
    
    print(p)
    
  })
  
  output$metabPlotDRPR <- renderPlot({
    
    q <- ggplot(data = dataNormDRPR, aes_string(x = "Diet", y = input$metabDRPR,
                                                group = "Diet")) +
      geom_boxplot(aes_string(fill = "Diet")) +
      scale_fill_manual(values = c("#4575b4", "#d73027", "#42e0e3", "#f38f8a")) +
      labs(x = NULL) + myTheme
    
    print(q)
    
  })
  
  
  
}