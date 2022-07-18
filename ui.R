library(ggplot2)
library(plotly)
library(shiny)
library(DT)
library(statmod)

load("shinyPack.RData")
load("shinyPackTitration.RData")
load("metabTitr.RData")

navbarPage("Multi-omics assessment of protein restriction",
           tabPanel("Introduction",
                    fluidPage(
                      titlePanel("Profiling hepatic omic responses to low protein diets"
                      ),
                      #mainPanel(includeHTML("include.html"))
                      mainPanel(
                        p("PLACEHOLDER TEXT"),
                        img(src = "frontpage.jpg",
                            width = "100%")
                      )
                    )
           ),
          tabPanel("Strain by Sex RNAseq",
                    sidebarLayout(
                      sidebarPanel(width = 2,
                                   selectInput("DE_group1",
                                               label = "Choose DE Group 1",
                                               choices = c("Main effect: Sex" = "tab_male",
                                                           "Main effect: Strain" = "tab_dba",
                                                           "Main effect: Med prot" = "tab_diet14",
                                                           "Main effect: Low prot" = "tab_diet07", 
                                                           "Interaction: Sex x strain" = "tab_male_dba",
                                                           "Interaction: Sex x med prot" = "tab_male_diet14",
                                                           "Interaction: Sex x low prot" = "tab_male_diet07",
                                                           "Interaction: Strain x med prot" = "tab_diet14_dba",
                                                           "Interaction: Stain x low prot" = "tab_diet07_dba",
                                                           "Interaction: Sex x strain x med prot" = "tab_male_diet14_dba",
                                                           "Interaction: Sex x strain x low protein" = "tab_male_diet07_dba"),
                                               selected = "tab_diet07"
                                   ),
                                   selectInput("geneChoose",
                                               label = "Gene for DE Table Boxplot",
                                               choices = tab_diet07$symbol),
                                   p("To display a gene in the lower boxplot, click on a point
                                     on the volcano. Or a gene can be selected from the dropdown
                                     menu and displayed on the lower boxplot on the DE Tables tab.")
                      ),
                      mainPanel(
                        tabsetPanel(
                          tabPanel(
                            "Volcano Plot",
                            plotlyOutput('volcanoOut', height = 600),
                            plotlyOutput("DEbarplot1")
                          ),
                          tabPanel(
                            "DE Table",
                            DT::dataTableOutput("diffTable"),
                            plotlyOutput("DEbarplot2")
                          )
                        )
                      )
                    )
           ),
          tabPanel("Titration RNAseq",
                   sidebarLayout(
                     sidebarPanel(width = 2,
                                  selectInput("DE_group3",
                                              label = "Choose DE Group 1",
                                              choices = c("18%" = "p18",
                                                          "14%" = "p14",
                                                          "10%" = "p10",
                                                          "6%" = "p06", 
                                                          "2%" = "p02",
                                                          "0%" = "p00"),
                                              selected = "p18"
                                  ),
                                  selectInput("DE_group4",
                                              label = "Choose DE Group 2",
                                              choices = c("18%" = "p18",
                                                          "14%" = "p14",
                                                          "10%" = "p10",
                                                          "6%" = "p06", 
                                                          "2%" = "p02",
                                                          "0%" = "p00"),
                                              selected = "p00"
                                  ),
                                  selectInput("geneChoose2",
                                              label = "Gene for DE Table Boxplot",
                                              choices = p18_p00_top$symbol),
                                  p("To display a gene in the lower boxplot, click on a point
                                     on the volcano. Or a gene can be selected from the dropdown
                                     menu and displayed on the lower boxplot on the DE Tables tab.")
                     ),
                     mainPanel(
                       tabsetPanel(
                         tabPanel(
                           "Volcano Plot2",
                           plotlyOutput('volcanoOut2', height = 600),
                           plotlyOutput("DEbarplot3")
                         ),
                         tabPanel(
                           "DE Table2",
                           DT::dataTableOutput("diffTable2"),
                           plotlyOutput("DEbarplot4")
                         )
                       )
                     )
                   )
          ),
          tabPanel("Titration Metabolomics",
                   fluidRow(
                     column(2,
                            selectInput('metab', 'Metabolite', 
                                        colnames(dataNorm),
                                        selected = "serine"),
                            selectInput('metabDRPR', 'Metabolite', 
                                        colnames(dataNormDRPR),
                                        "Ophthalmic_acid")),
                     column(10,
                            plotOutput('metabPlot'),
                            plotOutput('metabPlotDRPR'))
                     )
                   )
          )

