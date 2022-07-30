library(ggplot2)
library(plotly)
library(shiny)
library(DT)
library(statmod)
library(shinyjs)
library(heatmaply)

load("shinyPack.RData")
load("shinyPackTitration.RData")
load("metabTitr.RData")
dataNorm2 <- read.csv("greenMetabolomicsRaw.csv")

navbarPage("Multi-omics assessment of protein restriction",
           useShinyjs(),
           extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('plotly_click-nVolcano', 'null'); }",
                         functions = c("resetClick")),
           extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('plotly_click-nVolcano2', 'null'); }",
                         functions = c("resetClick2")),
           tabPanel("Introduction",
                    fluidPage(
                      titlePanel("Profiling hepatic omic responses to low protein diets"),
                      #mainPanel(includeHTML("include.html"))
                      mainPanel(
                        tags$h4(tags$a(href = "https://www.sciencedirect.com/science/article/pii/S1550413121006379",
                                       "Green, Lamming et al.", target="_blank")),
                        tags$h4(tags$a(href = "https://macarthur.shinyapps.io/kdshiny/",
                                       "MacArthur, Mitchell et al.", target="_blank")),
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
                                   actionButton("reset", "Plot selected gene"),
                                   p("To change the lower boxplot, either click a point
                                     on the volcano plot OR select a gene from the dropdown 
                                     menu then click the button below")
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
                                  actionButton("reset2", "Plot selected gene"),
                                  p("To change the lower boxplot, either click a point
                                     on the volcano plot OR select a gene from the dropdown 
                                     menu then click the button below")
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
          tabPanel("Strain by Sex Metabolomics",
                   fluidRow(
                     column(2,
                            selectInput("Aov_group1",
                                        label = "Choose ANOVA Group",
                                        choices = c("Main effect: Sex" = "sexLs",
                                                    "Main effect: Strain" = "strainLs",
                                                    "Main effect: Diet" = "dietLs",
                                                    "Interaction: Sex x strain" = "sexStrainLs",
                                                    "Interaction: Sex x diet" = "sexDietLs",
                                                    "Interaction: Strain x diet" = "strainDietLs",
                                                    "Interaction: Sex x strain x diet" = "sexStrainDietLs"),
                                        selected = "sexLs"
                            ),
                            selectInput('metab2', 'Metabolite', 
                                        colnames(dataNorm2),
                                        selected = "Serine")),
                     column(10,
                            plotOutput('metabPlot2'),
                            plotlyOutput('metabHeat2'),
                            verbatimTextOutput('aovLs')
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
                            plotOutput('metabPlotDRPR'),
                            plotlyOutput('metabHeat1'))
                     )
                   )
          )


