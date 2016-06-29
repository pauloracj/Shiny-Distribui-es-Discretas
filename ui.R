library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Distribuições de Probabilidade"),
  sidebarPanel(
    selectInput("mod", "Tipo de Distribuição:",
                choices = c(Null     = "null",
                            Discreta = "disc")),
    conditionalPanel(condition = "input.mod == 'disc'",
                     selectInput("dist", "Distribuicao:", choices = c(Null = "null",
                                                                      Binomial = "bin", 
                                                                      Poisson = "poisson",
                                                                      Hipergeométrica = "hyper",
                                                                      Geométrica = "geom",
                                                                      BinomialNegativo = "nbinom"),
                     selectize = FALSE,selected = NULL),
############################ Sidebar BINOMIAL ################################################                     
                     conditionalPanel(condition = "input.dist == 'bin'",
                                      numericInput("nbin",
                                                   "Quantidade De Repetições",
                                                   value = 10, 
                                                   min = 0),
                                      uiOutput("XDaBin"),
                                      sliderInput("pbin", 
                                                  "Probabilidade de Sucesso",
                                                  min = 0,
                                                  max = 1,
                                                  value = 0.5)
                                      ),
############################ Sidebar POISSON ################################################
                     conditionalPanel(condition = "input.dist == 'poisson'",
                                      numericInput("lambdapois",
                                                   "Valor Esperado de Ocorrências",
                                                   value = 10,
                                                   min = 0),
                                      numericInput("xpois",
                                                   "Número de Ocorrências Encontradas",
                                                   value = 10,
                                                   min = 0)
                                     ),
############################ Sidebar HIPERGEOMÉTRICA ################################################
                     conditionalPanel(condition = "input.dist == 'hyper'",
                                      numericInput("Nhyper",
                                                   "Total de objetos",
                                                   value = 100,
                                                   min = 1),
                                      uiOutput("MDaHyper"),
                                      uiOutput("nDaHyper"),
                                      uiOutput("xDahyper")
                                      ),
############################ Sidebar GEOMÉTRICA ################################################
                     conditionalPanel(condition = "input.dist == 'geom'",
                                      numericInput("xgeo",
                                                   "Número de Vezes até o Sucesso",
                                                   value = 5, 
                                                   min = 1),
                                      sliderInput("pgeo", 
                                                  "Probabilidade de Sucesso",
                                                  min = 0,
                                                  max = 1,
                                                  value = 0.5)     
                                      ),
############################ Sidebar BINOMIAL NEGATIVO ################################################
                     conditionalPanel(condition = "input.dist == 'nbinom'",
                                      numericInput("knb",
                                                   "Quantidade De Sucessos",
                                                   value = 10, 
                                                   min = 1),
                                      uiOutput("xDaNBin"),
                                      sliderInput("pnb", 
                                                  "Probabilidade de Sucesso",
                                                  min = 0,
                                                  max = 1,
                                                  value = 0.5)   
                                      )      
                    )),
################### main panel ###################
  mainPanel(
            
#### introdução das distribuições ####

tabsetPanel(
  tabPanel("Introdução",
#### Binomial ####
           conditionalPanel(condition = "input.dist == 'bin'",
                            withMathJax( p("Consideremos um experimento \\(\\LARGE{\\varepsilon}\\) e seja \\(\\LARGE{A}\\)  algum evento associado a \\(\\LARGE{ \\varepsilon }\\). Admita-se que \\(\\LARGE{ P(A) = p}\\) e conseqüentemente \\(\\LARGE{P(\\bar{A}) = 1 - p}\\). Considerem-se \\(\\LARGE{n}\\)  repetições de \\(\\LARGE{\\varepsilon}\\). Daí, o espaço amostral será formado por todas as seqüências possíveis \\(\\LARGE{\\left \\{ a_{1}, a_{2}, ... a_{n} \\right \\}}\\), onde cada \\(\\LARGE{a_{i} }\\)  é ou \\(\\LARGE{A}\\)  ou \\(\\LARGE{\\bar{A}}\\), dependendo de que tenha ocorrido \\(\\LARGE{A}\\)  ou \\(\\LARGE{\\bar{A}}\\)  na \\(\\LARGE{i}\\)-ésima repetição de \\(\\LARGE{\\varepsilon}\\). (Existem \\(\\LARGE{2^{n}}\\) dessas seqüências.) Além disso, suponha-se que \\(\\LARGE{P(A) = p}\\) permaneça a mesma para todas as repetições. A varivável aleatória \\(\\LARGE{x}\\)  será assim definida: \\(\\LARGE{X}\\) = número de Vezes que o evento \\(\\LARGE{A}\\) tenha ocorrido. Denominaremos \\(\\LARGE{X}\\) de variável aleatória binomial, com parâmetros \\(\\LARGE{n}\\)  e \\(\\LARGE{p}\\). Seus valores possíveis são evidentemente \\(\\LARGE{0, 1, 2, ..., n}\\). (De maneira equivalente, diremos que \\(\\LARGE{X}\\)  tem uma distribuição binomial.)")),
                            withMathJax(uiOutput("FuncaoBin")),
                            withMathJax(uiOutput("EspacoBin")),
                            withMathJax(uiOutput("EspBin")),
                            withMathJax(uiOutput("VariBin")),
                            withMathJax(uiOutput("MomBin"))
           ),
#### Hipergeometrica ####
          conditionalPanel(condition = "input.dist == 'hyper'",
                           withMathJax(p("Suponha-se que tenhamos um lote de \\(\\LARGE{ N }\\) peças, \\(\\LARGE{ M }\\) das quais sejam defeituosas e \\(\\LARGE{ (N - M) }\\) das quais sejam não-defeituosas. Suponha-se que escolhamos, ao acaso, \\(\\LARGE{ n }\\) peças desse lote \\(\\LARGE{ (n \\leq N) }\\) sem reposição. Seja \\(\\LARGE{ X }\\) o número de peças defeituosas encontradas. Desde que \\(\\LARGE{ X = x }\\) se, e somente se, obtivermos exatamente \\(\\LARGE{ x }\\) peças defeituosas (dentre as \\(\\LARGE{ M }\\) defeituosas do lote) e exatamente \\(\\LARGE{ (n - x) }\\) não-defeituosas dentre as \\(\\LARGE{ (N - M) }\\) não-defeituosas do lote.")),
                           withMathJax(uiOutput("FuncaoHyper")),
                           withMathJax(uiOutput("EspacoHyper")),
                           withMathJax(uiOutput("EspHyper")),
                           withMathJax(uiOutput("VariHyper")),
                           withMathJax(uiOutput("MomHyper"))
          ),
#### Geometrica ####                          
          conditionalPanel(condition = "input.dist == 'geom'",
                           withMathJax(p("Suponha-se que realizemos um experimento \\(\\LARGE{\\varepsilon}\\) e que estejamos interessados apenas na ocorrência ou não-ocorrência de algum evente \\(\\LARGE{A}\\). Admita-se, tal como na explicação da distribuição binomial, que realizemos \\(\\LARGE{ \\varepsilon }\\) repetidamente, que as repetições sejam independentes, e que em cada repetição \\(\\LARGE{ P(A) = p }\\) e \\(\\LARGE{ P(\\bar{A}) = 1 - p }\\) permaneçam os mesmos. Suponha-se que repetimos o experimento até que \\(\\LARGE{ A }\\) ocorra pela primeira vez."), p("Defina-se a variável aleatória \\(\\LARGE{ X }\\) como o número de repetições necessárias para obter a primeira ocorrência de \\(\\LARGE{ A }\\), nele se incluindo essa última. Assim, \\(\\LARGE{ X }\\) toma os valores possíveis \\(\\LARGE{ 1, 2, ... }\\) Como \\(\\LARGE{ X = x }\\) se, e somente se, as primeiras \\(\\LARGE{ (x - 1) }\\) repetições de \\(\\LARGE{ \\varepsilon }\\) deram o resultado \\(\\LARGE{ \\bar{A} }\\), enquanto a \\(\\LARGE{ k }\\)-ésima repetição dê o resultado \\(\\LARGE{ A }\\). ")),
                           withMathJax(uiOutput("FuncaoGeo")),
                           withMathJax(uiOutput("EspacoGeo")),
                           withMathJax(uiOutput("EspGeo")),
                           withMathJax(uiOutput("VariGeo")),
                           withMathJax(uiOutput("MomGeo"))
          ),
#### Poisson ####                          
            conditionalPanel(condition = "input.dist == 'poisson'",
                             withMathJax(p("A distribuição de Poisson é frequentemente usada para modelar o número de ocorrências de um evento por um certo período de tempo ou por um certo volume ou por uma certa área. Por exemplo, para descrever o número de nematóides encontrados em amostras em solo, número diário de novos casos de câncer de mama, ou o número  de células contadas usando um hemocitrômetro. A Distribuição de Poisson tem apenas um parâmentro, \\(\\LARGE{\\lambda}\\) que é interpretado como uma taxa média de ocorrências do evento.")),
                             withMathJax(uiOutput("FuncaoPoisson")),
                             withMathJax(uiOutput("EspacoPoisson")),
                             withMathJax(uiOutput("EspPoisson")),
                             withMathJax(uiOutput("VariPoisson")),
                             withMathJax(uiOutput("MomPoisson"))
                             ),
#### Binomial Negativo ####                          
            conditionalPanel(condition = "input.dist == 'nbinom'",
                             withMathJax(p("Suponha-se que um experimento seja continuado até que um particular evento \\(\\LARGE{A}\\) ocorra na \\(\\LARGE{ n }\\)-ésima vez. Se \\(\\LARGE{ P(A) = p }\\) e \\(\\LARGE{ P(\\bar{A}) = 1 - p }\\) em cada repetição, definiremos a variável aleatória \\(\\LARGE{ X }\\) como  o número de repetições necessárias a fim de que \\(\\LARGE{ A }\\) possa ocorrer exatamente \\(\\LARGE{ n }\\) vezes.")),
                             withMathJax(uiOutput("FuncaoNegaBinom")),
                             withMathJax(uiOutput("EspacoNegaBinom")),
                             withMathJax(uiOutput("EspNegaBinom")),
                             withMathJax(uiOutput("VariNegaBinom")),
                             withMathJax(uiOutput("MomNegaBinom"))
                             )
            ),
######################### Barra de Caracteristicas #######################3
            tabPanel("Características",
                        
##################################### caracteristicas binomial ############################################################3    
                                                          conditionalPanel(condition = "input.dist == 'bin'",
                                                                           textOutput("mediabin"),
                                                                           textOutput("varbin"),
                                                                           textOutput("ProbPontoBin"),
                                                                           textOutput("ProbCoBin"),
                                                                           textOutput("ProbAcuBin"),
                                                                           textOutput("ProbCoAcuBin"),
                                                                           plotOutput("DistBin"),
                                                                           plotOutput("AcuBin")
                                                          ),
########################################################## Caracteristicas Hipergeometrica #####################################################################3
                                                          conditionalPanel(condition = "input.dist == 'hyper'",
                                                                           textOutput("mediahyper"),
                                                                           textOutput("varhyper"),
                                                                           textOutput("ProbPontoHyper"),
                                                                           textOutput("ProbCoHyper"),
                                                                           textOutput("ProbAcuHyper"),
                                                                           textOutput("ProbCoAcuHyper"),
                                                                           plotOutput("DistHyper"),
                                                                           plotOutput("AcuHyper")
                                                          ),
####################################################### caracteristicas Poisson ##########################################################################
                                                          conditionalPanel(condition = "input.dist == 'poisson'",
                                                                           textOutput("mediapois"),
                                                                           textOutput("varpois"),
                                                                           textOutput("ProbPontoPois"),
                                                                           textOutput("ProbCoPois"),
                                                                           textOutput("ProbAcuPois"),
                                                                           textOutput("ProbCoAcuPois"),
                                                                           plotOutput("DistPois"),
                                                                           plotOutput("AcuPois")
                                                          ),
##################################################### caracteristicas Geometrica ###################################################################3
                                                          conditionalPanel(condition = "input.dist == 'geom'",
                                                                           textOutput("mediageo"),
                                                                           textOutput("vargeo"),
                                                                           textOutput("ProbPontoGeo"),
                                                                           textOutput("ProbCoGeo"),
                                                                           textOutput("ProbAcuGeo"),
                                                                           textOutput("ProbCoAcuGeo"),
                                                                           plotOutput("DistGeo"),
                                                                           plotOutput("AcuGeo")
                                                          ), 
################################################# caracteristicas Binomial negativo ############################################################33
                                                          conditionalPanel(condition = "input.dist == 'nbinom'",
                                                                           textOutput("medianbin"),
                                                                           textOutput("varnbin"),
                                                                           textOutput("ProbPontoNBin"),
                                                                           textOutput("ProbCoNBin"),
                                                                           textOutput("ProbAcuNBin"),
                                                                           textOutput("ProbCoAcuNBin"),
                                                                           plotOutput("DistNBin"),
                                                                           plotOutput("AcuNBin")
                                                          )
               
            )

            ))
))
