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
                                      withMathJax(
                                      numericInput("nbin",
                                                   "Número De Repetições \\((n)\\)",
                                                   value = 10, 
                                                   min = 0)),
                                      uiOutput("XDaBin"),
                                      withMathJax(
                                      sliderInput("pbin", 
                                                  "Probabilidade de Sucesso \\((p)\\)",
                                                  min = 0,
                                                  max = 1,
                                                  value = 0.5))
                                      ),
############################ Sidebar POISSON ################################################
                     conditionalPanel(condition = "input.dist == 'poisson'",
                                      withMathJax(
                                      numericInput("lambdapois",
                                                   "Valor Esperado de Ocorrências \\((\\lambda)\\)",
                                                   value = 10,
                                                   min = 0)),
                                      withMathJax(
                                      numericInput("xpois",
                                                   "Número de Ocorrências Encontradas \\((x)\\)",
                                                   value = 10,
                                                   min = 0))
                                     ),
############################ Sidebar HIPERGEOMÉTRICA ################################################
                     conditionalPanel(condition = "input.dist == 'hyper'",
                                      withMathJax(
                                      numericInput("Nhyper",
                                                   "Total de objetos \\((N)\\)",
                                                   value = 100,
                                                   min = 1)),
                                      uiOutput("MDaHyper"),
                                      uiOutput("nDaHyper"),
                                      uiOutput("xDahyper")
                                      
                                      ),
############################ Sidebar GEOMÉTRICA ################################################
                     conditionalPanel(condition = "input.dist == 'geom'",
                                      withMathJax(numericInput("xgeo",
                                                   "Número de Repetições até o Sucesso \\((x)\\)",
                                                   value = 5, 
                                                   min = 1)),
                                      withMathJax(
                                      sliderInput("pgeo", 
                                                  "Probabilidade de Sucesso \\((p)\\)",
                                                  min = 0,
                                                  max = 1,
                                                  value = 0.5))     
                                      ),
############################ Sidebar BINOMIAL NEGATIVO ################################################
                     conditionalPanel(condition = "input.dist == 'nbinom'",
                                      withMathJax(numericInput("knb",
                                                   "Número De Sucessos \\((k)\\)",
                                                   value = 10, 
                                                   min = 1)),
                                      uiOutput("xDaNBin"),
                                      withMathJax(
                                      sliderInput("pnb", 
                                                  "Probabilidade de Sucesso \\((p)\\)",
                                                  min = 0,
                                                  max = 1,
                                                  value = 0.5))   
                                      )      
                    )),
################### main panel ###################
  mainPanel(
            
#### introdução das distribuições ####

tabsetPanel(
  tabPanel("Introdução",
#### Binomial ####
           conditionalPanel(condition = "input.dist == 'bin'",
                            withMathJax( p("Consideremos um experimento \\(\\varepsilon\\) e seja \\(A\\)  algum evento associado a \\(\\varepsilon \\). Admita-se que \\(P(A) = p\\) e conseqüentemente \\(P(\\bar{A}) = 1 - p\\). Considerem-se \\(n\\)  repetições de \\(\\varepsilon\\). Daí, o espaço amostral será formado por todas as seqüências possíveis \\(\\left \\{ a_{1}, a_{2}, ... a_{n} \\right \\}\\), onde cada \\(a_{i} \\)  é ou \\(A\\)  ou \\(\\bar{A}\\), dependendo de que tenha ocorrido \\(A\\)  ou \\(\\bar{A}\\)  na \\(i\\)-ésima repetição de \\(\\varepsilon\\). (Existem \\(2^{n}\\) dessas seqüências.) Além disso, suponha-se que \\(P(A) = p\\) permaneça a mesma para todas as repetições. A varivável aleatória \\(x\\)  será assim definida: \\(X\\) = número de Vezes que o evento \\(A\\) tenha ocorrido. Denominaremos \\(X\\) de variável aleatória binomial, com parâmetros \\(n\\)  e \\(p\\). Seus valores possíveis são evidentemente \\(0, 1, 2, ..., n\\). (De maneira equivalente, diremos que \\(X\\)  tem uma distribuição binomial.)")),
                            withMathJax(uiOutput("FuncaoBin")),
                            withMathJax(uiOutput("EspacoBin")),
                            withMathJax(uiOutput("EspBin")),
                            withMathJax(uiOutput("VariBin")),
                            withMathJax(uiOutput("MomBin"))
           ),
#### Hipergeometrica ####
          conditionalPanel(condition = "input.dist == 'hyper'",
                           withMathJax(p("Suponha-se que tenhamos um lote de \\(N \\) peças, \\( M \\) das quais sejam defeituosas e \\( (N - M) \\) das quais sejam não-defeituosas. Suponha-se que escolhamos, ao acaso, \\( n \\) peças desse lote \\( (n \\leq N) \\) sem reposição. Seja \\( X \\) o número de peças defeituosas encontradas. Desde que \\( X = x \\) se, e somente se, obtivermos exatamente \\( x \\) peças defeituosas (dentre as \\( M \\) defeituosas do lote) e exatamente \\( (n - x) \\) não-defeituosas dentre as \\( (N - M) \\) não-defeituosas do lote.")),
                           withMathJax(uiOutput("FuncaoHyper")),
                           withMathJax(uiOutput("EspacoHyper")),
                           withMathJax(uiOutput("EspHyper")),
                           withMathJax(uiOutput("VariHyper")),
                           withMathJax(uiOutput("MomHyper"))
          ),
#### Geometrica ####                          
          conditionalPanel(condition = "input.dist == 'geom'",
                           withMathJax(p("Suponha-se que realizemos um experimento \\(\\varepsilon\\) e que estejamos interessados apenas na ocorrência ou não-ocorrência de algum evente \\(A\\). Admita-se, tal como na explicação da distribuição binomial, que realizemos \\(\\varepsilon\\) repetidamente, que as repetições sejam independentes, e que em cada repetição \\(P(A) = p\\) e \\(P(\\bar{A}) = 1 - p \\) permaneçam os mesmos. Suponha-se que repetimos o experimento até que \\( A \\) ocorra pela primeira vez."), p("Defina-se a variável aleatória \\( X \\) como o número de falhas necessárias para obter a primeira ocorrência de \\( A \\), nele se incluindo essa última. Assim, \\( X \\) toma os valores possíveis \\( 0, 1, 2, ... \\) Como \\( X = x \\) se, e somente se, as primeiras \\( (x) \\) repetições de \\( \\varepsilon \\) deram o resultado \\( \\bar{A} \\), enquanto a \\( x+1 \\)-ésima repetição dê o resultado \\( A \\). ")),
                           withMathJax(uiOutput("FuncaoGeo")),
                           withMathJax(uiOutput("EspacoGeo")),
                           withMathJax(uiOutput("EspGeo")),
                           withMathJax(uiOutput("VariGeo")),
                           withMathJax(uiOutput("MomGeo"))
          ),
#### Poisson ####                          
            conditionalPanel(condition = "input.dist == 'poisson'",
                             withMathJax(p("A distribuição de Poisson é frequentemente usada para modelar o número de ocorrências de um evento por um certo período de tempo ou por um certo volume ou por uma certa área. Por exemplo, para descrever o número de nematóides encontrados em amostras em solo, número diário de novos casos de câncer de mama, ou o número  de células contadas usando um hemocitrômetro. A Distribuição de Poisson tem apenas um parâmentro, \\(\\lambda\\) que é interpretado como uma taxa média de ocorrências do evento.")),
                             withMathJax(uiOutput("FuncaoPoisson")),
                             withMathJax(uiOutput("EspacoPoisson")),
                             withMathJax(uiOutput("EspPoisson")),
                             withMathJax(uiOutput("VariPoisson")),
                             withMathJax(uiOutput("MomPoisson"))
                             ),
#### Binomial Negativo ####                          
            conditionalPanel(condition = "input.dist == 'nbinom'",
                             withMathJax(p("Suponha-se que um experimento seja continuado até que um particular evento \\(A\\) ocorra na \\( k \\)-ésima vez. Se \\( P(A) = p \\) e \\( P(\\bar{A}) = 1 - p \\) em cada repetição, definiremos a variável aleatória \\( X \\) como  o número de falhas necessárias a fim de que \\( A \\) possa ocorrer exatamente \\( k \\) vezes.")),
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
