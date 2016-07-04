library(shiny)
shinyServer(function(input, output) {

#############################################################################################################################################################
   
######## condições dos parametros da distribuição binomial ##########
  output$XDaBin <- renderUI(withMathJax(
                            numericInput("xbin",
                                         "Número De Sucessos \\((x)\\)",
                                         value = 5, 
                                         min = 0,
                                         max = input$nbin)))
######## condições dos parametros da distribuição hipergeometrica ##########  
  output$MDaHyper <- renderUI(withMathJax(numericInput("Mhyper",
                                           "Número total de objetos com a caracteristica estudada \\((M)\\)",
                                           value = 10,
                                           min = 0,
                                           max = input$Nhyper)))
  output$nDaHyper <- renderUI(withMathJax(numericInput("nhyper",
                                           "Tamanho da Amostra dos objetos \\((n)\\)",
                                            value = 10,
                                            min = 2,
                                            max = input$Nhyper)))
  output$xDahyper <- renderUI(withMathJax(numericInput("xhyper",
                                           "Número de objetos com a caracteristica encontrados na amostra \\((x)\\)",
                                           value = 0,
                                           min = (max(0,(input$nhyper - (input$Nhyper - input$Mhyper)))),
                                           max = (min(input$Mhyper,input$nhyper)))))
######## condições dos parametros da distribuição Binomial Negativo ########## 
  output$xDaNBin <- renderUI(withMathJax(numericInput("xnb",
                                          "Número de tentativas até o k-ésimo sucesso \\((x)\\)",
                                          value = input$knb,
                                          min = input$knb)))
  
###################################################################################################################
  
############ calculos binomial ###############
  output$mediabin <- renderText({c("E(X) = ", 
                                 ceiling(input$nbin*input$pbin))})
  output$varbin <- renderText({c("Var(X) = ", 
                                 round(input$nbin*input$pbin*(1-input$pbin),2))})
  output$ProbPontoBin <- renderText({c("P(X = ", input$xbin,") = " ,
                                       round(dbinom(input$xbin,input$nbin,input$pbin),2))})
  output$ProbCoBin <- renderText({paste("1 - P(X = ", input$xbin,") = " ,
                                       round(1 - dbinom(input$xbin,input$nbin,input$pbin),2))})
  output$ProbAcuBin <- renderText({paste("F(X = " , input$xbin,") = " ,
                                         round(pbinom(input$xbin,input$nbin,input$pbin),2))})
  output$ProbCoAcuBin <- renderText({paste("S(X = " , input$xbin,") = " ,
                                           round(1 - pbinom(input$xbin,input$nbin,input$pbin),2))})
  output$DistBin <- renderPlot(plot(0:input$nbin,dbinom(0:input$nbin,input$nbin,input$pbin),type = 'h',
                                    xlab = "X", ylab= "f(x)", main = "Gráfico da Distribuição", lwd = 2))
  output$AcuBin <- renderPlot(plot(0:input$nbin,pbinom(0:input$nbin,input$nbin,input$pbin),type = 'S', 
                                   xlab = "X", ylab = "F(x)", main = "Gráfico de Distribuição Acumulada", 
                                   lwd = 2 ))

############ calculos poisson ###############
  output$mediapois <- renderText({c("E(X) = ",input$lambdapois)})
  output$varpois <- renderText({c("Var(X) = ",input$lambdapois)})
  output$ProbPontoPois <- renderText({c("P(X = ", input$xpois,") = " ,
                                       round(dpois(input$xpois,input$lambdapois),2))})
  output$ProbCoPois <- renderText({c("1 - P(X = ", input$xpois,") = " ,
                                        round(1 - dpois(input$xpois,input$lambdapois),2))})
  output$ProbAcuPois <- renderText({paste("F(X = " , input$xpois,") = " ,
                                         round(ppois(input$xpois,input$lambdapois),2))})
  output$ProbCoAcuPois <- renderText({paste("S(X = " , input$xbin,") = " ,
                                           round(1 - ppois(input$xpois,input$lambdapois),2))})
  output$DistPois <- renderPlot(plot(0:input$xpois, dpois(0:input$xpois,input$lambdapois),type = 'h',
                                    xlab = "X", ylab= "f(x)", main = "Gráfico da Distribuição", lwd = 2))
  output$AcuPois <- renderPlot(plot(0:input$xpois,ppois(0:input$xpois,input$lambdapois),type = 'S', 
                                   xlab = "X", ylab = "F(x)", main = "Gráfico de Distribuição Acumulada", 
                                   lwd = 2 ))
############ media variancia Geométrica ###############
  output$mediageo <- renderText({c("E(X) = ",
                                 ceiling((1-input$pgeo)/input$pgeo))})
  output$vargeo <- renderText({c("Var(X) = ",
                                 round((1-input$pgeo)/(input$pgeo^2),
                                       2))})
  output$ProbPontoGeo <- renderText({c("P(X = ", input$xgeo,") = " ,
                                       round((dgeom(input$xgeo ,input$pgeo)),2))})
  output$ProbCoGeo <- renderText({paste("1 - P(X = ", input$xgeo,") = " ,
                                        round(1 - (dgeom(input$xgeo ,input$pgeo)),2))})
  output$ProbAcuGeo <- renderText({paste("F(X = " , input$xgeo,") = " ,
                                         round(pgeom(input$xgeo - 1,input$pgeo),2))})
  output$ProbCoAcuGeo <- renderText({paste("S(X = " , input$xgeo,") = " ,
                                           round(1 - pgeom(input$xgeo ,input$pgeo),2))})
  output$DistGeo <- renderPlot(plot(0:input$xgeo,dgeom(0:input$xgeo,input$pgeo),type = 'h',
                                    xlab = "X", ylab= "f(x)", main = "Gráfico da Distribuição", lwd = 2))
  output$AcuGeo <- renderPlot(plot(0:input$xgeo,pgeom(0:input$xgeo,input$pgeo),type = 'S', 
                                   xlab = "X", ylab = "F(x)", main = "Gráfico de Distribuição Acumulada", 
                                   lwd = 2 ))
############ media variancia Binomial Negativo ###############
  output$medianbin <- renderText({c("E(X) = ",
                                    ceiling(input$knb*input$pnb/input$pnb))})
  output$varnbin <- renderText({c("Var(X) = ",
                                  round((input$knb*(1-input$pnb))/(input$pnb^2),2))})
  output$ProbPontoNBin <- renderText({c("P(X = ", input$xnb,") = " ,
                                        round(dnbinom(input$xnb,input$knb,input$pnb),2))})
  output$ProbCoNBin <- renderText({paste("1 - P(X = ", input$xnb,") = " ,
                                         round(1 - dnbinom(input$xnb,input$knb, input$pnb),2))})
  output$ProbAcuNBin <- renderText({paste("F(X = " , input$xnb,") = " ,
                                          round(pnbinom(input$xnb,input$knb, input$pnb),2))})
  output$ProbCoAcuNBin <- renderText({paste("S(X = " , input$xnb,") = " ,
                                            round(1 - pnbinom(input$xnb, input$knb,input$pnb),2))})
  output$DistNBin <- renderPlot(plot(0:input$xnb,dnbinom(0:input$xnb, input$knb,input$pnb),type = 'h',
                                     xlab = "X", ylab= "f(x)", main = "Gráfico da Distribuição", lwd = 2))
  output$AcuNBin <- renderPlot(plot(0:input$xnb,pnbinom(0:input$xnb,input$knb,input$pnb),type = 'S', 
                                    xlab = "X", ylab = "F(x)", main = "Gráfico de Distribuição Acumulada", 
                                    lwd = 2 ))
############ media variancia poisson Hipergeometrica ###############  
  output$mediahyper <- renderText({c("E(X) = ",
                                     ceiling(input$nhyper*input$Mhyper/input$Nhyper))})
  output$varhyper <- renderText({c("Var(X) = ",
                                   round(input$nhyper*(input$Mhyper/input$Nhyper)*((input$Nhyper-input$Mhyper)/input$Nhyper)*(1-((input$nhyper-1)/(input$Nhyper-1))),
                                         2))})
  output$ProbPontoHyper <- renderText({c("P(X = ", input$xhyper,") = " ,
                                       round(dhyper(input$xhyper,input$Mhyper,(input$Nhyper-input$Mhyper),
                                                    input$nhyper),2))})
  output$ProbCoHyper <- renderText({paste("1 - P(X = ", input$xhyper,") = " ,
                                           round(1 - (dhyper(input$xhyper,input$Mhyper,(input$Nhyper-input$Mhyper),
                                                          input$nhyper)),2))})
  output$ProbAcuHyper <- renderText({paste("F(X = " , input$xhyper,") = " ,
                                         round(phyper(input$xhyper,input$Mhyper,
                                                      (input$Nhyper-input$Mhyper),input$nhyper),2))})
  output$ProbCoAcuHyper <- renderText({paste("S(X = " , input$xhyper,") = " ,
                                           round(1 - phyper(input$xhyper,input$Mhyper,
                                                            (input$Nhyper-input$Mhyper),input$nhyper),2))})
  output$DistHyper <- renderPlot(plot(0:input$xhyper,dhyper(0:input$xhyper,input$Mhyper,
                                                            (input$Nhyper-input$Mhyper),input$nhyper),
                                      type = 'h',
                                    xlab = "X", ylab= "f(x)", main = "Gráfico da Distribuição", lwd = 2))
  output$AcuHyper <- renderPlot(plot(0:input$xhyper,phyper(0:input$xhyper,input$Mhyper,
                                                          (input$Nhyper-input$Mhyper),input$nhyper),
                                     type = 'S', 
                                   xlab = "X", ylab = "F(x)", main = "Gráfico de Distribuição Acumulada", 
                                   lwd = 2 ))
  
################################### Funções de introdução ###############################################################################################################################
  
######### Funções binomial ############
  output$FuncaoBin <- renderUI(HTML(paste0("<p> Função de Probabilidade: $$P(X = x) = \\binom{n}{x}p^{x}(1 - p)^{n-x}$$</p>")))
  output$EspacoBin <- renderUI(HTML(paste0("<p> Onde: $$0 \\leq p \\leq 1 $$<p>,
                                           <p>$$ e $$<p>, 
                                           <p>$$ n = 0, 1, 2, ...$$</p>")))
  output$EspBin <- renderUI(HTML(paste0("<p> Esperança : $$E(X) = np$$</p>")))
  output$VariBin <- renderUI(HTML(paste0("<p> Variância: $$Var(X) = np(1 - p)$$</p>")))
  output$MomBin <- renderUI(HTML(paste0("<p> Função Geradora de Momentos: $$ M_{x}(t) = (pe^{t} + (1 - p))^{n}$$</p>")))
  ######### Funções Hipergeométrica ############
  output$FuncaoHyper <- renderUI(HTML(paste0("<p> Função de Probabilidade: $$ P(X = x) = \\frac{\\binom{M}{x} \\binom{N - M}{n - x}}{\\binom{N}{n}}$$</p>")))
  output$EspacoHyper <- renderUI(HTML(paste0("<p> Onde: $$ N = 1,2,...$$</p>,
                                             <p> $$ M = 0, 1, 2, ..., N $$<p>,
                                             <p>$$ n = 1, 2, ..., N $$<p>,
                                             <p>$$ max \\left \\{ n - (N - M) \\right \\}  \\leq x \\geq min \\left \\{ M, N \\right \\}  $$<p>")))
  output$EspHyper <- renderUI(HTML(paste0("<p> Esperança : $$ E(X) = n\\frac{M}{N}$$</p>")))
  output$VariHyper <- renderUI(HTML(paste0("<p> Variância: $$ n\\frac{M}{N}\\frac{(N - M)}{N}\\frac{(N - n)}{(N - 1)}$$</p>")))
  output$MomHyper <- renderUI(HTML(paste0("<p> Função Geradora de Momentos: Não é Útil</p>")))
  ######### Funções Poisson ################
  output$FuncaoPoisson <- renderUI(HTML(paste0("<p> Função de Probabilidade: $$ P(X = x) = \\frac{e^{-\\lambda}\\lambda^{x}}{x!}$$</p>")))
  output$EspacoPoisson <- renderUI(HTML(paste0("<p> Onde: $$ \\lambda > 0 $$</p>")))
  output$EspPoisson <- renderUI(HTML(paste0("<p> Esperança : $$ E(X) = \\lambda$$</p>")))
  output$VariPoisson <- renderUI(HTML(paste0("<p> Variância: $$ Var(X) = \\lambda$$</p>")))
  output$MomPoisson <- renderUI(HTML(paste0("<p> Função Geradora de Momentos: $$ M_{x}(t) = e^{\\lambda(e^{t}-1)}$$</p>")))
  ######### Funções Geométrica ################
  output$FuncaoGeo <- renderUI(HTML(paste0("<p> Função de Probabilidade: $$ P(X = x) = p(1 - p)^{x}$$</p>")))
  output$EspacoGeo <- renderUI(HTML(paste0("<p> Onde: $$  0 < p \\leq 1  $$</p>")))
  output$EspGeo <- renderUI(HTML(paste0("<p> Esperança : $$ E(X) = \\frac{(1 - p)}{p}$$</p>")))
  output$VariGeo <- renderUI(HTML(paste0("<p> Variância: $$ Var(X) = \\frac{(1 - p)}{p^{2}}$$</p>")))
  output$MomGeo <- renderUI(HTML(paste0("<p> Função Geradora de Momentos: $$ M_{x}(t) = \\frac{p}{1 - (1 - p)e^{t}} , t< -ln(1-p)$$</p>")))
  ######### Funções Binomial Negativa ################
  output$FuncaoNegaBinom <- renderUI(HTML(paste0("<p> Função de Probabilidade: $$ P(X = x) = \\binom{k + x - 1}{x}p^{k}(1 - p)^{x} $$</p>")))
  output$EspacoNegaBinom <- renderUI(HTML(paste0("<p> Onde: $$ 0 < p \\leq 1  $$</p>,
                                                 <p>$$ k > 0 $$<p>")))
  output$EspNegaBinom <- renderUI(HTML(paste0("<p> Esperança : $$ E(X) = \\frac{k(1 - p)}{p}$$</p>")))
  output$VariNegaBinom <- renderUI(HTML(paste0("<p> Variância: $$ Var(X) = \\frac{k(1 - p)}{p^{2}}$$</p>")))
  output$MomNegaBinom <- renderUI(HTML(paste0("<p> Função Geradora de Momentos: $$ M_{x}(t) = \\left ( \\frac{p}{1 - (1-p)e^{t}}\\right )^{k}, t < -ln(1 - p)$$</p>")))
  })