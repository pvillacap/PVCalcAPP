library(ggplot2)
library("mcr")
require("DT")
library(stringr)
library(tidyverse)
library(readxl)
library(magrittr)
library(plotly)
library("BlandAltmanLeh")
library(blandr)
library(shiny)
library(shinyjs)
library(openxlsx)



#funciones
blandr.statistics <- function( method1,
                               method2,
                               sig.level = 0.95,
                               LoA.mode = 1,
                               tipo = "Absoluto") {
  
  # This sends to the preparation function, which does some sense checks on the data And
  # makes sure that the values are prepared
  ba.data <- blandr.data.preparation( method1 , method2 , sig.level )
  
  # method1 and method2 are the measurements
  means <- (ba.data$method1 + ba.data$method2)/2
  differences <- ba.data$method1 - ba.data$method2
  if (tipo == "Absoluto"){
    bias <- mean(differences)
    biasStdDev <- sd(differences)
  } else{
    proportion <- differences / means * 100
    bias <- mean(proportion)
    biasStdDev <- sd(proportion)
  }
  # Convert confidence interval to a two-tailed z-value Don't really need this but kept as
  # a remnant of old version to possibly use in the future
  alpha <- 1 - sig.level
  sig.level.two.tailed <- 1 - (alpha/2)
  sig.level.convert.to.z <- qnorm(sig.level.two.tailed)
  
  # Compute the 95% limits of agreement (based on 1st paper) Don't use the significance
  # level supplied to the function here!  (The significance level is for confidence
  # intervals, not for limits of agreement!)  We want to know in the sample what limits 95%
  # of the pop resides within The LoA.mode switch can change before the +/-1.96 multiplier
  # used in more recent papers Or the estimated +/-2 multiplier used in the original paper
  # The default is LoA.mode which gives a LoA.multiplier of 1.96. In future we could use an
  # even more precise multipler for limits of agreement.
  if (LoA.mode == 2) {
    LoA.multiplier <- 2
  } else {
    LoA.multiplier <- 1.96
  }
  upperLOA <- bias + (LoA.multiplier * biasStdDev)
  lowerLOA <- bias - (LoA.multiplier * biasStdDev)
  
  # Confidence intervals (based on 2nd paper) Based on significance level supplied
  # (defaults to 95% CI) CI for mean
  biasSEM <- sd(differences)/sqrt(length(differences))
  biasCI <- qt(sig.level.two.tailed, df = length(differences) - 1) * biasSEM
  biasUpperCI <- bias + biasCI
  biasLowerCI <- bias - biasCI
  
  # CI for limits of agreement LOAVariance from Carkeet
  LOAVariance <- ((1/length(differences)) + ((sig.level.convert.to.z^2)/(2 * (length(differences) -
                                                                                1)))) * biasStdDev^2
  LOA_SEM <- sqrt(LOAVariance)
  LOA_CI <- qt(sig.level.two.tailed, df = length(differences) - 1) * LOA_SEM
  upperLOA_upperCI <- upperLOA + LOA_CI
  upperLOA_lowerCI <- upperLOA - LOA_CI
  lowerLOA_upperCI <- lowerLOA + LOA_CI
  lowerLOA_lowerCI <- lowerLOA - LOA_CI
  
  # Difference/mean proportion
  proportion <- differences / means * 100
  
  # Number of observations
  no.of.observations <- length(means)
  
  # Works out numbers for proportional bias
  # Couldn't figure this out myself So took example code from
  # http://rforpublichealth.blogspot.co.uk/2013/11/ggplot2-cheatsheet-for-scatterplots.html
  m <- lm( differences ~  means )
  a <- signif(coef(m)[1], digits = 2)
  b <- signif(coef(m)[2], digits = 2)
  regression.equation <- paste("y(differences) = ", b, " x(means) + ", a, sep = "")
  
  results =
    list(
      means = means ,
      differences = differences ,
      method1 = method1 ,
      method2 = method2 ,
      sig.level = sig.level,
      sig.level.convert.to.z = sig.level.convert.to.z ,
      bias = bias ,
      biasUpperCI = biasUpperCI ,
      biasLowerCI = biasLowerCI ,
      biasStdDev = biasStdDev ,
      biasSEM = biasSEM ,
      LOA_SEM = LOA_SEM ,
      upperLOA = upperLOA ,
      upperLOA_upperCI = upperLOA_upperCI ,
      upperLOA_lowerCI = upperLOA_lowerCI ,
      lowerLOA = lowerLOA ,
      lowerLOA_upperCI = lowerLOA_upperCI ,
      lowerLOA_lowerCI = lowerLOA_lowerCI ,
      proportion = proportion ,
      no.of.observations = no.of.observations ,
      regression.equation = regression.equation ,
      regression.fixed.slope = b ,
      regression.fixed.intercept = a
    )  #CLOSE OF LIST
  
  class(results) = "blandr"
  return(results)
  
  # END OF FUNCTION
}



globalVariables(c("biasUpperCI", "x", "y", "differences", "..density..", "dat", "means"))

plotBlandAltman<-function(ba_df, acuerdo= "Absoluto",m1){
  if (!"biasUpperCI" %in% colnames(ba_df)) {
    ba_df$biasUpperCI <- ba_df$bias + 1.96 * ba_df$sd  # Ejemplo de cálculo, ajustar según sea necesario
  }
  if (!"means" %in% colnames(ba_df)) {
    ba_df$means <- (ba_df$col1 + ba_df$col2) / 2  # Ejemplo de cálculo, ajustar según sea necesario
  }
  if (!"differences" %in% colnames(ba_df)) {
    ba_df$differences <- ba_df$col1 - ba_df$col2  # Ejemplo de cálculo, ajustar según sea necesario
  }
  
  if(acuerdo=="Absoluto"){
    plot<- ggplot(data = ba_df, aes(x = means, y = differences)) +
      geom_point(pch = 1, size = 1.5, col = "black") +
      labs(title = "Bland-Altman plot", x = str_interp("media método ${m1@mnames[1]} y ${m1@mnames[2]}"), 
           y = str_interp("${m1@mnames[1]} - ${m1@mnames[2]}")) +
      ylim(mean(ba_df$differences) - 4 * sd(ba_df$differences), 
           mean(ba_df$differences) + 4 * sd(ba_df$differences) )+
      # Línea de bias
      geom_hline(yintercept = ba_df$bias[1], lwd = 1) +
      geom_ribbon(aes(ymin=ba_df$biasLowerCI,ymax=biasUpperCI),alpha=0.1,fill="green")+
      # Línea en y=0
      geom_hline(yintercept = 0, lty = 3, col = "grey30") +
      # Limits of Agreement
      geom_hline(yintercept = ba_df$upperLOA[1],
                 lty = 2, col = "firebrick") +
      geom_hline(yintercept = ba_df$lowerLOA[1],
                 lty = 2, col = "firebrick") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      
      geom_text(label = "Bias", x = 0.9*max(ba_df$means), y = 1.3 * mean(ba_df$differences), size = 3, 
                colour = "black") +
      geom_text(label = "+1.96SD", x = 0.9*max(ba_df$means), y = mean(ba_df$differences) + 2.2 * sd(ba_df$differences), size = 3, 
                colour = "firebrick") +
      geom_text(label = "-1.96SD", x = 0.9*max(ba_df$means), y = mean(ba_df$differences) -2.2 * sd(ba_df$differences), size = 3, 
                colour = "firebrick") +
      theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
      
      geom_smooth(method = "lm", se = TRUE, fill = "lightgrey", lwd = 0.1, lty = 5)
    
    return(ggplotly(plot))    
    
  } else {
    
    plot<- ggplot(data = ba_df, aes(x = means, y = proportion)) +
      geom_point(pch = 1, size = 1.5, col = "black") +
      labs(title = "Bland-Altman plot porcentaje", x = str_interp("media método ${m1@mnames[1]} y ${m1@mnames[2]}"), 
           y = str_interp("${m1@mnames[1]} - ${m1@mnames[2]}/media (%)")) +
      ylim(mean(ba_df$proportion) - 4 * sd(ba_df$proportion), 
           mean(ba_df$proportion) + 4 * sd(ba_df$proportion)) +
      # Línea de bias
      geom_hline(yintercept = mean(ba_df$proportion), lwd = 1) +
      # Línea en y=0
      geom_hline(yintercept = 0, lty = 3, col = "grey30") +
      # Limits of Agreement
      geom_hline(yintercept = mean(ba_df$proportion) + 
                   1.96 * sd(ba_df$proportion), 
                 lty = 2, col = "firebrick") +
      geom_hline(yintercept = mean(ba_df$proportion) - 
                   1.96 * sd(ba_df$proportion), 
                 lty = 2, col = "firebrick") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) +
      
      geom_text(label = "Bias", x = 0.9*max(ba_df$means), y = 1.3 * mean(ba_df$proportion), size = 3, 
                colour = "black") +
      geom_text(label = "+1.96SD", x = 0.9*max(ba_df$means), y = mean(ba_df$proportion) +2.2 * sd(ba_df$proportion), size = 3, 
                colour = "firebrick") +
      geom_text(label = "-1.96SD", x = 0.9*max(ba_df$means), y = mean(ba_df$proportion)- 2.2 * sd(ba_df$proportion), size = 3, 
                colour = "firebrick") +
      theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
      
      geom_smooth(method = "lm", se = TRUE, fill = "lightgrey", lwd = 0.1, lty = 5) 
    
    
    return(ggplotly(plot))
    
  }
  
  
}

plotRegresion<- function(modelo,titulo,dat){
  return(ggplot(data = dat(), aes(x = dat()[,1], y =dat()[,2])) + 
           geom_point(pch = 1, size = 3, col = "black") +
           geom_abline( intercept = modelo@para[1], slope = modelo@para[2], color="blue" )+
           geom_abline(intercept = 0,slope = 1,linetype = "dashed",color = "#7083b5") +
           geom_hline(yintercept = 0, color="darkblue") +
           geom_vline(xintercept = 0, color="darkblue") +
           labs(title = titulo, x = str_interp("${modelo@mnames[1]}"), 
                y = str_interp("${modelo@mnames[2]}")) + 
           
           scale_x_continuous(expand = expansion(mult = c(0, 0.2))) + scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + 
           theme_bw() +
           theme(plot.title = element_text(hjust = 0.5, size = 18)))
}

plotResiduales <- function(residuals,titulo,modelo,dat){
  data<-data.frame(x=dat()[,2],y=residuals$optimized)
  
  return(ggplot(data = data, aes(x =x , y = y)) + 
           geom_point(pch = 1, size = 3, col = "black") +
           geom_hline( yintercept = 0, color="blue" ) +
           labs(title = titulo, x = str_interp("${modelo@mnames[1]}"),y = str_interp("${modelo@mnames[2]}-F(x)[residuales] "))+
           theme_bw() +
           theme(plot.title = element_text(hjust = 0.5, size = 18))
  )}



#Lado servidor
server <- function(input, output, session) {
  
  # Variable necesario para pasar los datos entre los dos observeEvent
  filepath <- reactive({input$mi_fichero})
  
  dat <- reactive({
    req(filepath())  # Asegurarse de que el archivo esté cargado
    openxlsx::read.xlsx(filepath()$datapath, sheet = 1)
  })
  #observo evento de elegir fichero, una vez se alija fichero habilita el boton procesar
  observeEvent(filepath(), {
    
    shinyjs::disable("procesar")
    
    if (is.null(filepath())){
      return(NULL)
    }
    
    
    # Notar que la asignacion es con '<<-' en vez de '<-', para que tenga alcance global, es decir, sea accesible fuera de este observeEvent
    checkfailure <-try( dat<<- reactive({openxlsx::read.xlsx(filepath()$datapath, sheet=1)}))
    
    
    if (class(checkfailure())=="try-error") {
      
      showModal(modalDialog(
        title="Aviso",
        "Error al importar el fichero. Revise su contenido.",
        footer = modalButton("OK"),
        easyClose = TRUE
      ))
      
      shinyjs::disable("procesar")
      
    } else { 
      
      shinyjs::enable("procesar")
   
      
  }
    
    
}) 
  
  
  
  observeEvent(input$procesar, {
    
    output$BA_h3_title<-renderUI({
      h3("Test de Normalidad de la distribución de las diferencias entre los métodos")
    })
    output$BAstats_title <- renderUI({
      h3("Tabla Resumen Estadísticos Bland Altman")
    })
    output$BAtabla <- renderUI({
      h3("Tabla Resultados")
    })
    
    #calculos PABA
    
    m1 <- mcreg(dat()[,1],dat()[,2],method.reg="PaBa",method.ci="analytical",slope.measure=input$slope_measurepb,
                mref.name=names(dat())[1],mtest.name=names(dat())[2],alpha = input$alphapb)
    m2 <- mcreg(dat()[,1],dat()[,2],method.reg="Deming",method.ci="analytical",error.ratio = input$errorRatio,
                mref.name=names(dat())[1],mtest.name=names(dat())[2],alpha = input$alphadm)
    #residuales de los metodos de regresion
    resPb<- getResiduals(m1)
    resDm<- getResiduals(m2)
    
    #Estadisticos bland altman
    BA_stats<-blandr.statistics(dat()[,1],dat()[,2],sig.level=1-input$alphaba,tipo = input$acuerdo)
    
    ba_df<- data.frame(unclass(BA_stats))
    ba_df_valores <- ba_df[c(1,2,3,4,19)]
    ba_df_valores[,5] <- round(ba_df_valores[,5],2)
   
    ba_df_stats<- ba_df[1,-c(1,2,3,4,19,22,23)]
    
    
    # Funcion para la regresión pb
    
    pbplot <- plotRegresion(m1,"Gráfico Regresión Passing Bablok",dat)
    # Funcion para la regresión deming
    demingplot<-plotRegresion(m2,"Gráfico Regrasión Deming",dat)
    
    
    
    #Gráfico del histograma de las diferencias de las medidas entre métodos
    histBA<- ggplot(data = ba_df, aes(x = differences)) + 
      geom_histogram(bins = input$bins,fill="white", color="black",aes(y=..density..)) +
      geom_function(fun=function(x) 
        dnorm(x, 
              mean=mean(ba_df$differences),
              sd=sd(ba_df$differences)),
        color="red") +
      geom_density(alpha = 0.6,color="blue") +
      labs(title = "Histograma diferencias",x=str_interp("${m1@mnames[1]} - ${m1@mnames[2]}"), y="Densidad") +
      theme_classic() + theme(plot.title = element_text(hjust = 0.5)) 
    
    #Gráfico del qqnorm de las diferencias de las medidas entre métodos
    
    qqnormPlot<-ggplot(data = ba_df, aes(sample = differences)) + 
      stat_qq(pch = 1, size = 1.5, col = "black") +
      stat_qq_line(col="blue")+
      labs(title = "qqnorm diferencias",x=str_interp("Cuantiles teóricos de la diferencia"),y=str_interp("${m1@mnames[1]} - ${m1@mnames[2]}")) +
      theme_classic() + theme(plot.title = element_text(hjust = 0.5)) 
   
    
   #grafico residuales
    
    
    output$resPb<- renderPlotly({
      plotResiduales(resPb,"Residuales de la regresión Passing Bablok",m1,dat)
    })
      
    
    acuerdo<-input$acuerdo
      #grafico bland altman en porcetaje
    output$BA <- renderPlotly({
        
        plotBlandAltman(ba_df,acuerdo,m1)
    })
      
    
    #renderizar los graficos
    output$PB<-renderPlotly({
      pbplot})
    
    output$DM<-renderPlotly({
      demingplot
    })
    output$resDM<-renderPlotly({
      plotResiduales(resDm,"Residuales de la Regresión Deming",m2,dat)
    })
    
    output$hist<-renderPlotly({
      histBA
     
    })
    
    output$qqnorm<-renderPlotly({
      qqnormPlot
      
    })
    
    #tabla de shapiro
    shapiro<- data.frame(unclass(shapiro.test(ba_df$differences)),row.names = NULL)
    shapiro$datos<-"Diferencia entre métodos"
    shapiro<-shapiro[,c(5,3,1,2)]
    names(shapiro)<-c("Datos","Método","Estadístico","P-valor")
    
   
    
    
    
    
    
    
    
    
    output$PBtable<-DT::renderDataTable(m1@para,options = list(searching = FALSE))
    
    output$DMtable<-DT::renderDataTable(m2@para,options = list(searching = FALSE))
    
    output$BAshapiro<-DT::renderDataTable(shapiro,options = list(searching = FALSE))
    
   
    
    output$BAtable<-DT::renderDataTable(ba_df_valores,  # Convert summary to data frame
                                        options = list(searching = FALSE)
    )
    output$BAstats<-DT::renderDataTable(as.data.frame(t(ba_df_stats)),# Convert summary to data frame
                                        options = list(searching = FALSE),colnames=c("Resultado"),rownames=c("Nivel significación","Nivel significación Dist. Normal","Bias","Lim.Sup.IC.Bias","Lim.Inf.IC.Bias", "Desv.Std.Bias",
                                                                                                             "Error.Std.Bias","Error.Std.LOA","Lim.Sup.LOA","Lim.Sup.IC.Sup.LOA","Lim.Sup.IC.Inf.LOA",
                                                                                                             "Lim.Inf.LOA","Lim.Inf.IC.Sup.LOA","Lim.Inf.IC.Inf.LOA","n.Observaciones","Ecuación regresión"))
    
    
    output$informe<- downloadHandler(
      filename = "report.html",
      content = function(file){
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed)
        tempReport <- file.path(getwd(), "informe.Rmd")
       
        file.copy("report/informe.Rmd", tempReport, overwrite = TRUE)
        # Set up parameters to pass to Rmd document
        params <- list(m1 = m1,
                       m2 = m2,
                       datos = dat(),
                       alpha = isolate(input$alphaba),
                       bins = isolate(input$bins),
                       tipoAcuerdo=input$acuerdo,
                       tipoError=input$tipoError,
                       limiteAcuerdo=input$loa,
                       limiteBias=input$bias,
                       criterio= input$criterio,
                       ba = ba_df)
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          output_format = 'html_document', 
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
  }
  
  )
}

