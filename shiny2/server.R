library("shiny")
library("openxlsx")
library("rbokeh")
library("htmlwidgets")
library("ggplot2")
library("ggthemes")

theGGPlot <- function(di, whiskers=TRUE, onX = "Number", unit = "ng/mL", logx=FALSE) {
  if (is.null(di))
    return(NULL)
  pl <- ggplot(di, aes_string(x=onX, y="diff", color="absDiff")) 
  if (whiskers) 
    pl <- pl + 
      geom_linerange(data=di[abs(di$diff) > 20,], aes_string(x=onX, ymin="diff", ymax="diffSign", color="absDiff"))
  pl <- pl + 
    geom_point() + theme_bw() +
    geom_hline(yintercept=0, linetype=2) +
    geom_hline(yintercept=c(20,-20)) +
    scale_y_continuous(breaks = c(seq(-100,100,20)), name="Difference [%]",
                       expand = c(0,0),
                       limits=c(min(-100,min(di$diff, na.rm=TRUE)),max(100, max(di$diff, na.rm=TRUE))))+
    scale_color_manual(values=c("black","red3"))+
    theme(legend.position="none", text=element_text(size=15))
  if (onX == "Number") {
    pl + xlab("Sample Number") + ggtitle("Difference vs. Sample Number")
  } else {
    if (logx) 
      pl <- pl + scale_x_log10()
    pl + xlab(paste("Initial Value",unit)) + ggtitle("Difference vs. Initial Value")
  }
}

function(input, output) {
  # get data
  dataInput <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df <- read.xlsx(inFile$datapath, "Arkusz1", startRow=3)
    colnames(df) <- c(
      "Number",	"Subject", "Period", "Sampling_Point", "Initial_Value",	"Repeat_Value",	"diff",	"ISR" 
    )
    df <- na.omit(df[,c(1,5:8)])
    df$absDiff <- abs(df$diff) > 20
    df$diffSign <- 20*sign(df$diff)
    attr(df, which = "name") = inFile[[1]]
    df
  })
  
  output$ggPlotDiffNo <- renderPlot({
    di <- dataInput()
    theGGPlot(di, whiskers = input$whiskers) 
  })
  
  output$ggPlotDiffInit <- renderPlot({
    di <- dataInput()
    theGGPlot(di, whiskers = input$whiskers, onX="Initial_Value", unit = input$unit, logx = input$logx) 
  })
  
  output$ggPlotDiffECDF <- renderPlot({
    di <- dataInput()
    if (is.null(di)) return(NULL)
    
    frac <- mean(abs(di$diff)<=20)
    pl <- ggplot(di, aes(abs(diff))) +
      stat_ecdf() + 
      theme_bw() +
      geom_linerange(x=20, ymin=0, ymax=frac, linetype=2) +
      geom_hline(yintercept=frac, linetype=2, color="red3") +
      geom_text(x=max(abs(di$diff))-1, y=frac, label=paste(round(100*frac,1), "% of samples \nwith difference < 20 %"), vjust=1.3, hjust=1, color="red", size=5) +
      scale_y_continuous(expand = c(0,0), breaks = round(c(seq(0,1,.1)),2), name="Samples in range [%]",labels = scales::percent) +
      scale_x_continuous(expand = c(0,0), breaks = seq(0,100,10), name="Absolute difference [%]") +
      theme(legend.position="none", text=element_text(size=15)) +
      ggtitle("Distribution of absolute differences")
    pl
  })
  
  
  output$ggPlotISR <- renderPlot({
    di <- dataInput()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with IRS data'))
    }

    frac <- mean(abs(di$diff)<=20)
    
    di$succ   <- cumsum(abs(di$diff)<=20)
    di$trials <- 1:nrow(di)
    di$cumm   <- di$succ / di$trials

    ci <- ifelse(input$ciband == "50%", 0.5, 0.9)
    
    CI <- t(sapply(1:nrow(di), function(i){
      prop.test(di$succ[i], di$trials[i], conf.level = ci, correct = TRUE)$conf.int
    }))
    di$CI1 <- CI[,1]
    di$CI2 <- CI[,2]
    
    yname <- "ISR [%]"
    
    pl <- ggplot(di, aes(Number, cumm)) 
    if (input$ciband != "none") {
      pl <- pl + geom_ribbon(aes(x=Number, ymin=CI1, ymax=CI2), fill="grey", alpha=0.5)
      yname <- paste(yname, "+-", input$ciband)
    } 
    pl <- pl +
      geom_point() +
      geom_line() +
      theme_bw() +
      geom_text(x=max(di$Number), y=frac, label=paste(round(100*frac,1), "%"), vjust=1, hjust=-0.1, color="red", size=6) +
      geom_hline(yintercept=0.666, linetype=2) + 
      scale_y_continuous(limits=c(0,1), expand = c(0,0), breaks = round(c(seq(0,1,.2),0.666),2), name=yname,labels = scales::percent) +
      scale_x_continuous(expand = c(0,0), limits = c(0,1.2*max(di$Number)), name="Sample Number") +
      theme(legend.position="none", text=element_text(size=15))+
      ggtitle("Cummulative ISR plot")
    pl
  })
  
  output$ggPlotHist <- renderPlot({
    di <- dataInput()
    if (is.null(di)) return(NULL)
    
    pl <- ggplot(di, aes(diff)) +
      geom_histogram(breaks=seq(-100,100,10), col="white") +
      geom_vline(xintercept=20, linetype=2, col="red3") + 
      geom_vline(xintercept=-20, linetype=2, col="red3") + 
      theme_bw() + 
      scale_fill_manual(values=c("black","red3"))+
      scale_y_continuous(name="Number of Samples",expand = c(0,0)) +
      scale_x_continuous(name="Difference [%]", expand = c(0,0), breaks = seq(-100, 100, 20)) +
      theme(legend.position="none", text=element_text(size=15)) +
      ggtitle("Histogram of differences")
    pl
  })
  
  output$textSummary <- renderPrint({
    di <- dataInput()
    if (is.null(di)) return(cat(""))
    fn <- attr(di, which = "name")
    cat("File name: ", fn, "\n\n")
    cat("Number of samples: ", nrow(di), "\n")
    cat("The most different sample: ", which.max(abs(di$diff)), "\n")
    cat("The largest absolute difference: ", signif(max(abs(di$diff)), 2), "% \n")
    cat("Fraction of samples with difference < 20%: ", round(100*mean(abs(di$diff) < 20),2), "% \n")
    cat("\nDate of the analysis: ", as.character(Sys.Date()), " \n")
    cat("\nDownload a one page report [pdf]\n")
  })
  
}
