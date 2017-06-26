library("dplyr")
library("shiny")
library("openxlsx")
library("ggplot2")
library("grid")
library("gridExtra")
library("ggthemes")
load("datas.rda")

function(input, output) {
  # get data
  dataInput <- reactive({
    df <- datas[[as.character(input$datasets)]]
    df
  })

  output$ggPplot <- renderPlot({
    dane_RIS <- dataInput()
    sizel <- 300 / nrow(dane_RIS)
      
    dane_RIS$cumOK <- cumsum(abs(dane_RIS$difference) < 20) / (1:nrow(dane_RIS))
    
    if(input$subgroups == "2") {
      dane_RIS <-  
        mutate(dane_RIS,
               concentration = ggplot2::cut_number(dane_RIS$`Initial value`, 2, c("LOW concentration",
                                                                                  "HIGH concentration")))
    } 
    if(input$subgroups == "3") {
      dane_RIS <-  
        mutate(dane_RIS,
               concentration = ggplot2::cut_number(dane_RIS$`Initial value`, 3, c("LOW concentration",
                                                                                  "MID concentration",
                                                                                  "HIGH concentration")))
    } 
    
    dane_RIS %>% 
      mutate(color = cut(difference, c(min(difference)-0.001, median(difference[difference < -20], na.rm=TRUE), -20, 20, median(difference[difference > 20], na.rm=TRUE), max(difference))),
             Difference = cut(difference, 
                              c(min(difference),
                                median(difference[difference < -20], na.rm=TRUE),
                                -20, 
                                20,  
                                median(difference[difference > 20], na.rm=TRUE),
                                max(difference)))) %>%
      mutate(ObsNumber = 1:n(),
             ok = abs(difference) < 20,
             Obscumok = cumsum(ok)/Number) %>%
      group_by(concentration) %>%
      mutate(Number = 1:n(),
             ok = abs(difference) < 20,
             cumok = cumsum(ok)/Number) ->
      dane_RIS3

    pal <- as.numeric(input$colorscale)
    maxy <- as.numeric(input$maxy)
    
    if (input$typey == "correct") {
      dane_RIS3$cumok <- 1 - dane_RIS3$cumok
      dane_RIS3$Obscumok <- 1 - dane_RIS3$Obscumok
    }
    
    
    if (input$typep == "barplot") {
      plR <- ggplot(dane_RIS3, aes(Number, ymin=1-cumok, y=1-cumok, ymax=0, color=Difference)) +
        geom_linerange(size=sizel)
      plL <- ggplot(na.omit(dane_RIS3), aes(ObsNumber, y=1-Obscumok, ymin=1-Obscumok, ymax=0, color=Difference)) +
        geom_linerange(size=sizel)      
    } else {
      plR <- ggplot(dane_RIS3, aes(Number, ymin=1-cumok, y=1-cumok, ymax=0, color=Difference)) +
        geom_point(aes(size=abs(difference))) + scale_size(range=c(0,5))
      plL <- ggplot(na.omit(dane_RIS3), aes(ObsNumber, y=1-Obscumok, ymin=1-Obscumok, ymax=0, color=Difference)) +
        geom_point(aes(size=abs(difference))) + scale_size(range=c(0,5))
    }
    
    
    plL <- plL + 
      coord_cartesian(ylim=c(0,maxy))+
      theme_tufte() + 
      scale_color_brewer(type = "div",
                         palette = pal,
                         name="Difference") +
      
      theme(panel.grid.major.y = element_line(color="grey80", linetype = 3), 
            axis.ticks.y = element_line(color="white"),
            strip.text = element_text(hjust=0),
            legend.position = c(0.6,0.9),
            legend.box = "horizontal")+
      guides(color=guide_legend(ncol=5), size=FALSE) + xlab("Sample number")
    
    plR <- plR + 
      coord_cartesian(ylim=c(0,maxy))+
      theme_tufte() + 
      scale_color_brewer(type = "div",
                         palette = pal,
                         name="abs(difference)") +
      facet_wrap(~concentration, ncol = 1) + 
      theme(panel.grid.major.y = element_line(color="grey50", linetype = 3), 
            axis.ticks.y = element_line(color="white"),
            strip.text = element_text(hjust=0),
            legend.position = "none")+ xlab("")+ylab("")+
      ggtitle("")
    
    if (input$scalerev == "fractions") {
      plR <- plR + 
        scale_y_continuous( breaks = 1/c(1,2,3,4,5,10), 
                            labels = paste0("1/", c(1,2,3,4,5,10)))
      plL <- plL + 
        scale_y_continuous( breaks = 1/c(1,2,3,4,5,10), 
                            labels = paste0("1/", c(1,2,3,4,5,10)))
    }
    if (input$scalerev == "only 33%") {
      plR <- plR + 
        scale_y_continuous( breaks = 0.34, 
                            labels = "33%")
      plL <- plL + 
        scale_y_continuous( breaks = 0.33, 
                            labels = "33%")
    }
    if (input$scalerev == "only 67%") {
      plR <- plR + 
        scale_y_continuous( breaks = 0.67, 
                            labels = "67%")
      plL <- plL + 
        scale_y_continuous( breaks = 0.67, 
                            labels = "67%")
    }
    if (input$outline) {
      plL <- plL + geom_step(aes(x=ObsNumber-0.5), color="black")
      plR <- plR + geom_step(aes(x=Number-0.5), color="black")
    }
    
    if (input$typey == "correct") {
      plL <- plL + ylab("Cummulative errors (|difference| > 20%)")
    } else {
      plL <- plL + ylab("Cummulative successess (|difference| < 20%)")
    }
    
    grid.arrange(plL, plR, ncol=2, widths=c(0.7,0.3))
  }, res = 120)

}
