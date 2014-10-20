library(shiny)
library(httpuv)
#install.packages("rWBclimate")
library(rWBclimate)
library(ggplot2)

# get data from API
# get temperature data for ensembles
st=1900
en=2100
world <- c(NoAm_country, SoAm_country, Eur_country, Asia_country, Africa_country, Oceana_country)
world<-world[! world %in% c("UMI")]  #remove "UMI"
#world <- c("USA")
wd=getwd()
options(kmlpath = wd)
world_map_df <- create_map_df(world)
world_dat <- get_ensemble_temp(world, "annualavg", start=st, end=en)
world_dat$data <- as.numeric(as.character(world_dat$data))
world_dat<-subset(world_dat,world_dat$percentile==50) #subset to median percentile
world_dat$years=paste(world_dat$fromYear,world_dat$toYear, sep="-")
world_dat<-subset(world_dat, select=-c(percentile, fromYear, toYear))

shinyServer(function(input, output){

  yr1<-reactive({switch(
    input$scen1,
    "past" = input$yearspred1a,
    "a2" = input$yearspred1b,
    "b1" = input$yearspred1c)})
  yr2<-reactive({switch(
    input$scen2,
    "past" = input$yearspred2a,
    "a2" = input$yearspred2b,
    "b1" = input$yearspred2c)})
  
  output$add1 <- renderText({paste(input$scen1, " scenario", " (",yr1(),")") })
  output$add2 <- renderText({paste(input$scen2, " scenario", " (",yr2(),")") })
  #output$add2 <- renderText({paste("Temperature prediction for years ", yr2(), " and ", input$scen2, " scenario") })
  #output$add3<- renderText({paste("Difference between ", input$scen1, " model predictions ", "(", yr1(), ") and ",input$scen2, " model predictions", "(", yr2(), ")")})
  output$add3<- renderText({paste("Difference between ", input$scen1, " scenario", " (",yr1(),") and ",input$scen2, " scenario", " (",yr2(),")")})
  
  dfyr1<-reactive({subset(world_dat, world_dat$years==yr1())})
  dfyr2<-reactive({subset(world_dat, world_dat$years==yr2())})
  df1<-reactive({subset(dfyr1(), dfyr1()$scenario==input$scen1)})
  df2<-reactive({subset(dfyr2(), dfyr2()$scenario==input$scen2)})
  
  output$plot1<- renderPlot({
    #dfyr1<-subset(world_dat, world_dat$scenario==input$scen1)
    #df1 <- subset(dfyr1, dfyr1$years==yr1())
    climate_map(world_map_df,df1(),return_map = T) + scale_fill_gradient2(name="Temperature",limits=c(-20, 34), low="blue", mid="white", high = "red", space="rgb", guide="colourbar") 
  })
  output$plot2<- renderPlot({
    climate_map(world_map_df,df2(),return_map = T) + scale_fill_gradient2(name="Temperature",limits=c(-20, 34), low="blue", mid="white", high = "red", space="rgb", guide="colourbar") 
  })
  output$plotdiff<- renderPlot({
    #dfdiff<-df1
    dfdiff<-df1()
    dfdiff$data<-df1()$data-df2()$data
    climate_map(world_map_df,dfdiff,return_map = T) + scale_fill_gradient2(name="Temperature",low="blue", mid="white", high = "red", space="rgb", guide="colourbar") 
  })
})