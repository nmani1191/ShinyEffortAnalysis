# This is an R Shiny Application helping to do MTTR Analysis

run_MTTR <-  function()
{
  #Package Loading
  #######################################################
  library(shiny)
  library(dplyr)
  library(DT)
  library(lubridate)
  library(ggplot2)
  library(reshape2)
  library(ggrepel)
  library(reshape)
  library(shinydashboard)

  #Function Loading
  #######################################################
  #Summary of Input Dataset
  Unique_NA_counts <- function(input_data)
  {
    df1 <- data.frame(character(),character(),integer(),integer(),integer(),numeric())

    for (name in colnames(input_data)) {

      df1 <- rbind(df1,data.frame(ColName=name,Datatype=class(input_data[,name]),Total_Records=nrow(input_data),
                                  Unique_Counts=length(unique(input_data[,name])),
                                  NA_Counts=sum(is.na(input_data[,name])),
                                  NA_Percent=round(sum(is.na(input_data[,name]))/nrow(input_data),2)))

    }

    df1 <- as.data.frame(df1 %>% arrange(-NA_Counts))

    return(df1)
  }

  #######################################################
  #Get MTTR Graph Days:
  GetMTTRGraphInDays <- function(ins_data_set)
  {
    ResolutionTime_variation<- as.data.frame(ins_data_set %>% dplyr::group_by(CatCode) %>%
                                               dplyr::summarise(Count=n(),ValuePresent=sum(!is.na(ResolutionTimeindays)),Min=quantile(ResolutionTimeindays,0,na.rm=T),
                                                                P5=quantile(ResolutionTimeindays,0.05,na.rm=T),
                                                                P25=quantile(ResolutionTimeindays,0.25,na.rm=T),
                                                                P50=quantile(ResolutionTimeindays,0.50,na.rm=T),
                                                                P75=quantile(ResolutionTimeindays,0.75,na.rm=T),
                                                                P95=quantile(ResolutionTimeindays,0.95,na.rm=T),
                                                                Max=quantile(ResolutionTimeindays,1,na.rm=T)) %>% dplyr::filter(ValuePresent >=10)
                                             %>% dplyr::arrange(-ValuePresent))


    mod_ResolutionTime_variation <- melt(ResolutionTime_variation,id.vars =c("CatCode","Count","ValuePresent"))

    mod_ResolutionTime_variation$CatCode <- factor(mod_ResolutionTime_variation$CatCode,levels = unique(mod_ResolutionTime_variation[order(mod_ResolutionTime_variation$ValuePresent,decreasing=T),"CatCode"]))

    RTV_InDays_plot   <- ggplot(mod_ResolutionTime_variation, aes(x=variable,y= as.numeric(value),group=CatCode,colour=CatCode)) +
      geom_line() + geom_point() + facet_wrap(~CatCode,scales = "free_y") +
      geom_text(aes(x="P50",y=max(as.numeric(value))/2,label=ValuePresent),colour="#b2beb5",size=5) +
      geom_text_repel(aes(label=round(as.numeric(value),2)),size=3.5,show.legend = F) +
      labs(x="MTTR Quantile",y="MTTR (Days)") +
      theme_dark()+ theme(title=element_text(face = "bold.italic"),
                          plot.title = element_text(hjust=0.5),
                          legend.position = "none",
                          panel.border = element_rect(colour = "black", fill=NA),
                          panel.background = element_blank(),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank())

    return(list(plot=RTV_InDays_plot,data=mod_ResolutionTime_variation))
  }

  #######################################################
  #Get MTTR Table Days:
  GetMTTRTableDay <- function(ins_data_set)
  {
    datatoplot <- ins_data_set
    remove(df)
    Pattern<- unique(datatoplot$CatCode)
    n<- length(Pattern)

    df <- data.frame(Pattern = character(n),
                     TckCnt =integer(n),
                     TckCnt_OpenCloseDate= integer(n),
                     MeanMTTR = numeric(n),
                     P50 = numeric(n), P75 = numeric(n),
                     ImprovedMTTR_ByTGTP75 = numeric(n),
                     PercentImprovement_ByTGTP75 = numeric(n),
                     ImprovedMTTR_ByTGTP50 = numeric(n),
                     PercentImprovement_ByTGTP50 = numeric(n),
                     stringsAsFactors = FALSE)

    for(i in 1:length(unique(datatoplot$CatCode)))
    {
      # i<-15
      dataset<- datatoplot %>% filter(CatCode == unique(datatoplot$CatCode)[i])
      p<- boxplot.stats(as.numeric(dataset$ResolutionTimeindays),do.conf = T,do.out = T,coef = 0.2)

      df$TckCnt[i] <- nrow(dataset)
      df$TckCnt_OpenCloseDate[i] <- sum(!is.na(dataset$ResolutionTimeindays))
      df$Pattern[i] <-  paste(unique(datatoplot$CatCode)[i],sep="")
      df$MeanMTTR[i] <- mean(dataset$ResolutionTimeindays,na.rm=T)
      df$P50[i] <- p$stats[3]
      df$P75[i] <- quantile(dataset$ResolutionTimeindays,0.75,na.rm=T)

      xz<- dataset
      xz$ResolutionTimeindays[which(xz$ResolutionTimeindays > quantile(dataset$ResolutionTimeindays,0.75,na.rm=T))] <- quantile(dataset$ResolutionTimeindays,0.75,na.rm=T)
      df$ImprovedMTTR_ByTGTP75[i] <- mean(xz$ResolutionTimeindays,na.rm=T)

      df$PercentImprovement_ByTGTP75[i] = paste(round(((df$MeanMTTR[i] - df$ImprovedMTTR_ByTGTP75[i] ) / df$MeanMTTR[i])*100,2),"%",sep="")

      yz<- dataset
      yz$ResolutionTimeindays[which(yz$ResolutionTimeindays > quantile(dataset$ResolutionTimeindays,0.50,na.rm=T))] <- quantile(dataset$ResolutionTimeindays,0.50,na.rm=T)
      df$ImprovedMTTR_ByTGTP50[i] <- mean(yz$ResolutionTimeindays,na.rm=T)

      df$PercentImprovement_ByTGTP50[i] = paste(round(((df$MeanMTTR[i] - df$ImprovedMTTR_ByTGTP50[i] ) / df$MeanMTTR[i])*100,2),"%",sep="")


    }
    df <- as.data.frame(df %>% filter(TckCnt_OpenCloseDate > 10) %>% arrange(-TckCnt_OpenCloseDate))

    mod_MTTRTable1 <- melt(df[,c("Pattern","TckCnt","TckCnt_OpenCloseDate","MeanMTTR","ImprovedMTTR_ByTGTP75","ImprovedMTTR_ByTGTP50")],
                           id.vars =c("Pattern","TckCnt","TckCnt_OpenCloseDate"))

    mod_MTTRTable2 <- df[,c("Pattern","TckCnt","TckCnt_OpenCloseDate","MeanMTTR","P50","P75",
                            "PercentImprovement_ByTGTP75","PercentImprovement_ByTGTP50")]

    mod_MTTRTable3<- merge(mod_MTTRTable1,mod_MTTRTable2,by=c("Pattern","TckCnt","TckCnt_OpenCloseDate"))

    mod_MTTRTable3$ymax <- mod_MTTRTable3$MeanMTTR
    mod_MTTRTable3$ymax[mod_MTTRTable3$variable =="MeanMTTR"] <- NA

    mod_MTTRTable3$labelaxes <- mod_MTTRTable3$value + (mod_MTTRTable3$ymax-mod_MTTRTable3$value)/2
    mod_MTTRTable3$labelaxes[mod_MTTRTable3$variable =="MeanMTTR"] <- NA

    mod_MTTRTable3$PercentImprovement_label <- NA

    for(i in 1:nrow(mod_MTTRTable3))
    {
      if(mod_MTTRTable3$variable[i]=="ImprovedMTTR_ByTGTP75")
      {
        mod_MTTRTable3$PercentImprovement_label[i] <- mod_MTTRTable3$PercentImprovement_ByTGTP75[i]
      }
      else if(mod_MTTRTable3$variable[i]=="ImprovedMTTR_ByTGTP50")
      {
        mod_MTTRTable3$PercentImprovement_label[i] <- mod_MTTRTable3$PercentImprovement_ByTGTP50[i]
      }
    }

    Mod_MTTRchart <-  ggplot(mod_MTTRTable3,aes(x=variable,y=round(value,2),fill=variable)) +
      geom_bar(stat = "identity",width = 0.5) + facet_wrap(~Pattern,scales = "free_y") +
      geom_errorbar(aes(ymax=ymax,ymin=round(value,2)),width = 0.25,colour="darkgreen")   +
      geom_text(aes(label=round(value,2)),vjust=1,show.legend = F) +
      geom_text(aes(y=labelaxes,label=PercentImprovement_label),hjust=0,colour="darkgreen",size=3) +
      labs(title="MTTR Improvement (InDays)",y="MTTR (Days)") + theme_bw() +
      theme(title=element_text(face = "bold.italic"),
            plot.title = element_text(hjust=0.5),
            legend.position = "bottom",
            panel.border = element_rect(colour = "black", fill=NA),
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank()) + scale_fill_discrete(name = "Bar Definition")

    return(list(Mod_MTTRchart=Mod_MTTRchart,Mod_MTTRtable=mod_MTTRTable3,MTTRtable=df))
  }

  #######################################################
  #Get MTTR Graph Hours:
  GetMTTRGraphInHours <- function(ins_data_set)
  {
    ResolutionTime_variation<- as.data.frame(ins_data_set %>% dplyr::group_by(CatCode) %>%
                                               dplyr::summarise(Count=n(),ValuePresent=sum(!is.na(ResolutionTimeinHours)),
                                                                Min=quantile(ResolutionTimeinHours,0,na.rm=T),
                                                                P5=quantile(ResolutionTimeinHours,0.05,na.rm=T),
                                                                P25=quantile(ResolutionTimeinHours,0.25,na.rm=T),
                                                                P50=quantile(ResolutionTimeinHours,0.50,na.rm=T),
                                                                P75=quantile(ResolutionTimeinHours,0.75,na.rm=T),
                                                                P95=quantile(ResolutionTimeinHours,0.95,na.rm=T),
                                                                Max=quantile(ResolutionTimeinHours,1,na.rm=T)) %>% dplyr::filter(ValuePresent >=10)
                                             %>% dplyr::arrange(-ValuePresent))

    mod_ResolutionTime_variation <- melt(ResolutionTime_variation,id.vars =c("CatCode","Count","ValuePresent") )

    mod_ResolutionTime_variation$CatCode <- factor(mod_ResolutionTime_variation$CatCode,levels = unique(mod_ResolutionTime_variation[order(mod_ResolutionTime_variation$ValuePresent,decreasing=T),"CatCode"]))

    RTV_InHours_plot <-   ggplot(mod_ResolutionTime_variation, aes(x=variable,y= as.numeric(value),group=CatCode,colour=CatCode)) +
      geom_line() + geom_point() + facet_wrap(~CatCode,scales = "free_y") +
      geom_text(aes(x="P50",y=max(as.numeric(value))/2,label=ValuePresent),colour="#b2beb5",size=5) +
      geom_text_repel(aes(label=round(as.numeric(value),2)),size=3.5,show.legend = F) +
      labs(x="MTTR Percentile",y="MTTR (Hours)") +
      theme_dark()+ theme(title=element_text(face = "bold.italic"),
                          plot.title = element_text(hjust=0.5),
                          legend.position = "none",
                          panel.border = element_rect(colour = "black", fill=NA),
                          panel.background = element_blank(),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank())

    return(list(plot=RTV_InHours_plot,data=mod_ResolutionTime_variation))
  }

  #######################################################
  #Get MTTR Table Hours:
  GetMTTRTableHour <- function(ins_data_set)
  {
    datatoplot <- ins_data_set
    remove(df)
    Pattern<- unique(datatoplot$CatCode)
    n<- length(Pattern)

    df <- data.frame(Pattern = character(n),
                     TckCnt =integer(n),
                     TckCnt_OpenCloseDate= integer(n),
                     MeanMTTR = numeric(n),
                     P50 = numeric(n), P75 = numeric(n),
                     ImprovedMTTR_ByTGTP75 = numeric(n),
                     PercentImprovement_ByTGTP75 = numeric(n),
                     ImprovedMTTR_ByTGTP50 = numeric(n),
                     PercentImprovement_ByTGTP50 = numeric(n),
                     stringsAsFactors = FALSE)

    for(i in 1:length(unique(datatoplot$CatCode)))
    {
      # i<-15
      dataset<- datatoplot %>% filter(CatCode == unique(datatoplot$CatCode)[i])
      p<- boxplot.stats(as.numeric(dataset$ResolutionTimeinHours),do.conf = T,do.out = T,coef = 0.2)

      df$TckCnt[i] <- nrow(dataset)
      df$TckCnt_OpenCloseDate[i] <- sum(!is.na(dataset$ResolutionTimeinHours))
      df$Pattern[i] <-  paste(unique(datatoplot$CatCode)[i],sep="")
      df$MeanMTTR[i] <- mean(dataset$ResolutionTimeinHours,na.rm=T)
      df$P50[i] <- p$stats[3]
      df$P75[i] <- quantile(dataset$ResolutionTimeinHours,0.75,na.rm=T)

      xz<- dataset
      xz$ResolutionTimeinHours[which(xz$ResolutionTimeinHours > quantile(dataset$ResolutionTimeinHours,0.75,na.rm=T))] <- quantile(dataset$ResolutionTimeinHours,0.75,na.rm=T)
      df$ImprovedMTTR_ByTGTP75[i] <- mean(xz$ResolutionTimeinHours,na.rm=T)

      df$PercentImprovement_ByTGTP75[i] = paste(round(((df$MeanMTTR[i] - df$ImprovedMTTR_ByTGTP75[i] ) / df$MeanMTTR[i])*100,2),"%",sep="")

      yz<- dataset
      yz$ResolutionTimeinHours[which(yz$ResolutionTimeinHours > quantile(dataset$ResolutionTimeinHours,0.50,na.rm=T))] <- quantile(dataset$ResolutionTimeinHours,0.50,na.rm=T)
      df$ImprovedMTTR_ByTGTP50[i] <- mean(yz$ResolutionTimeinHours,na.rm=T)

      df$PercentImprovement_ByTGTP50[i] = paste(round(((df$MeanMTTR[i] - df$ImprovedMTTR_ByTGTP50[i] ) / df$MeanMTTR[i])*100,2),"%",sep="")

    }

    df <- as.data.frame(df %>% filter(TckCnt_OpenCloseDate > 10) %>% arrange(-TckCnt_OpenCloseDate))

    mod_MTTRTable1 <- melt(df[,c("Pattern","TckCnt","TckCnt_OpenCloseDate","MeanMTTR","ImprovedMTTR_ByTGTP75","ImprovedMTTR_ByTGTP50")],
                           id.vars =c("Pattern","TckCnt","TckCnt_OpenCloseDate"))

    mod_MTTRTable2 <- df[,c("Pattern","TckCnt","TckCnt_OpenCloseDate","MeanMTTR","P50","P75",
                            "PercentImprovement_ByTGTP75","PercentImprovement_ByTGTP50")]

    mod_MTTRTable3<- merge(mod_MTTRTable1,mod_MTTRTable2,by=c("Pattern","TckCnt","TckCnt_OpenCloseDate"))

    mod_MTTRTable3$ymax <- mod_MTTRTable3$MeanMTTR
    mod_MTTRTable3$ymax[mod_MTTRTable3$variable =="MeanMTTR"] <- NA

    mod_MTTRTable3$labelaxes <- mod_MTTRTable3$value + (mod_MTTRTable3$ymax-mod_MTTRTable3$value)/2
    mod_MTTRTable3$labelaxes[mod_MTTRTable3$variable =="MeanMTTR"] <- NA

    mod_MTTRTable3$PercentImprovement_label <- NA

    for(i in 1:nrow(mod_MTTRTable3))
    {
      if(mod_MTTRTable3$variable[i]=="ImprovedMTTR_ByTGTP75")
      {
        mod_MTTRTable3$PercentImprovement_label[i] <- mod_MTTRTable3$PercentImprovement_ByTGTP75[i]
      }
      else if(mod_MTTRTable3$variable[i]=="ImprovedMTTR_ByTGTP50")
      {
        mod_MTTRTable3$PercentImprovement_label[i] <- mod_MTTRTable3$PercentImprovement_ByTGTP50[i]
      }
    }

    Mod_MTTRchart<-  ggplot(mod_MTTRTable3,aes(x=variable,y=round(value,2),fill=variable)) +
      geom_bar(stat = "identity",width = 0.5) + facet_wrap(~Pattern,scales = "free_y") +
      geom_errorbar(aes(ymax=ymax,ymin=round(value,2)),width = 0.25,colour="darkgreen")   +
      geom_text(aes(label=round(value,2)),vjust=1,show.legend = F) +
      geom_text(aes(y=labelaxes,label=PercentImprovement_label),hjust=0,colour="darkgreen",size=3) +
      labs(title="MTTR Improvement (InHours)",y="MTTR (Hours)") + theme_bw() +
      theme(title=element_text(face = "bold.italic"),
            plot.title = element_text(hjust=0.5),
            legend.position = "bottom",
            panel.border = element_rect(colour = "black", fill=NA),
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank()) + scale_fill_discrete(name = "Bar Definition")

    return(list(Mod_MTTRchart=Mod_MTTRchart,Mod_MTTRtable=mod_MTTRTable3,MTTRtable=df))
  }

  ########################################################
  options(shiny.maxRequestSize = 1024*1024^2)
  #Shiny App
  shinyApp (

    ui =dashboardPage(
      dashboardHeader(title = "MTTR Analysis"),
      dashboardSidebar(width = 300,
                       conditionalPanel(condition="input.conditionedPanels==1",
                                        fileInput('dataset', 'Choose CSV File',accept=c('.csv')),
                                        actionButton("Validate","validate"),

                                        tags$hr(),

                                        uiOutput("OpenDate"),
                                        uiOutput("ResolvedDate"),
                                        uiOutput("DateFormat"),
                                        uiOutput("Category"),
                                        uiOutput("MttrType"),
                                        uiOutput("CategoryFilter"),
                                        uiOutput("runbutton"))
      ),

      dashboardBody(

        tabsetPanel(id="conditionedPanels",
                    tabPanel("Output",value = 1,
                             h4("1) Data Summary",style="color:darkgreen"),
                             DT::dataTableOutput("dataSummary"),
                             br(),
                             h4("2) Top Contributed Category",style="color:darkgreen"),
                             DT::dataTableOutput("TopCategory"),
                             br(),
                             h4("3) MTTR Variation for Category",style="color:darkgreen"),
                             plotOutput("MTTRgraph"),
                             br(),
                             h4("4) MTTR Improvement for Category - Chart",style="color:darkgreen"),
                             plotOutput("MTTRImproveChart")
                    ),
                    tabPanel("Help",value = 2,
                             h3("How to choose Date Formats to compute MTTR ?",style="color:darkgreen"),
                             DT::dataTableOutput("dateFormatSample"))
        )
      )
    ),
    server= function(input, output, session) {

      observeEvent(input$Validate,{

        input$Validate # Re-run when button is clicked

        withProgress(message = 'Validating...', value = 0, {

          incProgress(0.25, detail = " 25%")

          inFile <- input$dataset
          ins_data_set <- read.csv(inFile$datapath,header = T,strip.white = T,
                                   na.strings = c(""," ","NA","NULL","na","null"))
          Column_names <- colnames(ins_data_set)

          output$OpenDate <- renderUI({
            selectInput("openDate", label = "Select OpenDate:",  c("--select--", Column_names))
          })

          output$ResolvedDate <- renderUI({
            selectInput("resolvedDate", label = "Select ResolvedDate:",  c("--select--", Column_names))
          })

          date_formats <- c("ydm HM","mdY HM","mdY HMS","dmY HMS",
                            "dby HMS","dbY HMS","dBy HMS","dBY HMS",
                            "ymd HMS","Ymd HMS","BdY HMS","bdY HMS",
                            "Bdy HMS","mdy HMS","dmy HMS","dmY IMSp")

          date_formats_eg <- c(format(Sys.time(),"%y-%d-%m %H:%M"),format(Sys.time(),"%m-%d-%Y %H:%M"),format(Sys.time(),"%m-%d-%Y %H:%M:%S"),
                               format(Sys.time(),"%d-%m-%Y %H:%M:%S"),format(Sys.time(),"%d-%b-%y %H:%M:%S"),format(Sys.time(),"%d-%b-%Y %H:%M:%S"),format(Sys.time(),"%d-%B-%y %H:%M:%S"),
                               format(Sys.time(),"%d-%B-%Y %H:%M:%S"),format(Sys.time(),"%y-%m-%d %H:%M:%S"),format(Sys.time(),"%Y-%m-%d %H:%M:%S"),format(Sys.time(),"%B-%d-%Y %H:%M:%S"),
                               format(Sys.time(),"%b-%d-%Y %H:%M:%S"),format(Sys.time(),"%B-%d-%y %H:%M:%S"),format(Sys.time(),"%m-%d-%y %H:%M:%S"),format(Sys.time(),"%d-%m-%y %H:%M:%S"),
                               format(Sys.time(),"%d-%m-%Y %I:%M:%S %p"))

          sample_format <- data.frame(DateFormat=date_formats,DateExample=date_formats_eg)

          output$DateFormat <- renderUI({
            selectInput("dateFormat", label = "Select DateFormat:", c("--select--",date_formats))
          })

          output$Category <- renderUI({
            selectInput("category", label = "Select Category:",  c("--select--", Column_names))
          })

          output$MttrType <- renderUI({
            selectInput("mttrType", label = "Select MTTR Type:",  c("--select--", "InHours","InDays"))
          })

          output$CategoryFilter <- renderUI({
            sliderInput("catFilter", label = "Select Top Category (%):", min = 1,max = 100,value = 100)
          })

          output$runbutton <- renderUI({
            actionButton("run","Run")
          })

          incProgress(0.5, detail = " 50%")

          dataSummary <- reactive({

            validate(
              need(input$Validate != 0, "Please Upload Date & Validate")
            )

            isolate({
              summary<- Unique_NA_counts(ins_data_set)
              return(summary)
            })
          })

          incProgress(0.75, detail = " 75%")

          output$dataSummary<- DT::renderDataTable(datatable(dataSummary(),filter='top',options=list(autoWidth=TRUE)))
          output$dateFormatSample<- DT::renderDataTable(datatable(sample_format,filter='top',options=list(autoWidth=TRUE)))

          incProgress(1, detail = " 100%")
        })
      })

      observeEvent(input$run,{

        input$run

        withProgress(message = 'Main Processing...', value = 0, {

          incProgress(0.05, detail = " 5%")

          inFile <- input$dataset
          ins_data_set <- read.csv(inFile$datapath,header = T,strip.white = T,
                                   na.strings = c(""," ","NA","NULL","na","null"))
          Column_names <- colnames(ins_data_set)

          incProgress(0.1, detail = " 10%")

          processing <- reactive({

            validate(
              need(input$run != 0 &
                     (!input$openDate %in% c("--select--","")) &
                     (!input$resolvedDate %in% c("--select--","")) &
                     (!input$dateFormat %in% c("--select--","")) &
                     (!input$category %in% c("--select--","")) &
                     (!input$mttrType %in% c("--select--","")),"Please update Mandatory Fileds & Run it")
            )
            isolate({
              withProgress(message = 'Sub Processing...', value = 0, {

                incProgress(0.1, detail = " 10%")
                total_rows <- nrow(ins_data_set)
                ins_data_set$CatCode <- ins_data_set[,which(colnames(ins_data_set)==input$category)]
                ins_data_set$d_mod_created_date<-parse_date_time(ins_data_set[,which(colnames(ins_data_set)==input$openDate)],input$dateFormat)
                ins_data_set$d_mod_resolved_date<-parse_date_time(ins_data_set[,which(colnames(ins_data_set)==input$resolvedDate)],input$dateFormat)

                ins_data_set$ResolutionTimeindays<- round(difftime(ins_data_set$d_mod_resolved_date,ins_data_set$d_mod_created_date, units = "days"),2)
                ins_data_set$ResolutionTimeinHours<- round(difftime(ins_data_set$d_mod_resolved_date,ins_data_set$d_mod_created_date, units = "hours"),2)

                Top_Cat <- as.data.frame(ins_data_set %>% dplyr::group_by(CatCode) %>% dplyr::summarise(TotalTck=n(),
                                                                                                        TotalTck_withMTTR=sum(!is.na(ResolutionTimeindays))) %>% dplyr::arrange(-TotalTck_withMTTR))
                Top_Cat$prt <- round(Top_Cat$TotalTck_withMTTR/nrow(ins_data_set)*100,2)
                Top_Cat$cumprt <- cumsum(Top_Cat$prt)
                Top_Cats <- Top_Cat$CatCode[Top_Cat$cumprt <= input$catFilter]

                ins_data_set <- as.data.frame(ins_data_set %>% filter(CatCode %in% Top_Cats))
                if(input$mttrType=="InDays")
                {
                  MTTRgraph <- GetMTTRGraphInDays(ins_data_set)
                  MTTRtables <- GetMTTRTableDay(ins_data_set)
                  MTTR_Imp_Chart <- MTTRtables$Mod_MTTRchart

                  write.csv(MTTRgraph$data,"MTTR_Variation_InDays_Data.csv",row.names = F)
                  write.csv(MTTRtables$Mod_MTTRtable,"MTTR_Improvement_InDays_Data.csv",row.names = F)
                }
                else if(input$mttrType=="InHours")
                {
                  MTTRgraph <- GetMTTRGraphInHours(ins_data_set)
                  MTTRtables <- GetMTTRTableHour(ins_data_set)
                  MTTR_Imp_Chart <- MTTRtables$Mod_MTTRchart

                  write.csv(MTTRgraph$data,"MTTR_Variation_InHours_Data.csv",row.names = F)
                  write.csv(MTTRtables$Mod_MTTRtable,"MTTR_Improvement_InHours_Data.csv",row.names = F)
                }

                return(list(MTTRgraph=MTTRgraph$plot,MTTR_Imp_Chart=MTTR_Imp_Chart,
                            Top_Cat=Top_Cat))


                incProgress(1, detail = " 100%")
              })
            })
          })

          incProgress(0.25, detail = " 25%")

          final_outputs <- processing()

          incProgress(0.5, detail = " 50%")

          output$MTTRgraph <- renderPlot({final_outputs$MTTRgraph})

          output$MTTRImproveChart <- renderPlot({final_outputs$MTTR_Imp_Chart},height = "auto")

          output$TopCategory <- DT::renderDataTable(datatable(final_outputs$Top_Cat,options=list(autoWidth=TRUE)))

          incProgress(0.75, detail = " 75%")

          incProgress(1, detail = " 100%")

        })
      })
    }
  )

}


###########################################################

MTTR_variation_day_plot <- function(MTTR_variation_data)
{
  MTTR_variation_data$CatCode <- as.character(MTTR_variation_data$CatCode)

  MTTR_variation_data$CatCode <- factor(MTTR_variation_data$CatCode,levels = unique(MTTR_variation_data[order(MTTR_variation_data$ValuePresent,decreasing=T),"CatCode"]))

  MTTR_variation_data$variable <- factor(MTTR_variation_data$variable,levels = c("Min","P5","P25","P50","P75","P95","Max"))

  plot <- ggplot(MTTR_variation_data, aes(x=variable,y= as.numeric(value),group=CatCode,colour=CatCode)) +
    geom_line() + geom_point() + facet_wrap(~CatCode,scales = "free_y") +
    geom_text(aes(x="P50",y=max(as.numeric(value))/2,label=ValuePresent),colour="#b2beb5",size=5) +
    geom_text_repel(aes(label=round(as.numeric(value),2)),size=3.5,show.legend = F) +
    labs(title="MTTR Variation (InDays)",x="MTTR Percentile",y="MTTR (Days)") +
    theme_dark()+ theme(title=element_text(face = "bold.italic"),
                        plot.title = element_text(hjust=0.5),
                        legend.position = "none",
                        panel.border = element_rect(colour = "black", fill=NA),
                        panel.background = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank())

  return(plot)
}

###########################################################

MTTR_variation_hr_plot <- function(MTTR_variation_data)
{
  MTTR_variation_data$CatCode <- as.character(MTTR_variation_data$CatCode)

  MTTR_variation_data$CatCode <- factor(MTTR_variation_data$CatCode,levels = unique(MTTR_variation_data[order(MTTR_variation_data$ValuePresent,decreasing=T),"CatCode"]))

  MTTR_variation_data$variable <- factor(MTTR_variation_data$variable,levels = c("Min","P5","P25","P50","P75","P95","Max"))

  plot <- ggplot(MTTR_variation_data, aes(x=variable,y= as.numeric(value),group=CatCode,colour=CatCode)) +
    geom_line() + geom_point() + facet_wrap(~CatCode,scales = "free_y") +
    geom_text(aes(x="P50",y=max(as.numeric(value))/2,label=ValuePresent),colour="#b2beb5",size=5) +
    geom_text_repel(aes(label=round(as.numeric(value),2)),size=3.5,show.legend = F) +
    labs(title="MTTR Variation (InHours)",x="MTTR Percentile",y="MTTR (Hours)") +
    theme_dark()+ theme(title=element_text(face = "bold.italic"),
                        plot.title = element_text(hjust=0.5),
                        legend.position = "none",
                        panel.border = element_rect(colour = "black", fill=NA),
                        panel.background = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank())

  return(plot)
}

###########################################################

MTTR_Improvement_hr_plot <- function(MTTR_Improvement_data)
{

  MTTR_Improvement_data$variable <- factor(MTTR_Improvement_data$variable,levels=c("MeanMTTR","ImprovedMTTR_ByTGTP75","ImprovedMTTR_ByTGTP50"))

  MTTR_Improvement_data$Pattern <- factor(MTTR_Improvement_data$Pattern,levels = unique(MTTR_Improvement_data[order(MTTR_Improvement_data$TckCnt_OpenCloseDate,decreasing=T),"Pattern"]))

  plot <- ggplot(MTTR_Improvement_data,aes(x=variable,y=round(value,2),fill=variable)) +
    geom_bar(stat = "identity",width = 0.5) + facet_wrap(~Pattern,scales = "free_y") +
    geom_errorbar(aes(ymax=ymax,ymin=round(value,2)),width = 0.25,colour="darkgreen")   +
    geom_text(aes(label=round(value,2)),vjust=1,show.legend = F) +
    geom_text(aes(y=labelaxes,label=PercentImprovement_label),hjust=0,colour="darkgreen",size=3) +
    labs(title="MTTR Improvement (InHours)",y="MTTR (Hours)") + theme_bw() +
    theme(title=element_text(face = "bold.italic"),
          plot.title = element_text(hjust=0.5),
          legend.position = "bottom",
          panel.border = element_rect(colour = "black", fill=NA),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank()) + scale_fill_discrete(name = "Bar Definition")

  return(plot)
}

###########################################################

MTTR_Improvement_day_plot <- function(MTTR_Improvement_data)
{

  MTTR_Improvement_data$variable <- factor(MTTR_Improvement_data$variable,levels=c("MeanMTTR","ImprovedMTTR_ByTGTP75","ImprovedMTTR_ByTGTP50"))

  MTTR_Improvement_data$Pattern <- factor(MTTR_Improvement_data$Pattern,levels = unique(MTTR_Improvement_data[order(MTTR_Improvement_data$TckCnt_OpenCloseDate,decreasing=T),"Pattern"]))

  plot <- ggplot(MTTR_Improvement_data,aes(x=variable,y=round(value,2),fill=variable)) +
    geom_bar(stat = "identity",width = 0.5) + facet_wrap(~Pattern,scales = "free_y") +
    geom_errorbar(aes(ymax=ymax,ymin=round(value,2)),width = 0.25,colour="darkgreen")   +
    geom_text(aes(label=round(value,2)),vjust=1,show.legend = F) +
    geom_text(aes(y=labelaxes,label=PercentImprovement_label),hjust=0,colour="darkgreen",size=3) +
    labs(title="MTTR Improvement (InDays)",y="MTTR (Days)") + theme_bw() +
    theme(title=element_text(face = "bold.italic"),
          plot.title = element_text(hjust=0.5),
          legend.position = "bottom",
          panel.border = element_rect(colour = "black", fill=NA),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank()) + scale_fill_discrete(name = "Bar Definition")

  return(plot)
}

###########################################################
