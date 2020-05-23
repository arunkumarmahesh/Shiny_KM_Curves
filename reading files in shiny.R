library(shiny)
library(openxlsx)
library(lattice)
library(dplyr)
library(latticeExtra)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Kaplan-Meier Curves"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose File',
                accept = c('.xlsx','.csv','.xlsx')
      ),

      tags$hr(),
      downloadButton("outputButton", "Download PDF")
    ),
    mainPanel(
      tableOutput("your_data"),
      plotOutput('plot')


    )
    )
  )
server <- function(input, output) {

  tableData <- reactive({
    inFile <- input$file1
    if (is.null(inFile)){
       return(NULL)

    } else {
    df <- read.xlsx(inFile$datapath,sheet = "Outcomes.info")

df <- subset(df , Outcome.Short.Form %in% c("OS","PFS") & Outcome.statistic %in% "KM curve")

df$all <- paste(df$Outcome.Short.Form,df$Reference.ID, df$Trial.Design.ID, df$Arm.ID, df$`Re-randomised.arm.id`, df$Phase.ID, df$Period.ID,
df$SubArm.Number, df$Statistical.population, df$Outcome.Threshold,sep=",")
df$all <- gsub(",NA","",df$all)
df[,c("Outcome.point.estimate","Outcome.measurement.time","N.at.risk")] <- apply(df[,c("Outcome.point.estimate","Outcome.measurement.time","N.at.risk")], 2, as.numeric)
df$N.at.risk <- ifelse(is.na(df$N.at.risk),"", df$N.at.risk)

for( i in 1:nrow(df)){
  if(nchar(df$all[i])>30){
    df$all[i]<-paste(substring(df$all[i],1,30),"\n",substring(df$all[i],31,60),"\n",
                             substring(df$all[i],61,90),"\n",
                             substring(df$all[i],91,nchar(df$all[i])),sep ="")
  }
}

res<-c()
for(i in unique(df$all)){
  newdf<-df[df$all==i,]
  newdf[,c("Outcome.point.estimate","Outcome.measurement.time")]<-apply(newdf[,c("Outcome.point.estimate","Outcome.measurement.time")],2,as.numeric)
  res<-append(res,"TRUE")
  for(j in 2:length(newdf$Outcome.measurement.time[newdf$all==i])){
    if((newdf$Outcome.measurement.time[j-1]<newdf$Outcome.measurement.time[j])&(newdf$Outcome.point.estimate[j-1]>newdf$Outcome.point.estimate[j]|
                                                                                    newdf$Outcome.point.estimate[j-1]==newdf$Outcome.point.estimate[j])){
      res<-append(res,"TRUE")
    }else{
      res<-append(res,"FALSE")
    }
  }
  df$new[df$all==i]<-res
  res<-c()
}

df$Outcome.measurement.time <- as.numeric(df$Outcome.measurement.time)
df$Outcome.point.estimate <- as.numeric(df$Outcome.point.estimate)
df <- df[order(df$Reference.ID,df$Outcome.measurement.time),]

    }
  })


  output$your_data <- renderTable({
    head(tableData())
  })



  plotinput <- reactive({
    tableData <- tableData()

for(i in unique(tableData[,ncol(tableData)-1])){
  val<-xyplot(Outcome.point.estimate~Outcome.measurement.time,groups=Reference.ID,
              data=tableData[tableData[,ncol(tableData)-1]==i,],grid=T,
              type="s",pch=19,cex=0.5,
              par.settings = list(strip.border = list(col = 0),superpose.symbol=list(pch=19,cex=0.5)),as.table=T,
              auto.key = list(title="Reference.ID", space="right", border=F, columns = 1,cex=0.5),
              main=paste("Endpoint:",i),xlab="Time(Week)",ylab="Percentage Change from baseline",scales=list(relation="free",
                x=list(at=tableData[,"Outcome.measurement.time"][tableData[,ncol(tableData)-1]==i],
                  labels=paste(tableData[,"Outcome.measurement.time"][tableData[,ncol(tableData)-1]==i],"\n\n",tableData[,"N.at.risk"][tableData[,ncol(tableData)-1]==i]))))+
layer(panel.text(tableData$Outcome.measurement.time[tableData$all==i&tableData$new%in%"TRUE"], tableData$Outcome.point.estimate[tableData$all==i&tableData$new%in%"TRUE"],
                     tableData$Outcome.point.estimate[tableData$all==i&tableData$new%in%"TRUE"],cex=0.7,hjust = 2,pos=4, col="magenta"),stat="identity",position="identity")+
    layer(panel.text(tableData$Outcome.measurement.time[tableData$all==i&tableData$new%in%"FALSE"], tableData$Outcome.point.estimate[tableData$all==i&tableData$new%in%"FALSE"],
                     tableData$Outcome.point.estimate[tableData$all==i&tableData$new%in%"FALSE"],cex=0.7,hjust = 2,pos=4, col="black"),stat="identity",position="identity")+

print(val)
}

  })

  output$plot <- renderPlot({
    plotinput()
  })

  output$outputButton <- downloadHandler(
    filename = function() {
                    paste0("Kaplan Meier",".pdf")
                },

   content = function(file){
     pdf(file)
     tableData <- tableData()
  for(i in unique(tableData[,ncol(tableData)-1])){
  val<-xyplot(Outcome.point.estimate~Outcome.measurement.time,groups=Reference.ID,
              data=tableData[tableData[,ncol(tableData)-1]==i,],grid=T,
              type="s",pch=19,cex=0.5,
              par.settings = list(strip.border = list(col = 0),superpose.symbol=list(pch=19,cex=0.5)),as.table=T,
              auto.key = list(title="Reference.ID", space="right", border=F, columns = 1,cex=0.5),
              main=paste("Endpoint:",i),xlab="Time(Week)",ylab="Percentage Change from baseline",scales=list(relation="free",
                x=list(at=tableData[,"Outcome.measurement.time"][tableData[,ncol(tableData)-1]==i],
                  labels=paste(tableData[,"Outcome.measurement.time"][tableData[,ncol(tableData)-1]==i],"\n\n",tableData[,"N.at.risk"][tableData[,ncol(tableData)-1]==i]))))*
layer(panel.text(tableData$Outcome.measurement.time[tableData$all==i&tableData$new%in%"TRUE"], tableData$Outcome.point.estimate[tableData$all==i&tableData$new%in%"TRUE"],
                     tableData$Outcome.point.estimate[tableData$all==i&tableData$new%in%"TRUE"],cex=0.7,hjust = 2,pos=4, col="magenta"),stat="identity",position="identity")*
    layer(panel.text(tableData$Outcome.measurement.time[tableData$all==i&tableData$new%in%"FALSE"], tableData$Outcome.point.estimate[tableData$all==i&tableData$new%in%"FALSE"],
                     tableData$Outcome.point.estimate[tableData$all==i&tableData$new%in%"FALSE"],cex=0.7,hjust = 2,pos=4, col="black"),stat="identity",position="identity")

print(val)
}

    dev.off()

   }
  )

}

shinyApp(ui = ui, server = server)
