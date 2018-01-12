
#install.packages("rsconnect")
#install.packages("plotly")
#install.packages("shinydashboard")
#install.packages("rpart")
#install.packages("ISLR")
#install.packages("pROC")
#install.packages("caret")
#install.packages("sqldf")
#install.packages("tree")

library(rsconnect)
library(shiny)
library(shinydashboard)
library(plotly)
library(ISLR)
library(pROC)
library(caret)
library(sqldf)
library(rpart)
library(tree)

#loading the dataset
hrdata = read.csv("HR_analytics_data_1000.csv"
                  ,header = T
                  ,sep = ","
                  ,colClasses = c("factor","factor","factor","factor","factor","factor","factor"
                                  ,"character","character"
                                  ,"factor","factor","factor","factor","factor","numeric"
                                  ,"character"
                                  ,"factor","factor","factor","factor"
                                  ,"character"
                                  ,"factor","factor","factor","factor"
                                  ,"numeric","numeric","numeric","numeric","numeric"
                                  ,"numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"
                                  ,"factor","factor"
                                  ,"factor","factor","numeric","numeric"
                                  ,"numeric","numeric")
)

#verifying the data loaded
str(hrdata)
names(hrdata)

#column level changes
hrdata$performancechange = hrdata$performance2014level/hrdata$performance2013level;
colnames(hrdata) <- gsub("\\.","_",colnames(hrdata))

#correlation matrix
cor(hrdata[,c('Hourly_Rate','Pnormal','performance2013level','performance2014level'
)])

par(mfcol = c(1,1))
pairs(hrdata[,c('Hourly_Rate','Pnormal'
                ,'performance2013level','performance2014level','Region'
)]
,cex.labels = 1.5)

#dropping some columns
drops = c("Hire_Date","Last_Hire.Date","Effective_Date","Vol_Invol","SP"
          ,'Open_Date','Location'
          ,'Period','Ethnic','Type','Gender','Grade','FT_PT','Action_Reason'
          ,'Ethnic_Desc','Market_Type','MT_Desc','Store_Class','left_numeric'
          ,'still_employed','still_employed_numeric')
hrdatasubset = hrdata[,!names(hrdata) %in% drops]

#create table for key metrics
mktSTD <- sqldf("select market_class, job_title, max(STD_DEV) new_STD_DEV from hrdatasubset group by market_class, job_title")

#training and test Dataset
set.seed(2016)

levels(hrdata$left) <- make.names(levels(factor(hrdata$left)))

inTrain=createDataPartition(hrdata$left,p=0.70,list=F)
train=hrdata[inTrain,]
test=hrdata[-inTrain,]
dim(train)
names(train)
summary(train)

#decision tree - predictive modeling

set.seed(200)

#tree
treefit_1 = rpart(left ~ Pnormal+Hourly_Rate+
                    +performance2013level+performance2014level
                  +Region, method="class"
                  ,control = rpart.control(nobs = nrow(train)
                                           ,mincut = 0
                                           ,minsize = 1
                                           ,mindev = 0.01)
                  ,data = train)

summary(treefit_1)
printcp(treefit_1)

str(train)

#ploting of tree
par(mar = rep(0.0, 4))
plot(treefit_1, uniform=TRUE)
text(treefit_1, use.n=TRUE, all=TRUE, cex=1, pretty = 1)

#pruning the tree
pfit<- prune(treefit_1, cp= treefit_1$cptable[treefit_1$nsplit==2,"CP"])

plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8, pretty = 1)
post(pfit,title = "Pruned Classification Tree")

#shiny dashboard app
ui = dashboardPage(
  dashboardHeader(title = "Workforce Management Analytical Tool", titleWidth = 450),
  
  dashboardSidebar(disable = TRUE),

  dashboardBody(
    fluidRow(
      box(title = "Inputs", background = "navy", 
          
          selectInput("Market_Class","Market", c("Rural","SubUrban","Urban","Metropolitan"),selected=NULL, multiple = FALSE, selectize = TRUE),
          
          selectInput("Region", "Region", c("22","23","25","26","27","40","44","49","50","53","54","56","57","58"), selected = NULL, multiple = FALSE, selectize = TRUE),
          
          selectInput("Job_Title","Job Title", c("Sales Associate","Sr. Sales Associate","Store Manager","Sales Consultant"),selected=NULL, multiple = FALSE, selectize = TRUE),
          
          sliderInput("Hourly_Rate",h5("Hourly Wage ($)"),value=10,min=7.25,max=30),
          
          sliderInput("performancePrevlevel", h5("Previous Performance Review"),value=1.00,min=-1.00,max=3.50, step = 0.05),
          
          sliderInput("performanceCurrlevel", h5("Current Performance Review"),value=1.00,min=-1.00,max=3.50, step = 0.05),
          
          actionButton("go", "Run")
      ),
      box(title = "Results", background = "navy",
          infoBox("Chances Employee will Quit",verbatimTextOutput("print1"),fill = TRUE , color = "olive"),
          infoBox("Employee Status", verbatimTextOutput("status"),fill = TRUE, width = 8, color = "olive")) ,
      
      box(title = "Decrease Chance of Employee Leaving", background = "navy",
          
          numericInput("max_raise","Maximum raise amount that can be allocated for this employee ($)", 0, min = 0, max = NA, step = NA, width = 410),
          
          sliderInput("raise_amount", h5("Select Raise"), value = 0.55, min = 0.00, max = 2.01)
      ),
      box(title = "Change in probability", background = "navy",
          infoBox("prob",verbatimTextOutput("prob"),fill = TRUE , color = "olive", width = 8)
      ))))


server <- function(input, output,session) {
  mydata = reactive({
    m = as.double((hrdatasubset[hrdatasubset$Job_Title == input$Job_Title & hrdatasubset$Market_Class == input$Market_Class,"P50"][1]),digits=5)
    n = as.double((mktSTD[mktSTD$Job_Title == input$Job_Title & mktSTD$Market_Class == input$Market_Class,"new_STD_DEV"][1]),digits=5)
    p = as.double(((as.double(input$Hourly_Rate,digits=5)-m)/n),digits=5)
    predicted <- predict(pfit, newdata = data.frame(Pnormal = p , Hourly_Rate =  input$Hourly_Rate , performance2013level = input$performancePrevlevel ,performance2014level =  input$performanceCurrlevel , Region = as.factor(input$Region)))[,2]
    return(predicted)
  })  
  
  output$print1<-renderText({
    {
      mydata()}
  })
  
  
  mydata2 = reactive({
    mktPerfInd <- paste0("select avg(performance2014level) mktPerfInd from hrdatasubset where Market_Class='",input$Market_Class,"' and Job_Title = '",input$Job_Title,"' group by Market_Class, Job_Title;")
    indPerfInd <- as.numeric(input$performanceCurrlevel)
    mktWageInd <- paste0("select sum(Hourly_Rate)/sum(P50) mktWageInd from hrdatasubset where Market_Class='",input$Market_Class,"' and Job_Title = '",input$Job_Title,"' group by Market_Class, Job_Title;")
    indWageInd <- paste0("select distinct ",input$Hourly_Rate,"/P50 from hrdatasubset where Market_Class='",input$Market_Class,"' and Job_Title = '",input$Job_Title,"' group by Market_Class, Job_Title;")
    result<- ifelse ((indPerfInd<as.numeric(sqldf(mktPerfInd)) & as.numeric(sqldf(indWageInd)>sqldf(mktWageInd))),"Summary:\nThe employee is \b Underperforming \b and is Overpaid\nDecision:\nTerminate", ifelse ((indPerfInd<as.numeric(sqldf(mktPerfInd)) & as.numeric(sqldf(indWageInd)<sqldf(mktWageInd))),"Summary:\nThe employee is \b Underperforming \b and Underpaid\nDecision:\n(1)Increase the wage to motivate the employee\n(2)Revaluate the performance in six months", ifelse ((indPerfInd>as.numeric(sqldf(mktPerfInd)) & as.numeric(sqldf(indWageInd)<sqldf(mktWageInd))),"Summary:\nThe employee is \b Overperforming \b and Underpaid\nDecision:\nIncrease the wage as the cost of employee turnover is high","Summary:\nThe employee is \b Overperforming \b and Overpaid\nDecision:\n(1)Increase the wage to keep the employee motivated\n(2)The increase depends upon the last appraisal and the current wage index of the employee")
    )
    )
    
    return(result)
  })
  
  output$status<-renderText({
    {
      mydata2()}
  })
  
  
  
  observe({
    mydata3 = reactive({
      val<-input$max_raise
      val1<-input$raise_amount
      updateSliderInput(session,"raise_amount",max=val, value = val1)
      m = (hrdatasubset[hrdatasubset$Job_Title == input$Job_Title & hrdatasubset$Market_Class == input$Market_Class,"P50"][1])
      n = (mktSTD[mktSTD$Job_Title == input$Job_Title & mktSTD$Market_Class == input$Market_Class,"new_STD_DEV"][1])
      p = as.numeric(((input$Hourly_Rate+input$raise_amount)-m)/as.numeric(n))
      predicted <- predict(pfit, newdata = data.frame(Pnormal = p , Hourly_Rate =  (input$Hourly_Rate+input$raise_amount) , performance2013level = input$performancePrevlevel ,performance2014level =  input$performanceCurrlevel , Region = as.factor(input$Region)))[,2]
      return(predicted)  
    })  
    
    output$prob<-renderText({
      mydata3()
    })
    
  })
}



#Integrating UI and SERVER components of the application
shinyApp(ui = ui, server = server)
