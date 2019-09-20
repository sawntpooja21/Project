
load(file="E:/POOJA/2. MSC PART 2/demo/IND_shp.RData")
b <- read.csv("E:/POOJA/2. MSC PART 2/demo/crimes.csv",header = T,sep=',')
names(b)
# Set the year for map 
year <- c("2001","2002","2003","2004","2005","2006","2007","2008",
          "2009","2010","2011","2012")
source("E:/POOJA/2. MSC PART 2/demo/stateCrime.R", local=TRUE)
states = b$state

#Population Data
pdata <- read.csv("E:/POOJA/2. MSC PART 2/proj/population.csv",header = T,sep=',')
colnames(pdata) <- c("STATEUT","p2001","p2011","f2001","f2011")

#literacy
litdata <- read.csv("E:/POOJA/2. MSC PART 2/proj/LiteracyRate.csv",header = T,sep=',')
colnames(litdata) <- c("STATEUT","lit2011","lit2001")


server <- shinyServer(function(input, output, session) {
 
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    
    
    if(!is.null(inFile)){
    df <- read.csv(inFile$datapath, header = T, sep = ',')
    colnames(df) <- c("STATEUT","DISTRICT","YEAR","Rape","Kidnapping","DowryDeaths","Assault","Insult","Cruelty","Importation")

    output$wordplot= renderPlot({
      text <- readLines("E:\\POOJA\\2. MSC PART 2\\proj\\word.txt")
      docs <- Corpus(VectorSource(text))
      inspect(docs)
      toSpace<-content_transformer(function(x,pattern)gsub(pattern,"",x))
      docs <- tm_map(docs,toSpace,"/")
      docs <- tm_map(docs,toSpace,"@")
      docs <- tm_map(docs,content_transformer(tolower))

      docs <- tm_map(docs,removeNumbers)
      docs <- tm_map(docs,removeWords,stopwords("english"))
      docs <- tm_map(docs,removeWords,c("blabla1","blabla2"))
      docs <- tm_map(docs,removePunctuation)
      docs <- tm_map(docs,stripWhitespace)
      docs <- tm_map(docs,stemDocument)

      dtm <- TermDocumentMatrix(docs)
      m <- as.matrix(dtm)
      v <- sort(rowSums(m),decreasing = TRUE)
      d <- data.frame(word=names(v),freq=v)
      head(d,10)
      set.seed(1234)

      wordcloud(words = d$word,freq = d$freq,min.freq = 1,max.words =600,scale=c(5, 0.5),use.r.layout = FALSE,random.order = FALSE,rot.per = 0.30,colors = brewer.pal(8,"Set1"))

    })

    #DistrictWise
    updateSelectInput(session,inputId = 'year', label='YEAR', choices = df$YEAR, selected = df$YEAR)
    updateSelectInput(session,inputId = 'state', label='STATE', choices = df$STATEUT, selected = "ASSAM")
  
    #AverageCrime
    updateSelectInput(session,inputId = 'agg_crimes', label='Select Crime', choices= names(df[-1:-3]),selected = "Rape")
    updateSelectInput(session,inputId = 'agg_state', label='STATE', choices = df$STATEUT, selected = "ASSAM")
   
    #LinearRegression
    updateSelectInput(session,inputId = 'linearstate', label='STATE', choices = df$STATEUT, selected = "ASSAM")
    
    #Kmeans
    updateSelectInput(session,inputId = 'state_cluster', label='STATE', choices = df$STATEUT, selected ="ASSAM")

    #Dendogram
    updateSelectInput(session,inputId = 'dendo_year', label='Select Year', choices = df$YEAR, selected = df$YEAR)
    
    #Map
    updateSelectInput(session,inputId = 'year_map', label='Select Year', choices = df$YEAR, selected = df$YEAR)  
    
    return(df)
  }
  })
 
  updateSelectizeInput(session, 'id', choices = year, server = TRUE,selected="2001")
  toggle("inputBox")
  toggle("id")
  
 observe({
   
   x <- data()%>%filter(data()$STATEUT == input$state & data()$YEAR == input$year) %>%select(DISTRICT)
   x1 <- data()%>%filter(data()$STATEUT == input$state & data()$YEAR == input$year) %>%select(Rape,DowryDeaths,Kidnapping,Assault,Insult,Cruelty,Importation)
   a <- data.frame(list(c(x,x1)))
   result <- a %>% filter(DISTRICT != "TOTAL")
   
   output$View_rape = renderPlot({
     p <-  ggplot(result,aes(DISTRICT,Rape,color=Rape)) + geom_bar(stat="identity", fill="steelblue")+
       geom_text(aes(label=Rape), vjust=1.6, color="white", size=3.5)+
       theme_minimal() 
     p + theme(axis.text.x =element_text(size  = 10,angle = 45,hjust = 1,vjust = 1))
   })
   output$RapeBox <- renderValueBox({
     maxvalue <- max(result$Rape) 
     result1 <- result %>% filter(Rape == maxvalue) %>% select(DISTRICT)
     
     valueBox(
       maxvalue, result1, icon = icon("align-justify", lib = "glyphicon"),
       color = "light-blue"
     )
   })
   
  
   output$View_dowry = renderPlot({
     p <-  ggplot(result,aes(DISTRICT,DowryDeaths,color=DowryDeaths)) + geom_bar(stat="identity", fill="steelblue")+
       geom_text(aes(label=DowryDeaths), vjust=1.6, color="white", size=3.5)+
       theme_minimal() 
     p + theme(axis.text.x =element_text(size  = 10,angle = 45,hjust = 1,vjust = 1))
     
   })
   output$DowryBox<- renderValueBox({
     maxvalue <- max(result$DowryDeaths) 
     result1 <- result %>% filter(DowryDeaths == maxvalue) %>% select(DISTRICT)
     
     valueBox(
       maxvalue, result1, icon = icon("align-justify", lib = "glyphicon"),
       color = "light-blue"
     )
   })
   
   
   output$View_kidnapping = renderPlot({
     p <-  ggplot(result,aes(DISTRICT,Kidnapping,color=Kidnapping)) + geom_bar(stat="identity", fill="steelblue")+
       geom_text(aes(label=Kidnapping), vjust=1.6, color="white", size=3.5)+
       theme_minimal() 
     p + theme(axis.text.x =element_text(size  = 10,angle = 45,hjust = 1,vjust = 1))
     
   })
   output$KidBox<- renderValueBox({
     maxvalue <- max(result$Kidnapping) 
     result1 <- result %>% filter(Kidnapping == maxvalue) %>% select(DISTRICT)
     
     valueBox(
       maxvalue, result1, icon = icon("align-justify", lib = "glyphicon"),
       color = "light-blue"
     )
   })
   
   output$View_Assault = renderPlot({
     p <-  ggplot(result,aes(DISTRICT,Assault,color=Assault)) + geom_bar(stat="identity", fill="steelblue")+
       geom_text(aes(label=Assault), vjust=1.6, color="white", size=3.5)+
       theme_minimal() 
     p + theme(axis.text.x =element_text(size  = 10,angle = 45,hjust = 1,vjust = 1))
     
   })
   output$AssaultBox<- renderValueBox({
     maxvalue <- max(result$Assault) 
     result1 <- result %>% filter(Assault == maxvalue) %>% select(DISTRICT)
     
     valueBox(
       maxvalue, result1, icon = icon("align-justify", lib = "glyphicon"),
       color = "light-blue"
     )
   })
   
   output$View_insult = renderPlot({
     p <-  ggplot(result,aes(DISTRICT,Insult,color=Insult)) + geom_bar(stat="identity", fill="steelblue")+
       geom_text(aes(label=Insult), vjust=1.6, color="white", size=3.5)+
       theme_minimal() 
     p + theme(axis.text.x =element_text(size  = 10,angle = 45,hjust = 1,vjust = 1))
     
   })
   output$InsultBox<- renderValueBox({
     maxvalue <- max(result$Insult) 
     result1 <- result %>% filter(Insult == maxvalue) %>% select(DISTRICT)
     
     valueBox(
       maxvalue, result1, icon = icon("align-justify", lib = "glyphicon"),
       color = "light-blue"
     )
   })
   
   output$View_cruelty = renderPlot({
     p <-  ggplot(result,aes(DISTRICT,Cruelty,color=Cruelty)) + geom_bar(stat="identity", fill="steelblue")+
       geom_text(aes(label=Cruelty), vjust=1.6, color="white", size=3.5)+
       theme_minimal() 
     p + theme(axis.text.x =element_text(size  = 10,angle = 45,hjust = 1,vjust = 1))
     
   })
   output$CrueltyBox <- renderValueBox({
     maxvalue <- max(result$Cruelty) 
     result1 <- result %>% filter(Cruelty == maxvalue) %>% select(DISTRICT)
     
     valueBox(
       maxvalue, result1, icon = icon("align-justify", lib = "glyphicon"),
       color = "light-blue"
     )
   })
   
   output$View_import = renderPlot({
     p <-  ggplot(result,aes(DISTRICT,Importation,color=Importation)) + geom_bar(stat="identity", fill="steelblue")+
       geom_text(aes(label=Importation), vjust=1.6, color="white", size=3.5)+
       theme_minimal() 
     p + theme(axis.text.x =element_text(size  = 10,angle = 45,hjust = 1,vjust = 1))
     
   })
   output$ImportBox <- renderValueBox({
     maxvalue <- max(result$Importation) 
     result1 <- result %>% filter(Importation == maxvalue) %>% select(DISTRICT)
     
     valueBox(
       maxvalue, result1, icon = icon("align-justify", lib = "glyphicon"),
       color = "light-blue"
     )
   })
   

})

 #Average-AcrossIndia
 observe({
    
    output$averageplot <- renderPlot({
       agg <- data()%>%filter(data()$DISTRICT != "TOTAL") %>%select(STATEUT,YEAR,input$agg_crimes)
       results<- data.frame(agg) 
       names(results)<- c("STATEUT","YEAR","Crimes")
       results %>%group_by(STATEUT) %>%summarise(mean_run = mean(Crimes)) %>% 
                 ggplot(aes(x = STATEUT, y = mean_run, fill = STATEUT)) +
                 geom_bar(stat = "identity") +theme_light() +labs(x = "States",y = "crime") +theme(axis.text.x =element_text(size  = 10,angle = 45,hjust = 1,vjust = 1))
    })
    
    
 })
 
 #Average-STATESUT
 observe({

    output$averageplot2 <- renderPlotly({
       agg <- data()%>%filter(data()$STATEUT== input$agg_state & data()$DISTRICT != "TOTAL" ) %>%select(STATEUT,DISTRICT,YEAR,DowryDeaths,Kidnapping,Insult,Importation,Rape,Cruelty,Assault)
       ex1 <- agg %>%
          group_by(YEAR) %>%
          summarise(Kidnapping = mean(Kidnapping),
                    Rape=mean(Rape),
                    Importation=mean(Importation),
                    DowryDeaths=mean(DowryDeaths))

       plot_ly(ex1, x = ~YEAR, y = ~Kidnapping, name = 'Kidnapping & Abduction', type = 'scatter', mode = 'lines',
               line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
         add_trace(y = ~Rape, name = 'Rape', line = list(color = 'rgb(22, 96, 167)', width = 4)) %>%
         add_trace(y = ~Importation, name = 'Importation of Girls', line = list(color = 'rgb(255,165,0)', width = 4)) %>%
          add_trace(y = ~DowryDeaths, name = 'Dowry Deaths', line = list(color = 'darkgreen', width = 4)) %>%
         
         layout(title = "Average Crime Rate (2001 - 2012)",
                xaxis = list(title = "Year 2001 - 2012"),
                yaxis = list (title = "Crime"))
       
    })
    output$averageplot3 <- renderPlotly({
       agg <- data()%>%filter(data()$STATEUT== input$agg_state & data()$DISTRICT != "TOTAL" ) %>%select(STATEUT,DISTRICT,YEAR,Assault,Cruelty,Insult)
       ex1 <- agg %>%
          group_by(YEAR) %>%
          summarise(Assault=mean(Assault),
                    Cruelty=mean(Cruelty),
                    Insult=mean(Insult))
       
       plot_ly(ex1, x = ~YEAR, y = ~Assault, name = 'Assault on women with intent to outrage her modesty', type = 'scatter', mode = 'lines',
               line = list(color = '	rgb(255, 191, 0)', width = 4)) %>%
         add_trace(y = ~Cruelty, name = 'Cruelty by Husband or his Relatives', line = list(color = 'rgb(255, 0, 191)', width = 4)) %>%
         add_trace(y = ~Insult, name = 'Insult to modesty of Women', line = list(color = 'rgb(0, 255, 64)', width = 4)) %>%
         layout(title = "Average Crime Rate (2001 - 2012)",
                xaxis = list(title = "Year 2001 - 2012"),
                yaxis = list (title = "Crime"))
 
    })
 })
 
 #Linear Regression
 observe({

    output$linearplot <- renderPlot({
       title <- paste("Incidence of ",tolower(input$radio),"in ",tolower(input$linearstate))
       linear1 <- data()%>% filter( data()$DISTRICT == "TOTAL" & data()$STATEUT == input$linearstate) %>%select(YEAR,Rape,DowryDeaths,Kidnapping,Insult,Assault,Cruelty)
       l<- data.frame(linear1) 

       if(input$radio =="TOTAL"){
           output$summary_linear <- renderPrint({
               m1 <-lm(YEAR ~ Rape+DowryDeaths+Kidnapping+Insult+Assault+Cruelty,l)  
               summary(m1)
              #
           })

           title1<-paste("Incidence of Total Crime Against Women in",input$linearstate)
            linear1 %>%group_by(YEAR) %>%summarise(mean_run = sum(Rape,DowryDeaths,Kidnapping,Insult,Assault,Cruelty)) %>%
               ggplot(aes(x = YEAR, y = mean_run)) +
               geom_point() + geom_smooth(method="lm") + ylab('TOTAL CRIMES AGAINST WOMEN') +ggtitle(title1)+theme_light()
        }
        else{
           linear <- data()%>% filter( data()$DISTRICT == "TOTAL" & data()$STATEUT == input$linearstate) %>%select(YEAR,input$radio)
           results<-as.data.frame(linear)
           names(results)<- c("YEAR","Crimes")
          
           output$summary_linear <- renderPrint({
              m1 <-lm(YEAR ~ Crimes,results)
              summary(m1)
              
           }) 
           
           newdata <- read.csv("E:/POOJA/2. MSC PART 2/proj/data2.csv",header = T,sep=',')
           colnames(newdata) <- c("ST","STATEUT","DISTRICT","YEAR","Rape","Kidnapping","DowryDeaths","Assault","Insult","Cruelty","Importation")
           newdata1 <- newdata %>% filter( newdata$DISTRICT == "TOTAL" & newdata$STATEUT == input$linearstate) %>%select(YEAR,input$radio)
           result1 <- as.data.frame(newdata1)
           names(result1)<- c("YEAR","Crimes")
           
           m1 <-lm(Crimes ~ YEAR  ,results)

           output$PredictOutput <- renderPrint({
              predict(m1,YEAR="2013",interval="predict")
             
           })
           output$ActualOutput <- renderPrint({
             result1
           })
           
            ggplot(results, aes(YEAR,Crimes)) +geom_point() + geom_smooth(method="lm") + ylab(input$radio) + ggtitle(title)+theme_light()
        }
    })
 })
 
 #PerLakh
 observe({
    if(input$radiolakh == "2001"){
    a <- data()%>% filter( data()$DISTRICT=="TOTAL" & data()$YEAR == "2001") %>%select(YEAR,STATEUT,Rape,DowryDeaths,Kidnapping,Insult,Assault,Cruelty,Importation)
    crime1<- a %>%group_by(STATEUT) %>%summarise(mean_run = sum(Rape,DowryDeaths,Kidnapping,Insult,Assault,Cruelty,Importation)) 
    newdata <- pdata[order(pdata$STATEUT),] 
    result1 <- cbind(crime1,newdata)
    
    newresult <- data.frame(list(c(result1[-3])))
    newresult1 <- data.frame(list(c(newresult[-1])))
    
    
    b <- newresult1 %>%
       mutate_all(funs(as.character(.)), newresult1$f2001) %>%
       mutate_all(funs(gsub(",", "", .)), newresult1$f2001) %>%
       mutate_all(funs(as.numeric(.)), newresult1$f2001)
    
    ans <- cbind(newresult,b)
    ans1 <- ans[-2:-5]
    cans <- ans1 %>% mutate(rate1 = (mean_run*100000) / f2001) %>% arrange(desc(rate1))
   
    arrange1 <-  head(arrange(cans,desc(rate1)),5)
    c <- paste(arrange1$STATEUT,"(",arrange1$f2011,")")
    stateCrime <- str_replace_all(c," ","")
    
    arrange2 <-  head(arrange(cans,order(rate1)),5)
    c <- paste(arrange2$STATEUT,"(",arrange2$f2011,")")
    stateCrime2 <- str_replace_all(c," ","")
    
    # Create a data frame to primt the top 5 ofenders
    labels <- data.frame(
       MinimumCrime =as.vector(stateCrime2),
       MaximumCrime = as.vector(stateCrime))
    
    maxstates <- labels %>% select (MaximumCrime)
    output$perlakhtext <- renderPrint({
       data.frame(maxstates)
    })
    
    minstates <- labels %>% select (MinimumCrime)
    output$perlakhtext2 <- renderPrint({
       data.frame(minstates)
    })
    
    output$pop <- renderPlotly({
       p1 <- plot_ly(cans,x = ~rate1, y = ~reorder(STATEUT, rate1), name = 'Crime Rate',
                     type = 'bar', orientation = 'h',
                     marker = list(color = 'YlOrRd',
                                   line = list(color = 'YlOrRd', width = 1)))
       
       
                                                                         
       p <- subplot(p1)%>% layout( title = "States VS Crime Rate per lakh Women ",
                                   xaxis = list(title= "Crime Rate"),
                                   legend = list(x = 0.029, y = 1.038,font = list(size = 10)),
                                   margin = list(l = 100, r = 20, t = 70, b = 70),
                                   paper_bgcolor = 'rgb(248, 248, 255)',
                                   plot_bgcolor = 'rgb(248, 248, 255)')
       
    })
   }
   else if(input$radiolakh == "2011"){
       a <- data()%>% filter( data()$DISTRICT=="TOTAL" & data()$YEAR == "2011") %>%select(YEAR,STATEUT,Rape,DowryDeaths,Kidnapping,Insult,Assault,Cruelty,Importation)
       crime1<- a %>%group_by(STATEUT) %>%summarise(mean_run = sum(Rape,DowryDeaths,Kidnapping,Insult,Assault,Cruelty,Importation)) 
       newdata <- pdata[order(pdata$STATEUT),] 
       result1 <- cbind(crime1,newdata)
       
       newresult <- data.frame(list(c(result1[-3])))
       newresult1 <- data.frame(list(c(newresult[-1])))
       
       
       b <- newresult1 %>%
          mutate_all(funs(as.character(.)), newresult1$f2011) %>%
          mutate_all(funs(gsub(",", "", .)), newresult1$f2011) %>%
          mutate_all(funs(as.numeric(.)), newresult1$f2011)
       
       
       ans <- cbind(newresult,b)
       ans1 <- ans[-2:-5]
       cans <- ans1 %>% mutate(rate1 = (mean_run*100000) / f2011) %>% arrange(desc(rate1))

       arrange1 <-  head(arrange(cans,desc(rate1)),5)
       c <- paste(arrange1$STATEUT,"(",arrange1$f2011,")")
       stateCrime <- str_replace_all(c," ","")
       
       arrange2 <-  head(arrange(cans,order(rate1)),5)
       c <- paste(arrange2$STATEUT,"(",arrange2$f2011,")")
       stateCrime2 <- str_replace_all(c," ","")
       
       # Create a data frame to primt the top 5 ofenders
       labels <- data.frame(
          MinimumCrime =as.vector(stateCrime2),
          MaximumCrime = as.vector(stateCrime))
       
       maxstates <- labels %>% select (MaximumCrime)
       output$perlakhtext <- renderPrint({
          data.frame(maxstates)
       })
       
       minstates <- labels %>% select (MinimumCrime)
       output$perlakhtext2 <- renderPrint({
          data.frame(minstates)
       })
       
       output$pop <- renderPlotly({
          p1 <- plot_ly(cans,x = ~rate1, y = ~reorder(STATEUT, rate1), name = 'Crime Rate',
                        type = 'bar', orientation = 'h',
                        marker = list(color = 'Reds',
                                      line = list(color = 'Reds', width = 1)))
          p <- subplot(p1)%>% layout( title = "States VS Crime Rate per lakh Women ",
                                      xaxis = list(title= "Crime Rate"),
                                      legend = list(x = 0.029, y = 1.038,font = list(size = 10)),
                                      margin = list(l = 100, r = 20, t = 70, b = 70),
                                      paper_bgcolor = 'rgb(248, 248, 255)',
                                      plot_bgcolor = 'rgb(248, 248, 255)')
          
       })
    }
 })
 
 #literacy-highest rate crime
 observe({
   if(input$radioliteracy == "2001"){
    a <- data()%>% filter( data()$DISTRICT=="TOTAL" & data()$YEAR == "2001") %>%select(YEAR,STATEUT,Rape,DowryDeaths,Kidnapping,Insult,Assault,Cruelty,Importation)
    crime1<- a %>%group_by(STATEUT) %>%summarise(mean_run = sum(Rape,DowryDeaths,Kidnapping,Insult,Assault,Cruelty,Importation)) 
    newdata <- pdata[order(pdata$STATEUT),] 
    newlit <- litdata[order(litdata$STATEUT),] 
    result1 <- cbind(crime1,newdata,newlit)
   
    newresult <- data.frame(list(c(result1[-3])))
    newresult1 <- data.frame(list(c(newresult[-1])))
    
    
    b <- newresult1 %>%
       mutate_all(funs(as.character(.)), newresult1$f2001) %>%
       mutate_all(funs(gsub(",", "", .)), newresult1$f2001) %>%
       mutate_all(funs(as.numeric(.)), newresult1$f2001)
    
    
    ans <- cbind(newresult,b)
    ans1 <- ans[-2:-5]
    cans <- ans1 %>% mutate(rate1 = (mean_run*100000) / f2001) %>% arrange(desc(rate1))
    
    arrange1 <-  head(arrange(cans,desc(rate1)),10) %>% select(STATEUT,lit2001,rate1)
    
    colnames(arrange1) <- c('STATEUT','Literacy','CrimeRate')
   
    output$literacy1 <- renderPlotly({
    p <- plot_ly(arrange1, labels = ~STATEUT, values = ~CrimeRate, type = 'pie') %>%
       layout(title = 'crime rate',
              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              legend = list (orientation ='h') )
    p
    })
    
    dfm <- melt(arrange1[,c('STATEUT','Literacy','CrimeRate')],)
    output$ggbar <- renderPlot({

      ggplot(dfm,aes(x = STATEUT,y = value)) + 
        geom_bar(aes(fill = variable),stat = "identity",position = "dodge") + theme_light() +
        scale_y_log10() +theme(axis.text.x = element_text(size  = 10, angle = 45,hjust = 1,vjust = 1)) +
        labs(x = "States and Union Territories",y = "Literacy rate and Crime rate",title = paste("Crime Rate in relation of literacy rate"))
    })
   }
   else if (input$radioliteracy == "2011"){
     a <- data()%>% filter( data()$DISTRICT=="TOTAL" & data()$YEAR == "2011") %>%select(YEAR,STATEUT,Rape,DowryDeaths,Kidnapping,Insult,Assault,Cruelty,Importation)
     crime1<- a %>%group_by(STATEUT) %>%summarise(mean_run = sum(Rape,DowryDeaths,Kidnapping,Insult,Assault,Cruelty,Importation)) 
     newdata <- pdata[order(pdata$STATEUT),] 
     newlit <- litdata[order(litdata$STATEUT),] 
     result1 <- cbind(crime1,newdata,newlit)
     
     newresult <- data.frame(list(c(result1[-3])))
     newresult1 <- data.frame(list(c(newresult[-1])))
     
     
     b <- newresult1 %>%
       mutate_all(funs(as.character(.)), newresult1$f2011) %>%
       mutate_all(funs(gsub(",", "", .)), newresult1$f2011) %>%
       mutate_all(funs(as.numeric(.)), newresult1$f2011)
     
     
     ans <- cbind(newresult,b)
     ans1 <- ans[-2:-5]
     cans <- ans1 %>% mutate(rate1 = (mean_run*100000) / f2011) %>% arrange(desc(rate1))
     
     arrange1 <-  head(arrange(cans,desc(rate1)),10) %>% select(STATEUT,lit2011,rate1)
     
     colnames(arrange1) <- c('STATEUT','Literacy','CrimeRate')
     
     output$literacy1 <- renderPlotly({
       p <- plot_ly(arrange1, labels = ~STATEUT, values = ~CrimeRate, type = 'pie') %>%
         layout(title = 'crime rate',
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                legend = list (orientation ='h') )
       p
     })
     
     dfm <- melt(arrange1[,c('STATEUT','Literacy','CrimeRate')],)
     output$ggbar <- renderPlot({
       
       ggplot(dfm,aes(x = STATEUT,y = value)) + 
         geom_bar(aes(fill = variable),stat = "identity",position = "dodge") + theme_light() +
         scale_y_log10() +theme(axis.text.x = element_text(size  = 10, angle = 45,hjust = 1,vjust = 1)) +
         labs(x = "States and Union Territories",y = "Literacy rate and Crime rate",title = paste("Crime Rate in relation of literacy rate"))
     })
   }
   
 })
 
 
 #literacy-lowest rate crime
 observe({
   if(input$radioliteracy == "2001"){
     a <- data()%>% filter( data()$DISTRICT=="TOTAL" & data()$YEAR == "2001") %>%select(YEAR,STATEUT,Rape,DowryDeaths,Kidnapping,Insult,Assault,Cruelty,Importation)
     crime1<- a %>%group_by(STATEUT) %>%summarise(mean_run = sum(Rape,DowryDeaths,Kidnapping,Insult,Assault,Cruelty,Importation)) 
     newdata <- pdata[order(pdata$STATEUT),] 
     newlit <- litdata[order(litdata$STATEUT),] 
     result1 <- cbind(crime1,newdata,newlit)
     
     newresult <- data.frame(list(c(result1[-3])))
     newresult1 <- data.frame(list(c(newresult[-1])))
     
     
     b <- newresult1 %>%
       mutate_all(funs(as.character(.)), newresult1$f2001) %>%
       mutate_all(funs(gsub(",", "", .)), newresult1$f2001) %>%
       mutate_all(funs(as.numeric(.)), newresult1$f2001)
     
     
     ans <- cbind(newresult,b)
     ans1 <- ans[-2:-5]
     cans <- ans1 %>% mutate(rate1 = (mean_run*100000) / f2001) %>% arrange(desc(rate1))
     
     arrange1 <-  head(arrange(cans,order(rate1)),10) %>% select(STATEUT,lit2001,rate1)
     
     colnames(arrange1) <- c('STATEUT','Literacy','CrimeRate')
     
     output$literacy1low <- renderPlotly({
       p <- plot_ly(arrange1, labels = ~STATEUT, values = ~CrimeRate, type = 'pie') %>%
         layout(title = 'crime rate',
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                legend = list (orientation ='h') )
       p
     })
     
     dfm <- melt(arrange1[,c('STATEUT','Literacy','CrimeRate')],)
     output$ggbarlow <- renderPlot({
       
       ggplot(dfm,aes(x = STATEUT,y = value)) + 
         geom_bar(aes(fill = variable),stat = "identity",position = "dodge") + theme_light() +
         scale_y_log10() +theme(axis.text.x = element_text(size  = 10, angle = 45,hjust = 1,vjust = 1)) +
         labs(x = "States and Union Territories",y = "Literacy rate and Crime rate",title = paste("Crime Rate in relation of literacy rate"))
     })
   }
   else if (input$radioliteracy == "2011"){
     a <- data()%>% filter( data()$DISTRICT=="TOTAL" & data()$YEAR == "2011") %>%select(YEAR,STATEUT,Rape,DowryDeaths,Kidnapping,Insult,Assault,Cruelty,Importation)
     crime1<- a %>%group_by(STATEUT) %>%summarise(mean_run = sum(Rape,DowryDeaths,Kidnapping,Insult,Assault,Cruelty,Importation)) 
     newdata <- pdata[order(pdata$STATEUT),] 
     newlit <- litdata[order(litdata$STATEUT),] 
     result1 <- cbind(crime1,newdata,newlit)
     
     newresult <- data.frame(list(c(result1[-3])))
     newresult1 <- data.frame(list(c(newresult[-1])))
     
     
     b <- newresult1 %>%
       mutate_all(funs(as.character(.)), newresult1$f2011) %>%
       mutate_all(funs(gsub(",", "", .)), newresult1$f2011) %>%
       mutate_all(funs(as.numeric(.)), newresult1$f2011)
     
     
     ans <- cbind(newresult,b)
     ans1 <- ans[-2:-5]
     cans <- ans1 %>% mutate(rate1 = (mean_run*100000) / f2011) %>% arrange(desc(rate1))
     
     arrange1 <-  head(arrange(cans,order(rate1)),10) %>% select(STATEUT,lit2011,rate1)
     
     colnames(arrange1) <- c('STATEUT','Literacy','CrimeRate')
     
     output$literacy1low <- renderPlotly({
       p <- plot_ly(arrange1, labels = ~STATEUT, values = ~CrimeRate, type = 'pie') %>%
         layout(title = 'crime rate',
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                legend = list (orientation ='h') )
       p
     })
     
     dfm <- melt(arrange1[,c('STATEUT','Literacy','CrimeRate')],)
     output$ggbarlow <- renderPlot({
       
       ggplot(dfm,aes(x = STATEUT,y = value)) + 
         geom_bar(aes(fill = variable),stat = "identity",position = "dodge") + theme_light() +
         scale_y_log10() +theme(axis.text.x = element_text(size  = 10, angle = 45,hjust = 1,vjust = 1)) +
         labs(x = "States and Union Territories",y = "Literacy rate and Crime rate",title = paste("Crime Rate in relation of literacy rate"))
     })
   }
   
 })

#Kmeans 
  observe({
   total1 <- data()%>%filter(data()$STATEUT ==input$state_cluster  & data()$DISTRICT != "TOTAL") %>%select(Rape,DowryDeaths,Kidnapping,Assault,Cruelty,Insult)
   total <- data()%>%filter(data()$STATEUT ==input$state_cluster  & data()$DISTRICT != "TOTAL") %>%select(DISTRICT,YEAR,Rape,DowryDeaths,Kidnapping,Assault,Cruelty,Insult) 
   
   crime <- data.matrix(total1)
   crime <- na.omit(crime)
   crime <- scale(crime)
   

     
     set.seed(1000)
     output$cluster2 <- renderPlot({
        set.seed(123)
            final <- kmeans(crime, 4, nstart = 25)
            print(final)
           fviz_cluster(final, data = crime)
        
    })

    output$Table <- renderDataTable({
        cl <- kmeans(crime,4)
        center <-as.data.frame(cl$centers)
        center
        
        dd <- cbind(total, cluster = cl$cluster)
        total1 <- dd %>%select(DISTRICT,cluster,YEAR)
        datatable(distinct( total1), options =list(searching= TRUE,pageLength= 5,lengthMenu = c(5, 10), scrollX = T))
  
      })

 })
 
#Dendogram
 observe({
    a <- data()%>%filter( data()$YEAR == input$dendo_year & data()$DISTRICT == 'TOTAL' &
                          data()$Rape >= input$dendo_rate ) %>%select(STATEUT,Rape,DowryDeaths,Kidnapping,Assault,Cruelty,Importation)
   mydata <- data.frame(list(c(a)))
  
 output$dendogrampair <- renderPlot({
   pairs(mydata)
 })
 
 #Normalization
 z <- mydata[,-c(1,1)]
 m <- apply(z,2,mean) #2 for columns and 1 for row
 s <- apply(z , 2, sd) #standard deviation
 z <-scale(z,m,s)
 
   output$dendogram <- renderPlot({
     # Claculating Euclidean distance
     distance <- dist(z)
     hc.c <- hclust(distance)
    plot(hc.c,labels = mydata$STATEUT ,hang=-1 )
    
   })

 })


 #Conclude
 observe({
   a <- data()%>% filter( data()$DISTRICT=="TOTAL") %>%select(YEAR,STATEUT,Rape,DowryDeaths,Kidnapping,Insult,Assault,Cruelty,Importation)
   crime1<- a %>%group_by(YEAR) %>%summarise(mean_run = sum(Rape,DowryDeaths,Kidnapping,Insult,Assault,Cruelty,Importation))
   c <- data.frame(c(list(crime1)))
  
   output$conclude <- renderPlotly({
      plot_ly(c,
       x =c$YEAR,
       y = c$mean_run, marker = list(color = 'rgb(158,202,225)'),type = "bar") %>%
         layout(xaxis = list(title = "YEAR"),
                yaxis = list(title = "Total Crime "))
   })
   
   text <- readLines("E:\\POOJA\\2. MSC PART 2\\proj\\data1 - Copy.csv")
   docs <- Corpus(VectorSource(text))
   inspect(docs)
   toSpace<-content_transformer(function(x,pattern)gsub(pattern,"",x))
   docs <- tm_map(docs,toSpace,"/")
   docs <- tm_map(docs,toSpace,"@")
   docs <- tm_map(docs,content_transformer(tolower))
   
   docs <- tm_map(docs,removeNumbers)
   docs <- tm_map(docs,removeWords,stopwords("english"))
   docs <- tm_map(docs,removeWords,c("blabla1","blabla2"))
   docs <- tm_map(docs,removePunctuation)
   docs <- tm_map(docs,stripWhitespace)
   docs <- tm_map(docs,stemDocument)
   
   dtm <- TermDocumentMatrix(docs)
   m <- as.matrix(dtm)
   v <- sort(rowSums(m),decreasing = TRUE)
   d <- data.frame(word=names(v),freq=v)
   head(d,10)
   set.seed(1234)
   output$wordcloudstate <- renderPlot({
     
     wordcloud(words = d$word,freq = d$freq,min.freq = 1,max.words =600,scale=c(5, 0.5),use.r.layout = FALSE,random.order = FALSE,rot.per = 0.30,colors = brewer.pal(8,"Dark2"))
     
   })
 })
  

 # Display the choropleth map
  output$distPlot <- renderPlot({  
     plotMap(b,input$radiomap,input$id)
  })
  

})


plotMap <- function(df,theCrime="RAPE",theYear="X2012"){
   if(theYear==""){
      theYear<-"2001"
   }
   # Create a dataframe based on the crime 
   r <- filter(df,crime==theCrime)
   r <- r[-36,]
   # Set the year for which the crime is required
   year <- paste("X",theYear)
   year <- str_replace_all(year," ","")
   
   # Subset columns with the chosen year
   a <-colnames(r)==year
   
   
   # Make the names in the crimes.csv consistent with the names in shape file
   crimeSet <- select(r,state,crime,which(a))    
   crimeSet$state = as.character(r$state)
   crimeSet[crimeSet$state=="D&N Haveli",]$state = "DADRA AND NAGAR HAVELI"
   crimeSet[crimeSet$state=="Daman & Diu",]$state = "DAMAN AND DIU"
   crimeSet[crimeSet$state=="A&N Islands",]$state = "ANDAMAN AND NICOBAR ISLANDS"
   crimeSet[crimeSet$state=="Jammu & Kashmir",]$state = "Jammu And Kashmir"
   crimeSet[crimeSet$state=="Delhi UT",]$state = "Nct of Delhi"
   crimeSet[crimeSet$state=="Odisha",]$state = "Orissa"
   crimeSet[crimeSet$state=="Uttarakhand",]$state = "Uttarakhand"
   crimeSet[crimeSet$state=="Lakshadweep",]$state = "LAKSHADWEEP"
   crimeSet[crimeSet$state=="Chandigarh",]$state = "CHANDIGARH"
   crimeSet[crimeSet$state=="Puducherry",]$state = "Pondicherry"
   
   # Compute min and max to set the range of shades
   names(crimeSet) <- c("state","crime","yearlyCrimes")
   m= max(crimeSet$yearlyCrimes)
   n = min(crimeSet$yearlyCrimes)
   mid = (m+n)/2
   
   
   b <- head(arrange(crimeSet,desc(yearlyCrimes)),5)
   c <- paste(b$state,"(",b$yearlyCrimes,")")
   stateCrime <- str_replace_all(c," ","")
   
   # Create a data frame to primt the top 5 ofenders
   labels <- data.frame(
      xc = c(90,90,90,90,90), 
      yc = c(7.0,5.6,4.2,2.8,1.4), 
      label = as.vector(stateCrime) 
      
   )
   
   # Plot the choropleth
   title <- paste("Incidence of",tolower(theCrime),"in India in ",theYear)
   ggplot(data = crimeSet)  +
      geom_map(data = crimeSet, aes(map_id = state, fill = yearlyCrimes), col="brown",  map = ind ) + 
      expand_limits(x = ind$long, y = ind$lat) +        
      scale_fill_gradient2(low = "pink",                                                                           
                           mid = "red", midpoint = mid, high = "black", limits = c(n, m)) +
      ggtitle(title) +
      geom_text(aes(label="Top offenders",90,8.6))+
      geom_text(data = labels, aes(x = xc, y = yc, label = label))+
      xlab("Longitude") + ylab("Latitude")

}