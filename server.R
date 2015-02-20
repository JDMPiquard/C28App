require(shiny)
require(reshape)
require(plyr)
require(rCharts)
require(lubridate)
require(ggmap)
require(ggplot2)
require(googleVis)


shinyServer(function(input,output, clientData, session){
  
  ###LOAD DATA
  all <- reactive({
    temp <- read.csv("data/sbs_na_sca_r2_1_Data.csv")
    temp
  })
  
  ###READ AND CONVERT INPUTS
  NUM <- reactive({
    vec <- input$Num
    vec <- gsub("Value Added","Value added at factor cost",vec)
    vec <- gsub("Personnel Costs","Personnel costs",vec)
    vec <- gsub("Turnover","Turnover or gross premiums written",vec)
    vec <- gsub("EBITDA","Gross operating surplus",vec)
    vec <- gsub("Number Employees","Number of persons employed",vec)
    vec
  })
  
  DEN <- reactive({
    
      vec <- input$Den
      vec <- gsub("Value Added","Value added at factor cost",vec)
      vec <- gsub("Personnel Costs","Personnel costs",vec)
      vec <- gsub("Turnover","Turnover or gross premiums written",vec)
      vec <- gsub("EBITDA","Gross operating surplus",vec)
      vec <- gsub("Number Employees","Number of persons employed",vec)
      return(vec)
    
  })
  
  NAME <- reactive({
    if(DEN()=="NONE"){
      return(input$Num)
    }
    else{
      return(paste("ratio of",input$Num,"by",input$Den))
    }
    
  })
  
  ###COMPUTE DATA FRAME
  df <- reactive({
    
    C28 <- all()
    
    #num and salaries
    selector1 <- NUM()
    subset <- C28[C28$INDIC_SB==selector1,]
    num_df <- cast(subset, value = 'Value', GEO ~ TIME) 
    
    #data cleaning
    x <- c(2,3,4,5,6)
    
    #for numerator
    num_df[num_df==':'] <- NA
    num <- apply(num_df[,x], 1:2, as.character)
    num <- gsub(",","",num)
    num <- apply(num, 1:2, as.numeric)
    
    ##for denominator
    if(DEN()=="NONE"){ #if none, then simply render the numerator data frame
      Num_df <- cbind(num_df[,1],as.data.frame(num))
      colnames(Num_df) <- c("GEO","2008","2009","2010","2011","2012")
      return(Num_df)
      
    }
    else{ #otherwise actually compute a ratio
      
      #den and salaries
      selector2 <- DEN()
      subset <- C28[C28$INDIC_SB==selector2,]
      den_df <- cast(subset, value = 'Value', GEO ~ TIME)
      
      den_df[den_df==':'] <- NA
      den <- apply(den_df[,x], 1:2, as.character)
      den <- gsub(",","",den)
      den <- apply(den, 1:2, as.numeric)
      
      #actually calculate ratio
      Ratio <- signif(num/den,digits = 3)
      
      #clean up df
      Ratio_df <- cbind(num_df[,1],as.data.frame(Ratio))
      colnames(Ratio_df) <- c("GEO","2008","2009","2010","2011","2012")
      
      return(Ratio_df)
      
    }
    
  })

  ###OUTPUTS
  
  #Title
  output$title <- renderText({
    paste("showing ",NAME())
  })
  
  #Data Table
  output$contents  <- renderDataTable({
    df()

  })
  
  #Download csv file
  output$downloadData <- downloadHandler(
    filename = function() { paste(NAME(), '.csv', sep='') },
    content = function(file) {
      write.csv(df(), file)
    }
  )
  
}
)