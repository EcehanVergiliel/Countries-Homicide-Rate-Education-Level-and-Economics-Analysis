### BEST CODER GANG ###
### Sena Gulizar Aktas 2428852, Gizem Sarul 2429256, M. Egemen Gundur 2429090, Ecehan Vergiliel 2429397, E. Doga Askin 2501955 ###
### STAT292 Final Project -- June, 2022 ###


library(tidyverse)
library(shiny)
library(plotly)
library(DT)
library(corrplot)

project <- read.csv("STAT292 Final Project Data.csv", sep = ',')

ui <- fluidPage(
  titlePanel('Countries Homicide Rate, Education Level and Economics Analysis'),
  
  mainPanel(
    h4(em("Purpose: Examine the effect of education and economy on gun homicide rate according to 24 different countries.")),
    h5("by Best Coder Gang"),
    
    # --five different tabs-- #
    tabsetPanel(type = "tabs",
                
                #data and plot panel with explanation and selection bar
                tabPanel("Data and Plot",plotlyOutput("plot"), br(),
                         em("Gun Homicide Rate, Primary School Enrollment (for both gender) and GNI of countries from 1996 to 2016. (NA values are replaced with the mean of that country.)"),
                         br(), br(), br(), DT::dataTableOutput("t"),
                         sidebarPanel(
                           tags$img(src="gun.jpg",height=200,weight=200),
                           style = "overflow-y: auto; position:fixed; top:100px; right:50px",
                           helpText("Choose topic and country to see the plot of the data"),
                           
                           #topic selection panel
                           selectInput(inputId = "t", label = "Topic",
                                       choices = c("gun_homicide", 
                                                   "primary_enrollment",
                                                   "GNI"),
                                       selected = "Gun Homicide Rate"),
                           
                           #country selection panel
                           selectInput(inputId = "c", "Country",
                                       choices = unique(project$country),
                                       multiple = TRUE,
                                       selected = c("Australia")),
                           
                           #download panel
                           helpText("You can download the filtered data."),
                           downloadButton(outputId = "download_data", label = "Download"),
                           h6("Powered By:"),
                           tags$img(src="r.jpeg",height=40,weight=40)
                         )), 
                
                #correlation plot tab
                tabPanel("Correlation", 
                         div(img(src="stat.jpeg",height=350,weight=350),style="overflow-y: auto; position:fixed; top:155x; right:20px"),br(),sidebarLayout(
                           position = "right",
                           sidebarPanel(textOutput(outputId = "corrtext"), width = 4),
                           mainPanel(selectInput(inputId = "cforcorr", "Country",
                                                 choices = unique(project$country),
                                                 selected = c("Australia")),
                                     plotOutput("corrplot"), br(),
                                     strong("Correlation Analysis (Gun Homicide Rate vs Primary School Enrollment)"),
                                     verbatimTextOutput("cr"), strong("Correlation Analysis (Gun Homicide Rate vs GNI)"),
                                     verbatimTextOutput("cr2")))),
                
                #linear regression model tab 
                tabPanel("Model Summary",
                         div(img(src="significiant.jpeg",height=400,weight=200),style="overflow-y: auto; position:fixed; top:120px; right:20px"),br(), sidebarLayout(
                           position = "right",
                           sidebarPanel(textOutput(outputId = "regtext"), width = 4),
                           mainPanel(selectInput(inputId = "cforcorr2", "Country",
                                                 choices = unique(project$country),
                                                 selected = c("Australia")),
                                     strong("Linear Regression Model (Gun Homicide Rate vs Primary School Enrollment)"),
                                     verbatimTextOutput("summary1"), br(),
                                     strong("Linear Regression Model (Gun Homicide Rate vs GNI)"),
                                     verbatimTextOutput("summary2"))
                         )), 
                
                #prediction tab - 1
                tabPanel("Prediction For Primary Enrollment",
                         br(),
                         div(img(src="edu.jpeg",height=500,weight=500),style = "overflow-y: auto; position:fixed; top:120px; right:50px"),
                         fluidRow(
                           column(3, 
                                  selectInput(inputId = "cforcorr3", "Country",
                                              choices = unique(project$country),
                                              selected = c("Australia"))),
                           column(5, 
                                  sliderInput("primary_enrollment",
                                              "Select primary enrollment:",
                                              min = 27844,
                                              max = 30000000,
                                              value = 27844))),
                         verbatimTextOutput("preq"),
                         tableOutput("distPlot"),
                         DT::dataTableOutput("pritable")),
                
                #prediction tab - 2
                tabPanel("Prediction For GNI",br(),
                         div(img(src="GNI.jpeg",height=300,weight=400),style="overflow-y: auto; position:fixed; top:150px; right:50px"),
                         fluidRow(
                           column(3,
                                  selectInput(inputId = "cforcorr4", "Country",
                                              choices = unique(project$country),
                                              selected = c("Australia"))),
                           column(5, 
                                  sliderInput("GNI",
                                              "Select GNI:",
                                              min = 7217037594,
                                              max = 1.902048e+13,
                                              value = 7217037594))),
                         verbatimTextOutput("preq2"),
                         tableOutput("distPlott"),
                         DT::dataTableOutput("gnitable"))), br(),
    
    #references part
    strong("References"), br(),
    div("Primary Eductation by Country. UIS Statistics. Data.uis.unesco.org. (2022).", span("http://data.uis.unesco.org/Index.aspx", style = "color:blue")),
    div("Gun Homicide Rate per 100,000 People, by Country. Rdatasets. (2022).", span("https://vincentarelbundock.github.io/Rdatasets/doc/stevedata/ghp100k.html", style = "color:blue")),
    div("Camli, O. (2022). Linear Regression Models. STAT250 Recitation 7 Notes. METU."),
    div("Shiny. Shiny from R Studio. (2022).", span("https://shiny.rstudio.com/", style = "color:blue")),
    div("R, M. (2022). Multiple Linear Regression in R - Articles - STHDA. Sthda.com.", br(), span("http://www.sthda.com/english/articles/40-regression-analysis/168-multiple-linear-regression-in-r/.", style = "color:blue")),
    div("UNdata | record view | Per capita GNI at current prices - US dollars. Data.un.org. (2022).", br(),span("http://data.un.org/Data.aspx?d=SNAAMA&f=grID:103;currID:USD;pcFlag:1.", style = "color:blue"))
  )
)





server <- function(input, output, session) {
  #we need different subsets of the main data so we create 7 filtered data
  filtered_data <- reactive({
    subset(project,
           country  %in% input$c)})
  
  filtered_data2 <- reactive({
    subset(project,
           country  %in% input$cforcorr3)})
  
  filtered_data3<- reactive({
    subset(project,
           country  %in% input$cforcorr4)})
  
  filtered_data4 <- reactive({
    a <- project %>% subset(country  %in% input$cforcorr3) %>% select(primary_enrollment)
  })
  
  filtered_data5 <- reactive({
    b <- project %>% subset(country  %in% input$cforcorr4) %>% select(GNI)
  })
  
  filtered_data6 <- reactive({
    c <- project %>% subset(country  %in% input$cforcorr3) %>% select(country, gun_homicide, primary_enrollment)
    c %>% 
      select(gun_homicide, primary_enrollment) %>% 
      filter(primary_enrollment==input$primary_enrollment)
    c[which.min(abs(c$primary_enrollment-input$primary_enrollment)),]
  })
  
  filtered_data7 <- reactive({
    d <- project %>% subset(country==input$cforcorr4) %>% select(country, gun_homicide, GNI)
    d %>% 
      select(gun_homicide, GNI) %>% 
      filter(GNI==input$GNI)
    d[which.min(abs(d$GNI-input$GNI)),]
  })
  
  #correlation analysis text
  output$corrtext<- renderText("To analyze the correlation between gun homicide rate vs primary school enrollment and gun homicide rate vs GNI according to the country you choose, firstly you should look at the p-value. If it is smaller than 0.05, you can say that there is a significant correlation between them. 
After checking the p-value, you need to analyze its correlation coefficient to identify whether the gun homicide rate and primary school enrollment or gun homicide rate and GNI have a linear relationship or not and correlations strength.
 If the correlation coefficient has a negative value there is a negative linear relationship between them. So, when one variable increases the other variable decreases. Also, if the value is close to -1.0 we can say that the negative relationship between variables is strong.
If the correlation coefficient has a positive value there is a positive linear relationship between them. So, when one variable increases the other variable increases too. Also, if the value is close to 1.0 we can say that the positive relationship between variables is strong.
Lastly, if the correlation coefficient is 0 we can confirm that there is no relationship between variables. 
")
  
  #regression analysis text
  output$regtext <- renderText('To interpret linear regression models, 
firstly, you need to examine the F-statistic and the associated p-value to determine whether one of the predictor variables is significantly related to the outcome variable or not. If the p-value is smaller than 0.05 There is a significant relationship between variables.
Secondly, we can observe the estimates of the beta coefficients B0 and B1, by looking at the coefficient table in our model summary and their standard errors that show the accuracy of these coefficients.
And If we look at the median residuals it shows us the difference between observed and response values that the model predicted. If the median residual value is close to 0, we can say that the distribution of the residuals is almost symmetrical and the model predicts certain points that do not fall far away from the actual observed points.
Lastly, to examine the quality of our model we need to check its R-squared and Residual Standard Error (RSE). Since the RSE gives us the measure of the error of prediction, we can say that our model is accurate if the RSE value is small. Additionally, R- squared is how well the regression model explains observed data and the proportion of variance in the dependent variable that can be explained by the independent variable.
')
  
  #the plot (line graph) appears in the first tab 
  output$plot <- renderPlotly({
    ggplotly({
      p <- ggplot(filtered_data(), aes_string(x="year", y=input$t, color="country")) +
        geom_point(alpha=0.7) + geom_line(linetype = 'dashed') + theme(legend.position = "none") +
        ylab("Topic")
      
      p
    })
  })
  
  
  #the data table in the first tab
  output$t <- DT::renderDataTable({filtered_data()})
  
  #table of primary education which helps us see the observed data (in the prediction tab)
  output$pritable <- DT::renderDataTable({filtered_data6()})
  
  #table of GNI which helps us see the observed data (in the prediction tab)
  output$gnitable <- DT::renderDataTable({filtered_data7()})
  
  #download button
  output$download_data <- downloadHandler(
    filename = "download_data.csv",
    content = function(file) {
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    })
  
  #linear model summary for primary education vs gun homicide
  output$summary1 <- renderPrint({
    data <- project %>% subset(country==input$cforcorr2)
    fit1 <- lm(gun_homicide ~ primary_enrollment, data=data)
    print(summary(fit1))
  })
  
  #linear model summary for GNI vs gun homicide
  output$summary2 <- renderPrint({
    data <- project %>% subset(country==input$cforcorr2)
    fit2 <- lm(gun_homicide ~ GNI, data=data)
    print(summary(fit2))
  })
  
  #this part makes the slider change according to the countries min/max values
  observeEvent(input$cforcorr3, {
    updateSliderInput(session, "primary_enrollment",
                      min = min(filtered_data4()), max = max(filtered_data4()))
  })
  
  observeEvent(input$cforcorr4, {
    updateSliderInput(session, "GNI",
                      min = min(filtered_data5()), max = max(filtered_data5()))
  })
  
  
  #gives the predicted value for primary enrollment
  output$distPlot <- renderTable({
    data <- project %>% subset(country==input$cforcorr3)
    Model <- lm(gun_homicide ~ primary_enrollment, data=data)
    New <- data.frame(primary_enrollment = input$primary_enrollment)
    New
    Pred <- predict(Model,New)
    Pred},
    colnames = F,
    striped = T)
  
  #gives the predicted value for GNI
  output$distPlott <- renderTable({
    data <- project %>% subset(country==input$cforcorr4)
    Model2 <- lm(gun_homicide ~ GNI, data=data)
    New2 <- data.frame(GNI = input$GNI)
    New2
    Pred2 <- predict(Model2,New2)
    Pred2},
    colnames = F,
    striped = T)
  
  #the regression equation which makes the prediction (primary enrollment)
  output$preq <- renderPrint({
    data <- project %>% subset(country==input$cforcorr3)
    Model3 <- lm(gun_homicide ~ primary_enrollment, data=data)
    cat("Gun Homicide Rate","=",summary(Model3)$coef[1,1],"+","(",summary(Model3)$coef[2,1],"x Primary Enrollment",")")
  })
  
  
  #the regression equation which makes the prediction (GNI)
  output$preq2 <- renderPrint({
    data <- project %>% subset(country==input$cforcorr4)
    Model4 <- lm(gun_homicide ~ GNI, data=data)
    cat("Gun Homicide Rate","=",summary(Model4)$coef[1,1],"+","(",summary(Model4)$coef[2,1],"x GNI",")")
  })
  
  #correlation plot 
  output$corrplot <- renderPlot({
    data <- project %>% subset(country==input$cforcorr)
    proj <- data[-1]
    cor.p <- cor(proj)
    corrplot(cor.p, method = "number")
    
  })
  
  #correlation tests
  output$cr <- renderPrint({
    
    data <- project %>% subset(country==input$cforcorr)
    cor.test(data$gun_homicide, data$primary_enrollment, method = "pearson")
  })
  
  output$cr2 <- renderPrint({
    
    data <- project %>% subset(country==input$cforcorr)
    cor.test(data$gun_homicide, data$GNI, method = "pearson")
  })
}



shinyApp(ui = ui, server = server)