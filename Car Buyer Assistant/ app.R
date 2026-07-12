
# The app is a Car Buyer Assistant that takes user preferences such as budget,
# mileage, and features (sound system, leather, etc.) and suggests suitable
# car brands based on how well they match these preferences.

# With the input received it filters the data to meet requirements and then also 
# comes up with a feature score for the dummy variables and filters those with a 
# score higher than 0.4 to meet the buyers expectations. 

# A bar graph is then made which shows how much a brand satisfies the requirements
# and an additional analysis table also shows a summary about the brand and various
# specifications. 

# ANOVA is conducted based on the selected variable to test whether
# average car prices differ significantly across its categories.

install.packages("modeldata")

library(shiny)
library(dplyr)
library(ggplot2)
library(modeldata)

data(car_prices)
car_prices <- na.omit(car_prices)

# Create a separate variable in the data frame comprising of the Brand information
# of the car 


car_prices$Brand <- apply(
  car_prices[, c("Buick","Cadillac","Chevy","Pontiac","Saab","Saturn")],
  1,
  function(x) names(x)[which(x == 1)]
)

# This creates a basic framework of the app and also the input options provided 
# to the user. It also creates a structure for the output like plots and tables. 


ui <- fluidPage(
  titlePanel(h2("CAR BUYER ASSISTANT", align = "center")),
  sidebarLayout(
    sidebarPanel(
      h3("Select Your Preferences"),
      sliderInput("budget",
                  "Budget Range:",
                  min = min(car_prices$Price),
                  max = max(car_prices$Price),
                  value = c(25000, 45000)),
      
      sliderInput("mileage",
                  "Mileage Range:",
                  min = min(car_prices$Mileage),
                  max = max(car_prices$Mileage),
                  value = c(15000, 30000)),
      
      selectInput("cylinders",
                  "Cylinders:",
                  choices = unique(car_prices$Cylinder)),
      
      selectInput("doors",
                  "Doors:",
                  choices = unique(car_prices$Doors) ),
      
      checkboxInput("leather", "Leather", value = 1),
      checkboxInput("sound", "Sound System", value = 1),
      checkboxInput("cruise", "Cruise Control", value = 1),
      checkboxInput("convertible", "Convertible", value = 0),
      
      h3("Select Your Plots:"), 
      checkboxInput("bm", "Brands vs Mileage", value = 1),
      checkboxInput("bc", "Brands vs Cylinders", value = 1),
      checkboxInput("bp", "Brands vs Price", value = 0),
      
      h3("Analysis:"),
      checkboxInput("analysis", "Analysis Table", value = 1),
      selectInput("anova",
                  "Select variable for ANOVA:",
                  choices = c("Brand", "Leather", "Sound", "Cruise"),
                  selected = "Leather")
      
      
    ),
    
    mainPanel(
      h3("Recommended Brands"),
      plotOutput("Plot"),
      tableOutput("brandTable"), 
      tableOutput("anovaTable"),
      h3("Additional Plots"),
      fluidRow(
        column(4, plotOutput("mil")),
        column(4, plotOutput("cyl")),
        column(4, plotOutput("price"))
      )
    )
  )
)

# This uses pipelines to filter the data in accordance with the user preferences. 

server <- function(input, output) {
  filtered <- reactive({
    car_prices %>%
      filter(Price >= input$budget[1],
             Price <= input$budget[2],
             Mileage >= input$mileage[1],
             Mileage <= input$mileage[2],
             Cylinder == input$cylinders,
             Doors == input$doors)
  })
  
# This creates a summary for each brand, including average price, mileage, and
# how well each feature matches user preferences. It also computes an overall 
# feature match score.
  
  brands <- reactive({
    df <- filtered()
    df %>%
      group_by(Brand) %>%
      summarise(
        count = n(),
        avg_price = mean(Price),
        avg_mileage = mean(Mileage),
        leather_match = mean(Leather == input$leather),
        sound_match = mean(Sound == input$sound),
        cruise_match = mean(Cruise == input$cruise),
        convertible_match = mean(convertible == input$convertible)
      ) %>%
      mutate(
        feature =
          (leather_match +
             sound_match +
             cruise_match +
             convertible_match) / 4
      ) %>%
      
      filter(
        count >= 3,
        feature >= 0.4
      ) 
      
})    
  
# This creates the main visualization for the brand and its compatibility with the 
# user's demand. 
  
# The visualization is a bar graph showing the score on x-axis and brand name on the y-axis
# The compatibility score is computed on the bases of the user preferences and then it is 
# shown as a comparison between brands. 

  output$Plot <- renderPlot({
    
    ggplot(brands(),
           aes(x = Brand,
               y = feature)) +
      
      geom_col(fill = "navy") +
      coord_flip() +
      theme_minimal(base_size = 14) +
      labs(
        title = "Best Matching Car Brands",
        x = "Brand",
        y = "Match Score"
      )
  })
  
# This renders the main analysis table. 
  
  output$brandTable <- renderTable({
    req(input$analysis == 1)
    brands() %>%
      select(Brand, count, avg_price, avg_mileage, feature) 
  })

# This renders a table for the result of ANOVA 
  
  output$anovaTable <- renderTable({
    df <- filtered()
    
    if (input$anova == "Brand") {
      modd <- aov(Price ~ Brand, data = df)
      } else if (input$anova == "Leather") {
      modd <- aov(Price ~ Leather, data = df)
      } else if (input$anova == "Sound") {
      modd <- aov(Price ~ Sound, data = df)
      } else if (input$anova == "Cruise") {
      modd <- aov(Price ~ Cruise, data = df) }
    
    result <- summary(modd)
    
    f_value <- result[[1]]$`F value`[1]
    p_value <- result[[1]]$`Pr(>F)`[1]
    ans <- if(p_value < 0.05){
      print("This test is statistically significant")
    } else {
      print("The test is NOT statistically significant")
    }
    impli <- if(p_value < 0.05){
      print(paste(input$anova,"significantly affects car price and is important in decision-making."))
    } else {
      print(paste(input$anova,"does not significantly affect car price and is NOT important in decision-making."))
    }
    
  data.frame(
    Variable = input$anova,
    F_Statistic = round(f_value, 3),
    P_Value = round(p_value, 5),
    Conclusion = ans,
    Implication = impli
  )
  })
  
# This section shows the additional plots that a user might want to look at for 
# better clarity. 
  
  
 output$mil <- renderPlot({
 if(input$bm == 1){
 df <- filtered()
 ggplot(df, aes(x = Brand, y = Mileage)) +
        geom_boxplot(fill = "maroon") +
        labs(title = "Mileage by Brand")}

 })

output$cyl <- renderPlot({
if(input$bc == 1){
df <- filtered()
ggplot(df, aes(Brand, fill= factor(Cylinder))) +
  geom_bar() +
  labs(title = "Cylinder Distribution by Brand", fill="Cylinders")}

  })
  
output$price <- renderPlot({
if(input$bp == 1){
df <- filtered()
ggplot(df, aes(x = Brand, y = Price)) +
        geom_boxplot(fill = "maroon") +
        labs(title = "Price by Brand")}
})
}
  
shinyApp(ui = ui, server = server)

