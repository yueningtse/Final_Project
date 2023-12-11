library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(shinyWidgets)
library(ggpubr)

source("Final_Project_df_codes.R")

#Codes for introductory page (Introduction page)
introductory_page <- fluidPage(
  h1(tags$p("Info201 Final Project", style = "font-family: Comic Sans MS")),
  h3(tags$p("Created by: Anisa Tse and Anna Pham", style = "font-family: Comic Sans MS")),
  br(),
  h4(tags$p("We were interested to see if air quality is related to asthma hospitalization rates, so we went online to find information about these two topics. We found a dataset about New York State air quality and a dataset about New York State Community Health.", style = "font-family: Ink Free;font-weight: bold")), 
  br(),
  h4(tags$p("For the air quality dataset, we found it from Data.gov.", style = "font-family: Ink Free;font-weight: bold")), 
  tags$h5(tags$p("The below links to the air quality dataset site:", style = "font-family: Ink Free;font-weight: bold")), 
  tags$a(href="https://catalog.data.gov/dataset/air-quality", "Air Quality"), 
  br(),
  br(), 
  h4(tags$p("For the community health dataset, we found it from Health.Data.NY.gov.", style = "font-family: Ink Free;font-weight: bold")), 
  tags$h5(tags$p("The below links to the community health dataset site:", style = "font-family: Ink Free;font-weight: bold")), 
  tags$a(href="https://health.data.ny.gov/Health/Community-Health-Indicator-Reports-CHIRS-Trend-Dat/jb5s-mei3/data", "Community Health Indicator Reports (CHIRS): Trend Data"), 
  br(),
  br(), 
  img(src = "5-boroughs-nyc.jpg", height="20%", width="20%", align = "right"), 
  h4(tags$p("The original air quality dataset covered the air quality in the entire state of New York. However, we wanted to focus only on New York City, so the dataset was filtered to include only the five boroughs/counties of New York City: The Bronx/Bronx County, Brooklyn/Kings County, Manhattan/New York County, Queens/Queens County, and Staten Island/Richmond County (see picture on the right).", style = "font-family: Ink Free;font-weight: bold")), 
  br(), 
  h4(tags$p("The original community health dataset covered many more types of health concerns, such as cancer, cardiovascular issues, maternity and infant health concerns, etc. Our dataset used for this project was filtered down from this dataframe to only include data about asthma hospitalizations and to encompass only the five counties in New York City, as with the air quality dataset.", style = "font-family: Ink Free;font-weight: bold")), 
  br(), 
  h4(tags$p("We joined the two datasets together, and in our filtered and joined dataset, there are several columns worth knowing about. From the air quality dataset, there is a column containing the NYC county names, a column with the air pollutant names, a column containing the values for the air pollutants, and a column indicating the time period when the data values were collected. From the community health dataset, there is a column containing the asthma hospitalization age groups and a column containing the hospitalization values. In the original community health dataset, there are no data for asthma hospitalization in 2015, so our combined dataset also has missing values for year 2015.", style = "font-family: Ink Free;font-weight: bold")), 
  br(), 
  h4(tags$p("The graphs in the following pages will be describing how does air quality relate to asthma hospitalization through 2009-2016.", style = "font-family: Ink Free;font-weight: bold")), 
  br(), 
  h4(tags$p("In general, based on the data, we make the following observations: First, the pollution level has declined over the years. Second, the asthma hospitalization rates are also declining over the years. Therefore, they are likely to have a positive correlation. This relationship aligns with our intuitive understanding, but it cannot be determined as a causal relationship because there are many other factors that may affect the results.", style = "font-family: Ink Free;font-weight: bold"))
)

#Codes for page 1 (Air Quality Data page)
page1 <- fluidPage(
  singleton(
    tags$head(tags$script("Shiny.addCustomMessageHandler(
  'update_background', function(message) {
    $('body').css({
      'background-image':'url(' + message +')',
      'background-repeat':'no-repeat',
      'background-position':'center',
      'background-size':'cover'

    });
  });"))),
  
  titlePanel(tags$p("Level of Air Pollution in New York Counties Through the Years", style = "font-family: Comic Sans MS")), 
  sidebarLayout(
    sidebarPanel(
      h3(tags$p("Choose an Air Pollutant Type", style = "font-family: Comic Sans MS")),
      h5(tags$p("There are two dropdown menus where you can select which air pollutant type to look at. Due to the restrictions of the dataset, some air pollutant types only have values in either the summer or the winter, while others have values for summer, winter, and the annual average. Therefore, the Time Period dropdown menu will depend on the chosen air pollutant type. For example, ozone only has values in the summer, so when \"Ozone (O3)\"  is selected, there will only be a \"Summer\" option.", style = "font-family: Ink Free;font-weight: bold")
), 
      selectInput(
        inputId = "pollutant_type_page1", 
        label = "Air Pollutant Type", 
        # choices = sort(distinct(df1, Name))
        choices = sort(unique(df1$Name))
      ), 
      selectInput(
        inputId = "time_period_page1",
        label = "Time Period (Dependent on Air Pollutant Type)",
        choices = ""
      )
    ), 
    mainPanel(
      h5(tags$p("This graph shows the changes in the air pollutant value chosen over the years from 2009 to 2016. There are five lines, represented by different colors and point shapes, for each county in New York City. Note that the range of the air pollutant values axis changes when a new selection in the dropdown menu is made.", style = "font-family: Ink Free;font-weight: bold; background: white")), 
      plotOutput(outputId = "line_chart_page1"), 
      h5(tags$p("By selecting different combinations in the dropdown menus, we can see that ozone levels in the summer seem to be the only ones that had a general increase over the years, while all the other pollutants had a general decrease. Additionally, New York County seems to have higher levels of air pollution than all the other counties, while having the lowest levels of only ozone in the summer.", style = "font-family: Ink Free;font-weight: bold; background: white"))
    )
  ), 
  br()
)

#Codes for page 2 (Hospitalization Data page)
page2 <- fluidPage(
  h2(tags$p("Asthma Hospitalization Rates by Age Through the Years in New York Counties", style = "font-family: Comic Sans MS")), 
  sidebarLayout(
    sidebarPanel(
      h3(tags$p("Choose an Age Group", style = "font-family: Comic Sans MS")),
      h5(tags$p("The dropdown menu below will allow you to choose what age group you want to view.", style = "font-family: Ink Free;font-weight: bold")), 
      selectInput(
        inputId = "age_page1",
        label = "Age Groups",
        choices = sort(unique(df2_1$Indicator.Name))
      )      
    ), 
    mainPanel(
      h5(tags$p("This graph examines the asthma hospitalization rates for the selected age group over the years. Once again, there are five lines, represented by different colors and point shapes, for each county in New York City. Note that the range for the y-axis is fixed; this allows for better comparison when a different age group is chosen.", style = "font-family: Ink Free;font-weight: bold; background: white")), 
      plotOutput(outputId = "line_chart_page2"), 
      h5(tags$p("By choosing different age groups, it is clear that Bronx County seems to have higher asthma hospitalization rates across all age groups, while Queens County and Richmond County generally have the lowest asthma hospitalization rates.", style = "font-family: Ink Free;font-weight: bold; background: white"))
    )
  ), 
  br(), 
  h2(tags$p("Asthma Hospitalization Rates by Age in a Particular Year and County", style = "font-family: Comic Sans MS")), 
  sidebarLayout(
    sidebarPanel(
      h3(tags$p("Select a NYC County", style = "font-family: Comic Sans MS")), 
      h5(tags$p("This dropdown allows you to select which county to view.", style = "font-family: Ink Free;font-weight: bold")), 
      selectInput(
        inputId = "borough", 
        label = "County", 
        choices = sort(unique(df2_2$County))
      ), 
      h3(tags$p("Slide to a Date", style = "font-family: Comic Sans MS")), 
      h5(tags$p("You can use the slider to choose the year you want to look at. Due to the dataset restriction, there are no values from 2015, so the slider does not include a value for 2015.", style = "font-family: Ink Free;font-weight: bold")), 
      sliderTextInput(
        inputId = "year_slide", 
        label = "Year", 
        choices = c(2009:2014, 2016), 
        grid = TRUE
      )
    ), 
    mainPanel(
      h5(tags$p("The following graph shows the hospitalization rates for different age groups, given a county and a year. Again, due to the dataset restriction, there is no graph for 2015. Note that the range for the x-axis is fixed; this allows for better comparison when a different county and year are chosen.", style = "font-family: Ink Free;font-weight: bold; background: white")), 
      plotOutput(outputId = "bar_graph"), 
      h5(tags$p("From the graphs with different county and year inputs, we can see that the two age groups with the highest asthma hospitalization rates are 0-4 years and 65 or older; this could be due to their weak immune systems not being able to fight against foreign particles. The two age groups with the lowest asthma hospitalization rates are 15-24 and 25-44; this can be because people in these age groups generally have strong and healthy immune systems.", style = "font-family: Ink Free;font-weight: bold; background: white"))
    )
  ), 
  br()
)

#Codes for page 3 (Air Quality vs. Hospitalization page)
page3 <- fluidPage(
  h2(tags$p("Asthma Hospitalization Rates vs. Air Pollutant Values", style = "font-family: Comic Sans MS")), 
  sidebarLayout(
    sidebarPanel(
      h3(tags$p("Choose an Asthma Hospitalization Age Group", style = "font-family: Comic Sans MS")), 
      h5(tags$p("The dropdown menu below will allow you to choose what age group you want to view. ", style = "font-family: Ink Free;font-weight: bold")), 
      selectInput(
        inputId = "age_page3_1", 
        label = "Age Groups", 
        choices = sort(unique(df3_1$Indicator.Name))
      ), 
      h3(tags$p("Choose an Air Pollutant Type", style = "font-family: Comic Sans MS")), 
      h5(tags$p("There are two dropdown menus where you can select which air pollutant type to look at. Due to the restrictions of the dataset, some air pollutant types only have values in either the summer or the winter, while others have values for summer, winter, and the annual average. Therefore, the Time Period dropdown menu will depend on the chosen air pollutant type.", style = "font-family: Ink Free;font-weight: bold")), 
      selectInput(
        inputId = "pollutant_type_page3_1", 
        label = "Air Pollutant Type", 
        choices = sort(unique(df3_1$Name))
      ), 
      selectInput(
        inputId = "time_period_page3_1",
        label = "Time Period (Dependent on Air Pollutant Type)",
        choices = ""
      )
    ), 
    mainPanel(
      h5(tags$p("The graph below shows the air pollutant values and asthma hospitalization rates when given an age group and air pollutant type in a time period. The points are grouped by year (indicated by the color of the points) and by county (indicated by the shape of the points). Note that both axes are not pre-set, so the axis ranges will differ when given different inputs.", style = "font-family: Ink Free;font-weight: bold; background: white")), 
      plotOutput(outputId = "scatter"), 
      h5(tags$p("In general, you can see that when given any age group, air pollutant type, and time period, the points for the counties are somewhat grouped together. When the age group is the general age group (no ages specified after \"Asthma hospitalization rate per 10,000\"), the air pollutant type is fine particulate matter, and the time period is the annual average, there is some correlation happening between the air pollutant values and asthma hospitalization in each county.", style = "font-family: Ink Free;font-weight: bold; background: white"))
    )
  ),
  h2(tags$p("Asthma Hospitalization Rates vs. Air Pollutant Values Through the Years", style = "font-family: Comic Sans MS")),
  sidebarLayout(
    sidebarPanel(
      h3(tags$p("Choose an Asthma Hospitalization Age Group", style = "font-family: Comic Sans MS")),
      h5(tags$p("The dropdown menu below will allow you to choose what age group you want to view.", style = "font-family: Ink Free;font-weight: bold")), 
      selectInput(
        inputId = "age_page3_2",
        label = "Age Groups",
        choices = sort(unique(df3_2$Indicator.Name))
      ),
      h3(tags$p("Choose an Air Pollutant Type", style = "font-family: Comic Sans MS")), 
      h5(tags$p("There are two dropdown menus where you can select which air pollutant type to look at. Due to the restrictions of the dataset, some air pollutant types only have values in either the summer or the winter, while others have values for summer, winter, and the annual average. Therefore, the Time Period dropdown menu will depend on the chosen air pollutant type.", style = "font-family: Ink Free;font-weight: bold")), 
      selectInput(
        inputId = "pollutant_type_page3_2",
        label = "Air Pollutant Type",
        choices = sort(unique(df3_2$Name))
      ),
      selectInput(
        inputId = "time_period_page3_2",
        label = "Time Period (Dependent on Air Pollutant Type)",
        choices = ""
      ),
      h3(tags$p("Select a NYC County", style = "font-family: Comic Sans MS")), 
      h5(tags$p("This dropdown allows you to select which county to view.", style = "font-family: Ink Free;font-weight: bold")), 
      selectInput(
        inputId = "county",
        label = "County",
        choices = sort(unique(df3_2$County))
      )
    ),
    mainPanel(
      h5(tags$p("There are two graphs below; the one on the left (with the blue line and triangular points) shows the change in asthma hospitalization rates over the years when given an age group, air pollutant type, time period, and county. The one on the right (with the green line and circular points) shows the change in air pollutant values over the years when given an age group, air pollutant type, time period, and county. Note (again) that the y-axes on both graphs are not pre-set, so the y-axis ranges will differ when given different inputs.", style = "font-family: Ink Free;font-weight: bold; background: white")),
      plotOutput(outputId = "line_chart_page3"), 
      h5(tags$p("When looking at the two graphs for \"Asthma hospitalization rate per 10,000\" and \"Fine Particulate Matter,\" asthma hospitalization rates and air pollutant values both decreased over the years, indicating a positive correlation between asthma hospitalization and the level of fine particulate matter. However, this relationship between asthma hospitalization rates and air pollutant values is not always present. For example, when the air pollutant type is ozone, for any given age group and county, there is a downward trend in asthma hospitalization rates over the years, while there is an upward trend in ozone pollutant values over the years. This suggests a negative correlation between hospitalization and pollutant values over the years; as the ozone air pollutant values increase over the years, asthma hospitalization decreases over the years.", style = "font-family: Ink Free;font-weight: bold; background: white"))
    )
  ), 
  br()
)

ui <- 
  tagList(tags$head(
    tags$script("
      Shiny.addCustomMessageHandler('background-color', function(color) {
        document.body.style.backgroundColor = color;
      });
    ")
  ),
  navbarPage(
  "Final Project", 
  id = "navbarID", 
  tabPanel("Introduction", introductory_page), 
  tabPanel("Air Quality Data", page1), 
  tabPanel("Hospitalization Data", page2), 
  tabPanel("Air Quality vs. Hospitalization", page3)
))


server <- function(input, output, session) {

  observeEvent(input$navbarID, {
    if(input$navbarID == "Air Quality Data"){
      session$sendCustomMessage("update_background", "page1_air_pollution.jpg")
    } else if(input$navbarID == "Hospitalization Data"){
      session$sendCustomMessage("update_background", "page2_hospitalization.jpg")
    } else if(input$navbarID == "Air Quality vs. Hospitalization"){
      session$sendCustomMessage("update_background", "page3_combined1.jpg")
    } else{
      session$sendCustomMessage("update_background", "")
      session$sendCustomMessage("background-color", "#FFE7E7")
    }
  })
  
  
# Below is the code to make the Time Period dropdown menu dependent on the Air Pollutant Type dropdown menu for page 1
    observe({
    df1 <- filter(df1, Name == input$pollutant_type_page1)
    updateSelectInput(session, 
                      "time_period_page1", 
                      label = "Time Period", 
                      choices = sort(unique(df1$Season)))
  })
    
# Below is the code to render plot for page 1
  output$line_chart_page1 <- renderPlot({
    df1 <- filter(df1, Name == input$pollutant_type_page1 & Season == input$time_period_page1)
    plot1 <- ggplot(data = df1, aes(x= Year, y = Data.Value, group = County)) + 
      geom_line(aes(color = County)) + 
      geom_point(aes(color = County, shape = County), size = 3.5) + 
      labs(
        x = "Year", 
        y = paste("Air Pollutant Values (", df1$Measure.Info, ")", sep = "")
      ) + 
      theme(
        axis.text=element_text(size=12), 
        axis.title=element_text(size=16, face="bold")
      )
    return(plot1)
  })
  
  
# Below is the code to render plot 1 for page 2
  output$line_chart_page2 <- renderPlot({
    df2_1 <- filter(df2_1, Indicator.Name == input$age_page1)
    plot2_1 <- ggplot(data = df2_1, aes(x= Year, y = Trend.Data.County.Value, group = County)) + 
      geom_line(aes(color = County)) + 
      geom_point(aes(color = County, shape = County), size = 3.5) + 
      labs(
        x = "Year", 
        y = "Asthma Hospitalization Rates per 10,000"
      ) + 
      ylim(0, 180) + 
      theme(
        axis.text=element_text(size=12), 
        axis.title=element_text(size=16, face="bold")
        )
    return(plot2_1)
  })

# Below is the code to render plot 2 for page 2
  output$bar_graph <- renderPlot({
    df2_2 <- filter(df2_2, County == input$borough & Year == input$year_slide)
    plot2_2 <- ggplot(data = df2_2) + 
      geom_bar(mapping = aes(x = Trend.Data.County.Value, y = Indicator.Name, fill = Indicator.Name), stat = "identity") + 
      labs(
        x = "Hospitalization Rate Per 10,000", 
        y = "Age Group", 
        color = "Category"
      ) + 
      xlim(0, 180) + 
      theme(
        axis.text=element_text(size=12), 
        axis.title=element_text(size=16, face="bold")
      )
    return(plot2_2)
  })

# Below is the code to make the Time Period dropdown menu dependent on the Air Pollutant Type dropdown menu for page 3, plot 1
  observe({
    df3_1 <- filter(df3_1, Name == input$pollutant_type_page3_1)
    updateSelectInput(session, 
                    "time_period_page3_1", 
                    label = "Time Period", 
                    choices = sort(unique(df3_1$Season)))
  })

# Below is the code to render the first plot for page 3
  output$scatter <- renderPlot({
    df3_1 <- filter(df3_1, Indicator.Name == input$age_page3_1 & Name == input$pollutant_type_page3_1 & Season == input$time_period_page3_1)
    plot3_1 <- ggplot(data = df3_1, aes(x = Data.Value, y = Trend.Data.County.Value, color = Year, shape = County)) +
      geom_point(size = 4) +
      labs(
        x = paste("Air Pollutant Values (", df3_1$Measure.Info, ")", sep = ""), 
        y = "Asthma Hospitalization by Age Rates",
        color = "Year"
      ) + 
      theme(
        axis.text=element_text(size=12), 
        axis.title=element_text(size=16, face="bold")
      )
    return(plot3_1)
  })
  
  
#Below is the code to make the Time Period dropdown menu dependent on the Air Pollutant Type dropdown menu for page 3, plot 2
  observe({
    df3_2 <- filter(df3_2, Name == input$pollutant_type_page3_2)
    updateSelectInput(session,
                      "time_period_page3_2",
                      label = "Time Period",
                      choices = sort(unique(df3_2$Season)))
  })

# Below is the code to render the second plot on page 3
  output$line_chart_page3 <- renderPlot({
    df3_2 <- filter(df3_2, Indicator.Name == input$age_page3_2 & Name == input$pollutant_type_page3_2 & Season == input$time_period_page3_2 & County == input$county)
    plot3_2_1 <- ggplot(data = df3_2, aes(x= Year, y = Trend.Data.County.Value, group = County)) +
      geom_line(color = "blue") +
      geom_point(color = "blue", size = 3.5, shape = "triangle") +
      labs(
        x = "Year", 
        y = "Asthma Hospitalization Rates per 10,000",
      ) + 
      theme(
        axis.text=element_text(size=12),
        axis.title=element_text(size=16, face="bold")
      )
    plot3_2_2 <- ggplot(data = df3_2, aes(x = Year, y = Data.Value, group = County)) + 
      geom_line(color = "green") + 
      geom_point(color = "green", size = 3.5) + 
      labs(
        x = "Year", 
        y = paste("Air Pollutant Values (", df3_2$Measure.Info, ")", sep = "")
      ) +
      theme(
        axis.text=element_text(size=12),
        axis.title=element_text(size=16, face="bold")
      )
    return(ggarrange(plot3_2_1, plot3_2_2))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
