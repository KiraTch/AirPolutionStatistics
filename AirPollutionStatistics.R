
library(tidyverse)
library(httr2)
library(plotly)
library(shiny)
library(shinydashboard)
library(dplyr)

#first part
library(readr)
global_air_pollution_dataset <- read_csv("C:/Users/LukaGvantse/Desktop/global air pollution dataset.csv")
View(global_air_pollution_dataset)

#first of all, it is simple but i want to show the pollution index in several countries 
#first, check if there is any NAs in rows of global_air_pollution_dataset
sum(!complete.cases(global_air_pollution_dataset))
#we do not need them, because i need to calculate average value of the numbers and i 



#cannot make it with NAs so, delete them
globalAirPollution <- na.omit(global_air_pollution_dataset)
globalAirPollution
#in dataframe there is countries and several cities from each country, so i decide 
#to remove the cities and group values by counties
airPollution <- global_air_pollution_dataset %>%
  group_by(Country) %>%
  summarize(averageAQIvalue = mean(`AQI Value`))

airPollution
#values are too big numbers so let's round and sort them from heighter to lower
airPollution <- global_air_pollution_dataset %>%
  group_by(Country) %>%
  summarize(averageAQIvalue = round(mean(`AQI Value`),1)) %>% arrange(desc(averageAQIvalue))
airPollution

#still check if there left any nas and remove them
sum(!complete.cases(airPollution))

airPollution <- na.omit(airPollution)

biggest30 <- airPollution %>%
  filter(Country=="Russian Federation"|Country=="Canada"|Country=="China"|Country=="United States of America"|Country=="Brazil"|Country=="Australia"|Country=="India"|Country=="Argentina"|Country=="Kazakhstan"|Country=="Algeria"|Country=="Democratic Republic of the Congo"|Country=="Saudi Arabia"|Country=="Mexico"|Country=="Indonesia"|
           Country=="Sudan"|Country=="Libya"|Country=="Mongolia"|Country=="Peru"|Country=="Chad"|Country=="Niger"|Country=="Angola"|Country=="Mali"|Country=="South Africa"|Country=="Colombia"|Country=="Ethiopia"|Country=="Mauritania"|Country=="Egypt"|Country=="Nigeria"|Country=="Pakistan"|Country=="Namibia")

biggest30
#i use the bar chart to visualize
ggplot(data=biggest30) + aes(x=Country, y= averageAQIvalue, fill=Country) + geom_bar(stat = "identity") + 
  labs(title ="temperature values in largest 30 countries", x="Countries", y="average values") + 
  ggtitle("temperature values in largest 30 countries") + coord_flip()

 
#second part
library(readr)
share_deaths_air_pollution <- read_csv("C:/Users/LukaGvantse/Desktop/share-deaths-air-pollution.csv")
View(share_deaths_air_pollution)
 
library(readr)
Annual_Surface_Temperature_Change <- read_csv("C:/Users/LukaGvantse/Desktop/Annual_Surface_Temperature_Change.csv")
View(Annual_Surface_Temperature_Change)

#first data get tidy, meaning that delete unnecessary columns, rows that has NAs and make 2 data frame proper to each other
deaths <- select(share_deaths_air_pollution, -Code)
deaths

sum(!complete.cases(deaths)) #clear
annualTempChange <- select(Annual_Surface_Temperature_Change, -ISO2, -ISO3, -Indicator, -Unit, -Source, -CTS_Code, -CTS_Name, -CTS_Full_Descriptor,-ObjectId)
annualTempChange
#convert each years from seperate columns into one column
annualTempChange <- annualTempChange %>%
  pivot_longer(cols = starts_with("F"), 
               names_to = "Year", values_to = "TempChangevalues")
sum(!complete.cases(annualTempChange))
annualTempChange <- na.omit(annualTempChange)
# Remove the letter "F" from each row in the "Year" column
#The gsub() function is used to replace the letter "F" with an empty string in each row of the "Year" column.
annualTempChange$Year <- gsub("F", "", annualTempChange$Year)
# Convert the "Year" column to numeric
annualTempChange$Year <- as.numeric(annualTempChange$Year)
#tidy temperature change data to have a same time period for both data, also 2020 is questionable for both data so delete from both.
annualTempChange <-filter(annualTempChange, Year!=1961,Year!=1962,Year!=1963,Year!=1964,Year!=1965,Year!=1966,Year!=1967, Year!=1968,Year!=1969,Year!=1970,Year!=1971,
                          Year!=1972,Year!=1973,Year!=1974,Year!=1975,Year!=1976,Year!=1977, Year!=1978,Year!=1979,Year!=1980,Year!=1981,
                          Year!=1982,Year!=1983,Year!=1984,Year!=1985,Year!=1986,Year!=1987, Year!=1988,Year!=1989,
                          Year!=2020,Year!=2021,Year!=2022,Year!=2023,Year!=2024,Year!=2025)   #now both data have the same years
deaths <-filter(deaths, Year !=2020)
names(deaths)[names(deaths) == "Entity"] <- "Country"
#next step is to check list of countries 
# Get unique countries in dataset1
countries1 <- unique(deaths$Country)
# Get unique countries in dataset2
countries2 <- unique(annualTempChange$Country)
#get unique years in dataset1
Years1 <- unique(deaths$Year)
# Get unique countries in dataset2
Years2 <- unique(annualTempChange$Year)
samecountries <- intersect(Years1, Years2)
# intersect() function is used to find the common values between the two sets
samecountries <- intersect(countries1, countries2)
# Filter datasets for samecountries and use %in% operator to test whether elements in one vector are present in another vector.
deaths <- subset(deaths, Country %in% samecountries)
annualTempChange <- subset(annualTempChange, Country %in% samecountries)
#finally
poluttionresult <- merge(deaths, annualTempChange, by = c("Country", "Year"))
#let's vizualize
names(poluttionresult)[names(poluttionresult) == "Deaths - Cause: All causes - Risk: Air pollution - Sex: Both - Age: Age-standardized (Percent)"] <- "Deathpercentage"
#after merge the several columns, still there is a issue in dataframe so try to make it appropriate
expected_years <- poluttionresult %>%
  group_by(Country) %>%
  summarise(num_years = n_distinct(Year))
#Determine the maximum number of years among all countries
expected_num_years <- max(expected_years$num_years)
# Filter the dataframe to include only the countries with the expected number of years
poluttionresult <- poluttionresult %>%
  group_by(Country) %>%
  filter(n_distinct(Year) == expected_num_years)
#too many countries so choose specific ones
g20countries <- poluttionresult %>%
  filter(Country=="Argentina"|Country=="Australia"|Country=="Brazil"|Country=="Canada"|Country=="France"|Country=="Germany"|Country=="India"|Country=="Indonesia"|Country=="Italy"|
           Country=="Japan"|Country=="Mexico"|Country=="Saudi Arabia"|Country=="South Africa"|Country=="United Kingdom"|Country=="United States")
g20countries <- as.tibble(g20countries)
#vizualize
deathpercentage <-ggplot(g20countries) + aes(x=Year, y= Deathpercentage, group = Country, col=Country) +
  geom_line(linewidth = 0.7) +
  ggtitle("death percentage vary across G20 countries for last 30 years") + labs(x= "Timeline", y = "Death number in percentage") +
  scale_x_continuous(breaks = c(1990,1995,2000,2005,2010,2015,2020), 
                     limits = c(1990,2020)) + 
  scale_y_continuous(breaks = c(5,10,15,20,25,30,35), 
                     limits = c(0,35))
ggplotly(deathpercentage)

#correlation between temperature change and death percentages
correlationMouse <- ggplot(data = g20countries) + aes(x=Deathpercentage, y=TempChangevalues) +
  geom_point(color="red", size= 1, aes(tooltip = Country)) +
  ggtitle("Correlation between temperature change and people death number percentage") +
  labs(x= "from air pollution death percentage", y="surface temperature change value")+
  geom_smooth(method='lm', formula=y~x, se=TRUE) # add the regression line

ggplotly(correlationMouse,tooltip="Country")


#third part
library(readr)
Forest_and_Carbon1 <- read_csv("C:/Users/LukaGvantse/Desktop/Forest_and_Carbon1.csv")
View(Forest_and_Carbon1)

forestdata <- select(Forest_and_Carbon1, -ISO2, -ISO3, -Unit, -Source, -CTS_Code, -CTS_Name, -CTS_Full_Descriptor,-ObjectId)
shareofforest <- forestdata %>%
  filter(Indicator == "Share of forest area")
shareofforest <- shareofforest %>%
  pivot_longer(cols = starts_with("F"), 
               names_to = "Year", values_to = "shareofforestpercentage")

shareofforest$Year <- gsub("F", "", shareofforest$Year)
shareofforest$Year <- as.numeric(shareofforest$Year)
sum(!complete.cases(shareofforest))
shareofforest <- na.omit(shareofforest)

biggest30c <- shareofforest %>%
  group_by(Country) %>%
  filter(Country=="Russian Federation"|Country=="Canada"|Country=="China, P.R.: Mainland"|Country=="Americas"|Country=="Brazil"|Country=="Australia"|Country=="India"|Country=="Argentina"|Country=="Kazakhstan, Rep. of"|Country=="Algeria"|
  Country=="Congo, Dem. Rep. of the"|Country=="Saudi Arabia"|Country=="Mexico"|Country=="Indonesia"|Country=="Sudan"|Country=="Libya"|Country=="Mongolia"|Country=="Peru"|Country=="Chad"|Country=="Niger"|Country=="Angola"|Country=="Mali"|
    Country=="South Africa"|Country=="Colombia"|Country=="Ethiopia, The Federal Dem. Rep. of"|Country=="Mauritania, Islamic Rep. of"|Country=="Egypt, Arab Rep. of"|Country=="Nigeria"|Country=="Pakistan"|Country=="Namibia") %>%
           summarize(averageforestshareperc = round(mean(shareofforestpercentage),2))
#see the visual side
ggplot(data=biggest30c) + aes(x=Country, y= averageforestshareperc, fill=Country) + geom_bar(stat = "identity") + 
  labs(title ="share of forest in largest 30 countries", x="Countries", y="average shares of forest in percentages") + 
  ggtitle("share of forest in largest 30 countries") + coord_flip()
#change the column name and reorder to be the same and can merge
biggest30c$Country[biggest30c$Country == 'Mauritania, Islamic Rep. of'] <- 'Mauritania'
biggest30c$Country[biggest30c$Country == 'China, P.R.: Mainland'] <- 'China'
biggest30c$Country[biggest30c$Country == 'Congo, Dem. Rep. of the'] <- 'Democratic Republic of the Congo'
biggest30c$Country[biggest30c$Country == 'Egypt, Arab Rep. of'] <- 'Egypt'
biggest30c$Country[biggest30c$Country == 'Ethiopia, The Federal Dem. Rep. of'] <- 'Ethiopia'
biggest30c$Country[biggest30c$Country == 'Kazakhstan, Rep. of'] <- 'Kazakhstan'
biggest30c$Country[biggest30c$Country == 'Americas'] <- 'United States of America'
biggest30 <- biggest30 %>% arrange(Country)
biggest30c <- biggest30c %>% arrange(Country)
biggest30new <- cbind(biggest30, averageforestsharepercent=biggest30c$averageforestshareperc)
#vizualization
correlation <- ggplot(data = biggest30new) + aes(x=averageAQIvalue, y=averageforestsharepercent) +
  geom_point(color="green", size= 1, aes(tooltip = Country)) +
  ggtitle("Correlation between temperature change and share of forest in largest 30 country") +
  labs(x= "average AQI value", y="share of forest in area")+
  geom_smooth(method='lm', formula=y~x, se=TRUE)

ggplotly(correlation,tooltip="Country")


#first part: first visualization in shiny that user can choose the number of countries
selectTopn <- top_n(biggest30, n=5, wt = averageAQIvalue)

ui<-dashboardPage(
  dashboardHeader(title="most air polluted countries"),
  dashboardSidebar(
    sliderInput(inputId="selectedNumber",
                label="choose any number",
                value=10,min=5, max= 30, step=1)
  ),
  
  dashboardBody(
    fluidPage(
      fluidRow(
        column(
          width=12,
          box(plotOutput("plot1"),
              width=NULL)
        )
      )
    )
  )
)

#server function with code
server <- function(input,output) {
  v <- reactive({
    validate(
      need(input$selectedNumber >=5 & input$selectedNumber <=30,
           "Please choose a number between 5 and 30")
    )
  })
  
  selectTopn <-reactive({
    top_n(biggest30,input$selectedNumber,wt=averageAQIvalue)
  })
   
  output$plot1 <- renderPlot({
    
    v()
    selectTopn() %>% 
      ggplot() + aes(x=Country, y= averageAQIvalue, fill=Country) + geom_bar(stat = "identity") + 
      labs(title ="temperature values in largest 30 countries", x="Countries", y="average values") + 
      ggtitle(paste("temperature values in largest", input$selectedNumber, "countries")) + coord_flip()
  })
  
}
#call of shiny app
shinyApp(ui=ui, server=server) 


#third part visualization in shiny
selectTopn <- top_n(biggest30c, n=5, wt = averageforestshareperc)

ui<-dashboardPage(
  dashboardHeader(title="average share of forest in 30 largest countries"),
  dashboardSidebar(
    numericInput(inputId="selectedNumber",
                label="enter the number, how many countries you want to see",
                value=10,min=3, max= 30, step=1)
  ),
  
  dashboardBody(
    fluidPage(
      fluidRow(
        column(
          width=12,
          box(plotOutput("plot2"),
              width=NULL)
        )
      )
    )
  )
)

#server function with code
server <- function(input,output) {
  v <- reactive({
    validate(
      need(input$selectedNumber >=3 & input$selectedNumber <=30,
           "Please enter a number between 3 and 30")
    )
  })
  
  selectTopn <-reactive({
    top_n(biggest30c,input$selectedNumber,wt=averageforestshareperc)
  })
  
  output$plot2 <- renderPlot({
    
    v()
    selectTopn() %>% 
      ggplot() + aes(x=Country, y= averageforestshareperc, fill=Country) + geom_bar(stat = "identity") + 
      labs(title ="share of forest in largest 30 countries", x="Countries list", y="average shares of forest in percentages") + 
      ggtitle(paste("share of forest in largest",input$selectedNumber,"countries")) + coord_flip()
  })

}
#call of shiny app
shinyApp(ui=ui, server=server)