library(readr)
library(httr)
library(rjson)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(stringr)
library(jpeg)
library(grid)
library(shiny)
library(rsconnect)
library(data.table)


rawdata<-read.csv("C:/Users/chaud/Desktop/Snap_lending-club-Project/loan.csv")
View(rawdata)
dim(rawdata)


a <- sample(rawdata$id,500)
a1 <- subset(rawdata, rawdata$id %in% a)
View(a1)

#funtion to find columns which has Na values
find_na = function(x)
{
  na_count <-sapply(x, function(y) sum(length(which(is.na(y)))))
  na_count <- data.frame(na_count)
  na_count <- round((na_count/nrow(x))*100,2)
  return(na_count)
}

View(find_na(a1))

#clubbed loan statuses: (Current, fully paid, issued and does not..: fully paid) as good status and rest as bad
a1$status <- ifelse(a1$loan_status == "Current" | 
                                  a1$loan_status == "Fully Paid" |
                                  a1$loan_status == "Issued" |
                                  a1$loan_status == "Does not meet the credit policy.  Status:Fully Paid",
                                "good","bad")

table(a1$status)

#Changing employement lenght into numeric variable
a1$emp_length = as.numeric(ifelse(as.character(a1$emp_length )=="< 1 year",'0',
                                              ifelse(as.character(a1$emp_length )=="1 year",'1',
                                                     ifelse(as.character(a1$emp_length )=="2 years",'2',
                                                            ifelse(as.character(a1$emp_length )=="3 years",'3',
                                                                   ifelse(as.character(a1$emp_length )=="4 years",'4',
                                                                          ifelse(as.character(a1$emp_length )=="5 years",'5',
                                                                                 ifelse(as.character(a1$emp_length )=="6 years",'6',
                                                                                        ifelse(as.character(a1$emp_length )=="7 years",'7',
                                                                                               ifelse(as.character(a1$emp_length )=="8 years",'8',
                                                                                                      ifelse(as.character(a1$emp_length )=="9 years",'9',
                                                                                                             ifelse(as.character(a1$emp_length )=="n/a",'0',
                                                                                                                    ifelse(as.character(a1$emp_length )=="10+ years",'11','12')
                                                                                                             ))))))))))))




#Converting State Abbreviation into State Name
stateFromLower <-function(x) {
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
  
}

#Adding new column with the name "region"
a1$region<-stateFromLower(a1$addr_state)

# removing columns with more than 50% NA values
a2 = a1[,c(-29,-30,-51,-54,-55,-60,-61,-62,
                                    -63,-64,-65,-66,-67,-68,-69,-70,-72,-73,-74)]

View(find_na(a2))


b1<-a2
View(b1)

####################COde to change region into lat and long#################

data <- paste0("[",paste(paste0("\"",b1$region,"\""),collapse=","),"]")
url  <- "http://www.datasciencetoolkit.org/street2coordinates"
response <- POST(url,body=data)
json     <- fromJSON(content(response,type="text"))
geocode  <- do.call(rbind,sapply(json,
                                 function(x) c(long=x$longitude,lat=x$latitude)))

geo.dsk <- function(addr){ # single address geocode with data sciences toolkit
  require(httr)
  require(rjson)
  url      <- "http://www.datasciencetoolkit.org/maps/api/geocode/json"
  response <- GET(url,query=list(sensor="FALSE",address=addr))
  json <- fromJSON(content(response,type="text"))
  loc  <- json['results'][[1]][[1]]$geometry$location
  return(c(address=addr,long=loc$lng, lat= loc$lat))
}
result <- do.call(rbind,lapply(as.character(b1$region),geo.dsk))
result <- data.frame(result)

View(result)

colnames(result)[which(colnames(result) == 'address')] <- 'region'
View(result)

c1 <- merge(b1,result,by="region", all.b1=T)
View(c1)


newdat<-c1
View(newdat)

##########################################Presentation in Shiny###################

newdat <-   newdat %>% select(region, loan_amnt, issue_d, long, lat)

newdat$issue_d = as.Date(gsub("^","01-",newdat$issue_d), format="%d-%b-%Y")#Converting the issue date to the reqired format
head(newdat$issue_d)

newdat <-
  newdat %>%
  separate(col = issue_d, into = c("Year", "Month"), sep = "-", remove = FALSE, extra = "drop")

newdat$issue_d <- as.Date(newdat$issue_d, "%Y-%m-%d")


newdat$issue_d<-NULL

View(find_na(newdat))

newdat$Month <- as.numeric(newdat$Month)
newdat$Year <- as.numeric(newdat$Year)

newdat <-
  newdat %>%
  mutate(Quarter = ifelse(Month >= 3 & Month <= 5, "Q1", 
                          ifelse(Month >= 6 & Month <= 8, "Q2",
                                 ifelse(Month >= 9 & Month <= 11, "Q3", "Q4"))))

View(newdat)


img <- readJPEG("C:/Users/chaud/Desktop/Snap_lending-club-Project/mapimage.JPG")


newdat$region <- as.factor(newdat$region)

orig_locale <- Sys.getlocale("LC_TIME") 
Sys.setlocale("LC_TIME", "C")

densityPerstate <- as.data.frame(table(newdat$region))

densityPerstate <-
  densityPerstate %>% 
  arrange(-Freq)

colnames(densityPerstate) <- c("Number of loans per state", "Freq")
View(densityPerstate)



######################## ui.R ######################

ui <- fluidPage(
  
  titlePanel("Loan Nature across States"),
  
  sidebarPanel(
    
    selectInput("region", "Select State:", levels(newdat$region)),
    sliderInput("Year", "Year:", min = 2008, max = 2016, value = c(2006, 2016), sep = "", ticks = FALSE),
    selectInput("Quarter", "Quarter (Months):", c("All" = "all",
                                                  "Q1 (Mar, Apr, May)" = "Q1",
                                                  "Q2 (Jun, Jul, Aug)" = "Q2", 
                                                  "Q3 (Sep, Oct, Nov)" = "Q3", 
                                                  "Q4 (Dec, Jan, Feb)" = "Q4")),
    # selectInput("dayofweek", "Day of Week:", c("All" = "all",
    #                                            "Weekdays" = "Weekday", 
    #                                            "Weekends" = "Weekend")),
    sliderInput("loan_amnt", "Loan Amount:", min = 2200, max = 35000, value = 0),
    #sliderInput("timeend", "To:", min = 0, max = 24, value = 24),
    tags$hr(),
    tableOutput("table")
    
  ),
  
  mainPanel(
    
    plotOutput("graph", width = "700px", height = "700px")
    
  )
)


########################  server.R #####################


server <- function(input, output) {
  
  output$table <- renderTable(densityPerstate)
  
  temp <- reactive({
    
    region <- input$region
    minyear <- input$year[1]
    maxyear <- input$year[2]
    Quarter <- input$quarter
    #dayofweek <- input$dayofweek
    loan_amnt <- input$loan_amnt
    #timeend <- input$timeend
    
    #Apply filters
    
    test <- 
      newdat %>% 
      filter(region == region,
             Year >= minyear,
             Year <= maxyear)
    
    # test <- 
    #   
    #   if(timeend >= timestart){
    #     
    #     test %>% 
    #       filter(Hour >= input$timestart & Hour <= input$timeend)
    #     
    #   } else {
    #     
    #     test %>% 
    #       filter(Hour >= input$timestart | Hour <= input$timeend)
    #     
    #   }
    
    test <-
      
      if(Quarter != "all") {
        
        test %>%
          filter(Quarter == quarter)
        
      } else {
        test
      }
    
    # test <-
    #   
    #   if (dayofweek != "all") {
    #     
    #     test %>%
    #       filter(Weekendornot == dayofweek)
    #     
    #   } else {
    #     test
    #   }
    
    test <- as.data.frame(test)
    
    test
    
  })
  
  output$graph <- renderPlot({
    
    temp2 <- temp()
    
    temp2 %>% 
      ggplot() +
      aes(x = long, y = lat) +
      xlim(-99.1, -74.95) +
      ylim(39.85, 40.15) +
      annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 
                        -Inf, Inf, -Inf, Inf) +
      geom_point(size = 3, alpha = 0.6, color = "firebrick1" )
    
  })
}

shinyApp(ui = ui, server = server)








