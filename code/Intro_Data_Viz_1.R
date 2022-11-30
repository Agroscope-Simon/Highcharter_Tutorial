library(tidyverse)
library(highcharter)
library(htmlwidgets)
library(RColorBrewer)
library(here)
library(quantmod)


# Load data

nations <- read_csv(here("data", "nations.csv")) %>%
  mutate(gdp_tn = round(gdp_percap*population/1000000000000, 2))
  
# prepare data
big4 <- nations %>%
  filter(iso3c == "USA"| iso3c == "CHN" | iso3c == "DEU" | iso3c == "JPN" ) %>%
  arrange(year) 

# basic symbol-and-line chart, default settings

# define colors
cols <- brewer.pal(4, "Set1")

big4 %>% 
hchart("streamgraph", hcaes(x = year, y = gdp_tn, group = country)) %>%      # basic definition
  hc_colors(cols) %>%                                                        # COLOR
  hc_xAxis(title = list(text="Year")) %>%                                    # x-axis
  hc_yAxis(title = list(text="GDP ($ trillion)"))  %>%                       # y-axis
  hc_chart(style = list(fontFamily = "Georgia",                  
                        fontWeight = "bold")) %>%                               # FONT
  hc_plotOptions(series = list(marker = list(symbol = "circle"))) %>%           # SYMBOLS        
  hc_legend(align = "right",                                         
            verticalAlign = "top") %>%                                       # LEGEND
  hc_tooltip(shared = TRUE,                    
             borderColor = "black",
             pointFormat = "{point.country}: {point.gdp_tn:.2f}<br>")       # TOOLTIP


big4_chart <- big4 %>% 
  hchart("line", hcaes(x = year, y = gdp_tn, group = country)) %>%      # basic definition
  hc_colors(cols) %>%                                                        # COLOR
  hc_xAxis(title = list(text="Year")) %>%                                    # x-axis
  hc_yAxis(title = list(text="GDP ($ trillion)"))  %>%                       # y-axis
  hc_chart(style = list(fontFamily = "Georgia",                  
                        fontWeight = "bold")) %>%                               # FONT
  hc_plotOptions(series = list(marker = list(enabled = F))) %>%           # SYMBOLS        
  hc_legend(align = "right",                                         
            verticalAlign = "top") %>%                                       # LEGEND
  hc_tooltip(shared = TRUE,                    
             borderColor = "black",
             pointFormat = "{point.country}: {point.gdp_tn:.2f}<br>")       # TOOLTIP

# save the chart
saveWidget(big4_chart, "big4.html", selfcontained = TRUE, libdir = NULL, background = "white")



# prepare data
regions <- nations %>%
  group_by(year,region) %>%
  summarize(gdp_tn = sum(gdp_tn, na.rm = TRUE)) %>%
  arrange(year,region)

# set color palette
cols <- brewer.pal(7, "Set2")

# basic area chart, default options
regions %>%
  hchart("area", hcaes(x = year,
                       y = gdp_tn, 
                       group = region))%>%
  hc_colors(cols) %>% 
  hc_chart(style = list(fontFamily = "Georgia",
                        fontWeight = "bold")) %>%
  hc_plotOptions(series = list(stacking = "normal",
                               marker = list(enabled = FALSE,
                                             states = list(hover = list(enabled = FALSE))),
                               lineWidth = 0.5,
                               lineColor = "white")) %>%
  hc_xAxis(title = list(text="Year")) %>%
  hc_yAxis(title = list(text="GDP ($ trillion)")) %>%
  hc_legend(align = "right", verticalAlign = "top",
            layout = "vertical") %>%
  hc_tooltip(enabled = FALSE)

# Changing the order the countries are stacked
# load forcats package, for handling categorical variables
library(forcats)

# convert region to a categorical variable, reverse order from default alphabetical using fct_relevel()
regions <- regions %>%
  mutate(region = as.factor(region),
         region = fct_relevel(region,
                              "Sub-Saharan Africa",
                              "South Asia",
                              "North America",
                              "Middle East & North Africa",
                              "Latin America & Caribbean",
                              "Europe & Central Asia",
                              "East Asia & Pacific"))

# run the chart code again, also reversing the colors, and the order of the legend
cols <- rev(cols)

regions %>%
  hchart("area", hcaes(x = year,
                       y = gdp_tn, 
                       group = region)) %>%
  hc_colors(cols) %>% 
  hc_chart(style = list(fontFamily = "Georgia",
                        fontWeight = "bold")) %>%
  hc_plotOptions(series = list(stacking = "normal",
                               marker = list(enabled = FALSE,
                                             states = list(hover = list(enabled = FALSE))),
                               lineWidth = 0.5,
                               lineColor = "white")) %>%
  hc_xAxis(title = list(text="Year")) %>%
  hc_yAxis(title = list(text="GDP ($ trillion)")) %>%
  hc_legend(align = "right", 
            verticalAlign = "top",
            layout = "vertical",
            reversed = TRUE) %>%
  hc_tooltip(enabled = FALSE)




# load data
food_stamps <- read_csv(here("data/food_stamps.csv"))

# set colors
cols <- c("red","black")

# draw chart
food_stamps %>% 
 hchart("column", hcaes(x=year, y=participants), 
                name = "Participants (millions)") %>%
  hc_add_series(data = food_stamps$costs,
                name = "Costs ($ billions)",
                type = "line") %>%
  hc_xAxis(categories = food_stamps$year,
           tickInterval = 5) %>%
  hc_colors(cols) %>%
  hc_chart(style = list(fontFamily = "Georgia",
                        fontWeight = "bold"))

# Multiple y-Axis
highchart() %>%
  hc_yAxis_multiples(
    list(title = list(text = "Participants (millions)")),
    list(title = list(text = "Costs ($ billions)"),
         opposite = TRUE)
  ) %>%
  hc_add_series(data = food_stamps$participants,
                name = "Participants (millions)",
                type = "column",
                yAxis = 0) %>%
  hc_add_series(data = food_stamps$costs,
                name = "Costs ($ billions)",
                type = "line",
                yAxis = 1) %>%
  hc_xAxis(categories = food_stamps$year,
           tickInterval = 5) %>%
  hc_colors(cols) %>%
  hc_chart(style = list(fontFamily = "Georgia",
                        fontWeight = "bold"))


# retrieve data for each company
google <- getSymbols("GOOG", src = "yahoo", auto.assign = FALSE)
facebook <- getSymbols("META", src = "yahoo", auto.assign = FALSE)
amazon <- getSymbols("AMZN", src = "yahoo", auto.assign = FALSE)

# set color palette
cols <- brewer.pal(3,"Set1")

# draw chart
highchart(type = "stock") %>% 
  hc_colors(cols) %>%
  hc_add_series(google$GOOG.Adjusted, name = "Google") %>%
  hc_add_series(facebook$META.Adjusted, name = "Facebook") %>%
  hc_add_series(amazon$AMZN.Adjusted, name = "Amazon") %>%
  hc_legend(enabled = TRUE,
            verticalAlign = "top") %>% 
  hc_chart(style = list(fontFamily = "Georgia",
                        fontWeight = "bold")) %>%
  hc_tooltip(borderColor = "black")


# install and load dygraphs
install.packages("dygraphs")
library(dygraphs)

# combine adjusted prices into a single xts object
companies <- cbind(google$GOOG.Adjusted, facebook$META.Adjusted, amazon$AMZN.Adjusted)

# rename the variables, so that they displace nicely in the legend
names(companies) <- c("Google","Facebook","Amazon")

# draw the chart
dygraph(companies,
        ylab = "Adjusted close") %>% 
  dyOptions(colors = brewer.pal(3, "Set1")) %>%
  dyRangeSelector() %>%
  dyAxis("x", drawGrid = FALSE)


# install and load leaflet and rdgal

library(leaflet)
library(rgdal)

# make leaflet map centered on Berkeley
leaflet() %>% 
  setView(lng = -122.2705383, lat = 37.8698807, zoom = 11) %>%
  addTiles()

# make leaflet map centered on Berkeley with CartoDB tiles
leaflet() %>%
  setView(lng = -122.2705383, lat = 37.8698807, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron")


# load seismic risk shapefile
seismic_risk <- readOGR(here("data/seismic_risk_clip"))


# load quakes data from USGS earthquakes API
quakes <- read_csv("http://earthquake.usgs.gov/fdsnws/event/1/query?starttime=1965-01-01T00:00:00&minmagnitude=6&format=csv&latitude=39.828175&longitude=-98.5795&maxradiuskm=6000&orderby=magnitude")


# view summary of seismic_risk data
summary(seismic_risk)


# load the seismic risk data into a leaflet map
seismic <- leaflet(data=seismic_risk)

# set breaks for custom bins
breaks <- c(0,19,39,59,79,200)

# set palette
binpal <- colorBin("Reds", seismic_risk$ACC_VAL, breaks)

# make choropleth map of seismic risks
seismic %>%
  setView(lng = -98.5795, lat = 39.828175, zoom = 4) %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(
    stroke = FALSE,
    fillOpacity = 0.7,
    smoothFactor = 0.1,
    color = ~binpal(ACC_VAL)
  )

# make choropleth map of seismic hazards
seismic %>%
  setView(lng = -98.5795, lat = 39.828175, zoom = 4) %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(
    stroke = FALSE, 
    fillOpacity = 0.7, 
    smoothFactor = 0.1,
    color = ~binpal(ACC_VAL)
  ) %>%
  # add historical earthquakes
  addCircles(
    data = quakes, 
    radius = sqrt(10^quakes$mag)*50, 
    color = "#000000",
    weight = 0.2,
    fillColor ="#ffffff",
    fillOpacity = 0.3,
    popup = paste0("<strong>Magnitude: </strong>", quakes$mag, "</br>",
                   "<strong>Date: </strong>", format(as.Date(quakes$time), "%b %d, %Y"))
  )

# make choropleth map of seismic hazards
seismic_map <- seismic %>%
  setView(lng = -98.5795, lat = 39.828175, zoom = 4) %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(
    stroke = FALSE, 
    fillOpacity = 0.7, 
    smoothFactor = 0.1,
    color = ~binpal(ACC_VAL)
  ) %>%
  # add historical earthquakes
  addCircles(
    data = quakes, 
    radius = sqrt(10^quakes$mag)*50, 
    color = "#000000",
    weight = 0.2,
    fillColor ="#ffffff",
    fillOpacity = 0.3,
    popup = paste0("<strong>Magnitude: </strong>", quakes$mag, "</br>",
                   "<strong>Date: </strong>", format(as.Date(quakes$time), "%b %d, %Y"))
  ) %>%
  # add legend
  addLegend (
    "bottomleft", pal = binpal, values = ~ACC_VAL,
    title = "Seismic risk",
    opacity = 0.7)

seismic_map


# make multi-layered leaflet map with layer-switching control
# make choropleth map of seismic hazards
seismic <- seismic %>%
  setView(lng = -98.5795, lat = 39.828175, zoom = 4) %>%
  addProviderTiles("CartoDB.Positron", group = "CartoDB") %>% 
  addProviderTiles("Stamen.TonerLite", group = "Toner") %>%
  addPolygons(
    stroke = FALSE, fillOpacity = 0.7, 
    smoothFactor = 0.1,
    color = ~binpal(ACC_VAL)
  ) %>%
  # add historical earthquakes
  addCircles(
    data=quakes, 
    radius = sqrt(10^quakes$mag)*50, 
    weight = 0.2, 
    color = "#000000", 
    fillColor ="#ffffff",
    fillOpacity = 0.3,
    popup = paste0("<strong>Magnitude: </strong>", quakes$mag, "</br>",
                   "<strong>Date: </strong>", format(as.Date(quakes$time), "%b %d, %Y")),
    group = "Quakes"
  ) %>%
  # add legend
  addLegend(
    "bottomleft", pal = binpal, values = ~ACC_VAL,
    title = "Seismic risk",
    opacity = 0.7
  ) %>%
  # add layers control
  addLayersControl(
    baseGroups = c("CartoDB", "Toner"),
    overlayGroups = "Quakes",
    options = layersControlOptions(collapsed = FALSE)
  )

# draw map
saveWidget(seismic, "seismic.html", selfcontained = TRUE, libdir = NULL, background = "white")
