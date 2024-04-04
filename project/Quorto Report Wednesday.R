install.packages("tidyverse")

library(tidyverse)

unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")
data_right_3 <- read_csv("data_right_3.csv")

data_join <- full_join(unicef_indicator_1, data_right_3)
data_join <- full_join(unicef_metadata, unicef_indicator_1, by = join_by(country , year)

data_join <- unicef_indicator_1 %>%
full_join(data_right_3) %>%
full_join(unicef_metadata) 

#map 2022         

library(ggplot2)

map_world <- map_data("world")


data_join_2022 <- data_join %>% 
  filter(year == 2022)


map_data_join_2022 <- full_join(data_join_2022, map_world, by = c("country" = "region"))


ggplot(map_data_join_2022) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon() +
  scale_fill_gradient(low = "orange", high = "green") +
  labs(title = "Percentage of the Population using Surface Water in each country in 2022",
  fill = "%") +
  theme(
    text = element_text(family = "Arial")  
  )

#map 2010 

library(ggplot2)

map_world <- map_data("world")

data_join_2022 <- data_join %>%
  filter(year == 2010)

map_data_join_2010 <- full_join(data_join_2022, map_world, by = c("country" = "region"))


ggplot(map_data_join_2010) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon()+
  scale_fill_gradient(low = "orange", high = "green") +
  labs(title = "Percentage of the Population using Surface Water in each country in 2022",
       fill = "%") +
  theme(
    text = element_text(family = "Arial")
  )



#timeseries from 2000 to 2022 in each country 

install.packages("plotly")

library(plotly)

timeseries_plot_1 <- data_join %>%
  filter(year >= 2000 & year <= 2022)%>%
  ggplot()+
  aes(year, obs_value, group = country, color = continent)+
  geom_line()+
  
  labs(x = "Year",
       y = "Percentage of the Popualtion usign Surface Water",
       title = "Percentage of the Popualtion using Surface Water from 2000 to 2022")+
  theme(text = element_text (family = "Arial"))

hover_info <- paste("Country: ", data_join$country, "<br>",
                    "Observation Value: ", data_join$obs_value, "<br>",
                    "Life Expectancy at Birth: ", data_join$Life_exp_at_birth)

ggplotly(timeseries_plot_1) %>%
  layout(
    plot_bgcolor = "white",  
    legend = list(
      bgcolor = "lightgrey",  
      bordercolor = "black",  
      borderwidth = 1
    ),  # Setting legend border width
    xaxis = list(title = "Year", tickangle = 45),  
    yaxis = list(title = "Percentage of the Population using Surface Water")
  )


ggplotly(timeseries_plot_1) %>%
  layout(plot_bgcolor = "white",  
         legend = list(bgcolor = "lightgrey",  
                       bordercolor = "black",  
                       borderwidth = 1),  
         xaxis = list(title = "Year",  
                      tickangle = 45),  
         yaxis = list(title = "Percentage of the Population using Surface Water")) 
  
  


ggplotly(timeseries_plot_1)


#scatterplot 
scatter_plot <- ggplot(data_join)+
  aes(obs_value, life_exp, color = continent, size = obs_value)+
  geom_point(alpha=0.5)+
  
  labs(x = "Percentage of Population Using Surface Water",
       y = "Life Expectancy",
       title = "Evolution of the Relationship between Life Expectancy and the Percentage of Population Using Surface Water")+
  theme(text = element_text (family = "Arial"))
  
ggplotly(scatter_plot) %>%
  layout(
    hoverlabel = list(font = list(family = "Arial", color = "black")),
    plot_bgcolor = "white"
  )

  hover_info <- paste("Country: ", data_join$country, "<br>",
                      "Observation Value: ", data_join$obs_value, "<br>",
                      "Life Expectancy at Birth: ", data_join$Life_exp_at_birth)

  

ggplotly(scatter_plot)

  

#barchart 2020

library(ggplot2)

data_join %>%
  group_by(continent, year) %>%
  summarise(m_life_exp = mean(life_exp, na.rm = TRUE)) %>%
  filter(year == 2020) %>%
  ggplot() +
  aes(continent, m_life_exp, fill = continent) +
  geom_col() +
  labs(
    x = "Continent",
    y = "Life Expectancy",
    title = "Life Expectancy in each Continent in 2020"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "Arial"),  
    axis.text.x = element_blank()
  ) +
  scale_fill_manual(values = c("red", "green", "orange", "purple", "yellow", "blue"))



#barchart 2010

library(ggplot2)

data_join %>%
  group_by(continent, year) %>%
  summarise(m_life_exp = mean(life_exp, na.rm = TRUE)) %>%
  filter(year == 2010) %>%
  ggplot() +
  aes(continent, m_life_exp, fill = continent) +
  geom_col() +
  labs(
    x = "Continent",
    y = "Life Expectancy",
    title = "Life Expectancy in each Continent in 2010"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "Arial"),  # Change font family to Arial
    axis.text.x = element_blank()
  ) +
  scale_fill_manual(values = c("red", "green", "orange", "purple", "yellow", "blue"))



library(ggplot2)

data_join %>%
  group_by(continent, year) %>%
  summarise(m_life_exp = mean(life_exp, na.rm = TRUE)) %>%
  filter(year == 2010) %>%
  ggplot() +
  aes(continent, m_life_exp, fill = continent, size = m_life_exp) +  
  geom_point(shape = 21) +  
  labs(
    x = "Continent",
    y = "Life Expectancy",
    title = "Life Expectancy in each Continent in 2010"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "Arial"),  
    axis.text.x = element_blank()
  ) +
  scale_fill_manual(values = c("red", "green", "orange", "purple", "yellow", "blue"))




