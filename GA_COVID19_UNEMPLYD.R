# https://rviews.rstudio.com/2020/04/16/state-unemployment-claims/
# by Jonathan Regenstein

# https://rviews.rstudio.com/

# Load the following libraries for GA Unemployment
# Insurance Claims

library(Hmisc)
library(Quandl)
library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)
library(tidybayes)
library(dlstats)
library(dplyr)
library(psych)
library(ggplot2)
library(LearnBayes)
library(tidyquant)

# The State of Georgia Umemployment Insurance Claims thru April 2020
ga_claims <- 
  "GAICLAIMS" %>% 
  tq_get(get = "economic.data", 
         from = "1999-01-01") %>% 
  rename(claims = price) 

ga_claims %>% 
  slice(1, n())

# Now to show a nice visualization of the GA State damage
(
  ga_claims %>% 
    ggplot(aes(x = date, y = claims)) +
    geom_line(color = "cornflowerblue")  +
    labs(
      x = "",
      y = "",
      title = "Georgia Unemployment Claims",
      subtitle = str_glue("{min(ga_claims$date)} through {max(ga_claims$date)}")
    ) +
    theme_minimal() +
    scale_y_continuous(labels = scales::comma)
)









# Load the following libraries for NC Unemployment
# Insurance Claims

library(Hmisc)
library(Quandl)
library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)
library(tidybayes)
library(dlstats)
library(dplyr)
library(psych)
library(ggplot2)
library(LearnBayes)
library(tidyquant)

# The State of North Carolina Umemployment Insurance Claims thru April 2020
nc_claims <- 
  "NCICLAIMS" %>% 
  tq_get(get = "economic.data", 
         from = "1999-01-01") %>% 
  rename(claims = price) 

nc_claims %>% 
  slice(1, n())

# Now to show a nice visualization of the NC State damage
(
  nc_claims %>% 
    ggplot(aes(x = date, y = claims)) +
    geom_line(color = "cornflowerblue")  +
    labs(
      x = "",
      y = "",
      title = "North Carolina Unemployment Insurance Claims",
      subtitle = str_glue("{min(ga_claims$date)} through {max(ga_claims$date)}")
    ) +
    theme_minimal() +
    scale_y_continuous(labels = scales::comma)
)

# Now let's create a series of Average monthly claims
nc_claims %>%
  mutate(
    year = year(date),
    month =  month(date, label = T, abbr  = T),
    week = week(date)
  ) %>%
  group_by(year, month) %>%
  filter(n() >= 4) %>% 
  summarise(avg_claims = mean(claims)) %>%
  ggplot(aes(x = avg_claims)) +
  geom_density(aes(fill = as_factor(month))) +
  facet_grid(rows = vars(as_factor(month))) +
  guides(fill = guide_legend(title = "")) +
  labs(
    title = "North Carolina Distribution of Avg Monthly Claims",
    subtitle = str_glue("{min(nc_claims$date)} through {max(nc_claims$date)}"),
    y = "",
    x = ""
  ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_x_continuous(labels = scales::comma)

# Now let's build a heat map to investigate months by year.
# We'll first create a column to hold the year and month 
# for each observation.

nc_claims %>% 
  mutate(year = year(date),
         month=  month(date, label = T, abbr  = T)) %>% 
  head()

nc_claims %>% 
  mutate(year = year(date),
         month=  month(date, label = T, abbr  = T)) %>% 
  tail()

# Next, we calculate the average number of claims for 
# each month of each year. We start with a group_by
# (year, month) before a call to summarise().
nc_claims %>% 
  mutate(year = year(date),
         month=  month(date, label = T, abbr  = T)) %>%
  group_by(year, month) %>%
  summarise(avg_claims = mean(claims)) %>% 
  head()

nc_claims %>% 
  mutate(year = year(date),
         month=  month(date, label = T, abbr  = T)) %>%
  group_by(year, month) %>%
  summarise(avg_claims = mean(claims)) %>% 
  tail()

# Display data in thousands with K, instead of detail
nc_claims %>%
  mutate(year = year(date),
         month =  month(date, label = T, abbr  = T)) %>%
  group_by(year, month) %>% 
  summarise(avg_claims = mean(claims)) %>%
  mutate(
    avg_claims_labels = scales::number_format(
      accuracy = .1,
      scale = 1 / 1000,
      suffix = "k",
      big.mark = ","
    )(avg_claims)
  ) %>%
  tail()

#  Here is a first crack at the North Carolina Heat Map
nc_claims %>%
  mutate(year = year(date),
         month =  month(date, label = T, abbr  = T)) %>%
  group_by(year, month) %>%
  filter(n() >= 4) %>% 
  summarise(avg_claims = mean(claims)) %>%
  mutate(
    avg_claims_labels = scales::number_format(
      accuracy = 1,
      scale = 1 / 1000,
      suffix = "k",
      big.mark = ","
    )(avg_claims)
  ) %>%
  ggplot(aes(
    x = month,
    y = year,
    fill = avg_claims,
    label = avg_claims_labels
  )) +
  geom_tile() 

# Clean up to tidy up the Heat Map Chart
nc_claims %>%
  mutate(year = year(date),
         month =  month(date, label = T, abbr  = T)) %>%
  group_by(year, month) %>%
  filter(n() >= 4) %>% 
  summarise(avg_claims = mean(claims)) %>%
  mutate(
    avg_claims_labels = scales::number_format(
      accuracy = 1,
      scale = 1 / 1000,
      suffix = "k",
      big.mark = ","
    )(avg_claims)
  ) %>%
  ggplot(aes(
    x = month,
    y = year,
    fill = avg_claims,
    label = avg_claims_labels
  )) +
  geom_tile(color = "white", size = .8, aes(height = 1)) 

# We're NOT done yet, the shades of Blue do not really draw 
# your eye
(
  nc_claims %>%
    mutate(
      year = year(date),
      month =  month(date, label = T, abbr  = T)
    ) %>%
    group_by(year, month) %>%
    filter(n() >= 4) %>% 
    summarise(avg_claims = mean(claims)) %>%
    mutate(
      avg_claims_labels = scales::number_format(
        accuracy = 1,
        scale = 1 / 1000,
        suffix = "k",
        big.mark = ","
      )(avg_claims)
    ) %>%
    ggplot(
      aes(
        x = month,
        y = year,
        fill = avg_claims,
        label = avg_claims_labels,
        text = str_glue("average claims:
                        {scales::comma(avg_claims)}")
      )
    ) +
    geom_tile(color = "white", size = .8, aes(height = 1)) +
    scale_fill_gradient(
      low = "blue",
      high = "red",
      labels = scales::comma
    ) +
    geom_text(color = "white" , size = 3.5) +
    theme_minimal() +
    theme(
      plot.caption = element_text(hjust = 0),
      panel.grid.major.y = element_blank(),
      legend.key.width = unit(1, "cm"),
      panel.grid = element_blank()
    ) +
    labs(
      y  = "",
      title = "Heatmap of NC Monthly Avg Unemployment Insurance Claims",
      fill = "Avg Claims",
      x = ""
    ) +
    scale_y_continuous(breaks =  scales::pretty_breaks(n = 18))
)

# we'll grab the worst week for each year,
(
  nc_claims %>%
    mutate(
      month = month(date, label = TRUE, abbr = FALSE),
      year = year(date)
    ) %>%
    group_by(year) %>%
    mutate(
      max_claims = max(claims),
      max_week_color = case_when(claims == max_claims ~ as.character(date),
                                 TRUE ~ "NA")
    ) %>%
    filter(max_week_color != "NA") %>%
    ggplot(aes(
      x = max_week_color,
      y = claims,
      fill = month,
      text = str_glue("{date}
                      claims: {scales::comma(claims)}")
    )) +
    geom_col(width = .5) +
    labs(
      x = "",
      title = str_glue("Highest Unemployment Claims Week, by Year
                                in North Carolina"),
      y = ""
    ) +
    scale_y_continuous(
      labels = scales::comma,
      limits = c(0, NA),
      breaks = scales::pretty_breaks(n = 6)
    ) +
    scale_fill_brewer(palette = "Dark2") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45),
      plot.title = element_text(hjust = .5)
    )
)

# Here's a chart of that period, 
# with the worst unemployment claims of each month displayed.
(
  nc_claims %>%
    mutate(
      month = month(date, label = TRUE, abbr = FALSE),
      year = year(date)
    ) %>%
    filter(between(
      date, ymd("2007-05-01"), ymd("2009-05-01")
    )) %>%
    group_by(year, month) %>%
    mutate(
      max_claims = max(claims),
      max_week_color = case_when(claims == max_claims ~ as.character(date),
                                 TRUE ~ "NA")
    ) %>%
    filter(max_week_color != "NA") %>%
    ggplot(aes(
      x = max_week_color,
      y = claims,
      fill = month,
      text = str_glue("{date}
                      claims: {scales::comma(claims)}")
    )) +
    geom_col(width = .5) +
    labs(
      x = "",
      title = str_glue("Worst NC Unemployment Claims Week of Each Month
                                mid-2007 to mid-2009"),
      y = "",
      fill = ""
    ) +
    scale_y_continuous(
      labels = scales::comma,
      limits = c(0, NA),
      breaks = scales::pretty_breaks(n = 6)
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45),
      plot.title  = element_text(hjust = .5)
    )
)

# The worst weeks during this period were 
# December and January of 2008 and 2009, with 35,000 and 41,000 claims.
(
  nc_claims %>%
    mutate(
      month = month(date, label = TRUE, abbr = FALSE),
      year = year(date)
    ) %>%
    filter(between(
      date, ymd("2018-04-01"), ymd("2020-05-01")
    )) %>%
    group_by(year, month) %>%
    mutate(
      max_claims = max(claims),
      max_week_color = case_when(claims == max_claims ~ as.character(date),
                                 TRUE ~ "NA")
    ) %>%
    filter(max_week_color != "NA") %>%
    ggplot(aes(
      x = max_week_color,
      y = claims,
      fill = month,
      text = str_glue("{date}
                      claims: {scales::comma(claims)}")
    )) +
    geom_col(width = .5) +
    labs(
      x = "",
      title = str_glue("Worst NC Unemployment Claims Week of Each Month
                                mid-2018 to mid-2020"),
      y = "",
      fill = ""
    ) +
    scale_y_continuous(
      labels = scales::comma,
      limits = c(0, NA),
      breaks = scales::pretty_breaks(n = 6)
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45),
      plot.title  = element_text(hjust = .5)
    )
)

# Find all 50 state abbreviations from FRED to perform analysis
datasets::state.abb %>% 
  head(50)

# First, let's create the function 
# and just test the creation of the FRED code.
all_state_claims_importer <- function(state_abbreviation){
  fred_code <- str_glue("{state_abbreviation}ICLAIMS")
  
  fred_code %>% 
    tibble()
}
all_state_claims_importer("CA")

# What happens when we apply this to the state.abb data set.
map_dfr(state.abb, all_state_claims_importer) %>% 
  head()

# The hard part is done. Now we pass those codes to tq_get(), 
# and use map_dfr() to iteratively pass each abbreviation to FRED. 
# the _dfr will bind our results together, row wise, into a `tibble.
all_state_claims_importer <- function(state_abbrevation) {
  fred_code <- str_glue("{state_abbrevation}ICLAIMS")
  
  fred_code %>%
    tq_get(get = "economic.data",
           from = "1999-01-01") %>%
    mutate(state = state_abbrevation) %>%
    rename(claims = price) %>%
    select(date, state, claims)
}

all_state_claims_tibble <-
  map_dfr(state.abb, all_state_claims_importer) 


all_state_claims_tibble %>% 
  group_by(state) %>% 
  slice(1, n())

# We can recreate any of those previous visualizations 
# for the state of our choice, by using filter(state == "state of choice"). 
# Let’s take a look at one of the charts for California.
(
  all_state_claims_tibble %>%
    filter(state == "CA") %>%
    mutate(
      month = month(date, label = TRUE, abbr = FALSE),
      year = year(date)
    ) %>%
    group_by(year) %>%
    mutate(
      max_claims = max(claims),
      max_week_color = case_when(claims == max_claims ~ as.character(date),
                                 TRUE ~ "NA")
    ) %>%
    filter(max_week_color != "NA") %>%
    ggplot(aes(
      x = max_week_color,
      y = claims,
      fill = month,
      text = str_glue("{date}
                      claims: {scales::comma(claims)}")
    )) +
    geom_col(width = .5) +
    labs(
      x = "",
      title = str_glue("Highest Unemployment Claims Week, by Year
                                in California"),
      y = ""
    ) +
    scale_y_continuous(
      labels = scales::comma,
      limits = c(0, NA),
      breaks = scales::pretty_breaks(n = 6)
    ) +
    scale_fill_brewer(palette = "Dark2") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45),
      plot.title = element_text(hjust = .5)
    )
)

# We're NOT done yet, the shades of Blue do not really draw your eye
# The State of California Unemployment Insurance Claims thru April 2020
ca_claims <- 
  "CAICLAIMS" %>% 
  tq_get(get = "economic.data", 
         from = "1999-01-01") %>% 
  rename(claims = price) 

ca_claims %>% 
  slice(1, n())

# Now do Heat Map of CA Unemployment Insurance Claims
(
  ca_claims %>%
    mutate(
      year = year(date),
      month =  month(date, label = T, abbr  = T)
    ) %>%
    group_by(year, month) %>%
    filter(n() >= 4) %>% 
    summarise(avg_claims = mean(claims)) %>%
    mutate(
      avg_claims_labels = scales::number_format(
        accuracy = 1,
        scale = 1 / 1000,
        suffix = "k",
        big.mark = ","
      )(avg_claims)
    ) %>%
    ggplot(
      aes(
        x = month,
        y = year,
        fill = avg_claims,
        label = avg_claims_labels,
        text = str_glue("average claims:
                        {scales::comma(avg_claims)}")
      )
    ) +
    geom_tile(color = "white", size = .8, aes(height = 1)) +
    scale_fill_gradient(
      low = "blue",
      high = "red",
      labels = scales::comma
    ) +
    geom_text(color = "white" , size = 3.5) +
    theme_minimal() +
    theme(
      plot.caption = element_text(hjust = 0),
      panel.grid.major.y = element_blank(),
      legend.key.width = unit(1, "cm"),
      panel.grid = element_blank()
    ) +
    labs(
      y  = "",
      title = "Heatmap of CA Monthly Avg Unemployment Insurance Claims",
      fill = "Avg Claims",
      x = ""
    ) +
    scale_y_continuous(breaks =  scales::pretty_breaks(n = 18))
)

# Let's end on a more positive note and 
# examine the months in California when claims are at their lowest
(
  all_state_claims_tibble %>%
    filter(state == "CA") %>%
    mutate(
      month = month(date, label = TRUE, abbr = FALSE),
      year = year(date)
    ) %>%
    group_by(year) %>%
    mutate(
      min_claims = min(claims),
      min_week_color = case_when(claims == min_claims ~ as.character(date),
                                 TRUE ~ "NA")
    ) %>%
    filter(min_week_color != "NA") %>%
    ggplot(aes(
      x = min_week_color,
      y = claims,
      fill = month,
      text = str_glue("{date}
                      claims: {scales::comma(round(claims, digits = 0))}")
    )) +
    geom_col(width = .5) +
    labs(
      x = "",
      title = str_glue("Lowest Unemployment Claims Week, by Year
                                in California"),
      y = ""
    ) +
    scale_y_continuous(
      labels = scales::comma,
      limits = c(0, NA),
      breaks = scales::pretty_breaks(n = 6)
    ) +
    scale_fill_brewer(palette = "Dark2") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45),
      plot.title = element_text(hjust = .5)
    )
)





