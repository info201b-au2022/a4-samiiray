library(tidyverse)
library(scales)
library(dplyr)
library(ggplot2)
library(mapproj)
library(maps)
library(ggmap)
library(magrittr)

incarceration <-read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",
                         stringsAsFactors = FALSE)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
max_black_jail_state <- incarceration %>%
   select(state, black_jail_pop) %>%
   filter(black_jail_pop == max(black_jail_pop, na.rm = T)) %>%
   filter(state == max(state)) %>%
   pull(state)

min_white_jail_state <- incarceration %>%
   select(state, white_jail_pop) %>%
   filter(white_jail_pop == min(white_jail_pop, na.rm = T)) %>%
   filter(state == min(state)) %>%
   pull(state)

max_year_black <- incarceration %>%
   filter(black_jail_pop == max(black_jail_pop, na.rm = T)) %>%
   pull(year)

white_max_pop <- incarceration %>%
   select(year, white_jail_pop) %>%
   filter(year == max(year)) %>%
   filter(white_jail_pop == max(white_jail_pop, na.rm = T)) %>%
   pull(white_jail_pop)

black_max_pop <- incarceration %>%
   select(year, black_jail_pop) %>%
   filter(year == max(year)) %>%
   filter(black_jail_pop == max(black_jail_pop, na.rm = T)) %>%
   pull(black_jail_pop)

latinx_max_pop <-incarceration %>%
   select(year, latinx_jail_pop) %>%
   filter(year == max(year)) %>%
   filter(latinx_jail_pop == max(latinx_jail_pop, na.rm = T)) %>%
   pull(latinx_jail_pop)

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
   jailpopuresults <-incarceration %>%
      select(total_jail_pop, year) %>%
      drop_na() %>%
      group_by(year) %>%
      summarise(total_jail_pop = sum(total_jail_pop))
   return(jailpopuresults)  
}


# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
   get_data <- get_year_jail_pop()
   YearJailPops <- ggplot(get_data) +
      geom_col(mapping = aes(x=year, y= total_jail_pop)) +
      scale_y_continuous("Total Jail Population", labels = comma)
   return(YearJailPops)  
}





## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_jail_pop_by_states <- function(states) {
   jail_data_states <- incarceration %>%
      select(total_jail_pop, state, year, black_jail_pop) %>%
      drop_na() %>%
      group_by(year) %>%
      filter(state %in% states)
   return(jail_data_states)
}

plot_jail_pop_by_states <- function(states) {
   jail_states <- get_jail_pop_by_states(states)
   data_state <- jail_states %>%
      gather(key = jail_pop, value = State, state) %>%
      group_by(year, State) %>%
      summarise(jail_pop = sum(total_jail_pop), .groups = "drop")
   plot_states_jail <- ggplot(data = data_state) +
      geom_line(mapping = aes(x = year, y = jail_pop, color = State, size = 4))
   return(plot_states_jail)
   
}
results_line_plot <-plot_jail_pop_by_states(c("WA", "MN", "CA", "FL"))
## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
Jail <- data.frame(incarceration$black_jail_pop,
                   incarceration$latinx_jail_pop,
                   incarceration$year)
Jail <- na.omit(Jail)
blackjail_vs_latinxjail <-Jail %>%
   filter(incarceration.year == 2009 | incarceration.year == 2010 |
             incarceration.year == 2011 | incarceration.year == 2012 ) %>%
   gather(Variable, population, -incarceration.year)

compare <- ggplot(data = blackjail_vs_latinxjail, aes(
   x = incarceration.year, y = population, fill = Variable)) +
   geom_bar(stat = 'identity', position = 'dodge') +
   scale_fill_discrete(name = "Race",
                       labels = c("black popultation" ,"latinx popultation")) +
   labs(title = "black vs latinx in 2009 to 2012" , x = "year" , y = " Population")
## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
data <- incarceration %>%
   filter(year == max(year))

county <- map_data("county") %>%
   unite(polyname, region, subregion, sep = ",") %>%
   left_join(county.fips, by = "polyname")

map <- county %>%
   left_join(data, by = "fips") %>%
   filter(state == "TX", county_name != "unknown")

blank_theme <- theme_bw()
theme(
   axis.line = element_blank(),
   axis.text = element_blank(),
   axis.ticks = element_blank(),
   axis.title = element_blank(),
   plot.background = element_blank(),
   panel.grid.major = element_blank(),
   panel.grid.minor = element_blank(),
   panel.border = element_blank()
)
black_jail <- ggplot(map) +
   geom_polygon(
      mapping = aes(
         x = long, y = lat, group = group, fill = black_jail_pop ),
      color = "dark grey", size = 0.2) +
   coord_map() +
   scale_fill_continuous(
      low = "blue", high = "dark blue" ) +
   labs(fill = "Population") +
   blank_theme +
   ggtitle("Black jail population in Texas")
## Load data frame ---- 


