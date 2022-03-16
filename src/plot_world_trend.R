library(tidyverse)
library(gapminder)
library(dash)
library(dashHtmlComponents)
library(dashCoreComponents)
library(plotly)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(ggplot2)
library(purrr)
library(ggthemes)

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

app$layout(
  dbcContainer(
    list(
      htmlLabel('Year'),
      dccSlider(
        id='YEAR1',
        min = 1952,
        max = 2007,
        step = 5,
        marks = list(
          "1952" = "1952",
          "1957" = "1957",
          "1962" = "1962",
          "1967" = "1967",
          "1972" = "1972",
          "1977" = "1977",
          "1982" = "1982",
          "1987" = "1987",
          "1992" = "1992",
          "1997" = "1997",
          "2002" = "2002",
          "2007" = "2007"
        ),
        value = 'year'
      ),
      dccDropdown(
        id='col-select',
        options = list(list(label = 'Life Expectancy', value = 'lifeExp'),
                       list(label = "Population", value = 'pop'),
                       list(label = 'GDP per Capita', value = 'gdpPercap')),
        value = 'pop'
      ),
      dccGraph(id='plot-area')
    )
  )
)

app$callback(
  output('plot-area', 'figure'),
  list(input('col-select', 'value'), input('YEAR1', 'value')),
  function(xcol, saal) {
    if (xcol == "pop" & saal == 1952) { 
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = sum) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() +  geom_vline(xintercept = 1952, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p) }
    else if (xcol == "pop" & saal == 1957) { 
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = sum) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() +  geom_vline(xintercept = 1957, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p) }
    else if (xcol == "pop" & saal == 1962) { 
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = sum) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() +  geom_vline(xintercept = 1962, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p) }
    else if (xcol == "pop" & saal == 1967) { 
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = sum) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() +  geom_vline(xintercept = 1967, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p) }
    else if (xcol == "pop" & saal == 1972) { 
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = sum) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() +  geom_vline(xintercept = 1972, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p) }
    else if (xcol == "pop" & saal == 1977) { 
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = sum) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() +  geom_vline(xintercept = 1977, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p) }
    else if (xcol == "pop" & saal == 1982) { 
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = sum) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() +  geom_vline(xintercept = 1982, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p) }
    else if (xcol == "pop" & saal == 1987) { 
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = sum) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() +  geom_vline(xintercept = 1987, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p) }
    else if (xcol == "pop" & saal == 1992) { 
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = sum) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() +  geom_vline(xintercept = 1992, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p) }
    else if (xcol == "pop" & saal == 1997) { 
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = sum) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() +  geom_vline(xintercept = 1997, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p) }
    else if (xcol == "pop" & saal == 2002) { 
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = sum) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() +  geom_vline(xintercept = 2002, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p) }
    else if (xcol == "pop" & saal == 2007) { 
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = sum) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() +  geom_vline(xintercept = 2007, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p) }
    else if (xcol == "lifeExp" & saal == 1952) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 1952, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else if (xcol == "lifeExp" & saal == 1957) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 1957, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else if (xcol == "lifeExp" & saal == 1962) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 1962, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else if (xcol == "lifeExp" & saal == 1967) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 1967, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else if (xcol == "lifeExp" & saal == 1972) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 1972, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else if (xcol == "lifeExp" & saal == 1977) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 1977, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else if (xcol == "lifeExp" & saal == 1982) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 1982, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else if (xcol == "lifeExp" & saal == 1987) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 1987, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else if (xcol == "lifeExp" & saal == 1992) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 1992, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else if (xcol == "lifeExp" & saal == 1997) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 1997, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else if (xcol == "lifeExp" & saal == 2002) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 2002, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else if (xcol == "lifeExp" & saal == 2007) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 2007, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else if (xcol == "gdpPercap" & saal == 1952) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 1952, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else if (xcol == "gdpPercap" & saal == 1957) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 1957, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else if (xcol == "gdpPercap" & saal == 1962) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 1962, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else if (xcol == "gdpPercap" & saal == 1967) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 1967, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else if (xcol == "gdpPercap" & saal == 1972) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 1972, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else if (xcol == "gdpPercap" & saal == 1977) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 1977, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else if (xcol == "gdpPercap" & saal == 1982) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 1982, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else if (xcol == "gdpPercap" & saal == 1987) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 1987, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else if (xcol == "gdpPercap" & saal == 1992) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 1992, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else if (xcol == "gdpPercap" & saal == 1997) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 1997, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else if (xcol == "gdpPercap" & saal == 2002) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 2002, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else if (xcol == "gdpPercap" & saal == 2007) {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 2007, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
    else {
      p <- ggplot(gapminder, aes(x = year,
                                 y = !!sym(xcol),
                                 color = continent)) +
        geom_line(stat = 'summary', fun = mean) + 
        scale_y_continuous(labels = scales::comma)+
        scale_x_continuous(breaks = c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)) + 
        scale_x_log10() + geom_vline(xintercept = 1952, linetype = "longdash") +
        ggthemes::scale_color_tableau()
      ggplotly(p)  
    }
  }
)

app$run_server(host = '0.0.0.0')
