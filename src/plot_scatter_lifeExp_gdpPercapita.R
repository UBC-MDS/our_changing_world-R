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
      dccGraph(id='plot-area')
    )
  )
)

app$callback(
  output('plot-area', 'figure'),
  list(input('YEAR1', 'value')),
  function(saal) {
      if (saal == 1952) {
        df <- gapminder %>%
          filter(country != "Kuwait") %>%
          filter(year==1952)
       p <- ggplot(df, aes(x = lifeExp, y = gdpPercap, color=continent)) + 
          facet_wrap(~year) + geom_point() + 
          ggthemes::scale_color_tableau() + 
          xlab("Life Expectancy") + 
          ylab("GDP Per Capita") +
         scale_y_continuous(labels = scales::comma)
       ggplotly(p)  
      }
    else if (saal == 1957) {
        df <- gapminder %>%
          filter(country != "Kuwait") %>%
          filter(year==1957)
        p <- ggplot(df, aes(x = lifeExp, y = gdpPercap, color=continent)) + 
          facet_wrap(~year) + geom_point() + 
          ggthemes::scale_color_tableau() + 
          xlab("Life Expectancy") + 
          ylab("GDP Per Capita") +
          scale_y_continuous(labels = scales::comma)
        ggplotly(p)  
    }
    else if (saal == 1962) {
      df <- gapminder %>%
        filter(country != "Kuwait") %>%
        filter(year==1962)
      p <- ggplot(df, aes(x = lifeExp, y = gdpPercap, color=continent)) + 
        facet_wrap(~year) + geom_point() + 
        ggthemes::scale_color_tableau() + 
        xlab("Life Expectancy") + 
        ylab("GDP Per Capita") +
        scale_y_continuous(labels = scales::comma)
      ggplotly(p)  
    }
    else if (saal == 1967) {
      df <- gapminder %>%
        filter(country != "Kuwait") %>%
        filter(year==1967)
      p <- ggplot(df, aes(x = lifeExp, y = gdpPercap, color=continent)) + 
        facet_wrap(~year) + geom_point() + 
        ggthemes::scale_color_tableau() + 
        xlab("Life Expectancy") + 
        ylab("GDP Per Capita") +
        scale_y_continuous(labels = scales::comma)
      ggplotly(p)  
    }
    else if (saal == 1972) {
      df <- gapminder %>%
        filter(country != "Kuwait") %>%
        filter(year==1972)
      p <- ggplot(df, aes(x = lifeExp, y = gdpPercap, color=continent)) + 
        facet_wrap(~year) + geom_point() + 
        ggthemes::scale_color_tableau() + 
        xlab("Life Expectancy") + 
        ylab("GDP Per Capita") +
        scale_y_continuous(labels = scales::comma)
      ggplotly(p)  
    }
    else if (saal == 1977) {
      df <- gapminder %>%
        filter(country != "Kuwait") %>%
        filter(year==1977)
      p <- ggplot(df, aes(x = lifeExp, y = gdpPercap, color=continent)) + 
        facet_wrap(~year) + geom_point() + 
        ggthemes::scale_color_tableau() + 
        xlab("Life Expectancy") + 
        ylab("GDP Per Capita") +
        scale_y_continuous(labels = scales::comma)
      ggplotly(p)  
    }
    else if (saal == 1982) {
      df <- gapminder %>%
        filter(country != "Kuwait") %>%
        filter(year==1982)
      p <- ggplot(df, aes(x = lifeExp, y = gdpPercap, color=continent)) + 
        facet_wrap(~year) + geom_point() + 
        ggthemes::scale_color_tableau() + 
        xlab("Life Expectancy") + 
        ylab("GDP Per Capita") +
        scale_y_continuous(labels = scales::comma)
      ggplotly(p)  
    }
    else if (saal == 1987) {
      df <- gapminder %>%
        filter(country != "Kuwait") %>%
        filter(year==1987)
      p <- ggplot(df, aes(x = lifeExp, y = gdpPercap, color=continent)) + 
        facet_wrap(~year) + geom_point() + 
        ggthemes::scale_color_tableau() + 
        xlab("Life Expectancy") + 
        ylab("GDP Per Capita") +
        scale_y_continuous(labels = scales::comma)
      ggplotly(p)  
    }
    else if (saal == 1992) {
      df <- gapminder %>%
        filter(country != "Kuwait") %>%
        filter(year==1992)
      p <- ggplot(df, aes(x = lifeExp, y = gdpPercap, color=continent)) + 
        facet_wrap(~year) + geom_point() + 
        ggthemes::scale_color_tableau() + 
        xlab("Life Expectancy") + 
        ylab("GDP Per Capita") +
        scale_y_continuous(labels = scales::comma)
      ggplotly(p)  
    }
    else if (saal == 1997) {
      df <- gapminder %>%
        filter(country != "Kuwait") %>%
        filter(year==1997)
      p <- ggplot(df, aes(x = lifeExp, y = gdpPercap, color=continent)) + 
        facet_wrap(~year) + geom_point() + 
        ggthemes::scale_color_tableau() + 
        xlab("Life Expectancy") + 
        ylab("GDP Per Capita") +
        scale_y_continuous(labels = scales::comma)
      ggplotly(p)  
    }
    else if (saal == 2002) {
      df <- gapminder %>%
        filter(country != "Kuwait") %>%
        filter(year==2002)
      p <- ggplot(df, aes(x = lifeExp, y = gdpPercap, color=continent)) + 
        facet_wrap(~year) + geom_point() + 
        ggthemes::scale_color_tableau() + 
        xlab("Life Expectancy") + 
        ylab("GDP Per Capita") +
        scale_y_continuous(labels = scales::comma)
      ggplotly(p)  
    }
    else {
      df <- gapminder %>%
        filter(country != "Kuwait") %>%
        filter(year==2007)
      p <- ggplot(df, aes(x = lifeExp, y = gdpPercap, color=continent)) + 
        facet_wrap(~year) + geom_point() + 
        ggthemes::scale_color_tableau() + 
        xlab("Life Expectancy") + 
        ylab("GDP Per Capita") +
        scale_y_continuous(labels = scales::comma)
      ggplotly(p)  
    }
    }
)

app$run_server(host = '0.0.0.0')