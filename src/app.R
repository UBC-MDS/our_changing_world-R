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
library(hrbrthemes)
library(ggthemes)
library(scales)

app <- Dash$new(external_stylesheets = dbcThemes$SKETCHY)

# Variables
year_breaks <- c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)


# components
header <- htmlH4(
  "Our Changing World!",
  className = "h1 bg-secondary bg-gradient-secondary p-2 mb-2 text-white text-center",
)

## Filter Layout
target_filter <- htmlDiv(
  list(
    dbcLabel("What do you want to know?", className = "h4"),
    dbcRadioItems(
      id = "target",
      options = list(
        list(label = "Life Expectancy", value = "lifeExp"),
        list(label = "Population", value = "pop"),
        list(label = "GDP per Capita", value = "gdpPercap")
      ),
      value = "pop",
      className = "mb-4",
      inline = TRUE
    )
  ),
  className = "mb-4",
)


year_filter <- htmlDiv(
  list(
    dbcLabel("Your Year Of Interest?", className = "h4"),
    dccSlider(
      id = "year_id",
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
      value = 2002
    )
  ),
  className = "mb-4"
)


filter_layout <- dbcCard(
  list(
    htmlBr(),
    target_filter,
    htmlBr(),
    htmlHr(),
    htmlBr(),
    year_filter,
    htmlBr()
  ),
  body = TRUE,
  color = "light",
)

## Plot layout
world_map <- dccGraph(
  id = "world-map",
  responsive = TRUE,
  className = "embed-responsive embed-responsive-item",
  style = list(width = "100%", height = "100%")
)


world_ranking <- htmlDiv(
  list(
    htmlH2("World Ranking"),
    htmlBr(),
    dccGraph(
      id = "world-ranking",
      # responsive = TRUE,
      className = "embed-responsive embed-responsive-item",
      style = list(width = "100%", height = "100%")
    )
  )
)

world_trend <- htmlDiv(
  list(
    ### Plot 4 goes here
    htmlH2("World Trend"),
    htmlBr(),
    dccGraph(
      id = "world-trend",
      # responsive = TRUE,
      className = "embed-responsive embed-responsive-item",
      style = list(width = "100%", height = "100%")
    )
  )
)

life_exp_vs_gdp <- htmlDiv(
  list(
    ### Plot 4 goes here
    htmlH2("Life Expectancy vs GDP per Capita"),
    htmlBr(),
    dccGraph(
      id = "life-exp-vs-gdp",
      responsive = TRUE,
      className = "embed-responsive embed-responsive-item",
      style = list(width = "100%", height = "100%")
    )
  )
)

# Layout
app$layout(
  dbcContainer(
    list(
      dbcRow(
        list(
          header,
          dbcCol(filter_layout, className = "col-sm-4"),
          dbcCol(world_map)
        ),
        justify = "center",
        className = "mb-4"
      ),
      dbcRow(list(
        dbcCol(life_exp_vs_gdp, className = "col-sm-6"),
        dbcCol(world_trend, className = "col-sm-6")
      ),
      justify = "center",
      className = "mb-4"
      ),
      dbcRow(world_ranking,
        className = "mb-4",
        align = "end"
      )
    ),
    className = "g-0",
    fluid = "TRUE",
    style = list(
      align = "center",
      "padding-left" = "10px",
      "padding-right" = "10px"
    )
  )
)


get_para <- function(yr, colnm) {
  col_var <- sym(colnm)
  col_name_df <- data.frame(
    row.names = c("lifeExp", "pop", "gdpPercap"),
    val = c("Life Expectancy", "Population", "GDP per Capita")
  )

  col_max_scale_df <- data.frame(
    row.names = c("lifeExp", "pop", "gdpPercap"),
    val = c(6, 15, 8)
  )

  summary_df <- gapminder %>%
    group_by(year) %>%
    summarise(sum = sum(!!col_var))
  scale <- summary_df %>%
    filter(year == yr) %>%
    select(sum) %>%
    pull()
  scale <- scale / max(summary_df$sum) * col_max_scale_df[colnm, 1]
  list(col_name_df[colnm, 1], scale)
}

app$callback(
  output("world-map", "figure"),
  list(input("year_id", "value"), input("target", "value")),
  plot_world <- function(yr, colnm) {
    world <- map_data("world") %>%
      filter(lat > -56)

    countries <- read_csv("data/world_country.csv",
      col_select = c(4, 3, 2)
    )
    colnames(countries) <- c("country", "long", "lat")

    col_var <- sym(colnm)

    gap_geo <- gapminder %>%
      inner_join(countries)

    param <- get_para(yr, colnm)
    title_txt <- param[[1]]
    scale <- param[[2]]
    txt <- paste0("World Overview of ", title_txt)

    world_map <- world %>%
      ggplot() +
      geom_polygon(
        data = world,
        aes(long, lat, group = group),
        color = "white", fill = "lightgray", size = 0.1
      ) +
      annotate(
        geom = "text", x = 30, y = 90, label = txt,
        color = "darkgray", size = 4
      ) +
      annotate(
        geom = "text", x = 178, y = 85, label = yr,
        color = "darkgray", size = 9
      ) +
      geom_point(
        data = filter(gap_geo, year == 2002),
        aes(long, lat, color = continent, size = !!col_var),
        alpha = 0.7
      ) +
      scale_size(range = c(0, scale)) +
      guides(size = "none") +
      xlab(NULL) +
      ylab(NULL) +
      labs(color = "Continent") +
      scale_color_ipsum() + theme_map() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
      )
    ggplotly(world_map) %>% layout(legend = list(itemsizing = "constant"))
  }
)

# World Trend
app$callback(
  output("world-trend", "figure"),
  list(input("year_id", "value"), input("target", "value")),
  function(year_filter, xcol) {
    if (xcol == "pop") {
      p <- ggplot(gapminder, aes(
        x = year,
        y = !!sym(xcol),
        color = continent
      )) +
        geom_line(stat = "summary", fun = sum) +
        scale_y_continuous(labels = scales::comma) +
        scale_x_continuous(breaks = year_breaks) +
        scale_x_log10() +
        labs(x = "Year", y = "Population") +
        geom_vline(xintercept = year_filter, linetype = "longdash")
    } else if (xcol == "lifeExp") {
      p <- ggplot(gapminder, aes(
        x = year,
        y = !!sym(xcol),
        color = continent
      )) +
        geom_line(stat = "summary", fun = mean) +
        scale_y_continuous(labels = scales::comma) +
        scale_x_continuous(breaks = year_breaks) +
        scale_x_log10() +
        labs(x = "Year", y = "Life Expectancy") +
        geom_vline(xintercept = year_filter, linetype = "longdash")
    } else if (xcol == "gdpPercap") {
      p <- ggplot(gapminder, aes(
        x = year,
        y = !!sym(xcol),
        color = continent
      )) +
        geom_line(stat = "summary", fun = mean) +
        scale_y_continuous(labels = scales::comma) +
        scale_x_continuous(breaks = year_breaks) +
        scale_x_log10() +
        labs(x = "Year", y = "GDP Per Capita") +
        geom_vline(xintercept = year_filter, linetype = "longdash")
    } else {
      p <- ggplot(gapminder, aes(
        x = year,
        y = !!sym(xcol),
        color = continent
      )) +
        geom_line(stat = "summary", fun = mean) +
        scale_y_continuous(labels = scales::comma) +
        scale_x_continuous(breaks = year_breaks) +
        scale_x_log10() +
        geom_vline(xintercept = year_filter, linetype = "longdash")
    }
    p <- p + scale_color_ipsum() + theme_ipsum_tw()

    ggplotly(p)
  }
)

# World ranking
app$callback(
  output("world-ranking", "figure"),
  list(input("year_id", "value"), input("target", "value")),
  function(x_year, y_axis) {
      df <- gapminder %>% 
        filter(year == x_year)
      bar <- df %>% 
        arrange(desc(!!sym(y_axis))) %>% 
        mutate(ranking = paste0('#', as.numeric(rownames(df)))) %>% 
        ggplot() +
        aes(x = !!sym(y_axis), 
            y = reorder(country, !!sym(y_axis)), 
            fill = continent, 
            label = ranking) +
        geom_bar(stat = 'identity') +
        geom_text() +
        theme(text = element_text(size = 20)) +
        scale_x_continuous(labels = comma)
      
      if (y_axis == "pop") {
        chart <- bar + labs(x = "Population", y = "Country")
      }
      if (y_axis == "lifeExp") {
        chart <- bar + labs(x = "Life Expectancy [years]", y = "Country")
      }
      if (y_axis == "gdpPercap") {
        chart <- bar + labs(x = "GDP per Capita [USD]", y = "Country")
      }
      
      chart <- chart + scale_color_ipsum() + theme_ipsum_tw()
      chart_final <- ggplotly(
        chart, height = 3000, width=800, tooltip = c(y_axis)
        ) |> layout(xaxis = list(side = "top"))
      
      return(chart_final)
    }
)


# Life Expectancy vs per capita
app$callback(
  output("life-exp-vs-gdp", "figure"),
  list(input("year_id", "value")),
  function(saal) {
    df <- gapminder %>%
      filter(year == saal) %>%
      dplyr::select(-year)

    # Interactive version
    p <- df %>%
      mutate(gdpPercap = round(gdpPercap, 0)) %>%
      mutate(pop = round(pop / 1000000, 2)) %>%
      mutate(lifeExp = round(lifeExp, 1)) %>%
      arrange(desc(pop)) %>%
      mutate(country = factor(country, country)) %>%
      mutate(text = paste("Country: ", country, "\nPopulation (M): ",
        pop, "\nLife Expectancy: ",
        lifeExp, "\nGdp per capita: ", gdpPercap,
        sep = ""
      )) %>%
      ggplot(aes(
        x = gdpPercap, y = lifeExp, size = pop,
       fill = continent, text = text
      ), shape = 21, colour = "white", alpha = 0.8) +
      geom_point() +
      scale_size(range = c(1.4, 19), name = "Population (M)") +
      ylab("Life Expectancy[Years]") +
      xlab("GDP Per Capita[USD]") +
      scale_x_log10(labels = scales::dollar) +
      scale_fill_brewer(palette = "Set2") +
      scale_size_continuous(range = c(1, 20)) +
      guides(size = FALSE) 

    p <- p + scale_color_ipsum() + theme_ipsum_tw()

    # ggplot interactive with plotly
    ggplotly(p, tooltip = "text")
  }
)


# # Life Expectancy vs per capita
# app$callback(
#   output("life-exp-vs-gdp", "figure"),
#   list(input("year_id", "value")),
#   function(saal) {
#     df <- gapminder %>%
#       filter(country != "Kuwait") %>%
#       filter(year == saal)
#     p <- ggplot(df, aes(x = lifeExp, y = gdpPercap, color = continent)) +
#       facet_wrap(~year) +
#       geom_point() +
#       ggthemes::scale_color_tableau() +
#       xlab("Life Expectancy") +
#       ylab("GDP Per Capita") +
#       scale_y_continuous(labels = scales::comma)
#     ggplotly(p)
#   }
# )

app$run_server(host = "0.0.0.0")