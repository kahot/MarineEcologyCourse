# Install the package!
install.packages("coronavirus")


library(coronavirus)
update_dataset()

covid19_df <- refresh_coronavirus_jhu()

head(covid19_df)

data("coronavirus")
head(coronavirus)

library(tidyr)
library(dplyr)

summary_df <- coronavirus %>% 
    filter(type == "confirmed") %>%
    group_by(country) %>%
    summarise(total_cases = sum(cases)) %>%
    arrange(-total_cases)

summary_df %>% head(20) 


library(plotly)

coronavirus %>% 
    group_by(type, date) %>%
    summarise(total_cases = sum(cases)) %>%
    pivot_wider(names_from = type, values_from = total_cases) %>%
    arrange(date) %>%
    mutate(active = confirmed - death - recovered) %>%
    mutate(active_total = cumsum(active),
           recovered_total = cumsum(recovered),
           death_total = cumsum(death)) %>%
    plot_ly(x = ~ date,
            y = ~ active_total,
            name = 'Active', 
            fillcolor = '#1f77b4',
            type = 'scatter',
            mode = 'none', 
            stackgroup = 'one') %>%
    add_trace(y = ~ death_total, 
              name = "Death",
              fillcolor = '#E41317') %>%
    add_trace(y = ~recovered_total, 
              name = 'Recovered', 
              fillcolor = 'forestgreen') %>%
    layout(title = "Distribution of Covid19 Cases Worldwide",
           legend = list(x = 0.1, y = 0.9),
           yaxis = list(title = "Number of Cases"),
           xaxis = list(title = "Source: Johns Hopkins University Center for Systems Science and Engineering"))

#### # Plot US data
unique(coronavirus$country)
levels(factor(coronavirus$country))

us<-coronavirus[coronavirus$country=="US"]

unique(coronavirus$country)
coronavirus %>% 
    group_by(type, date) %>%
    summarise(total_cases = sum(cases)) %>%
    pivot_wider(names_from = type, values_from = total_cases) %>%
    arrange(date) %>%
    mutate(active = confirmed - death - recovered) %>%
    mutate(active_total = cumsum(active),
           recovered_total = cumsum(recovered),
           death_total = cumsum(death)) %>%
    plot_ly(x = ~ date,
            y = ~ active_total,
            name = 'Active', 
            fillcolor = '#1f77b4',
            type = 'scatter',
            mode = 'none', 
            stackgroup = 'one') %>%
    add_trace(y = ~ death_total, 
              name = "Death",
              fillcolor = '#E41317') %>%
    add_trace(y = ~recovered_total, 
              name = 'Recovered', 
              fillcolor = 'forestgreen') %>%
    layout(title = "Distribution of Covid19 Cases in U.S.",
           legend = list(x = 0.1, y = 0.9),
           yaxis = list(title = "Number of Cases"),
           xaxis = list(title = "Source: Johns Hopkins University Center for Systems Science and Engineering"))
