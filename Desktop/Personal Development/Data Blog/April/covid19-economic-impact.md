What can the US learn from the economic impact of COVID-19 in China?
================
Stone
2020-04-04

  - [It’s a tough war: Understanding the magnitude of economic
    shock](#its-a-tough-war-understanding-the-magnitude-of-economic-shock)
      - [China’s Overview](#chinas-overview)
      - [Unemployment rates in both countries are much higher than 2008
        financial
        crisis](#unemployment-rates-in-both-countries-are-much-higher-than-2008-financial-crisis)
  - [PMI: Signs of rebound starts one month after the Wuhan
    Lockdown](#pmi-signs-of-rebound-starts-one-month-after-the-wuhan-lockdown)
      - [China’s PMI Rebound](#chinas-pmi-rebound)
      - [US PMI](#us-pmi)

``` r
# Libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(rvest)
library(ggpubr)

# Parameters
html_cn_all <-
  "https://www.visualcapitalist.com/covid-19-economic-impact/"
html_cn_pmi <- "http://www.stats.gov.cn/tjsj/zxfb/202003/t20200331_1735877.html"
file_us_pmi <- 
  "/Users/yywstone/Desktop/Personal Development/Data Blog/April/us_pmi.xlsx"
file_cn_covid19 <- 
  "/Users/yywstone/Desktop/Personal Development/Data Blog/April/cn_covid19.csv"
file_cn_unemployment <-
  "/Users/yywstone/Desktop/Personal Development/Data Blog/April/cn_unemployment.csv"
file_us_unemployment <-
  "/Users/yywstone/Desktop/Personal Development/Data Blog/April/us_unemployment.xlsx"
file_us_icsa <-
  "/Users/yywstone/Desktop/Personal Development/Data Blog/April/us_icsa.csv"
file_us_covid19 <-
  "/Users/yywstone/Desktop/Personal Development/Data Blog/April/us_covid19.csv"
  
# Colors for plot
deteriorating <- "#FFD700"
recovering <- "#C5E86C"
#===============================================================================
```

The COVID-19 pandemic has been impacting businesses across the globe. As
the first country that got hit by the virus, China’s experience with the
economic impact of the virus will serve as a good predictor for others.
In this article, I will investigate the changes in China’s key economic
indicators, and compare that with their US counterparts. This will be
divided into two parts:

  - It’s a tough war: Understanding the magnitude of economic shock
  - Light at the end of the tunnel: rebound in China’s PMI and what
    should US expect

## It’s a tough war: Understanding the magnitude of economic shock

### China’s Overview

``` r
css_selector <- "#tablepress-845"
cn_all <-
  html_cn_all %>% 
  read_html() %>% 
  html_node(css = css_selector) %>% 
  html_table() %>% 
  rename(
    "indicator" = `Economic Indicator`, 
    "change" = `Year-over-year Change (Jan-Feb 2020)`
  )
```

``` r
cn_all %>% 
  mutate(
    indicator = fct_reorder(indicator, change, .desc = TRUE),
    change = str_remove(change, "%") %>% as.double(change)
  ) %>% 
  ggplot(aes(indicator, change)) +
  geom_col() +
  scale_x_discrete(position = "top") +
  theme_minimal() +
  labs(
    x = NULL,
    y = "Percent change (%)",
    title = "Sharp drop in year-over-year Change for major indicators,Jan-Feb 2020",
    caption = "Source: National Bureau of Statistics\n*Excluding rural household investment"
  )
```

![](covid19-economic-impact_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### Unemployment rates in both countries are much higher than 2008 financial crisis

China’s unemployment rate jumped to 6.2% for February from 5.2% in
January and 5.3% a year earlier. According to CNBC, roughly 5 million
people in China lost their jobs amid the outbreak of the new coronavirus
in January and February.

Compared with unemployment caused by the 2008 financial crisis, the
numbers are much smaller - the numbers rose from 4.6% to 4.75%.

``` r
cn_unemployment <-
  file_cn_unemployment %>% 
  read_csv() %>% 
  mutate(date = mdy(date))
```

    ## Parsed with column specification:
    ## cols(
    ##   date = col_character(),
    ##   rate = col_double(),
    ##   annual_change = col_double()
    ## )

``` r
cn_unemployment %>%
  ggplot(aes(date, rate)) +
  geom_rect(
    aes(
      xmin = make_date(2008, 1), 
      ymin = -Inf, 
      xmax = make_date(2009, 6), 
      ymax = Inf
    ),
    fill = "lightgrey"
  ) +
  geom_point() +
  geom_path() +
  geom_point(
    data = cn_unemployment %>% filter(date >= ymd("2019-01-01")),
    color = "red"
  ) +
  geom_line(
    data = cn_unemployment %>% filter(date >= ymd("2019-01-01")),
    color = "red"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b %y") +
  annotate(
    "text",
    x = c(ymd("2020-02-01"), ymd("2008-01-01"), ymd("2008-01-01"), ymd("2008-01-01")),
    y = c(6.2, 6.2, 6, -Inf),
    hjust = c(1.05, 0, 0, 0),
    vjust = c(1, 1, 1, -0.2),
    label = 
      c(
        "Feburary unemployment rate at 6.2%", 
        "Urban surveyed unemployment rate",
        "Historical registered unemployment rate",
        "Recession"
      ),
    color = c("black", "red", "black", "grey35"),
    size = c(4, 3, 3, 3)
  ) +
  annotate(
    "segment",
    x = c(ymd("2007-01-01"), ymd("2007-01-01")),
    y = c(6.15, 5.95),
    xend = c(ymd("2007-12-01"), ymd("2007-12-01")),
    yend = c(6.15, 5.95),
    color = c("red", "black")
  ) +
  theme_minimal() +
  labs(
    x = NULL,
    y = "Urban unemployment rate",
    title = "China's unemployment rate increased due to the epidemic",
    subtitle = "But it stayed flat during the 2008 financial crisis",
    caption = "Source: National Bureau of Statistics, World Bank"
  )
```

![](covid19-economic-impact_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

The US unemployment rate presents an even grimmer picture. Claims for
unemployment benefits surged over the past few weeks.

The week ending March 21, 2020 shattered previous records, posting a
number five times higher than any in the history of the data: 3,283,000
new claims. That’s nearly 3.3 million new people out of work.

Then, a week later, that number was doubled again: 6.648 million new
claims for the week ending March 28. We’re far, far outside any previous
experience.

Based on the unemployment claims, three agencies have provided their
prediction about the March unemployment rate.

``` r
us_icsa <-
  file_us_icsa %>% 
  read_csv(skip = 1, col_names = c("date", "rate"))
```

    ## Parsed with column specification:
    ## cols(
    ##   date = col_date(format = ""),
    ##   rate = col_double()
    ## )

``` r
depressions <-
  tribble(
    ~ start_date, ~ end_date,
    ymd("1969-12-20"), ymd("1970-10-31"),
    ymd("1973-11-03"), ymd("1975-03-01"),
    ymd("1980-01-19"), ymd("1980-07-05"),
    ymd("1981-07-11"), ymd("1982-10-23"),
    ymd("1990-07-21"), ymd("1991-03-02"),
    ymd("2001-03-10"), ymd("2001-11-10"),
    ymd("2007-12-01"), ymd("2009-05-23")
  )
```

``` r
last <- ymd("2020-03-28")
second_last <- ymd("2020-03-21")
us_icsa %>% 
  ggplot() +
  geom_rect(
    aes(
      xmin = start_date,
      xmax = end_date,
      ymin = -Inf,
      ymax = Inf
    ),
    data = depressions,
    fill = "grey"
  ) +
  geom_line(aes(date, rate)) +
  geom_point(
    aes(date, rate),
    data = us_icsa %>% filter(date == last | date == second_last)
  ) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_comma()) +
  annotate(
    "text",
    x = c(last, second_last),
    y = 
      c(
        us_icsa %>% filter(date == last) %>% pull(rate), 
        us_icsa %>% filter(date == second_last) %>% pull(rate)
      ),
    hjust = 1.05,
    vjust = 1,
    label = c("Week ending on Mar 28", "Week ending on Mar 21"),
    size = 3
  ) +
  annotate(
    "text",
    x = c(last, second_last),
    y = 
      c(
        us_icsa %>% filter(date == last) %>% pull(rate), 
        us_icsa %>% filter(date == second_last) %>% pull(rate)
      ),
    hjust = 1.1,
    vjust = 2.2,
    label = c("6.648M", "3.307M"),
    size = 5,
    color = "red"
  ) +
  theme_minimal() +
  labs(
    x = NULL,
    y = NULL,
    title = "US initial claims for unemployment benefit surged",
    caption = "Source: U.S. Employment and Training Administration"
  )
```

![](covid19-economic-impact_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
us_unemployment <-
  file_us_unemployment %>% 
  read_xlsx(skip = 11) %>% 
  pivot_longer(cols = "Jan":"Dec", names_to = "month", values_to = "rate") %>% 
  drop_na(rate) %>% 
  mutate(month = ymd("2007-01-01") + months(row_number() - 1)) %>% 
  select(month, rate)
```

``` r
estimates <-
  tribble(
    ~ month, ~ name, ~ rate, 
    ymd("2020-03-01"), "US Treasury Estimate (20%)", 20,
    ymd("2020-03-01"), "Morgan Stanley Estimate (12.8%)", 12.8,
    ymd("2020-03-01"), "St Louis Fed Estimate (30%)", 30
  )

us_unemployment %>% 
  ggplot(aes(month, rate)) +
  geom_rect(
    aes(
      xmin = make_date(2008, 1), 
      ymin = -Inf, 
      xmax = make_date(2009, 6), 
      ymax = Inf
    ),
    fill = "lightgrey"
  ) +
  geom_point() +
  geom_path() +
  geom_point(aes(color = name), data = estimates) +
  geom_segment(
    aes(
      x = ymd("2020-02-01"), 
      y = us_unemployment %>% filter(month == max(month)) %>% pull(rate),
      xend = month,
      yend = rate,
      color = name
    ),
    data = estimates
  ) +
  geom_text(
    aes(x = month, y = rate, label = name, color = name),
    data = estimates,
    hjust = 1.05,
    vjust = 0
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b %y") +
  scale_y_continuous(breaks = scales::breaks_width(5)) +
  annotate(
    "text",
    x = ymd("2008-01-01"),
    y = -Inf,
    hjust = 0,
    vjust = -0.2,
    label = "Recession",
    color = "grey35",
    size = 3
  ) +
  guides(color = "none") +
  theme_minimal() +
  labs(
    x = NULL,
    y = "Unemployment rate (%)",
    title = "US unemployment due to the epidemic is projected to skyrocket",
    subtitle = "Far exceeding the impact of 2008 financial crisis",
    caption = "Source: Bureau of Labor Statistics, The Guardian"
  )
```

![](covid19-economic-impact_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## PMI: Signs of rebound starts one month after the Wuhan Lockdown

The Purchasing Managers’ Index (PMI) is an index of the prevailing
direction of economic trends in the manufacturing and service sectors.
China’s PMI rebound happens at the same time when the new case numbers
have slowed down its rate of increase. We could expect to see similar
recovery in US’ PMI once the rate of growth is contained.

### China’s PMI Rebound

``` r
css_selector <-
  "body > div.home > div.main > div.center > div.center_xilan > div > div > div > div:nth-child(29) > table"
df <-
  html_cn_pmi %>% 
  read_html() %>% 
  html_node(css = css_selector) %>% 
  html_table(fill = TRUE) %>% 
  as_tibble()
cn_pmi <-
  df %>% 
  filter(X1 != "") %>% 
  select(X1:X2) %>% 
  mutate(
    X2 = as.double(X2),
    year = str_extract(X1, "\\d+"),
    month = str_remove_all(X1, "[^\\d+]") %>% str_sub(5),
    month = make_date(year = year, month = month)
  ) %>% 
  select(
    month,
    "pmi" = X2
  )
```

``` r
mean_pmi <-
  cn_pmi %>% 
  summarize(mean = mean(pmi)) %>% 
  pull(mean)
  
cn_pmi_plot <-
  cn_pmi %>% 
  ggplot(aes(month, pmi)) +
  geom_rect(
    aes(
      xmin = ymd("2019-12-31"), 
      xmax = ymd("2020-02-15"), 
      ymin = -Inf, 
      ymax = Inf
    ),
    fill = deteriorating,
    alpha = 0.2
  ) +
  geom_rect(
    aes(
      xmin = ymd("2020-02-15"), 
      xmax = ymd("2020-04-01"), 
      ymin = -Inf, 
      ymax = Inf
    ),
    fill = recovering,
    alpha = 0.2
  ) +
  geom_hline(yintercept = mean_pmi, color = "red") +
  geom_hline(yintercept = 50, color = "blue") +
  geom_line() +
  geom_point() +
  geom_text(
    aes(label = pmi), 
    data = cn_pmi %>% filter(pmi > 51 | pmi < 40),
    hjust = -0.2,
    vjust = 0.5
  ) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels = "%b-%y", 
    minor_breaks = NULL
  ) +
  scale_y_continuous(breaks = scales::breaks_width(2), minor_breaks = NULL) +
  annotate(
    "text",
    x = c(ymd("2020-04-01"), ymd("2020-04-01")),
    y = c(mean_pmi, 50),
    hjust = c(0.7, 0.7),
    vjust = c(-0.2, -0.2),
    label = c("Average PMI", "No change"),
    color = c("red", "blue")
  ) +
  annotate(
    "text",
    x = c(ymd("2019-12-31"), ymd("2020-02-15")),
    y = c(Inf, Inf),
    hjust = c(0, 0),
    vjust = c(1, 1),
    label = c("Deteriorating", "Recovering"),
    color = c("#9c8302", "#517003"),
    size = c(3, 3)
  ) +
  annotate(
    "label",
    x = c(ymd("2019-12-31"), ymd("2020-01-23")),
    y = c(36, 36),
    hjust = c(1.2, 0.9),
    vjust = c(0, 0),
    label = c("Authorites confirmed first cases", "Wuhan\nlockdown"),
    size = c(3, 3)
  ) +
  annotate(
    "segment",
    x = ymd("2020-01-23"),
    xend = ymd("2020-01-23"),
    y = 36,
    yend = -Inf,
    arrow = arrow(length = unit(0.08, "inches"))
  ) +
  annotate(
    "segment",
    x = ymd("2019-12-01"),
    xend = ymd("2019-12-31"),
    y = 36,
    yend = -Inf,
    arrow = arrow(length = unit(0.08, "inches"))
  ) +
  theme_minimal() +
  labs(
    x = NULL,
    y = "Percentage Change in PMI (%)",
    title = "China's manufacturing Purchasing Managers' Index rebounds after Feburary",
    subtitle = "50% means no change from last month",
    caption = "Source: National Bureau of Statistics of China"
  )
```

``` r
cn_covid19 <-
  file_cn_covid19 %>% 
  read_csv() %>% 
  mutate(values = str_split(values, ",")) %>% 
  unnest(values) %>% 
  mutate(values = str_remove(values, "[\\[\\]]") %>% as.double()) %>% 
  select(values) %>% 
  mutate(date = ymd("2020-01-21") + days(row_number()))
```

    ## Warning: Missing column names filled in: 'X2' [2], 'X3' [3], 'X4' [4]

    ## Parsed with column specification:
    ## cols(
    ##   values = col_character(),
    ##   X2 = col_logical(),
    ##   X3 = col_logical(),
    ##   X4 = col_logical()
    ## )

``` r
cn_covid19_plot <-
  cn_covid19 %>% 
  ggplot(aes(date, values)) +
  geom_rect(
    aes(
      xmin = ymd("2020-01-15"),
      xmax = ymd("2020-02-15"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = deteriorating,
    alpha = 0.1
  ) +
  geom_rect(
    aes(
      xmin = ymd("2020-02-15"),
      xmax = ymd("2020-04-01"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = recovering,
    alpha = 0.1
  ) +
  geom_point() +
  geom_path() +
  scale_x_date(date_breaks = "7 days", date_labels = "%b-%d") +
  scale_y_continuous(
    breaks = scales::breaks_width(1e4),
    labels = scales::label_number(scale = 1e-3, suffix = "k")
  ) +
  annotate(
    "text",
    x = c(ymd("2020-01-16"), ymd("2020-02-15")),
    y = c(Inf, Inf),
    hjust = c(0, 0),
    vjust = c(1, 1),
    label = c("Deteriorating", "Recovering"),
    color = c("#9c8302", "#517003")
  ) +
  annotate(
    "label",
    x = ymd("2020-01-23"),
    y = 4000,
    hjust = 0.9,
    vjust = 0,
    label = "Wuhan\nlockdown",
    size = 3
  ) +
  annotate(
    "segment",
    x = ymd("2020-01-20"),
    xend = ymd("2020-01-23"),
    y = 4000,
    yend = -Inf,
    arrow = arrow(length = unit(0.08, "inches"))
  ) +
  theme_minimal() +
  labs(
    x = NULL,
    y = "Total cases",
    title = "Total cases in China",
    subtitle = "Increase in cases has been contained since mid Feb",
    caption = "Source: worldometer"
  )
```

``` r
cn_pmi_plot
```

![](covid19-economic-impact_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
cn_covid19_plot
```

![](covid19-economic-impact_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

### US PMI

``` r
us_pmi <-
  file_us_pmi %>% 
  read_xlsx(sheet = "Data") %>% 
  drop_na(...2) %>% 
  rename(
    "month" = `U.S. Purchasing Managers' Index (PMI) February 2020`,
    "pmi" = "...2"
  ) %>% 
  filter(str_detect(month, "19|20") & !str_detect(month, "(Jan|Feb) '19")) %>% 
  mutate(
    year = str_extract(month, "\\d+") %>% str_c("20", .),
    month = if_else(row_number() + 2 <= 12, row_number() + 2, row_number() - 10),
    month = make_date(year = year, month = month)
  ) 
```

    ## New names:
    ## * `` -> ...2

``` r
mean_pmi <-
  us_pmi %>% 
  summarize(mean = mean(pmi)) %>% 
  pull(mean)

us_pmi %>% 
  ggplot(aes(month, pmi)) +
  geom_rect(
    aes(
      xmin = ymd("2020-02-29"), 
      xmax = ymd("2020-04-01"), 
      ymin = -Inf, 
      ymax = Inf
    ),
    fill = deteriorating,
    alpha = 0.2
  ) +
  geom_hline(yintercept = mean_pmi, color = "red") +
  geom_hline(yintercept = 50, color = "blue") +
  geom_line() +
  geom_point() +
  geom_text(
    aes(label = pmi), 
    data = us_pmi %>% filter(pmi > 51 | pmi < 40),
    hjust = 0,
    vjust = 0
  ) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels = "%b-%y", 
    minor_breaks = NULL
  ) +
  scale_y_continuous(breaks = scales::breaks_width(2), minor_breaks = NULL) +
  annotate(
    "text",
    x = ymd("2020-04-01"),
    y = c(mean_pmi, 50),
    hjust = c(0.7, 0.7),
    vjust = c(1.2, -0.2),
    label = c("Average PMI", "No change"),
    color = c("red", "blue")
  ) +
  annotate(
    "label",
    x = ymd("2020-02-29"),
    y = 36,
    hjust = 1,
    vjust = 0,
    label = "First state of emergency\ndeclared in Washington"
  ) +
  annotate(
    "segment",
    x = ymd("2020-02-27"),
    xend = ymd("2020-02-29"),
    y = 36,
    yend = -Inf,
    arrow = arrow(length = unit(0.08, "inches"))
  ) +
  theme_minimal() +
  labs(
    x = NULL,
    y = "Percentage Change in PMI (%)",
    title = "US' manufacturing Purchasing Managers' Index plummets after Feburary",
    subtitle = "50% means no change from last month",
    caption = "Source: ISM, IHS Markit"
  )
```

![](covid19-economic-impact_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
us_covid19 <-
  file_us_covid19 %>% 
  read_csv() %>% 
  select(-X1) %>% 
  pivot_longer(
    cols = "1/21/20":"3/31/20",
    names_to = "date",
    values_to = "cases"
  ) %>% 
  mutate(date = mdy(date)) 
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   X1 = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
us_covid19 %>% 
  ggplot(aes(date, cases)) +
  geom_rect(
    aes(
      xmin = ymd("2020-02-29"),
      xmax = ymd("2020-04-01"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = deteriorating,
    alpha = 0.1
  ) +
  geom_point() +
  geom_line() +
  scale_y_continuous(
    breaks = scales::breaks_width(1e5),
    labels = scales::label_number(scale = 1e-3, suffix = "k")
  ) +
  theme_minimal() +
    labs(
    x = NULL,
    y = "Percentage Change in PMI (%)",
    title = "Total cases in the United States",
    subtitle = "US cases is growing exponentially",
    caption = "Source: CDC"
  )
```

![](covid19-economic-impact_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->
