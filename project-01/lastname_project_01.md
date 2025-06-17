---
title: "Mini-Project 01"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
---

# Data Visualization Project 01
Loading the Tidyverse for all the packages I will need...

``` r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
## ✔ purrr     1.0.2     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(viridis)
```

```
## Warning: package 'viridis' was built under R version 4.4.3
```

```
## Loading required package: viridisLite
```
Then loading readr loading the data.

``` r
library(readr)
fuel <- read_csv("data/fuel.csv")
```

```
## Rows: 38113 Columns: 81
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (11): make, model, class, drive, transmission, transmission_type, engine...
## dbl (59): vehicle_id, year, engine_index, engine_cylinders, engine_displacem...
## lgl (11): turbocharger, supercharger, fuel_type_2, gas_guzzler_tax, start_st...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```
After that creating a new column called Aspiration marking labeling cars that are turbocharged and supercharged as well as the naturally aspirated ones.

``` r
fuel <- fuel %>%
  mutate(
    Aspiration = case_when(
      turbocharger == TRUE ~ "Turbocharged",
      supercharger == TRUE ~ "Supercharged",
      TRUE ~ "Naturally Aspirated"
    )
  )
```

Then group by fuel type to see what cars need to be excluded due to unconventional fuel technology.

``` r
fuel %>% 
  group_by(fuel_type) %>% 
  summarise(n())
```

```
## # A tibble: 14 × 2
##    fuel_type                   `n()`
##    <chr>                       <int>
##  1 CNG                            60
##  2 Diesel                       1014
##  3 Electricity                   133
##  4 Gasoline or E85              1223
##  5 Gasoline or natural gas        20
##  6 Gasoline or propane             8
##  7 Midgrade                       77
##  8 Premium                     10133
##  9 Premium Gas or Electricity     18
## 10 Premium and Electricity        25
## 11 Premium or E85                122
## 12 Regular                     25258
## 13 Regular Gas and Electricity    20
## 14 Regular Gas or Electricity      2
```

Ensuring those fuel types are filtered out that are not important.

``` r
fuel <- fuel %>% 
  filter(!fuel_type %in% c("CNG", "Diesel", "Electricity","Gasoline or propane"))
```
I want to see how engine size and displacement change over time and effect fuel economy as well as turbos. 
Step one was to look at engine displacement which is linked to engine cylinders due to physics. To see fuel economy but also filtering by aspiration type as turbos as supposed to allow for a smaller engine but allow for it to be more efficient.

``` r
ggplot(fuel,mapping = aes(engine_displacement,combined_mpg_ft1,colour = Aspiration))+geom_point()+ ylim(0,60)+geom_smooth(se=FALSE)+  scale_color_manual(values = c("Turbocharged" = "#E41A1C", "Naturally Aspirated" = "#377EB8")) 
```

```
## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
```

```
## Warning: Removed 2 rows containing non-finite outside the scale range
## (`stat_smooth()`).
```

```
## Warning: Removed 2 rows containing missing values or values outside the scale range
## (`geom_point()`).
```

![](lastname_project_01_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
Looking at th graph we can see that as the engine is bigger the lower the fuel economy. We can also see that turbos are helpful in certain applications but not all I am assuming this is due to the use of turbos trying to make a smaller engine simply more powerful but technically less efficient then the NA variant of the same size.

Next looking at displacement and cylinders by aspiration we can see that with smaller engines NA and turbo charged engines are the same but as engines get bigger turbo charged engines are smaller for the same number on cylinders which is to be expected as for the most effective cylinder it is almost perfect 500cc per cylinder which does line up. All the way until the big engines.

``` r
ggplot(fuel,mapping = aes(engine_displacement,engine_cylinders,colour = Aspiration))+geom_point()+geom_smooth(method="lm",se=FALSE)
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

```
## Warning: Removed 3 rows containing non-finite outside the scale range
## (`stat_smooth()`).
```

```
## Warning: Removed 3 rows containing missing values or values outside the scale range
## (`geom_point()`).
```

![](lastname_project_01_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
This next graph is showing us fuel economy overtime as well as engine displacement.Which shows us that in the beginning engines were just inefficient then after they made engines bigger which increased the drop in the early 2000s where after they made engines smaller which increased fuel economy.

``` r
fuel %>% 
  group_by(year) %>% 
  summarise(mean_mpg = mean(combined_mpg_ft1, na.rm = TRUE),mean_displacement = mean(engine_displacement, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_mpg,color=mean_displacement)) + geom_point()+geom_smooth(se=FALSE)+   scale_color_viridis_c(option = "plasma", end = 0.95)
```

```
## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

```
## Warning: The following aesthetics were dropped during statistical transformation:
## colour.
## ℹ This can happen when ggplot fails to infer the correct grouping structure in
##   the data.
## ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
##   variable into a factor?
```

![](lastname_project_01_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
Finally I just have a graph showing how engine displacement changed overtime with the average of cylinders as well further showing higher displacement does mean more cylinders as the trend holds throughout time. 

``` r
fuel %>% 
  group_by(year) %>% 
  summarise(mean_displacement = mean(engine_displacement, na.rm = TRUE),mean_cylinders = mean(engine_cylinders, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_displacement,colour = mean_cylinders)) + geom_point()+geom_smooth(se=FALSE)+ scale_color_viridis_c(option = "plasma", end = 0.95)
```

```
## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

```
## Warning: The following aesthetics were dropped during statistical transformation:
## colour.
## ℹ This can happen when ggplot fails to infer the correct grouping structure in
##   the data.
## ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
##   variable into a factor?
```

![](lastname_project_01_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
###What were the original charts you planned to create for this assignments?
I mentioned it at the start just a look into how displacement, cylinders, and aspiration all changed over time.
###What story could you tell with your plots?
The story I can tell is that at first we were not good at making engines and then we got better at making them which means smaller engines and better fuel economy but we can also see that turbos have allowed engines to be smaller not to mention that it can also show us times when gas prices are high as the auto makers will engineer cars that are better on gas. 
###How did you apply the principles of data visualizations and design for this assignment?
I put some colors in places that needed them and did some quick data wrangling. I also tried to use lines to show trends over time. But the main focus was to get as much as possible in as little visualizations as possible.

