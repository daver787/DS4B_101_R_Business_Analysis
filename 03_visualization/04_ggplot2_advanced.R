# DS4B 101-R: R FOR BUSINESS ANALYSIS ---- 
# ADVANCED BUSINESS PLOTS ----


library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 Lollipop Chart: Top N Customers ----
# - Great for showing order

# Question: How much purchasing power is in top 5 customers?
# Goal: Visualize top N customers in terms of Revenue, include cumulative percentage

n <- 12

# Data Manipulation
top_customers_tbl <- bike_orderlines_tbl %>%
    select(bikeshop_name, total_price) %>%
    mutate(bikeshop_name = as_factor(bikeshop_name) %>% fct_lump(n = n, w= total_price)) %>%
    group_by(bikeshop_name) %>%
    summarize(revenue = sum(total_price)) %>%
    ungroup() %>%
    mutate(bikeshop_name = bikeshop_name %>% fct_reorder(revenue)) %>%
    mutate(bikeshop_name = bikeshop_name %>% fct_relevel("Other", after = 0)) %>%
    arrange(desc(bikeshop_name)) %>%
    mutate(revenue_text = scales::dollar(revenue, scale = 1e-6, suffix = "M")) %>%
    mutate(cum_pct = cumsum(revenue)/sum(revenue)) %>%
    mutate(cum_pct_text = scales::percent(cum_pct)) %>%
    mutate(rank = row_number()) %>%
    mutate(rank = case_when(
        rank == max(rank) ~NA_integer_,
        TRUE ~ rank
    )) %>%
    mutate(label_text = str_glue("Rank: {rank}\nRev: {revenue_text}\n CumPct:{cum_pct_text}"))
    
top_customers_tbl
# Data Visualization
top_customers_tbl %>%
    ggplot(aes(x= revenue, y = bikeshop_name)) +
    #geometries
    geom_segment(aes(xend = 0 , yend = bikeshop_name),
                 color = palette_light()[1],
                 size  = 1 ) +
    geom_point(aes(size = revenue),
               color = palette_light()[1]) +
    geom_label(aes(label = label_text), 
               hjust= "inward",
               size = 3,
               color = palette_light()[1]) +
    scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
    labs(
        title    = str_glue("Top {n} Customers"),
        subtitle = str_glue("Start: {year(min(bike_orderlines_tbl$order_date))} \n End:  {year(max(bike_orderlines_tbl$order_date))}"),
        x        = "Revenue($M)",
        y        = "Customer",
        caption  =str_glue("Top 6 Customers contribute \n 51% of purchasing power") 
        
    ) +
    theme_tq() +
    theme(
        legend.position = "none",
        plot.title      = element_text(face = "bold"),
        plot.caption    = element_text(face ="bold.italic")
    )

    
# 2.0 Heatmaps ----
# - Great for showing details in 3 dimensions

# Question: Do specific customers have a purchasing prefernce?
# Goal: Visualize heatmap of proportion of sales by Secondary Product Category

# Data Manipulation
pct_sales_by_customer_tbl <- bike_orderlines_tbl %>%
    select(bikeshop_name, category_1, category_2, quantity) %>%
    group_by(bikeshop_name, category_1, category_2) %>%
    summarize(total_quantity = sum(quantity)) %>%
    ungroup() %>%
    group_by(bikeshop_name) %>%
    mutate(pct = total_quantity /sum(total_quantity)) %>%
    ungroup() %>%
    mutate(bikeshop_name = as.factor(bikeshop_name)%>% fct_rev()) %>%
    mutate(bikeshop_name_num = as.numeric(bikeshop_name))
    

# Data Visualization
pct_sales_by_customer_tbl %>%
    ggplot(aes(x = category_2, y = bikeshop_name)) +
    geom_tile(aes(fill = pct )) +
    geom_text(aes(label =scales::percent(pct, accuracy = 0.1)),
              size = 3 ) +
    facet_wrap(vars(category_1), scales = "free_x") +
    scale_fill_gradient(low = "white", high = palette_light()[1]) +
    labs(
        title   = "Heatmap of Purchasing Habits",
        x       = "Bike Type (Category 2)",
        y       = "Customer",
        caption = str_glue("Customers that prefer Road: Ann Arbor Speed, Austin Crusiers & Indianaplis Velocipedes \n Customers that prefer Mountain: Ithaca Mountain Climbers, Pittsburgh Mountain Machines & Tampa 29ers")
    )+
    theme_tq() +
    theme(
        axis.text.x     =element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.caption    = element_text(face = "bold.italic"),
        plot.title      = element_text(face ="bold.italic"))
    


