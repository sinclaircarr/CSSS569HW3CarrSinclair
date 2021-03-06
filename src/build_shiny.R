## clear environment
rm(list = ls())

## ===============================
## Get packages loaded
## ===============================

## load or install packages
if (!require(pacman)) install.packages('pacman')
library(pacman)
pacman::p_load(shiny, shinythemes, data.table, plotly, DT, dplyr, RColorBrewer, ggpubr)

## ===============================
## Read in data
## ===============================
# Make sure to open project csss_569_hw3.Rproj
# If repository from github was copied successfully go to File -> Open Project -> csss_569_hw3/csss_569_hw3.Rproj

# Helper datasets
locs <- readRDS("data/location_metadata.RDS")
causes <- readRDS("data/cause_metadata.RDS")
reis <- readRDS("data/rei_metadata.RDS")
ages <- readRDS("data/age_metadata.RDS")

# Add some variables
format_data <- function(df, merge_cause_name = F) {
  if (! "location_name" %in% names(df)) df <- merge(df, locs[,.(location_id, location_name)], by = "location_id")
  if (! "super_region_name" %in% names(df)) df <- merge(df, locs[,.(location_id, super_region_name, region_name)], by = "location_id")
  if (! "age_group_name" %in% names(df)) df <- merge(df, ages[,.(age_group_id, age_group_name)], by = "age_group_id")
  if (! "sex" %in% names(df)) df[, sex := ifelse(sex_id == 1, "Male", "Female")]
  if (merge_cause_name) {
    df <- merge(df, causes[,.(cause_id, cause_name)], by = "cause_id")
  }
  return(df)
}

# load exposure data
exposure <- readRDS("data/alcohol_exposure.RDS") %>% format_data(.)

# load relative risk (RR) data
rr <- readRDS("data/alcohol_rr.RDS")
rr <- rr[version == "GBD 2016", location_id := 1]
# rr <- format_data(rr)
rr <- merge(rr, causes[,.(cause_id, cause_name)], by = "cause_id")

# Set up args
id_cols <- c("location_id", "year_id", "sex_id", "age_group_id")
lvl3_locs <- unique(locs[level == 3, location_id])
super_regions <- unique(locs$super_region_name)
super_regions <- super_regions[2:8] # Drop empty

# Chris's golden scatterplot scheme
goldenScatterCAtheme <- theme(
  panel.background = element_rect(fill = "white"),
  aspect.ratio = ((1 + sqrt(5))/2)^(-1),
  axis.ticks.length = unit(0.5, "char"),
  axis.line.x.top = element_line(size = 0.2),
  axis.line.x.bottom = element_line(size = 0.2), 
  axis.ticks.x = element_line(size = 0.2), 
  axis.text.x = element_text(color = "black", size = 12),
  axis.title.x = element_text(size = 14, margin = margin(t = 7.5, r = 0, b = 0, l = 0)), 
  axis.ticks.y = element_blank(),
  axis.text.y = element_text(color = "black", size = 12, margin = margin(t = 0, r = -4, b = 0, l = 0)),
  axis.title.y = element_text(size = 14,margin = margin(t = 0, r = 7.5, b = 0)),
  legend.key = element_rect(fill = NA, color = NA, size = 1),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = "gray45", size = 0.2),
  strip.background = element_blank(),
  strip.text.x = element_text(size=12), 
  strip.text.y = element_blank(), 
  strip.placement = "outside", 
  panel.spacing.x = unit(1.25, "lines"), 
  panel.spacing.y = unit(1, "lines")
)

## ===============================
## Shiny App
## ===============================
# Define vector of options to toggle from
cause_opts <- causes[cause_id %in% unique(rr$cause_id)]$cause_name
# Define vector of age groups to toggle from
age_opts <- ages[age_group_id %in% unique(exposure$age_group_id)]$age_group_name
# Define male female vector
sex_opts <- c("Male", "Female")
# Define loc opts
loc_opts <- locs[location_id %in% unique(exposure$location_id)]$location_name
# Define main panel height
main_panel_height <- "700px" # "700px" good for displaying on laptop; "1000px" good for displaying on large monitor
# Define ymin and ymax limits
ymin_opts <- as.factor(c(1, 0, seq(100, 1000, by = 100)))
ymax_opts <- as.factor(c(5, 2, 3, 4, seq(10, 100, by = 10), seq(100, 2200, by = 100)))
# Create shiny ----
ui <- navbarPage(
  title = "Comparing GBD Alcohol estimations by cycle",
  theme = shinytheme("lumen"),
  tabsetPanel(tabPanel(title = "Exposure",
                       sidebarLayout(
                         sidebarPanel(
                           width = 3,
                           selectInput(
                             inputId = "sel_sex",
                             label = "Sex",
                             choices = sex_opts
                           ),
                           selectInput(
                             inputId = "sel_age",
                             label = "Age",
                             choices = age_opts
                           ),
                           selectInput(
                             inputId = "add_id_line",
                             label = "Add identity line",
                             choices = c(FALSE, TRUE)
                           )
                         ),
                         mainPanel(plotlyOutput("exp_scatter", height = main_panel_height))
                       )),
              tabPanel(title = "Relative risk",
                       sidebarLayout(
                         sidebarPanel(
                           width = 3,
                           selectInput(
                             inputId = "sel_cause_rr",
                             label = "Cause",
                             choices = cause_opts
                           ),
                           numericInput(
                             inputId = "num_ymin_rr",
                             label = "Y-Axis lower",
                             value = NA,
                             min = 0,
                             max = 1000
                           ),
                           numericInput(
                             inputId = "num_ymax_rr",
                             label = "Y-Axis upper",
                             value = NA,
                             min = 1,
                             max = 2200
                           )
                         ),
                         mainPanel(plotlyOutput("rel_risk", height = main_panel_height))
                       ))
  )
)


## ----------------
## define server
## ----------------
server <- function(input, output, session) {
  
  ## Define exposure plot output
  output$exp_scatter <- renderPlotly({
    # Define colors
    cols <- brewer.pal(9, "Set1") # Has yellow at index 6
    cols <- cols[c(1:5, 7:8)] # Remove yellow and add pink
    # Subset data and make plot
    p1 <- exposure[age_group_name == input$sel_age
                   & sex == input$sel_sex] %>%
      ggplot(aes(
        x = gbd_2019,
        y = gbd_2020,
        color = super_region_name,
        group = location_name
      )) +
      geom_point(size = rel(2), alpha = 0.45) +
      labs(
        x = paste0("GBD 2019 ", unique(exposure$label), " (g/day)"),
        y = paste0("GBD 2020 ", unique(exposure$label), " (g/day)"),
        color = "Super-region"
      ) +
      theme(
        legend.direction = "vertical",
        legend.text = element_text(size = rel(0.75)),
        legend.title = element_blank()
      ) +
      goldenScatterCAtheme +
      scale_color_manual(values = cols)
    if (input$add_id_line) p1 <- p1 + geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.75)
    return(ggplotly(p1, tooltip = c("x", "y", "group")) %>%
             config(displayModeBar = F)  %>%
             layout(
               legend = list(
                 orientation = "v",
                 x = 0.01,
                 y = 1,
                 title = NA,
                 bgcolor = "rgba(0,0,0,0)" # Transparent background
               )
             ))
    
  })
  
  # Define relative risk plot output
  output$rel_risk <- renderPlotly({
    # Define colors
    cols <- c("darkorange", "seagreen")
    # Subset data
    temp <- rr[cause_name == input$sel_cause_rr]
    setnames(temp, c("mean_val", "lo_val", "hi_val"), c("mean_effect_size", "lower_2.5_bound", "upper_97.5_bound"))
    p2 <- ggplot(data = temp, aes(x = exposure, y = mean_effect_size, ymin = lower_2.5_bound, ymax = upper_97.5_bound, color = version, fill = version)) +
      geom_line() +
      geom_ribbon(linetype = "", alpha = 0.4) +
      labs(x = "Exposure (g/day)", y = "Relative Risk") +
      geom_hline(yintercept = 1) +
      goldenScatterCAtheme + 
      theme(legend.title = element_text(size = rel(1)), legend.text = element_text(size = rel(1.2))) +
      scale_color_manual(values = c(cols[1], cols[2])) +
      scale_fill_manual(values = c(cols[1], cols[2])) +
      scale_y_continuous(limits = c(input$num_ymin_rr, input$num_ymax_rr))
    
    return(ggplotly(p2, tooltip = c("x", "y", "ymin", "ymax", "color", "text")) %>%
             config(displayModeBar = F)  %>%
             layout(legend = list(
               orientation = "v",
               x = 0.01,
               y = 0.95,
               title = NA,
               bgcolor = "rgba(0,0,0,0)"
             )))
    
  })
}

## ----------------
## run app
## ----------------
shinyApp(ui,server)

