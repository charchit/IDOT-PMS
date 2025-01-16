# app.R

# Load required libraries
library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(VIM)
library(gridExtra)

# Define UI
ui <- navbarPage("Data Exploration Dashboard",
                 
  # Tab 1: Data Summary
  tabPanel("Data Summary",
    fluidRow(
      column(6,
             h4("Summary of Numerical Variables"),
             verbatimTextOutput("numeric_summary")
      ),
      column(6,
             h4("Summary of Categorical Variables"),
             verbatimTextOutput("categorical_summary")
      )
    )
  ),
  
  # Tab 2: IRI Distribution
  tabPanel("IRI Distribution",
    fluidRow(
      column(12,
             h4("Distribution of IRI"),
             plotOutput("iri_dist_plot")
      )
    )
  ),
  
  # Tab 3: Numerical Histograms
  tabPanel("Numerical Histograms",
    fluidRow(
      column(12,
             h4("Histograms of Numerical Variables"),
             plotOutput("numerical_histograms", height = "800px")
      )
    )
  ),
  
  # Tab 4: Numerical Boxplots
  tabPanel("Numerical Boxplots",
    fluidRow(
      column(12,
             h4("Boxplots of Numerical Variables"),
             plotOutput("numerical_boxplots", height = "800px")
      )
    )
  ),
  
  # Tab 5: Categorical Bar Plots
  tabPanel("Categorical Bar Plots",
    fluidRow(
      column(12,
             h4("Bar Plots of Categorical Variables"),
             plotOutput("categorical_barplots", height = "800px")
      )
    )
  ),
  
  # Tab 6: Missing Data
  tabPanel("Missing Data",
    fluidRow(
      column(12,
             h4("Missing Data Percentage by Feature"),
             plotOutput("missing_data_plot")
      )
    )
  ),
  
  # Tab 7: Correlation Matrix
  tabPanel("Correlation Matrix",
    fluidRow(
      column(12,
             h4("Correlation Matrix (Numeric Variables)"),
             plotOutput("correlation_heatmap")
      )
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  
  # Load, clean, and prepare the data as a reactive expression
  data_all3 <- reactive({
    # Set file path (update if needed)
    file <- "S5140PMIS_MODEL_DATA_2023.xlsx"
    
    # Read the Excel file
    original_data2 <- readxl::read_excel(file, sheet = "Sheet1")
    
    # Create a duplicate to manipulate
    data_all2 <- original_data2
    
    # Convert specific PAVTYP values to a single factor
    data_all2 <- data_all2 %>%
      dplyr::mutate(PAVTYP = dplyr::case_when(
        PAVTYP %in% c("1", "1A") ~ "1",
        PAVTYP %in% c("2A", "2B") ~ "2",
        PAVTYP %in% c("3A", "3B") ~ "3",
        TRUE ~ PAVTYP
      ))
    
    # Ensure there are no overlapping conditions in FHWA_Condition
    data_all2 <- data_all2 %>%
      dplyr::mutate(FHWA_Condition = dplyr::case_when(
        IRI < 95 & PAVTYP %in% c("3", "4") & RUT < 0.2 ~ "Good",
        IRI < 95 & !PAVTYP %in% c("3", "4") ~ "Good",
        (IRI >= 95 & IRI < 170) & PAVTYP %in% c("3", "4") ~ "Fair",
        (IRI >= 95 & IRI < 170) & !PAVTYP %in% c("3", "4") ~ "Fair",
        (IRI >= 170 | (PAVTYP %in% c("3", "4"))) ~ "Poor",
        (IRI >= 170) & !PAVTYP %in% c("3", "4") ~ "Poor",
        TRUE ~ NA_character_
      ))
    
    # Convert columns to appropriate types
    data_all2 <- data_all2 %>%
      dplyr::mutate(
        ORIGKEY = as.character(ORIGKEY),
        SHEETNAME = as.factor(SHEETNAME),
        SYSTEM = as.factor(SYSTEM),
        PAVTYP = as.factor(PAVTYP),
        CITY = as.factor(CITY),
        NHS = as.factor(NHS),
        LAYER_TYP = as.factor(LAYER_TYP),
        CIP_COUNT = as.factor(CIP_COUNT),
        CRK_SEAT_COUNT = as.factor(CRK_SEAT_COUNT),
        PAVE_CODE = as.factor(PAVE_CODE)
      ) %>%
      dplyr::mutate(across(-c(SHEETNAME, ORIGKEY, SYSTEM, PAVTYP, CITY, NHS,
                        LAYER_TYP, CIP_COUNT, CRK_SEAT_COUNT, PAVE_CODE,
                        FHWA_Condition), as.numeric))
    
    # Replace "N/A" strings with NA values
    data_all2[data_all2 == "N/A"] <- NA
    
    # Select relevant columns for analysis
    data_all3 <- data_all2 %>%
      dplyr::select(ORIGKEY, PAVTYP, IRI, RUT, PMIS_LENGTH, SYSTEM, CITY, NHS, PMISYR,
             CONYR, RESYR, AGE, PAVE_CODE, FHWA_Condition, ACRACK, TCRACK,
             LCRACK, LCRACKW, LCRACKPCC, COVERAGE, COVERAGE_LENGTH, COVERAGE_BEGIN,
             COVERAGE_END, CRACK_RATIO, FAULTAV, LAYR1)
    
    return(data_all3)
  })
  
  #### Output: Data Summaries ####
  
  # Summary of numerical variables
  output$numeric_summary <- renderPrint({
    ds <- data_all3()
    ds %>% 
      dplyr::select(where(is.numeric)) %>% 
      summary()
  })
  
  # Summary of categorical variables
  output$categorical_summary <- renderPrint({
    ds <- data_all3()
    ds %>% 
      dplyr::select(where(is.factor)) %>% 
      summary()
  })
  
  #### Output: Plots ####
  
  # Plot: Distribution of IRI
  output$iri_dist_plot <- renderPlot({
    ds <- data_all3()
    ggplot(ds, aes(x = IRI)) +
      geom_histogram(binwidth = 10, fill = "blue", color = "black") +
      labs(title = "Distribution of IRI", x = "IRI", y = "Frequency") +
      theme_minimal()
  })
  
  # Plot: Histograms of all numerical variables (faceted)
  output$numerical_histograms <- renderPlot({
    ds <- data_all3()
    numeric_vars_long <- ds %>%
      dplyr::select(where(is.numeric)) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
    ggplot(numeric_vars_long, aes(x = Value)) +
      geom_histogram(fill = "skyblue", color = "black", bins = 30) +
      facet_wrap(~ Variable, scales = "free", ncol = 4) +
      theme_minimal() +
      labs(title = "Histograms of Numerical Variables")
  })
  
  # Plot: Boxplots of numerical variables (faceted)
  output$numerical_boxplots <- renderPlot({
    ds <- data_all3()
    numeric_vars_long <- ds %>%
      dplyr::select(where(is.numeric)) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
    ggplot(numeric_vars_long, aes(y = Value)) +
      geom_boxplot(fill = "lightgreen") +
      facet_wrap(~ Variable, scales = "free", ncol = 4) +
      theme_minimal() +
      labs(title = "Boxplots of Numerical Variables")
  })
  
  # Plot: Bar plots for categorical variables (faceted)
  output$categorical_barplots <- renderPlot({
    ds <- data_all3()
    categorical_vars_long <- ds %>%
      dplyr::select(where(is.factor)) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "Variable", values_to = "Category")
    ggplot(categorical_vars_long, aes(x = Category)) +
      geom_bar(fill = "coral", color = "black") +
      facet_wrap(~ Variable, scales = "free", ncol = 4) +
      theme_minimal() +
      labs(title = "Bar Plots of Categorical Variables")
  })
  
  # Plot: Missing data percentage per feature
  output$missing_data_plot <- renderPlot({
    ds <- data_all3()
    missing_data <- ds %>%
      dplyr::summarise(across(everything(), ~ sum(is.na(.)) / n())) %>%
      tidyr::pivot_longer(everything(), names_to = "variable", values_to = "missing_fraction")
    ggplot(missing_data, aes(x = reorder(variable, -missing_fraction), y = missing_fraction)) +
      geom_bar(stat = "identity", fill = "tomato") +
      coord_flip() +
      labs(title = "Missing Data Percentage by Feature", x = "Feature", y = "Missing Fraction") +
      theme_minimal()
  })
  
  # Plot: Correlation Matrix Heatmap for numerical features (excluding year columns)
  output$correlation_heatmap <- renderPlot({
    ds <- data_all3()
    numeric_vars <- ds %>%
      dplyr::select(where(is.numeric)) %>%
      dplyr::select(-PMISYR, -CONYR, -RESYR)
    cor_matrix <- cor(numeric_vars, use = "complete.obs")
    corrplot(cor_matrix, method = "circle", tl.cex = 0.7, number.cex = 0.7, order = "hclust")
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
