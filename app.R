# Load necessary libraries
library(shiny)
library(tidyverse)
library(gt)

# UI for Shiny App
ui <- fluidPage(
  titlePanel("Brain Expression Visualization from Human Protein Atlas"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("genes", 
                "Enter up to 5 gene symbols (comma separated):", 
                value = ""),
      actionButton("submit", "Plot Data")
    ),
    
    mainPanel(
      plotOutput("expressionPlot"),
      gt_output("expressionTable")
    )
  )
)

# Server logic for Shiny App
server <- function(input, output) {
  # Read data
  df <- read_tsv("rna_pfc_brain_hpa.tsv.zip",
                 skip=1,
                 col_names = c("Gene", "Symbol", "Subregion", "TPM", "pTPM", "nTPM"))
  
  # Reactive expression to filter data based on input
  filtered_data <- eventReactive(input$submit, {
    gene_list <- str_split(input$genes, ",") %>% unlist() %>% str_trim()
    df %>% filter(Symbol %in% gene_list[1:min(length(gene_list), 5)])
  })
  
  # Output for the plot
  output$expressionPlot <- renderPlot({
    df1 <- filtered_data()
    
    ggplot(df1, aes(x=Subregion, y=nTPM, fill=Symbol)) +
      geom_col(position="dodge") +
      labs(title="Brain expression",
           subtitle="source: Human Protein Atlas",
           y= "normalized TPM") +
      scale_fill_manual(values=c("tomato", "steelblue", "gold", "seagreen", "purple")) +
      theme_bw() +
      theme(aspect.ratio = 1/2,
            axis.text.x = element_text(size=10, angle=45, hjust=1),
            axis.title.x = element_blank(),
            title = element_text(face="bold"))
  })
  
  # Output for the table
  output$expressionTable <- render_gt({
    df2 <- filtered_data()
    
    df2 %>% gt()
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
