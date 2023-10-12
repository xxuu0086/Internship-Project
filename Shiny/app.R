
# Load relevant libraries
library(shiny)
library(tidyverse)
library(haven)

dat <- haven::read_sav("../../Data and survey materials/ACSSM Final Data_27-Feb-2023 weighted.sav")

dat1 <- dat %>% 
  mutate(prob_based = ifelse(DataSource <= 5, "Probablity based", "Non-probablity based")) %>% # Indicate probability based
  filter(DataSource %in% c(2, 5, 6, 7, 8, 9)) # Filter data source

dat1 <- dat1 %>% 
  mutate(DataSource_name = case_when(
    DataSource == 1 ~ "VALI",
    DataSource == 2 ~ "Life in Australia",
    DataSource == 3 ~ "CATI high effort",
    DataSource == 4 ~ "CATI low effort",
    DataSource == 5 ~ "SMS push to web",
    DataSource == 6 ~ "Panel 1",
    DataSource == 7 ~ "Panel 2",
    DataSource == 8 ~ "Panel 3",
    DataSource == 9 ~ "Panel 4")) %>% 
  mutate(DataSource_name = factor(DataSource_name,
                                  levels = c("VALI", 
                                             "Life in Australia",
                                             "CATI high effort",
                                             "CATI low effort",
                                             "SMS push to web",
                                             "Panel 1",
                                             "Panel 2",
                                             "Panel 3",
                                             "Panel 4"),
                                  labels = c("VALI", 
                                             "Life in Australia",
                                             "CATI high effort",
                                             "CATI low effort",
                                             "SMS push to web",
                                             "Panel 1",
                                             "Panel 2",
                                             "Panel 3",
                                             "Panel 4")))


ui <- fluidPage(

    # Application title
    titlePanel("Page Time"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          radioButtons("bins", label = h6("Choose maximum page time limit"),
                       choices = list("Full Data" = 3000,
                                      "120 mins" = 120,
                                      "20 mins" = 20,
                                      "5 mins" = 5,
                                      "3 mins" = 3), 
                       selected = 3000),
          numericInput("num", label = h6("Cutoff Time, second per question"), value = 2),
          radioButtons("radio", label = h6(""),
                       choices = list("Violin plot" = "v",
                                      "Box plot" = "b"), 
                       selected = "v")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      
      switch1 <- switch (input$radio,
                         "v" = geom_violin(),
                         "b" = geom_boxplot()
      )
      
      # distribution of page time for section 2, 4-9
      dat1 %>%
        filter(Section_SUM_main < as.numeric(input$bins)) %>%
        ggplot(aes(x = DataSource_name,
                   y = Section_SUM_main,
                   color = prob_based)) +
        geom_point(size = 3) +
        switch1 +
        geom_hline(yintercept = input$num * 70 / 60, color = "purple", size = 1) +
        geom_text(aes(2,70 * input$num / 60, label = paste0("`Cutoff` Time:", round(input$num * 70 / 60, 2), "mins"), vjust = 2), show.legend = FALSE, color = "purple") +
        scale_color_brewer(palette = "Set1") +
        theme_bw() +
        theme(legend.position = "bottom") +
        labs(x = "",
             y = "Total time for sections in minutes",
             color = "",
             title = "Distributions of time answering the survey questions for panels ") 
      
    }, height = 800)
}

# Run the application 
shinyApp(ui = ui, server = server)
