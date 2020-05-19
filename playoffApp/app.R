#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

PLL = read.csv("~/NBA/alltimeNBA/results/PLL.csv")
'%!in%' <- function(x,y)!('%in%'(x,y))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Playoff Legacy"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("selectedPlayer",
                        "Player Name:",
                        PLL$Player)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    PLL <- reactive({
        input$selectedPlayer
    })

    output$distPlot <- renderPlot({
        playername = PLL()
        PLL %>% filter(Player == playername) %>% select(Player, Yr, PC1n)
        for (i in min(data$Yr):max(data$Yr)){
            if (i %!in% data$Yr){
                newline = cbind.data.frame(Player = data$Player[1], Yr = i, PC1n = 0)
                data = rbind.data.frame(data, newline)
            }
        }
        data %>% filter(Player == selectedPlayer) %>% ggplot(aes(x = Yr, y = PC1n)) + geom_point() +
            geom_line() + scale_x_continuous("", breaks = seq(min(data$Yr), max(data$Yr), by = 2)) + 
            scale_y_continuous("Playoff Run Score", limits = c(0, 1)) + theme_clean()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
