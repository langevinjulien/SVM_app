library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- shinyUI(
    dashboardPage(
        title = 'HRRC',
        header = dashboardHeader(
            titleWidth='100%',
            title = span(
                tags$img(src="bandeau2.png", width = '100%'), 
                column(12, class="title-box", 
                       tags$h1(class="primary-title", style='margin-top:10px;', 'Support Vector Machine'), 
                       tags$h2(class="primary-subtitle", style='margin-top:10px;', "Un exemple d\'application"),
                       tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                )
            ),
            dropdownMenuOutput("helpMenu")       
        ),
        
        sidebar = dashboardSidebar(
            sidebarMenu(
                menuItem("Présentation", tabName = "presentation", icon = icon("download")),
                menuItem("Principe du SVM", tabName = "principe", icon = icon("cog")),
                menuItem("Exploration des données", tabName= "param", icon = icon("download")),
                menuItem("Comparaisons", tabName = "comparaisons", icon = icon("cog"),
                         menuSubItem("Régression Logistique", tabName = "pd_ttc"),
                         menuSubItem("Arbre de décision", tabName = "tree", icon=icon("tree")),
                         menuSubItem("Random Forest", tabName = "shift", icon=icon("cog", lib="glyphicon")),
                         menuSubItem("Gradient Boosting", tabName = "pd_ifrs9", icon=icon("bootstrap")))
            )
            
        ),
        
        body = dashboardBody(
            tags$style(type="text/css", "
/*    Move everything below the header */
    .content-wrapper {
        margin-top: 50px;
    }
    .content {
        padding-top: 60px;
    }
/*    Format the title/subtitle text */
    .title-box {
        position: absolute;
        text-align: center;
        top: 50%;
        left: 50%;
        transform:translate(-50%, -50%);
    }
    @media (max-width: 590px) {
        .title-box {
            position: absolute;
            text-align: center;
            top: 10%;
            left: 10%;
            transform:translate(-5%, -5%);
        }
    }
    @media (max-width: 767px) {
        .primary-title {
            font-size: 1.1em;
        }
        .primary-subtitle {
            font-size: 1em;
        }
    }
/*    Make the image taller */
    .main-header .logo {
        height: 125px;
    }
/*    Override the default media-specific settings */
    @media (max-width: 5000px) {
        .main-header {
            padding: 0 0;
            position: relative;
        }
        .main-header .logo,
        .main-header .navbar {
            width: 100%;
            float: none;
        }
        .main-header .navbar {
            margin: 0;
        }
        .main-header .navbar-custom-menu {
            float: right;
        }
    }
/*    Move the sidebar down */
    .main-sidebar {
        position: absolute;
    }
    .left-side, .main-sidebar {
        padding-top: 175px;
    }"
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
