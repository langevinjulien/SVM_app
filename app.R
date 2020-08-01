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
                         menuSubItem("Régression Logistique", tabName = "logit"),
                         menuSubItem("Arbre de décision", tabName = "tree", icon=icon("tree")),
                         menuSubItem("Random Forest", tabName = "shift", icon=icon("cog", lib="glyphicon")),
                         menuSubItem("Gradient Boosting", tabName = "gboost", icon=icon("bootstrap")))
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
            ),
            
            tabItems(
                # Onglet Principe du SVM
                tabItem(tabName = "principe",
                        fluidRow(
                            box(width = 12, 
                                title = "Echantillon linéairement séparable",
                                plotOutput("plot_svm")
                            ) 
                        ),
                        
                        fluidRow(
                            box(title = "Echantillon presque linéairement séparable",
                                plotOutput("plot_svm_2")
                            )  
                        ),
                        
                        fluidRow(
                            box(width = 12,
                                title = "Echantillon non linéairement séparable",
                                uiOutput("plot_kt"),
                                actionButton("action_kernel_trick", "Changer la dimension"),
                                radioButtons("choix","Choisir dimension",choices=c("2d"="2d","3d"="3d"))
                            )  
                        )
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
## SetUp
    
    ## Valeurs reactives
    values <- reactiveValues(plot_dimensions = NULL,
                             kernel_trick_index = 1)
    
    ## Fonctions
    # Determiner si un nombre est pair ou non
    is.even <- function(x){ x %% 2 == 0 }

## Onglet Présentation ########################################################################################################
## Onglet Principe du SVM #####################################################################################################
    
    ## Cas linéairement séparable
    plot_svm <- reactive ({
        set.seed(585)
        n = 25
        a1 = rnorm(n)
        a2 = 1 - a1 + 2* runif(n)
        b1 = rnorm(n)
        b2 = -1 - b1 - 2*runif(n)
        x = rbind(matrix(cbind(a1,a2),,2),matrix(cbind(b1,b2),,2))
        y <- matrix(c(rep(1,n),rep(-1,n)))
        plot(x,col=ifelse(y>0,2,3),pch=".",cex=8)
        
        dat = data.frame(x, y = as.factor(y)) # Transforme les données en fataframe
        svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 50, scale = FALSE)
        # print(svmfit)
        # 
        # plot(svmfit, dat)
        
        ## Représentation graphique du SVM
        make.grid <- function(x, n=75) {
            grange = apply(x, 2, range) # récupère les plus petites et les plus grandes valeurs de toutes les variables
            x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
            x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
            expand.grid(X1=x1, X2=x2)
        }
        
        xgrid <- make.grid(x)
        xgrid[1:10,]
        
        ygrid = predict(svmfit, xgrid)
        # plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
        # points(x, col = y + 3, bg = y + 3, pch = 22)
        # points(x[svmfit$index,], pch = 1, cex = 4)
        
        beta = drop(t(svmfit$coefs)%*%x[svmfit$index,])
        beta0 = svmfit$rho
        
        plot(xgrid, col = c(rgb(red = 1, green = 0, blue = 0, alpha = 0.4),
                            rgb(red = 0, green = 0, blue = 1, alpha = 0.4))[as.numeric(ygrid)], pch = 20, cex = 0.01)
        points(x, col = y + 3, pch = 19, cex = 1)
        points(x[svmfit$index,], pch = 5, cex = 2)
        abline(beta0 / beta[2], -beta[1] / beta[2])
        abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
        abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty=2)
    })
    
    output$plot_svm <- renderPlot(plot_svm())
    
    
    ## Cas non linéairement séparable
    plot_svm_2 <- reactive({
        set.seed(585)
        n = 25
        a1 = rnorm(n)
        a2 = 1 - a1 + 2* runif(n)
        b1 = rnorm(n)
        b2 = -1 - b1 - 2*runif(n)
        x = rbind(matrix(cbind(a1,a2),,2),matrix(cbind(b1,b2),,2))
        y <- matrix(c(rep(1,n),rep(-1,n)))
        y[32] <- 1
        plot(x,col=ifelse(y>0,2,3),pch=".",cex=8)
        
        dat = data.frame(x, y = as.factor(y)) # Transforme les données en fataframe
        svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 50, scale = FALSE)
        
        ## Représentation graphique du SVM
        make.grid <- function(x, n=75) {
            grange = apply(x, 2, range) # récupère les plus petites et les plus grandes valeurs de toutes les variables
            x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
            x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
            expand.grid(X1=x1, X2=x2)
        }
        
        xgrid <- make.grid(x)
        xgrid[1:10,]
        
        ygrid = predict(svmfit, xgrid)
        
        beta = drop(t(svmfit$coefs)%*%x[svmfit$index,])
        beta0 = svmfit$rho
        
        plot(xgrid, col = c(rgb(red = 1, green = 0, blue = 0, alpha = 0.4),
                            rgb(red = 0, green = 0, blue = 1, alpha = 0.4))[as.numeric(ygrid)], pch = 20, cex = 0.2)
        points(x, col = y + 3, pch = 19)
        points(x[svmfit$index,], pch = 5, cex = 2)
        abline(beta0 / beta[2], -beta[1] / beta[2])
        abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
        abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty=2)
    })
    
    
    output$plot_svm_2 <- renderPlot(plot_svm_2())
    
    ## Kernel Trick
    
    # Dimension du graph
    observeEvent(input$action_kernel_trick, {

        values$kernel_trick_index <- values$kernel_trick_index +1
        
        if (is.even(values$kernel_trick_index)) {
            values$plot_dimensions <- "2d"
        } else {
            values$plot_dimensions <- "3d"
        }
    })
    
    # Graphique en 2d et 3d
    output$plot_kt <- renderUI ({
        
        theta <- sample(1:360, 70, replace=TRUE)
        
        # Points bleus
        x1 <- sample(8:10, 70, replace=TRUE)*cos(theta)
        x2 <- sample(8:10, 70, replace=TRUE)*sin(theta)
        x3 <- runif(70, min = 2, max = 4)
        
        # Points rouges
        x11 <- sample(1:3, 70, replace=TRUE)*cos(theta)
        x22 <- sample(1:3, 70, replace=TRUE)*sin(theta)
        x33 <- runif(70, min = 6, max = 8)
        
        df <- data.frame(x=c(x1,x11), y=c(x2,x22), z=c(x3,x33))
        
        a=ifelse(abs(df$x)>5 | abs(df$y)>5,1,-1)
        a
        

        if (input$choix == "3d") {
            
            output$plot_kernel_trick <- renderPlot({
                try(rgl.close())
                plot(df[,1:2], col = a + 3, pch = 19)
            })
            
            p <- plotOutput("plot_kernel_trick")
           
        } else {
            
            output$plot_kernel_trick <- renderRglwidget({
                plot3d(df$x, df$y, df$z, col = a + 3, size = 2, type='s')
                rglwidget()
            })
            
            p <- rglwidgetOutput("plot_kernel_trick")
        }
        
        browser()
        test <- p
        
        return(p)
    })
    
    # Output du Graphique sur le Kernel Trick
    # output$plot_kernel_trick <- renderRglwidget(plot_kernel_trick())
    
    
    
    
    # Graphique à 2 dimensions
    # Graphique à 3 dimensions
    
## Onglet Comparaisons ########################################################################################################
}

# Run the application 
shinyApp(ui = ui, server = server)
