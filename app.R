library(shiny)
library(shinydashboard)
library(rintrojs)
library(shinyBS)
library(rgl)
library(shinyWidgets)

library(dplyr)
library(e1071) # Pour le svm
library(DT)
library(plotly) # Graphiques interactifs
library(rAmCharts) # Graphiques interactifs
library(corrplot) # Pour matrice des corrélations
library(reshape2) # Pour la fonction melt()
library(randomForest) # Pour le Random Forest
library(unbalanced) # Pour le rééchantillonnage 


# Define UI for application that draws a histogram
ui <- shinyUI(
    dashboardPage(
        title = 'HRRC',
        header = dashboardHeader(
            titleWidth='100%',
            title = span(
                tags$img(src="bandeau3.png", width = '100%'), 
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
                menuItem("Comparaisons", tabName = "comparaisons", icon = icon("cog"))
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
                
                # Onglets Données
                tabItem(tabName = "param",
                        
                        fluidRow(
                            box(width = 12,
                                title = "Base de données",
                                DT::DTOutput("base")
                                )
                        ),
                        
                        fluidRow(
                                valueBoxOutput("valuebox_1", width = 3), 
                                valueBoxOutput("valuebox_2", width = 3),
                                valueBoxOutput("valuebox_3", width = 3), 
                                valueBox(
                                    "Class", "Variable Cible", icon = icon("bullseye"),
                                    color = "red",
                                    width = 3
                                ) 
                        ),
                        
                        fluidRow(
                            tabBox(width = 12,
                                title = "Exploration des données",
                                # The id lets us use input$tabset1 on the server to find the current tab
                                id = "tabset1", height = "250px",
                                tabPanel("Variables explicatives",
                                         uiOutput("uioutput_liste_variables"),
                                         fluidRow(
                                             box(width = 6,
                                                 title = "Histogramme",
                                                 plotOutput("graph_exploration_histogram")
                                             ),
                                             box(width = 6,
                                                 title = "Box Plot",
                                                 plotlyOutput("graph_exploration_boxplot")
                                             )
                                         )
                                ),
                                tabPanel("Variable Cible",
                                         fluidRow(
                                             box(width = 6,
                                                 title = "Bar Plot",
                                                 plotlyOutput("graph_exploration_barplot_cible")
                                             )
                                         )
                                ),
                                tabPanel("Corrélations",
                                         fluidRow(
                                             box(width = 6,
                                                 title = "Matrice des corrélations",
                                                 plotlyOutput("graph_exploration_corrplot", height = 480)
                                             ),
                                             box(width = 6,
                                                 title = "Matrice des corrélations 2 à 2",
                                                 fluidRow(
                                                     column(width = 6,
                                                            uiOutput("uioutput_liste_variables_2")
                                                     ),
                                                     column(width = 6,
                                                            uiOutput("uioutput_liste_variables_3")
                                                     )
                                                 ),
                                                 plotlyOutput("graph_exploration_corrplot_2")
                                             )
                                         )
                                )
                            ),
                        )
                 ),
                
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
                                sliderInput("cost", label = "Choix du paramètre", min = 0, max = 1, value = 1),
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
                ),
                
                # Onglet Comparaisons
                tabItem(tabName = "comparaisons",
                        fluidRow(
                            tabBox(width = 9,
                                   title = "Comparaisons des performances",
                                   tabPanel("Courbes ROC",
                                   ),
                                   tabPanel("Matrices de confusion",
                                   ),
                                   tabPanel("Résultats",
                                   )
                            ),
                            box(width = 3,
                                title = "Options",
                                uiOutput("uioutput_liste_variables_4"),
                                sliderInput("comp_option_train",label="Part de l'échantillon d'apprentissage", min = 0, max = 100, post  = " %", value = 50),
                                radioButtons("comp_option_reech","Méthode de rééchantillonage",c("Random Oversampling","Random Undersampling", "SMOTE"))
                            )
                        ),
                        
                        fluidRow(
                            box(width=3, solidHeader = TRUE, status = "primary",
                                title="Régression Logistique",
                                actionButton("action_logit","Lancer")
                            ),
                            box(width=3, solidHeader = TRUE, status = "success",
                                title="Arbre de décision",
                                actionButton("action_tree","Lancer")
                                
                            ),
                            box(width=3, solidHeader = TRUE, status = "warning",
                                title="Random Forest",
                                tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: orange}")),
                                tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: orange}")),
                                sliderInput("option_rf_1",label = "Nombre d'arbres", 1, 10000, 500),
                                sliderInput("option_rf_2",label = "Nombre de variables par arbre", 1, 29, 4),
                                actionButton("action_rf", label = "Lancer")
                                
                            ),
                            box(width=3, solidHeader = TRUE, status = "danger",
                                title="Gradient Boosting",
                                actionButton("action_gb","Lancer")
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
    
    # Importation des données
    data <- read.csv2("C:/Users/Utilisateur/Desktop/Appli_SVM/app/creditcard_extrait.csv", header = TRUE, stringsAsFactor = FALSE, sep = ";", dec = ".")
    attach(data)
    
    # Valeurs reactives
    values <- reactiveValues(plot_dimensions = NULL,
                             kernel_trick_index = 1)
    
    ## Fonctions
    # Determiner si un nombre est pair ou non
    is.even <- function(x){ x %% 2 == 0 }

## Onglet Présentation ########################################################################################################
    
    output$base <- DT::renderDataTable(data)
    
    ## Exploration des données
    
    # ValueBox nombre d'observations
    output$valuebox_1 <- renderValueBox({
        valueBox(
            nrow(data), "Nombre d'observations", icon = icon("lightbulb"),
            color = "purple"
        )
    })
    
    # ValueBox nombre de variables quantitatives
    output$valuebox_2 <- renderValueBox({
        nums <- unlist(lapply(data[,-ncol(data)], is.numeric))  
        valueBox(
            length(nums), "Nombre de variables quantitatives", icon = icon("search"),
            color = "orange"
        )
    })
    
    # ValueBox nombre de variables qualitatives
    output$valuebox_3 <- renderValueBox({

        nb_var <- ncol(data[,-ncol(data)])        
        nums <- unlist(lapply(data[,-ncol(data)], is.numeric))  
        
        valueBox(
            nb_var-length(nums), "Nombre de variables qualitatives", icon = icon("search"),
            color = "green"
        )
    })
    
    # Ui liste des variables
    output$uioutput_liste_variables <- renderUI ({
        selectInput("choix_variable",label = "Choisir une variable", choices = names(data), selected = "V1")
    })
    
    # Histograme
    output$graph_exploration_histogram <- renderPlot({
        x <- data[, input$choix_variable]
        
        bins <- seq(min(x), max(x), length.out = 100 + 1)
        
        hist(x, breaks = bins, col = "#FF851B", border = "white",
             xlab = "V1",
             main = paste0("Histogramme de la variable ", input$choix_variable))
    })
    
    # Box Plot (variables quantitatives)
    output$graph_exploration_boxplot <- renderPlotly({
        x <- data[, input$choix_variable]
        b<-boxplot(x)
        
        # find extremes from the boxplot's stats output
        lowerwhisker<-b$stats[1]
        upperwhisker<-b$stats[5]
        
        # remove the extremes
        testdata<-x[x>lowerwhisker & x<upperwhisker]
        plot_ly(x = testdata, type="box") %>%
            layout(title = paste0("Box Plot de la variable ", input$choix_variable))
    })
    
    # Bar Plot (variable cible)
    output$graph_exploration_barplot_cible <- renderPlotly({
        # Commpte le nombre de répétions par classe
        df <- data %>%
              group_by(Class) %>%
              summarise(counts = n())
        
        # Représentation graphique
        p <- ggplot(data=df, aes(x=Class, y=counts)) +
                geom_bar(stat="identity", color="black", fill="#DD4B39") +
                geom_text(aes(label=counts), color="black", vjust=-.2, size=5) +
                ggtitle("Répartition de la variable cible") +
                xlab("Valeur") + ylab("Nombre") +
                theme_minimal()
        
        p <- ggplotly(p)
    })
    
    # Matrice des corrélations
    output$graph_exploration_corrplot <- renderPlotly({
        cors <- cor(data[,])
        cor_data <- reshape2::melt(cors, value.name = "Correlation")
        
        p <- ggplot(cor_data, aes(Var1, Var2, fill=Correlation)) + 
                geom_tile(color = "white") + 
                scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 12, hjust = 1)) +
                coord_fixed()
            
        ggplotly(p)
    })
    
    # Matrice des corrélations 2 à 2
    output$uioutput_liste_variables_2 <- renderUI ({
        selectInput("choix_variable_1",label = "Choisir une variable", choices = names(data), selected = "V1")
    })
    
    output$uioutput_liste_variables_3 <- renderUI ({
        selectInput("choix_variable_2",label = "Choisir une variable", choices = names(data), selected = "V2")
    })
    
    output$graph_exploration_corrplot_2 <- renderPlotly({
            cors <- cor(data[,c(input$choix_variable_1, input$choix_variable_2)])
            cor_data <- reshape2::melt(cors, value.name = "Correlation")
            
            p <- ggplot(cor_data, aes(Var1, Var2, fill=Correlation)) + 
                    geom_tile(color = "white") + 
                    scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 12, hjust = 1)) +
                    coord_fixed()
            
            ggplotly(p)
    })
    
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
    
    
    ## Cas presque linéairement séparable
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
        svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = input$cost, scale = FALSE)
        
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
        

        if (input$choix == "2d") {
            
            output$plot_kernel_trick <- renderPlotly({
                plot_ly(df[1:2], x=df$x, y=df$y, type = "scatter",  color = ~as.factor(a), colors = c("red","blue"),  mode = "markers")
            })
            
            p <- plotlyOutput("plot_kernel_trick")
            
        } else {
            
            output$plot_kernel_trick <- renderPlotly({
                plot_ly(df, x=df$x, y=df$y, z=df$z, type = "scatter3d",  color = ~as.factor(a), colors = c("red","blue"),  mode = "markers")
            })
            
            p <- plotlyOutput("plot_kernel_trick")
        }
        
        
        test <- p
        
        return(p)
    })
    
    # Output du Graphique sur le Kernel Trick
    # output$plot_kernel_trick <- renderRglwidget(plot_kernel_trick())
    
    
    
    
    # Graphique à 2 dimensions
    # Graphique à 3 dimensions
    
## Onglet Comparaisons ########################################################################################################
    
    # Picker Input
    output$uioutput_liste_variables_4 <- renderUI({
        pickerInput(
            inputId = "comp_choix_variables",
            label = "Choix des variables",
            choices = names(data),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3, ", inline = TRUE),
            multiple = TRUE,
            selected = names(data)
        )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
