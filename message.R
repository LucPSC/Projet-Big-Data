cryptos <- read.csv2("C:/Users/Eleve/Desktop/script03012022/crypto.csv")

cryptos <-transform(cryptos, quoteEURmarket_cap_dominance = as.numeric(quoteEURmarket_cap_dominance))

cryptosTop <- cryptos[cryptos$quoteEURmarket_cap_dominance > 3,]



# Telechargement du fichier donnees bicoin
data_bitcoin <- read.csv2("C:/Users/Eleve/Desktop/script03012022/Bitcoin2017-2022Journalie.csv");
Close_integer_bitcoin<-as.integer(data_bitcoin$Close);
date_convertit_bitcoin<-dmy(data_bitcoin$Date);
#selection des donnees

# Selection des donnees
data_bitcoin_moyenne_2020 = c()

for(m in seq(1,12)) {
  data_bitcoin_moyenne_2020 <-c(data_bitcoin_moyenne_2020, mean(Close_integer_bitcoin[year(date_convertit_bitcoin) == 2020 & month(date_convertit_bitcoin) == m]))
}

# Telechargement du fichier donnees Ether
data_ether <- read.csv2("C:/Users/Eleve/OneDrive/Bureau/programmes/Data_Science/Ethereum2017-2022Journalie.csv");
Close_integer_ether<-as.integer(data_ether$Close);
date_convertit_ether<-dmy(data_ether$Date);
#selection des donnees

# Selection des donnees
data_ether_moyenne_2020 = c()

for(m in seq(1,12)) {
  data_ether_moyenne_2020 <-c(data_ether_moyenne_2020, mean(Close_integer_ether[year(date_convertit_ether) == 2020 & month(date_convertit_ether) == m]))
}




header <- dashboardHeader(title = "Dasboard Invest",
                          dropdownMenu(type = "tasks", badgeStatus = "danger",
                                       taskItem(value = 60, color = "aqua",
                                                "Etude des principales cryptos"
                                       ),
                                       taskItem(value = 40, color = "green",
                                                "Evaluation du cours du bitcoin"
                                       ),
                                       taskItem(value = 40, color = "yellow",
                                                "Evaluation du cours de l'ether"
                                       )
                          )
)

sidebar <- dashboardSidebar(
  sidebarSearchForm(label = "Enter a number", "searchText", "searchButton"),
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    menuItem("Dashboard", tabName = "dashboard", icon = icon("th")),
    menuItem("Charts", icon = icon("chart-bar"),
             menuSubItem("Bitcoin", tabName = "Bitcoin"),
             menuSubItem("Ether", tabName = "Ether")
    ),
    menuItem("Prediction", tabName = "prediction", icon = icon("eye"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(
              box(status = "primary",
                  title = "Etude des principales cryptos",
                  background = "light-blue",
                  width=12,
                  plotOutput("plot1")
              )
            ),
            fluidRow(
              infoBox(width = 6, "Bitcoin Market Cap", cryptos$quoteEURmarket_cap[cryptos$name == "Bitcoin"], icon = icon("credit-card"), fill = TRUE),
              infoBox(width = 6, "Ether Market Cap", cryptos$quoteEURmarket_cap[cryptos$name == "Ethereum"], icon = icon("credit-card"), fill = TRUE)
            ),
            
            fluidRow(
              box(status = "primary",
                  title = "Portefeuille client",
                  background = "light-blue",
                  width=12,
                  fluidRow(
                    
                    
                    infoBox(width = 6, "Benefice Bitcoin", 100, icon = icon("credit-card"), fill = TRUE),
                    infoBox(width = 6, "Benefice Ethereum", 150, icon = icon("credit-card"), fill = TRUE)
                    
                    
                  ),
                  
              )
            ),
    ),           
    
    tabItem(tabName = "Bitcoin",
            fluidRow(
              box(
                status = "primary",
                title = "Evolution du bitcoin",
                width=12,
                plotOutput("plot2")
              ),
              
              box(
                title = "Annee",
                width=12,
                sliderInput("sliderBitcoin", "Number of observations:",min = 2017, max = 2022, value = 2020, step = 1)
              )
            )
    ),
    
    tabItem(tabName = "Ether",
            fluidRow(
              box(
                status = "primary",
                title = "Evolution du ether",
                width=12,
                plotOutput("plot3")
              ),
              
              box(
                title = "Annee",
                width=12,
                sliderInput("sliderEther", "Number of observations:", min = 2017, max = 2022, value = 2020, step = 1)
              )
            )
    ),
    tabItem(tabName = "prediction",
            box(
              status = "primary",
              title = "Evolution du ether",
              width=12,
              plotOutput("plot4")
            ),
            
            box(
              title = "Somme investie",
              width=6,
              sliderInput("investissement_euro", "investisement:", min = 1000, max = 5000, value = 3000, step = 500),
            ),
            
            box(
              title = "Date",
              width=6,
              sliderInput("nbr_annee", "nombre d'annees:", min = 1, max = 10, value = 1)
            )
            
    )
  )
)

shinyApp(
  ui = dashboardPage(header, sidebar, body),
  server = function(input, output) {
    output$plot1 <-  renderPlot({
      
      mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
      
      ggplot(cryptosTop, aes(x = "", y = quoteEURmarket_cap, fill = name)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+
        geom_text(aes(y = , label = name), color = "white")+
        scale_fill_manual(values = mycols) +
        theme_void()
      
      
      
    })
    
    output$plot2 <-  renderPlot({
      
      # Selection des donnees
      data_bitcoin_moyenne_2020 = c()
      
      for(m in seq(1,12)) {
        data_bitcoin_moyenne_2020 <-c(data_bitcoin_moyenne_2020, mean(Close_integer_bitcoin[year(date_convertit_bitcoin) == input$sliderBitcoin & month(date_convertit_bitcoin) == m]))
      }
      
      #Representation graphique
      barplot(data_bitcoin_moyenne_2020,
              col = "blue",
              main = "Evolution du bitcoin" ,
              xlab = "Mois",
              names.arg = c("01","02","03","04","05","06","07","08", "09","10","11","12"),
              ylab = "valeur moyenne ($)"
      )
    })
    
    output$plot3 <-  renderPlot({
      
      # Selection des donnees
      data_ether_moyenne_2020 = c()
      
      for(m in seq(1,12)) {
        data_ether_moyenne_2020 <-c(data_ether_moyenne_2020, mean(Close_integer_ether[year(date_convertit_ether) == input$sliderEther & month(date_convertit_ether) == m]))
      }
      
      #Representation graphique
      barplot(data_ether_moyenne_2020,
              col = "blue",
              main = "Evolution du bitcoin" ,
              xlab = "Mois",
              names.arg = c("01","02","03","04","05","06","07","08", "09","10","11","12"),
              ylab = "valeur moyenne ($)"
      )
    })
    output$plot4 <- renderPlot({
      mylist <- c()
      mylistInvest <- c()
      matrice <- c()
      coefficient <- 0.12
      argument <- 1
      arguments <- c()
      
      for(m in seq(1,input$nbr_annee)) {
        coefficient_multiplicateur <- 1 + coefficient
        benefice <- (input$investissement_euro * coefficient_multiplicateur) - input$investissement_euro
        beneficeInvest <- input$investissement_euro
        
        mylist <- c(mylist, benefice)
        mylistInvest <- c(mylistInvest, beneficeInvest)
        arguments <- c(arguments, argument)
        
        matrice <- rbind(mylistInvest,mylist)
        coefficient <- coefficient + 0.12
        argument <- argument + 1
      }
      barplot(matrice,
              col = c("blue","red"),
              main = "Evolution du bitcoin" ,
              xlab = "Années",
              names.arg = arguments,
              ylab = "Patrimoine en euros"
      )
      legend(x="bottomleft",legend=c("Bénéfice","Investissement"),fill=c("red","blue"))
      
    })
    
  }
)