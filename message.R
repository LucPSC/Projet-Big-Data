cryptos <- read.csv2("C:/script03012022/crypto.csv")

cryptos <-transform(cryptos, quoteEURmarket_cap_dominance = as.numeric(quoteEURmarket_cap_dominance))

cryptosTop <- cryptos[cryptos$quoteEURmarket_cap_dominance > 3,]



# Telechargement du fichier donnees bicoin
data_bitcoin <- read.csv2("C:/script03012022/Bitcoin2017-2022Journalie.csv");
Close_integer_bitcoin<-as.integer(data_bitcoin$Close);
date_convertit_bitcoin<-dmy(data_bitcoin$Date);
#selection des donnees

# Selection des donnees
data_bitcoin_moyenne_2020 = c()

for(m in seq(1,12)) {
  data_bitcoin_moyenne_2020 <-c(data_bitcoin_moyenne_2020, mean(Close_integer_bitcoin[year(date_convertit_bitcoin) == 2020 & month(date_convertit_bitcoin) == m]))
}

# Telechargement du fichier donnees Ether
data_ether <- read.csv2("C:/script03012022/Ethereum2017-2022Journalie.csv");
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
    menuItem("Prediction Bitcoin", tabName = "predictionBitcoin", icon = icon("eye")),
    menuItem("Prediction Ether", tabName = "predictionEther", icon = icon("eye"))
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
    tabItem(tabName = "predictionBitcoin",
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
            
    ),
    tabItem(tabName = "predictionEther",
            box(
              status = "primary",
              title = "Evolution du ether",
              width=12,
              plotOutput("plot5")
            ),
            
            box(
              title = "Somme investie",
              width=6,
              sliderInput("investissement_euro_ether", "investisement:", min = 1000, max = 5000, value = 3000, step = 500),
            ),
            
            box(
              title = "Date",
              width=6,
              sliderInput("nbr_annee_ether", "nombre d'annees:", min = 1, max = 10, value = 1)
            )
            
    )
  )
)

shinyApp(
  ui = dashboardPage(header, sidebar, body),
  server = function(input, output) {
    output$plot1 <-  renderPlot({
      
      mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
      nomCrypto <- c("Bitcoin", "BNB", "Ethereum", "Tether")
      
      ggtitle("label") # pour le titre principal
      ggplot(cryptosTop, aes(x = "", y = quoteEURmarket_cap, fill = nomCrypto)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+
        geom_text(aes(y = , label = ""), color = "white")+
        scale_fill_manual(values = mycols) +
        theme_void()
      
      
      
    })
    
    output$plot2 <-  renderPlot({
      
      # Selection des donnees
      data_bitcoin_moyenne_2020 = c()
      
      for(m in seq(1,12)) {
        data_bitcoin_moyenne_2020 <-c(data_bitcoin_moyenne_2020, round(mean(Close_integer_bitcoin[year(date_convertit_bitcoin) == input$sliderBitcoin & month(date_convertit_bitcoin) == m])))
      }
      
      #Representation graphique
      barres2 <- barplot(data_bitcoin_moyenne_2020,
                         col = "blue",
                         main = "Evolution du bitcoin" ,
                         xlab = "Mois",
                         names.arg = c("01","02","03","04","05","06","07","08", "09","10","11","12"),
                         ylab = "valeur moyenne ($)"
      )
      for(i in seq(1,12)) {
        text(barres2[i], data_bitcoin_moyenne_2020[i]-(data_bitcoin_moyenne_2020[i] * 0.1), col='white', data_bitcoin_moyenne_2020[i])
      }
      
      # tableau <- c()
      # #Conversion en DataFrame
      # for(i in seq(1,12)) {
      #   tableau <- rbind(tableau, data.frame(x = c("01","02","03","04","05","06","07","08", "09","10","11","12"), y = data_bitcoin_moyenne_2020))
      # }
      # 
      # # Créer le barplot
      # ggplot(data=tableau, aes(x='mois', y=tableau$y)) +
      #   geom_bar(stat="identity", fill = "#FF6666")+
      #   geom_text(aes(y=tableau$y, label='valeur moyenne ($)'), vjust=1.6, 
      #             color="white", position = position_dodge(0.9), size=3.5)+ theme_minimal()
    })
    
    output$plot3 <-  renderPlot({
      
      # Selection des donnees
      data_ether_moyenne_2020 = c()
      
      for(m in seq(1,12)) {
        data_ether_moyenne_2020 <-c(data_ether_moyenne_2020, round(mean(Close_integer_ether[year(date_convertit_ether) == input$sliderEther & month(date_convertit_ether) == m])))
      }
      
      # #Representation graphique
      barres3 <- barplot(data_ether_moyenne_2020,
                         col = "blue",
                         main = "Evolution du Ether" ,
                         xlab = "Mois",
                         names.arg = c("01","02","03","04","05","06","07","08", "09","10","11","12"),
                         ylab = "valeur moyenne ($)"
      )
      for(i in seq(1,12)) {
        text(barres3[i], data_ether_moyenne_2020[i]-(data_ether_moyenne_2020[i] * 0.1), col='white', data_ether_moyenne_2020[i])
      }
      
      
      
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
        benefice <- round((input$investissement_euro * coefficient_multiplicateur) - input$investissement_euro)
        beneficeInvest <- input$investissement_euro
        
        mylist <- c(mylist, benefice)
        mylistInvest <- c(mylistInvest, beneficeInvest)
        arguments <- c(arguments, argument)
        
        matrice <- rbind(mylistInvest,mylist)
        coefficient <- coefficient + 0.12
        argument <- argument + 1
        
      }
      barres <- barplot(matrice,
                        col = c("#78281f","#0e6251"),
                        main = "Evolution du bitcoin" ,
                        xlab = "Années",
                        names.arg = arguments,
                        ylab = "Patrimoine en euros"
      )
      legend(x="topleft",legend=c("Bénéfice","Investissement"),fill=c("#0e6251","#78281f"))
      for(ii in seq(1,input$nbr_annee)) {
        text(barres[ii], mylistInvest[ii]/2, col='white', mylistInvest[ii])
        text(barres[ii], (mylist[ii]/2)+mylistInvest[ii], col='white', mylist[ii])
      }
      
    })
    output$plot5 <- renderPlot({
      mylist5 <- c()
      mylistInvest5 <- c()
      matrice5 <- c()
      coefficient5 <- 1.91
      argument5 <- 1
      arguments5 <- c()
      #initialisation DataFrame
      tableau <- data.frame(x = c(), y = c()) 
      
      for(m5 in seq(1,input$nbr_annee_ether)) {
        coefficient_multiplicateur5 <- 1 + coefficient5
        benefice5 <- round((input$investissement_euro_ether * coefficient_multiplicateur5) - input$investissement_euro_ether)
        beneficeInvest5 <- input$investissement_euro_ether
        
        mylist5 <- c(mylist5, benefice5)
        mylistInvest5 <- c(mylistInvest5, beneficeInvest5)
        arguments5 <- c(arguments5, argument5)
        
        matrice5 <- rbind(mylistInvest5,mylist5)
        coefficient5 <- coefficient5 + 1.91
        argument5 <- argument5 + 1
        
      }
      barres5 <- barplot(matrice5,
                         col = c("#78281f","#0e6251"),
                         main = "Evolution du Ether" ,
                         xlab = "Années",
                         names.arg = arguments5,
                         ylab = "Patrimoine en euros",
      )
      legend(x="topleft",legend=c("Bénéfice","Investissement"),fill=c("#0e6251","#78281f"))
      for(i in seq(1,input$nbr_annee_ether)) {
        text(barres5[i], mylistInvest5[i]/2, col='white', mylistInvest5[i])
        text(barres5[i], (mylist5[i]/2)+mylistInvest5[i], col='white', mylist5[i])
        tableau <- rbind(tableau, data.frame(x = mylistInvest5[i], y = mylist5[i]))
      }
      # 
      # print(tableau)
      #print(mylistInvest5)
      # #Conversion en DataFrame
      # for(i in seq(1,input$nbr_annee_ether)) {
      #   tableau <- rbind(tableau, data.frame(x = 'Investissement', y = mylistInvest5[i], annee = i))
      # }
      # 
      # for(i in seq(1,input$nbr_annee_ether)) {
      #   tableau <- rbind(tableau, data.frame(x = 'Benefice', y = mylist5[i], annee = i))
      # }
      # print(tableau)
      # 
      # # Créer le barplot
      # ggplot(data=tableau, aes(x=tableau$annee, y=tableau$y, fill=tableau$x)) +
      #   geom_bar(stat="identity")+
      #   geom_text(aes(y=tableau$y, label=tableau$y), vjust=1.6, 
      #             color="white", size=3.5)+ scale_fill_brewer(palette="Paired")+ theme_minimal()
      
      # ggplot(data=tableau, aes(x=arguments5, y=tableau$x)) +
      #   geom_bar(stat="identity")+
      #   geom_text(aes(y=tableau$x, label=tableau$x), vjust=1.6, 
      #             color="white", size=3.5)+ theme_minimal()
      
      
    })
    
    
  }
)