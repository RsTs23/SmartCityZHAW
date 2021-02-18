library(shiny)
library(shinydashboard)

library(readr)
library(dplyr)
library(shinythemes)
library(leaflet)
library(shinyWidgets)
library(ggplot2)
#devtools::install_github("ricardo-bion/ggRadar", dependencies = TRUE)
library(ggradar)
library(tidyverse)
library(plotly)
#library(png)
#library(emojifont)
library(DT)
#load.fontawesome()

# Use purrr's split() and map() function to create the list
# needed to display the name of the airline but pass its
# Carrier code as the value



conditional <- function(condition, success) {
  if (condition) success else TRUE
}
options(warn=-1)

tags$style(type="text/css",
           ".shiny-output-error { visibility: hidden; }",
           ".shiny-output-error:before { visibility: hidden; }"
           
  
           
)




# function to close plotly radar chart
add_closed_trace <- function(p, r, theta, ...)
{
  plotly::add_trace(p, r = c(r, r[1]), theta = c(theta, theta[1]), ...)
}

# Daten einlesen und mergen ####
location <- read.delim(file="../../Data/geocodes.csv", sep = ",")
city <- read_csv2(file="../../Data/SSCS_2020_FuerNovalytica.csv")
population <- read_delim(file="../../Data//population.csv", delim = ",")
grossregion <- read.delim(file="../../Data/Raumgliederungen.csv", sep = ";")

city$VG[city$VG == "Städtische Gemeinde einer grossen Agglomeration (11)"] = "Grosser Ballungsraum"
city$VG[city$VG == "Städtische Gemeinde einer mittelgrossen Agglomeration (12)"] = "Mittlerer Ballungsraum"
city$VG[city$VG == "Städtische Gemeinde einer kleinen oder ausserhalb einer Agglomeration (13)"] = "Einzelstädte"
city$VG[city$VG == "Periurbane Gemeinde hoher Dichte (21) & Ländliche Zentrumsgemeinde (31)"] = "Ländliche Zentren"

population <- population %>%
  group_by(GMDNR) %>%
  slice(which.max(year))

cityPlots <- merge(x=city,y=population, by.x ="BFS_GdNr", by.y= "GMDNR", all.x= TRUE)

cityPlots <- merge(x=cityPlots,y=grossregion, by.x ="BFS_GdNr", by.y= "BFS_GdNr", all.x=TRUE)
cityPlots$groessenkat <- ifelse(cityPlots$value < 20000, 1, ifelse(cityPlots$value < 100000, 2, 3))

location <- subset(location, select = c(3:5))

population <- subset(population, select =  -c(2))

cityKarte <- filter(city, !(city$AuswForm %in%  "Die Angaben zu meiner Gemeinde/Stadt dürfen   nur in aggregierter und anonymisierter Form   publiziert werden, die keine Rückschlüsse auf meine Gemeinde/Stadt zulässt."))

cityKarte <- merge(x=cityKarte,y=population, by.x ="BFS_GdNr", by.y= "GMDNR", all.x=TRUE)
cityKarte <- merge(x=cityKarte,y=location, by.x ="BFS_GdNr", by.y= "BFS_GdNr", all.x=TRUE)
cityKarte <- merge(x=cityKarte,y=grossregion, by.x ="BFS_GdNr", by.y= "BFS_GdNr", all.x=TRUE)
cityKarte$groessenkat <- ifelse(cityKarte$value < 20000, 1, ifelse(cityKarte$value < 100000, 2, 3))

Link = c()
for(i in cityKarte$GdName){
  Link = append(Link, paste("https://smartcityfactsheet.s3.eu-central-1.amazonaws.com/public/Factsheet_", gsub("[[:space:]]", "",gsub("/", ",", gsub("[[:space:]]\\([[:alpha:]]+\\)|[[:space:]]ZH", "",i))), ".pdf", sep = ""))
}
cityKarte$Link = Link

# Funktion zur Farbzuteilung der Pins
getIcon <- function(df) {
  sapply(df$Maturitaet, function(Maturitaet) {
    if(Maturitaet == "noch keine Phase") {
      "pin.png"
    } else if(Maturitaet == "Pilotprojektphase") {
      "pin-2.png"
    } else if(Maturitaet == "Institutionalisierungsphase") {
      "pin-3.png"
    } else if(Maturitaet == "Etablierungsphase"){
      "pin-4.png"
    } })
}



# Use rlang's set_names() to easily create a valide "choices"
# argument of the dropdown where the displayed text has to be
# different than the value passed as the input selection


ui <- dashboardPage(
  dashboardHeader(
    title = "Swiss Smart City Survey ",
    titleWidth = 250,
    # disable = TRUE
    tags$li(a(href = "https://smartcity-survey.ch",
              icon("home"),),
            class = "dropdown",
            tags$script(HTML("var openTab = function(tabName){
                              $('a', $('.sidebar')).each(function() {
                              if(this.getAttribute('data-value') == tabName) {
                              this.click()
                              };
                              });
                              }")))
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analyse", tabName = "dashboard", icon = icon("chart-bar")),
      menuItem("Karte", icon = icon("map-marker-alt"), tabName = "map"
      )
    ),

    uiOutput('resetable_input'),

    #Resetknopf
    actionButton("reset_input", "Zurücksetzen"),
    tags$br(),
    tags$br(),

    # Partnerbanner
    column(12,align="center",
           tags$b("Wir werden unterstützt von",align="center", color = "#465569"),
           tags$br(),
           tags$br(),

           tags$img(src="partner.png", width = "150px",align="center")
    )
  ),
  dashboardBody(
    # tags$style(".fa-city {color:#D7DCE6; font-size: 45px;}"),
    # tags$style(".fa-ban {color:#D7DCE6; font-size: 45px;}"),
    # tags$style(".fa-chart-line {color:#D7DCE6; font-size: 45px;}"),
    # tags$style(".fa-calendar_check {color:#D7DCE6; font-size: 45px;}"),
    # tags$style(".fa-rocket {color:#D7DCE6; font-size: 45px;}"),
    # 
    
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    tags$style(".small-box.bg-yellow { background-color: #0F467D!important;}"),
    
    tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #0F467D;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #0F467D;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #0F467D;
                                color: #000000;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color:#D7DCE6 ;
                                color:#465569;
                                }
                                /* color font input container */
                                section.sidebar .shiny-input-container{
                                color:#465569;
                                }
                                
                                /* color logo text */
                                .skin-blue .main-sidebar .sidebar{
                                color: #465569;}

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #A0EBB4;
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #0F467D ;
                                color: white;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #8296AF;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #8296AF;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: white ;
                                }
                              
              

                              
                    
                              @media (max-width: 1225px) and (min-width:768px) {

                              
                              
                                  .col-sm-2{
                                  width: 50%;
                                    text-align: center;

                                  }
                                  
                                  .col-sm-6 {
                                  width:100%;
                                  
                                  }
                                  .col-sm-3{
                                  width:100%;
                                  text-align: center;
                                  }
                                  
                                  .col-sm-8{
                                  width:100%;
                                  }
                              }
                                  
                                  
                                  .col-sm-2{
                                    overflow-wrap: break-word;
                             -webkit-hyphens: auto;
                               -ms-hyphens: auto;
                                hyphens: auto;
                                  
                                  }
                            .col-sm-3{
                                    overflow-wrap: break-word;
                             -webkit-hyphens: auto;
                               -ms-hyphens: auto;
                                hyphens: auto;
                                  
                                  }
            

                                '))),
    
    
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(column(8,
                            h1("Swiss Smart City Survey 2020"),
                            tags$div("Der Smart City Survey der ZHAW ist eine regelmässige Umfrage in Zusammenarbeit mit Kollaborationspartnern zum Stand der Entwicklung und Trends sowie zu den Bedürfnissen von Städten und Gemeinden in der Schweiz bei ihrer Transformation zu Smart Sustainable Cities & Communities wie auch bei der Umsetzung von Smart-City-Lösungen. Die Resultate basieren auf Selbsteinschätzungen verschiedener Städtevertreter. Der Survey wurde zwischen Januar 2020 und April 2020 durchgeführt."),
                            tags$div("Das Dashboard ermöglicht die interaktive Analyse der Ergebnisse. Alle Werte werden aus Gründen der Vertraulichkeit ", tags$b("erst ab vier Städten"), " angezeigt.")
                            ),
                     
                     column(3,
                            div( tags$img(src = "zhaw_logo.png", height = "90px", align = "right"))
                     )
                     ),
            
            br(),
            
            fluidRow(column(12, 
                            h3("Teilnehmende Städte und ihr Entwicklungsstand (vgl. ZHAW Smart City Leitfaden):")
                            ),
                     
                     valueBoxOutput("anzahl_cities",width=2),
                     valueBoxOutput("phase0",width=2),
                     valueBoxOutput("phase1",width=2),
                     valueBoxOutput("phase2",width=2),
                     valueBoxOutput("phase3",width=2)
                     ),
            
            #Plots Reihe 1
            fluidRow(box(h3("Swiss Smart City Index (0 bis 100)"),
                         h5("In welchen Bereichen liegen die Schwerpunkte bei der Smart City Entwicklung?
                         (Doppelkick: Zurück zur Ausgangsposition)"),
                         plotlyOutput("columnPlot1"),
                         height=500
                         ),
                     
                     box(h3("Kollaborationspartner"),
                         h5("Mit welchen Institutionen wird bei der Projektumsetzung zusammengearbeitet?"),
                         plotlyOutput("columnPlot2"),
                         tags$br(),
                         height=500
                         )
                     ),
              
            #Plots Reihe 2
            fluidRow(box(h3("Projekte nach Teilbereichen"),
                         h5("In welchem Bereich sind die aktuellen Smart City Projekte angesiedelt?"),
                         plotlyOutput("columnPlot3"),
                         height=500
                         ),
                     
                     box(h3("Organisationale Ausgestaltung"),
                         h5("Welche der folgenden Aspekte einer Smart City Politik sind vorhanden?"),
                         plotlyOutput("columnPlot4"),
                         height=500
                         ),
                     ),
          ),
    
    tabItem(tabName = "map",
            fluidRow(column(8,
                            h1("Swiss Smart City Survey 2020"),
                            tags$div("Der Smart City Survey der ZHAW ist eine regelmässige Umfrage in Zusammenarbeit mit Kollaborationspartnern zum Stand der Entwicklung und Trends sowie zu den Bedürfnissen von Städten und Gemeinden in der Schweiz bei ihrer Transformation zu Smart Sustainable Cities & Communities wie auch bei der Umsetzung von Smart-City-Lösungen. Die Resultate basieren auf Selbsteinschätzungen verschiedener Städtevertreter. Der Survey wurde zwischen Januar 2020 und April 2020 durchgeführt."),
            ),  
                     
                     column(3,
                            div( tags$img(src = "zhaw_logo.png", height = "90px", align = "right"))
                            )
                     ),
            br(),
            fluidRow(# Karte
                     box(h3("Smart Cities im Detail"),
                         tags$div("Es werden lediglich Städte angezeigt, welche zugestimmt haben, dass ihre Detaildaten veröffentlicht werden dürfen.", tags$br(),
                                  "Um auf Detailinformationen und auf das Factsheet einer Stadt zuzugreifen, klicken Sie auf den jeweiligen Pin der Stadt."),
                         tags$br(),
                         leafletOutput("leafletmap", height="600px", width = "1150px"),
                         width = 12,
                     ),
            ),
            br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
            )
    )
  )
)

#backend ####
server <- function(input, output, session) {
  
  
  output$anzahl_cities <- renderValueBox({
    nrow(filterPlots())%>%
      valueBox(
        icon=icon("city"),
        subtitle = "Städte",
        color="yellow"
        
      )
  })
  
  output$phase0 <- renderValueBox({
    paste(round(sum(filterPlots()$Maturitaet == "noch keine Phase" )/(nrow(filterPlots()))*100,1), "%")%>%
      valueBox(  
        icon=icon("ban"),
        subtitle = "Keine Phase erkennbar",
        color="yellow"
        
      )
  })  
  output$phase1 <- renderValueBox({
    paste(round(sum(filterPlots()$Maturitaet == "Pilotprojektphase" )/(nrow(filterPlots()))*100,1), "%")%>%
      valueBox(
        subtitle=HTML(paste("Pilotprojekt", "phase", sep="&shy")),
        icon=icon("rocket"),
        color="yellow"
      )
  })   
  
  output$phase2 <- renderValueBox({
    paste(round(sum(filterPlots()$Maturitaet == "Institutionalisierungsphase" )/(nrow(filterPlots()))*100,1), "%")%>%
      valueBox(
        subtitle=HTML(paste("Institutionalisie", "rungs","phase", sep="&shy")),
        icon=icon("chart-line"),
        color="yellow"
        
        
      )
  })  
  output$phase3 <- renderValueBox({
    paste(round(sum(filterPlots()$Maturitaet == "Etablierungsphase" )/(nrow(filterPlots()))*100,1), "%")%>%
      valueBox(
        subtitle = HTML(paste("Etablierungs", "phase", sep="&shy")),
        color="yellow",
        icon=icon("calendar-check")
        
      )
  }) 
  
  
  
  
  #Filter
  filterPlots <- reactive({
    cityPlots%>%
      filter(
        conditional(input$Sprache != "Keine Auswahl", language == input$Sprache), 
        conditional(input$Region != "Keine Auswahl", Grossregion == input$Region),
        conditional(input$Gruppe != "Keine Auswahl", VG == input$Gruppe),
        conditional(input$Grösse != "Keine Auswahl", groessenkat == input$Grösse),
        conditional(input$Strategie != "Keine Auswahl", Enabl1 == input$Strategie)
      )
  })
  
  
  # KPI Outputs
  output$anzahl <- renderText(nrow(filterPlots()))
  output$keine <- renderText(paste(round(sum(filterPlots()$Maturitaet == "noch keine Phase" )/(nrow(filterPlots()))*100,1), "%"))
  output$pilot <- renderText(paste(round(sum(filterPlots()$Maturitaet == "Pilotprojektphase" )/(nrow(filterPlots()))*100, 1), "%"))
  output$institutionalisierung <- renderText(paste(round(sum(filterPlots()$Maturitaet == "Institutionalisierungsphase" )/(nrow(filterPlots()))*100, 1), "%"))
  output$etablierung <- renderText(paste(round(sum(filterPlots()$Maturitaet == "Etablierungsphase" )/(nrow(filterPlots()))*100, 1), "%"))
  
  #Reset Output
  output$resetable_input <- renderUI({
    times <- input$reset_input
    div(selectInput(inputId = "Sprache", label = "Sprachregion", choices = list("Keine Auswahl"="Keine Auswahl","Deutsch"="Deutsch", "Français"="Français", "Italiano"="Italiano"), selected = "Keine Auswahl", multiple = FALSE),
        selectInput(inputId = "Region", label = "Grossregion", choices = list("Keine Auswahl"="Keine Auswahl","Genferseeregion"=1, "Espace Mittelland"= 2, "Nordwestschweiz"=3, "Zürich"=4, "Ostschweiz"= 5, "Zentralschweiz"= 6, "Tessin"= 7), selected = "Keine Auswahl", multiple = FALSE),
        selectInput(inputId = "Grösse", label = "Grösse", choices = list("Keine Auswahl"="Keine Auswahl","< 20'000"=1, "20'001-100'000"=2, "100'000 <"=3), selected = "> 20'000", multiple = FALSE),
        selectInput(inputId = "Gruppe", label = "Vergleichsgruppe", choices = list("Keine Auswahl"="Keine Auswahl","Ländliche Zentren"="Ländliche Zentren", "Einzelstädte"="Einzelstädte","Mittlerer Ballungsraum" = "Mittlerer Ballungsraum" , "Grosser Ballungsraum"="Grosser Ballungsraum"), selected = "Keine Auswahl", multiple = FALSE),
        selectInput(inputId = "Strategie", label = "Strategie", choices = list("Keine Auswahl"="Keine Auswahl","Ja"="Ja", "In Erarbeitung"="In Erarbeitung", "Nein"="Nein"), selected = "Keine Auswahl" , multiple = FALSE),
    )
  })
  
  # Plot Outputs
  output$columnPlot1 <-  renderPlotly({
    if (nrow(filterPlots())<4){
      print("fail")
    } else {
      radar = filterPlots() %>% 
        filter(Filter_VG_Index==1) %>%
        select("GdName", "SMob_SubInd", "SPpl_SubInd", "SEco_SubInd",
                                       "SEnv_SubInd", "SGov_SubInd", "SLiv_SubInd", "SDat_SubInd", "SInf_SubInd", "Enabl_SubInd") 

      radar_av = cityPlots %>% 
            filter(Filter_VG_Index==1)%>% 
          select("GdName", "SMob_SubInd", "SPpl_SubInd", "SEco_SubInd",
                                      "SEnv_SubInd", "SGov_SubInd", "SLiv_SubInd", "SDat_SubInd", "SInf_SubInd", "Enabl_SubInd") 

      
      av = c("Durchschnitt", as.numeric(as.character(colMeans(as.matrix(radar_av[, -1]), na.rm = T))))
      radar = rbind(av, radar)
      radar[, -1] = data.frame(lapply(radar[, -1], function(x) {as.numeric(x)}), stringsAsFactors = F)
      radar$GdName = as.character(radar$GdName)
      colnames(radar) = c("GdName", "Mobility", "People","Economy", "Environment", "Governance",  "Living", "Data", "Infrastructure", "Enabler")
      
      av = c("Auswahl", as.numeric(as.character(colMeans(as.matrix(radar[-1, -1]), na.rm = T))))
      radar = rbind(radar, av)
      radar = radar %>% filter(GdName %in% c("Auswahl", "Durchschnitt"))
      radar[, -1] = data.frame(lapply(radar[, -1], function(x) {as.numeric(x)}), stringsAsFactors = F)
      radar <- radar[, !colSums(is.na(radar))]
      radar2=data.frame(t(select(radar,-GdName)))
      
      fig0 <- plot_ly(
        type = 'scatterpolar',
        mode='lines',) %>%
        add_closed_trace(
          r = radar2$X1,
          theta = row.names(radar2),
          name = 'Durchschnitt',
          marker = list(color = '#A0EBB4'),
          line=list(color = '#A0EBB4'),
          hovertemplate='Durchschnitt: %{r:.1f } in %{theta} <extra></extra>'
          
          
        ) %>%
        add_closed_trace(
          type = 'scatterpolar',
          mode='lines',
          r = radar2$X2,
          theta = row.names(radar2),
          name = 'Auswahl',
          marker = list(color = '0F467D'),
          line=list(color = '0F467D'),
          hovertemplate='Auswahl: %{r:.1f } in %{theta} <extra></extra>'
          
        ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              tickvals = c(0, 10,20,30, 40, 50, 60, 70, 80, 90, 100)
              #range = c(0,100)
            )),
          xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE),
          legend = list(orientation = "h", x = 0.4, y = -0.2))%>%
        config(displayModeBar = F) 

      
    }
  })
  
  
  output$columnPlot2 <-  renderPlotly({
    if (nrow(filterPlots())<4){
      print("fail")
    } else {
      kollp = filterPlots() %>% select("GdName","ZVerw","ZStadtw","ZLokUnt","ZINatUnt", "ZBndbetr","ZVerb","ZLokVer", "ZWiss","ZAndGd", "ZKanton","ZBund")
      kollp_av = cityPlots %>% select("GdName","ZVerw","ZStadtw","ZLokUnt","ZINatUnt", "ZBndbetr","ZVerb","ZLokVer","ZWiss","ZAndGd", "ZKanton","ZBund")
      partner = c("Departementsübergreifend","Stadtwerke/EVU","Lokale Unternehmen","(Inter-)Nationale Unternehmen",
                  "Bundesbetriebe","Verbände","Lokale Vereine, Interessengruppen","Wissenschaftliche Institutionen","Andere Gemeinden/Städte",
                  "Kanton","Bund")
      
      kollp = data.frame(lapply(kollp, function(x) {gsub("0", NA, x)}), stringsAsFactors = F)
      kollp = data.frame(lapply(kollp, function(x) {gsub("nie", 1, x)}), stringsAsFactors = F)
      kollp = data.frame(lapply(kollp, function(x) {gsub("selten", 2, x)}), stringsAsFactors = F)
      kollp = data.frame(lapply(kollp, function(x) {gsub("ab und zu", 3, x)}), stringsAsFactors = F)
      kollp = data.frame(lapply(kollp, function(x) {gsub("oft", 4, x)}), stringsAsFactors = F)
      kollp = data.frame(lapply(kollp, function(x) {gsub("immer", 5, x)}), stringsAsFactors = F)
      kollp[, 2:12] = data.frame(lapply(kollp[, 2:12], function(x) {as.numeric(x)-1}), stringsAsFactors = F)
      kollp_av = data.frame(lapply(kollp_av, function(x) {gsub("0", NA, x)}), stringsAsFactors = F)
      kollp_av = data.frame(lapply(kollp_av, function(x) {gsub("nie", 1, x)}), stringsAsFactors = F)
      kollp_av = data.frame(lapply(kollp_av, function(x) {gsub("selten", 2, x)}), stringsAsFactors = F)
      kollp_av = data.frame(lapply(kollp_av, function(x) {gsub("ab und zu", 3, x)}), stringsAsFactors = F)
      kollp_av = data.frame(lapply(kollp_av, function(x) {gsub("oft", 4, x)}), stringsAsFactors = F)
      kollp_av = data.frame(lapply(kollp_av, function(x) {gsub("immer", 5, x)}), stringsAsFactors = F)
      kollp_av[, 2:12] = data.frame(lapply(kollp_av[, 2:12], function(x) {as.numeric(x)-1}), stringsAsFactors = F)
      names(kollp)[2:12] = partner
      av = c("Durchschnitt", as.numeric(as.character(colMeans(as.matrix(kollp_av[, -1]), na.rm = T))))
      kollp = rbind(av, kollp)
      kollp[, -1] = data.frame(lapply(kollp[, -1], function(x) {as.numeric(x)}), stringsAsFactors = F)
      av = c("Auswahl", as.numeric(as.character(colMeans(as.matrix(kollp[-1, -1]), na.rm = T))))
      kollp = rbind(kollp, av)
      kollp = gather(kollp, partner, score, -1)
      kollp$score = as.numeric(kollp$score)
      kollp = kollp %>% filter(GdName %in% c("Durchschnitt", "Auswahl"))
      kollp=kollp%>%
        spread(key=GdName,value=score)
      
      m <- list(
        l = 50,
        r = 30,
        b = 5,
        t = 5,
        pad = 10
      )
      
      fig2 <- plot_ly(kollp,
                      orientation='h',
                      x = ~Auswahl,
                      y = ~reorder(partner,Durchschnitt), 
                      type = 'bar', 
                      name = 'Auswahl',
                      marker = list(color = '0F467D'),
                      hovertemplate='Auswahl: %{x:.1f } <extra></extra>'
      )%>%
        add_trace(x = ~Durchschnitt, 
                  name = 'Durchschnitt',
                  marker = list(color = '#A0EBB4'), 
                  hovertemplate = 'Durchschnitt: %{x:.1f %} <extra></extra>'
        )%>%
        layout(xaxis = list(title = 'Häufigkeit der Zusammenarbeit (0:nie, 4: immer)', 
                            ticktext = list("nie", "", "", "", "immer"), 
                            tickvals = list(0, 1, 2, 3, 4),
                            range=c(0,4),
                            fixedrange=TRUE),
               margin = m,
               yaxis = list(title = '', fixedrange=TRUE),
               legend = list(orientation = "h", x = 0.4, y = -0.2))%>%
        config(displayModeBar = F)  
      
      fig2  
      
      
      
    }
  })
  
  output$columnPlot3 <-  renderPlotly({
    if (nrow(filterPlots())<4){
      print("fail")
    } else {
      projekte = filterPlots() %>% select("GdName","Proj1", "Proj2",  "Proj3",  "Proj4", "Proj5","Proj6", "Proj7", "Proj8", "Proj9", "Proj10")
      projekte_av = cityPlots %>% select("GdName","Proj1", "Proj2",  "Proj3",  "Proj4", "Proj5","Proj6", "Proj7", "Proj8", "Proj9", "Proj10")
      
      projekte$Governance = sapply("Smart Governance",function(x)rowSums(projekte==x))
      projekte$Mobility = sapply("Smart Mobility",function(x)rowSums(projekte==x))
      projekte$Environment = sapply("Smart Environment",function(x)rowSums(projekte==x))
      projekte$Economy = sapply("Smart Economy",function(x)rowSums(projekte==x))
      projekte$Living = sapply("Smart Living",function(x)rowSums(projekte==x))
      projekte$People = sapply("Smart People",function(x)rowSums(projekte==x))
      projekte = projekte %>% select(GdName, Governance, Mobility, Environment, Economy, Living, People)
      
      projekte_av$Governance = sapply("Smart Governance",function(x)rowSums(projekte_av==x))
      projekte_av$Mobility = sapply("Smart Mobility",function(x)rowSums(projekte_av==x))
      projekte_av$Environment = sapply("Smart Environment",function(x)rowSums(projekte_av==x))
      projekte_av$Economy = sapply("Smart Economy",function(x)rowSums(projekte_av==x))
      projekte_av$Living = sapply("Smart Living",function(x)rowSums(projekte_av==x))
      projekte_av$People = sapply("Smart People",function(x)rowSums(projekte_av==x))
      projekte_av = projekte_av %>% select(GdName, Governance, Mobility, Environment, Economy, Living, People)
      
      av = c("Durchschnitt", as.numeric(as.character(colSums(as.matrix(projekte_av[, -1]), na.rm = T))))
      projekte = rbind(av, projekte)
      projekte[, -1] = data.frame(lapply(projekte[, -1], function(x) {as.numeric(x)}), stringsAsFactors = F)
      projekte$Sum = rowSums(as.matrix(projekte[, -1]), na.rm = T)
      projekte[, -1] = data.frame(lapply(projekte[, -1], function(x) {as.numeric(x)}), stringsAsFactors = F)
      av = c("Auswahl", as.numeric(as.character(colSums(as.matrix(projekte[-1, -1]), na.rm = T))))
      projekte = rbind(projekte, av)
      projekte[, -1] = data.frame(lapply(projekte[, -1], function(x) {as.numeric(gsub("NaN", 0,(as.numeric(x)/as.numeric(projekte$Sum))*100))}), stringsAsFactors = F)
      projekte = projekte %>% select(-Sum)
      projekte = gather(projekte, Projekte, Anzahl, -1)
      projekte$Anzahl = as.numeric(projekte$Anzahl)
      projekte = projekte %>%
        filter(GdName %in% c("Durchschnitt", "Auswahl"))%>%
        spread(key=GdName,value=Anzahl)
      
      fig <- plot_ly(projekte, x = ~reorder(Projekte,-Durchschnitt),
                     y = ~Auswahl, 
                     type = 'bar', 
                     name = 'Auswahl',
                     marker = list(color = '0F467D'),
                     hovertemplate='Auswahl: %{y:.1f %}% <extra></extra>'
      )%>%
        add_trace(y = ~Durchschnitt, 
                  name = 'Durchschnitt',
                  marker = list(color = '#A0EBB4'), 
                  hovertemplate = 'Durchschnitt: %{y:.1f %}% <extra></extra>'
        )%>%
        layout(yaxis = list(title = 'Anteil Projekte in %', fixedrange=TRUE),
               xaxis = list(title = '', fixedrange=TRUE),
               legend = list(orientation = "h", x = 0.4, y = -0.2))%>%
        config(displayModeBar = F)
      
      #fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
      
      fig
      
    }
  })
  
  output$columnPlot4 <-  renderPlotly({
    if (nrow(filterPlots())<4){
      print("fail")
    } else {
      orga = filterPlots() %>% select("GdName", "SC_Ziele", "Enabl1", "SC_Aktiv","Enabl2", "SC_Polit", "Enabl3", "SC_Monitor")
      orga_av = cityPlots %>% select("GdName", "SC_Ziele", "Enabl1", "SC_Aktiv","Enabl2", "SC_Polit", "Enabl3", "SC_Monitor")
      
      orga = data.frame(lapply(orga, function(x) {gsub("Ja", 1, x)}), stringsAsFactors = F)
      orga = data.frame(lapply(orga, function(x) {gsub("Nein", 0, x)}), stringsAsFactors = F)
      orga = data.frame(lapply(orga, function(x) {gsub("In Erarbeitung", 0.5, x)}), stringsAsFactors = F)
      orga = data.frame(lapply(orga, function(x) {gsub("Weiss nicht", NA, x)}), stringsAsFactors = F)
      orga[, -1] = data.frame(lapply(orga[, -1], function(x) {as.numeric(x)}), stringsAsFactors = F)
      orga_av = data.frame(lapply(orga_av, function(x) {gsub("Ja", 1, x)}), stringsAsFactors = F)
      orga_av = data.frame(lapply(orga_av, function(x) {gsub("Nein", 0, x)}), stringsAsFactors = F)
      orga_av = data.frame(lapply(orga_av, function(x) {gsub("In Erarbeitung", 0.5, x)}), stringsAsFactors = F)
      orga_av = data.frame(lapply(orga_av, function(x) {gsub("Weiss nicht", NA, x)}), stringsAsFactors = F)
      orga_av[, -1] = data.frame(lapply(orga_av[, -1], function(x) {as.numeric(x)}), stringsAsFactors = F)
      
      av = c("Durchschnitt", as.numeric(as.character(colMeans(as.matrix(orga_av[, -1]), na.rm = T))))
      orga = rbind(av, orga)
      colnames(orga) = c("GdName", "Ziele", "Strategie","Aktive Bearbeitung", "Fachstelle", "Auftrag zur Strategieerarbeitung",  "Budget", "Monitoring")
      orga[, -1] = data.frame(lapply(orga[, -1], function(x) {as.numeric(x)*100}), stringsAsFactors = F)
      
      av = c("Auswahl", as.numeric(as.character(colMeans(as.matrix(orga[-1, -1]), na.rm = T))))
      orga = rbind(orga, av)
      orga = gather(orga, Orga, Anteil, -1)
      orga$Anteil = as.numeric(orga$Anteil)
      orga = orga %>% filter(GdName %in% c("Durchschnitt", "Auswahl"))%>%
        spread(key=GdName,value=Anteil)
      
      m <- list(
        l = 50,
        r = 30,
        b = 5,
        t = 5,
        pad = 10
      )
      
      fig4 <- plot_ly(orga,
                      orientation='h',
                      x = ~Auswahl,
                      y = ~reorder(Orga,Durchschnitt), 
                      type = 'bar', 
                      name = 'Auswahl',
                      marker = list(color = '0F467D'),
                      hovertemplate='Auswahl: %{x:.1f }% <extra></extra>'
      )%>%
        add_trace(x = ~Durchschnitt, 
                  name = 'Durchschnitt',
                  marker = list(color = '#A0EBB4'), 
                  hovertemplate = 'Durchschnitt: %{x:.1f %}% <extra></extra>'
        )%>%
        layout(xaxis = list(title = 'Anteil vorhanden in %', fixedrange=TRUE),
               yaxis = list(title = '', fixedrange=TRUE),
               margin = m,
               legend = list(orientation = "h", x = 0.4, y = -0.2))%>%
        config(displayModeBar = F)  
      
      fig4   
      
      
      
      
    }
   })
  
  
  
  # Filter map
  filterKarte <- reactive({
    cityKarte%>%
      filter(
        conditional(input$Sprache != "Keine Auswahl", language == input$Sprache), 
        conditional(input$Region != "Keine Auswahl", Grossregion == input$Region),
        conditional(input$Gruppe != "Keine Auswahl", VG == input$Gruppe),
        conditional(input$Grösse != "Keine Auswahl", groessenkat == input$Grösse),
        conditional(input$Strategie != "Keine Auswahl", Enabl1 == input$Strategie)
      )
  })
  
  
  output$leafletmap <- renderLeaflet({
    
    map_data <- filterKarte()  # Add this
    
    icons <- makeIcon(
      iconUrl = as.character((getIcon(map_data))),
      iconWidth = 38,
      iconAnchorX = 20, iconAnchorY = 40,
      #shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
      #shadowWidth = 50, shadowHeight = 64,
      #shadowAnchorX = 4, shadowAnchorY = 62
    )
    
    # Beschriftung der Kartenlegende
    label <- c(paste("Keine Phase erkennbar (", sum(map_data$Maturitaet == "noch keine Phase"), ")", sep = ""),
               paste("Pilotprojektphase (", sum(map_data$Maturitaet == "Pilotprojektphase"), ")", sep = ""),
               paste("Institutionalisierungsphase (", sum(map_data$Maturitaet == "Institutionalisierungsphase"), ")", sep = ""),
               paste("Etablierungsphase (", sum(map_data$Maturitaet == "Etablierungsphase"), ")", sep = ""))
    
    map_data %>% leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng=map_data$Longitude, lat=map_data$Latitude, icon=icons,  popup = paste(paste("<h3>",map_data$GdName ,"</h3>"), 
                                                                                           paste("Aktive Bearbeitung: " ,map_data$SC_Aktiv),
                                                                                           paste("Smart-City-Ziele: " ,map_data$SC_Ziele), 
                                                                                           paste("Smart-City-Strategie: ", map_data$Enabl1), 
                                                                                           paste("Smart-City-Fachstelle: ", map_data$Enabl2), 
                                                                                           paste("Politischer Auftrag: ", map_data$SC_Polit), 
                                                                                           paste("Smart-City-Budget: ", map_data$Enabl3), 
                                                                                           paste("Smart-City-Monitoring: ", map_data$SC_Monitor),
                                                                                           paste('<br/><a href=',map_data$Link,' target="_blank">Factsheet als PDF</a>'), 
                                                                                           sep =  "<br/>")) %>%  
      addLegend(
        position = 'topright',
        opacity=1,
        colors = c(rgb(130/255, 150/255, 175/255), rgb(160/255,235/255,280/355), rgb(45/255, 220/255, 195/255), rgb(15/255, 70/255, 125/255)),
        labels = label,
        title = 'Maturität')
  })
}


shinyApp(ui, server)