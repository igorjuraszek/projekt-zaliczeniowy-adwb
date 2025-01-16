# --------------------------------------------
# app.R
# --------------------------------------------

# 1) Załadowanie potrzebnych pakietów
library(shiny)
library(shinydashboard)   # ładny layout
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(DT)
library(leaflet)

# 2) Połączenie z bazą 
con <- dbConnect(RSQLite::SQLite(), "chinook.db")

# 3) UI aplikacji
ui <- dashboardPage(
  
  dashboardHeader(title = "Kokpit analityczny"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tabela",        tabName = "table_tab",        icon = icon("table")),
      menuItem("Wykresy",       tabName = "charts_tab",       icon = icon("chart-bar")),
      menuItem("Mapa",          tabName = "map_tab",          icon = icon("globe")),
      menuItem("Dodatkowe wykresy", tabName = "extra_charts_tab", icon = icon("chart-area")),
      
      br(),
      # Widżety do filtrowania:
      dateRangeInput(
        inputId = "dateRange", 
        label   = "Zakres dat zamówień (InvoiceDate):", 
        start   = "2009-01-01", 
        end     = "2013-12-31"
      ),
      selectInput(
        inputId = "countryInput",
        label   = "Wybierz kraj:",
        choices = c(
          "USA", "Canada", "France", "Brazil", "Germany", "United Kingdom",
          "Portugal", "India", "Czech Republic", "Sweden", "Spain", "Poland",
          "Norway", "Netherlands", "Italy", "Ireland", "Hungary", "Finland",
          "Denmark", "Chile", "Belgium", "Austria", "Australia", "Argentina",
          "Wszystkie"
        ), 
        selected = "Wszystkie"
      ),
      actionButton("applyFilters", "Zastosuj filtry")
    )
  ),
  
  dashboardBody(
    tabItems(
      # --- TABELA ---
      tabItem(tabName = "table_tab",
              fluidRow(
                box(width = 12,
                    title       = "Tabela z danymi",
                    status      = "primary",
                    solidHeader = TRUE,
                    DTOutput("customersTable"))
              )
      ),
      
      # --- WYKRESY ---
      tabItem(tabName = "charts_tab",
              fluidRow(
                box(width = 6,
                    title       = "Dochód wg gatunków",
                    status      = "primary",
                    solidHeader = TRUE,
                    plotOutput("genreRevenuePlot", height = "300px")
                ),
                box(width = 6,
                    title       = "Sprzedaż w czasie",
                    status      = "primary",
                    solidHeader = TRUE,
                    plotOutput("timeSeriesPlot",   height = "300px")
                )
              )
      ),
      
      # --- MAPA ---
      tabItem(tabName = "map_tab",
              fluidRow(
                box(width = 12,
                    title       = "Przychody wg kraju (mapa)",
                    status      = "primary",
                    solidHeader = TRUE,
                    leafletOutput("countryMap", height = "500px")
                )
              )
      ),
      
      # --- DODATKOWE WYKRESY ---
      tabItem(tabName = "extra_charts_tab",
              fluidRow(
                box(width = 6,
                    title       = "Top 10 utworów (Tracks) wg przychodu",
                    status      = "primary",
                    solidHeader = TRUE,
                    plotOutput("topTracksPlot", height = "300px")
                ),
                box(width = 6,
                    title       = "Średnia wartość faktur (Invoices) wg kraju",
                    status      = "primary",
                    solidHeader = TRUE,
                    plotOutput("avgInvoicePlot", height = "300px")
                )
              ),
              fluidRow(
                box(width = 6,
                    title       = "Rozkład długości utworów (Track Length)",
                    status      = "primary",
                    solidHeader = TRUE,
                    plotOutput("trackLengthPlot", height = "300px")
                ),
                box(width = 6,
                    title       = "Przychód wg artystów (Top 10)",
                    status      = "primary",
                    solidHeader = TRUE,
                    plotOutput("artistRevenuePlot", height = "300px")
                )
              )
      )
    )
  )
)


# 4) SERVER aplikacji
server <- function(input, output, session) {
  
  ###########################################
  # Uniwersalne współrzędne do MAPY
  ###########################################
  country_coords <- data.frame(
    Country = c(
      "USA", "Canada", "France", "Brazil", "Germany", "United Kingdom",
      "Portugal", "India", "Czech Republic", "Sweden", "Spain", "Poland",
      "Norway", "Netherlands", "Italy", "Ireland", "Hungary", "Finland",
      "Denmark", "Chile", "Belgium", "Austria", "Australia", "Argentina",
      "Wszystkie"
    ),
    lat = c(
      37.0902, 56.1304, 46.2276, -14.2350, 51.1657, 55.3781,
      39.3999, 20.5937, 49.8175, 60.1282, 40.4637, 51.9194,
      60.4720, 52.1326, 41.8719, 53.4129, 47.1625, 61.9241,
      56.2639, -35.6751, 50.5039, 47.5162, -25.2744, -38.4161,
      0
    ),
    lng = c(
      -95.7129, -106.3468, 2.2137, -51.9253, 10.4515, -3.4360,
      -8.2245, 78.9629, 15.4730, 18.6435, -3.7492, 19.1451,
      8.4689, 5.2913, 12.5674, -8.2439, 19.5033, 25.7482,
      9.5018, -71.5430, 4.4699, 14.5501, 133.7751, -63.6167,
      0
    )
  )
  
  ###########################################
  # 1) Reaktywny zbiór danych do TABELI
  ###########################################
  filteredData <- eventReactive(input$applyFilters, {
    sql_query <- "
      SELECT 
        c.CustomerId,
        c.FirstName,
        c.LastName,
        c.Country,
        c.City,
        c.Email,
        i.InvoiceDate,
        i.Total
      FROM customers c
      JOIN invoices i ON c.CustomerId = i.CustomerId
    "
    df <- dbGetQuery(con, sql_query)
    
    # Konwersja daty
    df$InvoiceDate <- as.Date(df$InvoiceDate)
    
    # Filtrowanie po dacie
    df <- df %>%
      filter(
        InvoiceDate >= input$dateRange[1],
        InvoiceDate <= input$dateRange[2]
      )
    
    # Filtrowanie po kraju
    if (input$countryInput != "Wszystkie") {
      df <- df %>% filter(Country == input$countryInput)
    }
    df
  })
  
  output$customersTable <- renderDT({
    datatable(
      filteredData(),
      options = list(pageLength = 10, autoWidth = TRUE),
      filter  = "top"
    )
  })
  
  
  ###########################################
  # 2) WYKRES: Dochód wg gatunków muzycznych
  ###########################################
  genreData <- eventReactive(input$applyFilters, {
    sql_genre <- "
      SELECT 
        i.InvoiceDate,
        c.Country,
        g.Name AS Genre,
        SUM(ii.UnitPrice * ii.Quantity) AS Revenue
      FROM invoice_items ii
      JOIN invoices i ON ii.InvoiceId = i.InvoiceId
      JOIN customers c ON i.CustomerId = c.CustomerId
      JOIN tracks t ON ii.TrackId = t.TrackId
      JOIN genres g ON t.GenreId = g.GenreId
      GROUP BY i.InvoiceId, g.GenreId
    "
    df <- dbGetQuery(con, sql_genre)
    df$InvoiceDate <- as.Date(df$InvoiceDate)
    
    # Filtry
    df <- df %>%
      filter(
        InvoiceDate >= input$dateRange[1],
        InvoiceDate <= input$dateRange[2]
      )
    if (input$countryInput != "Wszystkie") {
      df <- df %>% filter(Country == input$countryInput)
    }
    
    # Sumaryczny przychód na gatunek
    df %>%
      group_by(Genre) %>%
      summarise(TotalRevenue = sum(Revenue, na.rm = TRUE)) %>%
      arrange(desc(TotalRevenue))
  })
  
  output$genreRevenuePlot <- renderPlot({
    plot_data <- genreData()
    if (nrow(plot_data) == 0) return(NULL)
    
    ggplot(plot_data, aes(x = reorder(Genre, -TotalRevenue), y = TotalRevenue)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        x     = "Gatunek",
        y     = "Przychód",
        title = "Dochód wg gatunków muzycznych"
      )
  })
  
  
  ###########################################
  # 3) WYKRES: Trend sprzedaży w czasie
  ###########################################
  timeData <- eventReactive(input$applyFilters, {
    sql_time <- "
      SELECT 
        i.InvoiceDate,
        c.Country,
        SUM(ii.UnitPrice * ii.Quantity) AS Revenue
      FROM invoice_items ii
      JOIN invoices i ON ii.InvoiceId = i.InvoiceId
      JOIN customers c ON i.CustomerId = c.CustomerId
      GROUP BY i.InvoiceId
    "
    df <- dbGetQuery(con, sql_time)
    df$InvoiceDate <- as.Date(df$InvoiceDate)
    
    # Filtry
    df <- df %>%
      filter(
        InvoiceDate >= input$dateRange[1],
        InvoiceDate <= input$dateRange[2]
      )
    if (input$countryInput != "Wszystkie") {
      df <- df %>% filter(Country == input$countryInput)
    }
    
    # Agregacja miesięczna
    df %>%
      mutate(YearMonth = format(InvoiceDate, "%Y-%m")) %>%
      group_by(YearMonth) %>%
      summarise(MonthlyRevenue = sum(Revenue, na.rm = TRUE)) %>%
      arrange(YearMonth)
  })
  
  output$timeSeriesPlot <- renderPlot({
    plot_data <- timeData()
    if (nrow(plot_data) == 0) return(NULL)
    
    ggplot(plot_data, aes(x = YearMonth, y = MonthlyRevenue, group = 1)) +
      geom_line(color = "steelblue") +
      geom_point(color = "steelblue") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        x     = "Miesiąc",
        y     = "Przychód",
        title = "Sprzedaż w czasie"
      )
  })
  
  
  ###########################################
  # 4) MAPA: Rozkład przychodów wg kraju
  ###########################################
  mapData <- eventReactive(input$applyFilters, {
    sql_map <- "
      SELECT 
        c.Country,
        i.InvoiceDate,
        SUM(ii.UnitPrice * ii.Quantity) AS Revenue
      FROM invoice_items ii
      JOIN invoices i ON ii.InvoiceId = i.InvoiceId
      JOIN customers c ON i.CustomerId = c.CustomerId
      GROUP BY i.InvoiceId, c.Country
    "
    df <- dbGetQuery(con, sql_map)
    df$InvoiceDate <- as.Date(df$InvoiceDate)
    
    # Filtry
    df <- df %>%
      filter(
        InvoiceDate >= input$dateRange[1],
        InvoiceDate <= input$dateRange[2]
      )
    if (input$countryInput != "Wszystkie") {
      df <- df %>% filter(Country == input$countryInput)
    }
    
    # Zagregujemy do poziomu jednego wiersza na kraj
    df <- df %>%
      group_by(Country) %>%
      summarise(Revenue = sum(Revenue, na.rm = TRUE))
    
    # Dołączamy współrzędne
    df <- left_join(df, country_coords, by = "Country")
    df$lat[is.na(df$lat)] <- 0
    df$lng[is.na(df$lng)] <- 0
    df
  })
  
  output$countryMap <- renderLeaflet({
    map_df <- mapData()
    
    leaflet(map_df) %>%
      addTiles() %>%
      addCircleMarkers(
        lat         = ~lat,
        lng         = ~lng,
        radius      = ~ifelse(Revenue > 0, pmin(Revenue / 5, 20), 3),
        color       = "red",
        stroke      = FALSE,
        fillOpacity = 0.5,
        label       = ~paste(Country, ":", round(Revenue, 2))
      ) %>%
      setView(lng = 0, lat = 20, zoom = 2)
  })
  
  
  ###########################################
  # DODATKOWE WYKRESY                     #
  ###########################################
  #
  # A) Top 10 utworów (Tracks) wg przychodu
  # B) Średnia wartość faktur (Invoices) wg kraju
  # C) Rozkład długości utworów (Track length)
  # D) Przychód wg artystów (Top 10)
  
  # A) Top 10 Tracks
  topTracksData <- eventReactive(input$applyFilters, {
    sql_tracks <- "
      SELECT 
        i.InvoiceDate,
        c.Country,
        t.Name AS TrackName,
        SUM(ii.UnitPrice * ii.Quantity) AS Revenue
      FROM invoice_items ii
      JOIN invoices i   ON ii.InvoiceId = i.InvoiceId
      JOIN customers c  ON i.CustomerId = c.CustomerId
      JOIN tracks t     ON ii.TrackId = t.TrackId
      GROUP BY i.InvoiceId, t.TrackId
    "
    df <- dbGetQuery(con, sql_tracks)
    df$InvoiceDate <- as.Date(df$InvoiceDate)
    
    # Filtry
    df <- df %>%
      filter(
        InvoiceDate >= input$dateRange[1],
        InvoiceDate <= input$dateRange[2]
      )
    if (input$countryInput != "Wszystkie") {
      df <- df %>% filter(Country == input$countryInput)
    }
    # Suma przychodu na utwór
    df <- df %>%
      group_by(TrackName) %>%
      summarise(TotalRevenue = sum(Revenue, na.rm = TRUE)) %>%
      arrange(desc(TotalRevenue)) %>%
      head(10)  # top 10
    df
  })
  
  output$topTracksPlot <- renderPlot({
    plot_data <- topTracksData()
    if (nrow(plot_data) == 0) return(NULL)
    
    ggplot(plot_data, aes(x = reorder(TrackName, -TotalRevenue), y = TotalRevenue)) +
      geom_col(fill = "darkgreen") +
      coord_flip() +
      labs(
        x     = "Nazwa utworu",
        y     = "Przychód",
        title = "TOP 10 utworów wg przychodu"
      )
  })
  
  # B) Średnia wartość faktur wg kraju
  avgInvoiceData <- eventReactive(input$applyFilters, {
    sql_avg <- "
      SELECT 
        i.InvoiceId,
        i.InvoiceDate,
        c.Country,
        i.Total
      FROM invoices i
      JOIN customers c ON i.CustomerId = c.CustomerId
    "
    df <- dbGetQuery(con, sql_avg)
    df$InvoiceDate <- as.Date(df$InvoiceDate)
    
    # Filtry
    df <- df %>%
      filter(
        InvoiceDate >= input$dateRange[1],
        InvoiceDate <= input$dateRange[2]
      )
    if (input$countryInput != "Wszystkie") {
      df <- df %>% filter(Country == input$countryInput)
    }
    # Średnia
    df <- df %>%
      group_by(Country) %>%
      summarise(AvgInvoice = mean(Total, na.rm = TRUE)) %>%
      arrange(desc(AvgInvoice))
    df
  })
  
  output$avgInvoicePlot <- renderPlot({
    plot_data <- avgInvoiceData()
    if (nrow(plot_data) == 0) return(NULL)
    
    ggplot(plot_data, aes(x = reorder(Country, -AvgInvoice), y = AvgInvoice)) +
      geom_col(fill = "orange") +
      coord_flip() +
      labs(
        x     = "Kraj",
        y     = "Średnia wartość faktury",
        title = "Średnia wartość faktur wg kraju"
      )
  })
  
  # C) Rozkład długości utworów (Track length)
  #    Tu track length jest w "Milliseconds" w tabeli "tracks"
  #    Ale musimy brać pod uwagę tylko utwory, które wystąpiły w invoice_items
  #    (i w ten sposób były kupione) w danym zakresie filtra.
  
  trackLengthData <- eventReactive(input$applyFilters, {
    sql_length <- "
      SELECT 
        i.InvoiceDate,
        c.Country,
        t.Milliseconds
      FROM invoice_items ii
      JOIN invoices i   ON ii.InvoiceId = i.InvoiceId
      JOIN customers c  ON i.CustomerId = c.CustomerId
      JOIN tracks t     ON ii.TrackId = t.TrackId
    "
    df <- dbGetQuery(con, sql_length)
    df$InvoiceDate <- as.Date(df$InvoiceDate)
    
    # Filtry
    df <- df %>%
      filter(
        InvoiceDate >= input$dateRange[1],
        InvoiceDate <= input$dateRange[2]
      )
    if (input$countryInput != "Wszystkie") {
      df <- df %>% filter(Country == input$countryInput)
    }
    # Zwracamy kolumnę z Ms
    df
  })
  
  output$trackLengthPlot <- renderPlot({
    plot_data <- trackLengthData()
    if (nrow(plot_data) == 0) return(NULL)
    
    # Ms -> sekundy
    plot_data$LengthSec <- plot_data$Milliseconds / 1000
    
    ggplot(plot_data, aes(x = LengthSec)) +
      geom_histogram(bins = 30, fill = "cornflowerblue", color = "white") +
      labs(
        x     = "Długość utworu (sekundy)",
        y     = "Liczba utworów (kupionych)",
        title = "Rozkład długości utworów w zakupach"
      )
  })
  
  # D) Przychód wg artystów (Top 10)
  artistRevenueData <- eventReactive(input$applyFilters, {
    sql_artist <- "
      SELECT
        i.InvoiceDate,
        c.Country,
        ar.Name AS Artist,
        SUM(ii.UnitPrice * ii.Quantity) AS Revenue
      FROM invoice_items ii
      JOIN invoices i   ON ii.InvoiceId = i.InvoiceId
      JOIN customers c  ON i.CustomerId = c.CustomerId
      JOIN tracks t     ON ii.TrackId = t.TrackId
      JOIN albums al    ON t.AlbumId = al.AlbumId
      JOIN artists ar   ON al.ArtistId = ar.ArtistId
      GROUP BY i.InvoiceId, ar.ArtistId
    "
    df <- dbGetQuery(con, sql_artist)
    df$InvoiceDate <- as.Date(df$InvoiceDate)
    
    # Filtry
    df <- df %>%
      filter(
        InvoiceDate >= input$dateRange[1],
        InvoiceDate <= input$dateRange[2]
      )
    if (input$countryInput != "Wszystkie") {
      df <- df %>% filter(Country == input$countryInput)
    }
    # Zbiorcza suma
    df <- df %>%
      group_by(Artist) %>%
      summarise(TotalRevenue = sum(Revenue, na.rm = TRUE)) %>%
      arrange(desc(TotalRevenue)) %>%
      head(10)  # top 10
    df
  })
  
  output$artistRevenuePlot <- renderPlot({
    plot_data <- artistRevenueData()
    if (nrow(plot_data) == 0) return(NULL)
    
    ggplot(plot_data, aes(x = reorder(Artist, -TotalRevenue), y = TotalRevenue)) +
      geom_col(fill = "purple") +
      coord_flip() +
      labs(
        x     = "Artysta",
        y     = "Przychód",
        title = "Top 10 artystów wg przychodu"
      )
  })
  
}

# 5) Uruchomienie aplikacji
shinyApp(ui = ui, server = server)
