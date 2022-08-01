#' @export
#' @import shiny
#' @importFrom shinyjs 'useShinyjs'
#' @import dygraphs
#' @import ggplot2

runFTSdata <- function() {
  options(show.error.messages = FALSE)
  app <-
    list(ui = fluidPage(useShinyjs(),
                              sidebarLayout(
                                sidebarPanel(
                                  selectizeInput("ticker",
                                                 label = "Geben Sie ein Tickersymbol oder Namen ein",
                                                 choices = NULL,
                                                 options = list(create = TRUE)),

                                  dateRangeInput("Datum",
                                                 "Zeitraum",
                                                 start = Sys.Date()-750,
                                                 end = Sys.Date()),

                                  br(),
                                  br(),
                                  # p(actionButton("loadData", "Laden", icon = icon("r-project")),
                                  #   br(),
                                  #   "Ausgewählte Daten werden nach dem Beenden der Anwendung in die globale R-Umgebung geladen.
                                  #   Variablenname entspricht dem Ticker (ohne '^'). Bspw. werden die Daten für den Aktienindex DAX30 (mit Ticker ^GDAXI) in
                                  #   dem Objekt", code("GDAXI"), "abgespeichert. Es können auch mehrere Datensätze geladen werden.",
                                  #   style = "font-family: 'consolas'; font-si16pt"),
                                  p(downloadButton("downloadData", "Speichern", icon = icon("save")), br(),
                                    "Ausgewählte Daten werden lokal auf der Festplatte gespeichert.",
                                    style = "font-family: 'consolas'; font-si16pt"),
                                  br(),
                                  p(actionButton("closeApp", "Schließen", icon = icon("power-off"), verify_fa = FALSE), br(),
                                    "Anwendung wird beendet.",
                                    style = "font-family: 'consolas'; font-si16pt"),
                                  br()
                                  # p(actionButton("histApp", "Zeigen/Ausblenden Dichtefunktion", icon = icon("eye"), verify_fa = FALSE), br(),
                                  #   "Geschätzte Dichtefunktion der Renditen wird angezeigt/ausgeblendet.",
                                  #   style = "font-family: 'consolas'; font-si16pt"),
                                  # br(),
                                  # plotOutput("hist")
                                ),

                                mainPanel(br(),
                                          br(),
                                          dygraphOutput("plot1"),
                                          br(),
                                          br(),
                                          br(),
                                          dygraphOutput("plot2"))
                              )
  ),
  server = function(input, output, session){
    updateSelectizeInput(session, "ticker",
                         choices = c(paste0(quarks:::ticker_stocks$Ticker[-1], " (", quarks:::ticker_stocks$Name[-1], ")"),
                                     paste0(quarks:::ticker_indices$Ticker, " (", quarks:::ticker_indices$Name, ")")),
                         selected = '',
                         server = TRUE)

    dataInput <- reactive({
      req(input$ticker)
      req(input$Datum[1])
      req(input$Datum[2])
      ticker = strsplit(input$ticker, " ")[[1]][1]
      suppressWarnings(
        suppressMessages(
          BatchGetSymbols::BatchGetSymbols(tickers = ticker, first.date = input$Datum[1],
                                           last.date = input$Datum[2], freq.data = "daily",
                                           cache.folder = file.path(tempdir(), 'BGS_Cache'))$df.tickers
        )
      )
    })


    output$plot1 <- renderDygraph({
      req(input$ticker)
      req(input$Datum[1])
      req(input$Datum[2])
      req(dataInput())

      dat = dataInput()
      plot_data = xts::xts(dat$price.close, dat$ref.date)
      dygraphs::dygraph(plot_data, main = paste(input$ticker, "- Schlusskurs (täglich)"),
                        ylab = "X<sub>t</sub>", group = "FZS") %>%
        dygraphs::dyUnzoom()
    })

    output$plot2 <- renderDygraph({
      req(input$ticker)
      req(input$Datum[1])
      req(input$Datum[2])
      req(dataInput())

      dat2 = dataInput()[-1, ]
      plot_data2 = suppressMessages(xts::xts(dat2$ret.closing.price, dat2$ref.date))
      dygraphs::dygraph(plot_data2, main = paste(input$ticker, "- Log-Renditen (täglich)"),
                        ylab = "Y<sub>t</sub>", xlab = "Tägliche Beobachtungen (t)", group = "FZS") %>%
        dygraphs::dyRangeSelector(height = 20) %>%
        dygraphs::dyUnzoom()
    })
    output$downloadData <- downloadHandler(
      filename = function() {
        ticker = strsplit(input$ticker, " ")[[1]][1]
        if (length(strsplit(ticker, "^", fixed = TRUE)[[1]]) != 1) {
          ticker = strsplit(ticker, "^", fixed = TRUE)[[1]][2]
        }
        paste0(ticker, ".csv")
      },
      content = function(file) {
        write.csv(dataInput(), file, row.names = FALSE)
      }
    )
    observeEvent(input$closeApp, {
      stopApp()
    })
    # shinyjs::toggle("hist")
    # observeEvent(input$histApp, {
    #   output$hist <- renderPlot({
    #     req(input$ticker)
    #     req(input$Datum[1])
    #     req(input$Datum[2])
    #     req(dataInput())
    #     dat2 = dataInput()[-1, ]
    #     ggplot(dat2, aes(x = dat2$ret.closing.price)) +
    #       geom_histogram(aes(y = ..density..), fill = "steelblue", binwidth = 0.0025, alpha = 0.2) +
    #       geom_density(aes(color = "Kernel")) +
    #       stat_function(aes(color = "Normal"), fun = dnorm,
    #                     args = list(mean = mean(dat2$ret.closing.price), sd = sd(dat2$ret.closing.price))) +
    #       theme(legend.position = c(0.115, 0.7), axis.title.y = element_blank(),
    #             panel.background = element_rect(fill = 'white', color = 'black'),
    #             panel.grid.major = element_line(color = 'darkgrey'),
    #             legend.text = element_text(size = 11),
    #             legend.title = element_text(size = 12)) +
    #       scale_colour_manual("Dichte", values = c("red", "blue")) +
    #       labs(x = expression(Y[t]))
    #   })
    #   shinyjs::toggle("hist")
    # })

    }
)
shiny::runApp(app, display.mode = 'normal')
}
