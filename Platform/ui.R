####################################################################################################
### Initialization
####################################################################################################
setwd("/home/fls/Models/Ventura/HD/Code")
source("Init.R"); library("rhandsontable"); library("DT")

####################################################################################################
### UI App
####################################################################################################
shinyUI(
  fluidPage(
    tabsetPanel(
      tabPanel("Start"),
      tabPanel(
        "Predict", 
        fluidRow(
          column(2, actionButton("Predict.Button.refresh", "Refresh", styleclass = "primary")),
          hr(),
          column(11, dataTableOutput("Predict.Table.predict")),
        )
      ),
      tabPanel(
        "Trades",
        fluidRow(
          column(2, actionButton("Trades.Button.refresh", "Refresh", styleclass = "primary")),
          hr(),
          rHandsontableOutput("Trades.Table.predict"),
          hr(),
          h4("Correlation Matrix (Trade-Adjusted)"),
          tableOutput("Trades.Table.correlations")
        )
      ),
      tabPanel(
        "Instrument", 
        column(
          2,
          wellPanel(
            fluidRow(
              selectizeInput("Instrument.Select.instrument", "Instrument", arrange(INSTRUMENTS, pair)$pair, "EURUSD", options = list(create = TRUE, maxItems = 1)),
              textInput("Instrument.Input.date_start", "From", TO_DAY %m+% months(-4) - day(TO_DAY %m+% months(-4))),
              textInput("Instrument.Input.date_end", "To", TO_DAY),
              selectInput("Instrument.Select.strategy", "Strategy", as.character(1:14), "1"),
              textOutput("Instrument.Text.Spot"),
              textOutput("Instrument.Text.Spot_Scenario")
            )
          )
        ),
        column(
          6,
          fluidRow(
            plotOutput("Instrument.Plot.priceChart")
          )
        ),
        column(
          4,
          fluidRow(
            htmlOutput("Scenario.Table.predict")
          )
        )
      ),
      tabPanel(
        "Scenario", 
        fluidRow(
          actionButton("Scenario.Button.refresh", "Refresh", styleclass = "primary"),
          dataTableOutput("Scenario.Table.dat_scenario")
        )
      ),
      tabPanel(
        "Book",
        tabsetPanel(
          tabPanel(
            "FX position", 
            fluidRow(
              column(2, actionButton("Book.FX.Button.refresh", "Refresh", styleclass = "primary")),
              hr(),
              htmlOutput("Book.FX.Table.fx_position")
            )
          ),
          tabPanel(
            "Trades Closed", 
            fluidRow(
              column(2, actionButton("Book.Trades_Closed.Button.refresh", "Refresh", styleclass = "primary"))
            ),
            hr(),
            fluidRow(
              column(9, dataTableOutput("Book.Trades_Closed.Table.trades_closed")),
              column(
                3, 
                dateInput("Book.Trades_Closed.Select.dateFrom", label = "Since", value = as.Date(paste0(substr(Sys.Date(), 1, 4), "-01-01")) ),
                plotOutput("Book.Trades_Closed.Plot.grossPnLDistribution"),
                hr(),
                htmlOutput("Book.Trades_Closed.Table.monthly_stats"))
            )
          ),
          tabPanel(
            "Trades Live", 
            fluidRow(
              column(2, actionButton("Book.Trades_Live.Button.refresh", "Refresh", styleclass = "primary")),
              hr(),
              column(10, dataTableOutput("Book.Trades_Live.Table.trades_live"))
            )
          ),
          tabPanel(
            "NAV", 
            fluidRow(
              column(2, actionButton("Book.NAV.Button.refresh", "Refresh", styleclass = "primary")),
              column(2, textInput("Book.NAV.date_start", "From", "2024-01-01"))
            ),
            fluidRow(
              column(4, plotOutput("Book.NAV.Plot.NAV")),
              column(4, plotOutput("Book.NAV.Plot.sharpe")),
              column(4, plotOutput("Book.NAV.Plot.vol"))
            ),
            hr(),
            fluidRow(
              column(4, htmlOutput("Book.NAV.Table.Strategy")),
              column(4, htmlOutput("Book.NAV.Table.Trade_Status"))

            ),
            
          )
        )
      ),
      tabPanel(
        "Diagnostics", 
        tabsetPanel(
          tabPanel(
            "Jobs", 
            fluidRow(
              column(2, dateInput("Diagnostic.Jobs.Data.diagnosticDate", label = "Date", value = Sys.Date())), 
              column(2, actionButton("Diagnostic.Jobs.Button.refresh", "Refresh", styleclass = "primary"))
            ),
            plotOutput("Diagnostic.Jobs.Plot.Utilization", height = "2000px"), 
          ),
          tabPanel(
            "Instruments", 
            fluidRow(
              column(2, actionButton("Diagnostic.Instruments.Button.refresh", "Refresh", styleclass = "primary"))
            ),
            dataTableOutput("Diagnostic.Instruments.Table.status")
          )
        )
      )
    )
  )
)
