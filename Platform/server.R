####################################################################################################
### Initialization
####################################################################################################
setwd("/home/fls/Models/Ventura/HD/Code")
source("Init.R"); library("rhandsontable"); library("DT")

shinyServer(function(input, output, session) {
  ####################################################################################################
  ### Tab 1: Predict
  ####################################################################################################
  Predict.Data.predict <- reactive({
    input$Predict.Button.refresh; input$Trades.Button.refresh; 
    TO_DAY <<- Sys.Date()
    G.Predict.Data.predict() 
    })
  
  Predict.Table.predict <- reactive(G.Predict.Table.predict(Predict.Data.predict()))
  output$Predict.Table.predict <- renderDataTable(Predict.Table.predict())

  ####################################################################################################
  ### Tab 2: Trades
  ####################################################################################################
  Trades.Table.predict <- reactive(G.Trades.Table.predict(Predict.Data.predict()))
  output$Trades.Table.predict <- renderRHandsontable(Trades.Table.predict())

  Trades.Data.predict_filtered <- reactive({
      TO_DAY <<- Sys.Date()
      dat <- Predict.Data.predict()
      edited <- input$Trades.Table.predict
      if (!is.null(edited)) {
          edited_df <- hot_to_r(edited)
          excluded_tickers <- edited_df$Ticker[edited_df$Execute == FALSE]
          dat <- dat %>% filter(!(ticker %in% excluded_tickers))
      }
      dat
  })

  Trades.Data.cor_matrix <- reactive({
      TO_DAY <<- Sys.Date()
      dat <- Trades.Data.predict_filtered()
      instrument_ids <- dat %>%
          left_join(select(INSTRUMENTS, pair, instrument_id), by = "pair") %>%
          pull(instrument_id) %>% unique()
      if (length(instrument_ids) >= 2) {
          T.calcHistoricalCorrelationsMatrix(instrument_ids = instrument_ids, shrinkage = 0)
      } else {
          NULL
      }
  })

  Trades.Data.sizing <- reactive(G.Trades.Table.sizing(Trades.Data.predict_filtered(), cor_matrix = Trades.Data.cor_matrix()))
  output$Trades.Text.n_eff <- renderText(sprintf("Effective number of bets: %.2f", Trades.Data.sizing()$n_effective))
  output$Trades.Table.sizing <- renderGvis(Trades.Data.sizing()$table)

  Trades.Table.correlations <- reactive(G.Trades.Table.correlations(Trades.Data.predict_filtered(), cor_matrix = Trades.Data.cor_matrix()))
  output$Trades.Table.correlations <- renderTable(Trades.Table.correlations())

  Trades.Table.orders <- reactive(G.Trades.Table.orders(Trades.Data.predict_filtered(), cor_matrix = Trades.Data.cor_matrix()))
  output$Trades.Table.orders <- renderRHandsontable(Trades.Table.orders())
  
  ####################################################################################################
  ### Tab 3: Instrument
  ####################################################################################################
  bump_id_0 <- D.select("SELECT bump_id FROM static_scenario_bump WHERE bump = 0")$bump_id
  
  Instrument.Plot.priceChart <- reactive(G.Instrument.Plot.technicalChart(input$Instrument.Select.instrument, input$Instrument.Input.date_start, input$Instrument.Input.date_end, input$Instrument.Select.strategy))
  output$Instrument.Plot.priceChart <- renderPlot(Instrument.Plot.priceChart(), height = 1100)
  
  Instrument.Text.Spot <- reactive({ "SELECT close FROM live_px WHERE instrument_id = %s" %>% sprintf(A.getInstrumentId(input$Instrument.Select.instrument)) %>% D.select %>% U.vectorize %>% paste0("Spot Last: ", .)})
  output$Instrument.Text.Spot <- renderText(Instrument.Text.Spot())
  
  Instrument.Text.Spot_Scenario <- reactive({ "SELECT DISTINCT timestamp_scenario, price FROM Ventura.scenario_predict WHERE instrument_id = %s AND bump_id = %s ORDER BY timestamp_scenario DESC LIMIT 1" %>% sprintf(A.getInstrumentId(input$Instrument.Select.instrument), bump_id_0) %>% D.select %>% .$price %>% paste0("Spot Scenario: ", .)})
  output$Instrument.Text.Spot_Scenario <- renderText(Instrument.Text.Spot_Scenario())
  
  ####################################################################################################
  ### Tab 4: Scenario
  ####################################################################################################
  Scenario.Table.predict <- reactive(G.Scenario.Table.instrument(input$Instrument.Select.instrument))
  output$Scenario.Table.predict <- renderGvis(Scenario.Table.predict())
  
  Scenario.Table.dat_scenario <- reactive({input$Scenario.Button.refresh; G.Scenario.Data.dat_scenario()})
  output$Scenario.Table.dat_scenario <- renderDataTable(Scenario.Table.dat_scenario())
  
  ####################################################################################################
  ### Tab 5: Book
  ####################################################################################################
  Book.Trades.Data.trades <- reactive({
    input$Book.Position.Button.refresh; input$Book.Trades_Closed.Button.refresh; input$Book.Trades_Live.Button.refresh;
    TO_DAY <<- Sys.Date()
    B.readTradesFromDB()
    })

  Book.Position.Table.etf_position <- reactive(G.Book.Position.Table.etf_position(Book.Trades.Data.trades()))
  output$Book.Position.Table.etf_position <- renderGvis(Book.Position.Table.etf_position())

  Book.Position.Table.fx_position <- reactive(G.Book.FX.Table.fx_position(Book.Trades.Data.trades()))
  output$Book.Position.Table.fx_position <- renderGvis(Book.Position.Table.fx_position())
  
  Book.Trades_Closed.Table.trades_closed <- reactive(G.Book.Trades_Closed.Table.trades_closed(Book.Trades.Data.trades(), input$Book.Trades_Closed.Select.dateFrom))
  output$Book.Trades_Closed.Table.trades_closed <- renderDataTable(Book.Trades_Closed.Table.trades_closed())
  
  Book.Trades_Closed.Plot.grossPnLDistribution <- reactive(G.Book.Trades_Closed.Plot.grossPnLDistribution(Book.Trades.Data.trades(), input$Book.Trades_Closed.Select.dateFrom))
  output$Book.Trades_Closed.Plot.grossPnLDistribution <- renderPlot(Book.Trades_Closed.Plot.grossPnLDistribution())
  
  Book.Trades_Closed.Table.monthly_stats <- reactive(G.Book.Trades_Closed.Table.monthly_stats(Book.Trades.Data.trades(), input$Book.Trades_Closed.Select.dateFrom))
  output$Book.Trades_Closed.Table.monthly_stats <- renderGvis(Book.Trades_Closed.Table.monthly_stats())
  
  Book.Trades_Live.Table.trades_live <- reactive(G.Book.Trades_Live.Table.trades_live(Book.Trades.Data.trades()))
  output$Book.Trades_Live.Table.trades_live <- renderDataTable(Book.Trades_Live.Table.trades_live())
  
  Book.NAV.data.nav <- reactive({input$Book.NAV.Button.refresh; G.Book.NAV.data.nav(as.Date("2020-06-30")) })
  Book.NAV.data.sharpe <- reactive( B.calcSharpe(Book.NAV.data.nav() ))
  Book.NAV.Plot.NAV <- reactive({ G.Book.NAV.plot(Book.NAV.data.nav(), input$Book.NAV.date_start) + ggtitle("NAV")})
  Book.NAV.Plot.sharpe <- reactive({
    Book.NAV.data.sharpe() %>%  
      filter(date >= input$Book.NAV.date_start) %>%
      ggplot(aes(x = date, y = sharpe)) + 
      geom_step(color = "lightcoral") + 
      scale_x_date() +
      scale_y_continuous(labels = scales::percent) +
      theme(
        legend.position="None",
        axis.text=element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) + 
      ggtitle("1y rolling Sharpe")
      })
  
  Book.NAV.Plot.vol <- reactive({
    Book.NAV.data.sharpe() %>%  
      filter(date >= input$Book.NAV.date_start) %>%
      ggplot(aes(x = date, y = vol_6m)) + 
      geom_step(color = "blue") + 
      scale_x_date() +
      scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
      theme(
        legend.position="None",
        axis.text=element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) + 
      ggtitle("6-month rolling vol")
  })
  
  Book.NAV.Table.Strategy <- reactive({
    dat <- Book.Trades.Data.trades()$trd_matched_grouped %>% 
      filter(date_entry >= input$Book.NAV.date_start) %>% 
      mutate(gross_pnl = (price_exit - price_entry) / (target - price_entry)) 

    dat_1 <- dat %>% 
      group_by(strategy_id) %>%
      summarize(N_Total = n(), PnL_Total = mean(gross_pnl)) %>% 
      ungroup
    
    dat_2 <- dat %>% 
      mutate(instrument_type = paste0("N_", instrument_type)) %>%
      group_by(strategy_id, instrument_type) %>%
      summarize(N = n()) %>% 
      ungroup %>% 
      spread(instrument_type, N)
    
    dat_3 <- dat %>% 
      mutate(instrument_type = paste0("PnL_", instrument_type)) %>%
      group_by(strategy_id, instrument_type) %>%
      summarize(gross_pnl = mean(gross_pnl)) %>% 
      ungroup %>% 
      spread(instrument_type, gross_pnl)
    
    dat <- dat_1 %>% 
      left_join(dat_2, by="strategy_id") %>% 
      left_join(dat_3, by="strategy_id") %>% 
      select(strategy_id, N_FX, PnL_FX, N_ETF, PnL_ETF, N_Total, PnL_Total) 
    
    dat_total <- data.frame(strategy_id = "Total")
    dat_total$N_FX <- sum(dat$N_FX, na.rm=TRUE)
    dat_total$PnL_FX = sum(dat$N_FX * dat$PnL_FX, na.rm=TRUE) / sum(dat$N_FX, na.rm=TRUE)
    dat_total$N_ETF <- sum(dat$N_ETF, na.rm=TRUE)
    dat_total$PnL_ETF = sum(dat$N_ETF * dat$PnL_ETF, na.rm=TRUE) / sum(dat$N_ETF, na.rm=TRUE)
    dat_total$N_Total <- sum(dat$N_Total, na.rm=TRUE)
    dat_total$PnL_Total = sum(dat$N_Total * dat$PnL_Total, na.rm=TRUE) / sum(dat$N_Total, na.rm=TRUE)
    rbind(dat, dat_total) %>% gvisTable
    
  })
  
  Book.NAV.Table.Trade_Status <- reactive({
    dat <- Book.Trades.Data.trades()$trd_matched_grouped %>% 
      filter(date_entry >= input$Book.NAV.date_start) 
    
    dat_1 <- dat %>% 
      group_by(strategy_id) %>%
      summarize(total = n()) %>% 
      ungroup
    
    dat_2 <- dat %>% 
      group_by(strategy_id, trade_status) %>%
      summarize(N = n()) %>% 
      ungroup %>% 
      spread(trade_status, N)
    
    
    dat <- dat_1 %>% 
      left_join(dat_2, by="strategy_id") %>% 
      select(strategy_id, total, stop, flat, target)
    
    dat_total <- data.frame(strategy_id = "Total")
    dat_total$total<- sum(dat$total, na.rm=TRUE)
    dat_total$stop<- sum(dat$stop, na.rm=TRUE)
    dat_total$flat<- sum(dat$flat, na.rm=TRUE)
    dat_total$target<- sum(dat$target, na.rm=TRUE)
    
    rbind(dat, dat_total) %>% 
      mutate(
        stop = stop / total,
        flat = flat / total,
        target = target / total
        ) %>% 
      gvisTable(formats = list(stop = "#.#%", flat = "#.#%", target = "#.#%"))
      

    
  })
  
  
  
  output$Book.NAV.Plot.NAV <- renderPlot(Book.NAV.Plot.NAV())
  output$Book.NAV.Plot.sharpe <- renderPlot(Book.NAV.Plot.sharpe())
  output$Book.NAV.Plot.vol <- renderPlot(Book.NAV.Plot.vol())
  output$Book.NAV.Table.Strategy <- renderGvis(Book.NAV.Table.Strategy())
  output$Book.NAV.Table.Trade_Status <- renderGvis(Book.NAV.Table.Trade_Status())
  ####################################################################################################
  ### Tab 6: Diagnostics
  ####################################################################################################
  Diagnostic.Jobs.Data.Activity <- reactive({ 
    input$Diagnostic.Jobs.Button.refresh; 
    TO_DAY <<- Sys.Date()
    G.Diagnostic.Jobs.Data.Activity(input$Diagnostic.Jobs.Data.diagnosticDate) 
    })
  Diagnostic.Jobs.Plot.Utilization <- reactive({ G.Diagnostic.Jobs.Plot.Utilization(Diagnostic.Jobs.Data.Activity(), input$Diagnostic.Jobs.Data.diagnosticDate) })
  output$Diagnostic.Jobs.Plot.Utilization <- renderPlot(Diagnostic.Jobs.Plot.Utilization(), height = 2000)
  
  Diagnostic.Instruments.Table.status <- reactive({input$Diagnostic.Instruments.Button.refresh; G.Diagnostic.Instruments.Table.status() })
  output$Diagnostic.Instruments.Table.status <- renderDataTable(Diagnostic.Instruments.Table.status(), height = 900)
  
  ####################################################################################################
  #### Tab 6.1 : Machine Status
  ####################################################################################################
  Diagnostic.MachineStatus.Plot.cpuLoad <- reactive({ 
    input$Diagnostic.MachineStatus.Button.refresh; 
    TO_DAY <<- Sys.Date();
    G.Diagnostic.MachineStatus.Plot.systemLoad("cpu", NULL, input$Diagnostic.MachineStatus.Button.nbDays) 
    })
  output$Diagnostic.MachineStatus.Plot.cpuLoad <- renderPlot({ Diagnostic.MachineStatus.Plot.cpuLoad() })
  
  Diagnostic.MachineStatus.Plot.memoryLoad <- reactive({ input$Diagnostic.MachineStatus.Button.refresh; G.Diagnostic.MachineStatus.Plot.systemLoad("memory", NULL, input$Diagnostic.MachineStatus.Button.nbDays) })
  output$Diagnostic.MachineStatus.Plot.memoryLoad <- renderPlot({ Diagnostic.MachineStatus.Plot.memoryLoad() })
  
  Diagnostic.MachineStatus.Plot.swapLoad <- reactive({ input$Diagnostic.MachineStatus.Button.refresh; G.Diagnostic.MachineStatus.Plot.systemLoad("swap", "mem", input$Diagnostic.MachineStatus.Button.nbDays) })
  output$Diagnostic.MachineStatus.Plot.swapLoad <- renderPlot({ Diagnostic.MachineStatus.Plot.swapLoad() })
  
  Diagnostic.MachineStatus.Plot.headLoad <- reactive({ input$Diagnostic.MachineStatus.Button.refresh; G.Diagnostic.MachineStatus.Plot.systemLoad("heat", NULL, input$Diagnostic.MachineStatus.Button.nbDays) })
  output$Diagnostic.MachineStatus.Plot.headLoad <- renderPlot({ Diagnostic.MachineStatus.Plot.headLoad() })
  
  
  Diagnostic.MachineStatus.Plot.dbCpuLoad <- reactive({ input$Diagnostic.MachineStatus.Button.refresh; G.Diagnostic.MachineStatus.Plot.systemLoad("sql", "cpu", input$Diagnostic.MachineStatus.Button.nbDays) })
  output$Diagnostic.MachineStatus.Plot.dbCpuLoad <- renderPlot({ Diagnostic.MachineStatus.Plot.dbCpuLoad() })
  
  Diagnostic.MachineStatus.Plot.dbMemoryLoad <- reactive({ input$Diagnostic.MachineStatus.Button.refresh; G.Diagnostic.MachineStatus.Plot.systemLoad("sql", "mem", input$Diagnostic.MachineStatus.Button.nbDays) })
  output$Diagnostic.MachineStatus.Plot.dbMemoryLoad <- renderPlot({ Diagnostic.MachineStatus.Plot.dbMemoryLoad() })
  
  Diagnostic.MachineStatus.Plot.dbConnectionsLoad <- reactive({ input$Diagnostic.MachineStatus.Button.refresh; G.Diagnostic.MachineStatus.Plot.systemLoad("conn", NULL, input$Diagnostic.MachineStatus.Button.nbDays) })
  output$Diagnostic.MachineStatus.Plot.dbConnectionsLoad <- renderPlot({ Diagnostic.MachineStatus.Plot.dbConnectionsLoad() })
  
  Diagnostic.MachineStatus.Plot.dbSizeLoad <- reactive({ input$Diagnostic.MachineStatus.Button.refresh; G.Diagnostic.MachineStatus.Plot.systemLoad("dbsize", NULL, input$Diagnostic.MachineStatus.Button.nbDays) })
  output$Diagnostic.MachineStatus.Plot.dbSizeLoad <- renderPlot({ Diagnostic.MachineStatus.Plot.dbSizeLoad() })
  
  Diagnostic.MachineStatus.Plot.ibCpuLoad <- reactive({ input$Diagnostic.MachineStatus.Button.refresh; G.Diagnostic.MachineStatus.Plot.systemLoad("ib", "cpu", input$Diagnostic.MachineStatus.Button.nbDays) })
  output$Diagnostic.MachineStatus.Plot.ibCpuLoad <- renderPlot({ Diagnostic.MachineStatus.Plot.ibCpuLoad() })
  
  Diagnostic.MachineStatus.Plot.ibMemoryLoad <- reactive({ input$Diagnostic.MachineStatus.Button.refresh; G.Diagnostic.MachineStatus.Plot.systemLoad("ib", "mem", input$Diagnostic.MachineStatus.Button.nbDays) })
  output$Diagnostic.MachineStatus.Plot.ibMemoryLoad <- renderPlot({ Diagnostic.MachineStatus.Plot.ibMemoryLoad() })
  
  Diagnostic.MachineStatus.Plot.RCpuLoad <- reactive({ input$Diagnostic.MachineStatus.Button.refresh; G.Diagnostic.MachineStatus.Plot.systemLoad("R", "cpu", input$Diagnostic.MachineStatus.Button.nbDays) })
  output$Diagnostic.MachineStatus.Plot.RCpuLoad <- renderPlot({ Diagnostic.MachineStatus.Plot.RCpuLoad() })
  
  Diagnostic.MachineStatus.Plot.RMemoryLoad <- reactive({ input$Diagnostic.MachineStatus.Button.refresh; G.Diagnostic.MachineStatus.Plot.systemLoad("R", "mem", input$Diagnostic.MachineStatus.Button.nbDays) })
  output$Diagnostic.MachineStatus.Plot.RMemoryLoad <- renderPlot({ Diagnostic.MachineStatus.Plot.RMemoryLoad() })
  
  ####################################################################################################
  ### End
  ####################################################################################################
})

