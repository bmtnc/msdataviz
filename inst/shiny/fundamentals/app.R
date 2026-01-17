# Fundamentals Explorer Shiny App

# Load the package
library(msdataviz)

# Helper: convert uppercase/snake_case to Title Case
to_title_case <- function(x) {
  x %>%
    tolower() %>%
    gsub("_", " ", .) %>%
    gsub("(^|\\s)([a-z])", "\\1\\U\\2", ., perl = TRUE)
}

# Load artifacts on startup (cached)
artifacts <- get_cached_artifacts()

# Get unique tickers with sector/subsector/industry for search
ticker_list <- artifacts$ttm_data %>%
  dplyr::distinct(ticker, sector, subsector, industry) %>%
  dplyr::arrange(ticker)

ticker_choices <- stats::setNames(ticker_list$ticker, ticker_list$ticker)

# UI
ui <- shiny::fluidPage(
  shiny::titlePanel("Fundamentals Explorer"),

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 2,
      shiny::selectizeInput(
        inputId = "ticker",
        label = "Search Ticker",
        choices = ticker_choices,
        options = list(
          placeholder = "Type to search...",
          maxOptions = 50,
          # Custom score function to prioritize exact and prefix matches
          score = I("function(search) {
            var token = search.toLowerCase();
            return function(item) {
              var text = String(item.text || item.value).toLowerCase();
              if (text === token) return 10000;
              if (text.indexOf(token) === 0) return 1000 + (100 - text.length);
              if (text.indexOf(token) > -1) return 100 - text.indexOf(token);
              return 0;
            };
          }")
        )
      ),
      shiny::hr(),
      shiny::uiOutput("company_info"),
      shiny::hr(),
      shiny::h5("KPI Peer Universe"),
      shiny::radioButtons(
        inputId = "peer_universe",
        label = NULL,
        choices = c(
          "Market" = "market",
          "Sector" = "sector",
          "Subsector" = "subsector",
          "Industry" = "industry",
          "None" = "none"
        ),
        selected = "subsector"
      ),
      shiny::hr(),
      shiny::h5("Date Range"),
      shiny::numericInput(
        inputId = "lookback_days",
        label = "Lookback (days)",
        value = 3650,
        min = 365,
        max = 7300,
        step = 365
      )
    ),

    shiny::mainPanel(
      width = 10,
      shiny::tabsetPanel(
        id = "main_tabs",

        # Price Tab
        shiny::tabPanel(
          "Price",
          shiny::h3("TSR Decomposition"),
          shiny::plotOutput("tsr_plot", height = "500px"),
          shiny::hr(),
          shiny::h3("Drawdown"),
          shiny::plotOutput("drawdown_plot", height = "400px")
        ),

        # Financials Tab
        shiny::tabPanel(
          "Financials",
          shiny::tabsetPanel(
            # Income Statement
            shiny::tabPanel(
              "Income Statement",
              shiny::tabsetPanel(
                shiny::tabPanel("Revenue", shiny::plotOutput("revenue_plot", height = "500px")),
                shiny::tabPanel("Gross Profit", shiny::plotOutput("gross_profit_plot", height = "500px")),
                shiny::tabPanel("EBIT", shiny::plotOutput("ebit_plot", height = "500px")),
                shiny::tabPanel("EBITDA", shiny::plotOutput("ebitda_plot", height = "500px")),
                shiny::tabPanel("NOPAT", shiny::plotOutput("nopat_plot", height = "500px")),
                shiny::tabPanel("Interest Income", shiny::plotOutput("interest_income_plot", height = "500px")),
                shiny::tabPanel("Interest Expense", shiny::plotOutput("interest_expense_plot", height = "500px")),
                shiny::tabPanel("Net Interest Income", shiny::plotOutput("net_interest_income_plot", height = "500px")),
                shiny::tabPanel("Net Income", shiny::plotOutput("net_income_plot", height = "500px"))
              )
            ),
            # Cash Flow
            shiny::tabPanel(
              "Cash Flow",
              shiny::tabsetPanel(
                shiny::tabPanel("Operating Cash Flow", shiny::plotOutput("ocf_plot", height = "500px")),
                shiny::tabPanel("Free Cash Flow", shiny::plotOutput("fcf_plot", height = "500px")),
                shiny::tabPanel("CapEx", shiny::plotOutput("capex_plot", height = "500px")),
                shiny::tabPanel("Dividends", shiny::plotOutput("dividends_plot", height = "500px")),
                shiny::tabPanel("Buybacks", shiny::plotOutput("buybacks_plot", height = "500px")),
                shiny::tabPanel("Total Capital Returned", shiny::plotOutput("total_capital_returned_plot", height = "500px"))
              )
            ),
            # Balance Sheet
            shiny::tabPanel(
              "Balance Sheet",
              shiny::tabsetPanel(
                shiny::tabPanel("Total Assets", shiny::plotOutput("assets_plot", height = "500px")),
                shiny::tabPanel("Cash", shiny::plotOutput("cash_plot", height = "500px")),
                shiny::tabPanel("Total Debt", shiny::plotOutput("debt_plot", height = "500px")),
                shiny::tabPanel("Shareholder Equity", shiny::plotOutput("equity_plot", height = "500px")),
                shiny::tabPanel("Shares Outstanding", shiny::plotOutput("shares_plot", height = "500px"))
              )
            ),
            # Per-Share Growth
            shiny::tabPanel(
              "Per-Share Growth",
              shiny::tabsetPanel(
                shiny::tabPanel("Revenue", shiny::plotOutput("ps_revenue_plot", height = "500px")),
                shiny::tabPanel("Gross Profit", shiny::plotOutput("ps_gross_profit_plot", height = "500px")),
                shiny::tabPanel("EBIT", shiny::plotOutput("ps_ebit_plot", height = "500px")),
                shiny::tabPanel("EBITDA", shiny::plotOutput("ps_ebitda_plot", height = "500px")),
                shiny::tabPanel("NOPAT", shiny::plotOutput("ps_nopat_plot", height = "500px")),
                shiny::tabPanel("FCF", shiny::plotOutput("ps_fcf_plot", height = "500px")),
                shiny::tabPanel("Book Value", shiny::plotOutput("ps_bv_plot", height = "500px"))
              )
            ),
            # KPIs
            shiny::tabPanel(
              "KPIs",
              shiny::tabsetPanel(
                shiny::tabPanel("Margins", shiny::plotOutput("kpi_margins_plot", height = "500px")),
                shiny::tabPanel("ROIC", shiny::plotOutput("kpi_roic_plot", height = "500px")),
                shiny::tabPanel("GROIC", shiny::plotOutput("kpi_groic_plot", height = "500px")),
                shiny::tabPanel("ROE", shiny::plotOutput("kpi_roe_plot", height = "500px")),
                shiny::tabPanel("FCF Conversion", shiny::plotOutput("kpi_fcf_conversion_plot", height = "500px")),
                shiny::tabPanel("Cost of Debt", shiny::plotOutput("kpi_cost_of_debt_plot", height = "500px")),
                shiny::tabPanel("Interest Coverage", shiny::plotOutput("kpi_interest_coverage_plot", height = "500px")),
                shiny::tabPanel("Leverage", shiny::plotOutput("kpi_leverage_plot", height = "500px"))
              )
            )
          )
        ),

        # Valuation Tab
        shiny::tabPanel(
          "Valuation",
          shiny::tabsetPanel(
            shiny::tabPanel("P/S", shiny::plotOutput("val_ps_plot", height = "500px")),
            shiny::tabPanel("P/B", shiny::plotOutput("val_pb_plot", height = "500px")),
            shiny::tabPanel("P/Gross Profit", shiny::plotOutput("val_pgp_plot", height = "500px")),
            shiny::tabPanel("P/EBIT", shiny::plotOutput("val_pebit_plot", height = "500px")),
            shiny::tabPanel("P/E", shiny::plotOutput("val_pe_plot", height = "500px")),
            shiny::tabPanel("P/FCF", shiny::plotOutput("val_pfcf_plot", height = "500px")),
            shiny::tabPanel("EV/EBITDA", shiny::plotOutput("val_ev_ebitda_plot", height = "500px")),
            shiny::tabPanel("EV/NOPAT", shiny::plotOutput("val_ev_nopat_plot", height = "500px")),
            shiny::tabPanel("Shareholder Yield", shiny::plotOutput("val_yield_plot", height = "500px"))
          )
        ),

        # DuPont Tab
        shiny::tabPanel(
          "DuPont",
          shiny::tabsetPanel(
            shiny::tabPanel("ROIC", shiny::plotOutput("roic_plot", height = "550px")),
            shiny::tabPanel("ROE", shiny::plotOutput("roe_plot", height = "550px"))
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Set ggplot theme
  set_ggplot_theme()

  # Reactive: start date based on lookback days
  start_date <- shiny::reactive({
    shiny::req(input$lookback_days)
    Sys.Date() - input$lookback_days
  })

  # Reactive: selected ticker info
  selected_info <- shiny::reactive({
    shiny::req(input$ticker)
    ticker_list %>%
      dplyr::filter(ticker == input$ticker)
  })

  # Reactive: peer label for charts (dynamic based on actual group name)
  peer_label <- shiny::reactive({
    shiny::req(input$peer_universe)
    if (input$peer_universe == "none") {
      return(NULL)
    }
    if (input$peer_universe == "market") {
      return("Market Median")
    }
    shiny::req(input$ticker)
    info <- selected_info()
    group_name <- to_title_case(info[[input$peer_universe]])
    universe_type <- to_title_case(input$peer_universe)
    paste0(group_name, " ", universe_type, " Median")
  })

  # Company info sidebar
  output$company_info <- shiny::renderUI({
    shiny::req(input$ticker)
    info <- selected_info()
    shiny::tagList(
      shiny::h4(input$ticker),
      shiny::p(shiny::strong("Sector: "), to_title_case(info$sector)),
      shiny::p(shiny::strong("Subsector: "), to_title_case(info$subsector)),
      shiny::p(shiny::strong("Industry: "), to_title_case(info$industry))
    )
  })

  # Reactive: prepared fundamentals data
  fundamentals_data <- shiny::reactive({
    shiny::req(input$ticker, start_date())
    prepare_fundamentals_data(
      ticker = input$ticker,
      ttm_data = artifacts$ttm_data,
      start_date = start_date()
    )
  })

  # Reactive: TTM data with calculations for per-share decomposition
  ttm_with_calcs <- shiny::reactive({
    shiny::req(input$ticker)
    artifacts$ttm_data %>%
      dplyr::filter(ticker == input$ticker) %>%
      dplyr::mutate(
        nopat = calculate_nopat(
          ebit_ttm,
          depreciationAndAmortization_ttm,
          depreciation_ttm
        ),
        fcf = calculate_fcf(operatingCashflow_ttm, capitalExpenditures_ttm)
      )
  })

  # Reactive: KPI data for financial ratio charts
  kpi_data <- shiny::reactive({
    shiny::req(input$ticker)
    fund_data <- fundamentals_data()
    if (!is.null(fund_data) && nrow(fund_data) > 0) {
      prepare_kpi_data(fund_data)
    }
  })

  # Reactive: Universe KPIs (all tickers, recomputed when date range changes)
  universe_kpis <- shiny::reactive({
    shiny::req(start_date())
    prepare_universe_kpis(artifacts$ttm_data, start_date())
  })

  # Reactive: Peer medians based on selected peer universe
  # Groups by calendar_quarter_ending (not fiscalDateEnding) since companies have different fiscal calendars
  peer_medians <- shiny::reactive({
    shiny::req(input$ticker, input$peer_universe)

    # Return NULL if "none" selected
    if (input$peer_universe == "none") {
      return(NULL)
    }

    data <- universe_kpis()

    # Filter to peer group (unless "market" which uses all tickers)
    if (input$peer_universe != "market") {
      info <- selected_info()
      peer_group <- info[[input$peer_universe]]
      data <- data %>%
        dplyr::filter(.data[[input$peer_universe]] == peer_group)
    }

    # Calculate medians by calendar quarter (normalized quarter-end dates)
    data %>%
      dplyr::group_by(calendar_quarter_ending) %>%
      dplyr::summarize(
        peer_roic = median(roic, na.rm = TRUE),
        peer_groic = median(groic, na.rm = TRUE),
        peer_roe = median(roe, na.rm = TRUE),
        peer_fcf_conversion = median(fcf_conversion, na.rm = TRUE),
        peer_cost_of_debt = median(cost_of_debt, na.rm = TRUE),
        peer_interest_coverage = median(interest_coverage, na.rm = TRUE),
        peer_debt_to_ebitda = median(debt_to_ebitda, na.rm = TRUE),
        n_peers = dplyr::n_distinct(ticker),
        .groups = "drop"
      )
  })

  # Reactive: KPI data with peer medians joined (or just KPI data if "none" selected)
  kpi_data_with_peers <- shiny::reactive({
    shiny::req(kpi_data())
    kpi <- kpi_data()

    peers <- peer_medians()
    if (is.null(peers)) {
      return(kpi)
    }

    kpi %>%
      dplyr::left_join(peers, by = "calendar_quarter_ending")
  })

  # === Valuation Data ===

  # Reactive: Valuation data for ticker (daily frequency)
  valuation_data <- shiny::reactive({
    shiny::req(input$ticker, start_date())
    prepare_valuation_multiples_data(
      ticker = input$ticker,
      price_data = artifacts$price_data,
      ttm_data = artifacts$ttm_data,
      start_date = start_date()
    )
  })

  # Reactive: Universe valuations (all tickers, daily, recomputed when date range changes)
  universe_valuations <- shiny::reactive({
    shiny::req(start_date())
    prepare_universe_valuations_daily(
      artifacts$ttm_data,
      artifacts$price_data,
      start_date()
    )
  })

  # Reactive: Valuation peer medians based on selected peer universe (daily frequency)
  valuation_peer_medians <- shiny::reactive({
    shiny::req(input$ticker, input$peer_universe)

    # Return NULL if "none" selected
    if (input$peer_universe == "none") {
      return(NULL)
    }

    data <- universe_valuations()

    # Filter to peer group (unless "market" which uses all tickers)
    if (input$peer_universe != "market") {
      info <- selected_info()
      peer_group <- info[[input$peer_universe]]
      data <- data %>%
        dplyr::filter(.data[[input$peer_universe]] == peer_group)
    }

    # Calculate medians by date (daily frequency)
    data %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(
        peer_price_to_sales = median(price_to_sales, na.rm = TRUE),
        peer_price_to_book = median(price_to_book, na.rm = TRUE),
        peer_price_to_gross_profit = median(price_to_gross_profit, na.rm = TRUE),
        peer_price_to_ebit = median(price_to_ebit, na.rm = TRUE),
        peer_price_to_earnings = median(price_to_earnings, na.rm = TRUE),
        peer_price_to_fcf = median(price_to_fcf, na.rm = TRUE),
        peer_ev_to_ebitda = median(ev_to_ebitda, na.rm = TRUE),
        peer_ev_to_nopat = median(ev_to_nopat, na.rm = TRUE),
        peer_dividend_yield = median(dividend_yield, na.rm = TRUE),
        peer_buyback_yield = median(buyback_yield, na.rm = TRUE),
        peer_shareholder_yield = median(shareholder_yield, na.rm = TRUE),
        n_peers = dplyr::n_distinct(ticker),
        .groups = "drop"
      )
  })

  # Reactive: Valuation data with peer medians joined (or just valuation data if "none" selected)
  valuation_data_with_peers <- shiny::reactive({
    shiny::req(valuation_data())
    val <- valuation_data()

    peers <- valuation_peer_medians()
    if (is.null(peers)) {
      return(val)
    }

    val %>%
      dplyr::left_join(peers, by = "date")
  })

  # Helper: safe bar plot
  safe_bar_plot <- function(data, metric_col, ticker, title) {
    prepped <- data %>%
      dplyr::select(fiscalDateEnding, value = dplyr::all_of(metric_col)) %>%
      dplyr::filter(!is.na(value))
    if (nrow(prepped) > 0) {
      plot_fundamental_bar(prepped, ticker, title)
    }
  }

  # === Price Charts ===

  # TSR Decomposition
  output$tsr_plot <- shiny::renderPlot({
    shiny::req(input$ticker, start_date())
    ticker <- input$ticker
    sd <- start_date()

    price_data <- artifacts$price_data %>%
      dplyr::filter(ticker == !!ticker, date >= sd) %>%
      dplyr::select(date, adjusted_close, close, split_coefficient) %>%
      dplyr::filter(
        !is.na(adjusted_close), adjusted_close > 0,
        !is.na(close), close > 0
      ) %>%
      dplyr::arrange(date)

    quarterly_shares <- ttm_with_calcs() %>%
      dplyr::select(date = fiscalDateEnding, shares = commonStockSharesOutstanding) %>%
      dplyr::filter(!is.na(shares), shares > 0) %>%
      dplyr::arrange(date)

    tsr_input <- price_data %>%
      dplyr::left_join(quarterly_shares, by = "date") %>%
      tidyr::fill(shares, .direction = "down") %>%
      dplyr::filter(!is.na(shares))

    if (nrow(tsr_input) > 1) {
      base_date <- min(tsr_input$date)
      tsr_data <- calculate_tsr_decomposition(tsr_input, base_date = base_date)
      plot_tsr_decomposition(tsr_data, ticker, base_date)
    }
  })

  # Drawdown
  output$drawdown_plot <- shiny::renderPlot({
    shiny::req(input$ticker, start_date())
    ticker <- input$ticker
    sd <- start_date()

    drawdown_data <- artifacts$price_data %>%
      dplyr::filter(ticker == !!ticker, date >= sd) %>%
      dplyr::arrange(date) %>%
      dplyr::select(date, price = adjusted_close) %>%
      dplyr::filter(!is.na(price), price > 0) %>%
      dplyr::mutate(drawdown = drawdown_from_high(price))

    if (nrow(drawdown_data) > 0) {
      plot_drawdown(drawdown_data, ticker, show_anomalies = FALSE)
    }
  })

  # === Income Statement ===

  output$revenue_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    safe_bar_plot(fundamentals_data(), "revenue", input$ticker, "Revenue (TTM)")
  })

  output$gross_profit_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    safe_bar_plot(fundamentals_data(), "gross_profit", input$ticker, "Gross Profit (TTM)")
  })

  output$ebit_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    safe_bar_plot(fundamentals_data(), "ebit", input$ticker, "EBIT (TTM)")
  })

  output$ebitda_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    safe_bar_plot(fundamentals_data(), "ebitda", input$ticker, "EBITDA (TTM)")
  })

  output$nopat_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    safe_bar_plot(fundamentals_data(), "nopat", input$ticker, "NOPAT (TTM)")
  })

  output$interest_income_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    safe_bar_plot(fundamentals_data(), "interest_income", input$ticker, "Interest Income (TTM)")
  })

  output$interest_expense_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    safe_bar_plot(fundamentals_data(), "interest_expense", input$ticker, "Interest Expense (TTM)")
  })

  output$net_interest_income_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    safe_bar_plot(fundamentals_data(), "net_interest_income", input$ticker, "Net Interest Income (TTM)")
  })

  output$net_income_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    safe_bar_plot(fundamentals_data(), "net_income", input$ticker, "Net Income (TTM)")
  })

  # === Cash Flow ===

  output$ocf_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    safe_bar_plot(fundamentals_data(), "operating_cashflow", input$ticker, "Operating Cash Flow (TTM)")
  })

  output$fcf_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    safe_bar_plot(fundamentals_data(), "fcf", input$ticker, "Free Cash Flow (TTM)")
  })

  output$capex_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    safe_bar_plot(fundamentals_data(), "capex", input$ticker, "Capital Expenditures (TTM)")
  })

  output$dividends_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    safe_bar_plot(fundamentals_data(), "dividends", input$ticker, "Dividends (TTM)")
  })

  output$buybacks_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    safe_bar_plot(fundamentals_data(), "buybacks", input$ticker, "Share Buybacks (TTM)")
  })

  output$total_capital_returned_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    safe_bar_plot(fundamentals_data(), "total_capital_returned", input$ticker, "Total Capital Returned (TTM)")
  })

  # === Balance Sheet ===

  output$assets_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    safe_bar_plot(fundamentals_data(), "total_assets", input$ticker, "Total Assets")
  })

  output$cash_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    safe_bar_plot(fundamentals_data(), "cash", input$ticker, "Cash & Short-Term Investments")
  })

  output$debt_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    safe_bar_plot(fundamentals_data(), "total_debt", input$ticker, "Total Debt")
  })

  output$equity_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    safe_bar_plot(fundamentals_data(), "shareholder_equity", input$ticker, "Shareholder Equity")
  })

  output$shares_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    safe_bar_plot(fundamentals_data(), "shares_outstanding", input$ticker, "Shares Outstanding")
  })

  # === Per-Share Growth ===

  output$ps_revenue_plot <- shiny::renderPlot({
    shiny::req(input$ticker, start_date())
    data <- prepare_per_share_decomposition_data(
      ttm_with_calcs(), input$ticker, "totalRevenue_ttm",
      start_date()
    )
    if (!is.null(data)) {
      plot_share_count_decomposition(data, input$ticker, "Revenue")
    }
  })

  output$ps_gross_profit_plot <- shiny::renderPlot({
    shiny::req(input$ticker, start_date())
    data <- prepare_per_share_decomposition_data(
      ttm_with_calcs(), input$ticker, "grossProfit_ttm",
      start_date()
    )
    if (!is.null(data)) {
      plot_share_count_decomposition(data, input$ticker, "Gross Profit")
    }
  })

  output$ps_ebit_plot <- shiny::renderPlot({
    shiny::req(input$ticker, start_date())
    data <- prepare_per_share_decomposition_data(
      ttm_with_calcs(), input$ticker, "ebit_ttm",
      start_date()
    )
    if (!is.null(data)) {
      plot_share_count_decomposition(data, input$ticker, "EBIT")
    }
  })

  output$ps_ebitda_plot <- shiny::renderPlot({
    shiny::req(input$ticker, start_date())
    data <- prepare_per_share_decomposition_data(
      ttm_with_calcs(), input$ticker, "ebitda_ttm",
      start_date()
    )
    if (!is.null(data)) {
      plot_share_count_decomposition(data, input$ticker, "EBITDA")
    }
  })

  output$ps_nopat_plot <- shiny::renderPlot({
    shiny::req(input$ticker, start_date())
    data <- prepare_per_share_decomposition_data(
      ttm_with_calcs(), input$ticker, "nopat",
      start_date()
    )
    if (!is.null(data)) {
      plot_share_count_decomposition(data, input$ticker, "NOPAT")
    }
  })

  output$ps_fcf_plot <- shiny::renderPlot({
    shiny::req(input$ticker, start_date())
    data <- prepare_per_share_decomposition_data(
      ttm_with_calcs(), input$ticker, "fcf",
      start_date()
    )
    if (!is.null(data)) {
      plot_share_count_decomposition(data, input$ticker, "FCF")
    }
  })

  output$ps_bv_plot <- shiny::renderPlot({
    shiny::req(input$ticker, start_date())
    data <- prepare_per_share_decomposition_data(
      ttm_with_calcs(), input$ticker, "totalShareholderEquity",
      start_date()
    )
    if (!is.null(data)) {
      plot_share_count_decomposition(data, input$ticker, "Book Value")
    }
  })

  # === KPIs ===

  output$kpi_margins_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    data <- kpi_data()
    if (!is.null(data) && nrow(data) > 0) {
      plot_financial_ratio(
        data = data,
        ratio_cols = c("gross_margin", "operating_margin", "net_margin"),
        labels = c("Gross Margin", "Operating Margin", "Net Margin"),
        colors = c("steelblue", "darkgreen", "navy"),
        y_format = "percent",
        y_label = "Margin",
        ticker = input$ticker,
        title_suffix = "Profit Margins"
      )
    }
  })

  output$kpi_roic_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    data <- kpi_data_with_peers()
    if (!is.null(data) && nrow(data) > 0) {
      pl <- peer_label()
      plot_financial_ratio(
        data = data,
        ratio_cols = "roic",
        labels = "ROIC",
        colors = "navy",
        y_format = "percent",
        y_label = "NOPAT / IC",
        ticker = input$ticker,
        title_suffix = "ROIC",
        peer_col = if (!is.null(pl)) "peer_roic" else NULL,
        peer_label = pl,
        n_peers = if (!is.null(pl)) data$n_peers[1] else NULL
      )
    }
  })

  output$kpi_groic_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    data <- kpi_data_with_peers()
    if (!is.null(data) && nrow(data) > 0) {
      pl <- peer_label()
      plot_financial_ratio(
        data = data,
        ratio_cols = "groic",
        labels = "GROIC",
        colors = "steelblue",
        y_format = "percent",
        y_label = "Gross Profit / IC",
        ticker = input$ticker,
        title_suffix = "GROIC",
        peer_col = if (!is.null(pl)) "peer_groic" else NULL,
        peer_label = pl,
        n_peers = if (!is.null(pl)) data$n_peers[1] else NULL
      )
    }
  })

  output$kpi_roe_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    data <- kpi_data_with_peers()
    if (!is.null(data) && nrow(data) > 0) {
      pl <- peer_label()
      plot_financial_ratio(
        data = data,
        ratio_cols = "roe",
        labels = "ROE",
        colors = "darkgreen",
        y_format = "percent",
        y_label = "Net Income / Equity",
        ticker = input$ticker,
        title_suffix = "ROE",
        peer_col = if (!is.null(pl)) "peer_roe" else NULL,
        peer_label = pl,
        n_peers = if (!is.null(pl)) data$n_peers[1] else NULL
      )
    }
  })

  output$kpi_fcf_conversion_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    data <- kpi_data_with_peers()
    if (!is.null(data) && nrow(data) > 0) {
      pl <- peer_label()
      plot_financial_ratio(
        data = data,
        ratio_cols = "fcf_conversion",
        labels = "FCF Conversion",
        colors = "steelblue",
        y_format = "percent",
        y_label = "FCF / NOPAT",
        ticker = input$ticker,
        title_suffix = "FCF Conversion",
        peer_col = if (!is.null(pl)) "peer_fcf_conversion" else NULL,
        peer_label = pl,
        n_peers = if (!is.null(pl)) data$n_peers[1] else NULL
      )
    }
  })

  output$kpi_cost_of_debt_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    data <- kpi_data_with_peers()
    if (!is.null(data) && nrow(data) > 0) {
      pl <- peer_label()
      plot_financial_ratio(
        data = data,
        ratio_cols = "cost_of_debt",
        labels = "Cost of Debt",
        colors = "steelblue",
        y_format = "percent",
        y_label = "Interest / Avg Debt",
        ticker = input$ticker,
        title_suffix = "Cost of Debt",
        peer_col = if (!is.null(pl)) "peer_cost_of_debt" else NULL,
        peer_label = pl,
        n_peers = if (!is.null(pl)) data$n_peers[1] else NULL
      )
    }
  })

  output$kpi_interest_coverage_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    data <- kpi_data_with_peers()
    if (!is.null(data) && nrow(data) > 0) {
      pl <- peer_label()
      plot_financial_ratio(
        data = data,
        ratio_cols = "interest_coverage",
        labels = "Interest Coverage",
        colors = "steelblue",
        y_format = "turns",
        y_label = "EBIT / Interest",
        ticker = input$ticker,
        title_suffix = "Interest Coverage",
        peer_col = if (!is.null(pl)) "peer_interest_coverage" else NULL,
        peer_label = pl,
        n_peers = if (!is.null(pl)) data$n_peers[1] else NULL
      )
    }
  })

  output$kpi_leverage_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    data <- kpi_data_with_peers()
    if (!is.null(data) && nrow(data) > 0) {
      pl <- peer_label()
      plot_financial_ratio(
        data = data,
        ratio_cols = "debt_to_ebitda",
        labels = "Debt / EBITDA",
        colors = "steelblue",
        y_format = "turns",
        y_label = "Debt / EBITDA",
        ticker = input$ticker,
        title_suffix = "Leverage",
        peer_col = if (!is.null(pl)) "peer_debt_to_ebitda" else NULL,
        peer_label = pl,
        n_peers = if (!is.null(pl)) data$n_peers[1] else NULL
      )
    }
  })

  # === Valuation Charts ===

  output$val_ps_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    data <- valuation_data_with_peers()
    if (!is.null(data) && nrow(data) > 0) {
      pl <- peer_label()
      plot_financial_ratio(
        data = data,
        ratio_cols = "price_to_sales",
        labels = "P/S",
        colors = "steelblue",
        y_format = "turns",
        y_label = "Price / Revenue",
        ticker = input$ticker,
        title_suffix = "Price to Sales",
        peer_col = if (!is.null(pl)) "peer_price_to_sales" else NULL,
        peer_label = pl,
        n_peers = if (!is.null(pl)) data$n_peers[1] else NULL
      )
    }
  })

  output$val_pb_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    data <- valuation_data_with_peers()
    if (!is.null(data) && nrow(data) > 0) {
      pl <- peer_label()
      plot_financial_ratio(
        data = data,
        ratio_cols = "price_to_book",
        labels = "P/B",
        colors = "steelblue",
        y_format = "turns",
        y_label = "Price / Book Value",
        ticker = input$ticker,
        title_suffix = "Price to Book",
        peer_col = if (!is.null(pl)) "peer_price_to_book" else NULL,
        peer_label = pl,
        n_peers = if (!is.null(pl)) data$n_peers[1] else NULL
      )
    }
  })

  output$val_pgp_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    data <- valuation_data_with_peers()
    if (!is.null(data) && nrow(data) > 0) {
      pl <- peer_label()
      plot_financial_ratio(
        data = data,
        ratio_cols = "price_to_gross_profit",
        labels = "P/GP",
        colors = "steelblue",
        y_format = "turns",
        y_label = "Price / Gross Profit",
        ticker = input$ticker,
        title_suffix = "Price to Gross Profit",
        peer_col = if (!is.null(pl)) "peer_price_to_gross_profit" else NULL,
        peer_label = pl,
        n_peers = if (!is.null(pl)) data$n_peers[1] else NULL
      )
    }
  })

  output$val_pebit_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    data <- valuation_data_with_peers()
    if (!is.null(data) && nrow(data) > 0) {
      pl <- peer_label()
      plot_financial_ratio(
        data = data,
        ratio_cols = "price_to_ebit",
        labels = "P/EBIT",
        colors = "steelblue",
        y_format = "turns",
        y_label = "Price / EBIT",
        ticker = input$ticker,
        title_suffix = "Price to EBIT",
        peer_col = if (!is.null(pl)) "peer_price_to_ebit" else NULL,
        peer_label = pl,
        n_peers = if (!is.null(pl)) data$n_peers[1] else NULL
      )
    }
  })

  output$val_pe_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    data <- valuation_data_with_peers()
    if (!is.null(data) && nrow(data) > 0) {
      pl <- peer_label()
      plot_financial_ratio(
        data = data,
        ratio_cols = "price_to_earnings",
        labels = "P/E",
        colors = "steelblue",
        y_format = "turns",
        y_label = "Price / Earnings",
        ticker = input$ticker,
        title_suffix = "Price to Earnings",
        peer_col = if (!is.null(pl)) "peer_price_to_earnings" else NULL,
        peer_label = pl,
        n_peers = if (!is.null(pl)) data$n_peers[1] else NULL
      )
    }
  })

  output$val_pfcf_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    data <- valuation_data_with_peers()
    if (!is.null(data) && nrow(data) > 0) {
      pl <- peer_label()
      plot_financial_ratio(
        data = data,
        ratio_cols = "price_to_fcf",
        labels = "P/FCF",
        colors = "steelblue",
        y_format = "turns",
        y_label = "Price / FCF",
        ticker = input$ticker,
        title_suffix = "Price to Free Cash Flow",
        peer_col = if (!is.null(pl)) "peer_price_to_fcf" else NULL,
        peer_label = pl,
        n_peers = if (!is.null(pl)) data$n_peers[1] else NULL
      )
    }
  })

  output$val_ev_ebitda_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    data <- valuation_data_with_peers()
    if (!is.null(data) && nrow(data) > 0) {
      pl <- peer_label()
      plot_financial_ratio(
        data = data,
        ratio_cols = "ev_to_ebitda",
        labels = "EV/EBITDA",
        colors = "navy",
        y_format = "turns",
        y_label = "EV / EBITDA",
        ticker = input$ticker,
        title_suffix = "EV to EBITDA",
        peer_col = if (!is.null(pl)) "peer_ev_to_ebitda" else NULL,
        peer_label = pl,
        n_peers = if (!is.null(pl)) data$n_peers[1] else NULL
      )
    }
  })

  output$val_ev_nopat_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    data <- valuation_data_with_peers()
    if (!is.null(data) && nrow(data) > 0) {
      pl <- peer_label()
      plot_financial_ratio(
        data = data,
        ratio_cols = "ev_to_nopat",
        labels = "EV/NOPAT",
        colors = "navy",
        y_format = "turns",
        y_label = "EV / NOPAT",
        ticker = input$ticker,
        title_suffix = "EV to NOPAT",
        peer_col = if (!is.null(pl)) "peer_ev_to_nopat" else NULL,
        peer_label = pl,
        n_peers = if (!is.null(pl)) data$n_peers[1] else NULL
      )
    }
  })

  output$val_yield_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    data <- valuation_data_with_peers()
    if (!is.null(data) && nrow(data) > 0) {
      pl <- peer_label()
      plot_financial_ratio(
        data = data,
        ratio_cols = "shareholder_yield",
        labels = "Shareholder Yield",
        colors = "navy",
        y_format = "percent",
        y_label = "(Dividends + Buybacks) / Price",
        ticker = input$ticker,
        title_suffix = "Shareholder Yield",
        peer_col = if (!is.null(pl)) "peer_shareholder_yield" else NULL,
        peer_label = pl,
        n_peers = if (!is.null(pl)) data$n_peers[1] else NULL
      )
    }
  })

  # === Return Decomposition ===

  output$roic_plot <- shiny::renderPlot({
    shiny::req(input$ticker, start_date())
    result <- prepare_dupont_over_time_data(
      ticker = input$ticker,
      start_date = start_date(),
      numerator = "nopat",
      denominator = "invested_capital",
      artifacts = artifacts
    )
    labels <- get_dupont_labels("nopat", "invested_capital")
    if (!is.null(result$data) && nrow(result$data) > 0) {
      plot_dupont_over_time(
        data = result$data,
        ticker = input$ticker,
        return_label = labels$return_label,
        effect_label = labels$effect_label,
        numerator_name = "NOPAT",
        denominator_name = "Invested Capital",
        multiplier_label = labels$multiplier_label,
        title_suffix = labels$title_suffix,
        roa_label = labels$roa_label
      )
    }
  })

  output$roe_plot <- shiny::renderPlot({
    shiny::req(input$ticker, start_date())
    result <- prepare_dupont_over_time_data(
      ticker = input$ticker,
      start_date = start_date(),
      numerator = "netIncome",
      denominator = "equity",
      artifacts = artifacts
    )
    labels <- get_dupont_labels("netIncome", "equity")
    if (!is.null(result$data) && nrow(result$data) > 0) {
      plot_dupont_over_time(
        data = result$data,
        ticker = input$ticker,
        return_label = labels$return_label,
        effect_label = labels$effect_label,
        numerator_name = "Net Income",
        denominator_name = "Equity",
        multiplier_label = labels$multiplier_label,
        title_suffix = labels$title_suffix,
        roa_label = labels$roa_label
      )
    }
  })
}

# Run the app
shiny::shinyApp(ui = ui, server = server)
