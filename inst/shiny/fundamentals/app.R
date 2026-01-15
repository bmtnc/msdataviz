# Fundamentals Explorer Shiny App

# Load the package
library(msdataviz)

# Load artifacts on startup (cached)
artifacts <- get_cached_artifacts()

# Get unique tickers with sector/industry for search
ticker_list <- artifacts$ttm_data %>%
  dplyr::distinct(ticker, sector, industry) %>%
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
        choices = NULL,
        options = list(
          placeholder = "Type to search...",
          maxOptions = 20
        )
      ),
      shiny::hr(),
      shiny::uiOutput("company_info")
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
            )
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

  # Server-side selectize for better performance with many tickers
  shiny::updateSelectizeInput(
    session,
    "ticker",
    choices = ticker_choices,
    server = TRUE
  )

  # Reactive: selected ticker info
  selected_info <- shiny::reactive({
    shiny::req(input$ticker)
    ticker_list %>%
      dplyr::filter(ticker == input$ticker)
  })

  # Company info sidebar
  output$company_info <- shiny::renderUI({
    shiny::req(input$ticker)
    info <- selected_info()
    shiny::tagList(
      shiny::h4(input$ticker),
      shiny::p(shiny::strong("Sector: "), info$sector),
      shiny::p(shiny::strong("Industry: "), info$industry)
    )
  })

  # Reactive: prepared fundamentals data
  fundamentals_data <- shiny::reactive({
    shiny::req(input$ticker)
    prepare_fundamentals_data(
      ticker = input$ticker,
      ttm_data = artifacts$ttm_data,
      start_date = as.Date("2014-12-31")
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
    shiny::req(input$ticker)
    ticker <- input$ticker
    start_date <- as.Date("2014-12-31")

    price_data <- artifacts$price_data %>%
      dplyr::filter(ticker == !!ticker, date >= start_date) %>%
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
    shiny::req(input$ticker)
    ticker <- input$ticker
    start_date <- as.Date("2014-12-31")

    drawdown_data <- artifacts$price_data %>%
      dplyr::filter(ticker == !!ticker, date >= start_date) %>%
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
    shiny::req(input$ticker)
    data <- prepare_per_share_decomposition_data(
      ttm_with_calcs(), input$ticker, "totalRevenue_ttm",
      as.Date("2014-12-31")
    )
    if (!is.null(data)) {
      plot_share_count_decomposition(data, input$ticker, "Revenue")
    }
  })

  output$ps_gross_profit_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    data <- prepare_per_share_decomposition_data(
      ttm_with_calcs(), input$ticker, "grossProfit_ttm",
      as.Date("2014-12-31")
    )
    if (!is.null(data)) {
      plot_share_count_decomposition(data, input$ticker, "Gross Profit")
    }
  })

  output$ps_ebit_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    data <- prepare_per_share_decomposition_data(
      ttm_with_calcs(), input$ticker, "ebit_ttm",
      as.Date("2014-12-31")
    )
    if (!is.null(data)) {
      plot_share_count_decomposition(data, input$ticker, "EBIT")
    }
  })

  output$ps_ebitda_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    data <- prepare_per_share_decomposition_data(
      ttm_with_calcs(), input$ticker, "ebitda_ttm",
      as.Date("2014-12-31")
    )
    if (!is.null(data)) {
      plot_share_count_decomposition(data, input$ticker, "EBITDA")
    }
  })

  output$ps_nopat_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    data <- prepare_per_share_decomposition_data(
      ttm_with_calcs(), input$ticker, "nopat",
      as.Date("2014-12-31")
    )
    if (!is.null(data)) {
      plot_share_count_decomposition(data, input$ticker, "NOPAT")
    }
  })

  output$ps_fcf_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    data <- prepare_per_share_decomposition_data(
      ttm_with_calcs(), input$ticker, "fcf",
      as.Date("2014-12-31")
    )
    if (!is.null(data)) {
      plot_share_count_decomposition(data, input$ticker, "FCF")
    }
  })

  output$ps_bv_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    data <- prepare_per_share_decomposition_data(
      ttm_with_calcs(), input$ticker, "totalShareholderEquity",
      as.Date("2014-12-31")
    )
    if (!is.null(data)) {
      plot_share_count_decomposition(data, input$ticker, "Book Value")
    }
  })

  # === Return Decomposition ===

  output$roic_plot <- shiny::renderPlot({
    shiny::req(input$ticker)
    result <- prepare_dupont_over_time_data(
      ticker = input$ticker,
      start_date = as.Date("2014-12-31"),
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
    shiny::req(input$ticker)
    result <- prepare_dupont_over_time_data(
      ticker = input$ticker,
      start_date = as.Date("2014-12-31"),
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
