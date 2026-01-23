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

# Pre-compute universe data on startup for fast filtering
# Using early start date to capture all available history
message("Pre-computing universe KPIs...")
universe_kpis_full <- prepare_universe_kpis(
  artifacts$ttm_data,
  start_date = as.Date("2000-01-01")
)

# Helper to compute KPI median columns
compute_kpi_medians <- function(data, ...) {
  data %>%
    dplyr::group_by(...) %>%
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
}

# Pre-compute market-wide KPI medians only
kpi_medians_market <- compute_kpi_medians(universe_kpis_full, calendar_quarter_ending)

message("Pre-computing universe valuations (this may take a moment)...")
universe_valuations_full <- prepare_universe_valuations_daily(
  artifacts$ttm_data,
  artifacts$price_data,
  start_date = as.Date("2000-01-01")
)

# Pre-compute market-wide peer medians only (sector/subsector/industry computed on demand)
message("Pre-computing market-wide peer medians...")

# Helper to compute valuation median columns
compute_valuation_medians <- function(data, ...) {
  data %>%
    dplyr::group_by(...) %>%
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
}

# Market-wide medians only (by date)
valuation_medians_market <- compute_valuation_medians(universe_valuations_full, date)

message("Pre-computation complete.")

# Get unique tickers with sector/subsector/industry for search
ticker_list <- artifacts$ttm_data %>%
  dplyr::distinct(ticker, sector, subsector, industry) %>%
  dplyr::arrange(ticker)

ticker_choices <- stats::setNames(ticker_list$ticker, ticker_list$ticker)

# Helper: wrap plotOutput with spinner
with_spinner <- function(plot_output) {

  shinycssloaders::withSpinner(plot_output, color = "#7C90A0")
}

# UI
ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  class = "theme-slate",
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      /* Slate theme */
      body { background-color: #f8f9fa; }

      /* Home page styles */
      #home-panel {
        position: fixed !important;
        top: 0 !important;
        left: 0 !important;
        right: 0 !important;
        bottom: 0 !important;
        background-color: #f8f9fa !important;
        z-index: 100 !important;
      }
      .home-search-wrapper {
        position: fixed !important;
        top: 50% !important;
        left: 50% !important;
        transform: translate(-50%, -50%) !important;
        width: 500px !important;
        max-width: 90vw !important;
        z-index: 101 !important;
      }
      .home-search-wrapper .form-group {
        margin-bottom: 0;
        width: 100%;
      }
      .home-search-wrapper .form-group > div {
        width: 100%;
      }
      .home-search-wrapper .selectize-control {
        width: 100% !important;
      }
      .home-search-wrapper .selectize-input {
        width: 100% !important;
        padding: 14px 20px;
        font-size: 1.1em;
        border: 2px solid #7C90A0;
        border-radius: 25px;
        background-color: white;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
      }
      .home-search-wrapper .selectize-input.focus {
        border-color: #4E5166;
        box-shadow: 0 2px 12px rgba(0, 0, 0, 0.15);
      }
      /* Hide the dropdown arrow */
      .home-search-wrapper .selectize-input::after {
        display: none !important;
      }
      .home-search-wrapper .selectize-control.single .selectize-input::after {
        display: none !important;
      }
      /* Style the dropdown as autocomplete suggestions */
      .home-search-wrapper .selectize-dropdown {
        border: 1px solid #ddd;
        border-top: none;
        border-radius: 0 0 12px 12px;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
        margin-top: -2px;
      }
      .home-search-wrapper .selectize-dropdown-content {
        max-height: 300px;
        padding: 5px 0;
      }
      .home-search-wrapper .selectize-dropdown .option {
        padding: 10px 20px;
      }
      .home-search-wrapper .selectize-dropdown .active {
        background-color: #f5f5f5;
        color: #4E5166;
      }

      .well {
        background-color: #4E5166;
        border: none;
        border-radius: 8px;
        color: white;
      }
      .well label { color: white; }
      .well h4, .well h5 { color: #B5AA9D; font-weight: 600; }
      .well p { color: rgba(255, 255, 255, 0.9); }
      .well strong { color: #B9B7A7; }
      .well hr { border-color: rgba(255, 255, 255, 0.2); }

      .well .selectize-input {
        background-color: white;
        border: 2px solid #7C90A0;
        border-radius: 4px;
      }
      .well .selectize-input.focus {
        border-color: #B5AA9D;
        box-shadow: 0 0 0 2px rgba(124, 144, 160, 0.2);
      }
      .well .selectize-dropdown {
        border: 2px solid #7C90A0;
        border-top: none;
      }
      .well .selectize-dropdown .active {
        background-color: #7C90A0;
        color: white;
      }

      .well .radio label { color: rgba(255, 255, 255, 0.9); }
      .well input[type='radio'] { accent-color: #7C90A0; }

      .well input[type='number'] {
        background-color: white;
        border: 2px solid #7C90A0;
        border-radius: 4px;
        color: #4E5166;
      }
      .well input[type='number']:focus {
        border-color: #B5AA9D;
        box-shadow: 0 0 0 2px rgba(124, 144, 160, 0.2);
        outline: none;
      }

      .container-fluid > h2 {
        color: #4E5166;
        font-weight: 600;
        padding: 15px 0;
        border-bottom: 3px solid #7C90A0;
        margin-bottom: 20px;
      }

      .nav-tabs { border-bottom: 2px solid #B9B7A7; }
      .nav-tabs > li > a {
        color: #4E5166;
        border: none;
        border-radius: 4px 4px 0 0;
        margin-right: 2px;
        transition: all 0.2s ease;
      }
      .nav-tabs > li > a:hover {
        background-color: #B9B7A7;
        border: none;
        color: #4E5166;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background-color: #7C90A0;
        color: white;
        border: none;
        font-weight: 500;
      }

      .tab-content .nav-tabs { border-bottom: 1px solid #B9B7A7; }
      .tab-content .nav-tabs > li > a { font-size: 0.9em; padding: 8px 12px; }
      .tab-content .nav-tabs > li.active > a { background-color: #747274; }

      .tab-pane h3 {
        color: #4E5166;
        font-weight: 600;
        margin-top: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #B9B7A7;
      }
      .tab-pane hr { border-color: #B9B7A7; margin: 30px 0; }

      /* Lookback buttons */
      .lookback-buttons {
        display: flex;
        flex-wrap: wrap;
        gap: 4px;
        margin-bottom: 10px;
      }
      .lookback-buttons .btn {
        flex: 1;
        min-width: 40px;
        padding: 4px 8px;
        font-size: 0.85em;
        background-color: #7C90A0;
        border: none;
        color: white;
      }
      .lookback-buttons .btn:hover {
        background-color: #5a6d7a;
      }
      .lookback-buttons .btn.active {
        background-color: #B5AA9D;
        color: #4E5166;
        font-weight: 600;
      }
    "))
  ),

  # Hidden ticker input that controls view state (always rendered)
  shiny::div(
    style = "display: none;",
    shiny::textInput("ticker", label = NULL, value = "")
  ),

  # Home Page View (no ticker selected)
  shiny::conditionalPanel(
    condition = "input.ticker === ''",
    shiny::tags$div(
      id = "home-panel",
      shiny::tags$div(
        class = "home-search-wrapper",
        shiny::selectizeInput(
          inputId = "home_ticker",
          label = NULL,
          choices = NULL,
          selected = NULL,
          options = list(
            placeholder = "Enter ticker symbol...",
            maxOptions = 50,
            score = I("function(search) {
              var token = search.toLowerCase();
              return function(item) {
                if (!item.value) return 0;
                var text = String(item.text || item.value).toLowerCase();
                if (text === token) return 10000;
                if (text.indexOf(token) === 0) return 1000 + (100 - text.length);
                if (text.indexOf(token) > -1) return 100 - text.indexOf(token);
                return 0;
              };
            }")
          )
        )
      )
    )
  ),

  # Dashboard View (ticker selected)
  shiny::conditionalPanel(
    condition = "input.ticker !== ''",
    shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 2,
      shiny::selectizeInput(
        inputId = "sidebar_ticker",
        label = "Search Ticker",
        choices = ticker_choices,
        selected = character(0),
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
      shiny::div(
        class = "lookback-buttons",
        shiny::actionButton("lookback_1y", "1Y", class = "btn-sm"),
        shiny::actionButton("lookback_3y", "3Y", class = "btn-sm"),
        shiny::actionButton("lookback_5y", "5Y", class = "btn-sm"),
        shiny::actionButton("lookback_10y", "10Y", class = "btn-sm active"),
        shiny::actionButton("lookback_si", "SI", class = "btn-sm")
      ),
      shiny::numericInput(
        inputId = "lookback_days",
        label = "Custom (days)",
        value = 3650,
        min = 30,
        max = 7300,
        step = 30
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
          with_spinner(shiny::plotOutput("tsr_plot", height = "500px")),
          shiny::hr(),
          shiny::h3("Drawdown"),
          with_spinner(shiny::plotOutput("drawdown_plot", height = "400px"))
        ),

        # Financials Tab
        shiny::tabPanel(
          "Financials",
          shiny::tabsetPanel(
            # Income Statement
            shiny::tabPanel(
              "Income Statement",
              shiny::tabsetPanel(
                shiny::tabPanel("Revenue", with_spinner(shiny::plotOutput("revenue_plot", height = "500px"))),
                shiny::tabPanel("Gross Profit", with_spinner(shiny::plotOutput("gross_profit_plot", height = "500px"))),
                shiny::tabPanel("EBIT", with_spinner(shiny::plotOutput("ebit_plot", height = "500px"))),
                shiny::tabPanel("EBITDA", with_spinner(shiny::plotOutput("ebitda_plot", height = "500px"))),
                shiny::tabPanel("NOPAT", with_spinner(shiny::plotOutput("nopat_plot", height = "500px"))),
                shiny::tabPanel("Interest Income", with_spinner(shiny::plotOutput("interest_income_plot", height = "500px"))),
                shiny::tabPanel("Interest Expense", with_spinner(shiny::plotOutput("interest_expense_plot", height = "500px"))),
                shiny::tabPanel("Net Interest Income", with_spinner(shiny::plotOutput("net_interest_income_plot", height = "500px"))),
                shiny::tabPanel("Net Income", with_spinner(shiny::plotOutput("net_income_plot", height = "500px")))
              )
            ),
            # Cash Flow
            shiny::tabPanel(
              "Cash Flow",
              shiny::tabsetPanel(
                shiny::tabPanel("Operating Cash Flow", with_spinner(shiny::plotOutput("ocf_plot", height = "500px"))),
                shiny::tabPanel("Free Cash Flow", with_spinner(shiny::plotOutput("fcf_plot", height = "500px"))),
                shiny::tabPanel("CapEx", with_spinner(shiny::plotOutput("capex_plot", height = "500px"))),
                shiny::tabPanel("Dividends", with_spinner(shiny::plotOutput("dividends_plot", height = "500px"))),
                shiny::tabPanel("Buybacks", with_spinner(shiny::plotOutput("buybacks_plot", height = "500px"))),
                shiny::tabPanel("Total Capital Returned", with_spinner(shiny::plotOutput("total_capital_returned_plot", height = "500px")))
              )
            ),
            # Balance Sheet
            shiny::tabPanel(
              "Balance Sheet",
              shiny::tabsetPanel(
                shiny::tabPanel("Total Assets", with_spinner(shiny::plotOutput("assets_plot", height = "500px"))),
                shiny::tabPanel("Cash", with_spinner(shiny::plotOutput("cash_plot", height = "500px"))),
                shiny::tabPanel("Total Debt", with_spinner(shiny::plotOutput("debt_plot", height = "500px"))),
                shiny::tabPanel("Shareholder Equity", with_spinner(shiny::plotOutput("equity_plot", height = "500px"))),
                shiny::tabPanel("Shares Outstanding", with_spinner(shiny::plotOutput("shares_plot", height = "500px")))
              )
            )
          )
        ),

        # KPIs Tab (top-level)
        shiny::tabPanel(
          "KPIs",
          shiny::tabsetPanel(
            shiny::tabPanel("Margins", with_spinner(shiny::plotOutput("kpi_margins_plot", height = "500px"))),
            shiny::tabPanel("ROIC",
              with_spinner(shiny::plotOutput("kpi_roic_plot", height = "500px")),
              shiny::plotOutput("kpi_roic_sparkline", height = "120px")
            ),
            shiny::tabPanel("GROIC",
              with_spinner(shiny::plotOutput("kpi_groic_plot", height = "500px")),
              shiny::plotOutput("kpi_groic_sparkline", height = "120px")
            ),
            shiny::tabPanel("ROE",
              with_spinner(shiny::plotOutput("kpi_roe_plot", height = "500px")),
              shiny::plotOutput("kpi_roe_sparkline", height = "120px")
            ),
            shiny::tabPanel("FCF Conversion",
              with_spinner(shiny::plotOutput("kpi_fcf_conversion_plot", height = "500px")),
              shiny::plotOutput("kpi_fcf_conversion_sparkline", height = "120px")
            ),
            shiny::tabPanel("Cost of Debt",
              with_spinner(shiny::plotOutput("kpi_cost_of_debt_plot", height = "500px")),
              shiny::plotOutput("kpi_cost_of_debt_sparkline", height = "120px")
            ),
            shiny::tabPanel("Interest Coverage",
              with_spinner(shiny::plotOutput("kpi_interest_coverage_plot", height = "500px")),
              shiny::plotOutput("kpi_interest_coverage_sparkline", height = "120px")
            ),
            shiny::tabPanel("Leverage",
              with_spinner(shiny::plotOutput("kpi_leverage_plot", height = "500px")),
              shiny::plotOutput("kpi_leverage_sparkline", height = "120px")
            )
          )
        ),

        # Per-Share Growth Tab (top-level)
        shiny::tabPanel(
          "Per-Share Growth",
          shiny::tabsetPanel(
            shiny::tabPanel("Revenue", with_spinner(shiny::plotOutput("ps_revenue_plot", height = "500px"))),
            shiny::tabPanel("Gross Profit", with_spinner(shiny::plotOutput("ps_gross_profit_plot", height = "500px"))),
            shiny::tabPanel("EBIT", with_spinner(shiny::plotOutput("ps_ebit_plot", height = "500px"))),
            shiny::tabPanel("EBITDA", with_spinner(shiny::plotOutput("ps_ebitda_plot", height = "500px"))),
            shiny::tabPanel("NOPAT", with_spinner(shiny::plotOutput("ps_nopat_plot", height = "500px"))),
            shiny::tabPanel("FCF", with_spinner(shiny::plotOutput("ps_fcf_plot", height = "500px"))),
            shiny::tabPanel("Book Value", with_spinner(shiny::plotOutput("ps_bv_plot", height = "500px")))
          )
        ),

        # Valuation Tab
        shiny::tabPanel(
          "Valuation",
          shiny::tabsetPanel(
            shiny::tabPanel("P/S", with_spinner(shiny::plotOutput("val_ps_plot", height = "400px")), shiny::plotOutput("val_ps_count", height = "100px")),
            shiny::tabPanel("P/B", with_spinner(shiny::plotOutput("val_pb_plot", height = "400px")), shiny::plotOutput("val_pb_count", height = "100px")),
            shiny::tabPanel("P/Gross Profit", with_spinner(shiny::plotOutput("val_pgp_plot", height = "400px")), shiny::plotOutput("val_pgp_count", height = "100px")),
            shiny::tabPanel("P/EBIT", with_spinner(shiny::plotOutput("val_pebit_plot", height = "400px")), shiny::plotOutput("val_pebit_count", height = "100px")),
            shiny::tabPanel("P/E", with_spinner(shiny::plotOutput("val_pe_plot", height = "400px")), shiny::plotOutput("val_pe_count", height = "100px")),
            shiny::tabPanel("P/FCF", with_spinner(shiny::plotOutput("val_pfcf_plot", height = "400px")), shiny::plotOutput("val_pfcf_count", height = "100px")),
            shiny::tabPanel("EV/EBITDA", with_spinner(shiny::plotOutput("val_ev_ebitda_plot", height = "400px")), shiny::plotOutput("val_ev_ebitda_count", height = "100px")),
            shiny::tabPanel("EV/NOPAT", with_spinner(shiny::plotOutput("val_ev_nopat_plot", height = "400px")), shiny::plotOutput("val_ev_nopat_count", height = "100px")),
            shiny::tabPanel("Shareholder Yield", with_spinner(shiny::plotOutput("val_yield_plot", height = "400px")), shiny::plotOutput("val_yield_count", height = "100px"))
          )
        ),

        # DuPont Tab
        shiny::tabPanel(
          "DuPont",
          shiny::tabsetPanel(
            shiny::tabPanel("ROIC", with_spinner(shiny::plotOutput("roic_plot", height = "550px"))),
            shiny::tabPanel("ROE", with_spinner(shiny::plotOutput("roe_plot", height = "550px")))
          )
        )
      )
    )
  )
  )  # Close conditionalPanel for dashboard
)

# Server
server <- function(input, output, session) {
  # Set ggplot theme
  set_ggplot_theme()

  # Initialize home ticker choices (server-side to start empty)
  shiny::updateSelectizeInput(
    session, "home_ticker",
    choices = ticker_choices,
    selected = character(0),
    server = TRUE
  )

  # Sync home page ticker selection to hidden ticker (controls view state)
  shiny::observeEvent(input$home_ticker, {
    shiny::req(input$home_ticker, nzchar(input$home_ticker))
    shiny::updateTextInput(session, "ticker", value = input$home_ticker)
    shiny::updateSelectizeInput(session, "sidebar_ticker", selected = input$home_ticker)
  }, ignoreInit = TRUE)

  # Sync sidebar ticker selection to hidden ticker
  shiny::observeEvent(input$sidebar_ticker, {
    shiny::req(input$sidebar_ticker)
    shiny::updateTextInput(session, "ticker", value = input$sidebar_ticker)
  }, ignoreInit = TRUE)

  # Helper: update lookback button active states via JavaScript
  update_lookback_buttons <- function(active_id) {
    js <- sprintf("
      $('.lookback-buttons .btn').removeClass('active');
      $('#%s').addClass('active');
    ", active_id)
    shinyjs::runjs(js)
  }

  # Lookback button observers
  shiny::observeEvent(input$lookback_1y, {
    shiny::updateNumericInput(session, "lookback_days", value = 365)
    update_lookback_buttons("lookback_1y")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$lookback_3y, {
    shiny::updateNumericInput(session, "lookback_days", value = 1095)
    update_lookback_buttons("lookback_3y")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$lookback_5y, {
    shiny::updateNumericInput(session, "lookback_days", value = 1825)
    update_lookback_buttons("lookback_5y")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$lookback_10y, {
    shiny::updateNumericInput(session, "lookback_days", value = 3650)
    update_lookback_buttons("lookback_10y")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$lookback_si, {
    shiny::req(input$ticker)
    # Calculate days since earliest price data for this ticker
    earliest_date <- artifacts$price_data %>%
      dplyr::filter(ticker == input$ticker) %>%
      dplyr::summarize(min_date = min(date, na.rm = TRUE)) %>%
      dplyr::pull(min_date)
    if (!is.na(earliest_date)) {
      days_since_inception <- as.integer(Sys.Date() - earliest_date)
      shiny::updateNumericInput(session, "lookback_days", value = days_since_inception)
    }
    update_lookback_buttons("lookback_si")
  }, ignoreInit = TRUE)

  # Clear button highlight when custom value is manually entered

  shiny::observeEvent(input$lookback_days, {
    # Only clear if value doesn't match preset buttons
    preset_values <- c(365, 1095, 1825, 3650)
    if (!input$lookback_days %in% preset_values) {
      shinyjs::runjs("$('.lookback-buttons .btn').removeClass('active');")
    }
  }, ignoreInit = TRUE)

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

  # Reactive: Universe KPIs (filter pre-computed data by date range)
  universe_kpis <- shiny::reactive({
    shiny::req(start_date())
    universe_kpis_full %>%
      dplyr::filter(date >= start_date())
  })

  # Reactive: Peer medians
  # Market: use pre-computed medians (instant)
  # Sector/Subsector/Industry: filter universe first, then aggregate
  # Groups by calendar_quarter_ending (not fiscalDateEnding) since companies have different fiscal calendars
  peer_medians <- shiny::reactive({
    shiny::req(input$ticker, input$peer_universe)

    # Return NULL if "none" selected
    if (input$peer_universe == "none") {
      return(NULL)
    }

    sd <- start_date()

    if (input$peer_universe == "market") {
      # Use pre-computed market medians
      kpi_medians_market %>%
        dplyr::filter(calendar_quarter_ending >= sd)
    } else {
      # Filter to peer group first, then compute medians
      info <- selected_info()
      peer_group <- info[[input$peer_universe]]

      universe_kpis_full %>%
        dplyr::filter(
          calendar_quarter_ending >= sd,
          .data[[input$peer_universe]] == peer_group
        ) %>%
        compute_kpi_medians(calendar_quarter_ending)
    }
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

  # Reactive: Extract peer "as of" date (most recent calendar_quarter_ending with peer data)
  peer_as_of_date <- shiny::reactive({
    data <- kpi_data_with_peers()
    if (is.null(data) || !"n_peers" %in% names(data)) {
      return(NULL)
    }
    # Get the most recent quarter that has peer data
    data %>%
      dplyr::filter(!is.na(n_peers)) %>%
      dplyr::pull(calendar_quarter_ending) %>%
      max(na.rm = TRUE)
  })

  # Reactive: Extract n_peers for the most recent quarter (matches peer_as_of_date)
  peer_n_peers <- shiny::reactive({
    data <- kpi_data_with_peers()
    as_of <- peer_as_of_date()
    if (is.null(data) || is.null(as_of) || !"n_peers" %in% names(data)) {
      return(NULL)
    }
    data %>%
      dplyr::filter(calendar_quarter_ending == as_of) %>%
      dplyr::pull(n_peers) %>%
      dplyr::first()
  })

  # Reactive: X-axis limits for KPI charts (for alignment with cross-section count)
  kpi_xlim <- shiny::reactive({
    data <- kpi_data_with_peers()
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    date_range <- range(data$date)
    date_buffer <- as.numeric(diff(date_range)) * 0.08
    c(date_range[1], date_range[2] + date_buffer)
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

  # Reactive: Universe valuations (filter pre-computed data by date range)
  universe_valuations <- shiny::reactive({
    shiny::req(start_date())
    universe_valuations_full %>%
      dplyr::filter(date >= start_date())
  })

  # Reactive: Valuation peer medians
  # Market: use pre-computed medians (instant)
  # Sector/Subsector/Industry: filter universe first, then aggregate (faster on smaller subset)
  valuation_peer_medians <- shiny::reactive({
    shiny::req(input$ticker, input$peer_universe)

    # Return NULL if "none" selected
    if (input$peer_universe == "none") {
      return(NULL)
    }

    sd <- start_date()

    if (input$peer_universe == "market") {
      # Use pre-computed market medians
      valuation_medians_market %>%
        dplyr::filter(date >= sd)
    } else {
      # Filter to peer group first (reduces data), then compute medians
      info <- selected_info()
      peer_group <- info[[input$peer_universe]]

      universe_valuations_full %>%
        dplyr::filter(
          date >= sd,
          .data[[input$peer_universe]] == peer_group
        ) %>%
        compute_valuation_medians(date)
    }
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
        n_peers = if (!is.null(pl)) peer_n_peers() else NULL,
        peer_as_of_date = peer_as_of_date(),
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
        n_peers = if (!is.null(pl)) peer_n_peers() else NULL,
        peer_as_of_date = peer_as_of_date(),
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
        n_peers = if (!is.null(pl)) peer_n_peers() else NULL,
        peer_as_of_date = peer_as_of_date(),
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
        n_peers = if (!is.null(pl)) peer_n_peers() else NULL,
        peer_as_of_date = peer_as_of_date(),
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
        n_peers = if (!is.null(pl)) peer_n_peers() else NULL,
        peer_as_of_date = peer_as_of_date(),
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
        n_peers = if (!is.null(pl)) peer_n_peers() else NULL,
        peer_as_of_date = peer_as_of_date(),
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
        n_peers = if (!is.null(pl)) peer_n_peers() else NULL,
        peer_as_of_date = peer_as_of_date(),
              )
    }
  })

  # === KPI Sparkline Charts (peer sample size) ===

  output$kpi_roic_sparkline <- shiny::renderPlot({
    data <- kpi_data_with_peers()
    if (!is.null(data) && "n_peers" %in% names(data) && input$peer_universe != "none") {
      plot_cross_section_count(
        data,
        date_col = "date",
        count_col = "n_peers",
        title = NULL,
        y_label = "Count",
        xlim = kpi_xlim()
      )
    }
  })

  output$kpi_groic_sparkline <- shiny::renderPlot({
    data <- kpi_data_with_peers()
    if (!is.null(data) && "n_peers" %in% names(data) && input$peer_universe != "none") {
      plot_cross_section_count(
        data,
        date_col = "date",
        count_col = "n_peers",
        title = NULL,
        y_label = "Count",
        xlim = kpi_xlim()
      )
    }
  })

  output$kpi_roe_sparkline <- shiny::renderPlot({
    data <- kpi_data_with_peers()
    if (!is.null(data) && "n_peers" %in% names(data) && input$peer_universe != "none") {
      plot_cross_section_count(
        data,
        date_col = "date",
        count_col = "n_peers",
        title = NULL,
        y_label = "Count",
        xlim = kpi_xlim()
      )
    }
  })

  output$kpi_fcf_conversion_sparkline <- shiny::renderPlot({
    data <- kpi_data_with_peers()
    if (!is.null(data) && "n_peers" %in% names(data) && input$peer_universe != "none") {
      plot_cross_section_count(
        data,
        date_col = "date",
        count_col = "n_peers",
        title = NULL,
        y_label = "Count",
        xlim = kpi_xlim()
      )
    }
  })

  output$kpi_cost_of_debt_sparkline <- shiny::renderPlot({
    data <- kpi_data_with_peers()
    if (!is.null(data) && "n_peers" %in% names(data) && input$peer_universe != "none") {
      plot_cross_section_count(
        data,
        date_col = "date",
        count_col = "n_peers",
        title = NULL,
        y_label = "Count",
        xlim = kpi_xlim()
      )
    }
  })

  output$kpi_interest_coverage_sparkline <- shiny::renderPlot({
    data <- kpi_data_with_peers()
    if (!is.null(data) && "n_peers" %in% names(data) && input$peer_universe != "none") {
      plot_cross_section_count(
        data,
        date_col = "date",
        count_col = "n_peers",
        title = NULL,
        y_label = "Count",
        xlim = kpi_xlim()
      )
    }
  })

  output$kpi_leverage_sparkline <- shiny::renderPlot({
    data <- kpi_data_with_peers()
    if (!is.null(data) && "n_peers" %in% names(data) && input$peer_universe != "none") {
      plot_cross_section_count(
        data,
        date_col = "date",
        count_col = "n_peers",
        title = NULL,
        y_label = "Count",
        xlim = kpi_xlim()
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
        n_peers = if (!is.null(pl)) peer_n_peers() else NULL
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
        n_peers = if (!is.null(pl)) peer_n_peers() else NULL
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
        n_peers = if (!is.null(pl)) peer_n_peers() else NULL
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
        n_peers = if (!is.null(pl)) peer_n_peers() else NULL
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
        n_peers = if (!is.null(pl)) peer_n_peers() else NULL
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
        n_peers = if (!is.null(pl)) peer_n_peers() else NULL
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
        n_peers = if (!is.null(pl)) peer_n_peers() else NULL
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
        n_peers = if (!is.null(pl)) peer_n_peers() else NULL
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
        n_peers = if (!is.null(pl)) peer_n_peers() else NULL
      )
    }
  })

  # === Valuation Cross-Section Counts ===

  output$val_ps_count <- shiny::renderPlot({
    data <- valuation_peer_medians()
    if (!is.null(data) && "n_peers" %in% names(data) && input$peer_universe != "none") {
      plot_cross_section_count(
        data = data,
        date_col = "date",
        count_col = "n_peers",
        title = NULL,
        y_label = "Count"
      )
    }
  })

  output$val_pb_count <- shiny::renderPlot({
    data <- valuation_peer_medians()
    if (!is.null(data) && "n_peers" %in% names(data) && input$peer_universe != "none") {
      plot_cross_section_count(
        data = data,
        date_col = "date",
        count_col = "n_peers",
        title = NULL,
        y_label = "Count"
      )
    }
  })

  output$val_pgp_count <- shiny::renderPlot({
    data <- valuation_peer_medians()
    if (!is.null(data) && "n_peers" %in% names(data) && input$peer_universe != "none") {
      plot_cross_section_count(
        data = data,
        date_col = "date",
        count_col = "n_peers",
        title = NULL,
        y_label = "Count"
      )
    }
  })

  output$val_pebit_count <- shiny::renderPlot({
    data <- valuation_peer_medians()
    if (!is.null(data) && "n_peers" %in% names(data) && input$peer_universe != "none") {
      plot_cross_section_count(
        data = data,
        date_col = "date",
        count_col = "n_peers",
        title = NULL,
        y_label = "Count"
      )
    }
  })

  output$val_pe_count <- shiny::renderPlot({
    data <- valuation_peer_medians()
    if (!is.null(data) && "n_peers" %in% names(data) && input$peer_universe != "none") {
      plot_cross_section_count(
        data = data,
        date_col = "date",
        count_col = "n_peers",
        title = NULL,
        y_label = "Count"
      )
    }
  })

  output$val_pfcf_count <- shiny::renderPlot({
    data <- valuation_peer_medians()
    if (!is.null(data) && "n_peers" %in% names(data) && input$peer_universe != "none") {
      plot_cross_section_count(
        data = data,
        date_col = "date",
        count_col = "n_peers",
        title = NULL,
        y_label = "Count"
      )
    }
  })

  output$val_ev_ebitda_count <- shiny::renderPlot({
    data <- valuation_peer_medians()
    if (!is.null(data) && "n_peers" %in% names(data) && input$peer_universe != "none") {
      plot_cross_section_count(
        data = data,
        date_col = "date",
        count_col = "n_peers",
        title = NULL,
        y_label = "Count"
      )
    }
  })

  output$val_ev_nopat_count <- shiny::renderPlot({
    data <- valuation_peer_medians()
    if (!is.null(data) && "n_peers" %in% names(data) && input$peer_universe != "none") {
      plot_cross_section_count(
        data = data,
        date_col = "date",
        count_col = "n_peers",
        title = NULL,
        y_label = "Count"
      )
    }
  })

  output$val_yield_count <- shiny::renderPlot({
    data <- valuation_peer_medians()
    if (!is.null(data) && "n_peers" %in% names(data) && input$peer_universe != "none") {
      plot_cross_section_count(
        data = data,
        date_col = "date",
        count_col = "n_peers",
        title = NULL,
        y_label = "Count"
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
