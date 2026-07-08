# ============================================================
# app.R — IEA Item Bank Explorer
# ------------------------------------------------------------
# Public read-only visualizer for questionnaire items, scales,
# questionnaires, item selection cart, and Excel export.
# ============================================================

# ------------------------------------------------------------
# Libraries
# ------------------------------------------------------------
library(shiny)
library(bslib)
library(DBI)
library(RPostgres)
library(pool)
library(dplyr)
library(dbplyr)
library(tibble)
library(stringr)
library(DT)
library(openxlsx)

# ------------------------------------------------------------
# App version
# ------------------------------------------------------------
APP_VERSION <- "0.1.0"

# ------------------------------------------------------------
# Institutional colors
# ------------------------------------------------------------
iea_blue <- "#0070b8"
iea_red  <- "#e2211c"

# ------------------------------------------------------------
# Conexión (pool: conexiones reutilizables, una sola vez por proceso)
# ------------------------------------------------------------
pool <- dbPool(
  RPostgres::Postgres(),
  host     = Sys.getenv("SUPABASE_HOST"),
  port     = as.integer(Sys.getenv("SUPABASE_PORT")),
  dbname   = Sys.getenv("SUPABASE_DB"),
  user     = Sys.getenv("SUPABASE_RO_USER"),
  password = Sys.getenv("SUPABASE_RO_PWD"),
  sslmode  = "require"
)

onStop(function() poolClose(pool))

# ------------------------------------------------------------
# Helpers generales
# ------------------------------------------------------------

safe_chr <- function(x) {
  ifelse(is.na(x), "", as.character(x))
}

add_missing_columns <- function(df, cols) {
  for (col in cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  
  df
}

read_table_safe <- function(pool, table_name) {
  tryCatch(
    {
      tbl(pool, table_name) %>%
        collect()
    },
    error = function(e) {
      tibble()
    }
  )
}

filter_by_selected_items <- function(df, selected_item_uids) {
  if (nrow(df) == 0) {
    return(df)
  }
  
  if (!"item_uid" %in% names(df)) {
    return(df)
  }
  
  df %>%
    filter(item_uid %in% selected_item_uids)
}

# ------------------------------------------------------------
# Standardization layer
# ------------------------------------------------------------

standardize_items <- function(df) {
  
  if ("wording_item" %in% names(df) && !"wording" %in% names(df)) {
    df <- df %>% rename(wording = wording_item)
  }
  
  if ("item_var" %in% names(df) && !"item_code" %in% names(df)) {
    df <- df %>% rename(item_code = item_var)
  }
  
  if ("varname" %in% names(df) && !"source_variable" %in% names(df)) {
    df <- df %>% rename(source_variable = varname)
  }
  
  if ("scale_description" %in% names(df) && !"scale" %in% names(df)) {
    df <- df %>% rename(scale = scale_description)
  }
  
  if ("type" %in% names(df) && !"item_type" %in% names(df)) {
    df <- df %>% rename(item_type = type)
  }
  
  if ("target" %in% names(df) && !"population" %in% names(df)) {
    df <- df %>% rename(population = target)
  }
  
  required_cols <- c(
    "item_admin_pk",
    "item_admin_id",
    "admin_id",
    "item_uid",
    "item_code",
    "source_variable",
    "dataset_label",
    "wording",
    "wording_question",
    "wording_instruction",
    "wording_context",
    "wording_heading",
    "study",
    "phase",
    "year",
    "instrument",
    "population",
    "cycle",
    "section",
    "scale_id",
    "scale",
    "scale_varname",
    "item_type",
    "response_format",
    "trend_status",
    "puf",
    "miss_id",
    "response_id"
  )
  
  df <- add_missing_columns(df, required_cols)
  
  df <- df %>%
    mutate(
      item_uid = if_else(
        is.na(item_uid) | item_uid == "",
        paste(
          safe_chr(admin_id),
          safe_chr(item_admin_id),
          safe_chr(item_code),
          sep = "_"
        ),
        safe_chr(item_uid)
      ),
      
      item_code = if_else(
        is.na(item_code) | item_code == "",
        safe_chr(source_variable),
        safe_chr(item_code)
      ),
      
      wording = if_else(
        is.na(wording) | wording == "",
        safe_chr(wording_question),
        safe_chr(wording)
      ),
      
      study = safe_chr(study),
      phase = safe_chr(phase),
      year = safe_chr(year),
      instrument = safe_chr(instrument),
      population = safe_chr(population),
      cycle = safe_chr(cycle),
      section = safe_chr(section),
      
      scale_id = safe_chr(scale_id),
      scale = safe_chr(scale),
      scale_varname = safe_chr(scale_varname),
      
      item_type = safe_chr(item_type),
      source_variable = safe_chr(source_variable),
      dataset_label = safe_chr(dataset_label),
      
      response_format = if_else(
        is.na(response_format) | response_format == "",
        safe_chr(response_id),
        safe_chr(response_format)
      ),
      
      trend_status = if_else(
        is.na(trend_status) | trend_status == "",
        "Not classified",
        safe_chr(trend_status)
      ),
      
      puf = if_else(is.na(puf), FALSE, as.logical(puf))
    )
  
  df
}

# ------------------------------------------------------------
# Data access layer
# ------------------------------------------------------------

get_items <- function(pool) {
  
  item_admin <- tbl(pool, "item_admin")
  
  admin_tbl <- tbl(pool, "admin") %>%
    select(
      admin_id,
      study,
      phase,
      year,
      instrument,
      target,
      cycle
    )
  
  scale_items <- tbl(pool, "v_scale_items") %>%
    select(
      item_admin_id,
      admin_id,
      item_uid,
      scale_id,
      scale_description,
      scale_varname
    )
  
  df <- item_admin %>%
    left_join(
      admin_tbl,
      by = "admin_id"
    ) %>%
    left_join(
      scale_items,
      by = c("item_admin_id", "admin_id", "item_uid")
    ) %>%
    collect()
  
  standardize_items(df)
}

get_item_history <- function(pool, selected_item_uids) {
  tibble()
}

get_item_variants <- function(pool, selected_item_uids) {
  tibble()
}

get_scale_membership <- function(pool, selected_item_uids) {
  df <- read_table_safe(pool, "v_scale_items")
  
  if (nrow(df) == 0) {
    return(tibble())
  }
  
  filter_by_selected_items(df, selected_item_uids)
}

get_response_options <- function(pool, selected_items) {
  
  selected_response_ids <- selected_items %>%
    filter(!is.na(response_id), response_id != "") %>%
    distinct(response_id) %>%
    pull(response_id)
  
  if (length(selected_response_ids) == 0) {
    return(tibble())
  }
  
  response_values <- read_table_safe(pool, "value_scheme_value")
  
  if (nrow(response_values) == 0) {
    return(tibble())
  }
  
  if (!"response_id" %in% names(response_values)) {
    return(response_values)
  }
  
  response_values %>%
    filter(response_id %in% selected_response_ids)
}

get_missing_values <- function(pool, selected_items) {
  
  selected_miss_ids <- selected_items %>%
    filter(!is.na(miss_id), miss_id != "") %>%
    distinct(miss_id) %>%
    pull(miss_id)
  
  if (length(selected_miss_ids) == 0) {
    return(tibble())
  }
  
  miss_values <- read_table_safe(pool, "miss_scheme_value")
  
  if (nrow(miss_values) == 0) {
    return(tibble())
  }
  
  if (!"miss_id" %in% names(miss_values)) {
    return(miss_values)
  }
  
  miss_values %>%
    filter(miss_id %in% selected_miss_ids)
}

get_variables_and_data <- function(pool, selected_items) {
  selected_items %>%
    select(
      item_uid,
      item_admin_id,
      admin_id,
      study,
      phase,
      year,
      cycle,
      population,
      instrument,
      item_code,
      source_variable,
      dataset_label,
      puf
    ) %>%
    distinct()
}

derive_scales <- function(items) {
  
  items %>%
    filter(!is.na(scale), scale != "") %>%
    distinct(
      study,
      phase,
      year,
      cycle,
      population,
      instrument,
      scale_id,
      scale,
      scale_varname,
      item_uid,
      trend_status
    ) %>%
    group_by(
      study,
      phase,
      year,
      cycle,
      population,
      instrument,
      scale_id,
      scale,
      scale_varname
    ) %>%
    summarise(
      n_items = n_distinct(item_uid),
      n_trend_items = sum(trend_status == "Trend item", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      scale_uid = paste(
        safe_chr(study),
        safe_chr(cycle),
        safe_chr(population),
        safe_chr(instrument),
        safe_chr(scale_id),
        sep = "_"
      ),
      scale_uid = str_replace_all(scale_uid, "[^A-Za-z0-9_]", "_")
    )
}

derive_questionnaires <- function(items) {
  
  items %>%
    distinct(
      study,
      phase,
      year,
      cycle,
      population,
      instrument,
      section,
      scale,
      item_uid
    ) %>%
    group_by(
      study,
      phase,
      year,
      cycle,
      population,
      instrument
    ) %>%
    summarise(
      n_items = n_distinct(item_uid),
      n_sections = n_distinct(section[section != ""], na.rm = TRUE),
      n_scales = n_distinct(scale[scale != ""], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      questionnaire_uid = paste(
        safe_chr(study),
        safe_chr(cycle),
        safe_chr(population),
        safe_chr(instrument),
        sep = "_"
      ),
      questionnaire_uid = str_replace_all(questionnaire_uid, "[^A-Za-z0-9_]", "_")
    )
}

# ------------------------------------------------------------
# Theme
# ------------------------------------------------------------

app_theme <- bs_theme(
  version = 5,
  primary = iea_blue,
  secondary = "#6C757D",
  base_font = font_google("Source Sans 3"),
  heading_font = font_google("Source Sans 3")
)

# ------------------------------------------------------------
# Branding
# ------------------------------------------------------------

app_brand <- div(
  class = "app-brand",
  tags$img(
    src = "iea_logo.png",
    class = "app-logo",
    alt = "IEA logo"
  ),
  div(
    class = "app-title",
    div(class = "app-title-main", "Item Bank Explorer")
  )
)

app_footer <- div(
  class = "app-footer",
  div(
    class = "app-footer-inner",
    div(
      class = "footer-line footer-version",
      tags$span(
        HTML("<strong>IEA Item Bank Explorer</strong> · Version ")
      ),
      actionLink(
        inputId = "version_click",
        label = APP_VERSION,
        class = "version-easter-egg"
      )
    ),
    div(
      class = "footer-line footer-copyright",
      HTML(
        "&copy; IEA, 2026 · International Association for the Evaluation of Educational Achievement"
      )
    )
  )
)

# ------------------------------------------------------------
# UI helpers
# ------------------------------------------------------------

page_header <- function(title, subtitle = NULL) {
  div(
    class = "page-header",
    h2(title),
    if (!is.null(subtitle)) {
      p(class = "text-muted", subtitle)
    }
  )
}

empty_state <- function(message) {
  div(
    class = "empty-state",
    p(class = "text-muted", message)
  )
}

item_card <- function(item) {
  
  item_uid_safe <- str_replace_all(item$item_uid, "[^A-Za-z0-9_]", "_")
  
  card(
    class = "item-card",
    card_body(
      div(
        class = "d-flex justify-content-between align-items-start gap-3",
        div(
          h5(item$item_code),
          p(class = "item-wording", item$wording)
        ),
        actionButton(
          inputId = paste0("add_item_", item_uid_safe),
          label = "+ Add",
          class = "btn btn-sm btn-primary"
        )
      ),
      tags$hr(),
      div(
        class = "text-muted small",
        paste(
          ifelse(item$study == "", "Study not available", item$study),
          ifelse(item$year == "", "", item$year),
          "·",
          ifelse(item$population == "", "Target not available", item$population),
          "·",
          ifelse(item$instrument == "", "Instrument not available", item$instrument)
        )
      ),
      div(
        class = "small",
        paste(
          "Variable:",
          ifelse(item$source_variable == "", "Not available", item$source_variable)
        )
      ),
      div(
        class = "small",
        paste(
          "Scale:",
          ifelse(item$scale == "", "Not assigned", item$scale)
        )
      ),
      div(
        class = "small",
        paste(
          "Type:",
          ifelse(item$item_type == "", "Not available", item$item_type)
        )
      ),
      div(
        class = "mt-2",
        span(
          class = "badge rounded-pill text-bg-light",
          ifelse(isTRUE(item$puf), "Public data", "PUF not specified")
        ),
        span(
          class = "badge rounded-pill text-bg-light",
          item$trend_status
        ),
        span(
          class = "badge rounded-pill text-bg-light",
          "Details →"
        )
      )
    )
  )
}

scale_card <- function(scale_row) {
  
  scale_uid_safe <- str_replace_all(scale_row$scale_uid, "[^A-Za-z0-9_]", "_")
  
  card(
    class = "scale-card",
    card_body(
      div(
        class = "d-flex justify-content-between align-items-start gap-3",
        div(
          h5(scale_row$scale),
          p(
            class = "text-muted",
            paste(
              scale_row$study,
              scale_row$year,
              "·",
              scale_row$population,
              "·",
              scale_row$instrument
            )
          )
        ),
        actionButton(
          inputId = paste0("add_scale_", scale_uid_safe),
          label = "+ Add",
          class = "btn btn-sm btn-primary"
        )
      ),
      tags$hr(),
      div(paste(scale_row$n_items, "items")),
      div(paste(scale_row$n_trend_items, "trend items")),
      div(
        class = "small",
        paste(
          "Scale variable:",
          ifelse(scale_row$scale_varname == "", "Not available", scale_row$scale_varname)
        )
      ),
      div(
        class = "mt-2",
        span(class = "badge rounded-pill text-bg-light", "View scale →")
      )
    )
  )
}

questionnaire_card <- function(questionnaire_row) {
  
  questionnaire_uid_safe <- str_replace_all(
    questionnaire_row$questionnaire_uid,
    "[^A-Za-z0-9_]",
    "_"
  )
  
  card(
    class = "questionnaire-card",
    card_body(
      div(
        class = "d-flex justify-content-between align-items-start gap-3",
        div(
          h5(
            paste(
              questionnaire_row$study,
              questionnaire_row$year,
              questionnaire_row$population,
              questionnaire_row$instrument
            )
          ),
          p(class = "text-muted", "Complete questionnaire")
        ),
        actionButton(
          inputId = paste0("add_questionnaire_", questionnaire_uid_safe),
          label = "+ Add",
          class = "btn btn-sm btn-primary"
        )
      ),
      tags$hr(),
      div(paste(questionnaire_row$n_items, "items")),
      div(paste(questionnaire_row$n_sections, "sections")),
      div(paste(questionnaire_row$n_scales, "scales")),
      div(
        class = "mt-2",
        span(class = "badge rounded-pill text-bg-light", "View questionnaire →")
      )
    )
  )
}

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------

ui <- tagList(
  
  page_navbar(
    id = "main_nav",
    title = app_brand,
    theme = app_theme,
    
    header = tags$head(
      tags$style(HTML("
        :root {
          --iea-blue: #0070b8;
          --iea-red: #e2211c;
          --iea-bg: #F7F9FC;
          --iea-border: #E5E7EB;
          --iea-text: #1F2937;
          --iea-muted: #6B7280;
        }

        body {
          background-color: var(--iea-bg);
          color: var(--iea-text);
        }

        .navbar {
          border-bottom: 1px solid #D9DEE5;
          background-color: #FFFFFF !important;
          box-shadow: 0 1px 2px rgba(0,0,0,0.03);
          padding-top: 0.6rem;
          padding-bottom: 0.6rem;
          min-height: 76px;
        }

        .navbar > .container-fluid {
          display: grid;
          grid-template-columns: auto minmax(0, 1fr) auto;
          align-items: center;
          column-gap: 3.5rem;
        }

        .navbar-brand {
          padding-top: 0;
          padding-bottom: 0;
          margin-right: 0;
          display: flex;
          align-items: center;
          min-height: 58px;
          grid-column: 1;
        }

        .app-brand {
          display: flex;
          align-items: center;
          gap: 1rem;
          min-height: 58px;
        }

        .app-logo {
          height: 48px;
          width: auto;
          display: block;
          flex: 0 0 auto;
        }

        .app-title {
          display: flex;
          align-items: center;
          justify-content: center;
          min-height: 48px;
          line-height: 1;
          margin: 0;
          padding: 0;
        }

        .app-title-main {
          font-weight: 700;
          font-size: 1.08rem;
          color: #222222;
          letter-spacing: 0.01em;
          white-space: nowrap;
          margin: 0;
          padding: 0;
          line-height: 1;
        }

        .navbar-toggler {
          grid-column: 3;
        }

        .navbar-collapse {
          grid-column: 2;
          display: flex !important;
          justify-content: center;
          align-items: center;
        }

        .navbar-nav {
          margin-left: auto;
          margin-right: auto;
          gap: 1.15rem;
          align-items: center;
        }

        .navbar-nav .nav-link {
          position: relative;
          font-weight: 600;
          color: #2F3A45 !important;
          padding-left: 0.35rem !important;
          padding-right: 0.35rem !important;
          border-radius: 0;
          background: transparent !important;
          white-space: nowrap;
        }

        .navbar-nav .nav-link:hover {
          color: var(--iea-red) !important;
        }

        .navbar-nav .nav-link.active {
          color: var(--iea-red) !important;
          font-weight: 700;
          background: transparent !important;
        }

        .btn-primary {
          background-color: var(--iea-blue);
          border-color: var(--iea-blue);
        }

        .btn-primary:hover {
          background-color: #005f9e;
          border-color: #005f9e;
        }

        .btn-outline-primary {
          color: var(--iea-blue);
          border-color: var(--iea-blue);
        }

        .btn-outline-primary:hover {
          background-color: var(--iea-blue);
          border-color: var(--iea-blue);
          color: #FFFFFF;
        }

        .page-header {
          padding: 1.5rem 0 1rem 0;
        }

        .page-header h2 {
          margin-bottom: 0.25rem;
          font-weight: 700;
          color: #1F2937;
        }

        .page-header h2::after {
          content: '';
          display: block;
          width: 44px;
          height: 3px;
          background: var(--iea-red);
          border-radius: 999px;
          margin-top: 0.55rem;
        }

        .filter-panel {
          background: #FFFFFF;
          border: 1px solid var(--iea-border);
          border-radius: 12px;
          padding: 1rem;
          position: sticky;
          top: 1rem;
        }

        .filter-panel h5 {
          color: #111827;
          font-weight: 700;
          margin-bottom: 1rem;
        }

        .item-card,
        .scale-card,
        .questionnaire-card {
          border: 1px solid var(--iea-border);
          border-radius: 14px;
          margin-bottom: 1rem;
          box-shadow: 0 1px 2px rgba(0,0,0,0.03);
          background-color: #FFFFFF;
          overflow: hidden;
        }

        .item-card::before,
        .scale-card::before,
        .questionnaire-card::before {
          content: '';
          display: block;
          height: 4px;
          background: var(--iea-red);
        }

        .item-card:hover,
        .scale-card:hover,
        .questionnaire-card:hover {
          box-shadow: 0 6px 18px rgba(0,0,0,0.07);
          transform: translateY(-1px);
          transition: box-shadow 0.15s ease-in-out, transform 0.15s ease-in-out;
        }

        .item-card h5,
        .scale-card h5,
        .questionnaire-card h5 {
          color: #111827;
          font-weight: 700;
        }

        .item-wording {
          font-size: 1.05rem;
          margin-bottom: 0;
          color: #1F2937;
        }

        .badge {
          margin-right: 0.25rem;
          border: 1px solid var(--iea-border);
          font-weight: 600;
        }

        .badge.text-bg-light {
          background-color: #F8FAFC !important;
          color: #334155 !important;
        }

        .cart-summary {
          background: #FFFFFF;
          border: 1px solid var(--iea-border);
          border-radius: 14px;
          padding: 1rem;
          margin-bottom: 1rem;
        }

        .empty-state {
          background: #FFFFFF;
          border: 1px dashed #CBD5E1;
          border-radius: 14px;
          padding: 2rem;
          text-align: center;
        }

        .home-hero {
          background: #FFFFFF;
          border: 1px solid var(--iea-border);
          border-radius: 18px;
          padding: 2.25rem;
          margin-top: 1.5rem;
          margin-bottom: 1.5rem;
          position: relative;
          overflow: hidden;
        }

        .home-hero::before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          width: 6px;
          height: 100%;
          background: var(--iea-red);
        }

        .home-hero h1 {
          color: #1F2937;
          font-weight: 750;
          max-width: 900px;
        }

        .home-hero .lead {
          color: #4B5563 !important;
        }

        .home-card {
          border: 1px solid var(--iea-border);
          border-radius: 14px;
          background-color: #FFFFFF;
          min-height: 180px;
        }

        .home-card h4 {
          color: var(--iea-blue);
          font-weight: 700;
        }

        a {
          color: var(--iea-blue);
        }

        .app-footer {
          margin-top: 2rem;
          border-top: 1px solid #D9DEE5;
          background: #FFFFFF;
          padding: 1rem 1.25rem;
        }

        .app-footer-inner {
          max-width: 1400px;
          margin: 0 auto;
          display: flex;
          flex-direction: column;
          align-items: center;
          justify-content: center;
          text-align: center;
          gap: 0.25rem;
          font-size: 0.92rem;
          color: var(--iea-muted);
        }

        .footer-line {
          width: 100%;
          text-align: center;
          line-height: 1.35;
        }

        .footer-version strong {
          color: #1F2937;
        }

        .footer-copyright {
          color: var(--iea-muted);
        }

        .version-easter-egg {
          color: var(--iea-muted) !important;
          text-decoration: none !important;
          cursor: default;
          font-weight: 500;
        }

        .version-easter-egg:hover {
          color: var(--iea-red) !important;
        }

        @media (max-width: 991px) {
          .navbar {
            min-height: 68px;
          }

          .navbar > .container-fluid {
            display: flex;
            align-items: center;
            column-gap: 1rem;
          }

          .navbar-brand {
            min-height: 50px;
          }

          .navbar-collapse {
            justify-content: flex-start;
          }

          .navbar-nav {
            margin-left: 0;
            margin-right: 0;
            gap: 0;
            align-items: flex-start;
          }

          .app-brand {
            min-height: 50px;
            gap: 0.75rem;
          }

          .app-logo {
            height: 40px;
          }

          .app-title {
            min-height: 40px;
          }

          .app-title-main {
            font-size: 1rem;
          }
        }
      ")),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('open_url', function(url) {
          window.open(url, '_blank');
        });
      "))
    ),
    
    nav_panel(
      "Home",
      div(
        class = "home-hero",
        h1("Explore questionnaire items across studies, cycles and scales"),
        p(
          class = "lead text-muted",
          "Search, compare and export item metadata from IEA studies."
        ),
        div(
          class = "mt-3",
          actionButton("go_items", "Explore items", class = "btn btn-primary"),
          actionButton("go_scales", "Browse scales", class = "btn btn-outline-primary"),
          actionButton("go_questionnaires", "Browse questionnaires", class = "btn btn-outline-primary")
        )
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        card(
          class = "home-card",
          card_body(
            h4("Items"),
            p("Search individual items, wording, response options and metadata."),
            span(class = "badge rounded-pill text-bg-light", "Item-level")
          )
        ),
        card(
          class = "home-card",
          card_body(
            h4("Scales"),
            p("Browse constructs and groups of items across cycles."),
            span(class = "badge rounded-pill text-bg-light", "Scale-level")
          )
        ),
        card(
          class = "home-card",
          card_body(
            h4("Questionnaires"),
            p("Explore complete instruments, sections and questionnaire structures."),
            span(class = "badge rounded-pill text-bg-light", "Instrument-level")
          )
        )
      )
    ),
    
    nav_panel(
      "Explore Items",
      page_header(
        "Explore Items",
        "Search and filter questionnaire items across IEA studies."
      ),
      layout_columns(
        col_widths = c(3, 9),
        div(
          class = "filter-panel",
          h5("Filters"),
          uiOutput("filter_study_items_ui"),
          uiOutput("filter_year_items_ui"),
          uiOutput("filter_cycle_items_ui"),
          uiOutput("filter_population_items_ui"),
          uiOutput("filter_instrument_items_ui"),
          uiOutput("filter_scale_items_ui"),
          uiOutput("filter_type_items_ui"),
          uiOutput("filter_puf_items_ui"),
          actionButton(
            "reset_item_filters",
            "Reset filters",
            class = "btn btn-outline-secondary btn-sm"
          )
        ),
        div(
          uiOutput("items_count"),
          textInput(
            "search_items",
            NULL,
            placeholder = "Search within items, wording, variable, scale or section..."
          ),
          uiOutput("items_results")
        )
      )
    ),
    
    nav_panel(
      "Scales",
      page_header(
        "Explore Scales",
        "Browse constructs and item groups across studies and cycles."
      ),
      layout_columns(
        col_widths = c(3, 9),
        div(
          class = "filter-panel",
          h5("Filters"),
          uiOutput("filter_study_scales_ui"),
          uiOutput("filter_year_scales_ui"),
          uiOutput("filter_cycle_scales_ui"),
          uiOutput("filter_population_scales_ui"),
          uiOutput("filter_instrument_scales_ui"),
          actionButton(
            "reset_scale_filters",
            "Reset filters",
            class = "btn btn-outline-secondary btn-sm"
          )
        ),
        div(
          uiOutput("scales_count"),
          uiOutput("scales_results")
        )
      )
    ),
    
    nav_panel(
      "Questionnaires",
      page_header(
        "Explore Questionnaires",
        "Browse complete questionnaires and instruments."
      ),
      layout_columns(
        col_widths = c(3, 9),
        div(
          class = "filter-panel",
          h5("Filters"),
          uiOutput("filter_study_questionnaires_ui"),
          uiOutput("filter_year_questionnaires_ui"),
          uiOutput("filter_cycle_questionnaires_ui"),
          uiOutput("filter_population_questionnaires_ui"),
          uiOutput("filter_instrument_questionnaires_ui"),
          actionButton(
            "reset_questionnaire_filters",
            "Reset filters",
            class = "btn btn-outline-secondary btn-sm"
          )
        ),
        div(
          uiOutput("questionnaires_count"),
          uiOutput("questionnaires_results")
        )
      )
    ),
    
    nav_panel(
      "Selection Cart",
      page_header(
        "Selection Cart",
        "Review selected unique items and export structured metadata."
      ),
      div(
        class = "cart-summary",
        uiOutput("cart_summary"),
        div(
          class = "mt-2",
          downloadButton(
            "download_excel",
            "Download Excel",
            class = "btn btn-primary"
          ),
          actionButton(
            "clear_cart",
            "Clear cart",
            class = "btn btn-outline-secondary"
          )
        )
      ),
      DTOutput("cart_table")
    )
  ),
  
  app_footer
)

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # Easter egg: 10 clicks on version opens IEA website
  # ----------------------------------------------------------
  
  version_clicks <- reactiveVal(0)
  
  observeEvent(input$version_click, {
    n <- version_clicks() + 1
    version_clicks(n)
    
    if (n >= 10) {
      version_clicks(0)
      
      showNotification(
        "Opening IEA website...",
        type = "message",
        duration = 2
      )
      
      session$sendCustomMessage(
        type = "open_url",
        message = "https://www.iea.nl"
      )
    }
  })
  
  # ----------------------------------------------------------
  # Load data from Supabase
  # ----------------------------------------------------------
  
  items_data <- reactive({
    get_items(pool)
  })
  
  scales_data <- reactive({
    derive_scales(items_data())
  })
  
  questionnaires_data <- reactive({
    derive_questionnaires(items_data())
  })
  
  # ----------------------------------------------------------
  # Navigation buttons from Home
  # ----------------------------------------------------------
  
  observeEvent(input$go_items, {
    nav_select("main_nav", "Explore Items")
  })
  
  observeEvent(input$go_scales, {
    nav_select("main_nav", "Scales")
  })
  
  observeEvent(input$go_questionnaires, {
    nav_select("main_nav", "Questionnaires")
  })
  
  # ----------------------------------------------------------
  # Cart
  # ----------------------------------------------------------
  
  cart <- reactiveVal(tibble())
  
  add_items_to_cart <- function(new_items) {
    current <- cart()
    
    updated <- bind_rows(current, new_items) %>%
      standardize_items() %>%
      distinct(item_uid, .keep_all = TRUE)
    
    cart(updated)
  }
  
  # ----------------------------------------------------------
  # Dynamic filter UI: Items
  # ----------------------------------------------------------
  
  output$filter_study_items_ui <- renderUI({
    choices <- sort(unique(na.omit(items_data()$study)))
    selectInput("filter_study_items", "Study", choices = c("All", choices))
  })
  
  output$filter_year_items_ui <- renderUI({
    choices <- sort(unique(na.omit(items_data()$year)))
    selectInput("filter_year_items", "Year", choices = c("All", choices))
  })
  
  output$filter_cycle_items_ui <- renderUI({
    choices <- sort(unique(na.omit(items_data()$cycle)))
    selectInput("filter_cycle_items", "Cycle", choices = c("All", choices))
  })
  
  output$filter_population_items_ui <- renderUI({
    choices <- sort(unique(na.omit(items_data()$population)))
    selectInput("filter_population_items", "Target", choices = c("All", choices))
  })
  
  output$filter_instrument_items_ui <- renderUI({
    choices <- sort(unique(na.omit(items_data()$instrument)))
    selectInput("filter_instrument_items", "Instrument", choices = c("All", choices))
  })
  
  output$filter_scale_items_ui <- renderUI({
    choices <- sort(unique(na.omit(items_data()$scale)))
    selectInput("filter_scale_items", "Scale", choices = c("All", choices))
  })
  
  output$filter_type_items_ui <- renderUI({
    choices <- sort(unique(na.omit(items_data()$item_type)))
    selectInput("filter_type_items", "Item type", choices = c("All", choices))
  })
  
  output$filter_puf_items_ui <- renderUI({
    selectInput(
      "filter_puf_items",
      "Public use file",
      choices = c("All", "TRUE", "FALSE")
    )
  })
  
  # ----------------------------------------------------------
  # Filtered items
  # ----------------------------------------------------------
  
  filtered_items <- reactive({
    df <- items_data()
    
    if (!is.null(input$filter_study_items) && input$filter_study_items != "All") {
      df <- df %>% filter(study == input$filter_study_items)
    }
    
    if (!is.null(input$filter_year_items) && input$filter_year_items != "All") {
      df <- df %>% filter(year == input$filter_year_items)
    }
    
    if (!is.null(input$filter_cycle_items) && input$filter_cycle_items != "All") {
      df <- df %>% filter(cycle == input$filter_cycle_items)
    }
    
    if (!is.null(input$filter_population_items) && input$filter_population_items != "All") {
      df <- df %>% filter(population == input$filter_population_items)
    }
    
    if (!is.null(input$filter_instrument_items) && input$filter_instrument_items != "All") {
      df <- df %>% filter(instrument == input$filter_instrument_items)
    }
    
    if (!is.null(input$filter_scale_items) && input$filter_scale_items != "All") {
      df <- df %>% filter(scale == input$filter_scale_items)
    }
    
    if (!is.null(input$filter_type_items) && input$filter_type_items != "All") {
      df <- df %>% filter(item_type == input$filter_type_items)
    }
    
    if (!is.null(input$filter_puf_items) && input$filter_puf_items != "All") {
      df <- df %>% filter(as.character(puf) == input$filter_puf_items)
    }
    
    if (!is.null(input$search_items) && nzchar(input$search_items)) {
      q <- tolower(input$search_items)
      
      df <- df %>%
        filter(
          grepl(q, tolower(item_code), fixed = TRUE) |
            grepl(q, tolower(wording), fixed = TRUE) |
            grepl(q, tolower(source_variable), fixed = TRUE) |
            grepl(q, tolower(scale), fixed = TRUE) |
            grepl(q, tolower(scale_varname), fixed = TRUE) |
            grepl(q, tolower(section), fixed = TRUE) |
            grepl(q, tolower(dataset_label), fixed = TRUE)
        )
    }
    
    df
  })
  
  output$items_count <- renderUI({
    h5(paste(nrow(filtered_items()), "items found"))
  })
  
  output$items_results <- renderUI({
    df <- filtered_items()
    
    if (nrow(df) == 0) {
      return(empty_state("No items found."))
    }
    
    tagList(
      lapply(seq_len(nrow(df)), function(i) {
        item_card(df[i, ])
      })
    )
  })
  
  observeEvent(input$reset_item_filters, {
    updateSelectInput(session, "filter_study_items", selected = "All")
    updateSelectInput(session, "filter_year_items", selected = "All")
    updateSelectInput(session, "filter_cycle_items", selected = "All")
    updateSelectInput(session, "filter_population_items", selected = "All")
    updateSelectInput(session, "filter_instrument_items", selected = "All")
    updateSelectInput(session, "filter_scale_items", selected = "All")
    updateSelectInput(session, "filter_type_items", selected = "All")
    updateSelectInput(session, "filter_puf_items", selected = "All")
    updateTextInput(session, "search_items", value = "")
  })
  
  observe({
    df <- items_data()
    
    if (nrow(df) == 0) {
      return(NULL)
    }
    
    lapply(seq_len(nrow(df)), function(i) {
      local({
        item <- df[i, ]
        item_uid_safe <- str_replace_all(item$item_uid, "[^A-Za-z0-9_]", "_")
        
        observeEvent(input[[paste0("add_item_", item_uid_safe)]], {
          add_items_to_cart(item)
        }, ignoreInit = TRUE)
      })
    })
  })
  
  # ----------------------------------------------------------
  # Dynamic filter UI: Scales
  # ----------------------------------------------------------
  
  output$filter_study_scales_ui <- renderUI({
    choices <- sort(unique(na.omit(scales_data()$study)))
    selectInput("filter_study_scales", "Study", choices = c("All", choices))
  })
  
  output$filter_year_scales_ui <- renderUI({
    choices <- sort(unique(na.omit(scales_data()$year)))
    selectInput("filter_year_scales", "Year", choices = c("All", choices))
  })
  
  output$filter_cycle_scales_ui <- renderUI({
    choices <- sort(unique(na.omit(scales_data()$cycle)))
    selectInput("filter_cycle_scales", "Cycle", choices = c("All", choices))
  })
  
  output$filter_population_scales_ui <- renderUI({
    choices <- sort(unique(na.omit(scales_data()$population)))
    selectInput("filter_population_scales", "Target", choices = c("All", choices))
  })
  
  output$filter_instrument_scales_ui <- renderUI({
    choices <- sort(unique(na.omit(scales_data()$instrument)))
    selectInput("filter_instrument_scales", "Instrument", choices = c("All", choices))
  })
  
  # ----------------------------------------------------------
  # Filtered scales
  # ----------------------------------------------------------
  
  filtered_scales <- reactive({
    df <- scales_data()
    
    if (!is.null(input$filter_study_scales) && input$filter_study_scales != "All") {
      df <- df %>% filter(study == input$filter_study_scales)
    }
    
    if (!is.null(input$filter_year_scales) && input$filter_year_scales != "All") {
      df <- df %>% filter(year == input$filter_year_scales)
    }
    
    if (!is.null(input$filter_cycle_scales) && input$filter_cycle_scales != "All") {
      df <- df %>% filter(cycle == input$filter_cycle_scales)
    }
    
    if (!is.null(input$filter_population_scales) && input$filter_population_scales != "All") {
      df <- df %>% filter(population == input$filter_population_scales)
    }
    
    if (!is.null(input$filter_instrument_scales) && input$filter_instrument_scales != "All") {
      df <- df %>% filter(instrument == input$filter_instrument_scales)
    }
    
    df
  })
  
  output$scales_count <- renderUI({
    h5(paste(nrow(filtered_scales()), "scales found"))
  })
  
  output$scales_results <- renderUI({
    df <- filtered_scales()
    
    if (nrow(df) == 0) {
      return(empty_state("No scales found."))
    }
    
    tagList(
      lapply(seq_len(nrow(df)), function(i) {
        scale_card(df[i, ])
      })
    )
  })
  
  observeEvent(input$reset_scale_filters, {
    updateSelectInput(session, "filter_study_scales", selected = "All")
    updateSelectInput(session, "filter_year_scales", selected = "All")
    updateSelectInput(session, "filter_cycle_scales", selected = "All")
    updateSelectInput(session, "filter_population_scales", selected = "All")
    updateSelectInput(session, "filter_instrument_scales", selected = "All")
  })
  
  observe({
    df <- scales_data()
    
    if (nrow(df) == 0) {
      return(NULL)
    }
    
    lapply(seq_len(nrow(df)), function(i) {
      local({
        scale_row <- df[i, ]
        scale_uid_safe <- str_replace_all(scale_row$scale_uid, "[^A-Za-z0-9_]", "_")
        
        observeEvent(input[[paste0("add_scale_", scale_uid_safe)]], {
          selected_items <- items_data() %>%
            filter(
              study == scale_row$study,
              year == scale_row$year,
              cycle == scale_row$cycle,
              population == scale_row$population,
              instrument == scale_row$instrument,
              scale_id == scale_row$scale_id
            )
          
          add_items_to_cart(selected_items)
        }, ignoreInit = TRUE)
      })
    })
  })
  
  # ----------------------------------------------------------
  # Dynamic filter UI: Questionnaires
  # ----------------------------------------------------------
  
  output$filter_study_questionnaires_ui <- renderUI({
    choices <- sort(unique(na.omit(questionnaires_data()$study)))
    selectInput("filter_study_questionnaires", "Study", choices = c("All", choices))
  })
  
  output$filter_year_questionnaires_ui <- renderUI({
    choices <- sort(unique(na.omit(questionnaires_data()$year)))
    selectInput("filter_year_questionnaires", "Year", choices = c("All", choices))
  })
  
  output$filter_cycle_questionnaires_ui <- renderUI({
    choices <- sort(unique(na.omit(questionnaires_data()$cycle)))
    selectInput("filter_cycle_questionnaires", "Cycle", choices = c("All", choices))
  })
  
  output$filter_population_questionnaires_ui <- renderUI({
    choices <- sort(unique(na.omit(questionnaires_data()$population)))
    selectInput("filter_population_questionnaires", "Target", choices = c("All", choices))
  })
  
  output$filter_instrument_questionnaires_ui <- renderUI({
    choices <- sort(unique(na.omit(questionnaires_data()$instrument)))
    selectInput("filter_instrument_questionnaires", "Instrument", choices = c("All", choices))
  })
  
  # ----------------------------------------------------------
  # Filtered questionnaires
  # ----------------------------------------------------------
  
  filtered_questionnaires <- reactive({
    df <- questionnaires_data()
    
    if (!is.null(input$filter_study_questionnaires) && input$filter_study_questionnaires != "All") {
      df <- df %>% filter(study == input$filter_study_questionnaires)
    }
    
    if (!is.null(input$filter_year_questionnaires) && input$filter_year_questionnaires != "All") {
      df <- df %>% filter(year == input$filter_year_questionnaires)
    }
    
    if (!is.null(input$filter_cycle_questionnaires) && input$filter_cycle_questionnaires != "All") {
      df <- df %>% filter(cycle == input$filter_cycle_questionnaires)
    }
    
    if (!is.null(input$filter_population_questionnaires) && input$filter_population_questionnaires != "All") {
      df <- df %>% filter(population == input$filter_population_questionnaires)
    }
    
    if (!is.null(input$filter_instrument_questionnaires) && input$filter_instrument_questionnaires != "All") {
      df <- df %>% filter(instrument == input$filter_instrument_questionnaires)
    }
    
    df
  })
  
  output$questionnaires_count <- renderUI({
    h5(paste(nrow(filtered_questionnaires()), "questionnaires found"))
  })
  
  output$questionnaires_results <- renderUI({
    df <- filtered_questionnaires()
    
    if (nrow(df) == 0) {
      return(empty_state("No questionnaires found."))
    }
    
    tagList(
      lapply(seq_len(nrow(df)), function(i) {
        questionnaire_card(df[i, ])
      })
    )
  })
  
  observeEvent(input$reset_questionnaire_filters, {
    updateSelectInput(session, "filter_study_questionnaires", selected = "All")
    updateSelectInput(session, "filter_year_questionnaires", selected = "All")
    updateSelectInput(session, "filter_cycle_questionnaires", selected = "All")
    updateSelectInput(session, "filter_population_questionnaires", selected = "All")
    updateSelectInput(session, "filter_instrument_questionnaires", selected = "All")
  })
  
  observe({
    df <- questionnaires_data()
    
    if (nrow(df) == 0) {
      return(NULL)
    }
    
    lapply(seq_len(nrow(df)), function(i) {
      local({
        questionnaire_row <- df[i, ]
        questionnaire_uid_safe <- str_replace_all(
          questionnaire_row$questionnaire_uid,
          "[^A-Za-z0-9_]",
          "_"
        )
        
        observeEvent(input[[paste0("add_questionnaire_", questionnaire_uid_safe)]], {
          selected_items <- items_data() %>%
            filter(
              study == questionnaire_row$study,
              year == questionnaire_row$year,
              cycle == questionnaire_row$cycle,
              population == questionnaire_row$population,
              instrument == questionnaire_row$instrument
            )
          
          add_items_to_cart(selected_items)
        }, ignoreInit = TRUE)
      })
    })
  })
  
  # ----------------------------------------------------------
  # Cart outputs
  # ----------------------------------------------------------
  
  output$cart_summary <- renderUI({
    n <- nrow(cart())
    
    if (n == 0) {
      p("No items selected yet.", class = "text-muted")
    } else {
      h5(paste(n, "unique items selected"))
    }
  })
  
  output$cart_table <- renderDT({
    df <- cart()
    
    if (nrow(df) == 0) {
      return(
        datatable(
          tibble(
            item_code = character(),
            wording = character(),
            study = character(),
            year = character(),
            cycle = character(),
            population = character(),
            instrument = character(),
            scale = character()
          ),
          rownames = FALSE,
          options = list(pageLength = 10, scrollX = TRUE)
        )
      )
    }
    
    df %>%
      select(
        item_code,
        wording,
        source_variable,
        study,
        phase,
        year,
        cycle,
        population,
        instrument,
        scale,
        scale_varname,
        item_type,
        dataset_label,
        puf
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      )
  })
  
  observeEvent(input$clear_cart, {
    cart(tibble())
  })
  
  # ----------------------------------------------------------
  # Excel export
  # ----------------------------------------------------------
  
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("iea_item_bank_selection_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      
      selected <- cart() %>%
        standardize_items()
      
      selected_item_uids <- selected$item_uid
      
      item_history <- get_item_history(pool, selected_item_uids)
      
      item_variants <- get_item_variants(pool, selected_item_uids)
      
      scale_membership <- get_scale_membership(
        pool,
        selected_item_uids
      )
      
      response_options <- get_response_options(
        pool,
        selected
      )
      
      missing_values <- get_missing_values(
        pool,
        selected
      )
      
      variables_and_data <- get_variables_and_data(
        pool,
        selected
      )
      
      export_metadata <- tibble(
        field = c(
          "export_date",
          "source",
          "app_version",
          "number_of_selected_items"
        ),
        value = c(
          as.character(Sys.Date()),
          "IEA Item Bank Explorer",
          APP_VERSION,
          as.character(nrow(selected))
        )
      )
      
      wb <- createWorkbook()
      
      addWorksheet(wb, "Selected_Items")
      writeData(wb, "Selected_Items", selected)
      
      addWorksheet(wb, "Item_History")
      writeData(wb, "Item_History", item_history)
      
      addWorksheet(wb, "Item_Variants")
      writeData(wb, "Item_Variants", item_variants)
      
      addWorksheet(wb, "Scale_Membership")
      writeData(wb, "Scale_Membership", scale_membership)
      
      addWorksheet(wb, "Response_Options")
      writeData(wb, "Response_Options", response_options)
      
      addWorksheet(wb, "Missing_Values")
      writeData(wb, "Missing_Values", missing_values)
      
      addWorksheet(wb, "Variables_and_Data")
      writeData(wb, "Variables_and_Data", variables_and_data)
      
      addWorksheet(wb, "Export_Metadata")
      writeData(wb, "Export_Metadata", export_metadata)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

# ------------------------------------------------------------
# Run app
# ------------------------------------------------------------

shinyApp(ui, server)