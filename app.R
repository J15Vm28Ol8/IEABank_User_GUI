# ============================================================
# app.R — Explorador de solo-lectura del banco de ítems (ICILS)
# ------------------------------------------------------------
# Muestra item_admin (columnas seleccionadas) con:
#   * filtros por clic derivados de la tabla admin (study, phase, year,
#     instrument, cycle),
#   * filtros de columna nativos de DT (incl. item_uid para rastrear
#     versiones de un mismo ítem a través de administraciones),
#   * un botón por fila que abre el esquema de respuesta (value -> label).
#
# Conexión: rol de solo-lectura vía session pooler de Supabase.
# Credenciales por variables de entorno (.Renviron o panel de hosting):
#   SUPABASE_HOST, SUPABASE_PORT, SUPABASE_DB, SUPABASE_RO_USER, SUPABASE_RO_PWD
# ============================================================

# --- Paquetes (solo los necesarios) ---
library(shiny)
library(DBI)
library(RPostgres)
library(pool)
library(DT)

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
# Carga de datos (una vez al arrancar; el banco es chico y cambia poco)
# ------------------------------------------------------------
item_data <- dbGetQuery(pool, "
  select ia.item_uid, ia.item_admin_id, ia.admin_id,
         ia.varname, ia.dataset_label, ia.wording_question, ia.wording_item,
         ia.response_id,
         a.study, a.phase, a.year, a.instrument, a.cycle
  from   item_admin ia
  join   admin a on a.admin_id = ia.admin_id
  order by ia.item_uid, ia.admin_id")

response_scheme <- dbGetQuery(pool, "
  select response_id, value, label
  from   value_scheme_value
  order by response_id, value")

# Columnas visibles en la grilla (las siete pedidas).
display_cols <- c("item_uid", "item_admin_id", "admin_id", "varname",
                  "dataset_label", "wording_question", "wording_item")

# Helper: choices ordenados y únicos para un filtro.
choices_of <- function(x) sort(unique(x))

# Helper: botón "Ver" por fila que envía su response_id a Shiny al pulsarlo.
scheme_button <- function(rid) {
  sprintf(
    '<button class="btn btn-default btn-sm" onclick="Shiny.setInputValue(\'scheme_click\', \'%s\', {priority: \'event\'})">Ver</button>',
    rid
  )
}

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Banco de ítems — explorador de solo-lectura"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectizeInput("f_study", "Study", choices_of(item_data$study),
                     multiple = TRUE, options = list(placeholder = "Todos")),
      selectizeInput("f_phase", "Phase", choices_of(item_data$phase),
                     multiple = TRUE, options = list(placeholder = "Todas")),
      selectizeInput("f_year", "Year", choices_of(item_data$year),
                     multiple = TRUE, options = list(placeholder = "Todos")),
      selectizeInput("f_instrument", "Instrument", choices_of(item_data$instrument),
                     multiple = TRUE, options = list(placeholder = "Todos")),
      selectizeInput("f_cycle", "Cycle", choices_of(item_data$cycle),
                     multiple = TRUE, options = list(placeholder = "Todos")),
      actionButton("clear", "Limpiar filtros")
    ),
    mainPanel(
      width = 9,
      DTOutput("tbl")
    )
  )
)

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- Filtrado por los selectores de admin (vacío = sin filtro) ---
  filtered <- reactive({
    d <- item_data
    if (length(input$f_study))      d <- d[d$study %in% input$f_study, ]
    if (length(input$f_phase))      d <- d[d$phase %in% input$f_phase, ]
    if (length(input$f_year))       d <- d[as.character(d$year) %in% input$f_year, ]
    if (length(input$f_instrument)) d <- d[d$instrument %in% input$f_instrument, ]
    if (length(input$f_cycle))      d <- d[d$cycle %in% input$f_cycle, ]
    d
  })
  
  # --- Tabla: columnas visibles + botón de esquema, sin buscador ni paginado ---
  output$tbl <- renderDT({
    d <- filtered()
    tbl <- d[, display_cols]
    tbl$Esquema <- vapply(d$response_id, scheme_button, character(1))
    
    datatable(
      tbl,
      filter    = "top",          # filtros por columna (incl. item_uid)
      selection = "none",
      rownames  = FALSE,
      escape    = seq_along(display_cols),  # escapa el texto; deja el botón como HTML
      options   = list(
        dom         = "t",        # quita buscador global y selector "Show entries"
        paging      = FALSE,      # muestra la tabla completa
        scrollX     = TRUE,
        scrollY     = "65vh",     # con scroll vertical para que no crezca sin fin
        scrollCollapse = TRUE,
        columnDefs  = list(list(  # la columna del botón no se filtra ni se ordena
          targets = length(display_cols), searchable = FALSE, orderable = FALSE
        ))
      )
    )
  }, server = TRUE)
  
  # --- Limpiar filtros ---
  observeEvent(input$clear, {
    for (id in c("f_study", "f_phase", "f_year", "f_instrument", "f_cycle"))
      updateSelectizeInput(session, id, selected = character(0))
  })
  
  # --- Pop-up del esquema de respuesta (disparado por el botón de la fila) ---
  current_rid <- reactiveVal(NULL)
  
  observeEvent(input$scheme_click, {
    current_rid(input$scheme_click)
    showModal(modalDialog(
      title     = paste("Esquema de respuesta:", input$scheme_click),
      tableOutput("scheme_tbl"),
      easyClose = TRUE,
      footer    = modalButton("Cerrar")
    ))
  })
  
  output$scheme_tbl <- renderTable({
    req(current_rid())
    response_scheme[response_scheme$response_id == current_rid(), c("value", "label")]
  }, colnames = TRUE)
}

shinyApp(ui, server)