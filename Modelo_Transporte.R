# ============================================================
#  MODELO DE TRANSPORTE — Aplicación Shiny con GUI corregida
#  Instalar con: install.packages(c("shiny","lpSolve","igraph","DT","shinyjs"))
#  Ejecutar con: shiny::runApp("transporte_shiny.R")
#               o source("transporte_shiny.R")
# ============================================================

# ── Dependencias ─────────────────────────────────────────────
paquetes <- c("shiny", "lpSolve", "igraph", "DT", "shinyjs")

for (p in paquetes) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}

library(shiny)
library(lpSolve)
library(igraph)
library(DT)
library(shinyjs)

# ============================================================
#  Helpers
# ============================================================
`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0) a else b
}

calcular_multiplicadores <- function(asig, costos, m, n) {
  u <- rep(NA_real_, m)
  v <- rep(NA_real_, n)
  
  names(u) <- rownames(costos)
  names(v) <- colnames(costos)
  
  u[1] <- 0
  
  for (iter in seq_len((m + n) * 30)) {
    for (i in seq_len(m)) {
      for (j in seq_len(n)) {
        if (asig[i, j] > 0) {
          if (!is.na(u[i]) && is.na(v[j])) {
            v[j] <- costos[i, j] - u[i]
          } else if (is.na(u[i]) && !is.na(v[j])) {
            u[i] <- costos[i, j] - v[j]
          }
        }
      }
    }
    
    if (!any(is.na(u)) && !any(is.na(v))) {
      break
    }
  }
  
  # Evita errores visuales si la solución es degenerada.
  u[is.na(u)] <- 0
  v[is.na(v)] <- 0
  
  list(u = u, v = v)
}

# ============================================================
#  UI
# ============================================================
ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&family=JetBrains+Mono:wght@400;500;600;700&display=swap",
      rel  = "stylesheet"
    ),
    
    tags$style(HTML("
      /* ============================================================
         RESET GENERAL
         ============================================================ */

      *, *::before, *::after {
        box-sizing: border-box;
      }

      html, body {
        width: 100%;
        min-height: 100%;
        margin: 0;
        padding: 0;
        overflow-x: hidden;
      }

      body {
        font-family: 'Inter', system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        background: #0b0f14;
        color: #e5e7eb;
        letter-spacing: normal;
      }

      .container-fluid {
        padding-left: 0 !important;
        padding-right: 0 !important;
      }

      /* ============================================================
         HEADER
         ============================================================ */

      .app-header {
        background: linear-gradient(135deg, #0b0f14 0%, #111827 100%);
        border-bottom: 1px solid #253041;
        padding: 24px 40px 18px;
        position: sticky;
        top: 0;
        z-index: 100;
      }

      .app-title {
        font-family: 'Inter', sans-serif;
        font-size: clamp(22px, 2vw, 30px);
        font-weight: 800;
        letter-spacing: -0.03em;
        line-height: 1.15;
        color: #f0fdf4;
      }

      .app-title span {
        color: #4ade80;
      }

      .app-sub {
        font-family: 'JetBrains Mono', monospace;
        font-size: 11px;
        letter-spacing: 0.06em;
        color: #7b8494;
        margin-top: 6px;
      }

      /* ============================================================
         LAYOUT
         ============================================================ */

      .main-wrap {
        display: grid;
        grid-template-columns: minmax(300px, 360px) minmax(0, 1fr);
        min-height: calc(100vh - 88px);
      }

      .sidebar-panel,
      .content-panel {
        min-width: 0;
      }

      .sidebar-panel {
        background: #0f141b;
        border-right: 1px solid #253041;
        padding: 28px 22px;
        overflow-y: auto;
      }

      .content-panel {
        background: #0b0f14;
        padding: 32px clamp(20px, 3vw, 42px);
        overflow-x: hidden;
        overflow-y: auto;
      }

      /* ============================================================
         SECCIONES Y TARJETAS
         ============================================================ */

      .section-title {
        display: flex;
        align-items: center;
        gap: 10px;
        font-family: 'Inter', sans-serif;
        font-size: 12px;
        font-weight: 800;
        letter-spacing: 0.12em;
        text-transform: uppercase;
        color: #35d07f;
        margin-bottom: 16px;
      }

      .section-title::after {
        content: '';
        flex: 1;
        height: 1px;
        background: #253041;
      }

      .card {
        background: #111821;
        border: 1px solid #263241;
        border-radius: 14px;
        padding: 22px;
        margin-bottom: 22px;
        box-shadow: 0 10px 30px rgba(0, 0, 0, .18);
        overflow: hidden;
      }

      .card-header {
        font-family: 'Inter', sans-serif;
        font-size: 15px;
        font-weight: 700;
        color: #d9fbe7;
        margin-bottom: 16px;
        letter-spacing: -0.01em;
      }

      /* ============================================================
         INPUTS
         ============================================================ */

      .shiny-input-container {
        width: 100% !important;
        max-width: 100% !important;
        min-width: 0 !important;
        margin-bottom: 0 !important;
      }

      .form-group {
        margin-bottom: 12px !important;
        min-width: 0 !important;
      }

      .form-group label,
      label {
        font-family: 'Inter', sans-serif;
        font-size: 12px !important;
        font-weight: 700 !important;
        letter-spacing: 0.04em !important;
        text-transform: uppercase;
        color: #9ca3af !important;
        margin-bottom: 7px !important;
      }

      .form-control,
      input[type=number],
      input[type=text],
      select.form-control {
        width: 100% !important;
        min-width: 0 !important;
        height: 42px !important;
        background: #151c25 !important;
        border: 1px solid #2d3948 !important;
        border-radius: 10px !important;
        color: #f3f4f6 !important;
        font-family: 'Inter', sans-serif !important;
        font-size: 14px !important;
        font-weight: 500 !important;
        letter-spacing: normal !important;
        padding: 9px 12px !important;
        box-shadow: none !important;
        transition: border-color .18s ease, box-shadow .18s ease, background .18s ease;
      }

      .form-control:focus,
      input[type=number]:focus,
      input[type=text]:focus {
        background: #17212d !important;
        border-color: #35d07f !important;
        box-shadow: 0 0 0 3px rgba(53, 208, 127, .16) !important;
        outline: none !important;
      }

      .two-col-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 14px;
      }

      .node-name-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 14px;
      }

      .input-stack {
        display: grid;
        gap: 14px;
      }

      /* ============================================================
         BOTONES
         ============================================================ */

      .btn-primary-custom {
        background: #22c55e !important;
        color: #07110b !important;
        border: none !important;
        border-radius: 12px !important;
        font-family: 'Inter', sans-serif !important;
        font-weight: 800 !important;
        font-size: 14px !important;
        letter-spacing: normal !important;
        padding: 12px 18px !important;
        width: 100%;
        transition: transform .18s ease, box-shadow .18s ease, background .18s ease;
      }

      .btn-primary-custom:hover {
        background: #4ade80 !important;
        transform: translateY(-1px);
        box-shadow: 0 12px 26px rgba(34, 197, 94, .22);
      }

      .btn-primary-custom:focus {
        outline: none !important;
        box-shadow: 0 0 0 3px rgba(34, 197, 94, .25) !important;
      }

      /* ============================================================
         MATRIZ DE COSTOS
         ============================================================ */

      .matrix-scroll {
        width: 100%;
        overflow-x: auto;
        padding-bottom: 6px;
      }

      .matrix-scroll::-webkit-scrollbar {
        height: 8px;
      }

      .matrix-scroll::-webkit-scrollbar-track {
        background: #111821;
      }

      .matrix-scroll::-webkit-scrollbar-thumb {
        background: #334155;
        border-radius: 999px;
      }

      .matrix-grid {
        display: grid;
        gap: 12px;
        margin-top: 8px;
        width: 100%;
        min-width: 0;
        align-items: center;
      }

      .matrix-grid > * {
        min-width: 0;
      }

      .matrix-cell {
        min-width: 0;
      }

      .matrix-cell .form-group {
        margin: 0 !important;
      }

      .matrix-cell input {
        width: 100% !important;
        min-width: 0 !important;
        height: 42px !important;
        text-align: center;
        background: #151c25 !important;
        border: 1px solid #2d3948 !important;
        border-radius: 10px !important;
        color: #f3f4f6 !important;
        font-family: 'JetBrains Mono', monospace !important;
        font-size: 14px !important;
        font-weight: 600 !important;
        padding: 8px 10px !important;
      }

      .matrix-cell input:focus {
        background: #13241c !important;
        border-color: #35d07f !important;
        box-shadow: 0 0 0 3px rgba(53, 208, 127, .16) !important;
      }

      .matrix-header-cell {
        font-family: 'Inter', sans-serif;
        font-size: 12px;
        font-weight: 800;
        letter-spacing: 0.04em;
        text-transform: uppercase;
        color: #35d07f;
        text-align: center;
        padding: 8px 4px;
        white-space: nowrap;
      }

      .matrix-row-label {
        font-family: 'Inter', sans-serif;
        font-size: 13px;
        font-weight: 800;
        color: #cbd5e1;
        display: flex;
        align-items: center;
        min-width: 0;
        white-space: nowrap;
      }

      /* ============================================================
         OFERTA Y DEMANDA
         ============================================================ */

      .supply-demand-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 28px;
      }

      /* ============================================================
         BADGE DE BALANCEO
         ============================================================ */

      .balance-badge {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        max-width: 100%;
        background: #12301e;
        border: 1px solid #166534;
        border-radius: 999px;
        padding: 7px 13px;
        font-size: 12px;
        color: #4ade80;
        font-weight: 700;
        line-height: 1.3;
        flex-wrap: wrap;
      }

      .balance-badge.warn {
        background: #302412;
        border-color: #92400e;
        color: #fbbf24;
      }

      /* ============================================================
         KPIS
         ============================================================ */

      .kpi-row {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 16px;
        margin-bottom: 24px;
      }

      .kpi-card {
        background: #111821;
        border: 1px solid #263241;
        border-radius: 14px;
        padding: 20px;
      }

      .kpi-label {
        font-family: 'Inter', sans-serif;
        font-size: 11px;
        letter-spacing: 0.08em;
        text-transform: uppercase;
        color: #8b95a5;
        margin-bottom: 10px;
      }

      .kpi-value {
        font-family: 'JetBrains Mono', monospace;
        font-size: clamp(24px, 2.5vw, 34px);
        font-weight: 800;
        color: #4ade80;
        line-height: 1.1;
      }

      .kpi-sub {
        font-size: 12px;
        color: #7b8494;
        margin-top: 6px;
      }

      /* ============================================================
         TABS
         ============================================================ */

      .nav-tabs {
        border-bottom: 1px solid #263241;
        margin-bottom: 24px;
      }

      .nav-tabs > li > a {
        font-family: 'Inter', sans-serif;
        font-size: 13px;
        font-weight: 700;
        letter-spacing: normal;
        color: #8b95a5;
        padding: 12px 18px;
        border: none !important;
        border-bottom: 2px solid transparent !important;
        background: transparent !important;
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li > a:hover {
        background: transparent !important;
        border: none !important;
        border-bottom: 2px solid #35d07f !important;
        color: #35d07f !important;
      }

      .tab-content {
        border: none;
        background: transparent;
      }

      /* ============================================================
         TABLAS
         ============================================================ */

      table {
        width: 100%;
      }

      .table {
        color: #d1d5db;
        background: transparent;
        border-color: #263241;
      }

      .table > thead > tr > th,
      .table > tbody > tr > th,
      .table > tfoot > tr > th {
        background: #151c25;
        color: #4ade80;
        border-color: #263241 !important;
        font-family: 'Inter', sans-serif !important;
        font-size: 12px !important;
        font-weight: 800;
        letter-spacing: 0.04em !important;
        text-transform: uppercase;
        text-align: center;
      }

      .table > tbody > tr > td {
        background: #111821;
        color: #d1d5db;
        border-color: #263241 !important;
        font-family: 'Inter', sans-serif !important;
        font-size: 13px !important;
        text-align: center;
      }

      .dataTables_wrapper {
        color: #d1d5db;
        width: 100%;
        overflow-x: auto;
      }

      table.dataTable {
        font-family: 'Inter', sans-serif !important;
        width: 100% !important;
      }

      table.dataTable thead th {
        background: #151c25;
        color: #4ade80;
        border-bottom: 1px solid #263241;
        font-family: 'Inter', sans-serif !important;
        font-size: 12px !important;
        font-weight: 800;
        letter-spacing: 0.04em !important;
        text-transform: uppercase;
      }

      table.dataTable tbody tr {
        background: #111821;
      }

      table.dataTable tbody tr:nth-child(even) {
        background: #0f141b;
      }

      table.dataTable tbody td {
        color: #d1d5db;
        border-color: #263241;
        font-family: 'Inter', sans-serif !important;
        font-size: 13px !important;
      }

      .dataTables_info,
      .dataTables_paginate {
        color: #8b95a5;
        font-size: 12px;
      }

      .paginate_button {
        color: #8b95a5 !important;
      }

      .paginate_button.current {
        background: #12301e !important;
        color: #4ade80 !important;
        border-color: #166534 !important;
      }

      /* ============================================================
         GRÁFICO Y STATUS
         ============================================================ */

      .plot-wrap {
        background: #111821;
        border: 1px solid #263241;
        border-radius: 14px;
        overflow: hidden;
        padding: 8px;
      }

      .status-bar {
        background: #080b10;
        border-top: 1px solid #253041;
        padding: 10px 40px;
        font-size: 12px;
        color: #7b8494;
        font-family: 'JetBrains Mono', monospace;
        display: flex;
        gap: 24px;
        flex-wrap: wrap;
      }

      /* ============================================================
         LOADING
         ============================================================ */

      #loading-overlay {
        position: fixed;
        inset: 0;
        background: rgba(0, 0, 0, .70);
        display: none;
        align-items: center;
        justify-content: center;
        z-index: 999;
      }

      .spinner {
        width: 42px;
        height: 42px;
        border: 3px solid #253041;
        border-top-color: #4ade80;
        border-radius: 50%;
        animation: spin .8s linear infinite;
      }

      @keyframes spin {
        to {
          transform: rotate(360deg);
        }
      }

      /* ============================================================
         RESPONSIVE
         ============================================================ */

      @media (max-width: 1100px) {
        .main-wrap {
          grid-template-columns: 1fr;
        }

        .sidebar-panel {
          border-right: none;
          border-bottom: 1px solid #253041;
        }

        .content-panel {
          padding: 26px 20px;
        }

        .kpi-row,
        .supply-demand-grid {
          grid-template-columns: 1fr;
        }
      }

      @media (max-width: 640px) {
        .app-header {
          padding: 22px 18px 16px;
        }

        .sidebar-panel {
          padding: 22px 16px;
        }

        .content-panel {
          padding: 22px 16px;
        }

        .two-col-grid,
        .node-name-grid {
          grid-template-columns: 1fr;
        }

        .matrix-grid {
          gap: 10px;
        }

        .status-bar {
          padding: 10px 18px;
        }
      }
    "))
  ),
  
  # ── Header ─────────────────────────────────────────────────
  div(
    class = "app-header",
    div(
      class = "app-title",
      "Modelo de ", tags$span("Transporte"), " — Optimización Logística"
    ),
    div(
      class = "app-sub",
      "MÉTODO COSTO MÍNIMO + MODI + ANÁLISIS DE SENSIBILIDAD"
    )
  ),
  
  div(
    id = "loading-overlay",
    div(class = "spinner")
  ),
  
  # ── Main layout ─────────────────────────────────────────────
  div(
    class = "main-wrap",
    
    # ── Sidebar ───────────────────────────────────────────────
    div(
      class = "sidebar-panel",
      
      div(class = "section-title", "Configuración"),
      
      div(
        class = "card",
        div(class = "card-header", "Dimensiones del problema"),
        
        div(
          class = "two-col-grid",
          numericInput(
            inputId = "num_origenes",
            label   = "Orígenes",
            value   = 3,
            min     = 1,
            max     = 8,
            step    = 1
          ),
          numericInput(
            inputId = "num_destinos",
            label   = "Destinos",
            value   = 4,
            min     = 1,
            max     = 8,
            step    = 1
          )
        ),
        
        br(),
        
        actionButton(
          inputId = "btn_config",
          label   = "Generar formulario →",
          class   = "btn-primary-custom"
        )
      ),
      
      uiOutput("ui_nombres"),
      uiOutput("ui_balanceo"),
      
      br(),
      
      actionButton(
        inputId = "btn_resolver",
        label   = "Resolver modelo",
        class   = "btn-primary-custom"
      )
    ),
    
    # ── Content ───────────────────────────────────────────────
    div(
      class = "content-panel",
      uiOutput("ui_matrices"),
      uiOutput("ui_resultados")
    )
  ),
  
  # ── Status bar ─────────────────────────────────────────────
  div(
    class = "status-bar",
    span("Estado: ", textOutput("status_txt", inline = TRUE)),
    span("Método: MODI + lpSolve"),
    span("v1.1")
  )
)

# ============================================================
#  SERVER
# ============================================================
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    configurado  = FALSE,
    resuelto     = FALSE,
    costos       = NULL,
    oferta       = NULL,
    demanda      = NULL,
    asig         = NULL,
    Z_opt        = NULL,
    u_vec        = NULL,
    v_vec        = NULL,
    delta        = NULL,
    origenes_a   = NULL,
    destinos_a   = NULL,
    flujos_a     = NULL,
    costos_a     = NULL
  )
  
  # ── Generar formulario ─────────────────────────────────────
  observeEvent(input$btn_config, {
    rv$configurado <- TRUE
    rv$resuelto    <- FALSE
  })
  
  # ── Nombres dinámicos ──────────────────────────────────────
  output$ui_nombres <- renderUI({
    req(rv$configurado)
    
    m <- input$num_origenes
    n <- input$num_destinos
    
    tagList(
      div(
        class = "card",
        div(class = "card-header", "Nombres de nodos"),
        
        div(
          class = "node-name-grid",
          
          tagList(
            lapply(seq_len(m), function(i) {
              div(
                tags$label(paste("Origen", i)),
                textInput(
                  inputId     = paste0("nom_o_", i),
                  label       = NULL,
                  value       = LETTERS[i],
                  placeholder = paste("Ej:", LETTERS[i])
                )
              )
            })
          ),
          
          tagList(
            lapply(seq_len(n), function(j) {
              div(
                tags$label(paste("Destino", j)),
                textInput(
                  inputId     = paste0("nom_d_", j),
                  label       = NULL,
                  value       = paste0("CD", j),
                  placeholder = paste0("Ej: CD", j)
                )
              )
            })
          )
        )
      )
    )
  })
  
  # ── Matriz de costos, oferta y demanda ─────────────────────
  output$ui_matrices <- renderUI({
    req(rv$configurado)
    
    m <- input$num_origenes
    n <- input$num_destinos
    
    nombres_o <- sapply(seq_len(m), function(i) {
      isolate(input[[paste0("nom_o_", i)]] %||% LETTERS[i])
    })
    
    nombres_d <- sapply(seq_len(n), function(j) {
      isolate(input[[paste0("nom_d_", j)]] %||% paste0("CD", j))
    })
    
    def_costos <- matrix(
      c(
        8,  6, 10, 9,
        9, 12, 13, 7,
        14, 9, 16, 5
      ),
      nrow = 3,
      byrow = TRUE
    )
    
    def_oferta  <- c(50, 60, 40)
    def_demanda <- c(30, 45, 35, 40)
    
    tagList(
      div(class = "section-title", "Matriz de costos unitarios"),
      
      div(
        class = "card",
        div(
          class = "matrix-scroll",
          div(
            class = "matrix-grid",
            style = paste0(
              "grid-template-columns: minmax(56px, 70px) repeat(",
              n,
              ", minmax(110px, 1fr));"
            ),
            
            div(),
            
            lapply(seq_len(n), function(j) {
              div(class = "matrix-header-cell", nombres_d[j])
            }),
            
            lapply(seq_len(m), function(i) {
              tagList(
                div(class = "matrix-row-label", nombres_o[i]),
                
                lapply(seq_len(n), function(j) {
                  def_val <- if (m == 3 && n == 4) {
                    def_costos[i, j]
                  } else {
                    0
                  }
                  
                  div(
                    class = "matrix-cell",
                    numericInput(
                      inputId = paste0("c_", i, "_", j),
                      label   = NULL,
                      value   = def_val,
                      min     = 0,
                      step    = 1
                    )
                  )
                })
              )
            })
          )
        )
      ),
      
      div(class = "section-title", "Oferta y demanda"),
      
      div(
        class = "card",
        div(
          class = "supply-demand-grid",
          
          div(
            div(class = "card-header", "Oferta por origen"),
            
            div(
              class = "input-stack",
              lapply(seq_len(m), function(i) {
                def_val <- if (i <= length(def_oferta)) {
                  def_oferta[i]
                } else {
                  0
                }
                
                div(
                  tags$label(nombres_o[i]),
                  numericInput(
                    inputId = paste0("oferta_", i),
                    label   = NULL,
                    value   = def_val,
                    min     = 0,
                    step    = 1
                  )
                )
              })
            )
          ),
          
          div(
            div(class = "card-header", "Demanda por destino"),
            
            div(
              class = "input-stack",
              lapply(seq_len(n), function(j) {
                def_val <- if (j <= length(def_demanda)) {
                  def_demanda[j]
                } else {
                  0
                }
                
                div(
                  tags$label(nombres_d[j]),
                  numericInput(
                    inputId = paste0("demanda_", j),
                    label   = NULL,
                    value   = def_val,
                    min     = 0,
                    step    = 1
                  )
                )
              })
            )
          )
        )
      )
    )
  })
  
  # ── Balanceo en tiempo real ────────────────────────────────
  output$ui_balanceo <- renderUI({
    req(rv$configurado)
    
    m <- input$num_origenes
    n <- input$num_destinos
    
    tot_o <- sum(sapply(seq_len(m), function(i) {
      input[[paste0("oferta_", i)]] %||% 0
    }))
    
    tot_d <- sum(sapply(seq_len(n), function(j) {
      input[[paste0("demanda_", j)]] %||% 0
    }))
    
    if (tot_o == tot_d && tot_o > 0) {
      div(
        class = "balance-badge",
        "✓ Balanceado —", tot_o, "unidades"
      )
    } else {
      div(
        class = "balance-badge warn",
        "⚠ Desbalanceo:", tot_o, "vs", tot_d,
        if (tot_o > tot_d) {
          "(se agregará destino ficticio)"
        } else {
          "(se agregará origen ficticio)"
        }
      )
    }
  })
  
  # ── Resolver modelo ────────────────────────────────────────
  observeEvent(input$btn_resolver, {
    req(rv$configurado)
    
    shinyjs::show("loading-overlay")
    
    on.exit({
      shinyjs::hide("loading-overlay")
    }, add = TRUE)
    
    m <- input$num_origenes
    n <- input$num_destinos
    
    nombres_o <- sapply(seq_len(m), function(i) {
      input[[paste0("nom_o_", i)]] %||% LETTERS[i]
    })
    
    nombres_d <- sapply(seq_len(n), function(j) {
      input[[paste0("nom_d_", j)]] %||% paste0("CD", j)
    })
    
    mat_c <- matrix(
      0,
      nrow = m,
      ncol = n,
      dimnames = list(nombres_o, nombres_d)
    )
    
    for (i in seq_len(m)) {
      for (j in seq_len(n)) {
        mat_c[i, j] <- input[[paste0("c_", i, "_", j)]] %||% 0
      }
    }
    
    oferta <- setNames(
      sapply(seq_len(m), function(i) {
        input[[paste0("oferta_", i)]] %||% 0
      }),
      nombres_o
    )
    
    demanda <- setNames(
      sapply(seq_len(n), function(j) {
        input[[paste0("demanda_", j)]] %||% 0
      }),
      nombres_d
    )
    
    if (any(is.na(mat_c)) || any(is.na(oferta)) || any(is.na(demanda))) {
      showNotification(
        "Existen valores vacíos o no válidos. Revisa costos, oferta y demanda.",
        type = "error"
      )
      return()
    }
    
    if (any(mat_c < 0) || any(oferta < 0) || any(demanda < 0)) {
      showNotification(
        "Los costos, ofertas y demandas no pueden ser negativos.",
        type = "error"
      )
      return()
    }
    
    if (sum(oferta) == 0 || sum(demanda) == 0) {
      showNotification(
        "La oferta y la demanda total deben ser mayores que cero.",
        type = "error"
      )
      return()
    }
    
    # Balanceo automático
    total_oferta  <- sum(oferta)
    total_demanda <- sum(demanda)
    
    if (total_oferta > total_demanda) {
      mat_c <- cbind(mat_c, DF = 0)
      demanda <- c(demanda, DF = total_oferta - total_demanda)
      n <- ncol(mat_c)
    } else if (total_demanda > total_oferta) {
      mat_c <- rbind(mat_c, OF = 0)
      oferta <- c(oferta, OF = total_demanda - total_oferta)
      m <- nrow(mat_c)
    }
    
    res <- tryCatch(
      lp.transport(
        cost.mat       = mat_c,
        direction      = "min",
        row.signs      = rep("<=", m),
        row.rhs        = oferta,
        col.signs      = rep("=", n),
        col.rhs        = demanda
      ),
      error = function(e) NULL
    )
    
    if (is.null(res) || res$status != 0) {
      showNotification(
        "No se encontró una solución factible. Revisa los datos ingresados.",
        type = "error"
      )
      return()
    }
    
    asig_opt <- matrix(
      res$solution,
      nrow = m,
      ncol = n,
      dimnames = list(rownames(mat_c), colnames(mat_c))
    )
    
    u_v <- calcular_multiplicadores(asig_opt, mat_c, m, n)
    
    delta <- matrix(
      NA_real_,
      nrow = m,
      ncol = n,
      dimnames = dimnames(asig_opt)
    )
    
    for (i in seq_len(m)) {
      for (j in seq_len(n)) {
        if (asig_opt[i, j] == 0) {
          delta[i, j] <- mat_c[i, j] - u_v$u[i] - u_v$v[j]
        }
      }
    }
    
    orig_a <- character()
    dest_a <- character()
    flu_a  <- numeric()
    cost_a <- numeric()
    
    for (i in rownames(asig_opt)) {
      for (j in colnames(asig_opt)) {
        if (asig_opt[i, j] > 0) {
          orig_a <- c(orig_a, i)
          dest_a <- c(dest_a, j)
          flu_a  <- c(flu_a, asig_opt[i, j])
          cost_a <- c(cost_a, mat_c[i, j])
        }
      }
    }
    
    rv$costos     <- mat_c
    rv$oferta     <- oferta
    rv$demanda    <- demanda
    rv$asig       <- asig_opt
    rv$Z_opt      <- res$objval
    rv$u_vec      <- u_v$u
    rv$v_vec      <- u_v$v
    rv$delta      <- delta
    rv$origenes_a <- orig_a
    rv$destinos_a <- dest_a
    rv$flujos_a   <- flu_a
    rv$costos_a   <- cost_a
    rv$resuelto   <- TRUE
  })
  
  # ── Resultados ─────────────────────────────────────────────
  output$ui_resultados <- renderUI({
    req(rv$resuelto)
    
    tagList(
      hr(style = "border-color:#263241; margin:28px 0;"),
      
      div(class = "section-title", "Resultados"),
      
      div(
        class = "kpi-row",
        
        div(
          class = "kpi-card",
          div(class = "kpi-label", "Costo óptimo Z*"),
          div(class = "kpi-value", rv$Z_opt),
          div(class = "kpi-sub", "unidades monetarias")
        ),
        
        div(
          class = "kpi-card",
          div(class = "kpi-label", "Rutas activas"),
          div(class = "kpi-value", length(rv$origenes_a)),
          div(
            class = "kpi-sub",
            paste("de", nrow(rv$costos) * ncol(rv$costos), "posibles")
          )
        ),
        
        div(
          class = "kpi-card",
          div(class = "kpi-label", "Unidades totales"),
          div(class = "kpi-value", sum(rv$flujos_a)),
          div(class = "kpi-sub", "transportadas")
        )
      ),
      
      tabsetPanel(
        tabPanel(
          "Asignación óptima",
          br(),
          div(
            class = "card",
            div(class = "card-header", "Matriz de asignaciones"),
            div(
              style = "overflow-x:auto;",
              tableOutput("tbl_asignacion")
            )
          )
        ),
        
        tabPanel(
          "Rutas activas",
          br(),
          div(
            class = "card",
            div(class = "card-header", "Detalle de rutas con asignación positiva"),
            DTOutput("tbl_rutas")
          )
        ),
        
        tabPanel(
          "Sensibilidad",
          br(),
          
          div(
            class = "card",
            div(class = "card-header", "Multiplicadores duales uᵢ y vⱼ"),
            tableOutput("tbl_multiplicadores")
          ),
          
          div(
            class = "card",
            div(class = "card-header", "Costos reducidos Δᵢⱼ"),
            div(
              style = "overflow-x:auto;",
              tableOutput("tbl_delta")
            )
          ),
          
          div(
            class = "card",
            div(class = "card-header", "Rangos de sensibilidad para celdas no básicas"),
            DTOutput("tbl_sensibilidad")
          )
        ),
        
        tabPanel(
          "Gráfico de flujos",
          br(),
          div(
            class = "plot-wrap",
            plotOutput("plot_flujos", height = "520px")
          )
        )
      )
    )
  })
  
  # ── Tabla de asignación ────────────────────────────────────
  output$tbl_asignacion <- renderTable({
    req(rv$resuelto)
    
    asig   <- rv$asig
    costos <- rv$costos
    
    m <- nrow(asig)
    n <- ncol(asig)
    
    df <- data.frame(
      matrix("", m, n + 2),
      stringsAsFactors = FALSE
    )
    
    colnames(df) <- c("Origen", colnames(asig), "Oferta")
    df[, 1] <- rownames(asig)
    
    for (i in seq_len(m)) {
      for (j in seq_len(n)) {
        if (asig[i, j] > 0) {
          df[i, j + 1] <- paste0("[", costos[i, j], "]  ", asig[i, j])
        } else {
          df[i, j + 1] <- paste0("[", costos[i, j], "]")
        }
      }
      
      df[i, n + 2] <- rv$oferta[i]
    }
    
    df_demanda <- as.data.frame(
      t(c("Demanda", as.character(rv$demanda), sum(rv$demanda))),
      stringsAsFactors = FALSE
    )
    
    colnames(df_demanda) <- colnames(df)
    
    rbind(df, df_demanda)
  },
  sanitize.text.function = identity,
  striped = FALSE,
  hover = FALSE,
  bordered = TRUE,
  spacing = "m",
  width = "100%")
  
  # ── Tabla de rutas activas ─────────────────────────────────
  output$tbl_rutas <- renderDT({
    req(rv$resuelto)
    
    df <- data.frame(
      Origen           = rv$origenes_a,
      Destino          = rv$destinos_a,
      `Costo unitario` = rv$costos_a,
      Cantidad         = rv$flujos_a,
      Subtotal         = rv$costos_a * rv$flujos_a,
      check.names      = FALSE
    )
    
    total <- data.frame(
      Origen           = "TOTAL",
      Destino          = "—",
      `Costo unitario` = NA,
      Cantidad         = sum(rv$flujos_a),
      Subtotal         = rv$Z_opt,
      check.names      = FALSE
    )
    
    rbind(df, total)
  },
  options = list(
    dom = "t",
    pageLength = 20,
    autoWidth = TRUE
  ),
  rownames = FALSE)
  
  # ── Multiplicadores ────────────────────────────────────────
  output$tbl_multiplicadores <- renderTable({
    req(rv$resuelto)
    
    u <- rv$u_vec
    v <- rv$v_vec
    
    data.frame(
      Variable = c(paste0("u_", names(u)), paste0("v_", names(v))),
      Valor    = round(c(u, v), 4),
      Tipo     = c(
        rep("Fila / origen", length(u)),
        rep("Columna / destino", length(v))
      ),
      check.names = FALSE
    )
  },
  striped = FALSE,
  hover = FALSE,
  bordered = TRUE,
  spacing = "s",
  width = "100%")
  
  # ── Costos reducidos ───────────────────────────────────────
  output$tbl_delta <- renderTable({
    req(rv$resuelto)
    
    delta <- rv$delta
    
    m <- nrow(delta)
    n <- ncol(delta)
    
    df <- data.frame(
      matrix("", m, n + 1),
      stringsAsFactors = FALSE
    )
    
    colnames(df) <- c("Origen", colnames(delta))
    df[, 1] <- rownames(delta)
    
    for (i in seq_len(m)) {
      for (j in seq_len(n)) {
        df[i, j + 1] <- if (is.na(delta[i, j])) {
          "(básica)"
        } else if (delta[i, j] < 0) {
          paste0(round(delta[i, j], 2), " ⚠")
        } else {
          paste0(round(delta[i, j], 2), " ✓")
        }
      }
    }
    
    df
  },
  sanitize.text.function = identity,
  striped = FALSE,
  bordered = TRUE,
  spacing = "s",
  width = "100%")
  
  # ── Sensibilidad ───────────────────────────────────────────
  output$tbl_sensibilidad <- renderDT({
    req(rv$resuelto)
    
    delta  <- rv$delta
    costos <- rv$costos
    u      <- rv$u_vec
    v      <- rv$v_vec
    
    filas <- list()
    
    for (i in rownames(delta)) {
      for (j in colnames(delta)) {
        if (!is.na(delta[i, j])) {
          umbral <- round(u[i] + v[j], 4)
          
          filas[[length(filas) + 1]] <- data.frame(
            Celda                  = paste0("(", i, ", ", j, ")"),
            c_ij                   = costos[i, j],
            `u + v`                = umbral,
            `Δ_ij`                 = round(delta[i, j], 4),
            `Puede bajar c hasta`  = umbral,
            Estado                 = if (delta[i, j] < 0) "Mejorable" else "OK",
            check.names            = FALSE
          )
        }
      }
    }
    
    if (length(filas) == 0) {
      return(data.frame())
    }
    
    do.call(rbind, filas)
  },
  options = list(
    dom = "t",
    pageLength = 30,
    autoWidth = TRUE
  ),
  rownames = FALSE)
  
  # ── Gráfico de flujos ──────────────────────────────────────
  output$plot_flujos <- renderPlot({
    req(rv$resuelto)
    
    orig_a <- rv$origenes_a
    dest_a <- rv$destinos_a
    flu_a  <- rv$flujos_a
    cost_a <- rv$costos_a
    costos <- rv$costos
    
    nodos <- c(rownames(costos), colnames(costos))
    
    g <- make_empty_graph(n = length(nodos), directed = TRUE)
    V(g)$name <- nodos
    
    for (k in seq_along(orig_a)) {
      g <- add_edges(g, c(orig_a[k], dest_a[k]))
    }
    
    E(g)$width <- (flu_a / max(flu_a)) * 9 + 1.5
    E(g)$label <- paste0(flu_a, "\nc=", cost_a)
    E(g)$color <- colorRampPalette(
      c("#4ade80", "#22d3ee", "#a78bfa")
    )(length(flu_a))
    
    es_origen <- V(g)$name %in% rownames(costos)
    
    V(g)$color       <- ifelse(es_origen, "#12301e", "#181f3a")
    V(g)$frame.color <- ifelse(es_origen, "#4ade80", "#818cf8")
    V(g)$frame.width <- 2.5
    V(g)$size        <- 30
    V(g)$label.color <- "#e5e7eb"
    V(g)$label.cex   <- 1.1
    V(g)$label.font  <- 2
    
    total_origenes <- sum(V(g)$name %in% rownames(costos))
    total_destinos <- sum(V(g)$name %in% colnames(costos))
    
    coords <- matrix(0, nrow = length(nodos), ncol = 2)
    
    for (i in seq_len(total_origenes)) {
      coords[i, ] <- c(0, -(i - 1) * 2.6)
    }
    
    for (j in seq_len(total_destinos)) {
      coords[total_origenes + j, ] <- c(
        5,
        -(j - 1) * 2.6 + (total_origenes - total_destinos) * 2.6 / 2
      )
    }
    
    par(
      bg  = "#111821",
      mar = c(2, 1, 3, 1),
      col.main = "#e5e7eb",
      col.sub  = "#9ca3af"
    )
    
    plot(
      g,
      layout           = coords,
      edge.arrow.size  = 0.55,
      edge.curved      = 0.2,
      edge.label.cex   = 0.82,
      edge.label.color = "#d1d5db",
      main             = paste0("Red de distribución óptima — Z* = ", rv$Z_opt),
      sub              = "Grosor proporcional al volumen transportado"
    )
    
    legend(
      "bottomleft",
      legend   = c("Origen", "Destino"),
      fill     = c("#12301e", "#181f3a"),
      border   = c("#4ade80", "#818cf8"),
      text.col = "#d1d5db",
      bty      = "n",
      cex      = 0.86
    )
  },
  bg = "#111821")
  
  # ── Status ─────────────────────────────────────────────────
  output$status_txt <- renderText({
    if (!rv$configurado) {
      "Esperando configuración"
    } else if (!rv$resuelto) {
      "Formulario listo — ingresa datos y resuelve"
    } else {
      paste0("Óptimo encontrado — Z* = ", rv$Z_opt)
    }
  })
}

# ============================================================
#  LANZAR APP
# ============================================================
shinyApp(ui, server)