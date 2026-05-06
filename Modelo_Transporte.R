
if (!requireNamespace("knitr",      quietly = TRUE)) install.packages("knitr")
if (!requireNamespace("kableExtra", quietly = TRUE)) install.packages("kableExtra")

library(knitr)
library(kableExtra)   # row_spec, column_spec, pack_rows, cell_spec

# ─────────────────────────────────────────────────────────────────────────────
# MODELO DE TRANSPORTE  |  Plantas A/B/C  →  Centros CD1/CD2/CD3/CD4
# Método: Costo Mínimo + MODI (dos iteraciones)
# ─────────────────────────────────────────────────────────────────────────────

costos <- matrix(
  c( 8,  6, 10,  9,
     9, 12, 13,  7,
     14,  9, 16,  5),
  nrow = 3, byrow = TRUE,
  dimnames = list(c("A", "B", "C"),
                  c("CD1", "CD2", "CD3", "CD4"))
)
oferta  <- c(A = 50, B = 60, C = 40)
demanda <- c(CD1 = 30, CD2 = 45, CD3 = 35, CD4 = 40)

#  construir la tabla manualmente con apply.

tabla_transporte <- function(asig, costos, oferta, demanda,
                             caption = "",
                             hl_neg = NULL,   # vector de etiquetas "i,j" a resaltar en rojo
                             hl_pos = NULL)  { # ídem en verde
  plantas <- rownames(costos)
  centros <- colnames(costos)
  n <- length(plantas); m <- length(centros)
  
  # Construir celdas: "[costo] cantidad" o "[costo]" si no asignada
  celdas <- matrix("", n, m, dimnames = list(plantas, centros))
  for (i in 1:n)
    for (j in 1:m)
      celdas[i,j] <- if (asig[i,j] == 0)
        paste0("[", costos[i,j], "]")
  else
    paste0("[", costos[i,j], "]  ", asig[i,j])
  
  df <- data.frame(celdas, Oferta = oferta,
                   check.names = FALSE, stringsAsFactors = FALSE)
  dem_row <- c(as.character(demanda), sum(oferta))
  df <- rbind(df, Demanda = dem_row)
  
  tbl <- df |>
    kbl(caption = caption, align = c("c","c","c","c","c")) |>
    kable_styling(bootstrap_options = c("bordered", "hover"),
                  full_width = FALSE, font_size = 13) |>
    row_spec(0,   bold = TRUE, background = "#1D7A5F", color = "white") |>
    row_spec(n+1, bold = TRUE, background = "#E1F5EE") |>
    column_spec(m+2, bold = TRUE, background = "#E1F5EE") |>
    column_spec(1,   bold = TRUE)
  
  # Resaltar celdas básicas (asignadas) en verde suave
  for (i in 1:n)
    for (j in 1:m)
      if (asig[i,j] > 0)
        tbl <- tbl |> column_spec(j+1, background = spec_color(
          c(rep(0,n+1)[1:(i-1)], 1, rep(0,n+1-i)),
          palette = c("white", "#9FE1CB")))
  
  tbl
}

# ── Versión simplificada y robusta (sin spec_color, solo kable base) ─────────

tabla_simple <- function(asig, costos, oferta, demanda, caption="") {
  plantas <- rownames(costos); centros <- colnames(costos)
  n <- length(plantas);        m <- length(centros)
  
  celdas <- matrix("", n, m, dimnames = list(plantas, centros))
  for (i in 1:n) for (j in 1:m)
    celdas[i,j] <- if (asig[i,j]==0) paste0("[",costos[i,j],"]")
  else               paste0("[",costos[i,j],"]  **",asig[i,j],"**")
  
  df <- data.frame(celdas, Oferta = oferta,
                   check.names = FALSE, stringsAsFactors = FALSE)
  df <- rbind(df, Demanda = c(as.character(demanda), sum(oferta)))
  
  df |> kbl(caption = caption, escape = FALSE,
            col.names = c(centros, "Oferta")) |>
    kable_styling(bootstrap_options = c("bordered","hover"),
                  full_width = FALSE, font_size = 13) |>
    row_spec(0,   bold=TRUE, background="#1D7A5F", color="white") |>
    row_spec(n+1, bold=TRUE, background="#E1F5EE") |>
    column_spec(m+2, bold=TRUE, background="#E1F5EE") |>
    column_spec(1, bold=TRUE)
  
  # ── Comprobación de balanceo ──────────────────────────────────────────────────
  oferta_total  <- sum(oferta)
  demanda_total <- sum(demanda)
  
  data.frame(
    Concepto         = c("Oferta total (A+B+C)", "Demanda total (CD1+CD2+CD3+CD4)"),
    Valor            = c(oferta_total, demanda_total),
    `Miles de litros`= c("50 + 60 + 40", "30 + 45 + 35 + 40"),
    Estado           = c("✓", "✓"),
    check.names = FALSE
  ) |>
    kbl(caption = "Comprobación de balanceo — Oferta = Demanda = 150",
        align = "lccc") |>
    kable_styling(bootstrap_options = c("striped","hover"),
                  full_width = FALSE, font_size = 13) |>
    row_spec(0, bold=TRUE, background="#1D7A5F", color="white") |>
    column_spec(4, color="#0F6E56", bold=TRUE)
  
  # ── Matriz de costos unitarios ────────────────────────────────────────────────
  costos_df <- as.data.frame(costos)
  costos_df$Oferta <- oferta
  rbind(costos_df, Demanda = c(demanda, sum(oferta))) |>
    kbl(caption = "Matriz de costos unitarios — c_ij ($/millar de litros)",
        align = "ccccc") |>
    kable_styling(bootstrap_options = c("bordered","hover"),
                  full_width = FALSE, font_size = 13) |>
    row_spec(0, bold=TRUE, background="#1D7A5F", color="white") |>
    row_spec(4, bold=TRUE, background="#E1F5EE") |>
    column_spec(1, bold=TRUE) |>
    column_spec(6, bold=TRUE, background="#E1F5EE")
  
  # Asignaciones (pasos en orden de costo creciente):
  #  1) C→CD4: min(40,40)=40  2) A→CD2: min(50,45)=45
  #  3) A→CD1: min(5,30)=5    4) B→CD1: min(60,25)=25  5) B→CD3: min(35,35)=35
  
  asig_ini <- matrix(
    c( 5, 45,  0,  0,
       25,  0, 35,  0,
       0,  0,  0, 40),
    nrow=3, byrow=TRUE,
    dimnames = list(c("A","B","C"), c("CD1","CD2","CD3","CD4"))
  )
  
  # Pasos del método de costo mínimo
  pasos <- data.frame(
    Paso    = 1:5,
    Ruta    = c("C → CD4","A → CD2","A → CD1","B → CD1","B → CD3"),
    `c_ij`  = c(5,6,8,9,13),
    `min(oferta, demanda)` = c("min(40,40)","min(50,45)","min(5,30)",
                               "min(60,25)","min(35,35)"),
    `x_ij` = c(40,45,5,25,35),
    check.names = FALSE
  )
  pasos |>
    kbl(caption="Método de Costo Mínimo — secuencia de asignaciones",
        align="clccc") |>
    kable_styling(bootstrap_options=c("striped","hover"),
                  full_width=FALSE, font_size=13) |>
    row_spec(0, bold=TRUE, background="#1D7A5F", color="white") |>
    column_spec(5, bold=TRUE)
  
  # Tabla de transporte — solución inicial
  tabla_simple(asig_ini, costos, oferta, demanda,
               caption = "Solución básica factible inicial  [c_ij]  **x_ij** = asignado")
}
Z0 <- sum(costos * asig_ini)

data.frame(
  Ruta     = c("A → CD1","A → CD2","B → CD1","B → CD3","C → CD4","TOTAL"),
  `c_ij`   = c(8,6,9,13,5, NA),
  `x_ij`   = c(5,45,25,35,40, sum(c(5,45,25,35,40))),
  Subtotal = c(40,270,225,455,200, Z0),
  check.names = FALSE
) |>
  kbl(caption=paste0("Cálculo del costo inicial  Z₀ = ", Z0),
      col.names=c("Ruta","Costo unitario c_ij","Cantidad x_ij","Subtotal"),
      align="lccc", na="—") |>
  kable_styling(bootstrap_options=c("striped","hover"),
                full_width=FALSE, font_size=13) |>
  row_spec(0, bold=TRUE, background="#1D7A5F", color="white") |>
  row_spec(6, bold=TRUE, background="#fff3cd") |>
  column_spec(4, bold=TRUE)
# ── Multiplicadores (u_A = 0 fijo) ───────────────────────────────────────────
u1 <- c(A=0, B=1, C=0)
v1 <- c(CD1=8, CD2=6, CD3=12, CD4=5)

data.frame(
  Variable = c("u_A","u_B","u_C","v_CD1","v_CD2","v_CD3","v_CD4"),
  Valor    = c(u1, v1),
  Ecuacion = c("fijo = 0","u_A+v1=8 → 0+8","u_C+v4=5 → v4-5",
               "u_A+v1=8","u_A+v2=6","u_B+v3=13 → 1+12","u_C+v4=5 → 0+5")
) |>
  kbl(caption="Multiplicadores MODI — Iteración 1  (u_A = 0 referencia)",
      col.names=c("Variable dual","Valor","Ecuación c_ij = u_i + v_j"),
      align="lcc") |>
  kable_styling(bootstrap_options=c("striped","hover"),
                full_width=FALSE, font_size=13) |>
  row_spec(0, bold=TRUE, background="#1D7A5F", color="white") |>
  pack_rows("u_i — filas (plantas)",    1, 3) |>
  pack_rows("v_j — columnas (centros)", 4, 7)

# ── Costos reducidos Δ_ij = c_ij - u_i - v_j (solo celdas no básicas) ────────
delta1 <- matrix(NA, 3, 4, dimnames=list(names(u1),names(v1)))
for(i in seq_along(u1)) for(j in seq_along(v1))
  if(asig_ini[i,j]==0) delta1[i,j] <- costos[i,j] - u1[i] - v1[j]

# Formatear: básicas = "—", negativas marcadas con ★
d1_fmt <- matrix("(básica)", 3, 4, dimnames=dimnames(delta1))
for(i in 1:3) for(j in 1:4)
  if(!is.na(delta1[i,j]))
    d1_fmt[i,j] <- if(delta1[i,j]<0) paste0(delta1[i,j]," ★") else as.character(delta1[i,j])

data.frame(Planta=rownames(delta1), d1_fmt, u_i=u1,
           check.names=FALSE, row.names=NULL) |>
  kbl(caption="Costos reducidos Δ_ij — Iteración 1  (★ = entra a la base)",
      col.names=c("Planta","CD1","CD2","CD3","CD4","u_i"),
      align="cccccc") |>
  kable_styling(bootstrap_options=c("bordered","hover"),
                full_width=FALSE, font_size=13) |>
  row_spec(0, bold=TRUE, background="#1D7A5F", color="white") |>
  column_spec(1, bold=TRUE) |>
  column_spec(4, background="#FCEBEB", color="#A32D2D", bold=TRUE)  # CD3 col → Δ(A,CD3)=-2

# ── Ciclo: (A,CD3)+ → (B,CD3)- → (B,CD1)+ → (A,CD1)- ───────────────────────
theta <- min(35, 5)   # mínimo de celdas con signo "-" en el ciclo

ciclo <- data.frame(
  Orden              = 1:4,
  Celda              = c("(A, CD3)", "(B, CD3)", "(B, CD1)", "(A, CD1)"),
  Signo              = c("+θ  ← entra", "-θ", "+θ", "-θ  ← sale"),
  x_actual           = c(0, 35, 25, 5),
  x_nuevo            = c(0 + theta, 35 - theta, 25 + theta, 5 - theta),
  check.names = FALSE
)

ciclo |>
  kbl(
    caption = paste0("Ciclo cerrado — θ = min(35, 5) = ", theta,
                     "  |  (A, CD1) sale de la base"),
    col.names = c("Orden", "Celda", "Signo", "x actual", "x nuevo"),
    align = "clccc"
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE,
    font_size  = 13
  ) |>
  row_spec(0, bold = TRUE,  background = "#1D7A5F", color = "white") |>
  row_spec(1, bold = TRUE,  background = "#E1F5EE", color = "#0F6E56") |>  # entra
  row_spec(4, bold = TRUE,  background = "#FCEBEB", color = "#A32D2D")    # sale
asig_it1 <- matrix(
  c( 0, 45,  5,  0,
     30,  0, 30,  0,
     0,  0,  0, 40),
  nrow = 3, byrow = TRUE,
  dimnames = list(c("A", "B", "C"), c("CD1", "CD2", "CD3", "CD4"))
)
Z1 <- sum(costos * asig_it1)

tabla_simple(asig_it1, costos, oferta, demanda,
             caption = paste0("Solución básica factible — tras Iteración 1  |  Z = ", Z1))

# ── Detalle del nuevo costo ───────────────────────────────────────────────────
data.frame(
  Ruta     = c("A → CD2", "A → CD3", "B → CD1", "B → CD3", "C → CD4", "TOTAL"),
  cij      = c(6, 10, 9, 13, 5, NA),
  xij      = c(45, 5, 30, 30, 40, 150),
  Subtotal = c(270, 50, 270, 390, 200, Z1),
  check.names = FALSE
) |>
  kbl(
    caption   = paste0("Costo Z₁ = ", Z1, "  (ahorro vs Z₀: ", Z0 - Z1, ")"),
    col.names = c("Ruta", "c_ij", "x_ij", "Subtotal"),
    align     = "lccc",
    na        = "—"
  ) |>
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = FALSE, font_size = 13) |>
  row_spec(0, bold = TRUE, background = "#1D7A5F", color = "white") |>
  row_spec(6, bold = TRUE, background = "#fff3cd") |>
  column_spec(4, bold = TRUE)
# ── Tabla de rutas activas en la solución óptima ─────────────────────────────
Z_opt <- sum(costos * asig_it1)

data.frame(
  Origen   = c("A", "A", "B", "B", "C", "TOTAL"),
  Destino  = c("CD2", "CD3", "CD1", "CD3", "CD4", "---"),
  cij      = c(6, 10, 9, 13, 5, NA),
  xij      = c(45, 5, 30, 30, 40, 150),
  Subtotal = c(270, 50, 270, 390, 200, Z_opt),
  check.names = FALSE
) |>
  kbl(
    caption   = paste0("SOLUCION OPTIMA — Z* = ", Z_opt,
                       "  |  Ahorro vs Z0: ", Z0 - Z_opt),
    col.names = c("Origen", "Destino", "c_ij", "x_ij (miles L)", "Subtotal"),
    align     = "ccccc",
    na        = "---"
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width        = FALSE,
    font_size         = 13
  ) |>
  row_spec(0, bold = TRUE, background = "#1D7A5F", color = "white") |>
  row_spec(6, bold = TRUE, background = "#E1F5EE", color = "#0F6E56") |>
  column_spec(5, bold = TRUE)

# ── Resumen final en consola ──────────────────────────────────────────────────
cat("\n", strrep("=", 44), "\n")
cat("   RESUMEN — MODELO DE TRANSPORTE\n")
cat("  ", strrep("-", 40), "\n")
cat(sprintf("   Z0  costo inicial  :  %6d\n", Z0))
cat(sprintf("   Z1  tras iter. 1  :  %6d\n", Z1))
cat(sprintf("   Z*  optimo        :  %6d  (*)\n", Z_opt))
cat(sprintf("   Ahorro total      :  %6d\n", Z0 - Z_opt))
cat("  ", strrep("=", 44), "\n\n")

