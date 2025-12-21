# app.R
# Shiny glyph editor for cross-stitch alphabets -> exports to JSON
#
# Features:
# - Create/load an alphabet (JSON)
# - Edit glyphs in a clickable grid (toggles 0/1)
# - Add/delete glyphs
# - Save/download alphabet as JSON
#
# Dependencies: shiny, DT, jsonlite
#
# Run: shiny::runApp()

library(shiny)
library(DT)
library(jsonlite)

# ---------------------------
# Helpers
# ---------------------------

new_alphabet <- function(
  name = "Untitled alphabet",
  height = 7L,
  baseline = height - 1L,
  defaultSpacing = 1L
) {
  list(
    format = "xstitch-alphabet",
    version = 1L,
    name = name,
    height = as.integer(height),
    baseline = as.integer(baseline),
    defaultSpacing = as.integer(defaultSpacing),
    glyphs = list()
  )
}

blank_glyph_matrix <- function(height, width) {
  matrix(0L, nrow = height, ncol = width)
}

glyph_matrix_to_strings <- function(mat) {
  apply(mat, 1, paste0, collapse = "")
}

glyph_strings_to_matrix <- function(strings, height) {
  # strings: character vector length == height; each string contains 0/1
  if (length(strings) != height) stop("Glyph strings height mismatch.")
  widths <- nchar(strings)
  if (length(unique(widths)) != 1) stop("Glyph rows must have equal width.")
  w <- widths[[1]]
  rows <- lapply(strings, function(s) as.integer(strsplit(s, "")[[1]]))
  mat <- do.call(rbind, rows)
  storage.mode(mat) <- "integer"
  if (ncol(mat) != w) stop("Failed parsing glyph width.")
  mat
}

normalize_glyph_strings <- function(strings, height) {
  # Ensure exactly 'height' rows; pad/crop. Ensure only 0/1 and equal width.
  strings <- as.character(strings)
  strings <- gsub("[^01]", "0", strings) # defensive
  if (length(strings) < height) strings <- c(strings, rep("0", height - length(strings)))
  if (length(strings) > height) strings <- strings[seq_len(height)]

  widths <- nchar(strings)
  w <- max(widths, 1)
  strings <- vapply(strings, function(s) {
    if (nchar(s) < w) paste0(s, paste(rep("0", w - nchar(s)), collapse = ""))
    else substr(s, 1, w)
  }, character(1))
  strings
}

sanitize_glyph_key <- function(x) {
  x <- as.character(x %||% "")
  x <- trimws(x)
  if (nchar(x) == 0) return("")
  # allow multi-byte chars; keep first "character" by substring is imperfect for some unicode,
  # but is OK for typical use (A-Z, æøå, etc.)
  substr(x, 1, 1)
}

as_dt <- function(mat) {
  # Show 0/1 but with nicer column names
  df <- as.data.frame(mat, stringsAsFactors = FALSE)
  colnames(df) <- paste0("C", seq_len(ncol(mat)))
  df
}

ensure_space_glyph <- function(alph) {
  # Optional: ensure space exists
  if (is.null(alph$glyphs[[" "]])) {
    alph$glyphs[[" "]] <- rep("0", alph$height)
  }
  alph
}

# Shiny null-coalescing helper
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------------------------
# UI
# ---------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .small-note { opacity: 0.8; font-size: 0.9em; }
      .mono { font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', 'Courier New', monospace; }
    "))
  ),
  titlePanel("Glyph-editor til korsstings-alfabeter (JSON)"),

  sidebarLayout(
    sidebarPanel(
      h4("Alphabet"),
      textInput("alphabet_name", "Navn", value = "Untitled alphabet"),
      numericInput("alphabet_height", "Højde (rækker)", value = 7, min = 3, max = 40, step = 1),
      numericInput("alphabet_baseline", "Baseline (0-indekseret)", value = 6, min = 0, max = 39, step = 1),
      numericInput("alphabet_spacing", "Default spacing (kolonner)", value = 1, min = 0, max = 10, step = 1),

      hr(),

      fileInput("load_json", "Indlæs alphabet JSON", accept = c(".json", "application/json")),
      actionButton("new_alphabet", "Nyt alphabet"),
      downloadButton("download_json", "Download JSON"),

      hr(),

      h4("Glyph"),
      selectInput("glyph_select", "Vælg glyph", choices = c("(ingen)" = "")),
      textInput("glyph_new_key", "Nyt glyph-tegn (1 tegn)", value = ""),
      numericInput("glyph_width", "Glyph-bredde (kolonner)", value = 5, min = 1, max = 80, step = 1),

      fluidRow(
        column(6, actionButton("add_glyph", "Tilføj/overskriv")),
        column(6, actionButton("delete_glyph", "Slet", class = "btn-danger"))
      ),

      hr(),

      h4("Redigering"),
      actionButton("clear_glyph", "Ryd (0)"),
      actionButton("invert_glyph", "Invertér"),
      fluidRow(
        column(4, actionButton("shift_left", "←")),
        column(4, actionButton("shift_up", "↑")),
        column(4, actionButton("shift_right", "→"))
      ),
      actionButton("shift_down", "↓"),

      hr(),

      checkboxInput("show_zeros", "Vis 0/1 i stedet for blank/X", value = FALSE),
      tags$p(class = "small-note",
             "Tip: klik i grid'et for at toggle en celle. Eksportér via Download JSON.")
    ),

    mainPanel(
      h4(textOutput("glyph_title")),
      tags$p(class = "small-note mono", textOutput("glyph_dims")),

      DTOutput("glyph_table"),

      hr(),
      h4("Preview"),
      plotOutput("glyph_preview", height = "280px"),

      hr(),
      h4("Glyph-oversigt"),
      DTOutput("glyph_list")
    )
  )
)

# ---------------------------
# Server
# ---------------------------

server <- function(input, output, session) {

  alphabet <- reactiveVal(new_alphabet())
  current_key <- reactiveVal("")

  # Keep derived state: current glyph matrix (editable)
  glyph_mat <- reactiveVal(blank_glyph_matrix(height = 7, width = 5))

  # Update UI fields from alphabet
  refresh_ui_from_alphabet <- function(alph) {
    updateTextInput(session, "alphabet_name", value = alph$name %||% "Untitled alphabet")
    updateNumericInput(session, "alphabet_height", value = alph$height %||% 7L)
    updateNumericInput(session, "alphabet_baseline", value = alph$baseline %||% (alph$height - 1L))
    updateNumericInput(session, "alphabet_spacing", value = alph$defaultSpacing %||% 1L)

    keys <- sort(names(alph$glyphs))
    choices <- c("(ingen)" = "", stats::setNames(keys, keys))
    updateSelectInput(session, "glyph_select", choices = choices, selected = current_key() %||% "")
  }

  # Ensure baseline constraints
  observeEvent(input$alphabet_height, {
    h <- as.integer(input$alphabet_height %||% 7)
    updateNumericInput(session, "alphabet_baseline", min = 0, max = max(h - 1L, 0L))
  }, ignoreInit = TRUE)

  # Apply alphabet metadata edits
  observeEvent(
    {
      input$alphabet_name
      input$alphabet_height
      input$alphabet_baseline
      input$alphabet_spacing
    },
    {
      alph <- alphabet()
      alph$name <- input$alphabet_name %||% alph$name
      new_h <- as.integer(input$alphabet_height %||% alph$height)
      new_h <- max(new_h, 1L)
      alph$height <- new_h

      alph$baseline <- as.integer(input$alphabet_baseline %||% (new_h - 1L))
      alph$baseline <- max(0L, min(alph$baseline, new_h - 1L))

      alph$defaultSpacing <- as.integer(input$alphabet_spacing %||% alph$defaultSpacing)
      alph$defaultSpacing <- max(0L, alph$defaultSpacing)

      # If height changed, normalize all glyphs
      if (!is.null(alph$glyphs) && length(alph$glyphs) > 0) {
        for (k in names(alph$glyphs)) {
          alph$glyphs[[k]] <- normalize_glyph_strings(alph$glyphs[[k]], new_h)
        }
      }

      alphabet(alph)

      # Also adjust current glyph matrix height
      mat <- glyph_mat()
      if (nrow(mat) != new_h) {
        new_mat <- blank_glyph_matrix(new_h, ncol(mat))
        rr <- min(nrow(mat), new_h)
        new_mat[seq_len(rr), ] <- mat[seq_len(rr), , drop = FALSE]
        glyph_mat(new_mat)
      }

      refresh_ui_from_alphabet(alph)
    },
    ignoreInit = TRUE
  )

  # New alphabet
  observeEvent(input$new_alphabet, {
    h <- as.integer(input$alphabet_height %||% 7)
    alph <- new_alphabet(
      name = input$alphabet_name %||% "Untitled alphabet",
      height = h,
      baseline = as.integer(input$alphabet_baseline %||% (h - 1L)),
      defaultSpacing = as.integer(input$alphabet_spacing %||% 1L)
    )
    alph <- ensure_space_glyph(alph)
    alphabet(alph)
    current_key("")
    glyph_mat(blank_glyph_matrix(h, max(1L, as.integer(input$glyph_width %||% 5))))
    refresh_ui_from_alphabet(alph)
  }, ignoreInit = TRUE)

  # Load JSON
  observeEvent(input$load_json, {
    req(input$load_json$datapath)
    txt <- readLines(input$load_json$datapath, warn = FALSE, encoding = "UTF-8")
    alph <- fromJSON(paste(txt, collapse = "\n"), simplifyVector = FALSE)

    # Minimal validation / normalization
    if (is.null(alph$height)) alph$height <- 7L
    alph$height <- as.integer(alph$height)
    if (is.null(alph$baseline)) alph$baseline <- alph$height - 1L
    alph$baseline <- as.integer(alph$baseline)
    if (is.null(alph$defaultSpacing)) alph$defaultSpacing <- 1L
    alph$defaultSpacing <- as.integer(alph$defaultSpacing)
    if (is.null(alph$name)) alph$name <- "Loaded alphabet"
    if (is.null(alph$glyphs) || !is.list(alph$glyphs)) alph$glyphs <- list()

    for (k in names(alph$glyphs)) {
      alph$glyphs[[k]] <- normalize_glyph_strings(alph$glyphs[[k]], alph$height)
    }

    alph <- ensure_space_glyph(alph)

    alphabet(alph)
    refresh_ui_from_alphabet(alph)

    # Select first glyph if available
    keys <- names(alph$glyphs)
    if (length(keys) > 0) {
      current_key(keys[[1]])
      updateSelectInput(session, "glyph_select", selected = keys[[1]])
    } else {
      current_key("")
      updateSelectInput(session, "glyph_select", selected = "")
    }
  }, ignoreInit = TRUE)

  # When glyph selected: load its matrix
  observeEvent(input$glyph_select, {
    alph <- alphabet()
    key <- input$glyph_select %||% ""
    current_key(key)

    if (nchar(key) == 0 || is.null(alph$glyphs[[key]])) {
      # blank
      w <- max(1L, as.integer(input$glyph_width %||% 5))
      glyph_mat(blank_glyph_matrix(alph$height, w))
      return()
    }

    strings <- normalize_glyph_strings(alph$glyphs[[key]], alph$height)
    mat <- glyph_strings_to_matrix(strings, alph$height)
    glyph_mat(mat)
    updateNumericInput(session, "glyph_width", value = ncol(mat))
  }, ignoreInit = TRUE)

  # Change glyph width: resize current glyph matrix
  observeEvent(input$glyph_width, {
    alph <- alphabet()
    h <- alph$height
    w <- max(1L, as.integer(input$glyph_width %||% 5))
    mat <- glyph_mat()
    if (ncol(mat) == w && nrow(mat) == h) return()

    new_mat <- blank_glyph_matrix(h, w)
    rr <- min(nrow(mat), h)
    cc <- min(ncol(mat), w)
    new_mat[seq_len(rr), seq_len(cc)] <- mat[seq_len(rr), seq_len(cc), drop = FALSE]
    glyph_mat(new_mat)
  }, ignoreInit = TRUE)

  # Add/overwrite glyph
  observeEvent(input$add_glyph, {
    alph <- alphabet()
    key <- sanitize_glyph_key(input$glyph_new_key)
    if (nchar(key) == 0) {
      showNotification("Indtast et tegn i 'Nyt glyph-tegn'.", type = "error")
      return()
    }

    mat <- glyph_mat()
    strings <- glyph_matrix_to_strings(mat)
    alph$glyphs[[key]] <- strings
    alphabet(alph)

    current_key(key)
    refresh_ui_from_alphabet(alph)
    updateSelectInput(session, "glyph_select", selected = key)
    showNotification(sprintf("Gemte glyph '%s'.", key), type = "message")
  }, ignoreInit = TRUE)

  # Delete glyph
  observeEvent(input$delete_glyph, {
    alph <- alphabet()
    key <- current_key() %||% ""
    if (nchar(key) == 0 || is.null(alph$glyphs[[key]])) {
      showNotification("Ingen glyph valgt.", type = "error")
      return()
    }
    alph$glyphs[[key]] <- NULL
    alphabet(alph)
    current_key("")
    glyph_mat(blank_glyph_matrix(alph$height, max(1L, as.integer(input$glyph_width %||% 5))))
    refresh_ui_from_alphabet(alph)
    updateSelectInput(session, "glyph_select", selected = "")
    showNotification(sprintf("Slettede glyph '%s'.", key), type = "message")
  }, ignoreInit = TRUE)

  # Clear/invert/shift
  observeEvent(input$clear_glyph, {
    mat <- glyph_mat()
    glyph_mat(mat * 0L)
  }, ignoreInit = TRUE)

  observeEvent(input$invert_glyph, {
    mat <- glyph_mat()
    glyph_mat(1L - mat)
  }, ignoreInit = TRUE)

  observeEvent(input$shift_left, {
    mat <- glyph_mat()
    if (ncol(mat) > 1) mat <- cbind(mat[, -1, drop = FALSE], 0L)
    glyph_mat(mat)
  }, ignoreInit = TRUE)

  observeEvent(input$shift_right, {
    mat <- glyph_mat()
    if (ncol(mat) > 1) mat <- cbind(0L, mat[, -ncol(mat), drop = FALSE])
    glyph_mat(mat)
  }, ignoreInit = TRUE)

  observeEvent(input$shift_up, {
    mat <- glyph_mat()
    if (nrow(mat) > 1) mat <- rbind(mat[-1, , drop = FALSE], rep(0L, ncol(mat)))
    glyph_mat(mat)
  }, ignoreInit = TRUE)

  observeEvent(input$shift_down, {
    mat <- glyph_mat()
    if (nrow(mat) > 1) mat <- rbind(rep(0L, ncol(mat)), mat[-nrow(mat), , drop = FALSE])
    glyph_mat(mat)
  }, ignoreInit = TRUE)

  # Click-to-toggle JS for DT
  toggle_js <- JS("
    table.on('click.dt', 'tbody td', function() {
      var cell = table.cell(this);
      var idx = cell.index();
      if(!idx) return;
      Shiny.setInputValue('cell_click', {row: idx.row + 1, col: idx.column + 1}, {priority: 'event'});
    });
  ")

  output$glyph_table <- renderDT({
    mat <- glyph_mat()
    df <- as_dt(mat)

    # Display: either 0/1 or blank/X
    if (!isTRUE(input$show_zeros)) {
      df[] <- lapply(df, function(col) ifelse(col == 1L, "X", ""))
    }

    datatable(
      df,
      rownames = FALSE,
      selection = "none",
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        autoWidth = TRUE,
        dom = "t",
        # Make it look grid-like
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      callback = toggle_js
    )
  })

  observeEvent(input$cell_click, {
    click <- input$cell_click
    req(click$row, click$col)
    mat <- glyph_mat()
    r <- as.integer(click$row)
    c <- as.integer(click$col)
    if (r >= 1 && r <= nrow(mat) && c >= 1 && c <= ncol(mat)) {
      mat[r, c] <- if (mat[r, c] == 1L) 0L else 1L
      glyph_mat(mat)
    }
  }, ignoreInit = TRUE)

  output$glyph_title <- renderText({
    key <- current_key() %||% ""
    if (nchar(key) == 0) "Ingen glyph valgt (rediger blank og 'Tilføj/overskriv')"
    else sprintf("Glyph: '%s'", key)
  })

  output$glyph_dims <- renderText({
    mat <- glyph_mat()
    sprintf("Størrelse: %d × %d (højde × bredde)", nrow(mat), ncol(mat))
  })

  output$glyph_preview <- renderPlot({
    mat <- glyph_mat()
    # Simple preview: plot filled cells
    op <- par(mar = c(1, 1, 1, 1))
    on.exit(par(op), add = TRUE)

    h <- nrow(mat)
    w <- ncol(mat)

    plot(
      NA, xlim = c(0, w), ylim = c(0, h),
      asp = 1, axes = FALSE, xlab = "", ylab = ""
    )

    # Draw grid
    for (x in 0:w) lines(c(x, x), c(0, h))
    for (y in 0:h) lines(c(0, w), c(y, y))

    # Filled squares
    filled <- which(mat == 1L, arr.ind = TRUE)
    if (nrow(filled) > 0) {
      for (i in seq_len(nrow(filled))) {
        r <- filled[i, 1] # 1..h top? We'll draw with y inverted for nicer view
        c <- filled[i, 2]
        y0 <- h - r
        rect(c - 1, y0, c, y0 + 1, border = NA, col = "black")
      }
    }
  })

  output$glyph_list <- renderDT({
    alph <- alphabet()
    keys <- sort(names(alph$glyphs))
    if (length(keys) == 0) {
      return(datatable(data.frame(Glyph = character(), Width = integer()),
                       rownames = FALSE,
                       options = list(paging = FALSE, searching = FALSE, info = FALSE, dom = "t")))
    }

    widths <- vapply(keys, function(k) {
      strings <- alph$glyphs[[k]]
      if (length(strings) == 0) 0L else max(nchar(strings), 0L)
    }, integer(1))

    df <- data.frame(
      Glyph = keys,
      Width = widths,
      stringsAsFactors = FALSE
    )

    datatable(
      df,
      rownames = FALSE,
      selection = "single",
      options = list(paging = FALSE, searching = TRUE, info = FALSE)
    )
  })

  observeEvent(input$glyph_list_rows_selected, {
    sel <- input$glyph_list_rows_selected
    if (is.null(sel) || length(sel) == 0) return()
    alph <- alphabet()
    keys <- sort(names(alph$glyphs))
    if (sel >= 1 && sel <= length(keys)) {
      key <- keys[[sel]]
      updateSelectInput(session, "glyph_select", selected = key)
    }
  }, ignoreInit = TRUE)

  # Download JSON
  output$download_json <- downloadHandler(
    filename = function() {
      alph <- alphabet()
      nm <- gsub("[^A-Za-z0-9._-]+", "_", alph$name %||% "alphabet")
      paste0(nm, ".json")
    },
    content = function(file) {
      alph <- alphabet()
      alph$format <- alph$format %||% "xstitch-alphabet"
      alph$version <- alph$version %||% 1L
      alph <- ensure_space_glyph(alph)

      json <- toJSON(alph, auto_unbox = TRUE, pretty = TRUE)
      writeLines(json, con = file, useBytes = TRUE)
    }
  )

  # Initialize UI from default alphabet
  observe({
    alph <- alphabet()
    refresh_ui_from_alphabet(alph)
    if (nchar(current_key() %||% "") == 0) {
      glyph_mat(blank_glyph_matrix(alph$height, max(1L, as.integer(input$glyph_width %||% 5))))
    }
  })
}

shinyApp(ui, server)
