diff --git a/app.R b/app.R
index 1111111..2222222 100644
--- a/app.R
+++ b/app.R
@@ -1,7 +1,7 @@
 # app.R
 # Shiny glyph editor for cross-stitch alphabets -> exports to JSON
 #
-# Features:
+# Features:
 # - Create/load an alphabet (JSON)
 # - Edit glyphs in a clickable grid (toggles 0/1)
 # - Add/delete glyphs
@@ -35,14 +35,24 @@ glyph_matrix_to_strings <- function(mat) {
   apply(mat, 1, paste0, collapse = "")
 }
 
 glyph_strings_to_matrix <- function(strings, height) {
   # strings: character vector length == height; each string contains 0/1
   if (length(strings) != height) stop("Glyph strings height mismatch.")
   widths <- nchar(strings)
   if (length(unique(widths)) != 1) stop("Glyph rows must have equal width.")
   w <- widths[[1]]
-  rows <- lapply(strings, function(s) as.integer(strsplit(s, "")[[1]]))
+  rows <- lapply(strings, function(s) {
+    ch <- strsplit(s, "", fixed = TRUE)[[1]]
+    if (!all(ch %in% c("0", "1"))) {
+      stop("Glyph rows must contain only '0' and '1'.")
+    }
+    as.integer(ch)
+  })
   mat <- do.call(rbind, rows)
   storage.mode(mat) <- "integer"
   if (ncol(mat) != w) stop("Failed parsing glyph width.")
   mat
 }
@@ -186,10 +196,16 @@ server <- function(input, output, session) {
 
   alphabet <- reactiveVal(new_alphabet())
   current_key <- reactiveVal("")
+  glyph_dirty <- reactiveVal(FALSE)
+  pending_key <- reactiveVal(NULL)
+  suppress_glyph_select <- reactiveVal(FALSE)
 
   # Keep derived state: current glyph matrix (editable)
   glyph_mat <- reactiveVal(blank_glyph_matrix(height = 7, width = 5))
 
+  set_dirty <- function(x) glyph_dirty(isTRUE(x))
+
   # Update UI fields from alphabet
   refresh_ui_from_alphabet <- function(alph) {
     updateTextInput(session, "alphabet_name", value = alph$name %||% "Untitled alphabet")
@@ -230,7 +246,6 @@ server <- function(input, output, session) {
       }
 
       alphabet(alph)
 
       # Also adjust current glyph matrix height
       mat <- glyph_mat()
       if (nrow(mat) != new_h) {
@@ -241,8 +256,6 @@ server <- function(input, output, session) {
         new_mat[seq_len(rr), ] <- mat[seq_len(rr), , drop = FALSE]
         glyph_mat(new_mat)
       }
-
-      refresh_ui_from_alphabet(alph)
     },
     ignoreInit = TRUE
   )
@@ -261,6 +274,7 @@ server <- function(input, output, session) {
     alph <- ensure_space_glyph(alph)
     alphabet(alph)
     current_key("")
+    set_dirty(FALSE)
     glyph_mat(blank_glyph_matrix(h, max(1L, as.integer(input$glyph_width %||% 5))))
     refresh_ui_from_alphabet(alph)
   }, ignoreInit = TRUE)
@@ -294,6 +308,7 @@ server <- function(input, output, session) {
 
     alphabet(alph)
     refresh_ui_from_alphabet(alph)
+    set_dirty(FALSE)
 
     # Select first glyph if available
     keys <- names(alph$glyphs)
@@ -306,28 +321,87 @@ server <- function(input, output, session) {
     }
   }, ignoreInit = TRUE)
 
+  load_glyph_into_editor <- function(key) {
+    alph <- alphabet()
+    current_key(key %||% "")
+
+    if (nchar(key %||% "") == 0 || is.null(alph$glyphs[[key]])) {
+      w <- max(1L, as.integer(input$glyph_width %||% 5))
+      glyph_mat(blank_glyph_matrix(alph$height, w))
+      set_dirty(FALSE)
+      return()
+    }
+
+    strings <- normalize_glyph_strings(alph$glyphs[[key]], alph$height)
+    mat <- glyph_strings_to_matrix(strings, alph$height)
+    glyph_mat(mat)
+    updateNumericInput(session, "glyph_width", value = ncol(mat))
+    set_dirty(FALSE)
+  }
+
   # When glyph selected: load its matrix
   observeEvent(input$glyph_select, {
-    alph <- alphabet()
     key <- input$glyph_select %||% ""
-    current_key(key)
 
-    if (nchar(key) == 0 || is.null(alph$glyphs[[key]])) {
-      # blank
-      w <- max(1L, as.integer(input$glyph_width %||% 5))
-      glyph_mat(blank_glyph_matrix(alph$height, w))
+    if (isTRUE(suppress_glyph_select())) {
       return()
     }
 
-    strings <- normalize_glyph_strings(alph$glyphs[[key]], alph$height)
-    mat <- glyph_strings_to_matrix(strings, alph$height)
-    glyph_mat(mat)
-    updateNumericInput(session, "glyph_width", value = ncol(mat))
+    cur <- current_key() %||% ""
+    if (isTRUE(glyph_dirty()) && key != cur) {
+      pending_key(key)
+      showModal(modalDialog(
+        title = "Ugemte ændringer",
+        "Du har ændringer i den nuværende glyph, som ikke er gemt. Skift glyph alligevel?",
+        footer = tagList(
+          modalButton("Nej (bliv her)"),
+          actionButton("confirm_switch_glyph", "Ja, skift", class = "btn-danger")
+        ),
+        easyClose = TRUE
+      ))
+      # revert select visually (without triggering a second modal)
+      suppress_glyph_select(TRUE)
+      updateSelectInput(session, "glyph_select", selected = cur)
+      suppress_glyph_select(FALSE)
+      return()
+    }
+
+    load_glyph_into_editor(key)
   }, ignoreInit = TRUE)
 
+  observeEvent(input$confirm_switch_glyph, {
+    removeModal()
+    key <- pending_key() %||% ""
+    pending_key(NULL)
+    suppress_glyph_select(TRUE)
+    updateSelectInput(session, "glyph_select", selected = key)
+    suppress_glyph_select(FALSE)
+    load_glyph_into_editor(key)
+  }, ignoreInit = TRUE)
+
   # Change glyph width: resize current glyph matrix
   observeEvent(input$glyph_width, {
     alph <- alphabet()
     h <- alph$height
     w <- max(1L, as.integer(input$glyph_width %||% 5))
@@ -341,6 +415,7 @@ server <- function(input, output, session) {
     rr <- min(nrow(mat), h)
     cc <- min(ncol(mat), w)
     new_mat[seq_len(rr), seq_len(cc)] <- mat[seq_len(rr), seq_len(cc), drop = FALSE]
     glyph_mat(new_mat)
+    set_dirty(TRUE)
   }, ignoreInit = TRUE)
 
   # Add/overwrite glyph
   observeEvent(input$add_glyph, {
@@ -361,10 +436,12 @@ server <- function(input, output, session) {
     alph$glyphs[[key]] <- strings
     alphabet(alph)
 
     current_key(key)
     refresh_ui_from_alphabet(alph)
     updateSelectInput(session, "glyph_select", selected = key)
+    set_dirty(FALSE)
     showNotification(sprintf("Gemte glyph '%s'.", key), type = "message")
   }, ignoreInit = TRUE)
 
   # Delete glyph
   observeEvent(input$delete_glyph, {
@@ -380,6 +457,7 @@ server <- function(input, output, session) {
     alphabet(alph)
     current_key("")
+    set_dirty(FALSE)
     glyph_mat(blank_glyph_matrix(alph$height, max(1L, as.integer(input$glyph_width %||% 5))))
     refresh_ui_from_alphabet(alph)
     updateSelectInput(session, "glyph_select", selected = "")
     showNotification(sprintf("Slettede glyph '%s'.", key), type = "message")
@@ -390,39 +468,47 @@ server <- function(input, output, session) {
   observeEvent(input$clear_glyph, {
     mat <- glyph_mat()
     glyph_mat(mat * 0L)
+    set_dirty(TRUE)
   }, ignoreInit = TRUE)
 
   observeEvent(input$invert_glyph, {
     mat <- glyph_mat()
     glyph_mat(1L - mat)
+    set_dirty(TRUE)
   }, ignoreInit = TRUE)
 
   observeEvent(input$shift_left, {
     mat <- glyph_mat()
     if (ncol(mat) > 1) mat <- cbind(mat[, -1, drop = FALSE], 0L)
     glyph_mat(mat)
+    set_dirty(TRUE)
   }, ignoreInit = TRUE)
 
   observeEvent(input$shift_right, {
     mat <- glyph_mat()
     if (ncol(mat) > 1) mat <- cbind(0L, mat[, -ncol(mat), drop = FALSE])
     glyph_mat(mat)
+    set_dirty(TRUE)
   }, ignoreInit = TRUE)
 
   observeEvent(input$shift_up, {
     mat <- glyph_mat()
     if (nrow(mat) > 1) mat <- rbind(mat[-1, , drop = FALSE], rep(0L, ncol(mat)))
     glyph_mat(mat)
+    set_dirty(TRUE)
   }, ignoreInit = TRUE)
 
   observeEvent(input$shift_down, {
     mat <- glyph_mat()
     if (nrow(mat) > 1) mat <- rbind(rep(0L, ncol(mat)), mat[-nrow(mat), , drop = FALSE])
     glyph_mat(mat)
+    set_dirty(TRUE)
   }, ignoreInit = TRUE)
 
   # Click-to-toggle JS for DT
   toggle_js <- JS("
@@ -469,9 +555,11 @@ server <- function(input, output, session) {
     c <- as.integer(click$col)
     if (r >= 1 && r <= nrow(mat) && c >= 1 && c <= ncol(mat)) {
       mat[r, c] <- if (mat[r, c] == 1L) 0L else 1L
       glyph_mat(mat)
+      set_dirty(TRUE)
     }
   }, ignoreInit = TRUE)
 
   output$glyph_title <- renderText({
     key <- current_key() %||% ""
     if (nchar(key) == 0) "Ingen glyph valgt (rediger blank og 'Tilføj/overskriv')"
-    else sprintf("Glyph: '%s'", key)
+    else sprintf("Glyph: '%s'%s", key, if (isTRUE(glyph_dirty())) " *" else "")
   })
 
   output$glyph_dims <- renderText({
     mat <- glyph_mat()
     sprintf("Størrelse: %d × %d (højde × bredde)", nrow(mat), ncol(mat))
@@ -504,6 +592,20 @@ server <- function(input, output, session) {
       for (y in 0:h) lines(c(0, w), c(y, y))
 
+    # Baseline (0-indekseret fra toppen): tegn en linje
+    alph <- alphabet()
+    bl <- suppressWarnings(as.integer(alph$baseline %||% (h - 1L)))
+    if (!is.na(bl)) {
+      bl <- max(0L, min(bl, h - 1L))
+      # y=0 er bund, så baseline fra top bliver: h - bl - 1
+      y_bl <- h - bl - 1
+      lines(c(0, w), c(y_bl, y_bl), lwd = 2, lty = 2)
+    }
+
     # Filled squares
     filled <- which(mat == 1L, arr.ind = TRUE)
     if (nrow(filled) > 0) {
       for (i in seq_len(nrow(filled))) {
@@ -523,7 +625,7 @@ server <- function(input, output, session) {
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
-      Glyph = keys,
+      Glyph = ifelse(keys == " ", "\u2420", keys),
       Width = widths,
       stringsAsFactors = FALSE
     )
 
     datatable(
       df,
@@ -571,6 +673,7 @@ server <- function(input, output, session) {
     refresh_ui_from_alphabet(alph)
     if (nchar(current_key() %||% "") == 0) {
       glyph_mat(blank_glyph_matrix(alph$height, max(1L, as.integer(input$glyph_width %||% 5))))
+      set_dirty(FALSE)
     }
   })
 }
 
 shinyApp(ui, server)
