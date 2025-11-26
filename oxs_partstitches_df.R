# install.packages("xml2")
library(xml2)

oxs_partstitches_df <- function(path) {
  doc <- read_xml(path)

  ## --- palette ---
  pal_nodes <- xml_find_all(doc, ".//palette_item")

  palette <- data.frame(
    index   = as.integer(xml_attr(pal_nodes, "index")),
    number  = xml_attr(pal_nodes, "number"),
    name    = xml_attr(pal_nodes, "name"),
    color   = xml_attr(pal_nodes, "color"),
    symbol  = xml_attr(pal_nodes, "symbol"),
    stringsAsFactors = FALSE
  )

  ## --- partstitches ---
  ps_nodes <- xml_find_all(doc, ".//partstitches/partstitch")

  part <- data.frame(
    x          = as.integer(xml_attr(ps_nodes, "x")),
    y          = as.integer(xml_attr(ps_nodes, "y")),
    palindex1  = as.integer(xml_attr(ps_nodes, "palindex1")),
    palindex2  = as.integer(xml_attr(ps_nodes, "palindex2")),
    direction  = xml_attr(ps_nodes, "direction"),
    type       = xml_attr(ps_nodes, "type"),
    marked     = xml_attr(ps_nodes, "marked"),
    stringsAsFactors = FALSE
  )

  # gør 'marked' til logical
  part$marked <- tolower(part$marked) %in% c("true", "1")

  ## --- join palette 1 ---
  part <- merge(
    part, palette,
    by.x = "palindex1", by.y = "index",
    all.x = TRUE,
    sort = FALSE
  )

  names(part)[names(part) %in% c("number", "name", "color", "symbol")] <-
    paste0(names(part)[names(part) %in% c("number","name","color","symbol")], "_1")

  ## --- join palette 2 ---
  part <- merge(
    part, palette,
    by.x = "palindex2", by.y = "index",
    all.x = TRUE,
    sort = FALSE
  )

  names(part)[names(part) %in% c("number", "name", "color", "symbol")] <-
    paste0(names(part)[names(part) %in% c("number","name","color","symbol")], "_2")

  # sorter tilbage så (x,y) giver en naturlig orden
  part <- part[order(part$y, part$x), ]

  rownames(part) <- NULL
  part
}

