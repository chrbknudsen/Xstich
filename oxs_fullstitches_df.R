# install.packages("xml2")
library(xml2)

oxs_fullstitches_df <- function(path) {
  doc <- read_xml(path)

  ## --- palette: én række per <palette_item> ---
  pal_nodes <- xml_find_all(doc, ".//palette_item")

  palette <- data.frame(
    index   = as.integer(xml_attr(pal_nodes, "index")),
    number  = xml_attr(pal_nodes, "number"),
    name    = xml_attr(pal_nodes, "name"),
    color   = xml_attr(pal_nodes, "color"),
    symbol  = xml_attr(pal_nodes, "symbol"),
    stringsAsFactors = FALSE
  )

  ## --- fullstitches: én række per <stitch> ---
  st_nodes <- xml_find_all(doc, ".//fullstitches/stitch")

  full <- data.frame(
    x        = as.integer(xml_attr(st_nodes, "x")),
    y        = as.integer(xml_attr(st_nodes, "y")),
    palindex = as.integer(xml_attr(st_nodes, "palindex")),
    marked   = xml_attr(st_nodes, "marked"),
    stringsAsFactors = FALSE
  )

  # marked -> logical (TRUE hvis "true"/"1", ellers FALSE/NA)
  full$marked <- tolower(full$marked) %in% c("true", "1")

  ## --- join: tilføj palette-info til hver stitch ---
  res <- merge(
    full, palette,
    by.x = "palindex", by.y = "index",
    all.x = TRUE,
    sort = FALSE
  )

  res
}


