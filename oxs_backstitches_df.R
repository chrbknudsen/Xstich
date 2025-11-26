# forudsætter: library(xml2) er loaded

oxs_backstitches_df <- function(path) {
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

  ## --- backstitches ---
  bs_nodes <- xml_find_all(doc, ".//backstitches/backstitch")

  if (length(bs_nodes) == 0L) {
    return(
      data.frame(
        x1 = integer(0),
        y1 = integer(0),
        x2 = integer(0),
        y2 = integer(0),
        palindex = integer(0),
        type = character(0),
        thickness = character(0),
        marked = logical(0),
        number = character(0),
        name = character(0),
        color = character(0),
        symbol = character(0),
        stringsAsFactors = FALSE
      )
    )
  }

  back <- data.frame(
    x1        = as.integer(xml_attr(bs_nodes, "x1")),
    y1        = as.integer(xml_attr(bs_nodes, "y1")),
    x2        = as.integer(xml_attr(bs_nodes, "x2")),
    y2        = as.integer(xml_attr(bs_nodes, "y2")),
    palindex  = as.integer(xml_attr(bs_nodes, "palindex")),
    type      = xml_attr(bs_nodes, "type"),
    thickness = xml_attr(bs_nodes, "thickness"),
    marked    = xml_attr(bs_nodes, "marked"),
    stringsAsFactors = FALSE
  )

  # marked -> logical
  back$marked <- tolower(back$marked) %in% c("true", "1")

  ## --- join palette på palindex ---
  res <- merge(
    back, palette,
    by.x = "palindex", by.y = "index",
    all.x = TRUE,
    sort = FALSE
  )

  # sortér nogenlunde fornuftigt (først y1, så x1)
  res <- res[order(res$y1, res$x1), ]
  rownames(res) <- NULL

  res
}

