# forudsætter: library(xml2) er loaded

oxs_ornaments_df <- function(path) {
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

  ## --- ornaments / objects ---
  obj_nodes <- xml_find_all(doc, ".//ornaments_inc_knots_and_beads/object")

  if (length(obj_nodes) == 0L) {
    return(
      data.frame(
        x1 = numeric(0),
        y1 = numeric(0),
        x2 = numeric(0),
        y2 = numeric(0),
        width = numeric(0),
        height = numeric(0),
        palindex = integer(0),
        objecttype = character(0),
        words = character(0),
        marked = logical(0),
        number = character(0),
        name = character(0),
        color = character(0),
        symbol = character(0),
        stringsAsFactors = FALSE
      )
    )
  }

  ornaments <- data.frame(
    x1         = as.numeric(xml_attr(obj_nodes, "x1")),
    y1         = as.numeric(xml_attr(obj_nodes, "y1")),
    x2         = as.numeric(xml_attr(obj_nodes, "x2")),
    y2         = as.numeric(xml_attr(obj_nodes, "y2")),
    width      = as.numeric(xml_attr(obj_nodes, "width")),
    height     = as.numeric(xml_attr(obj_nodes, "height")),
    palindex   = as.integer(xml_attr(obj_nodes, "palindex")),
    objecttype = xml_attr(obj_nodes, "objecttype"),
    words      = xml_attr(obj_nodes, "words"),
    marked     = xml_attr(obj_nodes, "marked"),
    stringsAsFactors = FALSE
  )

  # marked -> logical
  ornaments$marked <- tolower(ornaments$marked) %in% c("true", "1")

  ## --- join palette på palindex (hvis den findes) ---
  res <- merge(
    ornaments, palette,
    by.x = "palindex", by.y = "index",
    all.x = TRUE,
    sort = FALSE
  )

  # sortér nogenlunde fornuftigt (først y1, så x1)
  res <- res[order(res$y1, res$x1), ]
  rownames(res) <- NULL

  res
}
