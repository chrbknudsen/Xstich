library(ggplot2)



plot_oxs_full_gg <- function(path, major_every = 10) {

  fs <- oxs_fullstitches_df(path)
  bs <- oxs_backstitches_df(path)
  ps <- oxs_partstitches_df(path)

  ## --- helper til farver ---
  norm_hex <- function(z) {
    ifelse(
      is.na(z) | z == "",
      NA_character_,
      ifelse(substr(z, 1, 1) == "#", z, paste0("#", z))
    )
  }

  ## --- FULL STITCHES ---
  fs$color_hex <- norm_hex(fs$color)

  # max_x, max_y i GRID-koordinater (OXS: y nedad)
  max_y <- max(c(fs$y, ps$y), na.rm = TRUE)
  max_x <- max(c(fs$x, ps$x), na.rm = TRUE)

  # flip y til plot-koordinater: (1,1) øverst
  fs$y_plot <- max_y - fs$y + 1

  ## --- GRIDLINJER ---
  v_lines_minor <- data.frame(x = seq(0.5, max_x + 0.5, by = 1))
  h_lines_minor <- data.frame(y = seq(0.5, max_y + 0.5, by = 1))

  v_lines_major <- data.frame(x = seq(0.5, max_x + 0.5, by = major_every))
  h_lines_major <- data.frame(y = seq(0.5, max_y + 0.5, by = major_every))

  ## --- BACKSTITCHES ---
  if (nrow(bs) > 0) {
    bs$color_hex <- norm_hex(bs$color)

    # Backstitch-koordinater er i GRID-hjørner, så vi flytter 0.5 venstre og op
    bs$x1_adj_grid <- bs$x1 - 0.5
    bs$y1_adj_grid <- bs$y1 - 0.5
    bs$x2_adj_grid <- bs$x2 - 0.5
    bs$y2_adj_grid <- bs$y2 - 0.5

    # Konverter til plot-koordinater (samme transform som for fullstitches)
    bs$x1_adj <- bs$x1_adj_grid
    bs$x2_adj <- bs$x2_adj_grid
    bs$y1_plot <- max_y - bs$y1_adj_grid + 1
    bs$y2_plot <- max_y - bs$y2_adj_grid + 1
  }

  ## --- PART STITCHES: trekanter baseret på direction 1/2 ---
  ps_poly <- NULL
  if (nrow(ps) > 0) {

    ps$color_1_hex <- norm_hex(ps$color_1)
    ps$color_2_hex <- norm_hex(ps$color_2)

    polys <- list()
    k <- 1L

    for (i in seq_len(nrow(ps))) {
      row <- ps[i, ]

      # GRID-center
      xg <- row$x
      yg <- row$y

      left   <- xg - 0.5
      right  <- xg + 0.5
      top    <- yg - 0.5    # y nedad i grid
      bottom <- yg + 0.5

      # hjørner i GRID-koordinater
      tl_g <- c(left,  top)
      tr_g <- c(right, top)
      bl_g <- c(left,  bottom)
      br_g <- c(right, bottom)

      dir <- row$direction
      if (is.na(dir)) dir <- 1L

      ## helper: konverter en 3x2-matrix af GRID-punkter til plot-dataframe
      tri_to_df <- function(tri_g, fill, id) {
        # GRID -> PLOT
        x_plot <- tri_g[, 1]
        y_plot <- max_y - tri_g[, 2] + 1
        data.frame(
          x    = x_plot,
          y    = y_plot,
          fill = fill,
          id   = id,
          stringsAsFactors = FALSE
        )
      }

      ## palindex1 (første farve)
      if (!is.na(row$palindex1) && !is.na(row$color_1_hex)) {
        if (dir == 1L) {
          # direction 1: diagonal \ (TL -> BR) i GRID
          tri1_g <- rbind(tl_g, bl_g, br_g)   # under diagonalen i én retning
        } else if (dir == 2L) {
          # direction 2: diagonal / (BL -> TR) i GRID
          tri1_g <- rbind(bl_g, tl_g, tr_g)
        } else {
          tri1_g <- rbind(tl_g, bl_g, br_g)   # fallback
        }

        polys[[k]] <- tri_to_df(tri1_g, row$color_1_hex,
                                paste0("ps_", i, "_1"))
        k <- k + 1L
      }

      ## palindex2 (anden farve)
      if (!is.na(row$palindex2) && !is.na(row$color_2_hex)) {
        if (dir == 1L) {
          tri2_g <- rbind(tl_g, tr_g, br_g)
        } else if (dir == 2L) {
          tri2_g <- rbind(bl_g, br_g, tr_g)
        } else {
          tri2_g <- rbind(bl_g, br_g, tr_g)
        }

        polys[[k]] <- tri_to_df(tri2_g, row$color_2_hex,
                                paste0("ps_", i, "_2"))
        k <- k + 1L
      }
    }

    if (length(polys) > 0) {
      ps_poly <- do.call(rbind, polys)
    }
  }

  ## --- SELVE PLOTTET ---
  ggplot() +
    # FULL STITCHES
    geom_tile(
      data = fs,
      aes(x = x, y = y_plot, fill = color_hex),
      width = 1,
      height = 1
    ) +

    # PART STITCHES (diagonale trekanter ovenpå)
    { if (!is.null(ps_poly) && nrow(ps_poly) > 0)
        geom_polygon(
          data = ps_poly,
          aes(x = x, y = y, group = id, fill = fill)
        )
      else NULL
    } +

    # GRID: minor
    geom_vline(
      data = v_lines_minor,
      aes(xintercept = x),
      linewidth = 0.2,
      colour = "grey80"
    ) +
    geom_hline(
      data = h_lines_minor,
      aes(yintercept = y),
      linewidth = 0.2,
      colour = "grey80"
    ) +

    # GRID: major (hver 10.)
    geom_vline(
      data = v_lines_major,
      aes(xintercept = x),
      linewidth = 0.6,
      colour = "grey40"
    ) +
    geom_hline(
      data = h_lines_major,
      aes(yintercept = y),
      linewidth = 0.6,
      colour = "grey40"
    ) +

    # BACKSTITCHES
    { if (nrow(bs) > 0)
        geom_segment(
          data = bs,
          aes(
            x = x1_adj, y = y1_plot,
            xend = x2_adj, yend = y2_plot,
            colour = color_hex
          ),
          linewidth = 0.5,
          lineend = "round"
        )
      else NULL
    } +

    coord_equal() +
    scale_fill_identity() +
    scale_colour_identity() +
    theme_void()
}


plot_oxs_full_gg("piggies.OXS")
