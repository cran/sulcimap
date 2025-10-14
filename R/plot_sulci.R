#' Plot sulcal measures on brain
#'
#' Generates composite brain figures (left/right lateral/medial) by mapping sulcal values.
#'
#' @param sulcus_values data.frame or path/to/csv
#'   Either (1) a two-column dataframe with \code{Sulcus} and \code{Value}, or
#'   (2) a path to a CSV file containing those columns.
#'   \code{Sulcus} must use BrainVISA-style tags (left/right, 4 types) and include all
#'   expected names. Missing sulcus should have \code{Value = 0} (see README and data samples).
#' @param palette character
#'   Color palette for mapping values and the colorbar. Options include
#'   \code{"viridis"}, \code{"magma"}, \code{"plasma"}, \code{"inferno"}, \code{"cividis"}, \code{"heat"}, \code{"gyr"}.
#' @param value_range numeric length-2 or NULL (default \code{NULL})
#'   Controls the color scale.
#'   \itemize{
#'     \item \code{NULL}: auto-scale from the min/max of the provided values.
#'     \item \code{c(min, max)}: fix the scale across plots/conditions for comparability.
#'   }
#' @param save_dir character or NULL (default \code{NULL})
#'   Directory where the plot image(s) are saved. If it does not exist, it will
#'   be created. Set to \code{NULL} to skip saving and only return the plot object.
#' @param measure character
#'   What to plot:
#'   \itemize{
#'     \item \code{"all"}: generate all four measures in one figure.
#'     \item \code{"opening"}, \code{"depth"}, \code{"surface"}, \code{"length"}: generate a single-measure plot.
#'   }
#' @param show_colorbar logical Show or hide the color bar on the figure.
#' @param caption character or expression
#'   Label displayed with the color bar. For math, use e.g. \code{expression(-log[10](p))}.
#' @param ... Advanced options:
#'   \code{base_dir} (path to assets),
#'   \code{scale_width} (magick scale, default "1000x"),
#'   \code{file_prefix} (default "sulci"),
#'   \code{width_in}, \code{height_in} (inches; defaults 14, 8),
#'   \code{dpi} (default 300).
#'
#' @return If \code{save_dir = NULL}, returns a \code{ggplot} (or \code{patchwork}) object
#'   representing the brain figure, which can be further modified and saved.
#'   If \code{save_dir} is not \code{NULL}, the plot image(s) are automatically saved in that directory
#'   and the function (invisibly) returns a character vector with the path(s) to the saved file(s).
#'
#' @examples
#' # Minimal executable example using assets_light
#' ex <- data.frame(
#'   Sulcus = c("S.C._left.opening", "S.C._right.opening",
#'   "S.F.int._left.opening", "S.F.int._right.opening"),
#'   Value  = c(1, 0.5, 0.8, 0.4)
#' )
#' assets_light <- sulcimap::get_assets_light()
#' p <- plot_sulci(
#'   sulcus_values = ex,
#'   measure       = "opening",
#'   palette       = "gyr",
#'   show_colorbar = FALSE,
#'   base_dir      = assets_light,
#'   save_dir      = NULL
#' )
#' # p is a ggplot/patchwork object; printing it will draw the figure.
#'
#' \donttest{
#' # Full example
#' # df should have columns: Sulcus (BrainVISA-style names with suffixes), Value
#' # plot_out <- plot_sulci(
#' #  sulcus_values = df,
#' #    palette       = "gyr",
#' #   value_range   = NULL,
#' #  save_dir      = NULL,
#' #    measure       = "opening",
#' #    show_colorbar = TRUE,
#' #    caption       = expression(-log[10](p))
#' # )
#' }
#' @export

plot_sulci <- function(
    sulcus_values,
    palette       = "gyr",
    value_range   = NULL,
    save_dir      = NULL,
    measure       = c("all","opening","depth","surface","length"),
    show_colorbar = TRUE,
    caption       = expression(-log[10](p)),
    ...
) {
  # ---- hidden defaults (overridable if desired) ----
  base_dir    <- NULL
  scale_width <- "1000x"
  file_prefix <- "sulci"
  width_in    <- 14
  height_in   <- 8
  dpi         <- 300

  dots <- list(...)
  if (!is.null(dots$base_dir))    base_dir    <- dots$base_dir
  if (!is.null(dots$scale_width)) scale_width <- dots$scale_width
  if (!is.null(dots$file_prefix)) file_prefix <- dots$file_prefix
  if (!is.null(dots$width_in))    width_in    <- dots$width_in
  if (!is.null(dots$height_in))   height_in   <- dots$height_in
  if (!is.null(dots$dpi))         dpi         <- dots$dpi

  if (is.null(base_dir)) base_dir <- get_sulcimap_assets_dir()

  # ---- helper: recolor one sulcus PNG ----
  recolor_sulcus <- function(sulcus_image, color) {
    sulcus_image <- magick::image_convert(sulcus_image, format = "png")
    sulcus_mask  <- magick::image_transparent(sulcus_image, 'black')
    colored      <- magick::image_colorize(sulcus_image, opacity = 100, color = color)
    magick::image_composite(colored, sulcus_mask, operator = "atop")
  }

  # ---- helper: render a single brain view from a set of sulcus values ----
  render_sulci_view <- function(sulcus_values, sulcus_dir, background_path,
                                palette = "viridis", scale_width = "1000x",
                                value_range = NULL) {
    sulcus_names   <- names(sulcus_values)
    background_img <- magick::image_read(background_path)
    bg_info        <- magick::image_info(background_img)

    if (is.null(value_range)) value_range <- range(sulcus_values, na.rm = TRUE)

    # Map to [1,1000] and keep out-of-range values on the edges
    remapped <- scales::rescale(
      sulcus_values,
      to   = c(1, 1000),
      from = value_range,
      oob  = scales::oob_squish
    )
    remapped <- as.integer(round(remapped))
    remapped[!is.finite(remapped)] <- 1L
    remapped <- pmin(pmax(remapped, 1L), 1000L)

    palette_map <- switch(
      palette,
      viridis = viridisLite::viridis(1000),
      magma   = viridisLite::magma(1000),
      plasma  = viridisLite::plasma(1000),
      inferno = viridisLite::inferno(1000),
      cividis = viridisLite::cividis(1000),
      heat    = grDevices::heat.colors(1000),
      gyr     = grDevices::colorRampPalette(c("grey90","yellow","gold","orange","darkorange",
                                              "orangered","red","firebrick","darkred"))(1000),
      viridisLite::viridis(1000)
    )
    sulcus_colors <- palette_map[remapped]

    composite_img <- background_img
    for (i in seq_along(sulcus_names)) {
      png_path <- file.path(sulcus_dir, paste0(sulcus_names[i], ".png"))
      if (file.exists(png_path)) {
        s_img <- magick::image_read(png_path)
        s_img <- magick::image_resize(
          s_img,
          magick::geometry_size_pixels(width = bg_info$width, height = bg_info$height, preserve_aspect = FALSE)
        )
        recolored     <- recolor_sulcus(s_img, sulcus_colors[i])
        composite_img <- magick::image_composite(composite_img, recolored)
      } else {
        warning(sprintf("Missing PNG for: %s", sulcus_names[i]))
      }
    }

    magick::image_scale(magick::image_trim(magick::image_transparent(composite_img, "white")), scale_width)
  }

  # ---- colorbar helper ----
  plot_colorbar <- function(my_palette, min_val, max_val, caption = NULL, n_colors = 1000) {
    pal <- grDevices::colorRampPalette(my_palette)(n_colors)
    df  <- data.frame(x = seq(0, 1, length.out = n_colors), y = 1)
    ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = x)) +
      ggplot2::geom_tile(height = 0.3) +
      ggplot2::scale_fill_gradientn(colours = pal) +
      ggplot2::coord_cartesian(xlim = c(-0.1, 1.1), ylim = c(0.5, 1.5), clip = "off") +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none",
                     plot.margin = ggplot2::margin(t = 10, r = 40, b = 20, l = 40)) +
      ggplot2::annotate("text", x = -0.02, y = 1, label = min_val, hjust = 1, vjust = 0.5, size = 10) +
      ggplot2::annotate("text", x = 1.02,  y = 1, label = max_val, hjust = 0, vjust = 0.5, size = 10) +
      {
        if (!is.null(caption)) {
          if (is.expression(caption)) {
            cap_chr <- as.character(caption)
            cap_chr <- sub("^expression\\((.*)\\)$", "\\1", cap_chr)
            ggplot2::annotate("text", x = 0.5, y = 0.85, label = cap_chr,
                              hjust = 0.5, vjust = 1, size = 10, parse = TRUE)
          } else {
            ggplot2::annotate("text", x = 0.5, y = 0.85, label = as.character(caption),
                              hjust = 0.5, vjust = 1, size = 10)
          }
        }
      }
  }

  # ---- assets & inputs (from extracted cache) ----
  name_files <- list(
    left_lateral  = file.path(base_dir, "leftlat_files",  "leftlatnames.txt"),
    left_medial   = file.path(base_dir, "leftmed_files",  "leftmednames.txt"),
    right_lateral = file.path(base_dir, "rightlat_files", "rightlatnames.txt"),
    right_medial  = file.path(base_dir, "rightmed_files","rightmednames.txt")
  )
  stopifnot(all(file.exists(unlist(name_files))))
  left_lateral_names  <- readLines(name_files$left_lateral)
  left_medial_names   <- readLines(name_files$left_medial)
  right_lateral_names <- readLines(name_files$right_lateral)
  right_medial_names  <- readLines(name_files$right_medial)

  sulcus_dirs <- list(
    left_lateral  = file.path(base_dir, "leftlat"),
    left_medial   = file.path(base_dir, "leftmed"),
    right_lateral = file.path(base_dir, "rightlat"),
    right_medial  = file.path(base_dir, "rightmed")
  )
  bgs <- list(
    left_lateral  = file.path(sulcus_dirs$left_lateral,  "brain_leftlat.png"),
    left_medial   = file.path(sulcus_dirs$left_medial,   "brain_leftmed.png"),
    right_lateral = file.path(sulcus_dirs$right_lateral, "brain_rightlat.png"),
    right_medial  = file.path(sulcus_dirs$right_medial,  "brain_rightmed.png")
  )
  stopifnot(all(file.exists(unlist(bgs))))

  # ---- coerce input: allow CSV path or data.frame ----
  if (is.character(sulcus_values) && length(sulcus_values) == 1L && file.exists(sulcus_values)) {
    sulcus_values <- utils::read.csv(sulcus_values, check.names = FALSE, stringsAsFactors = FALSE)
  } else if (inherits(sulcus_values, "tbl_df")) {
    sulcus_values <- as.data.frame(sulcus_values, stringsAsFactors = FALSE)
  } else if (!is.data.frame(sulcus_values)) {
    stop("`sulcus_values` must be a data.frame or a path to a CSV file.")
  }

  # ---- split metrics ----
  df <- sulcus_values
  stopifnot(all(c("Sulcus", "Value") %in% names(df)))
  df_opening <- subset(df, grepl("\\.opening$", Sulcus));                     df_opening$Sulcus <- sub("\\.opening$", "", df_opening$Sulcus)
  df_depth   <- subset(df, grepl("\\.meandepth_native$", Sulcus));            df_depth$Sulcus   <- sub("\\.meandepth_native$", "", df_depth$Sulcus)
  df_surface <- subset(df, grepl("\\.surface_native$", Sulcus));              df_surface$Sulcus <- sub("\\.surface_native$", "", df_surface$Sulcus)
  df_length  <- subset(df, grepl("\\.hull_junction_length_native$", Sulcus)); df_length$Sulcus  <- sub("\\.hull_junction_length_native$", "", df_length$Sulcus)

  names(df_opening) <- c("Sulcus", "Value")
  names(df_depth)   <- c("Sulcus", "Value")
  names(df_surface) <- c("Sulcus", "Value")
  names(df_length)  <- c("Sulcus", "Value")

  nv <- function(x, order_names) {
    out <- stats::setNames(rep(0, length(order_names)), order_names)
    m <- match(order_names, x$Sulcus)
    hit <- !is.na(m)
    out[hit] <- x$Value[m[hit]]
    out
  }

  sulc_vals <- list(
    opening = list(
      left_lateral  = nv(df_opening, left_lateral_names),
      left_medial   = nv(df_opening, left_medial_names),
      right_lateral = nv(df_opening, right_lateral_names),
      right_medial  = nv(df_opening, right_medial_names)
    ),
    depth = list(
      left_lateral  = nv(df_depth, left_lateral_names),
      left_medial   = nv(df_depth, left_medial_names),
      right_lateral = nv(df_depth, right_lateral_names),
      right_medial  = nv(df_depth, right_medial_names)
    ),
    surface = list(
      left_lateral  = nv(df_surface, left_lateral_names),
      left_medial   = nv(df_surface, left_medial_names),
      right_lateral = nv(df_surface, right_lateral_names),
      right_medial  = nv(df_surface, right_medial_names)
    ),
    length = list(
      left_lateral  = nv(df_length, left_lateral_names),
      left_medial   = nv(df_length, left_medial_names),
      right_lateral = nv(df_length, right_lateral_names),
      right_medial  = nv(df_length, right_medial_names)
    )
  )

  # ---- global range ----
  rng <- if (is.null(value_range)) {
    all_values <- unlist(sulc_vals, use.names = FALSE)
    r <- range(all_values, na.rm = TRUE)
    r[2] <- ceiling(r[2] * 2) / 2
    r
  } else value_range

  # ---- validate range ----
  if (!is.numeric(rng) || length(rng) != 2 || any(!is.finite(rng)) || rng[1] >= rng[2]) {
    stop("`value_range` must be NULL or numeric length-2 with min < max.")
  }

  # ---- per-view builder ----
  make_view_plot <- function(vals, side) {
    fig <- render_sulci_view(
      sulcus_values   = vals[[side]],
      sulcus_dir      = sulcus_dirs[[side]],
      background_path = bgs[[side]],
      palette         = palette,
      scale_width     = scale_width,
      value_range     = rng
    )
    ggplot2::ggplot() +
      ggplot2::annotation_custom(grid::rasterGrob(fig)) +
      ggplot2::theme_void()
  }

  # ---- colorbar if requested ----
  if (isTRUE(show_colorbar)) {
    my_palette <- switch(
      palette,
      gyr     = c("grey90","yellow","gold","orange","darkorange","orangered","red","firebrick","darkred"),
      viridis = viridisLite::viridis(9),
      magma   = viridisLite::magma(9),
      plasma  = viridisLite::plasma(9),
      inferno = viridisLite::inferno(9),
      cividis = viridisLite::cividis(9),
      heat    = grDevices::heat.colors(9),
      c("grey90","yellow","gold","orange","darkorange","orangered","red","firebrick","darkred")
    )
    colorbar <- plot_colorbar(my_palette, min_val = rng[1], max_val = rng[2], caption = caption)
    bottom_row <- patchwork::wrap_plots(
      patchwork::plot_spacer(), colorbar, patchwork::plot_spacer(),
      ncol = 3, widths = c(0.3, 0.3, 0.3)
    )
  }

  # ---- build grids + titles (no operator syntax) ----
  build_metric_grid <- function(metric) {
    mvals <- sulc_vals[[metric]]
    patchwork::wrap_plots(
      make_view_plot(mvals, "left_lateral"),
      make_view_plot(mvals, "left_medial"),
      make_view_plot(mvals, "right_lateral"),
      make_view_plot(mvals, "right_medial"),
      ncol = 4
    )
  }

  title_plot  <- function(txt) cowplot::ggdraw() + cowplot::draw_label(txt, size = 30, fontface = "bold")
  metric_title <- function(metric) switch(metric,
                                          opening = "Width", depth = "Depth", surface = "Surface Area", length = "Length")

  title_font_size <- 22
  if (match.arg(measure) == "all") {
    label_width   <- patchwork::wrap_elements(full = grid::textGrob("Width",        gp = grid::gpar(fontsize = title_font_size), just = "center"))
    label_depth   <- patchwork::wrap_elements(full = grid::textGrob("Depth",        gp = grid::gpar(fontsize = title_font_size), just = "center"))
    label_surface <- patchwork::wrap_elements(full = grid::textGrob("Surface Area", gp = grid::gpar(fontsize = title_font_size), just = "center"))
    label_length  <- patchwork::wrap_elements(full = grid::textGrob("Length",       gp = grid::gpar(fontsize = title_font_size), just = "center"))
    label_left    <- cowplot::ggdraw() + cowplot::draw_label("Left",  size = title_font_size)
    label_right   <- cowplot::ggdraw() + cowplot::draw_label("Right", size = title_font_size)
    empty_label   <- patchwork::plot_spacer()

    top_row <- patchwork::wrap_plots(
      empty_label, label_left, label_right,
      ncol = 3, widths = c(0.15, 0.5, 0.5)
    )

    combined_opening_brain <- build_metric_grid("opening")
    combined_depth_brain   <- build_metric_grid("depth")
    combined_surface_brain <- build_metric_grid("surface")
    combined_length_brain  <- build_metric_grid("length")

    row1 <- patchwork::wrap_plots(label_width,   combined_opening_brain, ncol = 2, widths = c(0.15, 1))
    row2 <- patchwork::wrap_plots(label_depth,   combined_depth_brain,   ncol = 2, widths = c(0.15, 1))
    row3 <- patchwork::wrap_plots(label_surface, combined_surface_brain, ncol = 2, widths = c(0.15, 1))
    row4 <- patchwork::wrap_plots(label_length,  combined_length_brain,  ncol = 2, widths = c(0.15, 1))

    final_plot <- patchwork::wrap_plots(
      top_row, row1, row2, row3, row4,
      ncol = 1, heights = c(0.1, 1, 1, 1, 1)
    )

    combined_all <- if (isTRUE(show_colorbar)) {
      patchwork::wrap_plots(
        final_plot,
        patchwork::wrap_plots(patchwork::plot_spacer(), colorbar, patchwork::plot_spacer(), ncol = 3),
        ncol = 1, heights = c(1, 0.3)
      )
    } else final_plot

    if (!is.null(save_dir)) {
      if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
      out_path <- file.path(save_dir, sprintf("%s_all.png", file_prefix))
      ggplot2::ggsave(out_path, plot = combined_all, width = width_in, height = height_in, dpi = dpi)
      message("Plot saved to: ", out_path)
      return(invisible(out_path))
    }
    return(combined_all)
  }

  measure <- match.arg(measure)
  grid_plot <- build_metric_grid(measure)

  out_plot <- if (isTRUE(show_colorbar)) {
    patchwork::wrap_plots(
      title_plot(metric_title(measure)),
      grid_plot,
      bottom_row,
      ncol = 1, heights = c(0.15, 1, 0.25)
    )
  } else {
    patchwork::wrap_plots(
      title_plot(metric_title(measure)),
      grid_plot,
      ncol = 1, heights = c(0.15, 1)
    )
  }

  if (!is.null(save_dir)) {
    if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
    out_path <- file.path(save_dir, sprintf("%s_%s.png", file_prefix, measure))
    ggplot2::ggsave(out_path, plot = out_plot, width = width_in, height = height_in, dpi = dpi)
    message("Plot saved to: ", out_path)
    return(invisible(out_path))
  }
  out_plot
}
