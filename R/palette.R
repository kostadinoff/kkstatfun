# ============================================================
# DISCRETE COLOUR PALETTE ("flag" palette from N anchor colours)
# ============================================================

#' KK Colour Palette
#'
#' @description Generates `n` colours from a set of anchor ("flag") colours. When
#'   `n` does not exceed the number of anchors the anchor colours are used
#'   directly; when more are needed the anchors are interpolated with
#'   [grDevices::colorRampPalette()]. The anchors default to those set by
#'   [set_plot_colors()].
#'
#' @param n Number of colours required.
#' @param colors Character vector of anchor colours (hex). Defaults to the palette
#'   registered by [set_plot_colors()], or a red/navy/amber flag if none is set.
#'
#' @return Character vector of `n` hex colours.
#'
#' @examples
#' kk_pal(2)
#' kk_pal(6, colors = c("#D62828", "#003049", "#F77F00"))
#'
#' @export
kk_pal <- function(n, colors = getOption("kkstatfun.colors")) {
  if (is.null(colors)) colors <- c("#D62828", "#003049", "#F77F00")
  if (n <= length(colors)) {
    colors[seq_len(n)]
  } else {
    grDevices::colorRampPalette(colors)(n)
  }
}

#' Set the Default Discrete Plot Palette
#'
#' @description Registers a set of anchor ("flag") colours and makes every
#'   discrete `ggplot2` colour and fill scale draw from them by default — so
#'   [kkplot()] (and any `ggplot()`) automatically uses your chosen colours for
#'   grouped bars, points, lines, etc. Continuous colour/fill scales (e.g.
#'   heatmaps) are left untouched. Fewer groups than anchors use the anchor
#'   colours directly; more groups interpolate between them.
#'
#'   This is the colour counterpart to [set_plot_font()]; a typical analysis
#'   preamble is `kk_setup(); set_plot_font("Roboto Condensed", size = 14);
#'   set_plot_colors(c("#D62828", "#003049", "#F77F00"))`.
#'
#' @param colors Character vector of anchor colours (hex codes), e.g. three flag
#'   colours.
#' @param n_max Largest number of discrete levels to pre-build palettes for
#'   (default 24).
#' @param set_default Whether to register the palette as the global ggplot2
#'   default for discrete colour and fill (default TRUE).
#'
#' @return Invisibly, the validated anchor colours.
#'
#' @examples
#' \dontrun{
#' set_plot_colors(c("#D62828", "#003049", "#F77F00"))
#' # now discrete fills/colours use the flag palette automatically
#' kkplot(mtcars, ggplot2::aes(factor(cyl), fill = factor(cyl))) +
#'   ggplot2::geom_bar()
#' }
#'
#' @export
set_plot_colors <- function(colors, n_max = 24, set_default = TRUE) {
  if (!is.character(colors) || length(colors) < 1) {
    stop("`colors` must be a non-empty character vector of colours.")
  }
  # Validate that every entry is a usable colour
  ok <- vapply(colors, function(cl) {
    tryCatch({
      grDevices::col2rgb(cl)
      TRUE
    }, error = function(e) FALSE)
  }, logical(1))
  if (!all(ok)) {
    stop("Invalid colour(s): ", paste(colors[!ok], collapse = ", "))
  }

  options(kkstatfun.colors = unname(colors))

  if (set_default) {
    pal_list <- lapply(seq_len(n_max), function(k) kk_pal(k, colors))
    options(
      ggplot2.discrete.colour = pal_list,
      ggplot2.discrete.fill = pal_list
    )
    message(sprintf("✓ Default discrete palette set from %d anchor colour(s): %s",
      length(colors), paste(colors, collapse = ", ")))
  }
  invisible(unname(colors))
}

#' Discrete KK Colour/Fill Scales
#'
#' @description ggplot2 discrete scales that use the registered [set_plot_colors()]
#'   anchors via [kk_pal()]. Use these to apply the flag palette to a single plot
#'   explicitly (equivalent to what [set_plot_colors()] makes the default).
#'
#' @param ... Passed to [ggplot2::discrete_scale()].
#' @param aesthetics Aesthetics this scale applies to.
#'
#' @return A ggplot2 scale.
#'
#' @examples
#' \dontrun{
#' kkplot(mtcars, ggplot2::aes(factor(cyl), fill = factor(cyl))) +
#'   ggplot2::geom_bar() + scale_fill_kk()
#' }
#'
#' @rdname scale_kk
#' @export
scale_colour_kk <- function(..., aesthetics = "colour") {
  ggplot2::discrete_scale(aesthetics, palette = function(n) kk_pal(n), ...)
}

#' @rdname scale_kk
#' @export
scale_color_kk <- scale_colour_kk

#' @rdname scale_kk
#' @export
scale_fill_kk <- function(..., aesthetics = "fill") {
  ggplot2::discrete_scale(aesthetics, palette = function(n) kk_pal(n), ...)
}
