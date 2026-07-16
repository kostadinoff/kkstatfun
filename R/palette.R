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
#' @param colors Character vector of **1 to 12** anchor colours (hex codes or R
#'   colour names), e.g. three flag colours.
#' @param scheme Optional name of a derived palette to build from `colors` with
#'   [kk_gen_palettes()] before registering — one of `"custom"`, `"sequential"`,
#'   `"monochromatic"`, `"tints"`, `"shades"`, `"analogous"`, `"complementary"`,
#'   `"split_complementary"`, `"triadic"`, `"tetradic"`, `"spectral"`. When
#'   `NULL` (default) `colors` are registered as-is.
#' @param n Number of colours in the derived palette when `scheme` is supplied
#'   (default: the larger of `length(colors)` and 6). Ignored when `scheme` is
#'   `NULL`.
#' @param n_max Largest number of discrete levels to pre-build palettes for
#'   (default 24).
#' @param set_default Whether to register the palette as the global ggplot2
#'   default for discrete colour and fill (default TRUE).
#' @param continuous Whether to also make a gradient built from the anchors the
#'   global default for *continuous* colour and fill scales, e.g. heatmaps
#'   (default FALSE, leaving continuous scales as ggplot2's viridis default). The
#'   explicit [scale_fill_kk_c()] / [scale_colour_kk_c()] scales apply the
#'   gradient to a single plot regardless of this setting.
#'
#' @return Invisibly, the validated anchor colours that were registered.
#'
#' @seealso [kk_gen_palettes()] to browse the derived schemes, and
#'   [kk_show_palettes()] to preview them.
#'
#' @examples
#' \dontrun{
#' set_plot_colors(c("#D62828", "#003049", "#F77F00"))
#' # now discrete fills/colours use the flag palette automatically
#' kkplot(mtcars, ggplot2::aes(factor(cyl), fill = factor(cyl))) +
#'   ggplot2::geom_bar()
#'
#' # derive a palette from a single seed and register it
#' set_plot_colors("#D62828", scheme = "triadic", n = 6)
#'
#' # opt in to the gradient for heatmaps too
#' set_plot_colors(c("#D62828", "#003049", "#F77F00"), continuous = TRUE)
#' }
#'
#' @export
set_plot_colors <- function(colors, scheme = NULL, n = NULL, n_max = 24,
                            set_default = TRUE, continuous = FALSE) {
  colors <- .kk_validate_colors(colors)

  # Optionally expand a seed (or seeds) into a derived colour-theory scheme.
  if (!is.null(scheme)) {
    if (!is.character(scheme) || length(scheme) != 1) {
      stop("`scheme` must be a single scheme name (see ?kk_gen_palettes).")
    }
    if (is.null(n)) n <- max(length(colors), 6L)
    pals <- kk_gen_palettes(colors, n = n)
    if (!scheme %in% names(pals)) {
      stop(sprintf("Unknown scheme '%s'. Available: %s.",
        scheme, paste(names(pals), collapse = ", ")))
    }
    colors <- pals[[scheme]]
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

  if (continuous) {
    anchors <- unname(colors)
    options(
      ggplot2.continuous.fill = function(...) {
        ggplot2::scale_fill_gradientn(colours = anchors, ...)
      },
      ggplot2.continuous.colour = function(...) {
        ggplot2::scale_colour_gradientn(colours = anchors, ...)
      }
    )
    message("✓ Default continuous gradient set from the same anchors.")
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

#' Continuous KK Colour/Fill Gradient Scales
#'
#' @description ggplot2 *continuous* scales that build a gradient from the
#'   registered [set_plot_colors()] anchors (via [ggplot2::scale_fill_gradientn()]
#'   / [ggplot2::scale_colour_gradientn()]) — the continuous counterpart to
#'   [scale_fill_kk()], for heatmaps and other continuously-mapped fills/colours.
#'
#' @param ... Passed to the underlying `gradientn` scale.
#' @param colors Anchor colours; defaults to those set by [set_plot_colors()].
#'
#' @return A ggplot2 continuous scale.
#'
#' @examples
#' \dontrun{
#' kkplot(faithfuld, ggplot2::aes(waiting, eruptions, fill = density)) +
#'   ggplot2::geom_raster() + scale_fill_kk_c()
#' }
#'
#' @rdname scale_kk_c
#' @export
scale_fill_kk_c <- function(..., colors = getOption("kkstatfun.colors")) {
  if (is.null(colors)) colors <- c("#D62828", "#003049", "#F77F00")
  ggplot2::scale_fill_gradientn(colours = colors, ...)
}

#' @rdname scale_kk_c
#' @export
scale_colour_kk_c <- function(..., colors = getOption("kkstatfun.colors")) {
  if (is.null(colors)) colors <- c("#D62828", "#003049", "#F77F00")
  ggplot2::scale_colour_gradientn(colours = colors, ...)
}

#' @rdname scale_kk_c
#' @export
scale_color_kk_c <- scale_colour_kk_c

# ============================================================
# PALETTE GENERATORS  (derive scheme palettes from 1–12 seeds)
# ============================================================

# ---- internal colour helpers (base grDevices only) --------

# Validate 1–12 usable colours; return them unnamed.
.kk_validate_colors <- function(colors, max_n = 12L) {
  if (!is.character(colors) || length(colors) < 1) {
    stop("`colors` must be a non-empty character vector of colours.")
  }
  if (length(colors) > max_n) {
    stop(sprintf("Provide at most %d colours (got %d).", max_n, length(colors)))
  }
  ok <- vapply(colors, function(cl) {
    tryCatch({
      grDevices::col2rgb(cl)
      TRUE
    }, error = function(e) FALSE)
  }, logical(1))
  if (!all(ok)) {
    stop("Invalid colour(s): ", paste(colors[!ok], collapse = ", "))
  }
  unname(colors)
}

# hex/name -> HSV matrix (rows h, s, v in [0, 1]).
.kk_to_hsv <- function(cols) grDevices::rgb2hsv(grDevices::col2rgb(cols))

# HSV -> hex (vectorised; wraps hue, clamps s/v).
.kk_from_hsv <- function(h, s, v) {
  grDevices::hsv(h %% 1, pmin(pmax(s, 0), 1), pmin(pmax(v, 0), 1))
}

# Rotate the hue of `cols` by `deg` degrees (deg may be scalar or length(cols)).
.kk_rotate_hue <- function(cols, deg) {
  hsv <- .kk_to_hsv(cols)
  .kk_from_hsv(hsv["h", ] + deg / 360, hsv["s", ], hsv["v", ])
}

# Expand qualitative anchors to exactly n by cycling with tint/shade variation.
.kk_qual_expand <- function(anchors, n) {
  if (n <= length(anchors)) return(anchors[seq_len(n)])
  hsv <- .kk_to_hsv(anchors)
  out <- character(0)
  cycle <- 0
  while (length(out) < n) {
    if (cycle %% 2 == 0) {
      # darken/desaturate
      v <- hsv["v", ] * (0.78^(cycle / 2))
      s <- hsv["s", ] * (0.90^(cycle / 2))
    } else {
      # lighten (tint toward white) for contrast against the darker cycle
      v <- pmin(1, hsv["v", ] + (1 - hsv["v", ]) * 0.55)
      s <- hsv["s", ] * 0.55
    }
    out <- c(out, .kk_from_hsv(hsv["h", ], s, v))
    cycle <- cycle + 1
  }
  out[seq_len(n)]
}

#' Generate Colour-Theory Palettes from Seed Colours
#'
#' @description From **1 to 12** seed colours, derive a named catalogue of
#'   palettes — sequential/monochromatic ramps and qualitative colour-theory
#'   schemes (analogous, complementary, triadic, tetradic, …) — each returned as
#'   exactly `n` hex colours ready for [kkplot()] / [scale_fill_kk()]. Schemes are
#'   built in HSV space from the **first** seed; the `sequential` ramp (and the
#'   `custom` entry, present only when more than one seed is given) use all seeds
#'   as anchors.
#'
#'   This is the programmatic counterpart to online generators such as
#'   coolors.co: pick a scheme you like, then make it the plotting default with
#'   `set_plot_colors(seed, scheme = "...")`.
#'
#' @param colors Character vector of 1–12 seed colours (hex or R colour names).
#' @param n Number of colours per palette (default 6).
#' @param plot If TRUE, also draw the swatches via [kk_show_palettes()]
#'   (default FALSE).
#'
#' @return A named list (class `kk_palettes`) of length-`n` hex vectors, with the
#'   seed colours stored in `attr(x, "seed")`. Names: `custom` (multi-seed only),
#'   `sequential`, `monochromatic`, `tints`, `shades`, `analogous`,
#'   `complementary`, `split_complementary`, `triadic`, `tetradic`, `spectral`.
#'
#' @examples
#' pals <- kk_gen_palettes("#D62828", n = 6)
#' pals$triadic
#' \dontrun{
#' kk_gen_palettes(c("#D62828", "#003049", "#F77F00"), n = 8, plot = TRUE)
#' }
#'
#' @seealso [kk_show_palettes()], [set_plot_colors()], [kk_pal()].
#' @export
kk_gen_palettes <- function(colors, n = 6, plot = FALSE) {
  colors <- .kk_validate_colors(colors)
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1) {
    stop("`n` must be a single positive integer.")
  }
  n <- as.integer(n)

  seed <- colors[1]
  hsv0 <- .kk_to_hsv(seed)
  h0 <- hsv0["h", 1]
  s0 <- hsv0["s", 1]
  v0 <- hsv0["v", 1]

  grad <- function(cols) grDevices::colorRampPalette(cols)(n)

  # A light and a dark companion of the seed for the sequential ramp.
  seed_light <- .kk_from_hsv(h0, s0 * 0.30, v0 + (1 - v0) * 0.55)
  seed_dark <- .kk_from_hsv(h0, min(1, s0 + 0.10), v0 * 0.40)

  pals <- list(
    sequential = if (length(colors) > 1) grad(colors) else grad(c(seed_light, seed, seed_dark)),
    monochromatic = .kk_from_hsv(
      rep(h0, n), rep(s0, n),
      seq(v0 + (1 - v0) * 0.40, max(0.22, v0 * 0.45), length.out = n)
    ),
    tints = grad(c(seed, "#ffffff")),
    shades = grad(c(seed, .kk_from_hsv(h0, s0, v0 * 0.18))),
    analogous = .kk_rotate_hue(rep(seed, n), seq(-40, 40, length.out = n)),
    complementary = .kk_qual_expand(
      c(seed, .kk_rotate_hue(seed, 180)), n
    ),
    split_complementary = .kk_qual_expand(
      c(seed, .kk_rotate_hue(seed, 150), .kk_rotate_hue(seed, 210)), n
    ),
    triadic = .kk_qual_expand(
      c(seed, .kk_rotate_hue(seed, 120), .kk_rotate_hue(seed, 240)), n
    ),
    tetradic = .kk_qual_expand(
      c(seed, .kk_rotate_hue(seed, 90), .kk_rotate_hue(seed, 180),
        .kk_rotate_hue(seed, 270)), n
    ),
    spectral = .kk_rotate_hue(
      rep(seed, n), utils::head(seq(0, 360, length.out = n + 1), n)
    )
  )

  if (length(colors) > 1) {
    custom <- if (n <= length(colors)) colors[seq_len(n)] else grad(colors)
    pals <- c(list(custom = custom), pals)
  }

  attr(pals, "seed") <- colors
  attr(pals, "n") <- n
  class(pals) <- c("kk_palettes", "list")

  if (isTRUE(plot)) print(kk_show_palettes(pals))
  pals
}

#' Preview Generated Palettes as Swatches
#'
#' @description Draws each palette from [kk_gen_palettes()] (or any named list of
#'   colour vectors) as a row of swatches, hex codes annotated — a quick way to
#'   choose a scheme before calling [set_plot_colors()].
#'
#' @param palettes A `kk_palettes` list from [kk_gen_palettes()], or any named
#'   list of hex-colour vectors.
#' @param labels Whether to print the hex code inside each swatch (default TRUE).
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' kk_show_palettes(kk_gen_palettes("#003049", n = 6))
#' }
#'
#' @seealso [kk_gen_palettes()], [set_plot_colors()].
#' @export
kk_show_palettes <- function(palettes, labels = TRUE) {
  if (!is.list(palettes) || is.null(names(palettes))) {
    stop("`palettes` must be a *named* list of colour vectors.")
  }
  df <- do.call(rbind, lapply(names(palettes), function(nm) {
    cols <- palettes[[nm]]
    data.frame(
      scheme = nm,
      idx = seq_along(cols),
      hex = toupper(cols),
      stringsAsFactors = FALSE
    )
  }))
  df$scheme <- factor(df$scheme, levels = rev(names(palettes)))
  # Choose black/white label per swatch for contrast.
  lum <- apply(grDevices::col2rgb(df$hex) / 255, 2, function(x) {
    0.2126 * x[1] + 0.7152 * x[2] + 0.0722 * x[3]
  })
  df$txt <- ifelse(lum > 0.55, "#000000", "#FFFFFF")

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$idx, y = .data$scheme)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$hex),
      width = 0.96, height = 0.86, colour = "white", linewidth = 0.5
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL, title = "kkstatfun palettes")

  if (isTRUE(labels)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = .data$hex, colour = .data$txt),
      size = 2.6, fontface = "bold"
    ) +
      ggplot2::scale_colour_identity()
  }

  p +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    )
}
