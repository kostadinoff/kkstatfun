# ============================================================
# COLOUR SCIENCE
#   Perceptual colour space (OKLab / OKLCh), WCAG contrast, and
#   colour-vision-deficiency simulation for figure accessibility.
#
#   Sources for the constants used below:
#     - Ottosson, B. (2020). A perceptual color space for image
#       processing (OKLab). https://bottosson.github.io/posts/oklab/
#     - Machado, G. M., Oliveira, M. M. & Fernandes, L. A. F. (2009).
#       A physiologically-based model for simulation of color vision
#       deficiency. IEEE TVCG 15(6), 1291-1298. Table 1.
#     - W3C (2018). Web Content Accessibility Guidelines 2.1, SC 1.4.3
#       and 1.4.11.
#
#   Implemented with base grDevices only -- no new dependencies. The
#   CVD output is verified against colorspace::deutan()/protan()/
#   tritan() in tests/testthat/test-color-science.R.
# ============================================================

# ---- sRGB <-> linear-light ---------------------------------

.kk_srgb_to_linear <- function(x) {
  ifelse(x <= 0.04045, x / 12.92, ((x + 0.055) / 1.055)^2.4)
}

.kk_linear_to_srgb <- function(x) {
  x <- pmin(pmax(x, 0), 1)
  ifelse(x <= 0.0031308, 12.92 * x, 1.055 * x^(1 / 2.4) - 0.055)
}

# Any R colour spec -> "#RRGGBB" (upper case, alpha dropped).
.kk_as_hex <- function(colors) {
  rgb <- grDevices::col2rgb(colors)
  toupper(grDevices::rgb(rgb[1, ], rgb[2, ], rgb[3, ], maxColorValue = 255))
}

# Any R colour spec -> 3 x n matrix of linear-light RGB in [0, 1].
.kk_to_linear <- function(colors) {
  .kk_srgb_to_linear(grDevices::col2rgb(colors) / 255)
}

# 3 x n linear RGB -> 3 x n OKLab (rows L, a, b).
.kk_lin_to_oklab <- function(lin) {
  r <- lin[1, ]; g <- lin[2, ]; b <- lin[3, ]
  l <- (0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b)^(1 / 3)
  m <- (0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b)^(1 / 3)
  s <- (0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b)^(1 / 3)
  rbind(
    L = 0.2104542553 * l + 0.7936177850 * m - 0.0040720468 * s,
    a = 1.9779984951 * l - 2.4285922050 * m + 0.4505937099 * s,
    b = 0.0259040371 * l + 0.7827717662 * m - 0.8086757660 * s
  )
}

# 3 x n OKLab -> 3 x n linear RGB (unclamped, so gamut can be tested).
.kk_oklab_to_lin <- function(lab) {
  L <- lab[1, ]; a <- lab[2, ]; b <- lab[3, ]
  l <- (L + 0.3963377774 * a + 0.2158037573 * b)^3
  m <- (L - 0.1055613458 * a - 0.0638541728 * b)^3
  s <- (L - 0.0894841775 * a - 1.2914855480 * b)^3
  rbind(
    r =  4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s,
    g = -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s,
    b = -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s
  )
}

# 3 x n linear RGB -> hex (clamped).
.kk_lin_to_hex <- function(lin) {
  srgb <- round(.kk_linear_to_srgb(lin) * 255)
  toupper(grDevices::rgb(srgb[1, ], srgb[2, ], srgb[3, ], maxColorValue = 255))
}

.kk_oklab_to_hex <- function(lab) .kk_lin_to_hex(.kk_oklab_to_lin(lab))

# Is an OKLab colour inside the sRGB gamut?
.kk_in_gamut <- function(lab, tol = 1e-4) {
  lin <- .kk_oklab_to_lin(lab)
  apply(lin, 2, function(x) all(x >= -tol & x <= 1 + tol))
}

.kk_lch_to_lab <- function(L, C, h) {
  hr <- h * pi / 180
  rbind(L = L, a = C * cos(hr), b = C * sin(hr))
}

.kk_lab_to_lch <- function(lab) {
  rbind(
    L = lab[1, ],
    C = sqrt(lab[2, ]^2 + lab[3, ]^2),
    h = (atan2(lab[3, ], lab[2, ]) * 180 / pi) %% 360
  )
}

# OKLCh -> hex with chroma-reducing gamut mapping: when (L, C, h) falls
# outside sRGB the chroma is bisected down to the gamut boundary, which
# preserves hue and lightness instead of the hue shift caused by simply
# clipping the RGB channels.
.kk_lch_to_hex <- function(L, C, h, gamut_map = TRUE) {
  L <- pmin(pmax(L, 0), 1)
  C <- pmax(C, 0)
  h <- h %% 360
  if (!gamut_map) return(.kk_oklab_to_hex(.kk_lch_to_lab(L, C, h)))

  lo <- rep(0, length(C))
  hi <- C
  inside <- .kk_in_gamut(.kk_lch_to_lab(L, C, h))
  for (i in seq_len(12)) {
    mid <- (lo + hi) / 2
    ok <- .kk_in_gamut(.kk_lch_to_lab(L, mid, h))
    lo <- ifelse(ok, mid, lo)
    hi <- ifelse(ok, hi, mid)
  }
  .kk_oklab_to_hex(.kk_lch_to_lab(L, ifelse(inside, C, lo), h))
}

# WCAG 2.1 relative luminance from linear RGB.
.kk_wcag_lum <- function(lin) {
  as.numeric(0.2126 * lin[1, ] + 0.7152 * lin[2, ] + 0.0722 * lin[3, ])
}

# ---- colour-vision-deficiency transforms -------------------

# Machado, Oliveira & Fernandes (2009), Table 1: severity 0.0-1.0 in
# steps of 0.1, applied to linear-light RGB. Stored row-major.
.KK_CVD_MATRICES <- list(
  protan = list(
    "0" = c(1.000000, 0.000000, 0.000000, 0.000000, 1.000000, 0.000000, 0.000000, 0.000000, 1.000000),
    "1" = c(0.856167, 0.182038, -0.038205, 0.029342, 0.955115, 0.015544, -0.002880, -0.001563, 1.004443),
    "2" = c(0.734766, 0.334872, -0.069637, 0.051840, 0.919198, 0.028963, -0.004928, -0.004209, 1.009137),
    "3" = c(0.630323, 0.465641, -0.095964, 0.069181, 0.890046, 0.040773, -0.006308, -0.007724, 1.014032),
    "4" = c(0.539009, 0.579343, -0.118352, 0.082546, 0.866121, 0.051332, -0.007136, -0.011959, 1.019095),
    "5" = c(0.458064, 0.679578, -0.137642, 0.092785, 0.846313, 0.060902, -0.007494, -0.016807, 1.024301),
    "6" = c(0.385450, 0.769005, -0.154455, 0.100526, 0.829802, 0.069673, -0.007442, -0.022190, 1.029632),
    "7" = c(0.319627, 0.849633, -0.169261, 0.106241, 0.815969, 0.077790, -0.007025, -0.028051, 1.035076),
    "8" = c(0.259411, 0.923008, -0.182420, 0.110296, 0.804340, 0.085364, -0.006276, -0.034346, 1.040622),
    "9" = c(0.203876, 0.990338, -0.194214, 0.112975, 0.794542, 0.092483, -0.005222, -0.041043, 1.046265),
    "10" = c(0.152286, 1.052583, -0.204868, 0.114503, 0.786281, 0.099216, -0.003882, -0.048116, 1.051998)
  ),
  deutan = list(
    "0" = c(1.000000, 0.000000, 0.000000, 0.000000, 1.000000, 0.000000, 0.000000, 0.000000, 1.000000),
    "1" = c(0.866435, 0.177704, -0.044139, 0.049567, 0.939063, 0.011370, -0.003453, 0.007233, 0.996220),
    "2" = c(0.760729, 0.319078, -0.079807, 0.090568, 0.889315, 0.020117, -0.006027, 0.013325, 0.992702),
    "3" = c(0.675425, 0.433850, -0.109275, 0.125303, 0.847755, 0.026942, -0.007950, 0.018572, 0.989378),
    "4" = c(0.605511, 0.528560, -0.134071, 0.155318, 0.812366, 0.032316, -0.009376, 0.023176, 0.986200),
    "5" = c(0.547494, 0.607765, -0.155259, 0.181692, 0.781742, 0.036566, -0.010410, 0.027275, 0.983136),
    "6" = c(0.498864, 0.674741, -0.173604, 0.205199, 0.754872, 0.039929, -0.011131, 0.030969, 0.980162),
    "7" = c(0.457771, 0.731899, -0.189670, 0.226409, 0.731012, 0.042579, -0.011595, 0.034333, 0.977261),
    "8" = c(0.422823, 0.781057, -0.203881, 0.245752, 0.709602, 0.044646, -0.011843, 0.037423, 0.974421),
    "9" = c(0.392952, 0.823610, -0.216562, 0.263559, 0.690210, 0.046232, -0.011910, 0.040281, 0.971630),
    "10" = c(0.367322, 0.860646, -0.227968, 0.280085, 0.672501, 0.047413, -0.011820, 0.042940, 0.968881)
  ),
  tritan = list(
    "0" = c(1.000000, 0.000000, 0.000000, 0.000000, 1.000000, 0.000000, 0.000000, 0.000000, 1.000000),
    "1" = c(0.926670, 0.092514, -0.019184, 0.021191, 0.964503, 0.014306, 0.008437, 0.054813, 0.936750),
    "2" = c(0.895720, 0.133330, -0.029050, 0.029997, 0.945400, 0.024603, 0.013027, 0.104707, 0.882266),
    "3" = c(0.905871, 0.127791, -0.033662, 0.026856, 0.941251, 0.031893, 0.013410, 0.148296, 0.838294),
    "4" = c(0.948035, 0.089490, -0.037526, 0.014364, 0.946792, 0.038844, 0.010853, 0.193991, 0.795156),
    "5" = c(1.017277, 0.027029, -0.044306, -0.006113, 0.958479, 0.047634, 0.006379, 0.248708, 0.744913),
    "6" = c(1.104996, -0.046633, -0.058363, -0.032137, 0.971635, 0.060503, 0.001336, 0.317922, 0.680742),
    "7" = c(1.193214, -0.109812, -0.083402, -0.058496, 0.979410, 0.079086, -0.002346, 0.403492, 0.598854),
    "8" = c(1.257728, -0.139648, -0.118081, -0.078003, 0.975409, 0.102594, -0.003316, 0.501214, 0.502102),
    "9" = c(1.278864, -0.125333, -0.153531, -0.084748, 0.957674, 0.127074, -0.000989, 0.601151, 0.399838),
    "10" = c(1.255528, -0.076749, -0.178779, -0.078411, 0.930809, 0.147602, 0.004733, 0.691367, 0.303900)
  )
)

.KK_VISION_TYPES <- c("normal", "deutan", "protan", "tritan", "achroma")

.KK_VISION_LABELS <- c(
  normal  = "Normal trichromacy",
  deutan  = "Deuteranopia (green-weak, ~6% of men)",
  protan  = "Protanopia (red-weak, ~2% of men)",
  tritan  = "Tritanopia (blue-yellow, rare)",
  achroma = "Achromatopsia (greyscale)"
)

# Interpolate the Machado table to an arbitrary severity in [0, 1].
.kk_cvd_matrix <- function(type, severity) {
  tab <- .KK_CVD_MATRICES[[type]]
  s <- severity * 10
  lo <- floor(s)
  hi <- ceiling(s)
  v <- if (lo == hi) {
    tab[[as.character(lo)]]
  } else {
    tab[[as.character(lo)]] * (hi - s) + tab[[as.character(hi)]] * (s - lo)
  }
  matrix(v, nrow = 3, ncol = 3, byrow = TRUE)
}

# 3 x n linear RGB -> 3 x n linear RGB as seen with `vision`.
.kk_apply_vision <- function(lin, vision, severity = 1) {
  if (vision == "normal" || severity == 0) return(lin)
  if (vision == "achroma") {
    y <- .kk_wcag_lum(lin)
    grey <- rbind(r = y, g = y, b = y)
    return((1 - severity) * lin + severity * grey)
  }
  .kk_cvd_matrix(vision, severity) %*% lin
}

.kk_check_severity <- function(severity) {
  if (!is.numeric(severity) || length(severity) != 1 || is.na(severity) ||
      severity < 0 || severity > 1) {
    stop("`severity` must be a single number between 0 and 1.")
  }
  severity
}

# ============================================================
# EXPORTED: coordinates
# ============================================================

#' Colour Coordinates in Perceptual Space
#'
#' @description Converts colours to the **OKLab / OKLCh** perceptual colour
#'   space and reports WCAG relative luminance alongside the sRGB values. OKLab
#'   is perceptually uniform, so Euclidean distance in it approximates how
#'   different two colours *look* -- unlike HSV or RGB distance. This underpins
#'   [kk_pal_check()] and the `space = "oklch"` mode of [kk_gen_palettes()].
#'
#' @param colors Character vector of colours (hex codes or R colour names).
#'
#' @return A tibble with one row per colour: `color`, `hex`, `red`, `green`,
#'   `blue` (0-255), `ok_l` (lightness, 0-1), `ok_a`, `ok_b`, `ok_chroma`,
#'   `ok_hue` (degrees), and `luminance` (WCAG relative luminance, 0-1).
#'
#' @references Ottosson, B. (2020). *A perceptual color space for image
#'   processing* (OKLab).
#'
#' @examples
#' kk_color_convert(c("#D62828", "#003049", "#F77F00"))
#'
#' @seealso [kk_pal_check()], [kk_contrast()], [kk_cvd()].
#' @export
kk_color_convert <- function(colors) {
  colors <- .kk_validate_colors(colors, max_n = Inf)
  lin <- .kk_to_linear(colors)
  lab <- .kk_lin_to_oklab(lin)
  lch <- .kk_lab_to_lch(lab)
  rgb <- grDevices::col2rgb(colors)

  tibble::tibble(
    color = colors,
    hex = .kk_as_hex(colors),
    red = rgb[1, ], green = rgb[2, ], blue = rgb[3, ],
    ok_l = lab[1, ], ok_a = lab[2, ], ok_b = lab[3, ],
    ok_chroma = lch[2, ], ok_hue = lch[3, ],
    luminance = .kk_wcag_lum(lin)
  )
}

# ============================================================
# EXPORTED: colour-vision-deficiency simulation
# ============================================================

#' Simulate Colour Vision Deficiency
#'
#' @description Renders colours as they are seen with the common forms of
#'   colour blindness, using the physiologically-derived transforms of Machado,
#'   Oliveira & Fernandes (2009) applied in linear-light RGB. Roughly 8% of men
#'   and 0.5% of women have a red-green deficiency, so a figure whose groups are
#'   separated by colour alone should be checked before submission.
#'
#' @param colors Character vector of colours (hex codes or R colour names).
#' @param type Vision types to simulate: any of `"deutan"`, `"protan"`,
#'   `"tritan"`, `"achroma"` and `"normal"` (default: the first four).
#' @param severity Severity of the deficiency, 0 (normal vision) to 1 (full
#'   dichromacy, the default). Intermediate values are interpolated within the
#'   published table and correspond to *anomalous trichromacy*.
#'
#' @return A tibble with one row per colour x type: `color`, `hex`, `vision`,
#'   `severity`, `simulated` (hex as seen).
#'
#' @references Machado, G. M., Oliveira, M. M. & Fernandes, L. A. F. (2009).
#'   A physiologically-based model for simulation of color vision deficiency.
#'   *IEEE Transactions on Visualization and Computer Graphics*, 15(6),
#'   1291-1298.
#'
#' @examples
#' kk_cvd(c("#D62828", "#003049", "#F77F00"))
#' kk_cvd("#D62828", type = "deutan", severity = 0.5)
#'
#' @seealso [kk_show_cvd()] to preview, [kk_pal_check()] to test a whole
#'   palette, [kk_pal_safe()] to build one that passes.
#' @export
kk_cvd <- function(colors,
                   type = c("deutan", "protan", "tritan", "achroma"),
                   severity = 1) {
  colors <- .kk_validate_colors(colors, max_n = Inf)
  type <- match.arg(type, choices = .KK_VISION_TYPES, several.ok = TRUE)
  .kk_check_severity(severity)

  hex <- .kk_as_hex(colors)
  lin <- .kk_to_linear(colors)

  out <- lapply(type, function(v) {
    sim <- .kk_lin_to_hex(.kk_apply_vision(lin, v, severity))
    sev <- if (v == "normal") 0 else severity
    tibble::tibble(
      color = colors,
      hex = hex,
      vision = v,
      severity = sev,
      simulated = sim
    )
  })
  out <- do.call(rbind, out)
  out$vision <- factor(out$vision, levels = .KK_VISION_TYPES[.KK_VISION_TYPES %in% type])
  out
}

#' Preview a Palette Under Colour Vision Deficiency
#'
#' @description Draws a palette as a grid of swatches -- one row per vision type
#'   -- so that colours which collapse into one another under red-green
#'   deficiency are immediately visible. The companion plot to [kk_pal_check()].
#'
#' @param colors Character vector of colours (hex codes or R colour names).
#' @param type Vision types to show (default: all five, normal first).
#' @param severity Severity of the deficiency, 0-1 (default 1).
#' @param labels Whether to print the hex code inside each swatch (default TRUE).
#'
#' @return A ggplot object.
#'
#' @examples
#' kk_show_cvd(c("#D62828", "#003049", "#F77F00"))
#'
#' @seealso [kk_cvd()], [kk_pal_check()], [kk_show_palettes()].
#' @export
kk_show_cvd <- function(colors, type = .KK_VISION_TYPES, severity = 1,
                        labels = TRUE) {
  type <- match.arg(type, choices = .KK_VISION_TYPES, several.ok = TRUE)
  df <- kk_cvd(colors, type = type, severity = severity)
  df$idx <- rep(seq_len(nrow(df) / length(type)), times = length(type))
  df$vision <- factor(
    .KK_VISION_LABELS[as.character(df$vision)],
    levels = rev(.KK_VISION_LABELS[levels(df$vision)])
  )
  lum <- .kk_wcag_lum(.kk_to_linear(df$simulated))
  df$txt <- ifelse(lum > 0.30, "#000000", "#FFFFFF")

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$idx, y = .data$vision)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$simulated),
      width = 0.96, height = 0.86, colour = "white", linewidth = 0.5
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL, title = "Palette under colour vision deficiency")

  if (isTRUE(labels)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = .data$simulated, colour = .data$txt),
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

# ============================================================
# EXPORTED: WCAG contrast
# ============================================================

#' WCAG Contrast Ratio
#'
#' @description Contrast ratio of each colour against one or more backgrounds,
#'   with the WCAG 2.1 conformance flags. Use it to check that figure text,
#'   annotations and thin lines remain legible -- journals and university
#'   accessibility policies increasingly require it.
#'
#'   Thresholds: 4.5:1 for body text (AA), 3:1 for large text and for graphical
#'   objects such as lines, points and swatch borders, 7:1 for AAA text.
#'
#' @param colors Character vector of foreground colours.
#' @param background Character vector of background colours (default white and
#'   black). Every colour x background combination is returned.
#'
#' @return A tibble with `color`, `background`, `luminance`, `contrast`, and
#'   logical flags `aa_text` (>= 4.5), `aa_large` (>= 3), `aaa_text` (>= 7) and
#'   `graphic` (>= 3, the non-text threshold).
#'
#' @references W3C (2018). *Web Content Accessibility Guidelines (WCAG) 2.1*,
#'   success criteria 1.4.3 and 1.4.11.
#'
#' @examples
#' kk_contrast(c("#D62828", "#003049", "#F77F00"))
#' kk_contrast("#F77F00", background = "#FFFFFF")
#'
#' @seealso [kk_pal_check()].
#' @export
kk_contrast <- function(colors, background = c("#FFFFFF", "#000000")) {
  colors <- .kk_validate_colors(colors, max_n = Inf)
  background <- .kk_validate_colors(background, max_n = Inf)

  lum_fg <- .kk_wcag_lum(.kk_to_linear(colors))
  lum_bg <- .kk_wcag_lum(.kk_to_linear(background))

  grid <- expand.grid(
    i = seq_along(colors), j = seq_along(background),
    KEEP.OUT.ATTRS = FALSE
  )
  l1 <- pmax(lum_fg[grid$i], lum_bg[grid$j])
  l2 <- pmin(lum_fg[grid$i], lum_bg[grid$j])
  ratio <- (l1 + 0.05) / (l2 + 0.05)

  tibble::tibble(
    color = colors[grid$i],
    background = background[grid$j],
    luminance = lum_fg[grid$i],
    contrast = ratio,
    aa_text = ratio >= 4.5,
    aa_large = ratio >= 3,
    aaa_text = ratio >= 7,
    graphic = ratio >= 3
  )
}

# ============================================================
# EXPORTED: palette accessibility audit
# ============================================================

#' Audit a Palette for Figure Accessibility
#'
#' @description Checks whether the categories in a palette stay distinguishable
#'   for readers with colour vision deficiency, and whether each colour has
#'   enough contrast against the plotting background. For every colour and every
#'   vision type it reports the perceptual distance (Euclidean distance in
#'   OKLab, "delta-E OK") to the *nearest other colour in the palette* as that
#'   reader would see it.
#'
#'   A pair separated by less than `min_dist` will read as the same colour: fix
#'   it by changing the palette (see [kk_pal_safe()]) or by adding a redundant
#'   non-colour cue -- linetype, point shape or direct labels.
#'
#' @param colors Character vector of palette colours.
#' @param background Single background colour the figure is drawn on
#'   (default `"#FFFFFF"`).
#' @param min_dist Minimum acceptable OKLab distance between two categories
#'   (default 0.10). Values near 0.02 are at the just-noticeable-difference
#'   level; 0.10 is a practical floor for categorical encoding in print.
#' @param type Vision types to audit (default: all five).
#' @param severity Severity of the deficiency, 0-1 (default 1, full dichromacy).
#' @param plot If TRUE, also draw the swatches via [kk_show_cvd()]
#'   (default FALSE).
#'
#' @return A tibble with one row per colour x vision type: `color`, `vision`,
#'   `simulated`, `contrast` (against `background`, as seen), `graphic` (TRUE
#'   when contrast >= 3), `nearest` (the palette colour it is closest to),
#'   `min_dist` and `distinct` (TRUE when `min_dist >= min_dist`).
#'
#' @details Summarise the audit with, for example,
#'   `dplyr::filter(out, !distinct | !graphic)` -- an empty result means the
#'   palette passes.
#'
#' @examples
#' # A red/green pair that collapses for deuteranopes
#' kk_pal_check(c("#D62828", "#2A9D2A"))
#'
#' # The default flag palette
#' kk_pal_check(c("#D62828", "#003049", "#F77F00"))
#'
#' @seealso [kk_pal_safe()], [kk_show_cvd()], [kk_contrast()].
#' @export
kk_pal_check <- function(colors, background = "#FFFFFF", min_dist = 0.10,
                         type = .KK_VISION_TYPES, severity = 1, plot = FALSE) {
  colors <- .kk_validate_colors(colors, max_n = Inf)
  if (length(colors) < 2) {
    stop("`colors` must contain at least two colours to compare.")
  }
  background <- .kk_validate_colors(background, max_n = Inf)
  if (length(background) != 1) stop("`background` must be a single colour.")
  type <- match.arg(type, choices = .KK_VISION_TYPES, several.ok = TRUE)
  .kk_check_severity(severity)
  if (!is.numeric(min_dist) || length(min_dist) != 1 || is.na(min_dist) || min_dist < 0) {
    stop("`min_dist` must be a single non-negative number.")
  }

  lin <- .kk_to_linear(colors)
  lin_bg <- .kk_to_linear(background)
  n <- length(colors)

  out <- lapply(type, function(v) {
    sim <- .kk_apply_vision(lin, v, severity)
    lab <- .kk_lin_to_oklab(pmin(pmax(sim, 0), 1))
    d <- as.matrix(stats::dist(t(lab)))
    diag(d) <- Inf
    near <- apply(d, 1, which.min)
    dmin <- apply(d, 1, min)

    lum_fg <- .kk_wcag_lum(pmin(pmax(sim, 0), 1))
    lum_bg <- .kk_wcag_lum(pmin(pmax(.kk_apply_vision(lin_bg, v, severity), 0), 1))
    ratio <- (pmax(lum_fg, lum_bg) + 0.05) / (pmin(lum_fg, lum_bg) + 0.05)

    # Computed up front: inside tibble() the name `min_dist` would resolve to
    # the column being built rather than to the argument.
    ok_dist <- dmin >= min_dist
    ok_contrast <- ratio >= 3

    tibble::tibble(
      color = colors,
      vision = v,
      simulated = .kk_lin_to_hex(sim),
      contrast = ratio,
      graphic = ok_contrast,
      nearest = colors[near],
      min_dist = dmin,
      distinct = ok_dist
    )
  })
  out <- do.call(rbind, out)
  out$vision <- factor(out$vision, levels = .KK_VISION_TYPES[.KK_VISION_TYPES %in% type])

  if (isTRUE(plot)) {
    print(kk_show_cvd(colors, type = type, severity = severity))
  }
  out
}

# ============================================================
# EXPORTED: generate a colour-blind-safe palette
# ============================================================

#' Build a Colour-Blind-Safe Qualitative Palette
#'
#' @description Constructs `n` categorical colours that stay far apart in OKLab
#'   space *simultaneously* for normal, deuteranopic, protanopic and tritanopic
#'   vision, and that clear the WCAG non-text contrast threshold against the
#'   plotting background. Colours are chosen by deterministic farthest-point
#'   sampling over a gamut-mapped OKLCh grid, so the same call always returns
#'   the same palette -- important for reproducible figures.
#'
#'   Unlike picking from a fixed colour-blind-safe set, this lets you anchor the
#'   palette on your own brand or institutional colours via `seed_colors` and
#'   fill the remaining slots around them.
#'
#' @param n Number of colours required.
#' @param seed_colors Optional colours to keep at the start of the palette (for
#'   example an institutional colour). They are used as-is and are not checked.
#' @param background Background colour the figure is drawn on
#'   (default `"#FFFFFF"`).
#' @param lightness Length-2 range of OKLab lightness to sample from
#'   (default `c(0.45, 0.80)`).
#' @param chroma Length-2 range of OKLCh chroma (default `c(0.06, 0.20)`).
#' @param min_contrast Minimum WCAG contrast against `background`
#'   (default 3, the non-text threshold).
#' @param type Vision types the palette must satisfy simultaneously
#'   (default deutan, protan, tritan plus normal vision).
#' @param severity Severity of the deficiency, 0-1 (default 1).
#'
#' @return Character vector of `n` hex colours, with the achieved minimum
#'   pairwise OKLab distance in `attr(x, "min_dist")`.
#'
#' @details `"achroma"` is deliberately **not** in the default `type`. Adding it
#'   forces the categories to separate on lightness alone, which in practice
#'   caps a usable palette at three or four colours -- and those then also have
#'   to clear `min_contrast` against the background. If the figure must survive
#'   photocopying, either ask for fewer categories with
#'   `type = c("normal", "deutan", "protan", "tritan", "achroma")`, or keep a
#'   larger colour palette and add a redundant non-colour cue (linetype, point
#'   shape, direct labels). Check the result either way with [kk_pal_check()].
#'
#' @examples
#' pal <- kk_pal_safe(5)
#' pal
#' kk_pal_check(pal)
#'
#' # anchor on an institutional colour
#' kk_pal_safe(4, seed_colors = "#003049")
#'
#' @seealso [kk_pal_check()], [kk_show_cvd()], [set_plot_colors()].
#' @export
kk_pal_safe <- function(n, seed_colors = NULL, background = "#FFFFFF",
                        lightness = c(0.45, 0.80), chroma = c(0.06, 0.20),
                        min_contrast = 3,
                        type = c("normal", "deutan", "protan", "tritan"),
                        severity = 1) {
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1) {
    stop("`n` must be a single positive integer.")
  }
  n <- as.integer(n)
  type <- match.arg(type, choices = .KK_VISION_TYPES, several.ok = TRUE)
  .kk_check_severity(severity)
  if (!is.null(seed_colors)) seed_colors <- .kk_validate_colors(seed_colors, max_n = Inf)
  background <- .kk_validate_colors(background, max_n = Inf)
  if (length(background) != 1) stop("`background` must be a single colour.")

  # Candidate grid in OKLCh, gamut-mapped to sRGB.
  grid <- expand.grid(
    L = seq(lightness[1], lightness[2], length.out = 8),
    C = seq(chroma[1], chroma[2], length.out = 5),
    h = seq(0, 360, by = 4)[-91],
    KEEP.OUT.ATTRS = FALSE
  )
  cand <- .kk_lch_to_hex(grid$L, grid$C, grid$h)
  cand <- unique(cand)

  # Keep only candidates with enough contrast against the background *for
  # every* vision type -- a colour can lose contrast once it is simulated.
  lin_bg <- .kk_to_linear(background)
  lin_c <- .kk_to_linear(cand)
  ratio <- do.call(pmin, lapply(type, function(v) {
    lum_c <- .kk_wcag_lum(pmin(pmax(.kk_apply_vision(lin_c, v, severity), 0), 1))
    lum_bg <- .kk_wcag_lum(pmin(pmax(.kk_apply_vision(lin_bg, v, severity), 0), 1))
    (pmax(lum_c, lum_bg) + 0.05) / (pmin(lum_c, lum_bg) + 0.05)
  }))
  keep <- ratio >= min_contrast
  if (sum(keep) < n) {
    stop(sprintf(
      "Only %d candidate colours meet min_contrast = %s; widen `lightness`/`chroma` or lower `min_contrast`.",
      sum(keep), format(min_contrast)
    ))
  }
  cand <- cand[keep]
  lin_c <- lin_c[, keep, drop = FALSE]

  # OKLab coordinates of every candidate under every vision type.
  labs <- lapply(type, function(v) {
    .kk_lin_to_oklab(pmin(pmax(.kk_apply_vision(lin_c, v, severity), 0), 1))
  })

  # Distance between candidate `i` and every candidate = worst case across
  # vision types, so a pair must be separated for *all* readers.
  dist_to <- function(i) {
    d <- lapply(labs, function(m) sqrt(colSums((m - m[, i])^2)))
    do.call(pmin, d)
  }

  chosen <- integer(0)
  if (!is.null(seed_colors)) {
    lin_s <- .kk_to_linear(seed_colors)
    labs_s <- lapply(type, function(v) {
      .kk_lin_to_oklab(pmin(pmax(.kk_apply_vision(lin_s, v, severity), 0), 1))
    })
    best <- Reduce(pmin, Map(function(m, s) {
      apply(s, 2, function(p) sqrt(colSums((m - p)^2)))
    }, labs, labs_s))
    if (is.null(dim(best))) best <- matrix(best, ncol = length(seed_colors))
    dmin <- apply(best, 1, min)
    fixed <- seed_colors
  } else {
    # Deterministic start: the candidate furthest from the background.
    lab_bg <- .kk_lin_to_oklab(.kk_to_linear(background))
    start <- which.max(sqrt(colSums((labs[[1]] - lab_bg[, 1])^2)))
    chosen <- start
    dmin <- dist_to(start)
    dmin[start] <- -Inf
    fixed <- character(0)
  }

  n_extra <- n - length(fixed)
  while (length(chosen) < n_extra) {
    nxt <- which.max(dmin)
    chosen <- c(chosen, nxt)
    dmin <- pmin(dmin, dist_to(nxt))
    dmin[nxt] <- -Inf
  }

  out <- c(if (length(fixed)) .kk_as_hex(fixed) else character(0),
           cand[chosen])[seq_len(n)]
  attr(out, "min_dist") <- {
    lin_o <- .kk_to_linear(out)
    worst <- vapply(type, function(v) {
      lab <- .kk_lin_to_oklab(pmin(pmax(.kk_apply_vision(lin_o, v, severity), 0), 1))
      d <- as.matrix(stats::dist(t(lab)))
      diag(d) <- Inf
      min(d)
    }, numeric(1))
    if (length(out) < 2) NA_real_ else min(worst)
  }
  out
}
