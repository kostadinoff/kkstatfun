# Colour maths is checked against published constants and reference
# implementations, so a refactor cannot quietly change a published figure.

test_that("sRGB -> OKLab -> sRGB round-trips exactly", {
  cols <- c("#D62828", "#003049", "#F77F00", "#FFFFFF", "#000000", "steelblue")
  rt <- .kk_oklab_to_hex(.kk_lin_to_oklab(.kk_to_linear(cols)))
  expect_identical(rt, .kk_as_hex(cols))
})

test_that("OKLab matches Ottosson's reference values", {
  lab <- .kk_lin_to_oklab(.kk_to_linear(c("#FFFFFF", "#000000")))
  expect_equal(unname(lab[, 1]), c(1, 0, 0), tolerance = 1e-5)
  expect_equal(unname(lab[, 2]), c(0, 0, 0), tolerance = 1e-5)

  # Neutral greys are achromatic: a = b = 0, and lightness is monotone.
  greys <- kk_color_convert(c("#333333", "#777777", "#BBBBBB"))
  expect_equal(greys$ok_chroma, rep(0, 3), tolerance = 1e-6)
  expect_true(all(diff(greys$ok_l) > 0))

  # Hue angles land in the expected quadrants.
  hues <- kk_color_convert(c("#FF0000", "#00FF00", "#0000FF"))$ok_hue
  expect_true(hues[1] > 20 && hues[1] < 40)     # red
  expect_true(hues[2] > 130 && hues[2] < 150)   # green
  expect_true(hues[3] > 250 && hues[3] < 275)   # blue
})

test_that("CVD simulation reproduces Machado et al. (2009)", {
  cols <- c("#D62828", "#003049", "#F77F00", "steelblue")

  # Values verified against colorspace::deutan/protan/tritan (severity 1),
  # which implements the same published table.
  d <- kk_cvd(cols, type = "deutan")
  expect_identical(d$simulated, c("#8C7D1F", "#1A2A49", "#C0AA00", "#5D78B3"))

  p <- kk_cvd(cols, type = "protan")
  expect_identical(p$simulated, c("#615725", "#24304A", "#A38F00", "#6C83B6"))

  tr <- kk_cvd(cols, type = "tritan")
  expect_identical(tr$simulated, c("#EC002B", "#003639", "#FF636C", "#008E93"))

  # Interpolated severity (0.55) also matches colorspace.
  half <- kk_cvd(cols, type = "deutan", severity = 0.55)
  expect_identical(half$simulated, c("#A26B20", "#162C49", "#CF9E00", "#597CB4"))
})

test_that("severity 0 and achromatic vision behave sensibly", {
  cols <- c("#D62828", "#003049", "#F77F00")

  # No deficiency leaves the colours untouched.
  expect_identical(kk_cvd(cols, type = "deutan", severity = 0)$simulated, .kk_as_hex(cols))

  # Achromatopsia collapses to grey and preserves relative luminance.
  ach <- kk_cvd(cols, type = "achroma")$simulated
  rgb <- grDevices::col2rgb(ach)
  expect_true(all(rgb[1, ] == rgb[2, ] & rgb[2, ] == rgb[3, ]))
  # Equal up to 8-bit quantisation of the output hex.
  expect_equal(
    .kk_wcag_lum(.kk_to_linear(ach)),
    .kk_wcag_lum(.kk_to_linear(cols)),
    tolerance = 0.02
  )

  # White and black are fixed points of every transform.
  fixed <- kk_cvd(c("#FFFFFF", "#000000"), type = c("deutan", "protan", "tritan"))
  expect_true(all(fixed$simulated == fixed$hex))
})

test_that("kk_contrast follows WCAG 2.1", {
  expect_equal(kk_contrast("#FFFFFF", "#000000")$contrast, 21)
  expect_equal(kk_contrast("#777777", "#777777")$contrast, 1)

  # Ratio is symmetric in the two colours.
  expect_equal(
    kk_contrast("#D62828", "#FFFFFF")$contrast,
    kk_contrast("#FFFFFF", "#D62828")$contrast
  )

  out <- kk_contrast(c("#D62828", "#F77F00"), background = "#FFFFFF")
  expect_equal(nrow(out), 2)
  expect_equal(out$aa_text, out$contrast >= 4.5)
  expect_equal(out$graphic, out$contrast >= 3)

  # Every colour x background combination is returned.
  expect_equal(nrow(kk_contrast(c("#000000", "#FFFFFF"), c("#111111", "#EEEEEE"))), 4)
})

test_that("kk_pal_check finds the red/green trap", {
  chk <- kk_pal_check(c("#D62828", "#2A9D2A"))

  normal <- chk$min_dist[chk$vision == "normal"][1]
  deutan <- chk$min_dist[chk$vision == "deutan"][1]

  # The pair is far apart for normal vision but nearly identical for a
  # deuteranope -- the whole point of the audit.
  expect_gt(normal, 0.3)
  expect_lt(deutan, 0.05)
  expect_false(all(chk$distinct))

  # Distances are symmetric and each colour's nearest neighbour is the other.
  expect_equal(chk$nearest, rep(c("#2A9D2A", "#D62828"), 5))
})

test_that("kk_pal_check reports contrast against the background", {
  chk <- kk_pal_check(c("#D62828", "#F77F00", "#003049"), background = "#FFFFFF")
  normal <- chk[chk$vision == "normal", ]
  expect_equal(
    normal$contrast,
    kk_contrast(normal$color, "#FFFFFF")$contrast
  )
  # Amber on white is below the 3:1 graphical threshold.
  expect_false(normal$graphic[normal$color == "#F77F00"])
})

test_that("kk_pal_safe returns a deterministic palette that passes its own audit", {
  pal <- kk_pal_safe(5)
  expect_length(pal, 5)
  expect_true(all(grepl("^#[0-9A-F]{6}$", pal)))
  expect_identical(as.character(pal), as.character(kk_pal_safe(5)))

  audit <- kk_pal_check(pal, type = c("normal", "deutan", "protan", "tritan"))
  expect_true(all(audit$graphic))
  expect_true(all(audit$distinct))
  expect_equal(min(audit$min_dist), attr(pal, "min_dist"), tolerance = 1e-8)
})

test_that("kk_pal_safe honours seed colours and impossible constraints", {
  pal <- kk_pal_safe(4, seed_colors = "#003049")
  expect_length(pal, 4)
  expect_identical(pal[1], "#003049")

  pal2 <- kk_pal_safe(5, seed_colors = c("#D62828", "#003049"))
  expect_identical(pal2[1:2], c("#D62828", "#003049"))

  expect_error(kk_pal_safe(5, min_contrast = 21), "min_contrast")
})

test_that("input validation is strict", {
  expect_error(kk_color_convert("notacolour"), "Invalid colour")
  expect_error(kk_pal_check("#D62828"), "at least two")
  expect_error(kk_cvd("#D62828", severity = 2), "severity")
  expect_error(kk_cvd("#D62828", severity = -0.1), "severity")
  expect_error(
    kk_pal_check(c("#000000", "#FFFFFF"), background = c("#FFFFFF", "#000000")),
    "single colour"
  )
})

test_that("the default HSV palettes are unchanged", {
  # Regression guard: existing scripts and published figures must keep their
  # colours, so `space = 'hsv'` stays the default and its output is pinned.
  pals <- kk_gen_palettes("#D62828", n = 6)
  expect_identical(
    pals$triadic,
    c("#D62828", "#28D628", "#2828D6", "#ED8383", "#83ED83", "#8383ED")
  )
  expect_identical(
    pals$shades,
    c("#D62828", "#B32121", "#901A1A", "#6D1414", "#490D0D", "#270707")
  )
  expect_identical(attr(pals, "space"), "hsv")
})

test_that("the OKLCh schemes are valid and perceptually even", {
  ok <- kk_gen_palettes("#D62828", n = 6, space = "oklch")
  hsv <- kk_gen_palettes("#D62828", n = 6, space = "hsv")

  expect_named(ok, names(hsv))
  expect_true(all(vapply(ok, length, integer(1)) == 6))
  expect_true(all(grepl("^#[0-9A-F]{6}$", unlist(ok))))
  expect_identical(ok$tints[6], "#FFFFFF")

  # Hue rotation in OKLCh holds apparent lightness constant; HSV does not.
  l_ok <- kk_color_convert(ok$spectral)$ok_l
  l_hsv <- kk_color_convert(hsv$spectral)$ok_l
  expect_lt(stats::sd(l_ok), stats::sd(l_hsv) / 3)

  # A monochromatic ramp is monotone in lightness.
  expect_true(all(diff(kk_color_convert(ok$monochromatic)$ok_l) < 0))

  # Seeds are preserved and `custom` appears only with more than one seed.
  expect_identical(ok$triadic[1], "#D62828")
  expect_null(ok$custom)
  multi <- kk_gen_palettes(c("#D62828", "#003049"), n = 2, space = "oklch")
  expect_identical(multi$custom, c("#D62828", "#003049"))
})

test_that("edge cases in n are handled", {
  expect_length(kk_gen_palettes("#D62828", n = 1, space = "oklch")$monochromatic, 1)
  expect_length(kk_gen_palettes("#D62828", n = 2, space = "oklch")$triadic, 2)
  expect_length(
    kk_gen_palettes(c("#D62828", "#003049", "#F77F00"), n = 8, space = "oklch")$custom,
    8
  )
  expect_error(kk_gen_palettes("#D62828", n = 0), "positive integer")
})

test_that("preview plots build", {
  expect_s3_class(kk_show_cvd(c("#D62828", "#003049", "#F77F00")), "ggplot")
  expect_s3_class(
    kk_show_palettes(kk_gen_palettes("#D62828", n = 4, space = "oklch")),
    "ggplot"
  )
})
