# Example Usage of the kkstatfun palette system
#
#   set_plot_colors()  — register 1-12 anchor colours as the ggplot default
#   kk_gen_palettes()  — derive named colour-theory palettes from those seeds
#   kk_show_palettes() — preview the whole catalogue as labelled swatches
#   kk_pal()           — pull n colours from the registered anchors
#   scale_fill_kk()    — apply the palette to a single plot

library(kkstatfun)
library(ggplot2)

# ------------------------------------------------------------------
# 1. The basics: register anchor ("flag") colours once
# ------------------------------------------------------------------

# Anything from 1 to 12 colours is accepted.
set_plot_colors(c("#D62828", "#003049", "#F77F00"))

# Every discrete fill/colour scale now draws from the anchors automatically.
kkplot(mtcars, aes(factor(cyl), fill = factor(cyl))) +
              geom_bar() +
              labs(x = "Cylinders", fill = "cyl")

# kk_pal() pulls n colours: <= 3 uses the anchors directly, > 3 interpolates.
kk_pal(2)
kk_pal(6)

# ------------------------------------------------------------------
# 2. Generate a catalogue of palettes from a single seed
# ------------------------------------------------------------------

pals <- kk_gen_palettes("#D62828", n = 6)

names(pals)
pals$triadic
pals$sequential

# Preview every scheme as swatches (hex codes annotated).
kk_show_palettes(pals)

# Or generate and preview in one call.
kk_gen_palettes("#003049", n = 8, plot = TRUE)

# ------------------------------------------------------------------
# 3. Multi-seed input
# ------------------------------------------------------------------

# With more than one seed you also get a `custom` entry, and the
# `sequential` ramp is built across ALL the seeds rather than just one.
multi <- kk_gen_palettes(c("#D62828", "#003049", "#F77F00"), n = 8)

names(multi)      # note the extra "custom"
multi$custom
multi$sequential

# ------------------------------------------------------------------
# 4. Register a derived scheme as the plotting default
# ------------------------------------------------------------------

# `scheme =` expands the seed via kk_gen_palettes() and registers the result,
# so the derived palette drives every subsequent kkplot.
set_plot_colors("#D62828", scheme = "triadic", n = 6)
kk_pal(3)

kkplot(mtcars, aes(factor(cyl), fill = factor(cyl))) +
              geom_bar() +
              labs(x = "Cylinders", fill = "cyl")

# Ramps suit ordered quantities; qualitative schemes suit unordered categories.
set_plot_colors("#003049", scheme = "sequential", n = 6)
kkplot(mtcars, aes(factor(gear), fill = factor(gear))) +
              geom_bar() +
              labs(x = "Gears", fill = "gear")

# ------------------------------------------------------------------
# 5. Applying a palette to a single plot only
# ------------------------------------------------------------------

set_plot_colors(c("#D62828", "#003049", "#F77F00"))  # back to the flag anchors

# scale_fill_kk() takes NO palette argument — it reads the registered anchors
# and the number of levels from the data. A fill aesthetic must be mapped for
# the scale to have any effect.
kkplot(mtcars, aes(factor(cyl), fill = factor(cyl))) +
              geom_bar() +
              scale_fill_kk() +
              labs(x = "Cylinders", fill = "cyl")

# Continuous (gradient) counterpart, for heatmaps and density fills.
kkplot(faithfuld, aes(waiting, eruptions, fill = density)) +
              geom_raster() +
              scale_fill_kk_c()

# ------------------------------------------------------------------
# 6. Validation
# ------------------------------------------------------------------

# 1-12 colours are allowed; more than 12 is an error.
try(set_plot_colors(rep("#000000", 13)))

# Invalid colours are reported by name.
try(set_plot_colors(c("#D62828", "notacolour")))

# Unknown scheme names list the valid options.
try(set_plot_colors("#D62828", scheme = "nope"))
