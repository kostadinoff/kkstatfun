# ============================================================
# VISUALIZATION FUNCTIONS
# ============================================================

#' Set Plot Font with Comprehensive Search
#'
#' @description Advanced font loading system that searches Google Fonts, system fonts,
#'   and local directories. Automatically falls back to safe alternatives if needed.
#'
#' @param font Font family name (default: "Roboto Condensed")
#' @param size Base font size in points (default: 18)
#' @param search_sources Order to search: "google", "system", "local"
#' @param fallbacks Fallback fonts if primary not found
#' @param update_theme Apply theme to all subsequent plots
#'
#' @return List with font configuration (invisibly)
#'
#' @export
set_plot_font <- function(font = "Roboto Condensed", size = 18,
                          search_sources = c("google", "system", "local"),
                          fallbacks = c("Arial", "Helvetica", "sans"),
                          update_theme = TRUE) {
              # Input validation
              if (!is.character(font) || length(font) != 1) {
                            stop("Font must be a single character string.")
              }

              # Initialize tracking variables
              font_family <- font
              font_loaded <- FALSE
              search_results <- list()

              # Helper function to test if font family is available
              test_font <- function(family) {
                            families <- sysfonts::font_families()
                            return(family %in% families)
              }

              # Helper function to add font from Google Fonts
              add_google_font <- function(family) {
                            tryCatch(
                                          {
                                                        sysfonts::font_add_google(name = family, family = family, db_cache = FALSE)
                                                        return(test_font(family))
                                          },
                                          error = function(e) {
                                                        message(" ✗ Google Fonts: ", e$message)
                                                        return(FALSE)
                                          }
                            )
              }

              # Helper function to check system fonts
              check_system_font <- function(family) {
                            return(test_font(family))
              }

              # Helper function to search local font directories
              search_local_fonts <- function(family) {
                            # Common font directories
                            font_dirs <- c(
                                          file.path(Sys.getenv("WINDIR"), "Fonts"), # Windows
                                          file.path(Sys.getenv("HOME"), ".fonts"), # User fonts
                                          "/System/Library/Fonts", # macOS
                                          "/Library/Fonts", # macOS
                                          "/usr/share/fonts", # Linux
                                          "/usr/local/share/fonts" # Linux
                            )

                            # Remove duplicates and non-existent dirs
                            font_dirs <- unique(font_dirs[dir.exists(font_dirs)])

                            # Common font file extensions
                            extensions <- c("ttf", "otf", "ttc")
                            pattern <- paste0("(?i)", family, ".*\\.(", paste(extensions, collapse = "|"), ")$")

                            # Search for font files
                            for (dir in font_dirs) {
                                          font_files <- list.files(dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)

                                          if (length(font_files) > 0) {
                                                        # Try to add the first matching font file
                                                        tryCatch(
                                                                      {
                                                                                    sysfonts::font_add(family = family, regular = font_files[1])
                                                                                    if (test_font(family)) {
                                                                                                  message(" ✓ Added '", family, "' from: ", basename(font_files[1]))
                                                                                                  return(TRUE)
                                                                                    }
                                                                      },
                                                                      error = function(e) {
                                                                                    message(" ✗ Failed to add font from ", dir, ": ", e$message)
                                                                      }
                                                        )
                                          }
                            }

                            return(FALSE)
              }

              # Main search process
              cat("🔍 Searching for font:", font, "\n")

              # Search in specified order
              for (source in search_sources) {
                            if (font_loaded) break

                            cat(" Checking", source, "source...\n")

                            result <- switch(source,
                                          "google" = {
                                                        search_results[[source]] <- add_google_font(font)
                                                        search_results[[source]]
                                          },
                                          "system" = {
                                                        search_results[[source]] <- check_system_font(font)
                                                        search_results[[source]]
                                          },
                                          "local" = {
                                                        search_results[[source]] <- search_local_fonts(font)
                                                        search_results[[source]]
                                          },
                                          FALSE
                            )

                            if (result) {
                                          font_loaded <- TRUE
                                          message(" ✓ Found in ", source, " source!")
                                          break
                            }
              }

              # Try fallback fonts if original not found
              if (!font_loaded) {
                            cat(" No match found. Trying fallbacks...\n")
                            for (fb in fallbacks) {
                                          cat(" Testing fallback:", fb, "\n")
                                          if (check_system_font(fb)) {
                                                        font_family <- fb
                                                        font_loaded <- TRUE
                                                        message(" ✓ Using fallback: ", fb)
                                                        break
                                          }
                            }
              }

              # Final fallback to system default
              if (!font_loaded) {
                            font_family <- "sans"
                            message(" ⚠ Using system default 'sans'")
              }


              # Create and set theme if requested
              if (update_theme) {
                            # Define relative font sizes based on the `size` parameter
                            title_size <- size + 4
                            subtitle_size <- size + 2
                            caption_size <- size - 2
                            axis_title_size <- size
                            axis_text_size <- size
                            strip_text_size <- size

                            theme_nice <- ggthemes::theme_tufte() +
                                          ggplot2::theme(
                                                        axis.ticks = ggplot2::element_line(linewidth = 0.5, color = "black"),
                                                        axis.ticks.length = ggplot2::unit(4, "mm"),
                                                        plot.title = ggplot2::element_text(family = font_family, size = title_size, hjust = 0, vjust = 2, margin = ggplot2::margin(t = 10, b = 10)),
                                                        plot.subtitle = ggplot2::element_text(family = font_family, size = subtitle_size),
                                                        plot.caption = ggplot2::element_text(family = font_family, hjust = 0.5, vjust = 1, size = caption_size),
                                                        plot.caption.position = "plot",
                                                        axis.title = ggplot2::element_text(family = font_family, size = axis_title_size),
                                                        axis.text = ggplot2::element_text(family = font_family, size = axis_text_size),
                                                        axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
                                                        strip.text = ggplot2::element_text(family = font_family, size = strip_text_size),
                                                        axis.line = ggplot2::element_line()
                                          )

                            ggplot2::theme_set(theme_nice)
                            message("✓ Updated ggplot2 theme with font '", font_family, "'")
              }

              # Return comprehensive results
              result <- list(
                            requested = font,
                            used = font_family,
                            loaded = font_loaded,
                            source = if (font_family == font) "original" else "fallback",
                            search_sources = search_sources,
                            search_results = search_results,
                            size = size,
                            theme_updated = update_theme
              )

              message("✅ Font setup complete. Using: ", font_family)
              return(invisible(result))
}

#' List Available System Fonts
#'
#' @description Shows all fonts available on your system
#'
#' @return Character vector of font names
#'
#' @export
list_system_fonts <- function() {
              fonts <- sysfonts::font_families()
              message(sprintf(
                            "Available fonts (%d total):\n%s\n",
                            length(fonts),
                            paste(fonts, collapse = ", ")
              ))
              invisible(fonts)
}

#' Load Google Font
#'
#' @description Downloads and installs a Google Font
#'
#' @param font_name Font name from Google Fonts
#' @param family Family name to use in R
#'
#' @return Logical - TRUE if successful
#'
#' @export
load_google_font <- function(font_name, family = NULL) {
              if (is.null(family)) {
                            family <- tolower(gsub(" ", "_", font_name))
              }

              if (!requireNamespace("sysfonts", quietly = TRUE)) {
                            stop("Package 'sysfonts' required")
              }

              message(sprintf("Downloading '%s' from Google Fonts...", font_name))

              result <- tryCatch(
                            {
                                          sysfonts::font_add_google(name = font_name, family = family, db_cache = FALSE)
                                          message(sprintf("✓ Loaded '%s' as '%s'", font_name, family))
                                          TRUE
                            },
                            error = function(e) {
                                          message(sprintf("✗ Failed to load: %s", e$message))
                                          FALSE
                            }
              )

              invisible(result)
}

#' Load Local Font File
#'
#' @description Loads a font from a local file path
#'
#' @param font_path Full path to font file (.ttf, .otf)
#' @param family Family name to use in R
#'
#' @return Logical - TRUE if successful
#'
#' @export
load_local_font <- function(font_path, family) {
              if (!file.exists(font_path)) {
                            stop(sprintf("Font file not found: %s", font_path))
              }

              message(sprintf("Loading font from: %s", font_path))

              result <- tryCatch(
                            {
                                          sysfonts::font_add(family = family, regular = font_path)
                                          message(sprintf("✓ Loaded local font as '%s'", family))
                                          TRUE
                            },
                            error = function(e) {
                                          message(sprintf("✗ Failed to load: %s", e$message))
                                          FALSE
                            }
              )

              invisible(result)
}

#' Search and Load Font (Google, System, or Local)
#'
#' @description Smart font loader that searches multiple sources
#'
#' @param font_name Font name to search for
#' @param search_sources Order to search: "google", "system", "local"
#' @param local_dir Directory to search for local fonts
#'
#' @return Logical - TRUE if loaded successfully
#'
#' @export
load_font_smart <- function(font_name,
                            search_sources = c("google", "system", "local"),
                            local_dir = c(
                                          file.path(Sys.getenv("HOME"), ".fonts"),
                                          file.path(Sys.getenv("WINDIR"), "Fonts"),
                                          "/System/Library/Fonts",
                                          "/Library/Fonts",
                                          "/usr/share/fonts"
                            )) {
              available_fonts <- sysfonts::font_families()

              # Check if already loaded
              if (font_name %in% available_fonts) {
                            message(sprintf("✓ Font '%s' already available", font_name))
                            return(invisible(TRUE))
              }

              family_name <- tolower(gsub(" ", "_", font_name))

              for (source in search_sources) {
                            message(sprintf("Searching %s...", source))

                            if (source == "google") {
                                          if (load_google_font(font_name, family_name)) {
                                                        return(invisible(TRUE))
                                          }
                            } else if (source == "system") {
                                          if (font_name %in% available_fonts) {
                                                        message(sprintf("✓ Found '%s' in system fonts", font_name))
                                                        return(invisible(TRUE))
                                          }
                            } else if (source == "local") {
                                          # Search local directories
                                          for (dir in local_dir) {
                                                        if (!dir.exists(dir)) next

                                                        font_files <- list.files(
                                                                      dir,
                                                                      pattern = paste0("(?i)", font_name, ".*\\.(ttf|otf)$"),
                                                                      full.names = TRUE,
                                                                      ignore.case = TRUE
                                                        )

                                                        if (length(font_files) > 0) {
                                                                      if (load_local_font(font_files[1], family_name)) {
                                                                                    return(invisible(TRUE))
                                                                      }
                                                        }
                                          }
                            }
              }

              message(sprintf("✗ Could not find '%s' in any source", font_name))
              return(invisible(FALSE))
}


#' Enhanced ggplot Constructor with Axes Caps
#'
#' @description Convenience function for creating ggplot with axis caps.
#'   Applies guides that add caps to both x and y axes.
#'
#' @param ... Arguments passed to ggplot2::ggplot()
#'
#' @return ggplot object with axis caps
#'
#' @export
plot_base <- function(...) {
              ggplot2::ggplot(...) +
                            ggplot2::guides(
                                          x = ggplot2::guide_axis(cap = "both"),
                                          y = ggplot2::guide_axis(cap = "both")
                            )
}

#' Alias for plot_base (Original Name)
#'
#' @description Original function name. Use plot_base() instead.
#'
#' @param ... Arguments passed to ggplot2::ggplot()
#'
#' @return ggplot object with axis caps
#'
#' @export
kkplot <- plot_base

#' Univariate Categorical Plot
#'
#' @param data Data frame
#' @param variable Variable name (unquoted)
#'
#' @return ggplot object
#'
#' @export
univariate_categorical_plot <- function(data, variable) {
              validate_data_frame(data)
              variable <- rlang::ensym(variable)
              var_name <- rlang::as_name(variable)

              validate_categorical_column(data, var_name)

              title <- sprintf("Distribution of %s", var_name)

              data %>%
                            dplyr::count({{ variable }}) %>%
                            dplyr::filter(!is.na({{ variable }})) %>%
                            dplyr::mutate(prop = n / sum(n)) %>%
                            plot_base(ggplot2::aes(
                                          y = forcats::fct_reorder({{ variable }}, prop),
                                          x = prop
                            )) +
                            ggplot2::geom_col(alpha = 0.6, fill = "gray60", color = "black") +
                            ggplot2::geom_label(
                                          ggplot2::aes(label = sprintf("%d (%.1f%%)", n, prop * 100)),
                                          color = "black", size = 4, hjust = -0.1
                            ) +
                            ggplot2::scale_x_continuous(labels = scales::percent) +
                            ggplot2::expand_limits(x = 1) +
                            ggplot2::labs(x = "Proportion", y = "Category", title = title)
}

#' Univariate Continuous Plot
#'
#' @param data Data frame
#' @param variable Variable name (unquoted)
#'
#' @return ggplot object
#'
#' @export
univariate_continuous_plot <- function(data, variable) {
              validate_data_frame(data)
              variable <- rlang::ensym(variable)
              var_name <- rlang::as_name(variable)

              validate_numeric_column(data, var_name)

              title <- sprintf("Distribution of %s", var_name)

              mean_val <- mean(data[[var_name]], na.rm = TRUE)
              median_val <- stats::median(data[[var_name]], na.rm = TRUE)
              density_data <- stats::density(data[[var_name]], na.rm = TRUE)
              max_density <- max(density_data$y)

              data %>%
                            plot_base(ggplot2::aes(x = !!variable)) +
                            ggplot2::geom_density(
                                          adjust = 0.5, fill = "gray90", color = "black", alpha = 0.6
                            ) +
                            ggplot2::geom_vline(
                                          ggplot2::aes(xintercept = mean_val),
                                          color = "red", linetype = 1, linewidth = 1
                            ) +
                            ggplot2::geom_vline(
                                          ggplot2::aes(xintercept = median_val),
                                          color = "blue", linetype = "dashed", linewidth = 1
                            ) +
                            ggplot2::annotate(
                                          "label",
                                          x = mean_val, y = 0.9 * max_density,
                                          label = sprintf("Mean: %.2f", mean_val),
                                          color = "red", size = 4, hjust = -0.1
                            ) +
                            ggplot2::annotate(
                                          "label",
                                          x = median_val, y = 0.8 * max_density,
                                          label = sprintf("Median: %.2f", median_val),
                                          color = "blue", size = 4, hjust = -0.1
                            ) +
                            ggplot2::labs(x = "Value", y = "Density", title = title)
}

#' Univariate Plot (Auto-Detect Type)
#'
#' @param data Data frame
#' @param variable Variable name (unquoted)
#'
#' @return ggplot object
#'
#' @export
univariate_plot <- function(data, variable) {
              validate_data_frame(data)
              variable <- rlang::ensym(variable)
              var_name <- rlang::as_name(variable)

              if (is.numeric(data[[var_name]])) {
                            univariate_continuous_plot(data, !!variable)
              } else if (is.character(data[[var_name]]) || is.factor(data[[var_name]])) {
                            univariate_categorical_plot(data, !!variable)
              } else {
                            stop(sprintf("Variable '%s' must be numeric or categorical", var_name))
              }
}
