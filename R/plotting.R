# ============================================================
# PLOTTING FUNCTIONS
# ============================================================

#' Set Plot Font
#'
#' @description Sets the font for ggplot2 plots, searching Google Fonts, system fonts, and local directories.
#'
#' @param font Font family name (default: "Roboto Condensed")
#' @param size Base font size (default: 18)
#' @param search_sources Sources to search: "google", "system", "local"
#' @param fallbacks Fallback fonts
#' @param update_theme Whether to update the current ggplot theme
#'
#' @return List with font details
#'
#' @examples
#' \dontrun{
#' set_plot_font("Roboto Condensed")
#' }
#'
#' @import ggplot2
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

              # Helper function to normalize font names for matching
              # Removes spaces, hyphens, and converts to lowercase
              normalize_name <- function(name) {
                            tolower(gsub("[\\s-]", "", name))
              }

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

              # Helper function to get OS-specific font directories
              get_font_directories <- function() {
                            os_type <- Sys.info()["sysname"]

                            font_dirs <- character()

                            if (os_type == "Windows") {
                                          font_dirs <- c(
                                                        file.path(Sys.getenv("WINDIR"), "Fonts"),
                                                        file.path(Sys.getenv("LOCALAPPDATA"), "Microsoft", "Windows", "Fonts")
                                          )
                            } else if (os_type == "Darwin") { # macOS
                                          font_dirs <- c(
                                                        "/System/Library/Fonts",
                                                        "/Library/Fonts",
                                                        file.path(Sys.getenv("HOME"), "Library", "Fonts"),
                                                        "/System/Library/Fonts/Supplemental",
                                                        "/Network/Library/Fonts"
                                          )
                            } else { # Linux and other Unix-like
                                          font_dirs <- c(
                                                        "/usr/share/fonts",
                                                        "/usr/local/share/fonts",
                                                        file.path(Sys.getenv("HOME"), ".fonts"),
                                                        file.path(Sys.getenv("HOME"), ".local", "share", "fonts"),
                                                        "/usr/share/fonts/truetype",
                                                        "/usr/share/fonts/opentype"
                                          )
                            }

                            # Return only existing directories
                            return(unique(font_dirs[dir.exists(font_dirs)]))
              }

              # Helper function to recursively search font directories
              search_font_files <- function(dirs, family) {
                            # Normalize the search term
                            normalized_family <- normalize_name(family)

                            # Common font file extensions
                            extensions <- c("ttf", "otf", "ttc", "dfont")

                            all_fonts <- character()

                            for (dir in dirs) {
                                          # Recursively list all font files
                                          for (ext in extensions) {
                                                        pattern <- paste0("\\.", ext, "$")
                                                        files <- list.files(dir,
                                                                      pattern = pattern,
                                                                      full.names = TRUE,
                                                                      recursive = TRUE,
                                                                      ignore.case = TRUE
                                                        )
                                                        all_fonts <- c(all_fonts, files)
                                          }
                            }

                            # Match fonts by normalized filename
                            matched_fonts <- character()
                            for (font_file in all_fonts) {
                                          basename_normalized <- normalize_name(tools::file_path_sans_ext(basename(font_file)))

                                          # Check if normalized family name is in the normalized filename
                                          if (grepl(normalized_family, basename_normalized, fixed = TRUE)) {
                                                        matched_fonts <- c(matched_fonts, font_file)
                                          }
                            }

                            return(matched_fonts)
              }

              # Helper function to search local fonts
              search_local_fonts <- function(family) {
                            font_dirs <- get_font_directories()

                            if (length(font_dirs) == 0) {
                                          message(" ✗ No font directories found")
                                          return(FALSE)
                            }

                            message(" → Searching in ", length(font_dirs), " directories...")

                            # Search for matching font files
                            matched_fonts <- search_font_files(font_dirs, family)

                            if (length(matched_fonts) == 0) {
                                          message(" ✗ No matching font files found")
                                          return(FALSE)
                            }

                            message(" → Found ", length(matched_fonts), " matching font file(s)")

                            # Try to add the first regular font (prioritize "regular" in filename)
                            regular_fonts <- matched_fonts[grepl("regular", basename(matched_fonts), ignore.case = TRUE)]

                            font_to_add <- if (length(regular_fonts) > 0) regular_fonts[1] else matched_fonts[1]

                            # Try to add the font
                            tryCatch(
                                          {
                                                        # Extract the base family name without variant
                                                        base_name <- tools::file_path_sans_ext(basename(font_to_add))
                                                        # Remove common variant suffixes
                                                        base_name <- gsub("-(Regular|Bold|Italic|Light|Medium|Heavy|Black).*$", "", base_name, ignore.case = TRUE)

                                                        # Use the original family name for consistency
                                                        sysfonts::font_add(family = family, regular = font_to_add)

                                                        if (test_font(family)) {
                                                                      message(" ✓ Added '", family, "' from: ", basename(font_to_add))
                                                                      return(TRUE)
                                                        } else {
                                                                      message(" ✗ Font added but not available in family list")
                                                                      return(FALSE)
                                                        }
                                          },
                                          error = function(e) {
                                                        message(" ✗ Failed to add font: ", e$message)
                                                        return(FALSE)
                                          }
                            )
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

                                          # Try system first
                                          if (check_system_font(fb)) {
                                                        font_family <- fb
                                                        font_loaded <- TRUE
                                                        message(" ✓ Using fallback: ", fb)
                                                        break
                                          }

                                          # Try local search
                                          if (search_local_fonts(fb)) {
                                                        font_family <- fb
                                                        font_loaded <- TRUE
                                                        message(" ✓ Using fallback: ", fb, " (from local)")
                                                        break
                                          }
                            }
              }

              # Final fallback to system default
              if (!font_loaded) {
                            font_family <- "sans"
                            message(" ⚠ Using system default 'sans'")
              }

              # Enable showtext for consistent font rendering
              # Uncomment if you want to enable showtext
              # showtext::showtext_auto(enable = TRUE)
              # message("✓ Enabled showtext for font rendering")

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
                                          theme(
                                                        axis.ticks = element_line(linewidth = 0.5, color = "black"),
                                                        axis.ticks.length = unit(4, "mm"),
                                                        plot.title = element_text(family = font_family, size = title_size, hjust = 0, vjust = 2, margin = margin(t = 10, b = 10)),
                                                        plot.subtitle = element_text(family = font_family, size = subtitle_size),
                                                        plot.caption = element_text(family = font_family, hjust = 0.5, vjust = 1, size = caption_size),
                                                        plot.caption.position = "plot",
                                                        axis.title = element_text(family = font_family, size = axis_title_size),
                                                        axis.text = element_text(family = font_family, size = axis_text_size),
                                                        axis.text.x = element_text(margin = margin(5, b = 10)),
                                                        strip.text = element_text(family = font_family, size = strip_text_size),
                                                        axis.line = element_line()
                                          )

                            theme_set(theme_nice)
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

#' KK Plot Wrapper
#'
#' @description Wrapper for ggplot with axis caps
#'
#' @param ... Arguments passed to ggplot
#'
#' @examples
#' kkplot(mtcars, aes(x = mpg, y = wt)) + geom_point()
#'
#' @export
kkplot <- function(...) {
              ggplot(...) +
                            guides(x = guide_axis(cap = "both"), y = guide_axis(cap = "both"))
}

#' Univariate Categorical Plot
#'
#' @param data Data frame
#' @param label_size Size for text labels (default: 3.5)
#'
#' @examples
#' univariate_cat_plot(mtcars, "am")
#'
#' @export
univariate_cat_plot <- function(data, variable, label_size = 3.5) {
              variable <- rlang::ensym(variable)
              var_name <- rlang::as_name(variable)
              title <- paste("Univariate Categorical Plot of", var_name)

              # Calculate missing count
              na_count <- sum(is.na(data[[var_name]]))
              subtitle <- paste0("Missing: ", na_count)

              data %>%
                            dplyr::filter(!is.na(!!variable)) %>%
                            dplyr::count(!!variable) %>%
                            dplyr::mutate(prop = n / sum(n)) %>%
                            kkplot(aes(y = forcats::fct_reorder(factor(!!variable), prop), x = prop)) +
                            geom_col(
                                          alpha = 0.6,
                                          fill = "gray60",
                                          color = "black"
                            ) +
                            geom_label(
                                          aes(label = paste0(n, " (", scales::percent(prop), ")")),
                                          color = "black",
                                          size = label_size,
                                          family = "Roboto Condensed",
                                          hjust = -0.1
                            ) +
                            scale_x_continuous(labels = scales::percent) +
                            expand_limits(x = 1) +
                            labs(
                                          x = "Proportion",
                                          y = "Category",
                                          title = title,
                                          subtitle = subtitle
                            )
}

#' Univariate Continuous Plot
#'
#' @param data Data frame
#' @param label_size Size for text labels (default: 3.5)
#'
#' @examples
#' univariate_cont_plot(mtcars, "mpg")
#'
#' @export
univariate_cont_plot <- function(data, variable, label_size = 3.5) {
              variable <- rlang::ensym(variable)
              var_name <- rlang::as_name(variable)
              title <- paste("Univariate Continuous Plot of", var_name)

              # Calculate missing count
              na_count <- sum(is.na(data[[var_name]]))
              subtitle <- paste0("Missing: ", na_count)

              # Filter data for calculations
              plot_data <- data %>% dplyr::filter(!is.na(!!variable))
              vals <- plot_data[[var_name]]

              p <- kkplot(plot_data, aes(x = !!variable)) +
                            geom_density(
                                          adjust = 1 / 2,
                                          fill = "gray90",
                                          color = "black",
                                          alpha = 0.6
                            ) +
                            geom_vline(
                                          aes(xintercept = mean(!!variable)),
                                          color = "red",
                                          linetype = 1,
                                          linewidth = 1
                            ) +
                            geom_vline(
                                          aes(xintercept = stats::median(!!variable)),
                                          color = "blue",
                                          linetype = "dashed",
                                          linewidth = 1,
                            )

              if (length(vals) > 0) {
                            p <- p +
                                          annotate(
                                                        "label",
                                                        x = mean(vals),
                                                        y = 0.9 * max(stats::density(vals)$y),
                                                        label = paste0("Mean: ", round(mean(vals), 2)),
                                                        color = "red",
                                                        size = label_size,
                                                        family = "Roboto Condensed",
                                                        hjust = -0.1
                                          ) +
                                          annotate(
                                                        "label",
                                                        x = stats::median(vals),
                                                        y = 0.8 * max(stats::density(vals)$y),
                                                        label = paste0("Median: ", round(stats::median(vals), 2)),
                                                        color = "blue",
                                                        size = label_size,
                                                        family = "Roboto Condensed",
                                                        hjust = -0.1
                                          ) +
                                          expand_limits(x = range(vals))
              }

              p + labs(
                            x = "Value",
                            y = "Density",
                            title = title,
                            subtitle = subtitle
              )
}

#' Univariate Plot
#'
#' @description Automatically detects variable type and plots accordingly
#'
#' @param data Data frame
#' @param label_size Size for text labels (automatically calculated if NULL)
#'
#' @examples
#' univariate_plot(mtcars, "mpg")
#' univariate_plot(mtcars, "am")
#'
#' @export
univariate_plot <- function(data, ..., categorical = NULL, ordered = NULL, continuous = NULL, label_size = NULL, ncol = NULL, nrow = NULL) {
              # Select variables
              vars <- tidyselect::eval_select(rlang::expr(c(...)), data)
              if (length(vars) == 0) {
                            vars <- names(data)
              } else {
                            vars <- names(vars)
              }

              # Auto-calculate label size if not provided
              if (is.null(label_size)) {
                            n_plots <- length(vars)
                            if (n_plots <= 1) {
                                          label_size <- 4.5
                            } else if (n_plots <= 4) {
                                          label_size <- 4
                            } else if (n_plots <= 9) {
                                          label_size <- 3.5
                            } else {
                                          label_size <- 2.5
                            }
              }

              plots <- purrr::map(vars, function(var) {
                            # Determine type
                            type <- NULL
                            if (var %in% categorical) type <- "categorical"
                            if (var %in% ordered) type <- "ordered"
                            if (var %in% continuous) type <- "continuous"

                            if (is.null(type)) {
                                          if (is.numeric(data[[var]])) {
                                                        type <- "continuous"
                                          } else {
                                                        type <- "categorical"
                                          }
                            }

                            if (type == "continuous") {
                                          rlang::inject(univariate_cont_plot(data, !!rlang::sym(var), label_size = label_size))
                            } else {
                                          # For both categorical and ordered, we use cat_plot for now
                                          # (could expand ordered later with specific logic if needed)
                                          rlang::inject(univariate_cat_plot(data, !!rlang::sym(var), label_size = label_size))
                            }
              })

              if (length(plots) == 1) {
                            return(plots[[1]])
              } else {
                            return(patchwork::wrap_plots(plots, ncol = ncol, nrow = nrow))
              }
}

#' @rdname univariate_cat_plot
#' @export
univariate_categorical_plot <- univariate_cat_plot

#' @rdname univariate_cont_plot
#' @export
univariate_continuous_plot <- univariate_cont_plot

#' Plot Proportion Comparisons
#'
#' @description Visualizes the output of `compare_proportions` or `compare_proportions_kk_glm` using a forest plot.
#'
#' @param results Data frame returned by `compare_proportions` or `compare_proportions_kk_glm`
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param xlab Label for x-axis
#'
#' @return ggplot object
#'
#' @examples
#' \dontrun{
#' # Assuming results from compare_proportions
#' plot_proportion_comparisons(results)
#' }
#'
#' @export
plot_proportion_comparisons <- function(results,
                                        title = "Proportion Comparisons",
                                        subtitle = NULL,
                                        xlab = "Difference in Proportions (95% CI)") {
              # Check if required columns exist
              required_cols <- c("estimate", "conf_low", "conf_high")

              # Handle different column names from different functions
              if ("prop_diff" %in% names(results)) {
                            results <- results %>%
                                          dplyr::rename(
                                                        estimate = prop_diff,
                                                        conf_low = ci_lower,
                                                        conf_high = ci_upper
                                          )
              }

              if (!all(required_cols %in% names(results))) {
                            stop("Results data frame must contain 'estimate', 'conf_low', and 'conf_high' (or 'prop_diff', 'ci_lower', 'ci_upper').")
              }

              # Create a label for the comparison
              if ("group1" %in% names(results) && "group2" %in% names(results)) {
                            results <- results %>%
                                          dplyr::mutate(comparison = paste0(group1, " vs ", group2))
              } else if ("subgroup1" %in% names(results) && "subgroup2" %in% names(results)) {
                            results <- results %>%
                                          dplyr::mutate(comparison = paste0(group, ": ", subgroup1, " vs ", subgroup2))
              } else {
                            # Fallback if no clear group names
                            results <- results %>%
                                          dplyr::mutate(comparison = paste0("Comparison ", dplyr::row_number()))
              }

              kkplot(results, aes(x = estimate, y = comparison, xmin = conf_low, xmax = conf_high)) +
                            geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
                            geom_errorbarh(height = 0.2, color = "#2c3e50", linewidth = 0.8) +
                            geom_point(size = 3, color = "#e74c3c") +
                            labs(
                                          title = title,
                                          subtitle = subtitle,
                                          x = xlab,
                                          y = ""
                            )
}
