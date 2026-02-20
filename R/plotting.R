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
#' @import ggtext
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
                            title_size <- size
                            subtitle_size <- size * 0.95
                            caption_size <- size * 0.7
                            axis_title_size <- size * 0.93
                            axis_text_size <- size * 0.93
                            strip_text_size <- size * 0.93

                            # Scale ticks and line weights relative to font size
                            tick_len <- size * (3 / 16)
                            line_w <- size / 60

                            theme_nice <- ggthemes::theme_tufte() +
                                          theme(
                                                        text = element_text(size = size, family = font_family),
                                                        axis.ticks = element_line(linewidth = line_w, color = "black"),
                                                        axis.ticks.length = unit(tick_len, "mm"),
                                                        plot.title = ggtext::element_markdown(family = font_family, size = title_size, hjust = 0, vjust = 1, margin = margin(t = 2, b = 2), face = "bold"),
                                                        plot.subtitle = ggtext::element_markdown(family = font_family, size = subtitle_size, lineheight = 1, margin = margin(b = 2)),
                                                        plot.caption = ggtext::element_markdown(family = font_family, hjust = 0.5, vjust = 1, size = caption_size),
                                                        plot.caption.position = "plot",
                                                        axis.title = element_text(family = font_family, size = axis_title_size),
                                                        axis.text = element_text(family = font_family, size = axis_text_size),
                                                        axis.text.x = element_text(margin = margin(t = size / 3)),
                                                        # Explicitly blank out secondary axes to avoid duplication
                                                        axis.text.y.right = element_blank(),
                                                        axis.title.y.right = element_blank(),
                                                        axis.text.x.top = element_blank(),
                                                        axis.title.x.top = element_blank(),
                                                        strip.text = element_text(family = font_family, size = strip_text_size),
                                                        axis.line = element_line(linewidth = line_w * 1.5),
                                                        panel.grid = element_blank(),
                                                        panel.border = element_blank(),
                                                        # Add relative top margin for breathing room when title is missing
                                                        plot.margin = margin(t = size * 0.8, r = size / 2, b = size / 2, l = size / 2),
                                                        legend.key = element_blank()
                                          )

                            # Enable showtext only if manually requested (avoiding ggsave issues)
                            # showtext::showtext_auto(enable = TRUE)

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

#' Correlation Heatmap Plot
#'
#' Creates a correlation heatmap (Kendall's Tau by default) for all numeric and factor variables in a data frame.
#' Factors are automatically converted to numeric for correlation calculation.
#'
#' @param data A data frame or tibble.
#' @param method Correlation method ("kendall", "pearson", or "spearman"). Defaults to "kendall".
#' @param adjust P-value adjustment method for multiple comparisons (e.g., "fdr", "bonferroni", "none"). Defaults to "none".
#' @param font_size Base font size for the plot. Defaults to 12.
#'
#' @return A ggplot2 object showing the upper triangle of the correlation matrix with coefficients and significance stars.
#' @export
#'
#' @examples
#' \dontrun{
#' kk_fullcorplot(mtcars)
#' }
kk_fullcorplot <- function(data, method = "kendall", adjust = "none", font_size = 12) {
              # 1. Automatic numeric conversion
              analysis_numeric <- data %>%
                            dplyr::mutate(dplyr::across(dplyr::where(is.factor), as.numeric)) %>%
                            dplyr::select(dplyr::where(is.numeric))

              # 2. Correlation + p-value matrices
              tau_test <- psych::corr.test(
                            analysis_numeric,
                            method = method,
                            use    = "pairwise",
                            adjust = adjust
              )

              # 3. Melt to long format
              var_order <- colnames(analysis_numeric)

              cor_long <- tau_test$r %>%
                            as.data.frame() %>%
                            tibble::rownames_to_column("Var1") %>%
                            tidyr::pivot_longer(-Var1, names_to = "Var2", values_to = "tau") %>%
                            dplyr::left_join(
                                          tau_test$p %>%
                                                        as.data.frame() %>%
                                                        tibble::rownames_to_column("Var1") %>%
                                                        tidyr::pivot_longer(-Var1, names_to = "Var2", values_to = "p"),
                                          by = c("Var1", "Var2")
                            ) %>%
                            dplyr::mutate(
                                          sig = dplyr::case_when(
                                                        Var1 == Var2 ~ "",
                                                        p < 0.001 ~ "***",
                                                        p < 0.01 ~ "**",
                                                        p < 0.05 ~ "*",
                                                        TRUE ~ ""
                                          ),
                                          Var1 = factor(Var1, levels = var_order),
                                          Var2 = factor(Var2, levels = var_order)
                            )

              # 4. Filter for upper triangle only
              cor_upper <- cor_long %>%
                            dplyr::filter(as.integer(Var1) < as.integer(Var2))

              # 5. Legend label based on method
              legend_name <- dplyr::case_when(
                            method == "kendall" ~ "Kendall \u03c4",
                            method == "spearman" ~ "Spearman \u03c1",
                            TRUE ~ "Pearson r"
              )

              # 6. Plot
              kkplot(cor_upper, ggplot2::aes(x = Var1, y = Var2, fill = tau)) +
                            ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
                            ggplot2::geom_text(
                                          ggplot2::aes(label = sprintf("%.2f\n%s", tau, sig)),
                                          size = font_size / 3,
                                          colour = "black",
                                          lineheight = 0.8,
                                          parse = FALSE
                            ) +
                            ggplot2::scale_fill_gradient2(
                                          low      = "#2166ac",
                                          mid      = "white",
                                          high     = "#d6604d",
                                          midpoint = 0,
                                          limits   = c(-1, 1),
                                          name     = legend_name
                            ) +
                            ggplot2::labs(
                                          title = NULL,
                                          subtitle = NULL,
                                          x = NULL,
                                          y = NULL
                            ) +
                            ggplot2::theme(
                                          axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1),
                                          legend.position = "right",
                                          panel.grid = ggplot2::element_blank()
                            )
}

#' Univariate Categorical Plot
#'
#' @param data Data frame
#' @param variable Variable to plot
#' @param group Optional grouping variable
#' @param label_size Size for text labels (default: 3.5)
#' @param force_100 Whether to force the x-axis to 100% (default: FALSE)
#'
#' @examples
#' univariate_cat_plot(mtcars, "am")
#' univariate_cat_plot(mtcars, "am", group = "cyl")
#'
#' @export
univariate_cat_plot <- function(data, variable, group = NULL, label_size = 3.5, force_100 = FALSE) {
              variable <- rlang::ensym(variable)
              var_name <- rlang::as_name(variable)

              group_sym <- if (!is.null(group)) rlang::ensym(group) else NULL
              group_name <- if (!is.null(group_sym)) rlang::as_name(group_sym) else NULL

              title <- if (is.null(group_name)) {
                            paste("Univariate Categorical Plot of", var_name)
              } else {
                            paste("Univariate Categorical Plot of", var_name, "by", group_name)
              }

              # Calculate missing count
              na_count <- sum(is.na(data[[var_name]]))
              subtitle <- if (is.null(group_name)) {
                            paste0("Missing: ", na_count)
              } else {
                            NULL
              }

              # Get current theme base size for relative labeling
              base_size <- tryCatch(ggplot2::theme_get()$text$size, error = function(e) 11)
              if (is.null(base_size)) base_size <- 11
              rel_label_size <- (base_size * 0.93) / ggplot2::.pt

              # Get current theme font
              curr_font <- tryCatch(ggplot2::theme_get()$text$family, error = function(e) "sans")
              if (is.null(curr_font) || curr_font == "") curr_font <- "sans"

              if (is.null(group_name)) {
                            plot_data <- data %>%
                                          dplyr::filter(!is.na(!!variable)) %>%
                                          dplyr::count(!!variable) %>%
                                          dplyr::mutate(prop = n / sum(n))

                            p <- plot_data %>%
                                          kkplot(aes(y = forcats::fct_reorder(factor(!!variable), prop), x = prop)) +
                                          geom_col(
                                                        fill = "gray85",
                                                        color = "gray30",
                                                        width = 0.9,
                                                        linewidth = 0.2
                                          ) +
                                          geom_label(
                                                        aes(label = paste0(n, " (", scales::percent(prop, accuracy = 1), ")")),
                                                        color = "black",
                                                        size = rel_label_size,
                                                        family = curr_font,
                                                        hjust = -0.1,
                                                        label.size = 0.1,
                                                        label.padding = unit(0.1, "lines"),
                                                        label.r = unit(0.05, "lines"),
                                                        fill = "white",
                                                        alpha = 0.9
                                          )
              } else {
                            plot_data <- data %>%
                                          dplyr::filter(!is.na(!!variable), !is.na(!!group_sym)) %>%
                                          dplyr::count(!!group_sym, !!variable) %>%
                                          dplyr::group_by(!!group_sym) %>%
                                          dplyr::mutate(prop = n / sum(n)) %>%
                                          dplyr::ungroup()

                            # Reorder y by overall proportion
                            overall_counts <- data %>%
                                          dplyr::filter(!is.na(!!variable)) %>%
                                          dplyr::count(!!variable) %>%
                                          dplyr::mutate(total_prop = n / sum(n))

                            plot_data <- plot_data %>%
                                          dplyr::left_join(overall_counts %>% dplyr::select(!!variable, total_prop), by = var_name)

                            # Calculate group sizes for legend labels
                            group_sizes <- data %>%
                                          dplyr::filter(!is.na(!!variable), !is.na(!!group_sym)) %>%
                                          dplyr::count(!!group_sym) %>%
                                          dplyr::mutate(label = paste0(!!group_sym, "\nN=", n))

                            group_labels <- setNames(group_sizes$label, group_sizes[[group_name]])

                            p <- plot_data %>%
                                          kkplot(aes(y = forcats::fct_reorder(factor(!!variable), total_prop), x = prop, fill = factor(!!group_sym), color = factor(!!group_sym))) +
                                          geom_col(
                                                        position = position_dodge(width = 0.9),
                                                        width = 0.9,
                                                        linewidth = 0.4,
                                                        key_glyph = "path"
                                          ) +
                                          geom_label(
                                                        aes(
                                                                      label = paste0(n, " (", scales::percent(prop, accuracy = 1), ")"),
                                                                      group = factor(!!group_sym)
                                                        ),
                                                        position = position_dodge(width = 0.9),
                                                        color = "black",
                                                        size = rel_label_size,
                                                        family = curr_font,
                                                        hjust = -0.1,
                                                        vjust = 0.5,
                                                        label.size = 0.1,
                                                        label.padding = unit(0.1, "lines"),
                                                        label.r = unit(0.05, "lines"),
                                                        fill = "white",
                                                        alpha = 0.9,
                                                        show.legend = FALSE
                                          ) +
                                          scale_fill_brewer(palette = "Set2", labels = group_labels) +
                                          scale_color_brewer(palette = "Set2", labels = group_labels)
              }

              p +
                            # 2.5% expansion from Y axis, and room for labels on the right
                            scale_x_continuous(
                                          labels = scales::percent,
                                          expand = expansion(mult = c(0.025, 0.2)),
                                          limits = if (force_100) c(0, 1) else NULL
                            ) +
                            scale_y_discrete(expand = expansion(add = c(0.5, 0.5))) +
                            labs(
                                          x = "Proportion",
                                          y = "Category",
                                          title = title,
                                          subtitle = subtitle,
                                          fill = NULL,
                                          color = NULL
                            ) +
                            theme(
                                          legend.position = "top",
                                          legend.key = element_blank(),
                                          plot.subtitle = ggtext::element_markdown()
                            )
}

#' Univariate Continuous Plot
#'
#' @param data Data frame
#' @param variable Variable to plot
#' @param group Optional grouping variable
#' @param label_size Size for text labels (default: 3.5)
#'
#' @examples
#' univariate_cont_plot(mtcars, "mpg")
#' univariate_cont_plot(mtcars, "mpg", group = "cyl")
#'
#' @export
univariate_cont_plot <- function(data, variable, group = NULL, label_size = 3.5, stats = "mean") {
              variable <- rlang::ensym(variable)
              var_name <- rlang::as_name(variable)

              group_sym <- if (!is.null(group)) rlang::ensym(group) else NULL
              group_name <- if (!is.null(group_sym)) rlang::as_name(group_sym) else NULL

              title <- if (is.null(group_name)) {
                            paste("Univariate Continuous Plot of", var_name)
              } else {
                            paste("Univariate Continuous Plot of", var_name, "by", group_name)
              }

              # Calculate missing count
              na_count <- sum(is.na(data[[var_name]]))
              subtitle_missing <- paste0("Missing: ", na_count)

              base_size <- tryCatch(ggplot2::theme_get()$text$size, error = function(e) 11)
              if (is.null(base_size)) base_size <- 11
              line_w <- base_size / 60

              # Increase label size for stats as requested
              stat_text_size <- label_size * 1.2

              if (is.null(group_name)) {
                            # Filter data for non-grouped calculations
                            plot_data <- data %>% dplyr::filter(!is.na(!!variable))
                            vals <- plot_data[[var_name]]

                            m_val <- if (length(vals) > 0) round(mean(vals), 2) else NA
                            sd_val <- if (length(vals) > 0) round(stats::sd(vals), 2) else NA
                            med_val <- if (length(vals) > 0) round(stats::median(vals), 2) else NA
                            iqr_val <- if (length(vals) > 0) round(stats::IQR(vals), 2) else NA
                            min_val <- if (length(vals) > 0) round(min(vals), 2) else NA
                            max_val <- if (length(vals) > 0) round(max(vals), 2) else NA

                            stats_parts <- character()
                            if (stats %in% c("mean", "both")) {
                                          stats_parts <- c(stats_parts, paste0("<span style='color:red;'>Mean (SD): ", m_val, " (", sd_val, ")</span>"))
                            }
                            if (stats %in% c("median", "both")) {
                                          stats_parts <- c(stats_parts, paste0("<span style='color:blue;'>Median (IQR): ", med_val, " (", iqr_val, ")</span>"))
                            }

                            subtitle <- paste0(
                                          subtitle_missing,
                                          " | ", paste(stats_parts, collapse = " | "),
                                          " | Range: [", min_val, ", ", max_val, "]"
                            )

                            p <- kkplot(data, aes(x = !!variable)) +
                                          geom_density(color = "#2c3e50", alpha = 0.8, linewidth = line_w * 1.5)

                            if (stats %in% c("mean", "both")) {
                                          p <- p + geom_vline(
                                                        xintercept = mean(data[[var_name]], na.rm = TRUE),
                                                        color = "#e74c3c",
                                                        linewidth = line_w * 3
                                          )
                            }
                            if (stats %in% c("median", "both")) {
                                          p <- p + geom_vline(
                                                        xintercept = stats::median(data[[var_name]], na.rm = TRUE),
                                                        color = "#3498db",
                                                        linetype = "dashed",
                                                        linewidth = line_w * 3
                                          )
                            }
              } else {
                            group_stats <- data %>%
                                          dplyr::filter(!is.na(!!variable), !is.na(!!group_sym)) %>%
                                          dplyr::group_by(!!group_sym) %>%
                                          dplyr::summarise(
                                                        n = dplyr::n(),
                                                        m = round(mean(!!variable, na.rm = TRUE), 2),
                                                        sd = round(stats::sd(!!variable, na.rm = TRUE), 2),
                                                        med = round(stats::median(!!variable, na.rm = TRUE), 2),
                                                        iqr = round(stats::IQR(!!variable, na.rm = TRUE), 2),
                                                        .groups = "drop"
                                          ) %>%
                                          dplyr::mutate(legend_label = paste0(!!group_sym, "\nN=", n))

                            group_labels <- setNames(group_stats$legend_label, group_stats[[group_name]])

                            # Build color-coded subtitle for groups
                            subtitle <- NULL

                            p <- data %>%
                                          dplyr::filter(!is.na(!!variable), !is.na(!!group_sym)) %>%
                                          kkplot(aes(x = !!variable, color = factor(!!group_sym))) +
                                          geom_density(linewidth = line_w * 2, key_glyph = "path") +
                                          scale_color_brewer(palette = "Set2", labels = group_labels)

                            if (stats %in% c("mean", "both")) {
                                          p <- p + geom_vline(
                                                        data = group_stats,
                                                        aes(xintercept = m, color = factor(!!group_sym)),
                                                        linewidth = line_w * 4,
                                                        alpha = 0.9,
                                                        show.legend = FALSE
                                          ) +
                                                        geom_text(
                                                                      data = group_stats,
                                                                      aes(x = m, y = Inf, label = paste0("M=", m), color = factor(!!group_sym)),
                                                                      vjust = 1.5,
                                                                      hjust = -0.1,
                                                                      size = stat_text_size,
                                                                      fontface = "bold",
                                                                      show.legend = FALSE
                                                        )
                            }

                            if (stats %in% c("median", "both")) {
                                          p <- p + geom_vline(
                                                        data = group_stats,
                                                        aes(xintercept = med, color = factor(!!group_sym)),
                                                        linetype = "dashed",
                                                        linewidth = line_w * 4,
                                                        alpha = 0.9,
                                                        show.legend = FALSE
                                          ) +
                                                        geom_text(
                                                                      data = group_stats,
                                                                      aes(x = med, y = Inf, label = paste0("Md=", med), color = factor(!!group_sym)),
                                                                      vjust = if (stats == "both") 3.5 else 1.5,
                                                                      hjust = -0.1,
                                                                      size = stat_text_size,
                                                                      fontface = "italic",
                                                                      show.legend = FALSE
                                                        )
                            }
              }

              p <- p +
                            # Apply consistent 2.5% expansion
                            scale_x_continuous(expand = expansion(mult = 0.025)) +
                            scale_y_continuous(expand = expansion(mult = 0.025)) +
                            labs(
                                          title = title,
                                          subtitle = subtitle,
                                          x = "Value",
                                          y = "Density",
                                          fill = NULL,
                                          color = NULL
                            ) +
                            theme(
                                          legend.position = "top",
                                          legend.key = element_blank(),
                                          plot.subtitle = ggtext::element_markdown()
                            )

              if (!is.null(data[[var_name]])) {
                            p <- p + expand_limits(x = range(data[[var_name]], na.rm = TRUE))
              }

              return(p)
}

#' Univariate Plot
#'
#' @description Automatically detects variable type and plots accordingly
#'
#' @param data Data frame
#' @param ... Variables to plot
#' @param group Optional grouping variable
#' @param categorical Explicitly mark variables as categorical
#' @param ordered Explicitly mark variables as ordered
#' @param continuous Explicitly mark variables as continuous
#' @param label_size Size for text labels (automatically calculated if NULL)
#' @param ncol Number of columns for layout
#' @param nrow Number of rows for layout
#' @param force_100 Whether to force categorical x-axis to 100% (default: TRUE)
#'
#' @examples
#' univariate_plot(mtcars, "mpg")
#' univariate_plot(mtcars, "am")
#' univariate_plot(mtcars, "mpg", group = "cyl")
#'
#' @export
univariate_plot <- function(data, ..., group = NULL, categorical = NULL, ordered = NULL, continuous = NULL, label_size = NULL, ncol = NULL, nrow = NULL, force_100 = TRUE, stats = "mean") {
              # Select variables
              vars <- tidyselect::eval_select(rlang::expr(c(...)), data)
              if (length(vars) == 0) {
                            vars <- names(data)
              } else {
                            vars <- names(vars)
              }

              # Exclude group variable if provided
              if (!is.null(group)) {
                            group_name <- rlang::as_name(rlang::ensym(group))
                            vars <- vars[vars != group_name]
              }

              # Auto-calculate label size relative to base font size if not provided
              if (is.null(label_size)) {
                            base_size <- tryCatch(ggplot2::theme_get()$text$size, error = function(e) 11)
                            if (is.null(base_size)) base_size <- 11

                            n_plots <- length(vars)
                            scale_factor <- if (n_plots <= 1) 0.8 else if (n_plots <= 4) 0.7 else if (n_plots <= 9) 0.6 else 0.5

                            # Convert from pts to ggplot2 internal text size (mm)
                            label_size <- (base_size * scale_factor) / ggplot2::.pt
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
                                          rlang::inject(univariate_cont_plot(data, !!rlang::sym(var), group = !!group, label_size = label_size, stats = stats))
                            } else {
                                          # For both categorical and ordered, we use cat_plot for now
                                          # (could expand ordered later with specific logic if needed)
                                          rlang::inject(univariate_cat_plot(data, !!rlang::sym(var), group = !!group, label_size = label_size, force_100 = force_100))
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
