pkg_path <- "c:/Users/drkos/Desktop/kkstatfun"
devtools::load_all(pkg_path)

# Get all exported functions
ns <- asNamespace("kkstatfun")
exports <- getNamespaceExports(ns)
functions <- exports[sapply(exports, function(x) is.function(get(x, envir = ns)))]

# Check for Rd files and examples
results <- data.frame(
              Function = functions,
              HasRd = FALSE,
              HasExample = FALSE,
              stringsAsFactors = FALSE
)

man_dir <- file.path(pkg_path, "man")
rd_files <- list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE)

for (i in seq_along(functions)) {
              fn <- functions[i]

              # Check if Rd file exists (either as fn.Rd or alias)
              # This is a simplified check; real check involves parsing Rd files
              # We will check if the function name appears as \alias{fn} in any Rd file

              found_rd <- FALSE
              found_example <- FALSE

              for (rd_file in rd_files) {
                            rd_content <- readLines(rd_file)
                            if (any(grepl(paste0("\\\\alias\\{", fn, "\\}"), rd_content))) {
                                          found_rd <- TRUE
                                          # Check for example in this Rd file
                                          if (any(grepl("\\\\examples\\{", rd_content))) {
                                                        found_example <- TRUE
                                          }
                                          break
                            }
              }

              results$HasRd[i] <- found_rd
              results$HasExample[i] <- found_example
}

print(results)

if (all(results$HasRd) && all(results$HasExample)) {
              print("SUCCESS: All exported functions have documentation and examples.")
} else {
              print("FAILURE: Some functions are missing documentation or examples.")
              print(results[!results$HasRd | !results$HasExample, ])
}
