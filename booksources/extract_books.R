# Script to extract searchable text from PDF reference books in booksources/
# Run this script once to generate lightweight, searchable .txt files for fast lookup and AI pairing.

if (!requireNamespace("pdftools", quietly = TRUE)) {
  message("Installing 'pdftools' package for PDF text extraction...")
  install.packages("pdftools")
}

library(pdftools)

pdf_dir <- file.path("booksources")
out_dir <- file.path(pdf_dir, "extracted_text")

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

pdf_files <- list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE)

message("Found ", length(pdf_files), " PDF files to extract.")

for (pdf_path in pdf_files) {
  fname <- tools::file_path_sans_ext(basename(pdf_path))
  out_file <- file.path(out_dir, paste0(fname, ".txt"))
  
  if (file.exists(out_file)) {
    message("Skipping (already extracted): ", fname)
    next
  }
  
  message("Extracting text from: ", basename(pdf_path), " ...")
  pages <- pdftools::pdf_text(pdf_path)
  
  # Combine with page header markers for easy navigation
  formatted_pages <- character(length(pages))
  for (i in seq_along(pages)) {
    formatted_pages[i] <- paste0("--- PAGE ", i, " ---\n\n", pages[i])
  }
  
  writeLines(formatted_pages, out_file, useBytes = TRUE)
  message("Saved: ", out_file, " (", length(pages), " pages)")
}

message("\nAll text extracted to: ", out_dir)
message("You (and Antigravity) can now ripgrep/grep_search formulas, definitions, and code across all books instantly!")
