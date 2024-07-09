#' Combine PDF with PDF cover
combine_pdf_cover <- function(input,
                              cover,
                              output = NULL,
                              from = 2,
                              to = NULL,
                              password = "") {
  # stopifnot(
  #   here::here(file.exists(input)),
  #   here::here(file.exists(cover))
  # )

  input_pages <- seq.int(
    from = from,
    to = to %||% qpdf::pdf_length(input, password),
    by = 1
  )

  subset_output <- file.path(dirname(output), paste0("body_", basename(output)))

  subset_output <- qpdf::pdf_subset(
    input,
    pages = input_pages,
    output = subset_output,
    password = password
  )

  qpdf::pdf_combine(input = c(cover, subset_output), output = output)

  file.remove(subset_output)

  invisible(output)
}
