#' Replace a page or pages in a PDF optionally keeping the bookmarks and
#' document information
#'
#' By default, [qpdf::pdf_combine()] drops the document info and bookmarks from
#' an output file. This function replaces PDF pages (by default the first aka
#' cover page) while keeping the same document info and bookmarks as the input
#' file.
#'
replace_pdf_page <- function(input,
                             output = input,
                             from = 2,
                             replacement = here::here("files", "FY2530_CIP-Ordinance-Report_Cover-Update.pdf"),
                             keep = c("bookmarks", "docinfo")) {
  report_docinfo <- xmpdf::get_docinfo(input)[[1]]
  report_bookmarks <- xmpdf::get_bookmarks(input)[[1]]

  pages <- seq.int(from = from, to = pdftools::pdf_length(input), by = 1)

  temp_pdf <- tempfile(fileext = "pdf")

  qpdf::pdf_subset(
    input = input,
    pages = pages,
    output = temp_pdf
  )

  qpdf::pdf_combine(
    input = c(
      replacement,
      temp_pdf
    ),
    output = output
  )

  if ("docinfo" %in% keep) {
    xmpdf::set_docinfo(report_docinfo, output)
  }

  if ("bookmarks" %in% keep) {
    xmpdf::set_bookmarks(report_bookmarks, output)
  }

  output
}
