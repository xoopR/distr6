pdfcdf <- function(pdf) {
  d <- dim(pdf)
  if (is.null(d)) {
    cumsum(pdf)
  } else if (length(d) == 2) {
    C_mat_PdfCdf(pdf)
  } else if (length(d) == 3) {
    # quicker than apply with C_mat_
    dn <- dimnames(pdf)
    out <- aperm(apply(unname(pdf), c(1, 3), C_vec_PdfCdf), c(2, 1, 3))
    dimnames(out) <- dn
    out
  } else {
    stop(sprintf("Expected maximum of three dimensions but got '%s'.", length(d)))
  }
}

cdfpdf <- function(cdf) {
  d <- dim(cdf)
  if (is.null(d)) {
    c(cdf[1], diff(cdf))
  } else if (length(d) == 2) {
    C_mat_CdfPdf(cdf)
  } else if (length(d) == 3) {
    # quicker than apply with C_mat_
    dn <- dimnames(cdf)
    out <- aperm(apply(unname(cdf), c(1, 3), C_vec_CdfPdf), c(2, 1, 3))
    dimnames(out) <- dn
    out
  } else {
    stop(sprintf("Expected maximum of three dimensions but got '%s'.", length(d)))
  }
}
