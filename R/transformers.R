pdfcdf <- function(pdf) {
  d <- dim(pdf)
  dn <- dimnames(pdf)

  if (is.null(d)) {
    return(cumsum(pdf))
  } else if (length(d) == 2) {
    out <- C_mat_PdfCdf(pdf)
  } else if (length(d) == 3) {
    # quicker than apply with C_mat_
    out <- aperm(apply(unname(pdf), c(1, 3), C_vec_PdfCdf), c(2, 1, 3))
  } else {
    stop(sprintf("Expected maximum of three dimensions but got '%s'.", length(d)))
  }

  dimnames(out) <- dn
  out
}

cdfpdf <- function(cdf) {
  d <- dim(cdf)
  dn <- dimnames(cdf)

  if (is.null(d)) {
    return(c(cdf[1], diff(cdf)))
  } else if (length(d) == 2) {
    out <- C_mat_CdfPdf(cdf)
  } else if (length(d) == 3) {
    # quicker than apply with C_mat_
    out <- aperm(apply(unname(cdf), c(1, 3), C_vec_CdfPdf), c(2, 1, 3))
  } else {
    stop(sprintf("Expected maximum of three dimensions but got '%s'.", length(d)))
  }

    dimnames(out) <- dn
    out
}
