# unify column names across pdfs in
#  matrices (merge_matpdf_cols) or arrays (merge_arrpdf_cols)
.merge_cols <- function(arrs, fun = "pdf") {
  if (fun == "cdf") {
    arrs <- lapply(arrs, cdfpdf)
  } else if (fun == "surv") {
    arrs <- lapply(arrs, function(.x) cdfpdf(1 - .x))
  } else if (fun != "pdf") {
    stop(sprintf(
      "Expected 'fun' to be 'pdf', 'cdf', or 'surv'. Got: '%s'.",
      fun
    ))
  }

  if (length(dim(arrs[[1L]])) == 2L) {
    out <- .merge_matpdf_cols(arrs)
  } else {
    out <- .merge_arrpdf_cols(arrs)
  }

  if (fun == "cdf") {
    lapply(out, pdfcdf)
  } else if (fun == "surv") {
    lapply(out, function(.x) 1 - pdfcdf(.x))
  } else {
    out
  }
}

.merge_arrpdf_cols <- function(pdfs) {
  if (length(unique(viapply(pdfs, function(.x) dim(.x)[[3L]]))) > 1) {
    stop("Can only merge arrays with same length on third dimension.")
  }

  nc <- unique(viapply(pdfs, ncol))

  if (length(nc) == 1) {
    if (all(vapply(pdfs, colnames, character(nc)) == colnames(pdfs[[1]]))) {
      return(pdfs)
    }
  }

  cnms <- sort(unique(as.numeric(unlist(lapply(pdfs, colnames)))))
  # new number of rows and columns
  nc <- length(cnms)
  nl <- dim(pdfs[[1]])[3L]

  lapply(pdfs, function(.x) {
    out <- array(0, c(nrow(.x), nc, nl), list(NULL, cnms, NULL))
    out[, match(as.numeric(colnames(.x)), cnms), ] <- .x
    out
  })
}

.merge_matpdf_cols <- function(pdfs) {

  nc <- unique(viapply(pdfs, ncol))

  if (length(nc) == 1) {
    if (all(vapply(pdfs, colnames, character(nc)) == colnames(pdfs[[1]]))) {
      return(pdfs)
    }
  }

  cnms <- sort(unique(as.numeric(unlist(lapply(pdfs, colnames)))))
  # new number of rows and columns
  nc <- length(cnms)

  lapply(pdfs, function(.x) {
    out <- matrix(0, nrow(.x), nc, FALSE, list(NULL, cnms))
    out[, match(as.numeric(colnames(.x)), cnms)] <- .x
    out
  })
}