#' @title Show distr6 NEWS.md File
#' @description Displays the contents of the NEWS.md file for viewing distr6
#' release information.
#' @return NEWS.md in viewer.
#' @examples
#' \dontrun{
#' distr6News()
#' }
#' @export
# nocov start
distr6News <- function() {
  file.show(file.path(system.file(package = "distr6"), "NEWS.md"),
    title = "distr6 Changelog"
  )
}
# nocov end
