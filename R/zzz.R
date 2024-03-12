
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This is version ", utils::packageVersion(pkgname),
                        " of ", pkgname, ". All functions now use two-tailed hypothesis tests by default instead of one-tailed.\nOne-tailed tests can be manually specified through the new 'tailed' and 'greater' arguments.\nPlease consult the help files and vignette for more information.")
}
