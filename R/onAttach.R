
.onAttach <- function(libname, pkgname){
  unlockBinding(".assets", getNamespace(pkgname))
  unlockBinding(".indicators_fun", getNamespace(pkgname))
  unlockBinding(".indicators", getNamespace(pkgname))
  unlockBinding(".strategy", getNamespace(pkgname))
}
