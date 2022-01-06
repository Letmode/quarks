.onAttach <- function(libname, pkgname){
  packageStartupMessage("********************************************************************************")
  packageStartupMessage("                        Welcome to the package 'quarks'!")
  packageStartupMessage("********************************************************************************")
  packageStartupMessage("")
  packageStartupMessage("Please report any possible errors and bugs to sebastian.letmathe@uni-paderborn.de.")
  packageStartupMessage("Thank you.")
  packageStartupMessage("")
  packageStartupMessage("********************************************************************************")
}

.onUnload <- function(libpath) {
  library.dynam.unload("quarks", libpath)
}
