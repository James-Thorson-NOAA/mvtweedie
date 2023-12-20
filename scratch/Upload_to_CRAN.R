
library(devtools)
setwd( R'(C:\Users\James.Thorson\Desktop\Git\mvtweedie)' )

# Compile
if( FALSE ){
  document()
}

# Test install
install_local(force=TRUE, dep=TRUE, build_vignettes=TRUE, upgrade=FALSE)
#install_local(force=TRUE, dep=TRUE, build_vignettes=FALSE, upgrade=FALSE)
browseVignettes("phylosem")

#
if( FALSE ){
  library(TMB)
  setwd( R'(C:\Users\James.Thorson\Desktop\Git\mvtweedie\src)' )
  compile("phylosem.cpp")
}

# Try building vignetttes
if( FALSE ){
  library(rmarkdown)
  setwd( R'(C:\Users\James.Thorson\Desktop\Git\mvtweedie\vignettes)' )
  devtools::build_rmd("vignettes/Introduction.Rmd")
  render( file.path(getwd(),"Introduction.Rmd"), pdf_document())
}

# Try mapping dependencies
if( FALSE ){
  # Simple way
  library(renv)
  x = dependencies()

  # Listed dependencies
  tools::package_dependencies("mvtweedie")

  # All
  pack <- available.packages()
  pack["dynlm","Depends"]
  packrat:::recursivePackageDependencies("dynlm", ignore = "", lib.loc = .libPaths()[1], fields="Imports")
}

# Run checks ... doesn't seem to work
file.remove( file.path("vignettes","Introduction.pdf") )
#check( remote = TRUE, manual=TRUE )
check( manual=TRUE )

# Check manual
if( FALSE ){
  tools::texi2pdf
}

# Check online but document first!
document()
check_win_devel()

# Submit to CRAN via devtools .. not preferred!
if( FALSE ){
  file.remove( file.path("vignettes","Introduction.pdf") )
  release()
}

# Build for uploading via web interface
save_dir = file.path( getwd(), "scratch")
# https://cran.r-project.org/submit.html
build( path=save_dir, manual=TRUE )
# REMEMBER TO UPDATE NEWS.RMD

