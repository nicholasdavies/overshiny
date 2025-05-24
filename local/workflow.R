# INITIAL SETUP

# Readme and docs
usethis::use_readme_md() # or rmd
usethis::use_news_md()
usethis::use_roxygen_md()
usethis::use_package_doc()
usethis::use_mit_license()
devtools::document() # delete NAMESPACE before first time

# Set up git
usethis::git_vaccinate()
usethis::use_git()
usethis::use_github()
usethis::use_github_action()

# Web site
usethis::use_pkgdown_github_pages()

# Add 'local' directory
dir.create("./local")
usethis::write_union("./.Rbuildignore", "^local$")


# ADDING ELEMENTS
# Import package
usethis::use_package("")

# Add vignette
usethis::use_vignette("overshiny")


# BUILD CYCLE
devtools::document()
devtools::build_vignettes()

devtools::build(vignettes = TRUE)
devtools::install(build_vignettes = TRUE)

# RELEASE CYCLE


# What else is missing from here?
# Rcpp, testthat
