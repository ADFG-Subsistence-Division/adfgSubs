## R package update

# Define packages to check
required_packages <- c("devtools", "usethis", "roxygen2", "testthat", "covr")

# Check which packages are installed
installed_packages <- required_packages %in% rownames(installed.packages())

# Identify missing packages
missing_packages <- required_packages[!installed_packages]

# Install missing packages if needed
if (length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages)
}

# Load devtools for development functions
library(devtools)

# Make sure you're working from the most-up-to-date repository version. Pull changes before starting package edits.
#
# Implement updates (e.g., add functions to R/, modify existing functions, update documentation with roxygen2 comments)
#
# Generate documentation from roxygen comments
devtools::document()

# Use usethis to bump version
usethis::use_version("minor") # or "patch" or "major"

# Run your tests
devtools::test()

# Check package for CRAN compliance
devtools::check()

# Generate/update README
usethis::use_readme_md()

# Update NEWS file with changes
usethis::use_news_md()

# Commit and push changes; describe updates

# Create a Pull Request
# Go to your repository on GitHub
# Click "Compare & pull request"
# Describe your changes and submit the PR
# Request review from other contributors if need be

# After approval:
#
# Merge the PR into the main branch
# Create a release tag on GitHub
# Use usethis to create a GitHub release
usethis::use_github_release()
usethis::use_mit_license()
usethis::use_pkgdown()
pkgdown::build_site()
