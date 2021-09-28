
## CREATING TESTS
library(shinytest)
recordTest("inst/shiny") # Create new test (then can edit)

# MUST re-build package before testing or shinytests use wrong version
devtools::build() %>%
  install.packages(repos = NULL, type = "source")

# Test via shinytest (if adding testthat tests, can use devtools::test())
shinytest::testApp("inst/shiny/") #all
shinytest::testApp("inst/shiny/", "test_full_simple.R")

# Problems? check, fix, or resolve file-by-file
shinytest::snapshotCompare("inst/shiny", "test_full_simple")




## DOCUMENTATION

# Update NEWS

# Check spelling
dict <- hunspell::dictionary('en_CA')
devtools::spell_check()
spelling::update_wordlist()

# Update README.Rmd
# Compile README.md
# REBUILD!
devtools::build_readme()

# Check/update URLS
urlchecker::url_check()

## CHECKS

# MUST BUILD PACKAGE FIRST (shinytests will be weird otherwise)
devtools::build() %>%
  install.packages(repos = NULL, type = "source")

devtools::check()


