
## CREATING TESTS
library(magrittr)
#shinytest::recordTest("inst/shiny") # Create new test (then can edit)

# MUST re-build package before testing or shinytests use wrong version
devtools::build() %>%
  install.packages(repos = NULL, type = "source")

# Test via shinytest (if adding testthat tests, can use devtools::test())
shinytest::testApp("inst/shiny/") #all
shinytest::testApp("inst/shiny/", "test_999_full_mayo.R")

# Problems? check, fix, or resolve file-by-file
shinytest::snapshotCompare("inst/shiny", "test_05_pi_explore")

# Text diffs only
#shinytest::viewTestDiff("inst/shiny", "test_05_pi_explore", interactive = FALSE)

# Note:
# There are no snapshots for apps after the PI explore Map has been created
# This is because it creates a unique svg id every time AND because the map
# svg is so huge it freezes up the snapshotCompare(). Therefore the app runs through
# these options but doesn't take snapshots... something to report to shinytest
# or ggiraph?


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

devtools::document()
devtools::check()

devtools::install()

