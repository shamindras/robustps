# Let's load the required packages
# You should install these packages individually if they are not already
# installed
library(tidyverse)
library(magrittr)
library(here)
library(roxygen2)
library(testthat)
library(devtools)
library(pkgdown)
library(usethis)

# Let's examine `usethis`
ls("package:usethis") %>% base::length()
# There are 79 functions. Quite a few. Let's examine them sorted by name
# to get a feel for which ones will be useful

usethis_fns <- ls("package:usethis") %>% base::sort()
utils::head(usethis_fns, 10)
# Nice! They seem to have a consistent naming convention {verb}_{description}
# e.g. use_readme_md or create_package
usethis_fns[usethis_fns %>% str_detect(string = ., pattern = "proj")]

# Let's just examine the frequency of the verb prefixes
usethis_fns_freq <- ls("package:usethis") %>%
                        stringr::str_split(string = ., pattern = "_") %>%
                        purrr::map(~.x[[1]]) %>%
                        base::unlist() %>%
                        base::table() %>%
                        base::sort(decreasing = TRUE)
usethis_fns_freq

# So the main {verb} is "use" and then "edit", "browse" etc

# Great so we are dealing with a library that has an consistent and easy
# to understand naming convention for it's functions. Let's start using them
# to create a package

# Create a new package in the neuroada folder
usethis::create_package(path = here::here())

# Set up other files -------------------------------------------------
usethis::use_readme_md()
#> ✔ Writing 'README.md'
#> ● Edit 'README.md'

# In RStudio, this opens up the README.md file which is already
# filled in with the devtools installation guide for `robustps` - nice!

# Use MIT License - modify the description
usethis::use_mit_license(name = c("Pratik Patil", "Shamindra Shrotriya"))
#> ✔ Setting License field in DESCRIPTION to 'MIT + file LICENSE'
#> ✔ Writing 'LICENSE.md'
#> ✔ Adding '^LICENSE\\.md$' to '.Rbuildignore'
#> ✔ Writing 'LICENSE'

# Load external packages to be used in neuroada
usethis::use_package("tidyverse")
# ✔ Adding 'tidyverse' to Imports field in DESCRIPTION
# ● Refer to functions with `tidyverse::fun()`
usethis::use_package("magrittr")
# ✔ Adding 'magrittr' to Imports field in DESCRIPTION
# ● Refer to functions with `magrittr::fun()`

# Use Roxygen
usethis::use_roxygen_md()
# ✔ Setting Roxygen field in DESCRIPTION to 'list(markdown = TRUE)'
# ✔ Setting RoxygenNote field in DESCRIPTION to '6.0.1'
# ● Re-document

# Load the magrittr pipe locally
usethis::use_pipe()
# ✔ Setting Roxygen field in DESCRIPTION to 'list(markdown = TRUE)'
# ✔ Setting RoxygenNote field in DESCRIPTION to '6.0.1'
# ● Re-document

#  imports a standard set of helpers to facilitate programming with
# the tidy eval toolkit.
usethis::use_tidy_eval()
# ✔ Adding 'rlang' to Imports field in DESCRIPTION
# ✔ Writing 'R/utils-tidy-eval.R'
# ● Run document()

# We want to write unit tests effectively and the preferred way is using
# the `testthat` package
usethis::use_testthat()
# ✔ Adding 'testthat' to Suggests field in DESCRIPTION
# ✔ Creating 'tests/testthat/'
# ✔ Writing 'tests/testthat.R'

# Update the news markdown file for all of your fancy package updates
usethis::use_news_md()
#> ✔ Writing 'NEWS.md'
#> ● Edit 'NEWS.md'

# Let's update to use travis
# It will interactively take you to the travis page for the `neroada` repo
# so that you can activate it - very cool.
# I've temporarily deactivated it, but the travis yml files are created
# which is great!
usethis::use_travis()
# ✔ Writing '.travis.yml'
# ✔ Adding '^\\.travis\\.yml$' to '.Rbuildignore'
# ● Turn on travis for your repo at https://travis-ci.org/profile/shamindras
# ● Add a Travis build status badge by adding the following line to your README:
#     Copying code to clipboard:
#     [![Travis build status](https://travis-ci.org/shamindras/robustps.svg?branch=master)](https://travis-ci.org/shamindras/robustps)

# We may want to export this to a website later. So just setup pkgdown now
usethis::use_pkgdown()
# ● Modify '_pkgdown.yml'
# ✔ Adding '^_pkgdown\\.yml$' to '.Rbuildignore'
# ✔ Creating 'doc/'
# ✔ Adding '^doc$' to '.Rbuildignore'

pkgdown::build_site()
# ══ Building pkgdown site ═══════════════════════════════════════════════════════
# Reading from: '/Users/shamindras/PERSONAL/LEARNING/REPOS/robustps'
# Writing to:   '/Users/shamindras/PERSONAL/LEARNING/REPOS/robustps/docs'
# ── Initialising site ───────────────────────────────────────────────────────────
# Copying '../../../../../../Library/Frameworks/R.framework/Versions/3.5/Resources/library/pkgdown/assets/docsearch.css' to 'docsearch.css'
# Copying '../../../../../../Library/Frameworks/R.framework/Versions/3.5/Resources/library/pkgdown/assets/docsearch.js' to 'docsearch.js'
# Copying '../../../../../../Library/Frameworks/R.framework/Versions/3.5/Resources/library/pkgdown/assets/link.svg' to 'link.svg'
# Copying '../../../../../../Library/Frameworks/R.framework/Versions/3.5/Resources/library/pkgdown/assets/pkgdown.css' to 'pkgdown.css'
# Copying '../../../../../../Library/Frameworks/R.framework/Versions/3.5/Resources/library/pkgdown/assets/pkgdown.js' to 'pkgdown.js'
# ── Building home ───────────────────────────────────────────────────────────────
# Writing 'authors.html'
# Reading 'LICENSE.md'
# Writing 'LICENSE.html'
# Writing 'LICENSE-text.html'
# Writing 'index.html'
# ── Building function reference ─────────────────────────────────────────────────
# Updating robustps documentation
# Loading robustps
# Writing NAMESPACE
# Writing pipe.Rd
# Writing tidyeval.Rd
# Writing 'reference/index.html'
# Loading robustps
# ── Building news ───────────────────────────────────────────────────────────────
# Writing 'news/index.html'
# ══ DONE ════════════════════════════════════════════════════════════════════════
# ── Previewing site ─────────────────────────────────────────────────────────────

# Let's add a gitignore file
