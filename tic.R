# installs dependencies, runs R CMD check, runs covr::codecov()
do_package_checks()

if (ci_on_ghactions() && ci_has_env("BUILD_PKGDOWN")) {
  
get_stage("before_deploy") %>%
  add_step(step_setup_push_deploy(branch = "master", orphan = FALSE))
  
  get_stage("deploy") %>%
    add_code_step(rmarkdown::render("README.Rmd")) %>%
    add_step(step_do_push_deploy(commit_paths = c("README.md")))
  
  # creates pkgdown site and pushes to gh-pages branch
  # only for the runner with the "BUILD_PKGDOWN" env var set
  do_pkgdown()
}
