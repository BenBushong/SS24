
require(here)
require(blogdown)

WEEKS = 1:4

buildfiles = data.frame(filesfull = list.files(here('content'), recursive=T, pattern='^[0-9].*.Rmd', full.names = T),
                        filesshort = list.files(here('content'), recursive=T, pattern='^[0-9].*.Rmd', full.names = F))
buildfiles$week = as.numeric(str_extract(buildfiles$filesshort, '([0-9]+)'))

tobuild = buildfiles$filesfull[buildfiles$week%in% WEEKS]

tobuild = grep(tobuild, pattern='NOTUSED', invert=T, value=T)


blogdown::build_site(local=T, build_rmd = tobuild)
blogdown::build_site(local=T, build_rmd = "C:/Users/jkirk/OneDrive - Michigan State University/Teaching/SSC442_SS21/content/example/04-example.Rmd")


