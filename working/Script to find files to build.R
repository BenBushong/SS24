
require(here)
require(blogdown)

WEEKS = 1:4

buildfiles = data.frame(filesfull = list.files(here('content'), recursive=T, pattern='^[0-9].*.Rmd|index.Rmd', full.names = T),
                        filesshort = list.files(here('content'), recursive=T, pattern='^[0-9].*.Rmd|index.Rmd', full.names = F))
buildfiles$week = as.numeric(str_extract(buildfiles$filesshort, '([0-9]+)'))

tobuild = buildfiles$filesfull[buildfiles$week%in% WEEKS|grepl(buildfiles$filesfull, pattern='index')]

tobuild = grep(tobuild, pattern='NOTUSED', invert=T, value=T)
tobuild = grep(tobuild, pattern='index', invert=T, value=T)
tobuild.indexes = grep(buildfiles$filesfull, value=T, pattern='index')

blogdown::build_site(local=T, build_rmd = tobuild)
blogdown::build_site(local=T, build_rmd = tobuild.indexes)
blogdown::build_site(local=T, build_rmd = "C:/Users/jkirk/OneDrive - Michigan State University/Teaching/SSC442_SS21/content/example/04-example.Rmd")


