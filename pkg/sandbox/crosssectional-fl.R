
library(metlab) # source('sourcepkg.R')


### Cross-sectional study of all packages (current versions, except
### Sweave) where FL is envolved; see
### http://www.statistik.lmu.de/~leisch/research/software.html.

pkgs <- c('flexclust', 'flexmix', 'archetypes', 'biclust',
          'gcExplorer', 'strucchange', 'e1071', 'mlbench',
          'modeltools', 'pixmap')

options(repos='http://cran.r-project.org')
dir <- crosssectional.study(pkgs, './fl') # reinit.destdir('./fl')
dir


### Calculate all direct metrics metlab provides:

met <- metrics(dir[3,3], list.direct.metrics())
