
require(tools)
require(gsubfn)

files <- sprintf('../R/%s', c('evaluate_metrics.r',
                              'metrics-helpers.R',
                              'direct-metrics.R',
                              'derived-metrics.R',
                              'collection-derived-metrics.R',
                              'metrics.R',
                              'cranhelpers.R'))

for ( f in files ) {
  cat(f, '... ')
  source(f)
  cat('ok\n')
}


