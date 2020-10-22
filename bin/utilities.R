"
Utility functions
"
require(tidyverse)

# Preview pipe for tidyverse
hView <- function(x, n=100) {return(head(x, n) %>% View())}

# Taxonomy vector
taxonomy.levels <- c('kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species')

# Theme visualizer
colors.plot <- function(a) image(1:length(a), 1, as.matrix(1:length(a)), col=a, axes=FALSE , xlab="", ylab="")

# Taxonomy labelling
parse_taxonomy = function(name) {
  unlist(
    lapply(name, function(x) {
      parse(text = 
              paste0('italic("', 
                     str_replace(string = x, 
                                 pattern = '[a-z].*[_ ]', 
                                 replacement = '. '), 
                     '")')
            )
    }
    )) %>% 
    return()
}

percent_format_signif <-
  function(x, label.precision=2, label.percent=T) {
    if (label.percent == T) {
      paste0(signif(100 * x, label.precision), "%")
    } else {
      signif(digits = label.precision)
    }
  }