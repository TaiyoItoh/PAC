library(tcltk2)
library(MASS)

source("./R/input.R")
source("./R/compare.R")
source("./R/analyze.R")
source("./R/output.R")

# item.min needs to be more than 2
# clustering.method needs to be "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", or "centroid"
# mds.method needs to be "isoMDS" or "sammon" (which are for non-metric), not "cmdscale" (which is for metric)
# "metaMDS" in library(vegan) and other methods are available for non-metric MDS

item.min <- 3
item.max <- 15
stimulus <- "Stimulus."
instruction <- "Instruction."
clustering.method <- "ward.D"
mds.method <- "isoMDS"

if (item.min < 2) {
  tkmessageBox(message = "The variable 'item.min' needs to be more than 2.", icon = "error", type = "ok")
} else {
  input.data <- input(item.min, item.max, stimulus)
  if (!is.null(input.data)) {
    compare.result <- compare(input.data, instruction)
      if (!is.null(compare.result)) {
      analyze.result <- analyze(compare.result, clustering.method, mds.method)
      output(compare.result, clustering.method, mds.method, analyze.result)
    }
  }
}
