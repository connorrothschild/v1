# compiles all .Rmd files in _R directory into .md files in Pages directory,
# if the input file is older than the output file.

# run ./knitpages.R to update all knitr files that need to be updated.

KnitPost <- function(input, outfile, figsfolder, cachefolder, base.url="/") {
  # this function is from David Robinson, and it is a modified version of an example here:
  # http://jfisher-usgs.github.com/r/2012/07/03/knitr-jekyll/
  require(knitr);
  opts_knit$set(base.url = base.url)
  fig.path <- paste0(figsfolder, sub(".Rmd$", "", basename(input)), "/")
  cache.path <- file.path(cachefolder, sub(".Rmd$", "", basename(input)), "/")
  
  opts_chunk$set(fig.path = fig.path)
  opts_chunk$set(cache.path = cache.path)
  opts_chunk$set(fig.cap = "center")
  opts_chunk$set(dpi = 300)
  opts_chunk$set(fig.width = 12) 
  opts_chunk$set(fig.height = 8)
  render_jekyll()
  # render_html()
  knit(input, outfile, envir = parent.frame())
}

knit_folder <- function(infolder, outfolder, figsfolder, cachefolder, knit_all = FALSE) {
  for (infile in list.files(infolder, pattern = "*.Rmd", full.names = TRUE)) {
    pattern = "\\d\\d\\d\\d\\-\\d\\d\\-\\d\\d\\-"
    print(infile)
    # folder = ifelse(grepl(pattern, infile), outfolder, "pages")
    outfile = paste0(outfolder, "/", sub(".Rmd$", ".md", basename(infile)))
    print(outfile)
    
    # knit only if the input file is the last one modified
    if (knit_all == TRUE) {
      KnitPost(infile, outfile, figsfolder, cachefolder)
    } 
    if (!file.exists(outfile) |
        file.info(infile)$mtime > file.info(outfile)$mtime)
      KnitPost(infile, outfile, figsfolder, cachefolder)
    }
  }

knit_folder("_R", "_posts", "figs/", "_caches/", knit_all = FALSE)
#knit_folder("_R/drafts", "_drafts", "figs/drafts/")