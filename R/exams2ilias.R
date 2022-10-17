exams2ilias <- function(file, n = 1L, nsamp = NULL, dir = ".",
  name = NULL, quiet = TRUE, edir = NULL, tdir = NULL, sdir = NULL, verbose = FALSE,
  resolution = 100, width = 4, height = 4, svg = FALSE, encoding  = "UTF-8",
  num = list(fix_num = FALSE, minvalue = NA),
  mchoice = list(maxchars = c(3, NA, 3), minvalue = NA),
  schoice = mchoice, string = NULL, cloze = NULL,
  template = "ilias",
  duration = NULL, stitle = "Exercise", ititle = "Question",
  adescription = "Please solve the following exercises.",
  sdescription = "Please answer the following question.", 
  maxattempts = 1, cutvalue = 0, solutionswitch = TRUE, zip = TRUE,
  points = NULL, eval = list(partial = TRUE, negative = FALSE),
  converter = "pandoc-mathjax", xmlcollapse = TRUE,
  metasolution = FALSE, ...)
{
  ## assure a certain processing of items for ILIAS
  if(is.null(num)) {
    num <- list(fix_num = FALSE, minvalue = NA)
  } else {
    num$fix_num <- FALSE
    num$minvalue <- NA
  }
  if(is.null(mchoice)) {
    mchoice <- list(maxchars = c(3, NA, 3), minvalue = NA)
  } else {
    mchoice$maxchars <- c(3, NA, 3)
    mchoice$minvalue <- NA
  }
  if(is.null(schoice)) {
    schoice <- list(maxchars = c(3, NA, 3), minvalue = NA)  
  } else {
    schoice$maxchars <- c(3, NA, 3)
    schoice$minvalue <- NA
  }

  ## FIXME: default maxchars?

  ## default name
  if(is.null(name)) name <- gsub("\\.xml$", "", template)
  
  ## enforce base64 encoding for "everything"
  base64 <- .fileURI_mime_types[, "ext"]
  
  ## simply call exams2qti12 with custom template (and XML processing)
  rval <- exams2qti12(file = file, n = n, nsamp = nsamp, dir = dir,
    name = name, quiet = quiet, edir = edir, tdir = tdir, sdir = sdir, verbose = verbose,
    resolution = resolution, width = width, height = height, svg = svg, encoding  = encoding,
    num = num, mchoice = mchoice, schoice = schoice, string = string, cloze = cloze,
    template = template,
    duration = duration, stitle = stitle, ititle = ititle,
    adescription = adescription, sdescription = sdescription, 
    maxattempts = maxattempts, cutvalue = cutvalue, solutionswitch = solutionswitch, zip = zip,
    points = points, eval = eval, converter = converter, xmlcollapse = xmlcollapse,
    base64 = base64, flavor = "ilias", ...)

  ## fix-up zipping: base directory name must be the same as .xml name
  if(zip) {
    dir <- file_path_as_absolute(dir)
    zipfile <- file.path(dir, paste0(name, ".zip"))
    wdir <- getwd()
    dir.create(tdir <- tempfile())
    setwd(tdir)    
    on.exit(setwd(wdir))
    unzip(zipfile, exdir = name)
    file.rename(file.path(name, "qti.xml"), file.path(name, paste0(name, "_qti.xml")))

    ## Add qpl file.
    xml <- c(
      '<?xml version="1.0" encoding="utf-8"?>',
      '<!DOCTYPE Test SYSTEM "http://www.ilias.uni-koeln.de/download/dtd/ilias_co.dtd">',
      '<!--Export of ILIAS Test Questionpool 3029 of installation .-->',
      '<ContentObject Type="Questionpool_Test">',
      '<MetaData>',
      '<General Structure="Hierarchical">',
      paste0('<Identifier Catalog="ILIAS" Entry="', name, '_qpl"/>'),
      paste0('<Title Language="en">', name, '</Title>'),
      '<Language Language="en"/>',
      '<Description Language="en"/>',
      '<Keyword Language="en"/>',
      '</General>',
      '</MetaData>',
      '<Settings>',
      '<ShowTaxonomies>0</ShowTaxonomies>',
      '<NavTaxonomy>0</NavTaxonomy>',
      '<SkillService>0</SkillService>',
      '</Settings>'
    )
    for(i in 1:length(rval)) {
      for(j in 1:length(rval[[i]])) {
        xml <- c(xml,
          '<PageObject>',
          '<PageContent>',
          paste0('<Question QRef="', rval[[i]][[j]]$metainfo$id, '"/>'),
          '</PageContent>',
          '</PageObject>'
        )
      }
    }
    xml <- c(xml, '</ContentObject>')
    writeLines(xml, file.path(name, paste0(name, "_qpl.xml")))

    ## Add solution to xml as qtimetadata
    if(metasolution) solution_to_qtimetadata(name, rval)

    file.remove(zipfile)
    file.rename(name, paste0(name, "_qpl"))
    zip(zipfile = paste0(name, "_qpl.zip"), files = list.files())
    file.copy(paste0(name, "_qpl.zip"), file.path(dir, paste0(name, "_qpl.zip")))
  }

  ## return exams list
  invisible(rval)
}


# In order for an ILIAS test to recognize a solution to an essay question, the
# solution needs to be included in the xml as a qtimetadata tag.
solution_to_qtimetadata <- function(name, exm) {
  xml <- readLines(file.path(name, paste0(name, "_qti.xml")))
  
  for(i in seq_along(exm)) {
    ## Where to append qtimetadata?
    idx <- (grep("QUESTIONTYPE", xml) + 2)[i]

    ## Get solution, FIXME: exm[[i]] may have more than a single question
    solustr <- paste(exm[[i]][[1]]$solution, collapse="\n")

    ## Split into several solustr by ol/ul li items
    if(exm[[i]][[1]]$solution[1] %in% c("<ol>", "<ul>"))
      solustr <- gsub("</li>.*", "", strsplit(solustr, "<li>")[[1]])[-1]

    ## Encode solution
    solustr <- solustr_to_phpstruct(solustr, length(solustr))
    
    ## Append solution
    xml <- append(xml,
      c("<qtimetadatafield>",
        paste0("<fieldlabel>termscoring</fieldlabel><fieldentry>",
               solustr, "</fieldentry>"),
        "</qtimetadatafield>",
        "<qtimetadatafield>",
        "<fieldlabel>termrelation</fieldlabel><fieldentry>any</fieldentry>",
        "</qtimetadatafield>"),
      after = idx)
  }

  writeLines(xml, file.path(name, paste0(name, "_qti.xml")))
}


# And the solution needs to be wrapped in some php data structure,
# https://github.com/ILIAS-eLearning/ILIAS/blob/release_5-4/Modules/TestQuestionPool/classes/class.assAnswerMultipleResponseImage.php
# and be base encoded.
solustr_to_phpstruct <- function(solustr, nitems, encode = TRUE){
  items <- raw(0)
  for(i in seq_len(nitems)){
    items <- c(items,
      c(
        charToRaw(paste0(
          'i:', i - 1,
          ';O:31:"ASS_AnswerMultipleResponseImage":6:{',
          's:5:"image";s:0:"";s:16:"points_unchecked";s:1:"0";'
        )),
        charToRaw('s:13:"'), as.raw(0x00), charToRaw('*'), as.raw(0x00),
          charToRaw(paste0('answertext";s:', nchar(solustr[i]), ':"',
                           solustr[i], '";')),
        charToRaw('s:9:"'), as.raw(0x00), charToRaw('*'), as.raw(0x00),
          charToRaw('points";s:1:"1";'),
        charToRaw('s:8:"'), as.raw(0x00), charToRaw('*'), as.raw(0x00),
          charToRaw('order";i:0;'),
        charToRaw('s:5:"'), as.raw(0x00), charToRaw('*'), as.raw(0x00),
          charToRaw('id";i:-1;}')
      )
    )
  }
  rval <- c(charToRaw(paste0('a:', nitems, ':{')), items, charToRaw('}'))
  if(encode)
    rval <- base64enc::base64encode(rval)
  rval
}

