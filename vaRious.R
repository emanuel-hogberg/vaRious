library(tidyverse)
library(readxl)

# Load all csv and xls(x) files from a directory.
load_dir <- function(path, sep = ";") {
  files <- dir(path)
  r <- list()
  ns <- list()
  for (file in files) {
    p <- paste(path, file, sep = "\\")
    spl <- str_split(file, "\\.")[[1]]
    ext <- spl[-1] # \\. matchar mot punkter, annars blir det regex-viklen-bokstav-som-helst
    name <- str_replace(str_sub(file, 1, -str_length(ext) - 2), " ", "\\_")
    if (!is.na(ext) && str_length(paste0('', ext)) > 0) {
      if (ext == "xlsx" || ext == "xls")
        data <- read_excel(p)
      if (ext == "csv")
        data <- read_delim(p, delim = sep)
      r <- append(r, list(data))
      ns <- append(ns, name)
    }
  }
  names(r) <- ns
}


# Create line ggplot from a 1-dimensional vector.
quick_line <- function(v) {
  
  tibble(o = 1:length(v), p = v) %>%
    ggplot(aes(x=o,y = p)) + geom_line()
}

# Create line ggplot from any number of 1D vectors/lists
# example: quick_lines(6:1, 3:8)
quick_lines <- function(...) {
  v <- list(...)
  l <- length(v[[1]])
  if (is.list(v[[1]]))
    l <- length(v[[1]][[1]])
  t <- tibble(o = 1:l)
  gs <- list()
  for (i in 1:length(v)) {
    p <- v[[i]]
    if (is.list(p))
      p <- p[[1]]
    if (length(p) != l)
      stop(paste0('Error: element ', length(p), ' does not have length ', l))
    t <- bind_cols(t, s = p)
    names(t) <- c('o', paste0('l', as.character(1:i))) # paste0('`', 1:i, '`'))
    # name <- names(t)[i+1]
    gs <- append(gs, geom_line(aes_string(y=paste0('l', as.character(i)))))
  }
  append(list(ggplot(t, aes(x=o))), gs) %>% reduce(`+`)
}
# quick_lines(6:1, 3:8)

str_as_tibble <- function(ls, sep = "\n", colName = "s") {
  t <- ls %>%
    str_split(sep)
  names(t) = c(colName)
  as_tibble(t)
}

# Given string s, search for ... one at a time in order.
# Search pattern in ... is either regex or use § as a wildcard.
# Example: str_extract_crunch('ok a=bil_1, b=nånting, c=123_532_korv',
#          'a=bil_§', 'b=§,', 'c=123_§_korv', .names=c('a', 'b', 'korv'))
str_extract_crunch <- function(s, ..., names, verbose=FALSE){
  v <- list(...)
  
  if (verbose)
    print(paste0("v: ", v))
  
  crunch <- function(ls, rest) {
    
    sought <- ls[1] %>% str_replace('§', '.+')
    
    if (verbose){
      print(paste0("Crunching '", rest, "', finding '", sought, "'"))
    }
    
    wildcardPosition <- str_locate(ls[1], '§')[1,1]
    remainderAfterWildcard <- substring(ls[1], wildcardPosition + 1)
    
    if (is.na(remainderAfterWildcard) || str_length(remainderAfterWildcard) == 0) {
      # No additional search strings means remainder should be rest of str.
      # Otherwise, use next search word as remainder:
      if (length(ls) > 1){
        remainderAfterWildcard <- substring(ls[[2]], 0, str_locate(ls[[2]], '§')[1,1] - 1)
      }
    }
    
    strMatchPos <- str_locate(rest, sought)[1,1]
    strMatch <- substring(rest, strMatchPos) # str_extract(rest, sought)
    wildCardFoundUntrimmed <- substring(strMatch, wildcardPosition)
    wildCardFound <- substring(wildCardFoundUntrimmed, 0, str_locate(wildCardFoundUntrimmed, remainderAfterWildcard)[1,1] - 1)
    
    if ((is.na(wildCardFound) || str_length(wildCardFound) == 0) & length(ls) == 1)
      wildCardFound <- wildCardFoundUntrimmed
    
    newRest = substring(rest, -3 + strMatchPos + wildcardPosition + str_length(wildCardFound) + str_length(remainderAfterWildcard))
    
    
    if (verbose)
      print(paste0(
      "length(ls)='", length(ls),
      "', firstSought='", sought, 
      "', wildcardPosition='", wildcardPosition,
      "', remainderAfterWildcard='", remainderAfterWildcard, 
      "', strMatchPos='", strMatchPos,
      "', strMatch='", strMatch, 
      "', wildCardFoundUntrimmed='", wildCardFoundUntrimmed, 
      "', wildCardFound='", wildCardFound,
      "', newRest='", newRest, "'"
      ))
    
    if (length(ls) == 1) {
      list(c(wildCardFound))
    }
    else {
      flatten(list(c(wildCardFound, crunch(ls[2:length(ls)], newRest))))
    }
  }
  
  l <- crunch(v, s)
  names(l) <- names
  
  if (verbose) {
    list(names_l = names(l), dotNames = names, l = l)
  } else{
    l
  }
}
#str_extract_crunch('ok a=bil_1, b=nånting, c=123_532_korv',
#                   'a=bil_§', 'b=§,', 'c=123_§_korv', .names=c('a', 'b', 'korv'))


