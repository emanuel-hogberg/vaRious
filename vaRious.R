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
#          'a=bil_§', 'b=§,', 'c=123_§_korv', names=c('a', 'b', 'korv'))
str_extract_crunch <- function(s, ..., names, verbose=FALSE, wildcard = '§', fix_pattern = TRUE){
  v <- list(...)
  
  stopifnot(is.character(s))
  
  if (verbose) {
    print(paste0("v: ", v))
    print(paste0("wildcard: ", wildcard))
  }
    
  crunch <- function(ls, rest) {
    sought <- ls[1]
    
    corrected <- 0
    if (fix_pattern) {
      s0 <- str_length(sought)
      sought <- sought %>% str_replace("\\(", "\\\\(") %>% str_replace("\\)", "\\\\)") %>% str_replace("\\.", "\\\\.")
      corrected <- (str_length(sought) - s0) / 2
    }
    
    if (verbose){
      print(paste0("Crunching '", rest, "', finding '", sought, "', corrected ", corrected))
    }
    
    wildcardPosition <- str_locate(sought, wildcard)[1,1]
    remainderAfterWildcard <- substring(sought, wildcardPosition + 1)
    
    sought <- sought %>% str_replace(wildcard, '.+')
    
    if (verbose){
      print(paste0("sought is now ", sought))
    }
    
    if (is.na(remainderAfterWildcard) || str_length(remainderAfterWildcard) == 0) {
      # No additional search strings means remainder should be rest of str.
      # Otherwise, use next search word as remainder:
      if (length(ls) > 1){
        remainderAfterWildcard <- substring(ls[[2]], 0, str_locate(ls[[2]], wildcard)[1,1] - 1)
      }
    }
    
    strMatchPos <- str_locate(rest, sought)[1,1]
    strMatch <- substring(rest, strMatchPos) # str_extract(rest, sought)
    wildCardFoundUntrimmed <- substring(strMatch, wildcardPosition - corrected)
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
  names(l) <- c(names)
  
  if (verbose) {
    list(l = l)
  } else{
    l
  }
}
#str_extract_crunch('ok a=bil_1, b=nånting, c=123_532_korv',
#                   'a=bil_§', 'b=§,', 'c=123_§_korv', names=c('a', 'b', 'korv'))


prep <- function(data) {
  cols <- names(data)
  for(c in cols){
    if (is.character(data[[1, c]]))
      print("hej")
  }
  data
}


stringcols_to_hotencoded <- function(data, verbose = FALSE, colcount_threshold = 20) {
  data.new <- tibble(.rows = nrow(data))
  data.m <- tibble(name = names(data),
                   ischar = data %>% sapply(is.character))
  
  # r <- data.m[111,]
  for (i in 1:nrow(data.m)) {
    r <- data.m[i, ]
    
    if (!is.atomic(r)) {
      
      if (r$ischar) {
        lvls <- levels(factor(data[[r$name]]))
        # lvl <- lvls[1]
        if (length(lvls) <= colcount_threshold){
          for (lvl in lvls)
          {
            data.new[[paste(r$name, lvl, sep = "_")]] <- map(data[[r$name]], ~ if_else(. == lvl, 1, 0)) %>% .[[1]]
          }
        } else{
          data.new[[r$name]] <- data[[r$name]]
        }
      } else {
        data.new[[r$name]] <- data[[r$name]]
      }
    }
  }
  
  if (verbose){
    return(list(data.new = data.new, data.m = data.m))
  }else{
    return(data.new)
  }
}
