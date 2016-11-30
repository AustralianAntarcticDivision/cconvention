## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library(stringi)
library(Combin8R)


## ------------------------------------------------------------------------
## using this strange name for now because sp::as.character.DMS clashes otherwise
parserDMS1 <-
  Combin8R:::pRegex("DMS1",
              "^\\s*(\\d\\d?\\d?)°(?:(\\d\\d?)'(?:(\\d\\d?)\")?)?(E|W|N|S)")

## ------------------------------------------------------------------------
parserDMS1("57°S")
parserDMS1("157°20'E")
parserDMS1("57°2'14\"N")

## ------------------------------------------------------------------------
parserDMS1("some text")
parserDMS1("57.33S")

## ------------------------------------------------------------------------
## all in Combin8R
# formatAST <- function(x,indent=0) {
#   UseMethod("formatAST")
# }
# formatAST.default <- function(x,indent=0) {
#   pad <- paste(rep(" ",indent),collapse="")
#   paste(pad,deparse(x),sep="")
# }
# formatAST.pRegex <- function(x,indent=0) {
#   pad <- paste(rep(" ",indent),collapse="")
#   paste(pad,"[",class(x)[1],"; ",paste(x$value,collapse=" "),"]\n",sep="")
# }

## ------------------------------------------------------------------------
p <- parserDMS1("57°20'14\"N")
cat(formatAST(p$result))

## ------------------------------------------------------------------------
formatAST.DMS1 <- function(x,indent=0) {
  pad <- paste(rep(" ",indent),collapse="")
  paste0(pad,
         x$value[1],"°",
         if(!is.na(x$value[2])) 
           paste0(x$value[2],"'",
                  if(!is.na(x$value[3])) 
                    paste0(x$value[3],"\"")),
         x$value[4])
}

## ------------------------------------------------------------------------
cat(formatAST(p$result))

## ------------------------------------------------------------------------
## Combin8R:::pAlt
# parserAlt <- function(tag,...) {
#   ps <- list(...)
#   function(input) {
#     ## Try each parser in turn
#     for(p in ps) {
#       ## Return first successful parse
#       if(!is.null(parse <- p(input)))
#         return(list(input=parse$input,
#                     result=structure(list(value=parse$result),class=c(tag,"pAlt"))))
#     }
#     ## All parses failed
#     NULL
#   }
# }
formatAST.pAlt <- function(x,indent=0) {
  pad <- paste(rep(" ",indent),collapse="")
  paste(pad,"||",class(x)[1],"; ",formatAST(x$value),sep="")
}

## ------------------------------------------------------------------------
## Combin8R:::pSeq
# parserSeq <- function(tag,...) {
#   ps <- list(...)
#   function(input) {
#     ## Try each parser in sequence, accumulating parse results
#     values <- vector(mode="list",length(ps))
#     for(k in seq_along(ps)) {
#       parse <- ps[[k]](input)
#       ## If any match fails, the sequence fails
#       if(is.null(parse)) return(NULL)
#       values[[k]] <- parse$result
#       input <- parse$input
#     }
#     list(input=input,
#          result=structure(list(value=values),class=c(tag,"pSeq")))
#   }
# }
formatAST.pSeq <- function(x,indent=0) {
  pad <- paste(rep(" ",indent),collapse="")
  paste(pad,"(",class(x)[1],";\n",
        paste(sapply(x$value,formatAST,indent=indent+2),collapse=""),
        pad,")",sep="",collapse="")
}

## ------------------------------------------------------------------------
## Combin8R:::pMany
# parserMany <- function(tag,p) {
#   function(input) {
#     k <- 0
#     values <- list()
#     ## Repeatedly try parser, accumulating parse results
#     repeat {
#       parse <- p(input)
#       ## Finish when a parse fails
#       if(is.null(parse)) break
#       values[[k <- k+1]] <- parse$result
#       input <- parse$input
#     }
#     list(input=input,
#          result=structure(list(value=values),class=c(tag,"pMany")))
#   }
# }
formatAST.pMany <- function(x,indent=0) {
  pad <- paste(rep(" ",indent),collapse="")
  paste(pad,"*{",class(x)[1],";\n",
        paste(sapply(x$value,formatAST,indent=indent+2),collapse=""),
        pad,"}",sep="")
}

## ------------------------------------------------------------------------
##Combin8R:::pSome
# parserSome <- function(tag,p) {
#   function(input) {
#     parse <- p(input)
#     if(is.null(parse)) return(NULL)
#     k <- 1
#     values <- list(parse$result)
#     ## Continue trying parser, accumulating parse results
#     repeat {
#       parse <- p(input)
#       ## Finish when a parse fails
#       if(is.null(parse)) break
#       values[[k <- k+1]] <- parse$result
#       input <- parse$input
#     }
#     list(input=input,
#          result=structure(list(value=values),class=c(tag,"pSome")))
#   }
# }
formatAST.pSome <- function(x,indent=0) {
  pad <- paste(rep(" ",indent),collapse="")
  paste(pad,"+{",class(x)[1],";\n",
      paste(sapply(x$value,formatAST,indent=indent+2),collapse=""),
      pad,"}",sep="")
}

## ------------------------------------------------------------------------
parserStartPath0 <-
  Combin8R::pSeq("StartPath",
           Combin8R:::pRegex("StartText","^The waters bounded by a line starting at"),
           parserDMS1,
           parserDMS1,
           Combin8R:::pRegex("Semi","^;\\s*"))

## ------------------------------------------------------------------------
txt <- "The waters bounded by a line starting at 57°S 50°W; thence due east to 30°W longitude; thence due south to 64°S latitude; thence due west to 50°W longitude; thence due north to the starting point."
r <- parserStartPath0(txt)
r$input
cat(formatAST(r$result))

## ------------------------------------------------------------------------
## The waters bounded by a line starting at POINT
parserPathStart <-
  Combin8R:::pRegex("PathStart",
               "^\\s*The waters bounded by a line starting at (\\d\\d?)°(?:(\\d\\d?)'(?:(\\d\\d?)\")?)?(N|S) (\\d\\d?\\d?)°(?:(\\d\\d?)'(?:(\\d\\d?)\")?)?(E|W);\\s*")
## thence due DIRECTION to (PARALLEL|MERIDIAN)
parserPathLineLL <-
  Combin8R:::pRegex("PathLineLL",
              "^\\s*thence due (\\w+) to (\\d\\d?\\d?)°(?:(\\d\\d?)'(?:(\\d\\d?)\")?)?(N|S|E|W) (latitude|longitude);\\s*")
# thence DIRECTION to the point POINT
# thence to POINT 
parserPathLinePt <-
  Combin8R:::pRegex("PathLinePt",
              "^\\s*thence (?:(\\w+) )?to (?:the point )?(\\d\\d?)°(?:(\\d\\d?)'(?:(\\d\\d?)\")?)?(N|S), (\\d\\d?\\d?)°(?:(\\d\\d?)'(?:(\\d\\d?)\")?)?(E|W);\\s*")
## thence due DIRECTION to the starting point.
parserPathEnd <-
  Combin8R:::pRegex("PathEnd",
              "^\\s*thence due (\\w+) to the starting point.\\s*")

## ------------------------------------------------------------------------
parserPath <- 
  pSeq("Path",
            parserPathStart,
            pSome("PathLineList",
                       pAlt("parserPathLine",
                                 parserPathLineLL,
                                 parserPathLinePt)),
            parserPathEnd)

## ------------------------------------------------------------------------
r <- parserPath(txt)
cat(formatAST(r$result))

## ------------------------------------------------------------------------
txt <- "The waters bounded by a line starting at 45°S 60°E; thence due south to 53°14'S latitude; thence east to the point 53°14'07\"S, 67°03'20\"E; thence to 52°42'28\"S, 68°05'31\"E; thence to 51°58'18\"S, 69°44'02\"E; thence to 51°24'32\"S, 71°12'29\"E; thence to 51°03'09\"S, 72°28'28\"E; thence to 50°54'23\"S, 72°49'21\"E; thence to 49°49'34\"S, 75°36'08\"E; thence to 49°24'07\"S, 76°42'17\"E; thence due east to 80°E longitude; thence due north to 45°S latitude; thence due west to the starting point."
r1 <- parserPath(txt)
cat(formatAST(r1$result))

## ------------------------------------------------------------------------
library(stringi)
lexerRegexp <- function(tag,regex)
  function(input) {
    match <- stri_match_first_regex(input,regex)
    ## If the regex matched, consume input and return any captured
    ## groups as values
    if(!is.na(match[1,1]))
      list(input=substr(input,nchar(match[1,1])+1,nchar(input)),
           token=structure(list(values=match[1,-1]),class=c(tag,"lRegex")))
  }
print.lRegex <- function(x,...) {
  cat(class(x)[1],":",x$values)
}
formatAST.lRegex <- function(x,indent=0) {
  pad <- paste(rep(" ",indent),collapse="")
  paste(pad,"[",class(x)[1],"; ",paste(x$value,collapse=" "),"]\n",sep="")
}

## ------------------------------------------------------------------------
makeLexer <- function(...) {
  ls <- list(...)
  tokens <- list()
  
  function(input) {
    k <- 0
    repeat {
      ## Return if input consumed
      if(nchar(input)==0) return(list(input=input,tokens=tokens))
      ## Call lexers
      for(l in ls)
        if(!is.null(lex <- l(input))) break
      ## If all failed return partial result
      if(is.null(lex)) return(list(input=input,tokens=tokens))
      ## Else store token and consume input
      tokens[[k <- k+1]] <- lex$token
      input <- lex$input
    }
  }
}

## ------------------------------------------------------------------------
lexer <- makeLexer(
  ## The waters bounded by a line starting at POINT
  lexerRegexp("PathStart",
              "^\\s*The waters bounded by a line starting at (\\d\\d?)°(?:(\\d\\d?)'(?:(\\d\\d?)\")?)?(N|S) (\\d\\d?\\d?)°(?:(\\d\\d?)'(?:(\\d\\d?)\")?)?(E|W);\\s*"),
  ## thence due DIRECTION to (PARALLEL|MERIDIAN)
  lexerRegexp("PathLineLL",
              "^\\s*thence due (\\w+) to (\\d\\d?\\d?)°(?:(\\d\\d?)'(?:(\\d\\d?)\")?)?(N|S|E|W) (latitude|longitude);\\s*"),
  # thence DIRECTION to the point POINT
  # thence to POINT 
  lexerRegexp("PathLinePt",
              "^\\s*thence (?:(\\w+) )?to (?:the point )?(\\d\\d?)°(?:(\\d\\d?)'(?:(\\d\\d?)\")?)?(N|S), (\\d\\d?\\d?)°(?:(\\d\\d?)'(?:(\\d\\d?)\")?)?(E|W);\\s*"),
  ## thence due DIRECTION to the starting point.
  lexerRegexp("PathEnd",
              "^\\s*thence due (\\w+) to the starting point.\\s*"))

## ------------------------------------------------------------------------
txt <- "The waters bounded by a line starting at 45°S 60°E; thence due south to 53°14'S latitude; thence east to the point 53°14'07\"S, 67°03'20\"E; thence to 52°42'28\"S, 68°05'31\"E; thence to 51°58'18\"S, 69°44'02\"E; thence to 51°24'32\"S, 71°12'29\"E; thence to 51°03'09\"S, 72°28'28\"E; thence to 50°54'23\"S, 72°49'21\"E; thence to 49°49'34\"S, 75°36'08\"E; thence to 49°24'07\"S, 76°42'17\"E; thence due east to 80°E longitude; thence due north to 45°S latitude; thence due west to the starting point."
lex <- lexer(txt)
lex$tokens

## ------------------------------------------------------------------------
parserToken <- function(token)
  function(input) {
    if(length(input) > 0 && class(input[[1]])[1] == token)
      list(input=input[-1],result=input[[1]])
  }

## ------------------------------------------------------------------------
parserPath <- 
  pSeq("Path",
            parserToken("PathStart"),
            pSome("PathLineList",
                       pAlt("parserPathLine",
                                 parserToken("PathLineLL"),
                                 parserToken("PathLinePt"))),
            parserToken("PathEnd"))

## ------------------------------------------------------------------------
r2 <- parserPath(lex$tokens)
 cat(formatAST(r2$result))

