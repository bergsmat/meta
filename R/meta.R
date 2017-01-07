#' Save Meta Format as CSV
#' 
#' Saves meta format as csv.
#' 
#' Simply calls as.csv.data.frame
#' 
#' @inheritParams as.csv
#' @return invisible meta (x)
#' @export
as.csv.meta <- function(x,...)as.csv.data.frame(x,...)


#' Coerce to Meta Format
#' 
#' Coerces to meta format.
#' 
#' @param x object
#' @param ... passed arguments
#' @export
as.meta <- function(x,...)UseMethod('as.meta')

#' Coerce to Meta Format from character
#' 
#' Coerces to meta format from character.
#' 
#' Assumes x is a filename.
#' 
#' @inheritParams as.meta
#' @return meta
#' @export
as.meta.character <- function(x,...){
  y <- as.csv.character(x,...)
  y <- as.meta(y)
  y
}
#' Coerce to Meta Format from Meta Format
#' 
#' Coerces to meta format from meta.  A non-operation.
#' 
#' 
#' @inheritParams as.meta
#' @return meta
#' @export
as.meta.meta <- function(x,...)x


#' Coerce to Meta from Data Frame
#' 
#' Coerces to meta from data.frame.
#' 
#' Expects columns VARIABLE, META, and VALUE. Remaining columns are for classification and may be NA. Coerces VALUE to character. Removes duplicate records with warning. Sorts on non-value columns by default
#' 
#' @inheritParams as.meta
#' @sort Should the result be sorted?
#' @return meta
#' @export
as.meta.data.frame <- function(x,sort=TRUE,...){
  constitutive <- c('VARIABLE','META','VALUE')
  extras <- setdiff(names(x),constitutive)
  last <- if(length(extras)) rev(extras)[[1]] else character(0)
  stopifnot(
    all(constitutive %in% names(x))
  )
  x <- x[,c(constitutive,extras),drop=FALSE]
  if(any(duplicated(x))){
    warning('removing duplicates')
    x %>% unique
  }
  if(sort) x %>% arrange_(.dots=setdiff(names(x),'VALUE'))
  d <- x %>% select(-VALUE)
  d <- d[duplicated(d),,drop=FALSE]
  if(nrow(d)){
    eg <- d[1,,drop=FALSE]
    nms <- paste(names(eg),collapse=', ')
    eg <- do.call(paste,c(as.list(eg),list(collapse=', ')))
    warning('found duplicates of ',nms,' e.g. ',eg)
  }
  d <- x %>% select(-VALUE)
  if(ncol(d) > 2) d <- d[,1:(ncol(d) - 1),drop=FALSE] # select all but last
  d <- d[duplicated(d),,drop=FALSE]
  if(!nrow(d)){
    col <- names(d)[ncol(d)]
    warning('records not duplicated even without ',col)
  }
  class(x) <- union('meta',class(x))
  x
}


#' Distill a Component of an Object
#' 
#' Distills a component of an object.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keyword internal
distill <- function(x,...)UseMethod('distill')

#' Distill a Variable of Meta
#' 
#' Distills a variable of meta.
#' 
#' @inheritParams distill
#' @param variable character
#' @param meta character
#' @export
#' @keyword internal
distill.meta <- function(x,...){
  class(x) <- setdiff(class(x),'meta')
  distill(x,...)
}



#' Distill a Variable of a Data Frame
#' 
#' Distills a variable of a data.frame
#' 
#' Everything known about variable x, recursively mining metadata.
#' @inheritParams distill
#' @param variable character
#' @param meta character
#' @export
#' @keyword internal

distill.data.frame <- function(
  x,
  mission,
  parent=character(0),
  ...
){
  res=data.frame()
  data <- x %>% filter(VARIABLE == mission & META %>% is.na)
  if(nrow(data)) {
    data %<>% spread(VARIABLE,VALUE,convert=TRUE)
    data %<>% select(-META) 
    data %<>% informative
    res <- data
  }
  meta <- x %>% filter(VARIABLE == mission & META %>% is.defined)
  if(nrow(meta)){
    for(m in unique(meta$META)){
     # message('processing attribute ',m)
      me <- meta
      me %<>% filter(META == m) 
      me %<>% spread(META,VALUE) 
      me %<>% select(-VARIABLE)
      me %<>% informative
      lineage <- c(parent,mission)
      canonical <- c(lineage,m)
      canonical <- paste(canonical,collapse='_')
      names(me)[names(me) == m] <- canonical
      mo <- distill.meta(x,mission=m,parent=lineage,...)
      me %<>% weld(mo)
      enc <- all(encoded(me[[canonical]])) & length(me[[canonical]]) == 1
      if(!nrow(res))res <- me
      if(nrow(res) & !enc) res %<>% weld(me)
      if(nrow(res) &  enc) res %<>% decode(
        encoded=mission,
        encoding=me[[canonical]][[1]],
        decoded = canonical,
        ...
      )
    }
  }
  res
}


#' Unfold an Object
#' 
#' Unfolds an object
#' 
#' @param x object
#' @param ... passed arguments
#' @export
unfold <- function(x,...)UseMethod('unfold')

#' Unfold a Meta Format Object
#' 
#' Unfolds a meta data.frame.
#' 
#' Unwinds meta.  Ideally it should be losslessly reversible.
#' 
#' @inheritParams unfold
#' @param var variables to unfold, given as character vector
#' @export
unfold.meta <- function(  
  x,
  var = unique(x$VARIABLE[is.na(x$META)]),
  ...
){
  groups <- setdiff(names(x),c('VARIABLE','META','VALUE'))
  y <- lapply(var,function(v)distill(x,mission=v,...))
  z <- metaMerge(y)
  groups <- intersect(groups,names(z))
  z %<>% group_by_(.dots=groups)
  z
}
                          
filter_.meta <- function(.data,...,.dots,add = FALSE){
  x <- NextMethod()
  class(x) <- union('meta',class(x))
  x
}
group_by_.meta <- function(.data,...,.dots,add = FALSE){
  x <- NextMethod()
  class(x) <- union('meta',class(x))
  x
}
group_by_.meta <- function(.data,...,.dots,add = FALSE){
  x <- NextMethod()
  class(x) <- union('meta',class(x))
  x
}
anti_join.meta <- function(.data,...,.dots,add = FALSE){
  x <- NextMethod()
  class(x) <- union('meta',class(x))
  x
}
ungroup.meta <- function(.data,...,.dots,add = FALSE){
  x <- NextMethod()
  class(x) <- union('meta',class(x))
  x
}
mutate_.meta <- function(.data,...,.dots,add = FALSE){
  x <- NextMethod()
  class(x) <- union('meta',class(x))
  x
}
left_join.meta <- function(.data,...,.dots,add = FALSE){
  x <- NextMethod()
  class(x) <- union('meta',class(x))
  x
}
select_.meta <- function(.data,...,.dots,add = FALSE){
  x <- NextMethod()
  class(x) <- union('meta',class(x))
  x
}
arrange_.meta <- function(.data,...,.dots,add = FALSE){
  x <- NextMethod()
  class(x) <- union('meta',class(x))
  x
}


#' Fold an Object
#' 
#' Folds an object.
#' 
#' @param x object
#' @param ... passed arguments
#' @export
fold <- function(x,...)UseMethod('fold')

#' Fold a Data Frame
#' 
#' Folds a data.frame.
#' 
#' I.e. convert from wide format to tall format. An attempt will be made to harvest metadata using the header convention object_attribute.

#' @inheritParams fold
#' @param groups a vector of column names serving as key: included in result but not stacked
#' @param meta a list of formulas in the form object ~ attribute. Pass something with length 0 to suppress guessing.
#' @param simplify set to NA any group_by values that do not help distinguish values, and remove resulting duplicate records
#' @return meta data.frame (folded)
#' @export
fold.data.frame <- function(
  x, 
  group_by=groups(x),
  meta = obj_attr(x),
  simplify = TRUE,
  ...
){
  # meta
  VARIABLE <- sapply(meta,function(f)f %>% as.list %>% `[[`(2) %>% as.character)
  META     <- sapply(meta,function(f)f %>% as.list %>% `[[`(3) %>% as.character)
  COL      <- names(meta)
  table <- data.frame(stringsAsFactors = FALSE,VARIABLE,META,COL)
  # data
  d <- x[,setdiff(names(x),COL),drop=F]
  d %<>% gather_('VARIABLE','VALUE',setdiff(names(d),group_by))
  d %<>% mutate(META=NA_character_)
  d %<>% mutate(VALUE = VALUE %>% as.character)
  if(nrow(table)){
    m <- x %>% 
      select_(.dots=c(group_by,COL)) %>% 
      gather_('COL','VALUE',COL) %>%
      unique
    m %<>% left_join(table,by='COL') %>% select(-COL)
    m %<>% select_(.dots=c('VARIABLE','META','VALUE',group_by))
    m %<>% as.meta
    for(i in 1:nrow(table)){
      var <- table[i,'VARIABLE']
      met <- table[i,'META']
      col <- table[i,'COL']
      if(is.factor(x[[col]])){
        decodes <- levels(x[[col]])
        codes <- x[[var]][match(decodes,x[[col]])]
        encoding <- encode(codes,decodes)
        m$VALUE[m$VARIABLE == var & m$META == met] <- encoding
      }
    }
    if(simplify) m %<>% reduce %>% unique
    m %<>% select_(.dots=c('VARIABLE','META','VALUE',group_by))
    d <- bind_rows(m,d)
  }
  d %<>% as.meta # sorts by default. ?
  d
}
  

#' Infer Object-Attribute Relationships
#' 
#' Infers object-attribute relationships.
#' @param x character
#' @param ... passed arguments
obj_attr <- function(x,...)UseMethod('obj_attr')

#' Infer Object Attribute Relationships from Character.
#' 
#' Infers object ~ attribute relationships from character.
#' 
#' Character is, for example, the names of a data.frame.  Processes the names, splitting on underscore and converting to formulas, with names same as the values in x that contained any underscores.
#' @inheritParams obj_attr
#' @return a named list of formulas in the form object ~ attribute
#' @export
obj_attr.character <- function(x,...){
  x <- x[grepl( '_.',x)]
  y <- strsplit(x,'_') # all these should have two elements
  z <- lapply(y,paste,collapse='~')
  z <- lapply(z,as.formula)
  names(z) <- x
  z
}

#' Infer Object Attribute Relationships from Data Frame
#' 
#' Infers object ~ attribute relationships from data.frame.
#' 
#' Processes the names of a data.frame.
#' @inheritParams obj_attr
#' @return a list of formulas in the form object ~ attribute
#' @export
obj_attr.data.frame <- function(x,...)obj_attr(names(x),...)

print.meta <- function(x,...){
  x[] <- lapply(x,shortOrNot)
  NextMethod()
}

shortOrNot <- function(x){
  if(!is.character(x)) return(x)
  if(any(nchar(x[is.defined(x)]) > 8)){
    if(any(encoded(x[is.defined(x)]))){
      return(short(x))
    }
  }
  return(x)
}

short <- function(x){
  y <- substr(x,1,8)
  nchar <- nchar(x)
  y <- paste0(y,ifelse(nchar>8,'...',''))
  y <- as.character(y)
  y
}

weld <- function(x,y,...){
  stopifnot(
    x %>% inherits('data.frame'),
    y %>% inherits('data.frame')
  )
  if(!nrow(x) & !nrow(y))return(x)
  if(!nrow(x) &  nrow(y))return(y)
  if( nrow(x) & !nrow(y))return(x)
  if( nrow(x) &  nrow(y))merge(x,y,all=T)
}

decode.data.frame <- function(
  x, 
  encoded, 
  encoding,
  decoded,
  ...
){
  stopifnot(length(encoding) == 1)
  if(!encoded %in% names(x))return(x)
  if(decoded %in% names(x)){
    warning(decoded,' already present, skipping decode ')
    return(x)
  }
  #if(length(unique(x[[encoding]])) > 1)warning(encoding, 'unsing only the first value of ',encoding)
  #enc <- x[1,encoding]
  if(!encoded(encoding)){
    warning(encoding, ' appears not to be encoded, no decode attempted')
    return(x)
  }
  codes <- codes(encoding)
  decodes <- decodes(encoding)
  x[[decoded]] <- map(x[[encoded]], from=codes, to = decodes)
  x[[decoded]] <- factor(x[[decoded]],levels=decodes)
  #x[[encoded]] <- factor(x[[encoded]],levels=codes)
  x
}

reduce <- function(x,...)UseMethod('reduce')
reduce.meta <- function(x,ignore=character(0),protect=FALSE,...){
  classifiers <- setdiff(names(x),c('VARIABLE','META','VALUE'))
  classifiers <- setdiff(classifiers,ignore)
  if(!length(classifiers))return(x)
  test <- rev(classifiers)[[1]]
  remaining <- setdiff(classifiers,test)
  y <- x %>% group_by_(.dots=c('VARIABLE','META',remaining))
  y %<>% mutate(count = length(unique(VALUE)))
  y$protect <- protect
  target <- y$count == 1 & !y$protect
  y[[test]][target] <- NA
  y$count <- NULL
  y$protect <- NULL
  reduce.meta(y,ignore=c(test,ignore),protect = !target, ...)
}

is.defined <- function(x,...)!is.na(x)
map <- function (x, from, to, strict = TRUE, ...) 
{
  stopifnot(length(to) == length(from))
  res <- to[match(x, table = from)]
  if (!strict) 
    res[!(x %in% from)] <- x[!(x %in% from)]
  res
}

unique.meta <- function(x, incomparables = FALSE,...){
  y <- unique.data.frame(x)
  class(y) <- union('meta',class(y))
  y
}

#' Interpret something
#' 
#' Interprets something.
#' 
#' @param x object
#' @param ... passed arguments
#' @export
interpret <- function(x,...)UseMethod('interpret')

#' Interpret Data in Meta Format
#' 
#' Interprets data in meta format.  Specifically, substitutes decodes for codes
#' when presenting encoded variables.
#' @inheritParams interpret
#' @return meta data.frame
#' @export
interpret.meta <- function(x,...){
  meta <- x %>% 
    filter(META %in% c('GUIDE','LABEL')) %>%
    spread(META,VALUE) %>% 
    select(VARIABLE,GUIDE,LABEL) %>% 
    mutate(encoded = encoded(GUIDE))
  x %<>% filter(!META %in% c('GUIDE','LABEL'))
  x %<>% left_join(meta,by='VARIABLE')
  x %<>% mutate(VARIABLE = if_else(
    encoded,
    LABEL,
    paste(LABEL,parens(GUIDE))
  )) %>% select(-encoded,-LABEL)
  x %<>% 
    group_by(VARIABLE) %>%
    mutate(VALUE = decode(VALUE,GUIDE[[1]])) %>%
    ungroup %>%
    select(-GUIDE)
  x
}








