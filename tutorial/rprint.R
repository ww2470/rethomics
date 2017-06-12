rPrint <- function(data){
  if(nrow(data) > 500){
    return(data)
  }
  require(DT)
  js <- JS(
    "function(data, type, row, meta) {",
    "if(data == null){",
    "return '<b>NA</b>';}",
    "else {",
    "return type === 'display' && data.length > 5 ?",
    "'<span title=\"' + data + '\">' + data.substr(0, 5) + '...</span>' : data;",
    "}}")
  out <- datatable(data, 
                   options = list(columnDefs = list(list(
                      targets = c(1:ncol(data)),
                      render = js
                    ))))
  #return(datatable(data))
  k <- key(data)
  if(length(k) > 0)
    out = out %>% formatStyle(k,  color = 'red', backgroundColor = 'orange', fontWeight = 'bold')

  s <- sapply(data,
              function(x){
                if(class(x)[1]!="numeric")
                  return(F)
                return(T)
              }
  )
  
  
  s = names(s)[s]
  out = out %>% formatRound(s, digits = 3)
  out
}

print <- function(what,...){
  if(is.data.table(what)){
    rPrint(what)
    
  }
  else{
    base::print(what,...)
  }
}

