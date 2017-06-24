
rPrint <- function(data){
  
  if(nrow(data) > 500){
    print(data)
    return()
  }
    
  #todo assert datatableness
  require(DT)
  out <- datatable(data)
  k <- key(data)
  if(length(k) > 0)
    out = out %>% formatStyle(k,  color = 'red', backgroundColor = 'orange', fontWeight = 'bold')

  s <- sapply(sleep_sexual_dimorphism,
              function(x){
                if(class(x)[1]!="numeric")
                  return(F)
                return(T)
              }
  )
  
  s = names(s)[s]
  out = out %>% formatRound(s, digits = 3)
  print(out)
  return()
}
