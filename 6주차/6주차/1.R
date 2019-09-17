primality <-function(x) {
  
  stopifnot(x >= 2) # x가2보다작다면에러를반환
  limit <-trunc(sqrt(x) + 1)
  testvec<-2:limit
  results <-x %% testvec
  check <-any(results == 0)
  outcome <-"Yes."
  if(check == TRUE) outcome <-"No."
  if(x == 2) outcome <-"Yes."

  return (outcome)
  
}
  
e <- 0

for(i in 3:10000){
  if(primality(i) == "Yes.") {
    e = e + 1
    cat(i," ") 
    if(e %% 20 == 0) {cat("\n")}
  }
}
cat("소수개수 : ", e, "개")

