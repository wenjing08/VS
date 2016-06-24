difference <- function(s1,s2){
  setdiff(union(s1,s2),intersect(s1,s2))
}
