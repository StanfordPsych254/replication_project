esComp <- function(
x,
df1,
df2,
N,
esType){
esComp <- ifelse(esType=="t",
sqrt((x^2*(1 / df2)) / (((x^2*1) / df2) + 1)),
ifelse(
esType=="F",
sqrt((x*(df1 / df2)) / (((x*df1) / df2) + 1))*sqrt(1/df1),
ifelse(
esType=="r",
x,
ifelse(
esType=="Chi2",
sqrt(x/N),
ifelse(
esType == "z",
tanh(x * sqrt(1/(N-3))),
NA
)
)
)
))
return(esComp)
}