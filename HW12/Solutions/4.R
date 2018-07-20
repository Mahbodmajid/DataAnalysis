library(arules)
library(arulesViz)

ratings %>% select(UserID, MovieID) -> transactions
transactions <- as(
  split(as.vector(transactions$MovieID), as.vector(transactions$UserID)),
  "transactions"
)
rule_param = list(
  supp = 0.001,
  conf = 0.6,
  maxlen = 2,
  minlen = 2
)

rules <- apriori(
  transactions,
  parameter=rule_param
)
assoc_rules = as(rules,"data.frame")

rules2 = sapply(assoc_rules$rules,function(x){
  x = gsub("[\\{\\}]", "", regmatches(x, gregexpr("\\{.*\\}", x))[[1]])
  x = gsub("=>",",",x)
  x = str_replace_all(x," ","")
  return( x )
})

rules2 = as.character(rules2)
rules2 = str_split(rules2,",")

assoc_rules$lhs_movie = sapply( rules2, "[[", 1)
assoc_rules$rhs_movie = sapply( rules2 , "[[", 2)

assoc_rules$rules2 = NULL
rm(rules2)
gc()
assoc_rules$lhs_movie = as.numeric(assoc_rules$lhs_movie)
assoc_rules$rhs_movie = as.numeric(assoc_rules$rhs_movie)

assoc_rules = assoc_rules %>% left_join(movies,by=c("lhs_movie" = "MovieID") )

assoc_rules$lhs_movie = NULL
col_name = colnames(assoc_rules)
col_name
col_name[7:9] = str_c("left.",colnames(movies))[2:4]
colnames(assoc_rules) = col_name

assoc_rules = assoc_rules %>% left_join(movies,by=c("rhs_movie" = "MovieID"))
assoc_rules$rhs_movie = NULL
col_name = colnames(assoc_rules)
col_name
col_name[9:11] = str_c("right.",col_name[9:11])
colnames(assoc_rules) = col_name

assoc_rules %>% 
  filter(str_detect(left.Title,"Castle in the Sky")) %>% 
  select(left.Title,left.Year,right.Title,right.Year,support,confidence,lift) %>% 
  arrange(desc(lift)) %>% head() %>% kable()

assoc_rules %>% 
  filter(str_detect(left.Title,"Cast Away")) %>% 
  select(left.Title,left.Year,right.Title,right.Year,support,confidence,lift) %>% 
  arrange(desc(lift)) %>% head() %>% kable()

assoc_rules %>% 
  filter(str_detect(left.Title,"No Country for Old Men")) %>% 
  select(left.Title,left.Year,right.Title,right.Year,support,confidence,lift) %>% 
  arrange(desc(lift)) %>% head() %>% kable()

assoc_rules %>% 
  filter(str_detect(left.Title,"Memento")) %>% 
  select(left.Title,left.Year,right.Title,right.Year,support,confidence,lift) %>% 
  arrange(desc(lift)) %>% head() %>% kable()

