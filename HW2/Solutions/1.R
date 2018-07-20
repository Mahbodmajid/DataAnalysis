companies <- mobile %>%
  group_by(company) %>%
  summarise(n = n())

top_companies <- companies %>%
  top_n(n =  20, wt = n)

top_companies$company <-
  factor(top_companies$company,
         levels = top_companies$company[order(top_companies$n)])

ggplot(top_companies, aes(x = company, y = n, fill = n)) +
  geom_bar(stat = "identity")
