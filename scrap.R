weights <- freq_table(df$Weight)
weights <- weights %>% arrange(Value)

p <- ggplot(data=weights,aes(x=Value,y=N)) +
  geom_line()