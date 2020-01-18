# test

RAI.test.A <- records %>% 
  rai.calculate(camera_operation_matrix, "2017-01-01", "2018-01-31") %>%
  filter(Species == "Waterbuck")

RAI.test.B <- records %>% 
  rai.calculate(camera_operation_matrix, "2017-01-01", "2018-01-31") %>%
  filter(Species == "Baboon")

RAI.test.A$Subset <- "A"
RAI.test.B$Subset <- "B"

RAI.test.AB <- bind_rows(RAI.test.A, RAI.test.B) %>%
  select(Camera, RAI, Subset) %>%
  pivot_wider(names_from = Subset, values_from = RAI)

# dot plot of A vs B
ggplotly(ggplot(data = RAI.test.AB,
       aes(x = log(A), y = log(B), label = Camera)) +
  geom_point() +
  geom_smooth(method = "lm", col = "gray") +
  theme_bw())


RAI.test.monthly.A <- records %>% 
  rai.monthly(camera_operation_matrix, "2017-01-01", "2018-01-31") %>%
  filter(Species == "Waterbuck")

RAI.test.monthly.B <- records %>% 
  rai.monthly(camera_operation_matrix, "2017-01-01", "2018-01-31") %>%
  filter(Species == "Baboon")

RAI.test.monthly.A$Subset <- "A"
RAI.test.monthly.B$Subset <- "B"

RAI.test.monthly.AB <- bind_rows(RAI.test.monthly.A, RAI.test.monthly.B)

# side-by-side monthly bar plot
ggplotly(ggplot(data = (RAI.test.monthly.AB %>% filter(Camera == "All")),
                aes(x = Month_Year, y = RAI, fill = Subset)) +
           geom_bar(stat = "identity", position = "dodge") +
           scale_fill_manual(values=c("#F8766D", "#00BFC4")) +
           theme(axis.text.x = element_text(angle = 45, hjust = 1)))
         

