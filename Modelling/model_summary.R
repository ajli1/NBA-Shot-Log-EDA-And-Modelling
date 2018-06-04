library(knitr)
library(tidyverse)

#summary table:
summary_table <- tibble(Model = c("Bagging", "Random Forest", "Boosting",
                                  "Logistic", "LDA", "QDA", "Neural Network"),
                        Test_Accuracy = c(.6031, .6024, .6195, .6090, .6087, .5585, .6136))

#arrange by descending test error
summary_table <- summary_table %>%
  arrange(desc(Test_Accuracy)) %>% kable()

#saveRDS(summary_table, "results/summary_table.rds")
