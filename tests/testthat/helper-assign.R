an_input_dataset <- data.frame(male = round(runif(70)*100, 0), female = round(runif(70)*100, 0))
a_live_table <- life.table(an_input_dataset)
