limit = 100
steps = 5
lower <- seq(from = 0, to = limit-steps[1], by = steps)
upper <- seq(from = steps[1], to = limit, by = steps)-1

an_input_dataset <- list (
  male = data.frame(
    x = paste0(lower, "--", upper),
    a = steps,
    Dx = runif(length(lower))*50
  ),
  female = data.frame(
    x = paste0(lower, "--", upper),
    a = steps,
    Dx = runif(length(lower))*50
  )
)

a_live_table <- life.table(an_input_dataset)

single_life_table <- a_live_table$male
