an_input_dataset <- list (
  schleswig_ma = schleswig_ma[c("a", "Dx")],
  aiterhofen_oedmuehlen = aiterhofen_oedmuehlen[c("a", "Dx")]
)

attr(an_input_dataset,'grname') <- 'bohrmaschine'

suppressMessages(
 a_live_table <- life.table(an_input_dataset, agecor = FALSE)
)

single_life_table <- a_live_table$schleswig_ma
