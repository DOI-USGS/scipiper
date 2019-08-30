
plot_state_data <- function(figure_name, state_data, state) {
  png(figure_name)
  barplot(state_data, beside=T, names.arg = attr(state_data, 'dimnames')[[1]],
          ylab = 'Percent of population', main = state)
  dev.off()
}
    