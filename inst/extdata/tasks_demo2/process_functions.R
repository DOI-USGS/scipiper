
process_state_data <- function(process_data_name, national_data, state_name) {
  state_data <- national_data %>% 
    filter(state == state_name) %>% 
    select(Illiteracy, Murder, `HS Grad`) %>% 
    t()
  return(state_data)
}
