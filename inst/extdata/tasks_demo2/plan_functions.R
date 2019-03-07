
create_state_task_plan <- function(states) {
  
  step1 <- create_task_step(
    step_name = 'process',
    target_name = function(task_name, step_name, ...) {
      sprintf('data_%s', task_name)
    },
    command = function(task_name, ...) {
      psprintf(
        "process_state_data(",
        "process_data_name = target_name,",
        "national_data = all_data,", 
        "state = I('%s'))" = task_name)
    }
  )
  
  step2 <- create_task_step(
    step_name = 'visualize',
    target_name = function(task_name, step_name, ...) {
      sprintf('figures/plot_%s.png', task_name)
    },
    command = function(task_name, steps, ...) {
      psprintf(
        "plot_state_data(",
        "figure_name = target_name,", 
        "state_data = %s," = steps[['process']]$target_name,
        "state = I('%s'))" = task_name)
    }
  )
  
  task_plan <- create_task_plan(
    task_names=sort(states),
    task_steps=list(step1, step2),
    final_steps='visualize',
    add_complete = FALSE
  )
  
}
