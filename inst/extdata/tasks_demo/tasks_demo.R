# functions run within the task plan
process <- function(task, resource_C) {
  if(isTRUE(getOption('scipiper.test_verbose'))) {
    message(sprintf("   - processing %s, resource_C=%s", task, resource_C))
  }
  if(task != 'CA' && rnorm(1) < 0.1) stop("processing failed")
  return(sprintf('PROCESSED_%s', task))
}
visualize <- function(prep, capital) {
  if(isTRUE(getOption('scipiper.test_verbose'))) {
    message(sprintf("   - visualizing with prep=%s, capital=%s", prep, capital))
  }
  if(prep != 'PROCESSED_CA' && rnorm(1) < 0.1) stop("visualizing failed")
  return(sprintf('PLOTTED_%s', substring(prep, 1, 2)))
}
report <- function(task, prep, plot) {
  if(isTRUE(getOption('scipiper.test_verbose'))) {
    message(sprintf("   - reporting on %s, prep=%s, plot=%s", substring(task, 1, 2), prep, plot))
  }
  return(sprintf('REPORTED_%s', substring(task, 1, 2)))
}

# build a task plan
demo_task_plan <- function(task_config) {
  step1 <- create_task_step(
    step_name = 'prep',
    target_name = function(task_name, step_name, ...) {
      sprintf('%s_%s', task_name, step_name)
    },
    depends = c('resource_A','resource_B'),
    command = function(task_name, ...) {
      sprintf("process(I('%s'), I('C'))", task_name)
    }
  )
  step2 <- create_task_step(
    step_name = 'plot',
    command = function(target_name, task_name, ...) {
      capital <- task_config[task_config$id == task_name, 'capital']
      sprintf('visualize(%s_prep, I(\'%s\'))', task_name, capital)
    }
  )
  step3 <- create_task_step(
    step_name = 'report',
    target_name = function(task_name, ...) {
      sprintf('%s_report', task_name)
    },
    command = function(target_name, task_name, ...) {
      sprintf('report(target_name, %s_prep, %s_plot)', task_name, task_name)
    })
  task_plan <- create_task_plan(
    task_names=c('AZ','CA','CO'),
    task_steps=list(step1, step2, step3),
    final_steps='report',
    ind_dir='.',
    add_complete=TRUE)
  return(task_plan)
}

demo_task_makefile <- function(makefile, task_plan) {
  create_task_makefile(
    task_plan,
    makefile,
    include='remake.yml')
}
