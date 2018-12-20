## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, cache=FALSE, collapse=TRUE)
knitr::opts_knit$set(root.dir = tempdir())

## ----libs, message=FALSE-------------------------------------------------
suppressPackageStartupMessages({
  library(scipiper)
  library(dplyr)
})

## ----step_funs_shhh, echo=FALSE------------------------------------------
cat("
## @knitr step_funs

# step 1: prepare some data given the info in prep_name, arg2, file 'A.txt', and R object 'B'
prep <- function(prep_name) {
  message(sprintf('preparing %s', prep_name))
  return(prep_name)
}

# step 2: visualize the data
visualize <- function(plot_name, state_data, state_capital) {
  message(sprintf('visualizing %s - %s', plot_name, state_capital))
  file.create(plot_name, showWarnings=FALSE)
}

# step 3: create the report
report <- function(report_name) {
  message(sprintf('reporting on %s', report_name))
  file.create(report_name, showWarnings=FALSE)
}
", file='script.R')
knitr::read_chunk('script.R')

## ----step_funs-----------------------------------------------------------
# step 1: prepare some data given the info in prep_name, arg2, file 'A.txt', and R object 'B'
prep <- function(prep_name) {
  message(sprintf('preparing %s', prep_name))
  return(prep_name)
}

# step 2: visualize the data
visualize <- function(plot_name, state_data, state_capital) {
  message(sprintf('visualizing %s - %s', plot_name, state_capital))
  file.create(plot_name, showWarnings=FALSE)
}

# step 3: create the report
report <- function(report_name) {
  message(sprintf('reporting on %s', report_name))
  file.create(report_name, showWarnings=FALSE)
}

## ----tasks---------------------------------------------------------------
task_config <- data_frame(
  id=c('WI','AZ','PA'),
  capital=c('Madison','Phoenix','Harrisburg')
)

## ----task_steps----------------------------------------------------------
# step1 creates recipes like this:
# WI_prep:
#   command: prep(I('WI'))
step1 <- create_task_step(step_name = 'prep')

# step2 creates recipes like this:
# WI_plot.png:
#   command: visualize(target_name, 'WI_prep', I('Madison'))
step2 <- create_task_step(
  step_name = 'plot',
  target_name = function(task_name, ...) {
    sprintf('%s_plot.png', task_name)
  },
  command = function(target_name, task_name, ...) {
    capital <- task_config[task_config$id == task_name, 'capital']
    sprintf("visualize(target_name, %s_prep, I('%s'))", task_name, capital)
  })

# step3 creates recipes like this:
# WI.pdf:
#   command: report(target_name)
#   depends:
#    - WI_plot.png
step3 <- create_task_step(
  step_name = 'report',
  target_name = function(task_name, ...) {
    sprintf('%s.pdf', task_name)
  },
  depends = function(task_name, ...) {
    sprintf('%s_plot.png', task_name)
  },
  command = function(task_name, step_name, ...) {
    sprintf("report(target_name)", task_name)
  })

## ----plan----------------------------------------------------------------
task_plan_1 <- create_task_plan(
  task_config$id,
  list(step1, step2, step3),
  final_steps='report',
  ind_dir='states/log')

## ----makefile, results='markup'------------------------------------------
create_task_makefile(
  task_plan=task_plan_1,
  makefile='task_plan_1.yml',
  include=c(),
  packages=c('ggplot2'),
  sources=c('script.R'),
  file_extensions=c('ind'),
  ind_complete=TRUE)

## ----read_makefile, results='markup', comment='', echo=FALSE-------------
cat(readr::read_lines('task_plan_1.yml'), sep='\n')

## ----delete1, echo=FALSE-------------------------------------------------
unlink('.remake')
unlink('build')

## ----loop1a--------------------------------------------------------------
loop_tasks(
  task_plan=task_plan_1,
  task_makefile='task_plan_1.yml')

## ----try_rebuild_one-----------------------------------------------------
scmake('AZ_plot.png', 'task_plan_1.yml')

## ----delete_one----------------------------------------------------------
scdel('AZ_plot.png', 'task_plan_1.yml')

## ----rebuild_one---------------------------------------------------------
scmake('AZ_plot.png', 'task_plan_1.yml')

## ----force_one-----------------------------------------------------------
scmake('AZ_plot.png', 'task_plan_1.yml', force=TRUE)

## ----try_rebuild_all-----------------------------------------------------
loop_tasks(
  task_plan=task_plan_1,
  task_makefile='task_plan_1.yml')

## ----delete_all----------------------------------------------------------
scdel(target_names=list_all_targets('task_plan_1.yml'), remake_file='task_plan_1.yml')

## ----rebuild_all---------------------------------------------------------
loop_tasks(
  task_plan=task_plan_1,
  task_makefile='task_plan_1.yml')

## ----force_all-----------------------------------------------------------
scmake(list_all_targets('task_plan_1.yml'), 'task_plan_1.yml', force=TRUE)

## ----delete_all2---------------------------------------------------------
scdel(target_names=list_all_targets('task_plan_1.yml'), remake_file='task_plan_1.yml')

## ----loop_task_step_only-------------------------------------------------
loop_tasks(
  task_plan=task_plan_1,
  task_makefile='task_plan_1.yml',
  task_names='PA',
  step_names='prep')

## ----loop_steps_only-----------------------------------------------------
loop_tasks(
  task_plan=task_plan_1,
  task_makefile='task_plan_1.yml',
  step_names='plot')

## ----loop_tasks_only-----------------------------------------------------
loop_tasks(
  task_plan=task_plan_1,
  task_makefile='task_plan_1.yml',
  task_names=c('PA','AZ'))

## ----loop_remainder------------------------------------------------------
loop_tasks(
  task_plan=task_plan_1,
  task_makefile='task_plan_1.yml')

## ----plan2---------------------------------------------------------------
task_plan_2 <- create_task_plan(
  task_config$id,
  list(step1, step2, step3),
  final_steps='report',
  add_complete=FALSE)

## ----makefile2, results='markup'-----------------------------------------
create_task_makefile(
  task_plan=task_plan_2,
  makefile='task_plan_2.yml',
  include=c(),
  packages=c('ggplot2'),
  sources=c('script.R'),
  file_extensions=c('ind'),
  ind_complete=FALSE)

## ----read_makefile2, results='markup', comment='', echo=FALSE------------
cat(readr::read_lines('task_plan_2.yml'), sep='\n')

