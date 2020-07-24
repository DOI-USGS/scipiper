context("create_task_makefile")

task_config <- data.frame(
  id=c('AZ','CO','CA'),
  capital=c('Phoeniz','Denver','Sacramento')
)
step1 <- create_task_step(
  step_name = 'prep',
  target = function(task_name, step_name, ...) {
    sprintf('%s_%s', task_name, step_name)
  },
  depends = c('A','B'),
  command = "process(target_name, I('C'))"
)
step2 <- create_task_step(
  step_name = 'plot',
  command = function(target_name, task_name, ...) {
    capital <- task_config[task_config$id == task_name, 'capital']
    sprintf('visualize(\'%s\', \'%s\')', task_name, capital)
  }
)
step3 <- create_task_step('report')
task_plan <- create_task_plan(
  c('AZ','CA','CO'), 
  list(step1, step2, step3),
  final_steps='report',
  ind_dir='states/log')

test_that("can create task_plan", {
  expect_true(all(names(task_plan) == c('AZ','CA','CO')))
})

test_that("can create task_makefile as string with defaults", {
  task_makefile <- create_task_makefile(
    task_plan, makefile=NULL,
    file_extensions=c('ind'), packages=c('scipiper','mda.streams'))
  expect_is(task_makefile, "character")
  remake_list <- yaml::yaml.load(task_makefile)
  expect_equal(tail(names(remake_list$targets), 1), "all_tasks.ind_promise")
  expect_equal(head(names(remake_list$targets), 1), "all_tasks")
  expect_equal(tail(remake_list$targets, 1)[[1]]$command, 
               "combine_to_ind(I('states/log/all_tasks.ind'), 'states/log/AZ.ind', 'states/log/CA.ind', 'states/log/CO.ind')")
})

test_that("makefile reports on the function that called create_task_makefile", {
  makefile_maker <- function(task_plan) {
    create_task_makefile(
      task_plan, makefile=NULL,
      file_extensions=c('ind'), packages=c('scipiper','mda.streams'))
  }
  task_makefile <- makefile_maker(task_plan)
  expect_equal(strsplit(task_makefile, split='\n')[[1]][3], '# by create_task_makefile() via makefile_maker()')
})

test_that("errors when object target and combine_to_ind used", {
  expect_error(create_task_makefile(final_targets = 'my_object',
    task_plan, makefile=NULL,
    file_extensions=c('ind'), packages=c('scipiper','mda.streams')))
  
  expect_error(create_task_makefile(final_targets = c('my_object','my_file.ind'),
                                    task_plan, makefile=NULL,
                                    file_extensions=c('ind'), packages=c('scipiper','mda.streams')))
  expect_error(create_task_makefile(final_targets = c('my_object','my_file.ind'),
                                    task_plan, makefile=NULL,
                                    file_extensions=c('ind'), packages=c('scipiper','mda.streams'),
                                    finalize_funs = c('combine_to_ind','my_combiner')))
  expect_error(create_task_makefile(final_targets = c('my_object','my_file.ind'),
                                    task_plan, makefile=NULL,
                                    file_extensions=c('ind'), packages=c('scipiper','mda.streams'), as_promises = F))
  expect_error(create_task_makefile(final_targets = c('my_object','my_file.ind'),
                                    task_plan, makefile=NULL,
                                    file_extensions=c('ind'), packages=c('scipiper','mda.streams'),
                                    finalize_funs = c('combine_to_ind','my_combiner'), as_promises = F))
  task_makefile <- create_task_makefile(final_targets = c('my_object','my_file.ind'),
                       task_plan, makefile=NULL,
                       file_extensions=c('ind'), packages=c('scipiper','mda.streams'),
                       finalize_funs = c('my_combiner', 'combine_to_ind'))
  remake_list <- yaml::yaml.load(task_makefile)
  expect_equal(tail(names(remake_list$targets), 1), "my_file.ind_promise")
  expect_equal(tail(head(names(remake_list$targets), -1), 1), "my_object_promise")
})

test_that("can create task_makefile with named target", {
  task_makefile <- create_task_makefile(
    task_plan, makefile=NULL, final_targets = 'my_states.ind',
    file_extensions=c('ind'), packages=c('scipiper','mda.streams'))
  remake_list <- yaml::yaml.load(task_makefile)
  expect_equal(tail(names(remake_list$targets), 1), "my_states.ind_promise")
  expect_equal(head(names(remake_list$targets), 1), "my_states_all")
  expect_equal(tail(remake_list$targets, 1)[[1]]$command, 
               "combine_to_ind(I('my_states.ind'), 'states/log/AZ.ind', 'states/log/CA.ind', 'states/log/CO.ind')")
})

test_that("can create task_makefile with named target and remake_file", {
  task_makefile <- create_task_makefile(
    task_plan, makefile=file.path(tempdir(), 'states.yml'), final_targets = 'my_states.ind',
    file_extensions=c('ind'), packages=c('scipiper','mda.streams'))
  remake_list <- yaml::yaml.load_file(task_makefile)
  expect_equal(tail(names(remake_list$targets), 1), "my_states.ind_promise")
  expect_equal(head(names(remake_list$targets), 1), "states")
  expect_equal(tail(remake_list$targets, 1)[[1]]$command, 
               "combine_to_ind(I('my_states.ind'), 'states/log/AZ.ind', 'states/log/CA.ind', 'states/log/CO.ind')")
})

test_that("can create task_makefile with named target and remake_file w/o promises", {
  task_makefile <- create_task_makefile(
    task_plan, makefile=file.path(tempdir(), 'states.yml'), final_targets = 'my_states.ind',
    file_extensions=c('ind'), packages=c('scipiper','mda.streams'), as_promises = FALSE)
  remake_list <- yaml::yaml.load_file(task_makefile)
  expect_equal(tail(names(remake_list$targets), 1), "my_states.ind")
})

test_that("can't create task_makefile with unequal target names and combiners", {
  expect_error(task_makefile <- create_task_makefile(
    task_plan, makefile=file.path(tempdir(), 'states.yml'), final_targets = 'my_states',
    file_extensions=c('ind'), packages=c('scipiper','mda.streams'), as_promises = FALSE, 
    finalize_fun = c('combine_these','combine_those')))
})

test_that("task_makefiles with multiple combiners", {
  task_makefile <- create_task_makefile(
    task_plan, makefile=file.path(tempdir(), 'states.yml'), final_targets = c('these_states','those_states'),
    file_extensions=c('ind'), packages=c('scipiper','mda.streams'), as_promises = FALSE, 
    finalize_fun = c('combine_these','combine_those'))
  remake_list <- yaml::yaml.load_file(task_makefile)
  expect_true(all(remake_list$targets$states$depends == c('these_states','those_states')))
  expect_equal(tail(remake_list$targets, 1)[[1]]$command, 
               "combine_those( 'states/log/AZ.ind', 'states/log/CA.ind', 'states/log/CO.ind')")
  
  task_makefile <- create_task_makefile(
    task_plan, makefile=file.path(tempdir(), 'states.yml'), final_targets = c('these_states','those_states'),
    file_extensions=c('ind'), packages=c('scipiper','mda.streams'), 
    finalize_fun = c('combine_these','combine_those'))
  remake_list <- yaml::yaml.load_file(task_makefile)
  expect_true(all(remake_list$targets$states$depends == c('these_states_promise','those_states_promise')))
})

test_that("task_makefiles with promises w/ mix of files and objects", {
  task_makefile <- create_task_makefile(
    task_plan, makefile=file.path(tempdir(), 'states.yml'), final_targets = c('out/these_state_files.csv','those_states'),
    file_extensions=c('ind'), packages=c('scipiper','mda.streams'), 
    finalize_fun = c('combine_these','combine_those'))
  remake_list <- yaml::yaml.load_file(task_makefile)
  # check that we drop the dir when creating a promise. 
  expect_equal(tail(names(remake_list$targets), 2), c("these_state_files.csv_promise", "those_states_promise"))
  expect_equal(tail(remake_list$targets, 2)[[1]]$command, 
               "combine_these(I('out/these_state_files.csv'), 'states/log/AZ.ind', 'states/log/CA.ind', 'states/log/CO.ind')")
  expect_equal(tail(remake_list$targets, 1)[[1]]$command, 
               "combine_those( 'states/log/AZ.ind', 'states/log/CA.ind', 'states/log/CO.ind')")
})
