context("Explain why files or objects aren't up to date")

test_that("which_dirty identifies, and why_dirty explains, unbuilt or contaminated files",{
  dirinfo <- setup_tasks_demo()
  
  remake_yml <- yaml::read_yaml('remake.yml')
  
  # unbuilt targets: files, objects, fake targets
  expect_equal(which_dirty(), c("task_config", "task_plan", "models.yml", "models.ind"), info='default target')
  expect_equal(which_dirty('resource_A'), 'resource_A', info='specific target not directly in default target')
  remake_yml$targets <- append(remake_yml$targets, list(group_target=list(depends=c('resource_A','resource_B'))))
  yaml::write_yaml(remake_yml, 'remake.yml')
  expect_equal(which_dirty('group_target'), c('resource_A','resource_B','group_target'), info='group target')
  
  # expected states of targets, depends, and functions before anything is built
  expect_message(wd_models.yml <- why_dirty('models.yml'), "does not exist", info='file target')
  expect_message(wd_resource_A <- why_dirty('resource_A'), "does not exist", info='object target')
  expect_message(wd_group_target <- why_dirty('group_target'), "are never 'current'", info='fake target')
  expect_true({
    target <- dplyr::filter(wd_models.yml, type=='target')
    target$hash_old=='none' & target$hash_new=='none' && !target$hash_mismatch &&
      target$dirty && target$dirty_by_descent && !target$current})
  expect_true({
    target <- dplyr::filter(wd_resource_A, type=='target')
    target$hash_old=='none' & target$hash_new=='none' && !target$hash_mismatch &&
      target$dirty && !target$dirty_by_descent && !target$current})
  expect_true({
    target <- dplyr::filter(wd_group_target, type=='target')
    target$hash_old=='??' & target$hash_new=='??' && is.na(target$hash_mismatch) &&
      target$dirty && target$dirty_by_descent && !target$current})
  wd_combined <- dplyr::bind_rows(wd_models.yml, wd_resource_A, wd_group_target)
  expect_true({
    depends <- dplyr::filter(wd_combined, type=='depends');
    all(depends$hash_new == 'none') && all(is.na(depends$hash_mismatch)) && all(depends$dirty) && all(!depends$current)})
  expect_true({
    fixed <- dplyr::filter(wd_combined, type %in% c('fixed', 'functions'))
    all(is.na(fixed$hash_old)) && all(nchar(fixed$hash_new) == 32) && all(is.na(fixed$hash_mismatch)) &&
      all(!fixed$dirty) && all(!fixed$dirty_by_descent) && all(fixed$current)})
  
  # expected states of targets, depends, and functions after build
  scmake(c('models.yml', 'resource_A', 'group_target'))
  expect_equal(which_dirty(c('models.yml', 'resource_A', 'group_target')), 'group_target', info='fake is always dirty')
  
  # contaminate the three test targets
  remake_yml$targets$resource_A$command <- "c(I(21:40))"
  yaml::write_yaml(remake_yml, 'remake.yml')
  tasks_demo_R <- readr::read_lines('tasks_demo.R') %>%
    gsub("    step_name = 'prep',", "    step_name = 'prepare',", ., fixed=TRUE)
  readr::write_lines(tasks_demo_R, 'tasks_demo.R')
  
  # expected states after contamination
  expect_equal(which_dirty(c('models.yml', 'resource_A', 'group_target')), c('resource_A', 'task_plan', 'models.yml', 'group_target'))
  expect_message(wd_models.yml <- why_dirty('models.yml'), "depends on .* that have changed", info='file target')
  expect_message(wd_resource_A <- why_dirty('resource_A'), "fixed arguments .* to the target's command have changed", info='object target')
  expect_message(wd_group_target <- why_dirty('group_target'), "are never 'current'.*has these dirty dependencies", info='fake target')
  expect_true({
    target <- dplyr::filter(wd_models.yml, type=='target')
    target$hash_old==target$hash_new && nchar(target$hash_old)==32 && !target$hash_mismatch &&
      !target$dirty && target$dirty_by_descent && target$current}) # odd but true: dirty_by_descent and will be rebuilt by remake, yet 'current' according to remake
  expect_true({
    target <- dplyr::filter(wd_resource_A, type=='target')
    target$hash_old==target$hash_new && nchar(target$hash_old)==32 && !target$hash_mismatch &&
      target$dirty && !target$dirty_by_descent && !target$current})
  expect_true({
    target <- dplyr::filter(wd_group_target, type=='target')
    target$hash_old=='??' & target$hash_new=='??' && is.na(target$hash_mismatch) &&
      target$dirty && target$dirty_by_descent && !target$current})
  expect_equal(dplyr::filter(wd_models.yml, dirty)$name, 'task_plan', info='culprit is the task_plan')
  expect_equal(dplyr::filter(wd_resource_A, hash_mismatch)$type, 'fixed', info='culprit is the fixed argument')
  expect_equal(dplyr::filter(wd_group_target, dirty & type=='depends')$name, 'resource_A', info='culprit is resource_A')
  
  cleanup_tasks_demo(dirinfo)
})

