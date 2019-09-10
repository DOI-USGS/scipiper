context("Explain why files or objects aren't up to date")

test_that("scbless=sc_declare_current reassures remake and scipiper about unbuilt or contaminated files",{
  dirinfo <- setup_tasks_demo()
  
  remake_yml <- yaml::read_yaml('remake.yml')

  # define and identify an unbuilt target of each type: files, objects, fake targets
  remake_yml$targets <- append(remake_yml$targets, list(group_target=list(depends=c('resource_A','resource_B'))))
  yaml::write_yaml(remake_yml, 'remake.yml')
  test_targets <- c(file='models.yml', object='resource_A', fake='group_target')
  expect_true(all(test_targets %in% which_dirty(test_targets)))
  
  # unbuilt objects aren't blessable; nonexistent files aren't blessable; unbuilt but provided files are blessable
  expect_error(scbless(test_targets[c('file','object')]), 'cannot be blessed: models.yml, resource_A', fixed=TRUE)
  expect_warning(scbless(test_targets['fake']), 'will be skipped: group_target', fixed=TRUE)
  file.create(test_targets['file'])
  expect_error(
    expect_warning(scbless(test_targets['file']), "these dependencies.*could not be hashed"),
    "couldn't determine new dependency status", info="need dependency info to bless")
  scmake('task_plan')
  expect_equal(scbless(test_targets['file']), test_targets['file'], info="can be blessed when dependencies are all there")
  expect_false(any(grepl('BUILD', capture_messages(scmake(test_targets['file'])))), info='blessing should mean no more building')
    
  # should notice when blessing doesn't actually make something current
  expect_message(scmake('models.yml', force=TRUE), 'BUILD') # make this a real yml file so we can use its targets
  scmake('CA_prep', remake_file = 'models.yml')
  remake_yml$targets$resource_A$command <- "c(I(1:10), I(11:20))"
  remake_yml$targets$resource_B$command <- "c(I(31:41))"
  yaml::write_yaml(remake_yml, 'remake.yml')
  expect_true(
    expect_message(why_dirty('CA_prep', remake_file = 'models.yml'), 'objects.*that have changed') %>%
      dplyr::filter(type=='target') %>%
      mutate(looks_good=!hash_mismatch & !dirty & dirty_by_descent & current) %>%
      pull(looks_good), info='target is dirty by descent only')
  expect_warning(scbless('CA_prep', remake_file='models.yml'), 'still dirty by descent')
  
  # blessing descendents makes a dirty-by-descent file current
  expect_equal(scbless(c('resource_A', 'resource_B')), c('resource_A', 'resource_B'), info='bless multiple objects at once')
  expect_equal(which_dirty('CA_prep', 'models.yml'), character(0), info='was only dirty by descent, now current')
  
  # blessing should work if dependencies have changed but are not themselves dirty
  scmake('resource_B', force=TRUE) # change the contents of B to make CA_prep dirty again
  expect_equal(which_dirty('CA_prep', 'models.yml', RDSify_first=FALSE), 'CA_prep', info='should be dirty again')
  expect_equal(scbless('CA_prep', 'models.yml'), 'CA_prep', info='should be blessable')
  tasks_demo_R <- readr::read_lines('tasks_demo.R')
  tasks_demo_R[grep("  return(sprintf('PROCESSED_%s', task))", tasks_demo_R, fixed=TRUE)] <- "  return(sprintf('PREPARED_%s', task))"
  readr::write_lines(tasks_demo_R, 'tasks_demo.R') # change the contents of process() to make CA_prep dirty again
  expect_message(why_dirty('CA_prep', 'models.yml'), "the function 'process' used by the target has changed")
  expect_equal(scbless('CA_prep', 'models.yml'), 'CA_prep', info='should be blessable')
  
  # indicator files should get YAMLified
  expect_equal(dir('build/status'), character(0))
  scmake('models.ind')
  models_ind_yml_true <- yaml::read_yaml(scipiper:::locate_build_status_yml('models.ind'))
  readr::read_lines('models.ind') %>%
    c('extra_line: hooray!') %>% 
    readr::write_lines('models.ind')
  scbless('models.ind')
  models_ind_yml_blessed <- yaml::read_yaml(scipiper:::locate_build_status_yml('models.ind'))
  expect_true(models_ind_yml_true$hash != models_ind_yml_blessed$hash)
  unchanged_fields <- c('version', 'name', 'depends', 'fixed', 'code')
  expect_equal(models_ind_yml_true[unchanged_fields], models_ind_yml_blessed[unchanged_fields])
  expect_equal(which_dirty('models.ind'), character(0)) # this is actually true even between editing models.ind and scblessing it, but should definitely be true now too
  
  cleanup_tasks_demo(dirinfo)
})

