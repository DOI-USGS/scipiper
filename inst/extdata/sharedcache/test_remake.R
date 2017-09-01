library(remake)
# options('remake.verbose'=FALSE) would be great but seems to have no effect

run_scenario <- function(scen) {
  make('clean', verbose=FALSE)
  gc()
  make(scen, verbose=FALSE)
  print(get_status())
  make('B.rds', verbose=FALSE)
  print(get_status())
}

get_status <- function(target='B.rds') {
  remake_object <- remake:::remake(load_sources=FALSE)
  graph <- remake:::remake_dependency_graph(remake_object)
  
  # remake_status doesn't actually get used by remake::make, and the code is
  # full of caveats that make me wonder if they believe it...but it's the
  # nearest thing to a current status report that I've found so far
  status <- as.data.frame(remake:::remake_status(remake_object, target, graph))
  
  status$target <- rownames(status)
  
  status$is_current <- status$hash <- status$time <- status$fixed <- NA
  for(i in seq_len(nrow(status))) {
    tryCatch({
      status[i,'is_current'] <- remake:::remake_is_current(remake_object, status$target[i])
      remeta <- remetadata(status$target[i])
      status[i,'hash'] <- remeta$hash
      status[i,'time'] <- POSIX2char(remeta$time)
      status[i,'fixed'] <- if(!is.null(remeta$fixed)) remeta$fixed else NA
    }, error=function(e) NULL)
  }
  status[c('target','is_current','dirty','dirty_by_descent','time','hash','fixed')]
}

get_plan <- function(target='B.rds') {
  remake_object <- remake:::remake(load_sources=FALSE)
  plan <- remake:::remake_plan(remake_object, target_name=target)
  needed <- sapply(setNames(nm=plan), function(t) !remake:::remake_is_current(remake_object, t))
  names(needed)[needed]
}

touch <- function(target_name='A.txt.st') {
  if(!file.exists(target_name)) {
    stop(paste0("file doesn't exist so shouldn't be touched: ", target_name))
  }
  remake_object <- remake:::remake()
  target <- remake_object$targets[[target_name]]
  remake:::target_set(target=target, store=remake_object$store, NULL)
}

gc <- function() {
  remake_object <- remake:::remake()
  remake_object$store$db$gc()
}

remetadata <- function(target_name) {
  remake_object <- remake:::remake()
  remake_object$store$db$get(target_name) 
}


run_scenario('a1') # success
run_scenario('a2')
run_scenario('a3')
run_scenario('a4')
run_scenario('a5')
run_scenario('a6')
run_scenario('a7') # should make nothing. makes everything. how can we tell remake to look at a fresh git pull as possibly being built already?

run_scenario('b1')
run_scenario('b2')
run_scenario('b3')
run_scenario('b4')

run_scenario('c1')
run_scenario('c2')
run_scenario('c3')
run_scenario('c4')
