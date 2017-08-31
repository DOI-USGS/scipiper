library(remake)
# options('remake.verbose'=FALSE) would be great but seems to have no effect

make_scenario <- function(scen) {
  make(scen, verbose=FALSE)
  print(get_status())
  make('B.rds.st', verbose=FALSE)
  print(get_status())
}

make_scenario('a1') # success
make_scenario('a2')
make_scenario('a3')
make_scenario('a4')
make_scenario('a5')
make_scenario('a6')
make_scenario('a7') # should make nothing. makes everything. how can we tell remake to look at a fresh git pull as possibly being built already?

make_scenario('b1')
make_scenario('b2')
make_scenario('b3')
make_scenario('b4')

make_scenario('c1')
make_scenario('c2')
make_scenario('c3')
make_scenario('c4')
