# scipiper

This package provides support and guidance for project organization and advanced
dependency management (with a shared cache and/or with many downloading or
modeling tasks). Conventions are those agreed on by our USGS-IIDD Data Science
team; however, minor deviations will often be appropriate. `scipiper` is
intended for internal team use, so while you're welcome to borrow ideas and
code, we can provide no external support for this package.

## Package Status

| Name       | Status           |  
| :------------ |:-------------|  
| Linux Build: | [![Build Status](https://travis-ci.org/USGS-R/scipiper.svg?branch=master)](https://travis-ci.org/USGS-R/scipiper) |
| Package tests: | [![Coverage Status](https://coveralls.io/repos/github/USGS-R/scipiper/badge.svg?branch=master)](https://coveralls.io/github/USGS-R/scipiper?branch=master) |  

[![status](https://img.shields.io/badge/USGS-Support-yellow.svg)](https://owi.usgs.gov/R/packages.html#support)

This package is considered a 'support' package. For more information, see:
[https://owi.usgs.gov/R/packages.html#support](https://owi.usgs.gov/R/packages.html#support)

## Installation

`scipiper` depends on `remake`, so install `remake` and its dependencies first:

```r
install.packages(c("R6", "yaml", "digest", "crayon", "optparse", "storr", "remotes"))
remotes::install_github('richfitz/remake')
```

Next, install `scipiper`
```r
remotes::install_github('USGS-R/scipiper')
```

## Best Practices

General advice is given in help files and vignettes within this package. Try these:

- `?organization`
- `vignette('remake', package='scipiper')`
- `?sharedcache`
- `?tasktables`

(OK, these aren't actually written yet. But sounds like a good idea, right?)

## Disclaimer

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)

This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information. Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."


[
  ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
](http://creativecommons.org/publicdomain/zero/1.0/)
