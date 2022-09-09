# Development Notes for LecoSpec

## Upcoming Improvements
* Rebuild spectral library pre-processing
* move to doFuture package for parallel backend (fix variable exports for estimate_land_cover function)
* more support for different models

## Rant about R
* Terrible class support - custom datatypes are terrible
* not type safe (not even annotations!)
* functional programming operations are not so nice - 
    * e.g. apply/purrr::map are not guaranteed to respect order
    * difficult to use many functional programming patterns (e.g. Monads)
    * objects are mutable, always.  WHY U NO HAS 'const' or 'mut'?
* encapsulation is more difficult than it needs to be
* parallelism and asynchronous programming support is limited
* namespace masking is a real problem
* plus, the syntax is just plain weird
* low package quality: 
    * do not work as advertised more often than in other languages
    * change without respect for backwards compatability
    * install.packages() works, but package version control is not ideal
    * often poor algorithms - e.g. HSDAR, lidR
* No Tensors - no arrays with more than 2 dimensions (matrix only)

In contrast, the advantages are few:
* Lots of niche packages (e.g. lidR, spectrolab, hsdar) because of use in research
    * two of these have bugs that I know of, and are not that nice in general!
* expanded native types
    * dataframes, linear models, statistical tests, etc.
    * this is not really an advantage since you can import them to other languages with a line or two of code
        * Python: import pandas as pd \ import statsmodels
        * Also in Java (Kotlin, Scala), C#, Julia, MATLAB (since 2020), JavaScript (limited support), and many others.  Not sure about Go/Rust.
        * Apache Spark does all this and more in Python or any JVM language.

So basically it has no advantage, and lots of problems.  