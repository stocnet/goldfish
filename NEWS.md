# goldfish 1.4.3

## New features

* `mixedTrans()` effect is created. It requires the definition of two networks
  to compute the number of two paths between nodes.

  ```R
  mixedTrans(list(bilatnet, contignet))
  ```

## Minor improvements and fixes

* `estimate()` no longer give warnings in R 4.0.0.
* Classes and types checkings are revisited and extended.
* `linkEvents()` is refactored as a S3 method.
* New `print` or `summary` methods for `goldfish` objects are developed.
* `head()` and `tail()` methods for `nodes.goldfish`, `network.goldfish`
  and `dependent.goldfish` are available.

# goldfish 1.3.2

## New features

* `tertius()` and `tertius_diff()` effects are created. It requires the
  definition of a network and an attribute.

  ```R
  tertius_diff(bilatnet, states$gdp)
  tertius(bilatnet, states$gdp)
  ```

# golfish 1.3.1

## New features

* `estimate()` now estimation routine in `C` for `model = "DyNAM"` and
  `subModel = "choice_coordination"`.

# goldfish 1.3.0 

## New features

* `estimate()` parameter `engine` through `estimationInit` argument control the estimation
  routine used.
  
  ```R
  mod01 <- estimate(callsDependent ~ inertia + recip + trans,
                    model = "DyNAM", subModel = "choice",
                    estimationInit = list(engine = "gather_compute"))
  ```
  
* `estimate()` now estimation routine in `C` except for `model = "DyNAM"` and
  `subModel = "choice_coordination"`.
* New effects definition for `model = "DyNAM"` and `subModel = "rate"`.

## Minor improvements and fixes
* `inertia` fixed bug when `weighted = TRUE`.

## Breaking changes

* `modelType` argument in `estimate()` have been deprecated in favour of `model` 
   and `submodel`.
   
## Internal changes

* New implementation of effects to improve preprocessing time in the initialization
  of the statistical matrices and the reintroduction of a cache object for complex 
  structural network effects.

## Minor improvements and fixes

* Added more examples for `estimate()`.
* Extend documentation of `defineDependentEvents()`, `defineGlobalAttribute()`, 
  `defineNetwork()`, `defineNodes()` and `linkEvents()`.

# goldfish 1.2.1 

* Introduce a new efficient estimation routine.