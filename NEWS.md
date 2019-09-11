# Changes in goldfish 1.3.0 

## New features

- Estimation routine in `C` except for `model = "DyNAM"` and `subModel = "choice_coordination"`. 
  Parameter `engine` through `estimationInit` argument control the estimation routine used by `estimate()`.  
- New effects definition for `model = "DyNAM"` and `subModel = "rate"`.

## Bug fixes
- Bugfix in `inertia` when `weighted = TRUE`.

## Internal changes
- `modelType` argument in `estimate()` have been deprecated in favour of `model` 
   and `submodel`.
- New implementation of effects to improve preprocessing time in the initialization 
  of the statistical matrices and the reintroduction of a cache object for complex 
  structural network effects.

## Documentation
- Added more examples for `estimate()`.
- Extend documentation of `defineDependentEvents()`, `defineGlobalAttribute()`, 
  `defineNetwork()`, `defineNodes()` and `linkEvents()`.

# Changes in goldfish 1.2.1 

- Introduce a new efficient estimation routine.