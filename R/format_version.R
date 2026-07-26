# Fitted-object layout epoch ----
#
# The epoch a newly fitted result is stamped with. Deliberately an integer
# rather than the package version: it identifies the *shape* of the object
# (which components exist and how they are spelled), so an ordinary release
# that does not touch the layout leaves every stored fit current.
#
# Epoch 2 is the snake_case component set. Epoch 1 objects are not stamped at
# all -- the stamp did not exist -- which is why recognizing one falls back to a
# retired camelCase component.
#
# The preprocessed object has its own, older counter for the same job
# (`PREPROCESSED_GOLDFISH_VERSION`), which already aborts estimation on a stale
# object; it is bumped rather than duplicated here.

goldfish_result_format <- 2L
