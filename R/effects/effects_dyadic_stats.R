register_effect(
  "tie",
  init = function(effectFun, network, window, n1, n2, ...) {
    # get arguments
    params <- formals(effectFun)
    weighted <- eval(params[["weighted"]])
    funApply <- eval(params[["transformer_fn"]])
    
    # has window or is empty initialize empty
    if ((!is.null(window) && !is.infinite(window)) || all(network == 0)) {
      value <- if (weighted) forceAndCall(1, funApply, 0) else 0
      return(list(stat = matrix(value, nrow = n1, ncol = n2)))
    }
    
    if (weighted) {
      stat <- forceAndCall(1, funApply, network)
    } else {
      stat <- 1 * (network > 0)
    }
    return(list(stat = unname(stat)))
  },
  update = function(
    network,
    sender, receiver, replace,
    weighted = FALSE, transformer_fn = identity) {
    # No change check, irrelevant for two-mode network
    # if(sender == receiver) return(NULL)
    
    # init res
    res <- list(changes = NULL)
    
    # Get old value
    oldValue <- network[sender, receiver]
    
    # change for weighted effect
    if (!weighted) {
      oldValue <- sign(oldValue)
      replace <- sign(replace)
    }
    
    # If the old value of the tie is the same as the replace value
    if (oldValue == replace ) {
      return(res)
    }
    
    # change stat
    res$changes <- cbind(
      node1 = sender,
      node2 = receiver,
      replace = if (!weighted) {
        1 * (replace > 0)
      } else {
        forceAndCall(1, transformer_fn, replace)
      }
    )
    
    return(res)
  },
  meta = list(
    label = "Tie Value",
    description = "Represents the value of a specific tie.",
    family = "structural"
  ),
  args_schema = list(
    weighted = list(default = FALSE, allowed = c(TRUE, FALSE)),
    transformer_fn = list(default = "identity", allowed = "function") # Store as string, convert later
  )
)
