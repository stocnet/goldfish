# define functions deprecation

    Code
      defineNodes(data.frame(label = "a"))
    Condition
      Warning:
      `defineNodes()` was deprecated in goldfish 1.7.0.
      i Please use `make_nodes()` instead.
    Output
      Number of nodes: 1 
      
      First 1 rows
        label
      1     a

---

    Code
      defineGlobalAttribute(data.frame(time = 1, replace = 1))
    Condition
      Warning:
      `defineGlobalAttribute()` was deprecated in goldfish 1.7.0.
      i Please use `make_global_attributes()` instead.
    Output
        time replace
      1    1       1

---

    Code
      mat <- defineNetwork(mat, nodes_df)
    Condition
      Warning:
      `defineNetwork()` was deprecated in goldfish 1.7.0.
      i Please use `make_network()` instead.

---

    Code
      mat <- linkEvents(mat, events_df, nodes_df)
    Condition
      Warning:
      `linkEvents()` was deprecated in goldfish 1.7.0.
      i Please use `link_events()` instead.

---

    Code
      defineDependentEvents(events_df, nodes_df, default_network = mat)
    Condition
      Warning:
      `defineDependentEvents()` was deprecated in goldfish 1.7.0.
      i Please use `make_dependent_events()` instead.
    Output
      Number of events: 3 
      Nodes set(s): nodes_df 
      Default network: mat 
      
      First 3 rows
        time sender receiver increment
      1    1      a        b         1
      2    2      a        b         1
      3    3      a        b         1

# define dependent events deprecated

    Code
      defineGroups_interaction(records_df, actors_df, seed_randomization = 123)
    Condition
      Warning:
      `defineGroups_interaction()` was deprecated in goldfish 1.7.0.
      i Please use `make_groups_interaction()` instead.
    Message
      
      -- DyNAM-i interaction groups --------------------------------------------------
      1 layer, 8 nodes, 4 ties.
      
      -- Layers 
      * interactions (focal): event, directed, increment

