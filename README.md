# RshinyApps

some shiny apps

---

**how to run in R:** 

```
library(shiny)

runGitHub("RshinyApps", "niels-flohr", subdir = "natural_streets_vienna/")
```

see also https://github.com/rstudio/shiny_example

---

## Natural Streets Explorer Vienna

- Spatial Analysis Parameters:  range of spatial property parameters derived from the connectivity graph

  - Connectivity: number of nodes directly linked to each individual node in the connectivity graph

  - Control value: parameter which expresses the degree of choice each node represents for nodes directly linked to it

  - Depth: defined as the number of steps from a considered node to all other nodes (deep = many steps separating one node from other nodes, shallow = only few steps)

  - Integration: expressed by a value that indicates the degree to which a node is integrated or segregated from a system as a whole (global integration) or from a partial system consisting of nodes a few steps away (local integration) 
