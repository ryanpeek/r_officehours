# edge/vertex share: from https://rpubs.com/cyclemumner/642015

library(sf)

nc <- sf::read_sf(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
library(silicate)

x <- SC(nc)
objects <- sc_object(x) %>% dplyr::select(object_)  ## objects (SF calls these "features")
nobj <- nrow(objects)

set.seed(100)
nc$colour <- sample(viridis::viridis(nobj))

for (ith in seq_len(nobj)) {
  ## objects by edge
  u_edge <-   dplyr::slice(objects, ith) %>%
    dplyr::inner_join(x$object_link_edge, "object_") %>%
    dplyr::inner_join(sc_edge(x), "edge_") %>%
    dplyr::select(edge_) %>%
    dplyr::inner_join(x$object_link_edge, "edge_") %>%
    dplyr::distinct(object_)
  u_vertex <-
    dplyr::slice(objects, ith) %>%
    dplyr::inner_join(x$object_link_edge, "object_") %>%
    dplyr::inner_join(sc_edge(x), "edge_") %>%
    tidyr::pivot_longer( cols = c(.vx0, .vx1), names_to = "node", values_to = "vertex_") %>%
    dplyr::inner_join(sc_vertex(x), "vertex_") %>%
    dplyr::select(vertex_) %>%
    dplyr::inner_join(tidyr::pivot_longer(sc_edge(x),  cols = c(.vx0, .vx1), names_to = "node", values_to = "vertex_"), "vertex_" ) %>%
    dplyr::inner_join(x$object_link_edge, "edge_") %>%
    dplyr::distinct(object_)

  e_idx <- match(u_edge$object_, objects$object_)
  v_idx <- match(u_vertex$object_, objects$object_)
  ## only look at it if the set of neighbours is different by vertex vs. edge
  if (!length(e_idx) == length(v_idx)) {
    par(mfrow = c(3, 1), mar = c(rep(0.75, 4)))
    
    # Target county
    plot(nc[1], col = "firebrick", main = "Target county", reset = FALSE) ## this format requires special care to make a map
    plot(nc[ith, 1], add = TRUE, col = nc$colour[ith])
    
    plot(nc[, 1], col = "firebrick", main = "Edge share", reset = FALSE); plot(nc[e_idx, 1], add = TRUE, col = nc$colour[e_idx])
    plot(nc[, 1], col = "firebrick", main = "Vertex share", reset = FALSE); plot(nc[v_idx, 1], add = TRUE, col = nc$colour[v_idx])
    print(v_idx)
    print(e_idx)
  }
  
}
