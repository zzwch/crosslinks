## code to prepare `columns` dataset goes here

node_colors <- RColorBrewer::brewer.pal(11, "Spectral")
edge_colors <- RColorBrewer::brewer.pal(12, "Paired")
# nodes
nodes <- data.frame(
  id = c(paste0("Gene", 1:10), paste0("Meth", 1:10),
         paste0("Mir", 1:6), paste0("Drug", 1:8),
         paste0("Tar", 1:8), paste0("Path", 1:6)),
  type = c(rep("Gene", 10), rep("Meth", 10),
           rep("Mir", 6), rep("Drug", 8),
           rep("Tar", 8), rep("Path", 6)),
  color = sample(node_colors, 48, replace = T),
  size = sample(1:4, 48, replace = T),
  alpha = sample((5:10)/10, 48, replace = T),
  shape = sample(1:20, 48, replace = T)
)

# edges
edges <- data.frame(rbind(
  # gene vs mir
  data.frame(
    source = sample(nodes$id[nodes$type == "Gene"],
                    replace = T, 30),
    target = sample(nodes$id[nodes$type == "Mir"],
                    replace = T, 30)),
  # gene vs drug
  data.frame(
    source = sample(nodes$id[nodes$type == "Gene"],
                    replace = T, 100),
    target = sample(nodes$id[nodes$type == "Drug"],
                    replace = T, 100)),
  # meth vs drug
  data.frame(
    source = sample(nodes$id[nodes$type == "Meth"],
                    replace = T, 100),
    target = sample(nodes$id[nodes$type == "Drug"],
                    replace = T, 100)),
  # mir vs drug
  data.frame(
    source = sample(nodes$id[nodes$type == "Mir"],
                    replace = T, 20),
    target = sample(nodes$id[nodes$type == "Drug"],
                    replace = T, 20)),

  # drug vs target
  data.frame(
    source = nodes$id[nodes$type == "Drug"],
    target = nodes$id[nodes$type == "Tar"]),
  # target vs path
  data.frame(
    source = sample(nodes$id[nodes$type == "Tar"],
                    replace = T, 15),
    target = sample(nodes$id[nodes$type == "Path"],
                    replace = T, 15))
  ),
  color = sample(edge_colors, 273, replace = T),
  type = sample(1:3, 273, replace = T),
  alpha = sample((5:10)/10, 273, replace = T),
  shape = sample(1:20, 273, replace = T))

# gene annotation
geneData <- data.frame(
  gene = nodes$id[nodes$type == "Gene"],
  lfc = runif(length(nodes$id[nodes$type == "Gene"]), 0, 6))

# meth annotation
methData <- data.frame(
  meth = nodes$id[nodes$type == "Meth"],
  lfc = runif(length(nodes$id[nodes$type == "Meth"]), -0.5, -0.2))

# mir annotation
mirData <- data.frame(
  mir = nodes$id[nodes$type == "Mir"],# set the order as you want
  lfc = runif(length(nodes$id[nodes$type == "Mir"]), -4,0))

columns <- list(
  Gene = nodes$id[nodes$type == "Gene"],
  Drug = nodes$id[nodes$type == "Drug"],
  Target = nodes$id[nodes$type == "Tar"], # set the order as you like
  Pathway = nodes$id[nodes$type == "Path"]# set the order as you like
)
usethis::use_data(edges, nodes, columns, geneData, methData, mirData, overwrite = T)
