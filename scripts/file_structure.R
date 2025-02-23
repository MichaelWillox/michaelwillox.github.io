install.packages("fs")
install.packages("tree")

library(fs)
library(tree)

# Set the path to your local repo
path <- "C:/Users/micha/_Work/Software/michaelwillox.github.io"

# List the files recursively
files <- fs::dir_tree(path)
writeLines(files, "C:/Users/micha/_Work/Software/michaelwillox.github.io/directory_tree.txt")

# # Print the directory tree
# cat(files, sep = "/n")
# 
# 
# library(fs)
# library(igraph)
# library(ggraph)
# library(ggplot2)
# 
# # Function to create an edge list from a directory tree
# create_edges <- function(path) {
#  files <- fs::dir_ls(path, recurse = TRUE)
#  edges <- data.frame(
#   from = dirname(files),
#   to = files
#  )
#  return(edges)
# }
# 
# # Create the edge list
# path <- "C:/Users/micha/_Work/Software/michaelwillox.github.io"
# edges <- create_edges(path)
# 
# # Convert to an igraph object
# graph <- graph_from_data_frame(edges)
# 
# # Plot and save as an image
# plot <- ggraph(graph, layout = "tree") +
#  geom_edge_link() +
#  geom_node_point() +
#  geom_node_text(aes(label = name), hjust = -0.1, vjust = 0.5, size = 3) +
#  theme_void()
# 
# plot
# 
# # Save the plot
# ggsave("directory_tree.png", plot, width = 10, height = 8, dpi = 300)
# 
