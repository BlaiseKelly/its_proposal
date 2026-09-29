# library
library(igraph)

urb_web = read.csv("data/urban_web.csv")

"#EAE4DA"

# create data:
links <- data.frame(
  source=c("A ","A", "A", "A", "A","A", "A", "A", "A", "B", "D","I"),
  target=c("G","H", "I", "F", "E","C","B", "N", "M", "D", "I","I"),
  importance=(sample(1:4, 12, replace=T))
)

nodes = urb_web |> 
  select(name, charac)


# Turn it into igraph object
network <- graph_from_data_frame(d=links, vertices=nodes, directed=F) 

# Make a palette of 3 colors
library(RColorBrewer)
coul  <- brewer.pal(3, "Set1") 

# Create a vector of color
my_color <- coul[as.numeric(as.factor(V(network)$carac))]

# Make the plot
plot(network, vertex.color=my_color)

plot(network,
     vertex.color = my_color,
     vertex.label = V(network)$carac)

# Both together, e.g. "A" on one line and "young" below it
plot(network, vertex.color = my_color,
     vertex.label = paste0(V(network)$name, "\n", V(network)$carac))

# Tidy up the label appearance
plot(network, vertex.color = my_color,
     vertex.label = V(network)$carac,
     vertex.label.color = "black",
     vertex.label.cex = 0.8,
     vertex.size = 25)      # bigger nodes so longer words fit inside

# Add a legend
legend("bottomleft", legend=levels(as.factor(V(network)$carac))  , col = coul , bty = "n", pch=20 , pt.cex = 3, cex = 1.5, text.col=coul , horiz = FALSE, inset = c(0.1, 0.1))