# title: "Create a reference list of country neighbors"
# author: "Jesse Blanton"
# date: "9/9/2024"
# date_modified: "09/09/2024"

# Install libraries
source("Libraries_Dependencies.R")


# Download the world boundaries data
world <- gb_adm0()  # Gets the global country boundaries

# Ensure all geometries are valid
world <- st_make_valid(world)


# Simplify to speed up processing. Adjust the tolerance value as needed (higher values simplify more)
world <- st_simplify(world, dTolerance = 0.1, preserveTopology = TRUE)

# Build a spatial index to speed up spatial queries
st_agr(world) = "constant"  # Set attribute-geometry relationship
world <- st_make_valid(world)

# Initialize a list to store neighbors
neighbors_list <- list()

# Loop through each country and find its neighbors [this takes a long time to run]
for (i in seq_len(nrow(world))) {
  # Get the target country
  target_country <- world[i, ]
  
  # Find neighboring countries
  neighbors <- world %>%
    st_filter(target_country, .predicate = st_intersects) %>%
    pull(shapeName)
  
  # Exclude the target country from its neighbor list if included
  neighbors <- setdiff(neighbors, target_country$shapeName)
  
  # Store the neighbors in the list
  neighbors_list[[target_country$shapeName]] <- neighbors
  
  print(i)
}

# Convert the list to a data frame
countryneighbors_df <- data.frame(
  country = names(neighbors_list),
  neighbors = sapply(neighbors_list, function(x) paste(x, collapse = ", ")),
  stringsAsFactors = FALSE
)

# Save the data frame for future use
write.csv(countryneighbors_df, "Inputs/country_neighbors.csv", row.names = FALSE)

# Now you can load 'country_neighbors.csv' and use it for quick lookups

