# load needed library files

library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)


# If you need to overwrite an existing key:
census_api_key("a645472a3fc238e4bd8f2a5cd8495670e337f87f", overwrite = TRUE, install = TRUE)
# First time, relead your environment so you can use the key without restarting R.
readRenviron("~/.Renviron")
# Check Census API Key
Sys.getenv("CENSUS_API_KEY")


# Generate a list of race variables
racevars <- c(White = "P005003", Black = "P005004", Asian = "P005006", Hispanic = "P004003")

# Import 2010 census block population data by race using generated list of race varialbes and census.gov API
aa_pop_race2010 <- get_decennial(geography = "block", variables = racevars, state = "MD", county = "Anne Arundel County", output = "wide", geometry = TRUE, summary_var = "P001001")

# Check data
head(aa_pop_race)

# Get percentage of black population in County
(sum(aa_pop_race$Black)/sum(aa_pop_race$summary_value))*100

# Read in County Boundary shapefile
aaCounty <- st_read(dsn = '/RStats/RacialGeography/Data/AACounty.shp')

# Reproject census block data to match county boundary projection
aa_pop_raceProj <- st_transform(aa_pop_race, crs = st_crs(aaCounty, asText = TRUE))

# Clip census block data to county boundary
aa_pop_race_clip <- st_intersection(aaCounty, aa_pop_raceProj)

# Plot the results
plot(aa_pop_race_clip["NAME"])

# Generate coordiantes for each dot
# credit to Jens von Bergmann for this algo https://github.com/mountainMath/dotdensity/blob/master/R/dot-density.R

random_round <- function(x) {
  v=as.integer(x)
  r=x-v
  test=runif(length(r), 0.0, 1.0)
  add=rep(as.integer(0),length(r))
  add[r>test] <- as.integer(1)
  value=v+add
  ifelse(is.na(value) | value<0,0,value)
  return(value)
}

# data frame of number of dots to plot for each individual person

num_dots <- as.data.frame(aa_pop_race_clip) %>%
   select(White:Hispanic) %>% 
   mutate_all(funs(. / 1)) %>%
   mutate_all(random_round)

# generates data frame with coordinates for each point + what race it is assiciated with

sf_dots <- map_df(names(num_dots), 
                  ~ st_sample(aa_pop_race_clip, size = num_dots[,.x], type = "random") %>%  # generate the points in each polygon
                    st_cast("POINT") %>%                                               # cast the geom set as 'POINT' data
                    st_coordinates() %>%                                               # pull out coordinates into a matrix
                    as_tibble() %>%                                                    # convert to tibble
                    setNames(c("lon","lat")) %>%                                       # set column names
                    mutate(variable = .x)                                              # add categorical race variable
) %>% 
  slice(sample(1:n())) # once map_df binds rows randomise order to avoid bias in plotting order

# Check first few rows of sf_dots data

head(sf_dots)

# color palette for each race point
pal <- c("White" = "#99BFFF", "Black" = "#66CC99", "Asian" = "#CC0000", "Hispanic" = "#E69000")

# plot it and save as png large enough to avoid over-plotting of the points
ggplot() +
  geom_sf(data = aaCounty, fill = "white", color = NA) +
  geom_point(data = sf_dots, aes(x = long, y = lat), color = variable, size = 0.1, shape = ".", alpha = 0.1) +
  coord_sf(datum = NA) +
  scale_color_brewer(palette = "Set1", guide = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "", y = "", title = "The racial geography of Anne Arundel County", subtitle = "2010 decennial U. S. Census", fill = "", caption = "1 dot = 1 person.\nData acquired with R tidycensus package.")
  