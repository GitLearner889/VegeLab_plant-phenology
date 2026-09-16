
### Inverse Distance Weighting
## ROLE: 
#THis function estimates values for unknown spatial location from known data of determined locations and values by using the principle of the closer point are the more similare the values should be. The exact formula : the sum of values of all points weighted by the distance of the points to the point of interpolation, divided by the sum of the distance bteween all the points and the interpolation point.
# All about it on: https://en.wikipedia.org/wiki/Inverse_distance_weighting

## ARGUMENTS:
# empty_positions: dataframe of position of all unknown points: id, location
# data_positions: dataframe of position of all known points: id, location
# data_values: dataframe of values of all known points: id, value (e.g. temperature) 
# beta: power parameter that give the speed at which weigth decline with distance (great beta = only close sites matters) ; usually between 1 and 4
# radius: distance above which the points should not be considered to calculate the unknown value 


# maximum radius not yet implemented

idw_point <- function(empty_positions, data_positions, data_values, beta, radius = NULL){
  
  require(data.table)
  require(sf)
  require(DescTools)
  
  empty_positions_sf <- st_as_sf(empty_positions, coords = names(empty_positions),remove=F) %>% 
    st_set_crs(4326)
  
  data_positions_sf <- st_as_sf(data_positions, coords = names(data_positions),remove=F) %>% 
    st_set_crs(4326)
  
  distances <- st_distance(data_positions_sf,empty_positions_sf)^beta
  
  thenumerator <- as.data.table(t(data_values/distances))
  thedenominator <- as.data.table(t(1/distances))
  
  thenumerator[, numerator := rowSums(.SD)]
  thedenominator[, denominator := rowSums(.SD)]
  
  calctable <- cbind(empty_positions,thenumerator[,'numerator'],thedenominator[,'denominator'])
  calctable[, output := numerator/denominator]
  
  return(calctable$output)
  
}

