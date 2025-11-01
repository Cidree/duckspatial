

library(sf)
# sf::st_crs(countries_sf) <- NA
# sf::st_crs(argentina_sf) <- NA



## database setup
conn <- duckdb::dbConnect(duckdb::duckdb())
ddbs_install(conn)
ddbs_load(conn)

## read data
countries_sf <- st_read(system.file("spatial/countries.geojson", package = "duckspatial"))
argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))

brazil_sf <- subset(countries_sf, NAME_ENGL=="Brazil")

test <- sf::st_join(
    x = countries_sf,
    y = argentina_sf,
    join = st_intersects, left = F
)

nrow(countries_sf)
nrow(test)
head(test)

## store in duckdb
ddbs_write_vector(conn, countries_sf, "countries", overwrite = TRUE)
ddbs_write_vector(conn, argentina_sf, "argentina", overwrite = TRUE)
ddbs_write_vector(conn, brazil_sf, "brazil", overwrite = TRUE)

## spatial join: intersects
temp_1 <- ddbs_join(
    conn,
    x = "argentina",
    y = "countries",
    join = "ST_Intersects"
    )

## spatial join: intersects
temp_1 <- ddbs_join(
    conn,
    x = "brazil",
    y = "countries",
    join = "ST_Intersects"
)

temp_1


