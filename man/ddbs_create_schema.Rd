% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_utils.R
\name{ddbs_create_schema}
\alias{ddbs_create_schema}
\title{Check and create schema}
\usage{
ddbs_create_schema(conn, name)
}
\arguments{
\item{conn}{a connection object to a DuckDB database}

\item{name}{a character string with the name of the schema to be created}
}
\value{
TRUE (invisibly) for successful schema creation
}
\description{
Check and create schema
}
\examples{
## load packages
library(duckdb)
library(duckspatial)

## connect to in memory database
conn <- dbConnect(duckdb::duckdb())

## create a new schema
ddbs_create_schema(conn, "new_schema")

## check schemas
dbGetQuery(conn, "SELECT * FROM information_schema.schemata;")

## disconnect from db
dbDisconnect(conn)

}
