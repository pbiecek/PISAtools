getCountryIdsFromRegion <- function(region = "Europe", vname="ISO3", ontology=countryOntology) {
  as.character(ontology[grep(ontology$IS_IN_GROUP, pattern=region, ignore.case=TRUE), vname])
}

