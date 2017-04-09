


#### state pain-points
  - popularity order - eg. argo on top
  - greyscale logo subsitution - needed for harmonious presentation
  - wms/wfs assocition - _map _data , multiwms, multiwfs.
  - wfs download format - csv, shapefile (point-of-truth is a geoserver GetCapabilities call, for external - may need to override - if download frontend doesn't support - eg. shapefile.
  - health of service-endpoint - need to remove layer.
  - geospatial extent - currently talend updates  - may be better from gs which has direct access to postgres-bounding functions. 
  - search-term forms - mcp2 skos or freetext/keyword index
  - unique values - eg. filterable properties - currently a service on on geoserver 
  - facet/text search across - multiple geonetworks - achieving by harvesting to one locale - where can build a common index


#### other pain points
  - in working with full record - we need to transform between schemas - when for Portal presentation - only require a subset of information.
  - writing mcp1.4 to mcp2, and back again. combinatorial increase in transforms avoided - if scrape subset and store in common-form.



#### what was done this iteration.
  - reverse-engineer the javascript portal GN metadata store and look at all the fields
  - tracing the store extraction mechanism to understand representation in the GN xmls.search


  - vocab ingest - to parse and store skos platform and parameter concepts in a relational model - capturing the broader/narrower/scheme relationsihps

  - csw harvest code - to retrieve all record ids, and then download records in their native scheme

  - parse the records - to scrape the subset of information needed to drive portal - title, abstract, attr constraints, uselimitations, transfer links/online resources. 

  - index the records - using the facet relationships in the vocab

  - code to do facet propagaion - with counts propagated up to high-level concepts - and xml formatted the same as GN xml.search summary.

  - so have a db - with most of the basic structure/information - needed to drive the front-end . DB could be in-memory.



#### Advantages.
  - increases the range of supported protocols - 19139, mcp-2.0, mcp-1.4, 19115-1, gcmd?  GA, sensor ML. If an organisation has the necessary content - then we can work with it.
  - eliminates the need need to write complicated transforms from one schema to another
  - Instead we scrape the subset of deatil  necessary for portal. And we store it in technically most convenient form - with fastest access patterns.
  
  - no need to harvest Metadata records in their entirety to a single metadata store (catalogue-portal). So far as I can tell - the only reason this is done - is to support an aggregated search capability across metadata that has POT in another GN. Instead harvest the subset of state (eg. abstract, keywords) needed for search and layers.



#### General harvest mechansism.
  
  0. seed initial set of geonetworks 
  1. harvest records from geonetworks - and just extract relevant online-resources, search keywords
      sear 
  2. probe the online-resources for additional service detail - eg. call get_capabilities on service for output_format.
  3. inject/substitute additional portal state - greyscale logos
  4. continue to probe online-resources for service availability and layers that need disabling



#### Current information captured 
  - keywords/facets (aggregated across source GN) - to provide search. eg. under current harvesting model - need one point.
  - online resources/transfer
  - output formats (from getCapabilitieis - from the resource)
  - bad layers   (from probing the online resource)
  - geographic extenet - currently modifies the record. half static/half dynamic 
  - general record fields


