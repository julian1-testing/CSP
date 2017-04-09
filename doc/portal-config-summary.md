
Move this 

Focus on the difficult things,


state pain-points
  - popularity order - eg. argo on top
  - greyscale logo subsitution - needed for harmonious presentation
  - wms/wfs assocition - _map _data , multiwms, multiwfs.  
  - wfs download format - csv, shapefile (is a geoserver GetCapabilities, or manual or override) - is .
  - health of service
  - geospatial extent - from talend - would be better.
  - search term representation - mcp2 skos or keyword
  - unique values - eg. filterable properties - currently a service on on geoserver 
  - facet/text search across - multiple geonetworks - achieving by harvesting to one locale - where can build a common index


other pain points
  - in working with full record - we need to transform other records - when only need subset 
  - writing 1.4 to 2, and back again. 
  


what was done.
  - reverse-engineer the javascript portal GN metadata store and look at all the fields
  - tracing the store extraction mechanism to understand representation in the GN xmls.search


- vocab ingest - to parse and store platform and parameter concepts in a relational model - capturing the relationsihps 

- csw harvest code - to retrieve all record ids, and then download records in their native scheme 
- parse the records - to scrape the subset of information needed to drive portal - title, abstract, attr constraints, uselimitations, transfer links/online resources. 
- index the records - using the facet relationships in the vocab 
- do facet propagaion - so can provide the xml summary - with counts propagated up to high-level concepts. 

- so we have a db - with all the basic information - required to drive the front-end 



General harvest mechansism.

  0. seed initial set of geonetworks 
  1. harvest records from geonetworks - and just extract relevant online-resources, search keywords 
      sear 
  2. probe the online-resources for additional service detail - eg. call get_capabilities on service for output_format. 
  3. inject/substitute additional portal state - greyscale logos 
  4. continue to probe online-resources for service availability and layers that need disabling




Advantages.
  - increases the range of supported protocols - 19139, mcp-2.0, mcp-1.4, 19115-1, gcmd?  GA, sensor ML. If an organisation has the necessary content - then we can work with it.
  - eliminates the need need to write complicated transforms from one schema to another
  - Instead we scrape the subset of deatil  necessary for portal. And we store it in whatever form is most convenient.

  - no need to harvest Metadata records in their entirety to a single metadata store (catalogue-portal). So far as I can tell - the only reason this is done - is to support an aggregated search capability across metadata that has POT in another GN. Instead harvest the subset of state (eg. abstract, keywords) needed for search and layers. 


information captured 
  - keywords/facets (aggregated across source GN) - to provide search. eg. under current harvesting model - need one point.
  - online resources/transfer
  - output formats (from getCapabilitieis - from the resource)
  - bad layers   (from probing the online resource)
  - geographic extenet - currently modifies the record. half static/half dynamic 
  - general record fields



