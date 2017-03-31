
- done - do resources

- refine selection criteria - to harvest several files - and then try to implement search
- need more indexing. (refine query to mcp2)
  - separate out the index into an independent action - so that can run it again?
- try to construct return value - with loop.
  - maybe able to do this without full indexing.

------

- online resources
- vocab/ skos - facet - and search term
- wms - wfs association. 


- get the vocab - represented and then try to spit it out again...
  - maybe if get the list in sql, then do the recusion in haskell?


Catalog services for Web  (CSW).


so we want to 
  done - 1. - get list of all metadata records from a single GN
  done - get individual records and pull stuff
    but need to limit the resources - to actual downloadable links.

  done - store the fields to db.

wnat keywords

  2. - scan the getCapabilities


  extract vocab term urls - so we know what they look like
  add the vocab files - either at startup - on in db.
  construct the query term


facets,

https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&facet.q=Measured%2520parameter%2FPhysical-Atmosphere%2FAir%2520temperature&fast=index&filters=collectionavailability



geospatial - 

  getcapabilities
    - bounding box
    - also get features
    - outputformat
    - uniquevalues - 

    - filter configuration - filterable properties - association wms / and wfs

sos - sensor observation sevice. 
  - spatial bound.
  - domain - range. eg. unique values.





