
IMPORTANT - in firefox filter console with 'proxy'
  - it's one request and then a bunch of logos,

https://portal.aodn.org.au/proxy?url=https%3A%2F%2Fcatalogue-portal.aodn.org.au%2Fgeonetwork%2Fsrv%2Feng%2Fxml.search.imos%3Fprotocol%3DOGC%253AWMS-1.1.1-http-get-map%2520or%2520OGC%253AWMS-1.3.0-http-get-map%2520or%2520IMOS%253ANCWMS--proto%26sortBy%3Dpopularity%26from%3D1%26to%3D10%26fast%3Dindex%26filters%3Dcollectionavailability&_dc=1491004829710


# url decode -  and strip the _dc
https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&fast=index&filters=collectionavailability

https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search.imos
?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto
&sortBy=popularity
&from=1&to=10
&fast=index
&filters=collectionavailability


ok, so the protocol is basically getmap and the ncwms stuff. 

select * from resource_view where protocol = 'OGC:WMS-1.1.1-http-get-map' ;


# this does facet counts,
select  concept.id, concept.url as concept_id, count(facet.record_id) from concept  left join facet on facet.concept_id = concept.id  group by concept.id;


- so we just need to recurse the tree structure - and print the dimensions...


# specific
https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=10&facet.q=Measured%2520parameter%2FPhysical-Atmosphere%2FAir%2520temperature&fast=index&filters=collectionavailability


https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search.imos
?protocol=OGC%3AWMS-1.1.1-http-get-map%20
or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto
&sortBy=popularity
&from=1
&to=10
&facet.q=Measured%2520parameter%2FPhysical-Atmosphere%2FAir%2520temperature
&fast=index
&filters=collectionavailability


difficult bit is the offset - to from stuff.

-- okk so lets just try to get the db query working.
  might be easier to log the ggi


-- so we are going to need a webserver... but first step


---
- done - do resources

- done - refine selection criteria - to harvest several files - and then try to implement search
- done - need more indexing. (refine query to mcp2)
- done separate out the index into an independent action - so that can run it again?

- try to construct return value - with loop.
  - maybe able to do this without full indexing.

------

- online resources
- vocab/ skos - facet - and search term
- wms - wfs association. 


- done get the vocab - represented and then try to spit it out again...
  - maybe if get the list in sql, then do the recusion in haskell?


done - 1. - get list of all metadata records from a single GN
done - get individual records and pull stuff
  but need to limit the resources - to actual downloadable links.


done - extract vocab term urls - so we know what they look like
done -  add the vocab files - either at startup - on in db.


Catalog services for Web  (CSW).
facets,


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





