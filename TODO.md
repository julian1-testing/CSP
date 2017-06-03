
- understand exceptions in context of db handling etc..


- image from db. if only to prove it works.
    -- need to understand conn handling only issue is db conn handling.


- popularity


tidy QQ imports and qualify them. eg.
  import Text.RawString.QQ(r)


- bounding box for gridded data....

---

done - attribution - on download, use limitation , license link info

done - change record uuid etc to bytestring.

done for griddedi - work out why the polygon geopoly isn't working? 

done - other links - wfs downloads

done - externalize DB connection details for app and test. eg. Config.hs

done - free text search



done - get rid of the the horrible passing the record into the record decodd functions


done - transfer links... for wms at least

done - maybe it mucks up links because of id. or because of lacking link for wms.


done - get rid of the horrible record passing stuff where pass the current record state into the record decoding... 

done - fsSearchAddBtn is missing - 
  - perhaps because parsing bails out too early...
  - or maybe it's taken from the uuid in the pot link or something?

done - root parameter/platform
done - geopoly

done - organisation

done appears as if there is an issue with parameters being represented more than once - because harvested more than once.

done - get rid of geopoly again - and check xml nesting for geopoly doesn't use  - because we can see all the images properly.

-----
done - freetext search

done - - would be good to get the organisation in - since it fills a lot of the space.

done - - need to check for errors in portal geonetwork store.
    - it may be some id/uuid is being used as a css class to target the expansion box element etc..

----

OLD

----
-- need a simple concept limited query - for faceted search...
  - then develop the full nested matching
    - should work - with just a recursive generated function with parent_url = ... etc. 
    - but also need or.


---

-- done CHANGE INTEGER to INT

- done - also set the image explicitly in the metadata that we return - should be trivial  - and demonstrate it working...

- done - do the "to" and "from" to test the param decoding...


---

done - get portal rhs panel with some data as well as lhs.

- done - need to fill in extra metadata data.
- done -  and get the actual selection criteria to work.

- get the abstract in place and the logos.

- need to be able to log the non-logo requests.

----

-- done TODO Change to name to summary ?
  -- Summary
  -- and change FacetRequest to Search


https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=1&to=1&fast=index&filters=collectionavailability

- need to return and tack on metadata
  - need test code to format this....
    - metadataFormat?

- need to use the record - list - to generate stuff and xml format it - before we do anything else 

- analyze the real portal - query - and break it down into tables from there...

- extract visual database structure - to png. etc.
- extract Config references for Scheme loading to a Config.hs
    eg. postgres url, port
    schema
    catalogue. to scan

- identify the damn query.

- pull out the transform for the query selection
  - two stages
    - matching records
    - records matching 

    - then make unique - no concept is a bucket.
   
  This is the fundamental requirement 
    concept -> [ records ]

- rename facet to ConceptRecord. when dealing with the join of Concept and Record


1.  check the complete harvest
    - issue with empty fields.
1. store other fields


------
Ok - now we need to massage the XML

----
done - why doesn't the fucking thing, propagate to the parent Nothing node?....
  - we need to check that we actually have a relationship to the parent.


done get recusion and tree output working again? - on subset?
  - we only need the counts on the category - so we
      store the coutn associated with the node.
      and then do the monadic recursion.

  - done can either use text
  - or use hxt. need to - think about hxt graph construction...


done - fix platform indexing



Thing we should perhaps 


--------
It's a graph. 
Which makes the whole thing rather complicated.


OK - we can't just count the records. - 


------------

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





