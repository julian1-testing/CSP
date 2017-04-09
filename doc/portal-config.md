

state - for portal config, what, and sources 

  - wms-wfs mapping. done by convention - _map and _url  multi-wms, multi-wfs
  - general record fields
  - keywords/facets (aggregated across source GN) - to provide search. eg. under current harvesting model - need one point.
  - online resources - wms, wfs, 
  - output formats (from getCapabilitieis - from the resource - eg. get)
  - health - bool for bad layers
  - geographic extent - currently modifies the record. half static/half dynamic 
  - filterable properties - from geoserver


- geographic extent - does it really want to be encoded in the metadata.
  - or should we be querying geoserver for this information.

- VERY IMPORTANT
  - disabling layers. that are not responding.


- VERY IMPORTANT
  - hard-coding config such as known servers in chef and ebprep doesn't mean it's not state. It's just state that requires
    using going through chef to manipulate. 
 


- VERY IMPORTANT
  - need to combine metadata record for online resource, with geoserver (eg. getCapabilities for output format type), 
    with geospatial extent.  
    - eg. scrape for known servers. 
    - metadata (xml.get) -> wms server (get-capabilities) -> specifics of the service.

    LIKE GEOWEBCACHE - probe.


- VERY IMPORTANT
  - geographic extent - dynamic changed by talend - but is that really desirable?


- VERY IMPORTANT
  - don't even need to translate between schemas. 
  - just need to extract relevant info.

- VERY IMPORTANT
  - need to be able to inject info on top - eg. logo image.  associatively by record.


- REPEATING ALL INFORMATION of known servers in Portal.
    id, wms version, output_format type, 
  
- VERY IMPORTANT portal config. 
    - text / facet search  service
    - known servers static config .
    - online resource. 
 
  - initially used to restrict as open-proxy now holding lots of state about service .

 
    vim ./data_bags/imos_webapps_portal_known_servers_configs/prod.json 
    vim ./data_bags/imos_webapps_portal_known_servers/aodn_geoserver_123.json


- VERY IMPORTANT
  - there are two distinct ways of configuring - for po box, local dev. And for beanstalk.  


- issue - resources eg. a data.zip file - then we want to refer to the original catalogue service? 
    - or does portal do this.


- A harvester to pull in the data
- A transform - to extract what we need from existing metadat protocols
  - Visitor class.
- search functionality,
- xml.search records

sever the relationship between portal and GN

VERY IMPORTANT
  - get the portal gn database - and use that to start with - as an initial set of records, and test the free-text search...
  - xslt2 to transform or something else. perhaps something else. - because must translate into insert statements.
  - haskell hxt to transform? - complicated arrows
  - haskell xml-conduit - old 


protocols, - need to handle them all.
  - 19139, 
  - mcp-2, 
  - 19115-1
  - Irena /GA


- vocab
- online resources - wms, wfs, data file for download (soop-ba), 
  
output_format (shapefile) is a geoserver capability

state 
  - db, 
  - or git, 
  - or something else. 

api 
  - db only.
  - webapi  (make work with existing)
  - integrated in portal


api simplification for portal
  - eg. get things back in native json. but  

harvest xml and scrape -> 


whether to potentially forward requests to the source GN


local state versus forward request and transform
  - probably need state - for speed 


document (xml, json, yaml) versus relational
  - relational - need database - normally no change tracking
  - document - changesets - could use git.
  - files are super easy - track change with git. checkout with git, manage versions (prod versus rc) with branch, but doesn't fit with harvest 

  - if db, then should track changes - even if just xml diff. 

  - if expect more static structure - then relational may be better, as don't have to remodel the database

  - advantage relational - easy to see all the collection information
      organises faceted search, nicely by term 


database
  - index on insertion
  - can keep change-history like git  - eg. ingest on a queue.
  - portal could directly connect to it
  - handles xslt-1, can embed saxon in java/pl java extension for two


  - can hold config for how often to harvest.
  - can hold config for harvest ingest/transform - if we 


files
  - appears simpler.
  - git - but doesn't match harvest structure


  
the operation is a bit like setting up index-fields

- hiding items - when bad health.



search
  - lucene
  - solr - standalone full text search server (uggh), solr is a lucene project
  - postgres - free text search - or index?


separate service, or integrated in portal, or library in any context
  - 

  - the server could just a rest api on a set of postgres views - postgres can do the json .  


full text indexing - supported by postgres.
  https://www.postgresql.org/docs/9.5/static/textsearch-intro.html#TEXTSEARCH-MATCHING

xslt in postgres
  https://nbsoftsolutions.com/blog/xslt-2-0-processing-in-postgres

HXT intro tutorial,
  https://wiki.haskell.org/HXT

  see 4.4 Tree traversal filter 
  and 4.5 Arrows - for doing IO in response, and carrying state. 



