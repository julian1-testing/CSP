
### CSW

#### test.csw - to test filter constructions,
http://eo2-geonetwork.brgm-rec.fr/geonetwork/srv/en/test.csw


#### curl
curl 'https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml_iso19139.mcp?id=177&styleSheet=xml_iso19139.mcp.xsl' > argo.xml

curl http://localhost:8080/geoserver/csw?service=CSW&version=2.0.2&request=DescribeRecord&typeName=gmd:MD_Metadata

curl http://catalogue-portal.aodn.org.au/geonetwork/csw?service=CSW&version=2.0.2&request=DescribeRecord&typeName=gmd:MD_Metadata


#### GOOD DOCUMENTATION
http://geonetwork-opensource.org/manuals/2.10.4/eng/developer/xml_services/csw_services.html#getrecords



#### GetCapabilities works,  - lists http verbs/methods - GetRecordById, describeRecord etc,
https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetCapabilities&service=CSW&acceptVersions=2.0.2&acceptFormats=application%2Fxml

#### GetCapabilities,
https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetCapabilities&service=CSW


#### DescribeRecord works! - but not that useful,
https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=DescribeRecord&service=CSW&version=2.0.2&acceptFormats=application%2Fxml



#### GetRecords works - but doesn't actually return the ids?
https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecords&service=CSW&version=2.0.2&constraint=AnyText+like+%argo%&constraintLanguage=CQL_TEXT


#### GetRecords,  this returned multiple results
curl 'https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecords&service=CSW&version=2.0.2&constraint=AnyText+like+%argo%&constraintLanguage=CQL_TEXT&resultType=results&maxRecords=100'



#### GetRecords, no search constraint and limit to search
https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecords&service=CSW&version=2.0.2&constraint=AnyText+like+%*%&constraintLanguage=CQL_TEXT&resultType=results&maxRecords=1000       #### *%


url encoding is hard this also works,
curl 'https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecords&service=CSW&version=2.0.2&constraint="csw:AnyText+Like+'%*%'"&constraintLanguage=CQL_TEXT&resultType=results&maxRecords=1000'


#### GetRecordById - retrieves subset of argo record information by csw - note it's not the full record
#### and doesn't tell us the schema

# using a post following - this appears to work except no records retrieved, 
curl -k -v -H "Content-Type: application/xml"   -X POST -d @query.xml 'https://catalogue-123.aodn.org.au/geonetwork/srv/eng/csw'


#### GetRecordById Like this. will 
https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecordById&service=CSW&version=2.0.2&elementSetName=full&id=4402cb50-e20a-44ee-93e6-4728259250d2&outputSchema=csw:IsoRecord

#### GetRecordById This returns the the record in underlying schema format.
https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecordById&service=CSW&version=2.0.2&elementSetName=full&id=4402cb50-e20a-44ee-93e6-4728259250d2&outputSchema=http://www.isotc211.org/2005/gmd

#### GetRecordById, eg. a mcp-1.4 record,
https://catalogue-imos.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecordById&service=CSW&version=2.0.2&elementSetName=full&id=677a6c35-0c34-4479-afd3-cb0d2d091cfa&outputSchema=http://www.isotc211.org/2005/gmd


------------------------------------
#### XML services,

#### xml.search to retrieve all record ids , against a geonetwork - GN specific
curl 'https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search' | xmllint --format - | less


#### xml.search, note there is a limit of retrieving 1000 in xml.search
curl 'https://catalogue-imos.aodn.org.au/geonetwork/srv/eng/xml.search' | xmllint --format - | less

#### xml.metadata.get, get argo
curl 'https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.metadata.get?uuid=4402cb50-e20a-44ee-93e6-4728259250d2'









