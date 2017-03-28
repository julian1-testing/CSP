
# should be be using CSW or just xml.search?
#


curl 'https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml_iso19139.mcp?id=177&styleSheet=xml_iso19139.mcp.xsl' > argo.xml

curl http://localhost:8080/geoserver/csw?service=CSW&version=2.0.2&request=DescribeRecord&typeName=gmd:MD_Metadata

curl http://catalogue-portal.aodn.org.au/geonetwork/csw?service=CSW&version=2.0.2&request=DescribeRecord&typeName=gmd:MD_Metadata

# GOOD DOCUMENTATION
http://geonetwork-opensource.org/manuals/2.10.4/eng/developer/xml_services/csw_services.html#getrecords



# get capabilities works,
# this lists functions - GetRecordById, describeRecord etc,
https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetCapabilities&service=CSW&acceptVersions=2.0.2&acceptFormats=application%2Fxml

# or just,
https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetCapabilities&service=CSW



# DescribeRecord works!
https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=DescribeRecord&service=CSW&version=2.0.2&acceptFormats=application%2Fxml



# GetRecords works - but doesn't actually return the ids?
https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecords&service=CSW&version=2.0.2&constraint=AnyText+like+%argo%&constraintLanguage=CQL_TEXT


# ok, this returned multiple results
curl 'https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecords&service=CSW&version=2.0.2&constraint=AnyText+like+%argo%&constraintLanguage=CQL_TEXT&resultType=results&maxRecords=100'



# no search constraint and limit to search
https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecords&service=CSW&version=2.0.2&constraint=AnyText+like+%*%&constraintLanguage=CQL_TEXT&resultType=results&maxRecords=1000


# GetRecordById - retrieves subset of argo record information by csw - note it's not the full record
# and doesn't tell us the schema,
https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecordById&service=CSW&version=2.0.2&elementSetName=full&id=4402cb50-e20a-44ee-93e6-4728259250d2

Doc says it should be returned in full schema,

http://reference1.mapinfo.com/software/spatial_server/english/1_0/csw/postget/postgetgetrecordbyid.html

Specifies the URI for the schema to return the record. If either the
outputSchema is not specified, or the CSW service cannot generate the record in
the specified schema, then the record will be returned in the native schema
specified for the record.

schemas defaults to,
http://www.opengis.NET/CAT/CSW/2.0.2.


# Like this. will 
https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecordById&service=CSW&version=2.0.2&elementSetName=full&id=4402cb50-e20a-44ee-93e6-4728259250d2&outputSchema=csw:IsoRecord

# This returns the the record in underlying format.
https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecordById&service=CSW&version=2.0.2&elementSetName=full&id=4402cb50-e20a-44ee-93e6-4728259250d2&outputSchema=http://www.isotc211.org/2005/gmd

# eg. a mcp-1.4 record,
https://catalogue-imos.aodn.org.au/geonetwork/srv/eng/csw?request=GetRecordById&service=CSW&version=2.0.2&elementSetName=full&id=677a6c35-0c34-4479-afd3-cb0d2d091cfa&outputSchema=http://www.isotc211.org/2005/gmd


------------------------------------
# XML services,

# xml.search to retrieve all record ids , against a geonetwork - GN specific
curl 'https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.search' | xmllint --format - | less


# note there is a limit of retrieving 1000 in xml.search
curl 'https://catalogue-imos.aodn.org.au/geonetwork/srv/eng/xml.search' | xmllint --format - | less

# get argo via xml.metadata.get 
curl 'https://catalogue-portal.aodn.org.au/geonetwork/srv/eng/xml.metadata.get?uuid=4402cb50-e20a-44ee-93e6-4728259250d2'









