{-
    TODO - convert all String to ByteString

    Record structure - containing everything needed for Portal
      modelled after mcp2, but not dependent upon it.

    we work out what we need by looking at
        1. MetaDataRecord.js and seeing the fields
        2. looking at the fields in the response.xml
        3. then mapping the values in response.xml back into original argo.xml record to get tag name

    If any field is genuinely optional then we should use Maybe

    Eg.
    ./web-app/js/portal/data/MetadataRecord.js
        title               done
        abstract            done
        uuid                 done
        parameterField   -   done db join vocab  parameters we have - just join in the db and format
        platform         -   done db field join vocab

        organisationField    **** don't have yet - .

        jurisdictionLink    done
        licenseLink         done
        licenseName         done
        licenseImagelink    done

        attrConstrField -> attrConstr ->
            - more than one in MD_Commons

        otherConstrField ->  also in MDcommons

        otherCitation      ***** does not appear in summary-response
        useLimitationiField done -> useLimitation - more than one

        temperalExtentBegin done -> tempExtentBegin  -> it's just a date
            eg. <tempExtentBegin>1999-10-01t00:00:00.000z</tempExtentBegin>

        linksField          done -> links -> CI_Online_Resource  eg. 'View and download'  transferLinks
        linkedFilesField  -> linkedFiles -> ****** does not appear
        onlineResourcesFiled -> onlineResources ***** does not appear
        pointOfTruthLinkField -> pointOfTruthLink  **** does not appear
        bboxField -> bbox ->
                            -> geoBox  ** don't have for argo.... but do for satellite... i think
                            -> geoPolygon  done.

        wmsLayer             **** does not appear
        iconSourceUuid   -> source -> which is the uuid
-}


module Record where


import qualified Data.ByteString.Char8 as BS(ByteString(..) )


data DataIdentification = DataIdentification {

    title:: String,
    abstract:: String

} deriving (Show, Eq)



data MDCommons = MDCommons {

    jurisdictionLink :: String,
    licenseLink :: String ,
    licenseName :: String ,
    licenseImageLink:: String

} deriving (Show, Eq)



data TransferLink = TransferLink {

    protocol :: BS.ByteString,
    linkage :: BS.ByteString,
    name :: BS.ByteString,
    description :: BS.ByteString
} deriving (Show, Eq)



data DataParameter = DataParameter {

    term :: BS.ByteString, -- eg. label
    url :: BS.ByteString,

                              -- don't think this is correct for parsing. 
    rootTerm :: BS.ByteString-- this is a bit more expensive to compute, this is not used in db. but computed.

} deriving (Show, Eq)



data Record = Record {

    uuid :: Maybe String,
    dataIdentification :: Maybe DataIdentification ,
    mdCommons :: Maybe MDCommons,
    attrConstraints :: [ String ],   -- todo
    useLimitations :: [ String ],    -- todo
    dataParameters :: [ DataParameter ],
    temporalBegin :: Maybe String,   -- todo
    transferLinks :: [ TransferLink ],
    geopoly :: [ BS.ByteString ]            -- this is slow, should be a bytestring?, or indexing issue?
} deriving (Show, Eq)



emptyRecord = Record Nothing  Nothing Nothing [] [] [] Nothing [] []


