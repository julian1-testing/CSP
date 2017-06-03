{-

    Record structure - containing everything needed for Portal
      modelled after mcp2, but not dependent upon it.

    revese engineer steps, 
        1. MetaDataRecord.js for fields
        2. looking at the fields in the response.xml
        3. then mapping the values in response.xml back into original argo.xml record to get tag name

      optional field use Maybe
      multiple fields use List

    Eg.
    ./web-app/js/portal/data/MetadataRecord.js
        title               done
        abstract            done
        uuid                 done
        parameterField   -   done db join vocab  parameters we have - just join in the db and format
        platform         -   done db field join vocab

        organisationField    **** not marked in metadata

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

    title :: BS.ByteString,
    abstract :: BS.ByteString

} deriving (Show, Eq)



data MDCommons = MDCommons {

    jurisdictionLink :: BS.ByteString,
    licenseLink :: BS.ByteString,
    licenseName :: BS.ByteString,
    licenseImageLink:: BS.ByteString

} deriving (Show, Eq)



data TransferLink = TransferLink {

    protocol :: BS.ByteString,
    linkage :: BS.ByteString,
    name :: BS.ByteString,
    description :: BS.ByteString
} deriving (Show, Eq)



data DataParameter = DataParameter {

    term :: BS.ByteString, -- ie. pref label
    url :: BS.ByteString,

    -- the vocabulary used - eg. AODN Platform Category Vocabulary
    -- this is a computed property constructed with sql view. and not a serialized in the db
    -- convenient, but maybe should be removed from here
    rootTerm :: BS.ByteString

} deriving (Show, Eq)


data Record = Record {

    source :: Maybe BS.ByteString, -- should potentially be a datastructure to a source with an id.

    uuid :: Maybe BS.ByteString,
    dataIdentification :: Maybe DataIdentification ,
    mdCommons :: Maybe MDCommons,
    attrConstraints :: [ BS.ByteString ],
    useLimitations :: [ BS.ByteString ],
    dataParameters :: [ DataParameter ],
    temporalBegin :: Maybe BS.ByteString,   -- todo
    transferLinks :: [ TransferLink ],
    geopoly :: [ BS.ByteString ]            -- this is slow? or is there an indexing issue
} deriving (Show, Eq)


emptyRecord = Record Nothing Nothing Nothing Nothing [] [] [] Nothing [] []


-- some tests in RecordGet


