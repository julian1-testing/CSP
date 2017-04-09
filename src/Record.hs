{-
    Record structure - containing everything needed for Portal

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


-- why is there no uuid????

data DataIdentification = DataIdentification { 

    title:: String, 
    abstract:: String, 
    jurisdictionLink :: String, 
    licenseLink :: String , 
    licenseName :: String , 
    licenseImageLink:: String
} deriving (Show, Eq)


data TransferLink = TransferLink {

    protocol :: String,
    linkage :: String,
    description :: String
} deriving (Show, Eq)


data DataParameter = DataParameter {

    term :: String,
    url :: String
} deriving (Show, Eq)


-- change name - PortalRecord, or MCP2 Record
data Record = Record {

    uuid :: String,
    dataIdentification :: DataIdentification , 
    attrConstraints :: [ String ],
    useLimitations :: [ String ],
    dataParameters :: [ DataParameter ], 
    temporalBegin :: String,
    -- links :: [ TransferLink ],
    transferLinks :: [ TransferLink ],
    geoPoly :: [ String ]
} deriving (Show, Eq)


-- should use ByteString?

-- should pass in the formatting function to use....


showRecord myRecord =
  -- change to show 

    let formatList f xs = concatMap id $ map (\x ->  "\n  -" ++ f x) xs in

    concatMap id [ 
        "uuid= " ++ uuid  myRecord, "\n",

        -- TODO -- tidy        
        "dataIdentification.title= " ++ (show $ (title.dataIdentification) myRecord), "\n",
        "dataIdentification.abstract= " ++ (show $ (abstract.dataIdentification) myRecord), "\n",
        "dataIdentification.jurisdictionLinke = " ++ (show $ (jurisdictionLink.dataIdentification) myRecord), "\n",
        "dataIdentification.licenseLink= " ++ (show $ (licenseLink.dataIdentification) myRecord), "\n",
        "dataIdentification.licenseName= " ++ (show $ (licenseName.dataIdentification) myRecord), "\n",
        "dataIdentification.licenseImageLink = " ++ (show $ (licenseImageLink.dataIdentification) myRecord), "\n",
 
        "attrConstraints= ", formatList id (attrConstraints myRecord), "\n",
        "useLimitations= ", formatList id (useLimitations myRecord), "\n",
        "dataParameters= ", formatList show (dataParameters myRecord), "\n",
        "temporalBegin= ",  temporalBegin myRecord, "\n",
        "transferLinks= ", formatList show (transferLinks myRecord), "\n",
        "geoPoly= ", formatList id (geoPoly myRecord), "\n"
    ]



