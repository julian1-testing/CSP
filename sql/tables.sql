
-- psql -h postgres.localnet -U admin -d postgres -f db.sql


begin;


--------

-- this is all vocab - distinct from facet or relationship with records 



create table concept (
  -- TODO change name
  -- concept or conceptScheme

  id serial   primary key not null,
  url         text not null unique,
  label       text not null
);


-- skos relationships

create table narrower (

  id serial   primary key not null,
  concept_id  integer references concept(id), 
  narrower_id integer references concept(id)
);

-- TODO uniqueness constraint on the combination  that link things,


create table narrow_match (

  id serial   primary key not null,
  concept_id  integer references concept(id), 
  narrower_id integer references concept(id)
);


create table scheme_has_top_concept (
  -- eg. a skos:has_top_concept  in a scheme definition

  id serial   primary key not null,
  concept_id  integer references concept(id), 
  scheme_id   integer references concept(id)
);



/*
create table in_scheme (

  id serial   primary key not null,
  concept_id  integer references concept(id), 
  scheme_id   integer references scheme(id)
);
alter table in_scheme owner to harvest;
*/


--------------

-- want a catalog table as well - dependening on harvest source?
-- actually may not even need...

create table record (

  id          serial primary key not null,
  uuid        text not null unique
);



create table data_identification (

    id serial primary key not null,
    record_id integer references record(id) unique, 

    title               text, -- not null? 
    abstract            text
);




create table md_commons (
    id serial primary key not null,
    record_id integer references record(id) unique, 

    jurisdiction_link   text, 
    license_link        text, 
    license_name        text, 
    license_image_link  text
);




create table transfer_link (
  -- mcp2 online resource

  id serial   primary key not null,
  record_id  integer references record(id),  -- more than one

  protocol    text not null,
  linkage     text not null,
  description text 
);

-- unique on combination - eg. allow multi-wms 
CREATE UNIQUE INDEX my_transfer_link_unique_idx ON transfer_link(record_id, protocol, linkage);



-- relationship between concept_id and record_id
-- used to be called facet,

create table data_parameter (

  id serial   primary key not null,

  record_id  integer references record(id), 
  concept_id  integer references concept(id)
);

-- important
CREATE UNIQUE INDEX my_data_parameter_unique_idx ON data_parameter(record_id, concept_id);




    attrConstraints :: [ String ],   -- todo
    useLimitations :: [ String ],    -- todo
    dataParameters :: [ DataParameter ], 
    temporalBegin :: Maybe String,   -- todo
    transferLinks :: [ TransferLink ],
    geoPoly :: [ String ]            -- todo

create table attr_constraint  (

  id serial   primary key not null,

  record_id  integer references record(id), 
  attr       text not null
);


create table use_limitation (

  id serial   primary key not null,

  record_id  integer references record(id), 
  limitation text not null
);


create table temporal_begin (

  id serial   primary key not null,

  record_id  integer references record(id), 
  begin_	text not null
);


create table geo_poly (

  id serial   primary key not null,

  record_id  integer references record(id), 
	-- should be postgis type
  poly		  text not null
);

















commit;
