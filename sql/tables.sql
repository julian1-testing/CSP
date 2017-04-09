
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





-- have a secondary/derived view - that truncates the abstract - 
create or replace view record_view as
select 
    record.id, 
    record.uuid,
    di.title,
    left( di.abstract, 100) as abstract,

    mdc.jurisdiction_link,
    mdc.license_link,
    mdc.license_name,
    mdc.license_image_link 
from record 
left join data_identification di on di.record_id = record.id 
left join md_commons mdc on mdc.record_id = record.id 
;


create table transfer_link (
  -- mcp2 online resource

  id serial   primary key not null,
  record_id  integer references record(id),  -- more than one

  protocol    text not null,
  linkage     text not null,
  description text 
);

-- should be unique on these three...
CREATE UNIQUE INDEX my_transfer_link_unique_idx ON transfer_link(record_id, protocol, linkage);






-- TODO need uniqueness constraints - on protocol and linkage 
-- actually probably only needs to be on protocol...
-- actually it's unique on record_id, protocol, linkage 


--------------

-- facet is the relationship between the concept and record table.

-- TODO MAYBE change name of table to facetIndex or facet - postgres join table name
-- conceptRecord --- as the assocation.... or conceptRecordMap

create table data_parameter (

  id serial   primary key not null,

  record_id  integer references record(id), 
  concept_id  integer references concept(id)
);

-- important
CREATE UNIQUE INDEX my_data_parameter_unique_idx ON data_parameter(record_id, concept_id);


commit;
