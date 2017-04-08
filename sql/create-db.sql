
-- psql -h postgres.localnet -U admin -d postgres -f db.sql

drop database if exists harvest; 
drop role if exists harvest; 

create role harvest password 'harvest' login; 
create database harvest owner harvest; 

\c harvest


begin;


--------

-- this is all vocab - distinct from facet or relationship with records 

/*
create table scheme (

  id serial   primary key not null,
  url         text not null unique,
  title       text not null
);
alter table scheme owner to harvest;
*/


create table concept (
  -- TODO change name
  -- concept or conceptScheme

  id serial   primary key not null,
  url         text not null unique,
  label       text not null
);
alter table concept owner to harvest;


-- skos relationships

create table narrower (

  id serial   primary key not null,
  concept_id  integer references concept(id), 
  narrower_id integer references concept(id)
);
alter table narrower owner to harvest;

-- TODO uniqueness constraint on the combination  that link things,


create table narrow_match (

  id serial   primary key not null,
  concept_id  integer references concept(id), 
  narrower_id integer references concept(id)
);
alter table narrow_match owner to harvest;


create table scheme_has_top_concept (
  -- eg. a skos:has_top_concept  in a scheme definition

  id serial   primary key not null,
  concept_id  integer references concept(id), 
  scheme_id   integer references concept(id)
);
alter table scheme_has_top_concept owner to harvest;



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

alter table record owner to harvest;


create table data_identification (

    id serial primary key not null,
    record_id integer references record(id) unique, 

    title               text, -- not null? 
    abstract            text, 
    jurisdiction_link   text, 
    license_link        text, 
    license_name        text, 
    license_image_link  text
);

alter table record owner to harvest;



create or replace view record_view as
select 
    record.id, 
    record.uuid,
    di.title,
    di.abstract,
    di.jurisdiction_link,
    di.license_link,
    di.license_name,
    di.license_image_link 
from record 
left join data_identification di 
on di.record_id = record.id 
;


create table transfer_link (
  -- mcp2 resource

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

alter table resource owner to harvest;

--------------

-- facet is the relationship between the concept and record table.

-- TODO MAYBE change name of table to facetIndex or facet - postgres join table name
-- conceptRecord --- as the assocation.... or conceptRecordMap

create table facet (

  id serial   primary key not null,

  record_id  integer references record(id), 
  concept_id  integer references concept(id)
);

-- important
CREATE UNIQUE INDEX my_facet_unique_idx ON facet(record_id, concept_id);

alter table facet owner to harvest;

commit;
