
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
-- record

--
-- probably want a catalog source as well

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
create index on data_identification(record_id);



create table md_commons (
    id serial primary key not null,
    record_id           integer references record(id) unique,

    jurisdiction_link   text,
    license_link        text,
    license_name        text,
    license_image_link  text
);
create index on md_commons(record_id);



create table transfer_link (
  -- mcp2 online resource

  id serial   primary key not null,
  record_id   integer references record(id),  -- more than one

  protocol    text not null,
  linkage     text not null,
  name        text not null,
  description text
);

-- unique on combination - eg. allow multi-wms
CREATE UNIQUE INDEX my_transfer_link_unique_idx ON transfer_link(record_id, protocol, linkage, name);
create index on transfer_link(record_id);



-- relationship between concept_id and record_id -- used to be called facet,

create table data_parameter (

  id serial   primary key not null,

  record_id   integer references record(id),
  concept_id  integer references concept(id)
);

-- important
CREATE UNIQUE INDEX my_data_parameter_unique_idx ON data_parameter(record_id, concept_id);
create index on data_parameter(record_id);



create table attr_constraint  (

  id serial   primary key not null,

  record_id  integer references record(id),
  attr       text not null
);
create index on attr_constraint(record_id);


create table use_limitation (

  id serial   primary key not null,

  record_id  integer references record(id),
  limitation text not null
);
create index on use_limitation(record_id);


create table temporal_begin (

  id serial   primary key not null,

  record_id  integer references record(id),
  begin_	text not null
);
create index on temporal_begin(record_id);


create table geopoly (

  id serial   primary key not null,

  record_id  integer references record(id),
	-- should be postgis type
  poly		  text not null
);
create index on geopoly(record_id);


commit;

