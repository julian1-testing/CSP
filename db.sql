
-- psql -h postgres.localnet -U admin -d postgres -f db.sql

drop database if exists harvest; 
drop role if exists harvest; 

create role harvest password 'harvest' login; 
create database harvest owner harvest; 

\c harvest

-- TODO change name to record, or metadata,   catalog is the particular catalog instance that we scan.
create table catalog (

  id          serial primary key not null,
  uuid        text not null unique,
  title       text not null
);

alter table catalog owner to harvest;

-- TODO add uniqueness constraints

create table resource (

  id serial   primary key not null,
  catalog_id  integer references catalog(id), 

  protocol    text not null,
  linkage     text not null,
  description text 
);

alter table resource owner to harvest;


--------

-- facet search stuff?

create table concept (

  id serial   primary key not null,
  url         text not null unique,
  label       text not null
);

alter table concept owner to harvest;


create table narrower (

  id serial   primary key not null,
  concept_id  integer references concept(id), 
  narrower_id integer references concept(id)
);

alter table narrower owner to harvest;

-- TODO uniqueness constraint on the combination 


create table narrow_match (

  id serial   primary key not null,
  concept_id  integer references concept(id), 
  narrower_id integer references concept(id)
);

alter table narrow_match owner to harvest;


-- concept scheme
create table scheme (

  id serial   primary key not null,
  url         text not null unique,
  title       text not null
);

alter table scheme owner to harvest;

-- get the concept scheme membership - and then we can ... 

--------------



