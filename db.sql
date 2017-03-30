
-- psql -h postgres.localnet -U admin -d postgres -f db.sql

drop database if exists harvest; 
drop role if exists harvest; 

create role harvest password 'harvest' login; 
create database harvest owner harvest; 

\c harvest

-- TODO change name to record, or metadata,   catalog is the particular catalog instance that we scan.
create table catalog (

  id          serial primary key not null,
  uuid        text not null,
  title       text not null
);

alter table catalog owner to harvest;



create table resource (

  id serial   primary key not null,
  catalog_id  integer references catalog(id), 

  protocol    text not null,
  linkage     text not null,
  description text 
);

alter table resource owner to harvest;

-- insert into catalog(url) values ('http://catalogue');
-- insert into catalog(url) values ('http://catalogue2');

create table term (

  id serial   primary key not null,

  label       text not null,
  url         text not null
);

alter table term owner to harvest;


