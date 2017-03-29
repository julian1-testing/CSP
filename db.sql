
-- psql -h postgres.localnet -U admin -d postgres -f db.sql

drop database if exists harvest; 
drop role if exists harvest; 

create role harvest password 'harvest' login; 
create database harvest owner harvest; 

\c harvest

create table catalog (

  id serial primary key not null,

  url text
);

alter table catalog owner to harvest;

insert into catalog(url) values ('http://catalogue');
insert into catalog(url) values ('http://catalogue2');


