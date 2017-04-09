
-- non-transaction db creation - that needs to be done as root
-- psql -h postgres.localnet -U admin -d postgres -f db.sql

drop database if exists harvest; 
drop role if exists harvest; 

create role harvest password 'harvest' login; 
create database harvest owner harvest; 



