
-- separate from create tables - to allow easier change

-- provide an aggregated view of parent relationships... eg. a simple tree.

-- might be better as a union...

begin;

-- drop view if exists facet_view_3 ;
drop view if exists facet_view;
drop view if exists facet_count_basic_view;
drop view if exists wms_view;
drop view if exists wfs_view;
drop view if exists resource_view;
drop view if exists concept_view;
drop view if exists parent_view;


-- associate concept parent (eg. skos:broader) relationships between concepts via skos:narrower and skos:narrowMatch

create view parent_view as
select
  concept.id as id,
  case
    -- TODO another test to ensure not both narrower and narrowMatch
    when
        parent_concept.id is not null and parent_match_concept.id is null
        then parent_concept.id
    when
        parent_match_concept.id is not null and parent_concept.id is null
        then parent_match_concept.id
    when parent_match_concept.id is null and parent_concept.id is null
        then null
    -- error case - concept has both narrower and narrowMatch
    else -9999
  end as parent_id

  from concept

  left join narrower on     concept.id = narrower.narrower_id
  left join narrow_match on concept.id = narrow_match.narrower_id

  left join concept as parent_concept       on parent_concept.id = narrower.concept_id
  left join concept as parent_match_concept on parent_match_concept.id = narrow_match.concept_id
;




-- view of concept parent relationships with more detail provided

create view concept_view as
select
  concept.id    as id,
  concept.label as label,
  concept.url   as url,

  parent_concept.id as parent_id,
  parent_concept.url as parent_url,
  parent_concept.label as parent_label

  -- scheme.title as scheme_title

  from parent_view

  left join concept on concept.id = parent_view.id
  left join concept as parent_concept on parent_concept.id = parent_view.parent_id

  -- Careful - could have more than one scheme ?
  -- left join in_scheme on in_scheme.concept_id = concept.id
  -- left join scheme on scheme.id = in_scheme.scheme_id
;





-- TODO remove this - do we use it...
-- should this be by record,
-- drop view if exists facet_match_view;
--
-- create view facet_match_view as
-- select
--   record.uuid,
--  record.title,
--  concept.url,
--  concept.label
--  from record
--
--  left join facet on facet.record_id = record.id
--  left join concept on concept.id = facet.concept_id
-- ;

------

-- change name wms_view


create view resource_view as
select
  record.uuid,
  resource.protocol,
  resource.linkage,
  resource.description
  from record

  left join resource on resource.record_id = record.id
;

-----
-- quite cool

create view wms_view as
select * from resource_view where protocol = 'OGC:WMS-1.1.1-http-get-map'
;

-- select * from resource where protocol ~ 'WFS';

create view wfs_view as
select * from resource_view where protocol = 'OGC:WFS-1.0.0-http-get-capabilities'
;


--
-- count the matching records for a leaf facet
-- may need to be turned into a function - so that we can filter by resource and scheme
-- which means we cannot use views...

create view facet_count_basic_view as
select
  concept.id as concept_id,
  count(facet.record_id) as count
  from concept
  left join facet on facet.concept_id = concept.id
  group by concept.id
;


-- change name to facet_view?
-- because it includes all the useful stuff

create view facet_view as
select
  *
  from facet_count_basic_view
  left join concept_view on concept_view.id = facet_count_basic_view.concept_id

;




/*
drop table if exists data;

create table data (
  id INTEGER PRIMARY KEY
  , parent_id INTEGER
  , label VARCHAR(32)
  , count INTEGER
);


INSERT INTO data values ( 1, Null, 'Root', NULL) ;
INSERT INTO data values ( 2, 1, 'Flowers', NULL ) ;
INSERT INTO data values ( 3, 1, 'Electro', NULL ) ;
INSERT INTO data values ( 4, 2, 'Rose', 10 ) ;
INSERT INTO data values ( 5, 2, 'Violet', 5 );
INSERT INTO data values ( 6, 4, 'Red Rose', 12 );
INSERT INTO data values ( 7, 3, 'Television', 100 );
INSERT INTO data values ( 8, 3, 'Radio', 70 );
INSERT INTO data values ( 9, 8, 'Webradio', 90 );
*/
--
-- SQL Statement

/*

create view facet_view_3 as

WITH recursive ChildrenCTE AS (
  SELECT
    id as Rootid,
    id,
    "count"
  FROM    facet_count_view
  UNION ALL
  SELECT
    cte.Rootid,
    d.id,
    -- cte."count"
    d."count"
  FROM    ChildrenCTE cte
  INNER JOIN facet_count_view d ON d.parent_id = cte.id
)
SELECT
  d.id,
  d.parent_id,
  d.label,
  d."count",
  cnt.node_count,
  cnt.count_sum
FROM    facet_count_view d
INNER JOIN (
  SELECT
    Rootid as id ,
    COUNT(*) - 1 as node_count,
    cast( SUM("count")  as integer)  as count_sum
  FROM    ChildrenCTE
  GROUP BY Rootid
) cnt ON cnt.id = d.id
;


*/




commit;

