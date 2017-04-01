
-- separate from create tables - to allow easier change

-- provide an aggregated view of parent relationships... eg. a simple tree.

-- might be better as a union...

begin;

drop view if exists concept_count_view2;
drop view if exists concept_count_view;
drop view if exists facet_count_view;
drop view if exists facet_match_view;
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
  parent_concept.label as parent_label,

  scheme.title as scheme_title

  from parent_view

  left join concept on concept.id = parent_view.id
  left join concept as parent_concept on parent_concept.id = parent_view.parent_id

  -- Careful - could have more than one scheme ?
  left join in_scheme on in_scheme.concept_id = concept.id
  left join scheme on scheme.id = in_scheme.scheme_id
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

create view facet_count_view as
select  
  concept.id as concept_id,           -- rename to concept id? 
  -- concept.label as label,     -- should remove all this because we can get it by joining on view 
  -- concept.url as url,         -- I think we do want the url  - so that we have the term to search on....
  count(facet.record_id) as count 
  from concept 
  left join facet on facet.concept_id = concept.id 
  group by concept.id
;



-- now we want a view with the full parent...  to return to the client
-- even if we have to use a function later.

create view facet_count_view2 as
select  
  * 
  from facet_count_view
  left join concept_view on concept_view.id = facet_count_view.concept_id

;






commit;

