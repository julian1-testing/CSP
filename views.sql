
-- separate from create tables - to allow easier change

-- provide an aggregated view of parent relationships... eg. a simple tree.

-- might be better as a union...

begin;

drop view if exists parent_view;
drop view if exists concept_count_view;
drop view if exists facet_count_view;
drop view if exists wms_view;
drop view if exists wfs_view;
drop view if exists resource_view;
drop view if exists concept_view;



create view parent_view as
select 
  concept.id    as id,

  case 
    when parent_concept.id is not null then parent_concept.id
    when parent_match_concept.id is not null then parent_match_concept.id
    else null
  end as parent_id

  from concept 

  left join narrower on     concept.id = narrower.narrower_id 
  left join narrow_match on concept.id = narrow_match.narrower_id 
  -- left join in_scheme on    concept.id = in_scheme.concept_id 

  left join concept as parent_concept       on parent_concept.id = narrower.concept_id
  left join concept as parent_match_concept on parent_match_concept.id = narrow_match.concept_id
;






create view concept_view as
select 
  concept.id    as id,
  concept.label as label,
  concept.url   as url,

  case 
    when parent_concept.id is not null then parent_concept.id
    when parent_match_concept.id is not null then parent_match_concept.id
    else null
  end as parent_id,

  case 
    when parent_concept.id is not null then parent_concept.label
    when parent_match_concept.id is not null then parent_match_concept.label
    else null
  end as parent_label,

  -- parent_concept.id    as parent_id,
  -- parent_concept.label as parent_label,
  -- parent_match_concept.label as parent_match_label,

  scheme.title as scheme_title 
  from concept 

  left join narrower on     concept.id = narrower.narrower_id 
  left join narrow_match on concept.id = narrow_match.narrower_id 
  left join in_scheme on    concept.id = in_scheme.concept_id 


  left join concept as parent_concept       on parent_concept.id = narrower.concept_id
  left join concept as parent_match_concept on parent_match_concept.id = narrow_match.concept_id

  left join scheme on scheme.id = in_scheme.scheme_id

  order by concept.url

  -- each thing should only have its parent, otherwise it will be be entered twice, 
;






-- should this be by record,  

-- this is badly named - should be facet_index or something

drop view if exists facet_view;

create view facet_view as
select 
  record.uuid,
  record.title,
  concept.url,
  concept.label
  from record

  left join facet on facet.record_id = record.id
  left join concept on concept.id = facet.concept_id
;

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
-- TODO - IMPORTANT - looks broken - however - we need to iterate the records?????

create view wms_view as
select * from resource_view where protocol = 'OGC:WMS-1.1.1-http-get-map'
;

-- select * from resource where protocol ~ 'WFS';

create view wfs_view as
select * from resource_view where protocol = 'OGC:WFS-1.0.0-http-get-capabilities'
;


-- gets counts on leaf nodes - eg matching concepts,
-- TODO change name matching ???? record_matching? 

create view facet_count_view as
select  
  concept.id as concept_id, 
  concept.url as concept_url, 
  count(facet.record_id) 
  from concept 
  left join facet on facet.concept_id = concept.id 
  group by concept.id
;

-- sum the damn counts 




commit;

