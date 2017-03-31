
-- separate from create tables - to allow easier change

begin;
drop view if exists concept_view;

create view concept_view as
select 
  concept.id    as concept_id,
  concept.label as concept_label,
  concept.url as concept_url,
  narrower_concept.label as narrower_label,
  narrow_match_concept.label as narrow_match_label,
  scheme.title as scheme_title 

  from concept 
  left join narrower on     concept.id = narrower.narrower_id 
  left join narrow_match on concept.id = narrow_match.narrower_id 
  left join in_scheme on    concept.id = in_scheme.concept_id 


  left join concept as narrower_concept on narrower_concept.id = narrower.concept_id
  left join concept as narrow_match_concept on narrow_match_concept.id = narrow_match.concept_id

  left join scheme on scheme.id = in_scheme.scheme_id

  order by concept.url

  -- each thing should only have its parent, otherwise it will be be entered twice, 
;


-- should this be by record,  

drop view if exists facet_view;

create view facet_view as
select 
  record.uuid,
  concept.url,
  concept.label
  from record

  left join facet on facet.record_id = record.id
  left join concept on concept.id = facet.concept_id
;

------

drop view if exists wms;
drop view if exists wfs;
drop view if exists resource_view;

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

create view wms as
select * from resource_view where protocol = 'OGC:WMS-1.1.1-http-get-map'
;

-- select * from resource where protocol ~ 'WFS';

create view wfs as
select * from resource_view where protocol = 'OGC:WFS-1.0.0-http-get-capabilities'
;




commit;

