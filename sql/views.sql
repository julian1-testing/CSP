
-- separate from create tables - to allow easier change

-- provide an aggregated view of parent relationships... eg. a simple tree.

-- might be better as a union...

begin;

-- drop view if exists facet_view_3 ;
drop view if exists facet_view;
drop view if exists facet_basic_view;
drop view if exists wms_view;
drop view if exists wfs_view;
drop view if exists transfer_link_view;

drop view if exists root_label_concept_view;
drop view if exists qualified_concept_view;

drop view if exists concept_view;
drop view if exists parent_view;
drop view if exists record_view;

-- associate concept parent (eg. skos:broader) relationships between concepts via skos:narrower and skos:narrowMatch

create view parent_view as
select
  concept.id as id,
  case
    -- may be better as a union ....
    -- TODO another test to ensure not both narrower and narrowMatch
    when
        parent_concept.id is not null and parent_match_concept.id is null
        then parent_concept.id
    when
        parent_match_concept.id is not null and parent_concept.id is null
        then parent_match_concept.id

    when
        parent_scheme.id is not null and parent_concept.id is null
        then parent_scheme.id



    -- when parent_match_concept.id is null and parent_concept.id is null
    --    then null

    -- error case - concept has both narrower and narrowMatch
    else -9999
  end as parent_id

  from concept

  left join narrower on     concept.id = narrower.narrower_id
  left join narrow_match on concept.id = narrow_match.narrower_id
  left join scheme_has_top_concept on concept.id = scheme_has_top_concept.scheme_id

  left join concept as parent_concept       on parent_concept.id = narrower.concept_id
  left join concept as parent_match_concept on parent_match_concept.id = narrow_match.concept_id
  left join concept as parent_scheme        on parent_scheme.id = scheme_has_top_concept.concept_id

;




-- view of concept parent relationships with more detail provided

-- note concepts will be represented more than once - for each parent they have

create view concept_view as
select
  concept.id    as concept_id,  -- could rename to just id...
  concept.label as label,
  concept.url   as url,    -- rename as concept_url? no because this is not a join table, but concept only

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




-- change name wms_view


create or replace view transfer_link_view as
select
  record.id as record_id,
  record.uuid,
  data_identification.title,
  transfer_link.protocol,
  transfer_link.linkage,
  transfer_link.description
  from record

  left join transfer_link on transfer_link.record_id = record.id
  left join data_identification on data_identification.record_id = record.id
;

-----
-- quite cool

create view wms_view as
select * from transfer_link_view where protocol = 'OGC:WMS-1.1.1-http-get-map'
;


create view wfs_view as
select * from transfer_link_view where protocol = 'OGC:WFS-1.0.0-http-get-capabilities'
;


--
-- count the matching records for a leaf facet
-- may need to be turned into a function - so that we can filter by resource and scheme
-- which means we cannot use views...

create view facet_basic_view as
	-- rename 
select
  concept.id as concept_id,
  count(data_parameter.record_id) as count
  from concept
  left join data_parameter on data_parameter.concept_id = concept.id
  group by concept.id
;


-- facet is the count of the concept

create view facet_view as
select
  facet_basic_view.count as count,  
  concept_view.*

  from facet_basic_view
  left join concept_view on concept_view.concept_id = facet_basic_view.concept_id
;



create view record_view as
select 
    record.id, 
    record.uuid,
    di.title,
    left( di.abstract, 100) as abstract,

    mdc.jurisdiction_link,
    mdc.license_link,
    mdc.license_name,
    mdc.license_image_link,

	tb.begin_
from record 
left join data_identification di on di.record_id = record.id 
left join md_commons mdc on mdc.record_id = record.id 
left join temporal_begin tb on tb.record_id = record.id 
;



create view qualified_concept_view as
select
 
  concept_view.concept_id,
  concept_view.label as label0,
  concept_view_1.label as label1,
  concept_view_2.label as label2,
  concept_view_3.label as label3,
  concept_view_4.label as label4
    
  from concept_view

  left join concept_view concept_view_1
    on concept_view_1.concept_id = concept_view.parent_id

  left join concept_view concept_view_2
    on concept_view_2.concept_id = concept_view_1.parent_id

  left join concept_view concept_view_3
    on concept_view_3.concept_id = concept_view_2.parent_id

  left join concept_view concept_view_4
    on concept_view_4.concept_id = concept_view_3.parent_id

  left join concept_view concept_view_5
    on concept_view_5.concept_id = concept_view_4.parent_id

  -- order by concept_view.concept_id
;



create view root_label_concept_view as
select 
  concept_id,
  case 
    when label1 is null then label0 
    when label2 is null then label1 
    when label3 is null then label2 
    when label4 is null then label3 
    -- when label5 is null then label4 
  else label4 
  end as root_label
 
  -- do we want the root concept or the root label?
  from qualified_concept_view 

  order by root_label
;




commit;

