
select
  concept_view.concept_id,



 -- left(concept_view_parent_parent.parent_label, 20) as label1,
 -- left(concept_view_parent.parent_label, 20) as label2,


  left(concept_view.label, 40) as label4,
  left(concept_view_parent.label, 20) as label3,
  left(concept_view_parent_parent.label, 20) as label1,
  left(concept_view_parent_parent_parent.label, 20) as label0,
  left(concept_view_parent_parent_parent_parent.label, 20) as label_1

  -- concept_view.parent_id,


from concept_view


left join concept_view concept_view_parent
  on concept_view_parent.parent_id = concept_view.concept_id

left join concept_view concept_view_parent_parent
  on concept_view_parent_parent.parent_id = concept_view_parent.concept_id

left join concept_view concept_view_parent_parent_parent
  on concept_view_parent_parent_parent.parent_id = concept_view_parent_parent.concept_id

left join concept_view concept_view_parent_parent_parent_parent
  on concept_view_parent_parent_parent_parent.parent_id = concept_view_parent_parent_parent.concept_id




-- left join concept_view concept_view_parent_parent
--  on concept_view_parent_parent.concept_id = concept_view_parent.parent_id 

-- left join concept_view concept_view_parent_parent_parent
--  on concept_view_parent_parent_parent.concept_id = concept_view_parent_parent.parent_id 




order by concept_id
;

-- this may want to be tightened - so that it's only a toplevel thing... 
--where concept_view_parent_parent_parent.parent_id is not null

-- ok we could do it...


