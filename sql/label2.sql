
-- so create this as a view...
-- then we can look up a qualified concept ...

select
  concept_view.concept_id,

  left(concept_view.label, 40) as label4,
  left(concept_view_p.label, 20) as label3,
  left(concept_view_pp.label, 20) as label1,
  left(concept_view_ppp.label, 20) as label0,
  left(concept_view_pppp.label, 20) as label_1

  from concept_view

  left join concept_view concept_view_p
    on concept_view_p.parent_id = concept_view.concept_id

  left join concept_view concept_view_pp
    on concept_view_pp.parent_id = concept_view_p.concept_id

  left join concept_view concept_view_ppp
    on concept_view_ppp.parent_id = concept_view_pp.concept_id

  left join concept_view concept_view_pppp
    on concept_view_pppp.parent_id = concept_view_ppp.concept_id

  order by concept_id
;


