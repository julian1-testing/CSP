

select 
  concept.label,
  narrower.label

from scheme 
left join concept on concept.id = scheme.concept_id
left join concept as narrower on narrower.id = scheme.narrower_id

;

