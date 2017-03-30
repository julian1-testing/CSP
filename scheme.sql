

select 
  concept.label,
  narrower_concept.label,
  narrow_match_concept.label

  from concept 
  left join narrower on     concept.id = narrower.narrower_id 
  left join narrow_match on concept.id = narrow_match.narrower_id 


  left join concept as narrower_concept on narrower_concept.id = narrower.concept_id
  left join concept as narrow_match_concept on narrow_match_concept.id = narrow_match.concept_id

  order by concept.url

;



