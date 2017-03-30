

select * from concept 
  left join narrower on     concept.id = narrower.narrower_id 
  left join narrow_match on concept.id = narrow_match.narrower_id 

  order by url

;



