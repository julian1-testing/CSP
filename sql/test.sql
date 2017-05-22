
-- psql -h postgres.localnet -U harvest -d harvest -f sql/test.sql

select
  concept_view.concept_id,
  left(concept_view.parent_label, 20),
  left( concept_view.label, 20),
  -- concept_view.parent_id,

  data_parameter.record_id

from concept_view
left join data_parameter 
  on data_parameter.concept_id = concept_view.concept_id   
  and ( concept_view.parent_label = 'Mooring and buoy' 
    -- or concept_view.parent_label = 'mooring' 
  )


order by concept_id
;

/*
  VERY IMPORTANT

  concept_view.label = 'mooring' 


-- very important - we can structure the levels.... without a linear explosion

  - so rather than matching any level. we are precise.  
  and 

  label_level1 = 'platform' and label_level2 = 'Mooring and buoy' and label_level3 = 'mooring' etc....

  so we just need a view that exposes all the parents like this.... 

  level1, level2, level3, level4, record_id


  THIS SOLVES BOTH PROBLEMs
    - being explicit about the nested term a / b / c  etc. 
    - and picking out all the lower terms that could match

  - it's curious - the label doesn't look quite right 



*/

