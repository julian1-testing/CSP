
-- psql -h postgres.localnet -U harvest -d harvest -f sql/test.sql

select
  concept_view.concept_id,
  concept_view.parent_id,
  data_parameter.record_id,
  record.*
from concept_view
left join data_parameter on data_parameter.concept_id = concept_view.concept_id
left join record on data_parameter.record_id = record.id

-- order by concept_view
order by uuid 
;


-- transfer_link - contains protocol and resource


