
-- psql -h postgres.localnet -U harvest -d harvest -f sql/test.sql

select
  concept_view.concept_id,
  concept_view.label,
  concept_view.parent_id,
  case concept_view.label
    when 'asdf' then data_parameter.record_id 
    else data_parameter.record_id 
  end
        
  -- record.uuid
from concept_view
left join data_parameter on data_parameter.concept_id = concept_view.concept_id
-- left join record on data_parameter.record_id = record.id

-- where concept_view.label = 'mooring'
-- order by concept_view
order by concept_id
;


-- transfer_link - contains protocol and resource


