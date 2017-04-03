
begin;

/*
drop table if exists data;

create table data (
  id INTEGER PRIMARY KEY
  , parent_id INTEGER
  , label VARCHAR(32)
  , count INTEGER
);


INSERT INTO data values ( 1, Null, 'Root', NULL) ;
INSERT INTO data values ( 2, 1, 'Flowers', NULL ) ;
INSERT INTO data values ( 3, 1, 'Electro', NULL ) ;
INSERT INTO data values ( 4, 2, 'Rose', 10 ) ;
INSERT INTO data values ( 5, 2, 'Violet', 5 );
INSERT INTO data values ( 6, 4, 'Red Rose', 12 );
INSERT INTO data values ( 7, 3, 'Television', 100 );
INSERT INTO data values ( 8, 3, 'Radio', 70 );
INSERT INTO data values ( 9, 8, 'Webradio', 90 );
*/
-- 
-- SQL Statement

WITH recursive ChildrenCTE AS (
  SELECT  
    id as Rootid, 
    id,
    "count"
  FROM    facet_count_view2
  UNION ALL
  SELECT  
    cte.Rootid, 
    d.id,
    -- cte."count" 
    d."count"
  FROM    ChildrenCTE cte
          INNER JOIN facet_count_view2 d ON d.parent_id = cte.id
)
SELECT  
  d.id, 
  d.parent_id, 
  d.label, 
  d."count", 
  cnt.node_count,
  cnt.count_sum
FROM    facet_count_view2 d
        INNER JOIN (
          SELECT  
            Rootid as id , 
            COUNT(*) - 1 as node_count,
            SUM("count")  as count_sum
          FROM    ChildrenCTE
          GROUP BY Rootid
        ) cnt ON cnt.id = d.id
;

commit;

-- we should keep this as simple as possible. 

-- I think this might be working.... 
