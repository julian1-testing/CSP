
begin;

drop table if exists data;

create table data (
  ID INTEGER PRIMARY KEY
  , ParentID INTEGER
  , Text VARCHAR(32)
  , Price INTEGER
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
/*
  1, Null, 'Root', NULL) ;
  SELECT 2, 1, 'Flowers', NULL
  SELECT 3, 1, 'Electro', NULL
  SELECT 4, 2, 'Rose', 10
  SELECT 5, 2, 'Violet', 5
  SELECT 6, 4, 'Red Rose', 12
  SELECT 7, 3, 'Television', 100
  SELECT 8, 3, 'Radio', 70
  SELECT 9, 8, 'Webradio', 90
*/



-- SQL Statement

WITH recursive ChildrenCTE AS (
  SELECT  ID as RootID , ID
  FROM    data
  UNION ALL
  SELECT  cte.RootID, d.ID
  FROM    ChildrenCTE cte
          INNER JOIN data d ON d.ParentID = cte.ID
)
SELECT  d.ID, d.ParentID, d.Text, d.Price, cnt.Children
FROM    data d
        INNER JOIN (
          SELECT  RootID as ID , COUNT(*) - 1 as Children 
          FROM    ChildrenCTE
          GROUP BY RootID
        ) cnt ON cnt.ID = d.ID
;

commit;


