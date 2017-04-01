


{-
    this selects the top level objects...

    select concept_label from concept_view  
    where parent_id is null 
    and parent_match_id is null 
    and scheme_title = 'AODN Platform Category Vocabulary' ;

-}


import qualified Data.Set as Set

main :: IO ()
main = do

    let set = Set.fromList [ ("erik", 123), ("salaj", 456) ]


    print $ Set.member ("whoot", 123) set
    print $ Set.member ("erik",  456) set
    print $ Set.size set 
    print $ set 


-- ok, I think that we don't want a set - 


