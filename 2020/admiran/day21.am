|| day21.m


%export day21

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <avl>
%import <map>
%import <maybe>
%import <mirandaExtensions> -(***)
%import <parser> (<$>)/p_fmap (<*>)/p_apply (<*)/p_left (*>)/p_right (<|>)/p_alt
%import <set>

(***), (+++), (---) :: s_set ingredient -> s_set ingredient -> s_set ingredient
(***) = s_intersect  cmpingredient
(+++) = s_union      cmpingredient
(---) = s_difference cmpingredient

m_partition :: ordI * -> (** -> bool) -> m_map * ** -> (m_map * **, m_map * **)
m_partition cmp p m
    = (m_fromList cmp as, m_fromList cmp bs)
      where
        (as, bs) = partition (p . snd) (m_toList m)

ingredient  == string
allergen    == string
food        == (s_set ingredient, s_set allergen)
allergenMap == m_map ingredient (s_set allergen)

makeAllergenMap :: s_set ingredient -> s_set allergen -> allergenMap
makeAllergenMap is as = foldr (converse (m_insert cmpingredient) as) m_empty (s_toList is)

collectIngredients :: [food] -> (s_set ingredient, s_set allergen)
collectIngredients
    = foldl addFood (s_empty, s_empty)
      where
        addFood (si, sa) (si', sa') = (si +++ si', sa +++ sa')

processFood :: s_set ingredient -> allergenMap -> food -> allergenMap
processFood ais m (is, as)
    = s_foldl removeAllergens m nis
      where
        nis = ais --- is

        removeAllergens m k
            = m_adjust cmpingredient (--- as) k m

countOccurrences :: [ingredient] -> [food] -> int
countOccurrences noAllergens foods
    = foldl count 0 foods
      where
        nas = s_fromList cmpingredient noAllergens
        count n (is, _) = n + s_size (is *** nas)

solveAllergens :: allergenMap -> [(ingredient, allergen)]
solveAllergens
    = go []
      where
        go solution m
            = solution,                      if m_null m
            = go (solution' ++ solution) m2, otherwise
              where 
                (sing, m1)             = m_partition cmpingredient ((== 1) . s_size) m
                singList               = m_toList sing
                solution'              = map extractSing singList
                allergens              = foldr (+++) s_empty (map snd singList)
                m2                     = foldl (removeAllergens allergens) m1 (m_keys m1)
                extractSing (e, sa)    = (e, s_first sa)
                removeAllergens as m k = m_adjust cmpingredient (--- as) k m


|| Parsing

p_ingredient :: parser string
p_ingredient = p_some p_letter <* p_spaces

p_allergens :: parser [string]
p_allergens = p_string "(contains " *> (p_someSepBy (p_string ", ") p_ingredient) <* p_char ')' <* p_spaces

p_food :: parser ([string], [string])
p_food = p_liftA2 pair (p_some p_ingredient) p_allergens <* p_spaces

readFoods :: string -> io [food]
readFoods fn
    = go <$>. parse (p_many p_food) <$>. readFile fn
      where
        go (mfoods, ps) = fromMaybef (error (p_error ps)) (map mkFood) mfoods

       mkFood (il, al) = (s_fromList cmpingredient il, s_fromList cmpingredient al)

day21 :: io ()
day21
    = readFoods "../inputs/day21.txt" >>=. go
      where
        go foods
            = io_mapM_ putStrLn [part1, part2]
              where
                (ingredients, allergens)    = collectIngredients foods
                allergenMap                 = makeAllergenMap ingredients allergens
                allergenMap1                = foldl (processFood ingredients) allergenMap foods
                (noAllergens, allergenMap2) = m_partition cmpingredient s_null allergenMap1
                occurrences                 = countOccurrences (m_keys noAllergens) foods
                dangerousIngrs              = map fst . sortOn cmpingredient snd . solveAllergens $ allergenMap2
                dangerousList               = intercalate "," dangerousIngrs
                part1 = "part 1: " ++ showint occurrences
                part2 = "part 2: " ++ dangerousList
