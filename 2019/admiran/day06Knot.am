|| version of day06 using the "tie the knot" technique to create the objMap directly rather than the indirect lookup version

%export day06

%import <map>
%import <maybe>
%import <mirandaExtensions>


objId     == string
orbitSpec == (objId, objId)
path      == [objId]

obj ::= Obj string obj [obj]        || objId, parent obj, child objs

getId :: obj -> objId
getId (Obj oid p cs) = oid

getParent :: obj -> obj
getParent (Obj oid p cs) = p

setParent :: obj -> obj -> obj
setParent p' (Obj oid p cs) = Obj oid p' cs

getChildren :: obj -> [obj]
getChildren (Obj oid p cs) = cs

addChild :: obj -> obj -> obj
addChild c (Obj id p cs) = Obj id p (c : cs)

objMap   == m_map objId obj
depthMap == m_map objId num

makeObjMap :: [orbitSpec] -> objMap
makeObjMap specs
    = m
      where
        com          = Obj "COM" com []
        m            = foldl addSpec (m_singleton "COM" com) specs
        findFinal id = fromJust (m_lookup cmpstring id m)
        initObj id   = Obj id com []

        addSpec m1 (oid1, oid2)
            = m_insert cmpstring oid1 o1' (m_insert cmpstring oid2 o2' m1)
              where
                fo1 = findFinal oid1
                fo2 = findFinal oid2
                o1  = m_findWithDefault cmpstring (initObj oid1) oid1 m1
                o2  = m_findWithDefault cmpstring (initObj oid2) oid2 m1
                o1' = addChild fo2 o1
                o2' = setParent fo1 o2

makeDepthMap :: objMap -> depthMap
makeDepthMap om
    = go 0 m_empty (fromJust (m_lookup cmpstring "COM" om))
      where
        go d m o = foldl (go (d $seq d + 1)) (m_insert cmpstring (getId o) d m) (getChildren o)

pathToCom :: obj -> path
pathToCom
    = go []
      where
        go path o = path,                              if oid ==$ "COM"
                  = go (oid : path) (getParent o), otherwise
                    where
                      oid = getId o

orbitalTransfers :: objId -> objId -> objMap -> num
orbitalTransfers start stop om
    = unique pStart + unique pStop
      where
        pStart   = pathToCom (fromJust (m_lookup cmpstring start om))
        pStop    = pathToCom (fromJust (m_lookup cmpstring stop om))
        common   = (length . takeWhile (uncurry cmp)) (zip2 pStart pStop)
        unique p = #p - common - 1
        cmp      = _eq cmpstring

readSpecs :: string -> [(objId, objId)]
readSpecs
    = map mkSpec . lines . read
      where
        mkSpec s
            = (o1, o2)
              where
                (o1, _ : o2) = break (==. ')') s

day06 :: string
day06 = lay [part1, part2]
        where
          orbitSpecs = readSpecs "../inputs/day06.input"
          orbits     = makeObjMap orbitSpecs
          depths     = makeDepthMap orbits
          part1      = "part 1: " ++ (shownum . sum . m_elems) depths
          part2      = "part 2: " ++ (shownum (orbitalTransfers "YOU" "SAN" orbits))
