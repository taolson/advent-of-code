|| day23.m -- LAN Party


%export day23

%import "adventLib"
%import <map>
%import <maybe>
%import <set>


dependencyGraph * == m_map * [*]

addDependency :: ordI * -> * -> * -> dependencyGraph * -> dependencyGraph *
addDependency cmp a b
    = m_insertWith cmp (++) a [b] .> m_insertWith cmp (++) b [a]

|| make a dependency graph from a list of (node, uses-list) pairs
makeDependencyGraph :: ordI * -> [(*, *)] -> dependencyGraph *
makeDependencyGraph cmp
    = foldl addDep m_empty
      where
        addDep m (a, b) = addDependency cmp a b m

node == string
lan  == dependencyGraph node

(!!) :: lan -> node -> [node]
lan !! n = fromJust $ m_lookup cmpnode n lan

makeLan :: string -> lan
makeLan
    = lines .> map (split '-' .> mkDepend) .> makeDependencyGraph cmpnode
      where
        mkDepend [a, b] = (a, b)
        mkDepend _      = error "parse error"

|| Note: this is a greedy algorithm that assumes that the expansion always correctly identifies the maximally-connected clique
|| It apparently does so for the input (specially designed for this?), but is not guaranteed to find it in all cases
findFullyConnectedGroups :: lan -> [[node]]
findFullyConnectedGroups lan
    = m_keys lan |> foldl findGroup ([], s_empty) |> fst
      where
        || find the fully-connected group associated with node n
        findGroup (grps, seen) n
            = (grps,  seen),  if s_member cmpnode n seen                        || this node is already part of a group
            = (grps', seen'), otherwise
              where
                (g, seen') = foldl expand ([n], seen) (lan !! n)                || start a new group with n and expand from it
                grps'      = g : grps

                expand (grp, seen) n
                    = (grp,  seen),  if s_member cmpnode n seen                 || this node is already part of a group
                    = (grp', seen'), if all (member cmpnode (lan !! n)) grp     || add if it is connected to each of the other group members
                    = (grp, seen),   otherwise
                      where
                        grp'  = n : grp
                        seen' = s_insert cmpnode n seen

findConnected3WithT:: lan -> [[node]]
findConnected3WithT lan
    = grps |> filter hasT |> map (sortBy cmpnode) |> s_fromList (cmplist cmpnode) |> s_toList
      where
        grps = [[a, b, c] | a <- m_keys lan; b <- lan !! a; c <- lan !! b; member cmpnode (lan !! c) a]
        hasT = any (hd .> (==. 't'))

day23 :: io ()
day23
    = readFile "../inputs/day23.txt" >>= makeLan .> go
      where
        go lan
            = output [part1, part2]
              where
                part1 = lan |> findConnected3WithT |> length |> showint
                part2 = lan |> findFullyConnectedGroups |> maxBy cmpint length |> sortBy cmpnode |> intercalate ","
