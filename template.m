|| <DAY>.m


%export <DAY>

%import "adventLib"


<DAY> :: io ()
<DAY>
    = readFile "../inputs/<DAY>.txt" >>= go
      where
        go input
            = output [part1, part2]
              where
                part1 = undef
                part2 = undef
