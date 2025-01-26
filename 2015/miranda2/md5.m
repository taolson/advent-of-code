|| md5.m -- MD5 Hash Algorithm


%export md5State md5Hash md5Hex

%import <mirandaExtensions>
%import <state> (>>)/st_right
%import <vector>


|| a buffer is a vector of 16 32-bit words
buffer == vector int

emptyBuffer :: buffer
emptyBuffer = v_rep 16 0

|| pack string S into a 16-word buffer, padding with a terminating '0x80' byte and zero bytes to
|| fill out the full 64 bytes, if it the string is less than 64 bytes long
|| this is done mostly strictly (using case exprs) at a low level to reduce thunk overhead, since
|| it is heavily used.
makeBuffer :: string -> buffer
makeBuffer s
    = st_evalState (go 0 s >> v_unsafeFreeze vbuf) ()
      where
        vbuf = v_thaw $ emptyBuffer

        go 16 cs = st_pure ()

        go i cs
            = case cs of
                []      -> v_unsafeWrite vbuf i 0x80
                a : cs' -> case code a of w -> go1 i w cs'

        go1 i w cs
            = case cs of
                []      -> case 0x8000 .|. w of w' -> v_unsafeWrite vbuf i w'
                b : cs' -> case code b .<<. 8 .|. w of w' -> go2 i w' cs'


        go2 i w cs
            = case cs of
                []      -> case 0x800000 .|. w of w'       -> v_unsafeWrite vbuf i w'
                c : cs' -> case code c .<<. 16 .|. w of w' -> go3 i w' cs'

        go3 i w cs
            = case cs of
                []      -> case 0x80000000 .|. w of w'     -> v_unsafeWrite vbuf i w'
                d : cs' -> case code d .<<. 24 .|. w of w' -> case v_unsafeWrite vbuf i w' of _ -> case i + 1 of i' -> go i' cs'

|| put the final message bit length in the last 2 words of the buffer
addMsgLen :: int -> buffer -> buffer
addMsgLen len buf
    = buf // [(14, lo), (15, hi)]
      where
        bitLen = len * 8
        lo     = bitLen .&. 0xffffffff
        hi     = bitLen .>>. 32

md5State ::= MD5 int int int int

md5InitState :: md5State
md5InitState = MD5 0x67452301 0xefcdab89 0x98badcfe 0x10325476

md5Hex :: md5State -> string
md5Hex (MD5 a b c d)
    = concatMap toHex [a, b, c, d]
      where
        toHex n
            = foldr go [] [0 .. 3] 
              where
                code0      = code '0'
                codeA      = code 'a'

                hexDigit n = decode (code0 + n),      if n < 10
                           = decode (codeA + n - 10), otherwise

                go i k
                    = hexDigit hi : hexDigit lo : k
                      where
                        hi = (n .>>. (8 * i + 4)) .&. 0x0f
                        lo = (n .>>. (8 * i))     .&. 0x0f

md5Add :: md5State -> md5State -> md5State
md5Add (MD5 a b c d) (MD5 f g h i)
    = MD5 (a + f .&. mask) (b + g .&. mask) (c + h .&. mask) (d + i .&. mask)
      where
        mask = 0xffffffff

tableS
    = v_fromList [ 7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22 
                 , 5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20 
                 , 4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23 
                 , 6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21
                 ]

tableK
    = v_fromList [ 0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee
                 , 0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501
                 , 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be
                 , 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821
                 , 0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa
                 , 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8
                 , 0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed
                 , 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a
                 , 0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c
                 , 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70
                 , 0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05
                 , 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665
                 , 0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039
                 , 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1
                 , 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1
                 , 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
                 ]

|| rotate a 32-bit value left by n bits
|| most time spent here and in md5Rotate, so written in strict form to
|| reduce thunk production
leftRotate :: int -> int -> int
leftRotate v n
    = case (1 .<<. n) - 1 of
        mask -> case (v .>>. (32 - n)) .&. mask of
                  lo -> case (v .<<. n) .&. 0xffffffff of
                          hi -> hi .|. lo

|| rotate md5State using two additional values e and f, and index i into the MD5 tables
md5Rotate :: md5State -> int -> int -> int -> md5State
md5Rotate (MD5 a b c d) e f i
    = case tableK !! i + a + e + f of
        v -> case tableS !! i of
               n -> case b + leftRotate v n of
                      a' -> MD5 d a' b c

|| perform a round of 5 stages on the md5State and a buffer
md5Round :: md5State -> buffer -> md5State
md5Round st buf
    = md5Add st st4
      where
        || Note: this is written non-point-free, with explicit computation for st1, st2, etc. because if it were all
        || combined into a single expr, the expr would be too complex to do full inlining, thus decreasing overall
        || performance.  By making each computation into its own thunk, each thunk can get inlining done individually
        || without triggering the maxComplexity inlining limit.
        st1 = stage1 st
        st2 = stage2 st1
        st3 = stage3 st2
        st4 = stage4 st3

        stage1 st
            = foldl go st [0 .. 15]
              where
                go st i
                    = md5Rotate st (e st) f i
                      where
                        e (MD5 _ b c d) = (b .&. c) .|. (complement b .&. d)
                        f = buf !! i
        stage2 st
            = foldl go st [16 .. 31]
              where
                go st i
                    = md5Rotate st (e st) f i
                      where
                        e (MD5 _ b c d) = (d .&. b) .|. (complement d .&. c)
                        f = buf !! ((i * 5 + 1) .&. 15)
        stage3 st
            = foldl go st [32 .. 47]
              where
                go st i
                    = md5Rotate st (e st) f i
                      where
                        e (MD5 _ b c d) = b .^. c .^. d
                        f = buf !! ((i * 3 + 5) .&. 15)
        stage4 st
            = foldl go st [48 .. 63]
              where
                go st i
                    = md5Rotate st (e st) f i
                      where
                        e (MD5 _ b c d) = c .^. (b .|. complement d)
                        f = buf !! ((i * 7) .&. 15)

md5Hash :: string -> md5State
md5Hash s
    = doHash md5InitState . chunk 64 $ s
      where
        msgLen = #s

        || turn a null string into a null 64-byte chunk
        doHash st [] = doHash st [""]

        || last block of hash -- 3 possible scenerios
        || 1) block is full 64 bytes, so do another full block round, then do a last block with an empty string
        || 2) block is >= 56 bytes, so terminate block with 0x80 and the rest 0x00, then do a last block with an empty string
        || 3) block is < 56 bytes, so terminate block with 0x80 and add the msg-len pad
        doHash st [s]
            = doHash st [s, ""],  if #s == 64   || last block is full block -- hash it and do a final hash with a terminated empty block
            = md5Round st lenBuf, if #s < 56    || last block is partial block with enough room to add termination and size
            = doEmptyLast,        otherwise     || last block is partial block with room for termination, but not size -- add size only to empty block
              where
                buf    = makeBuffer s
                lenBuf = addMsgLen msgLen buf

                doEmptyLast
                    = case md5Round st buf of
                        st' -> md5Round st' lenEmpty
                      where
                        lenEmpty = addMsgLen msgLen emptyBuffer

        || full block, do a full block round and continue
        doHash st (s : bs)
            = case md5Round st $ makeBuffer s of
                st' -> doHash st' bs
