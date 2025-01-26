|| 
|| bignum - An infinite precision calculator and function library
||          for the Miranda functional programming language.
||
|| Copyright (c) Martin Guy, 6 November 2005.
||
|| Email: <martinwguy@yahoo.it>     Web: http://bignum.sourceforge.net
|| Free bignum server in Internet: telnet medialab.freaknet.org 31415
||
|| You can do as you please with this code, as long as you neither try to
|| prevent anyone else from doing as *they* please with it, nor tell lies!
||
|| This is an original work except for edigits, modified from David Turner's
|| example script "miralib/ex/edigits.m" distributed with the Miranda runtime
|| system and included here with permission,
|| and the scientific algorithms ripped from the library of GNU bc 1.06.
||
|| History:
|| Version 0, April 1986, Canterbury, UK: addition and multiplication
||    of positive numbers only.
|| Version 1, October 2001, Raddusa, Sicily, with full arithmetic, e, pi, 
||    sqrt, sin, cos, atan.
|| Version 2, 22 dec 2003, with logarithms and space and speed improvements.
|| Version 3, October 2004, Newcastle, England:    addition of bn_abs, bn_trunc,
||    bn_frac, bn2int and derived scientific fns. bn_rnd removed.
||    More speed improvements. First Haskell version.
|| Version 3.2, 22 Sept 2005: modifications to commentary for sourceforge.
|| Version 4, October 2005: sped up sin(huge_value), added asin and acos.
|| Version 4a, September 2024: ported to Miranda2 by Tim Olson
||
|| Numbers are represented as as infinite series of numbers which in a decimal
|| system would be referred to as decimal places.
|| The number represented by the bignum (4, [5, 2, 7]) is 0.527 * 10^4.
|| Support is not guaranteed for bases other than 10, but no base is
|| assumed by almost any of the functions.
||
|| bignum is a tuple containing sign, exponent and mantissa in that order.
||
|| in the code we have three principal sorts of functions:
|| bn_*     treat bignums (sign, exp, mantissa)
|| ubn_* treat unsigned bignums (exp, mantissa)
|| m_* just treat mantissae [num]
|| and normally the bn_ functions use ubn functions to achieve their result,
|| which in turn call m_ functions to perform the detailed calculation.

%export bignum bn ibn cmpbignum showbignum bn_show bn_0 bn_1 bn_2 bn_is_neg bn_is_zero bn_cmp bn_eq bn_ne bn_gt bn_lt bn_ge bn_le bn_abs bn_neg bn_trunc bn2int bn_frac bn_add bn_sub bn_twice bn_half bn_times bn_mul bn_raise bn_quot bn_div bn_sqr bn_sqrt bn_e bn_pi bn_pi_2 bn_pi_4 bn_2_pi bn_ln bn_exp bn_pow bn_sin bn_cos bn_tan bn_asin bn_acos bn_atan bn_sinh bn_cosh bn_tanh bn_asinh bn_acosh bn_atanh m_is_zero

%import <mirandaExtensions>


|| For debugging, check for impossible internal conditions
sanity_checks = True

|| The number base we are working in.
|| It's not guaranteed that anything other than 10 will work - but it might.
|| ubn_text (text to bignum conversion) only works up to base 10.
base = 10

|| Constants derived from base. Literal values are notably faster than values
|| derived from simple formulae of constants (which is strange - shouldn't
|| Miranda reduce them to a singularity on first evaluation?)

|| base_1 = base - 1
base_1 = 9
|| base_2 = base ^2
base_2 = 100
|| base_div_2 = base $div 2
base_div_2 = 5

|| The number of digits that bn_show prints.  if scale = 0, infinite.
scale = 0
|| The truncation function used in the printing functions below.
scaletrunc x
    = take scale x, if scale > 0
    = x, otherwise


|| Here beginneth...

bignum   == ( sign, exponent, mantissa )
ubignum  == ( exponent, mantissa )
sign     == int
exponent == int
mantissa == [int]

|| Note: herein, when we use type "mantissa", we mean [num] in which
|| every element x is 0 <= x < base.
|| In all cases dealing with partial results which may contain values
|| outside the permitted range for our number base, we say [num].

|| Some simple constants
bn_0 = bn_make (1, 0, [])
bn_1 = bn_make (1, 1, [1])
bn_2 = bn_make (1, 1, [2])

|| unsigned bignum constants
ubn_0 :: ubignum
ubn_0 = (0, [])

||
|| ---------- simple conversion functions ----------
||

|| make a bignum from its parts and vice versa (type-fooling glue)
bn_make (s,e,m) = (s,e,m)
bn_unmake (s,e,m) = (s,e,m)

|| functions to select parts of a bignum
sign_of (s,e,m) = s
exponent_of (s,e,m) = e
mantissa_of (s,e,m) = m

|| functions to select parts of a ubignum
uexponent_of (e,m) = e
umantissa_of (e,m) = m

|| Convert bignum to unsigned bignum and vice versa
bn2ubn (s,e,m)
    = error ("Internal error: bn2ubn of negative number: " ++ showbignum (s, e, m)), if sanity_checks & s ~= 1
    = (e,m), otherwise

ubn2bn (e,m) = bn_normalise (1,e,m)

|| bn_normalise converts a bignum into "canonical form", that is to say:
|| - the first digit of the mantissa is significant (not 0).
|| - convert the value zero to canonical form (eliminating "-0").
|| To do this it strips off leading zeroes from mantissa, adjusting exponent
|| accordingly.

bn_normalise (s, e, [])     = bn_0
bn_normalise (s, e, 0:m) = bn_normalise (s, e-1, m)
bn_normalise other     = other

ubn_normalise (e, [])  = ubn_0
ubn_normalise (e, 0:m) = ubn_normalise (e-1, m)
ubn_normalise other      = other

|| m_trim strips trailing zero digits from a mantissa.  It's tempting to use
|| it liberally because trailing zeroes cause pointless calculation.
|| In practice, it's hardly ever worth doing.  Instead, we test for a few cases
|| here and there where it costs us next to nothing to check for a generated
|| trailing zero (e.g. in bn_twice and bn_carry1), and then just apply m_trim
|| in showbignum to suppress trailing zeroes in the output.
||
|| This is probably where sums like "bn_e $bn_sub bn_e" bottom out.
|| Would it be better not to strip trailibg zeroes and output 0.0000000....
|| instead?  I think not.

m_trim :: mantissa -> mantissa
m_trim (0:x)
    = [],        if m_is_zero x
    = 0 : m_trim x, otherwise
m_trim (a:x) = a : m_trim x
m_trim [] = []

|| ubignum and bignum versions:
ubn_trim :: ubignum -> ubignum
ubn_trim (e,m) = (e, m_trim m)

bn_trim (s,e,m) = (s, e, m_trim m)


||
|| ---------- text <-> bignum conversion functions ----------
||

|| Convert bignum to text.
bn_show (s,e,m)
    = sign ++ ubn_show (e, m_trim m)    || suppress trailing zeroes
      where
      sign    = "-",    if s < 0
        = "",    otherwise

|| We produce output in three forms: .456, 123 and 123.456.
|| ubn_show deals with the first form, ubn_show' with the other two forms.
ubn_show :: (int,[int]) -> [char]
ubn_show (e,m)
    = "0", if m_is_zero m
    = ubn_show' (e,m), if e > 0
    = "." ++ scaletrunc ((rep (-e) '0') ++ m_show m), if e <= 0

|| Deal with bignums whose exponent > 0  (i.e. >= 1.0)
ubn_show' :: (int,[int]) -> [char]
ubn_show' (e, []) = rep e '0'    || Do final zeroes (if any) of integral bn.
ubn_show' (0, m) = '.' : scaletrunc (m_show m)
ubn_show' (e, (a:x)) = mkdigit a : ubn_show' (e-1, x)

m_show :: [int] -> [char]
m_show m = map mkdigit m

mkdigit :: int -> char
mkdigit n = decode(n + code '0'), if 0 <= n < 10
          = decode(n - 10 + code 'a'), if 10 <= n < 36

|| bn: Convert text to bignum
|| Syntax: ['-'] [digit] [digit]*  \/  ['-'] [digit]* ++ '.' ++ [digit]*
|| Once we've found a '.', we can accept an infinite number of decimal places.
|| Until we find a '.' (or the end of the string), we don't know what the
|| exponent will be.
|| For simplicity, we also allow "", "-", "." and "-." as synonyms for 0.
bn a = bn_normalise (bn_trim (bn_text a))
       where
       || We don't make bn_text public because it returns unnormalised bignums
       || (thus removing a trap for the unwary).
       bn_text ('-':x) = (-1,e,m) where (e,m) = ubn_text x
       bn_text x = (1,e,m) where (e,m) = ubn_text x

|| Convert text to unsigned bignum
ubn_text :: [char] -> ubignum
ubn_text (a:x)
    = (0, m_text x), if a ==. '.'
    = (xe+1, (code a - code '0'):xm), if isdigit a
    = error "Non-digit in bignum text", otherwise
      where (xe,xm) = ubn_text x
ubn_text [] = (0,[])

m_text :: [char] -> mantissa
m_text (a:x)
    = (code a - code '0'): m_text x, if isdigit a
    = error "Non-digit in bignum text", otherwise
m_text [] = []

isdigit :: char -> bool
isdigit c = code '0' <= code c <= code '9'

|| Convert a Miranda integer to a bignum (the lazy way!)
|| We don't allow people to import smelly floating point values!
ibn :: int -> bignum
ibn i = bn (showint i)

||
|| ----- utility functions used here and there in the rest of the library -----
||


|| ubn_same_exp takes a pair of ubignums and returns the same pair such that
|| they have the same exponent.  This means prefixing a load of zeroes and
|| upping the exponent of the ubignum with the smaller exponent.

ubn_same_exp :: (ubignum, ubignum) -> (ubignum, ubignum)
ubn_same_exp (a, b)
    = ((ae,am), (be,bm))       , if ae == be
    = (ubn_abnormalise be a, b) , if ae < be
    = (a, ubn_abnormalise ae b) , if ae > be
      where (ae,am) = a
        (be,bm) = b

|| ubn_abnormalise sets the exponent of its second parameter to its first
|| parameter, giving as a result an un-normalised bignum. This simply involves
|| prefixing a quantity of zeroes to the mantissa and adjusting the exponent.
|| Of course, the new exponent must be >= the old exponent!

ubn_abnormalise :: exponent -> ubignum -> ubignum
ubn_abnormalise new_e (old_e, m)
    = error "ubn_abnormalise called with duff parameters", if sanity_checks & new_e < old_e
    = (new_e, prefix0s (new_e - old_e) m), otherwise

|| Add a number of zeroes to the head of a mantissa.
prefix0s n x
    = take n [0,0..] ++ x

|| Return absolute (positive) value of a bignum
bn_abs (s,e,m) = (1,e,m)

|| Negate a bignum, avoiding the "-0" syndrome.
bn_neg (s,e,m)
    = bn_0, if m_is_zero m
    = (-s,e,m), otherwise

|| Truncate a Bigfloat towards 0, giving a Bigfloat
bn_trunc (s,e,m)
    = bn_0, if e <= 0 \/ m_is_zero m
    = (s,e,take e m), otherwise 

|| Truncate a bignum towards 0, giving a num
bn2int (s,e,m)
    = 0, if e <= 0 \/ m_is_zero m        || Not strictly necessary
    = s * digits2int (take e (m ++ repeat 0)), otherwise

digits2int
    = foldl f 0
      where
        f a b = base * a + b

|| Just give the part after the decimal point
bn_frac (s,e,m)
    = (s,e,m), if e <= 0    
    = bn_normalise (s,0,drop e m), otherwise

||
|| ----------- Stuff for addition/subtraction of bignums ------------
||

|| bn_add gives the arithmetic sum of two bignums.
    || 1) both +ve or both -ve
    || 2) sign differs and absolute value of a >= abs value of b.
    || 3) sign differs and abs(a) < abs(b)

bn_add (as,ae,am) (bs,be,bm)
    = bn_normalise (s,e,m)
      where
          (s,(e,m))
              = (as, ubn_add (ae,am) (be,bm)), if as == bs
              = (as, ubn_sub (ae,am) (be,bm)), if (ae,am) $ubn_ge (be,bm)
              = (bs, ubn_sub (be,bm) (ae,am)), otherwise

|| bn_sub performs subtraction of 2 bignums.
bn_sub a b = bn_add a (bn_neg b)

|| ubn_add gives the arithmetic sum of two unsigned bignums.
|| Strategy: denormalise the arguments so that the have the same exponent,
|| form the sum of the mantissae, then perform the sum after tacking a 0 onto
|| the heads to catch possible carry from the first digit. It would be
|| slightly faster to do m_carry ( 0: m_add2 ... ) but this is simpler.
ubn_add :: ubignum -> ubignum -> ubignum
ubn_add = ubn_add2

|| Older, simpler implementation of ubn_add.
ubn_add1 :: ubignum -> ubignum -> ubignum
ubn_add1 a b 
    = (ae+1, m_addcarry0 am bm)
      where
      ((ae,am),(be,bm)) = ubn_same_exp (a, b)

|| ubn_add2 instead takes advantage of unequal exponents in the operands to
|| avoid putting a string of 0s on the head of one parameter and then adding
|| them in uselessly. To be able to do this
|| 1) the exponents must differ by at least two
|| 2) there must be a non-9 digit in the first, non-overlapping digits of the
||    bigger operand, to prevent possible carry due to the second from affecting
||    the first digit of the result.

ubn_add2 :: ubignum -> ubignum -> ubignum
ubn_add2 (ae,am) (be,bm)
    || First, make sure ae >= be
    = (ae+1, m_addcarry0 am bm), if ae == be
    = ubn_add2' (ae,am) (be,bm), if ae > be
    = ubn_add2' (be,bm) (ae,am), otherwise

|| ubn_add2' knows that ae > be
ubn_add2' :: ubignum -> ubignum -> ubignum
ubn_add2' (ae,am) (be,bm)
      || First case: difference is just one
    = ubn_add2'a (ae,am) (be,bm), if ae == be + 1
      || Second case (the fun one): diff >= 2
    = ubn_add2'b (ae,am) (be,bm), if ae > be + 1

|| ubn_add2'a: ae = be + 1
|| It is (just barely) worth separating these two cases, as far as
|| speed is concerned, instead of always using m_addcarry0.
ubn_add2'a :: ubignum -> ubignum -> ubignum
ubn_add2'a (ae,(a:x)) (be,bm)
        || a) exponent differs by 1 and first digit < 9:
        ||    there can be no overflow from digit 1.
    = (ae, m_addcarry (a:x) (0:bm)), if a < base_1
        || b) exponent differs by 1 and first digit of a is 9:
        ||    there may be an overflow from the first digit.
    = (ae+1, m_addcarry0 (a:x) (0:bm)), otherwise

|| ubn_add2'b: ae >= be + 2
|| Make sure there can be no overflow from the result
|| and pass the work on to the lookahead function.
ubn_add2'b :: ubignum -> ubignum -> ubignum
        || a) a = 0
ubn_add2'b (ae,[]) (be,bm)    = (be,bm)
        || b) second digit of a is (implied) 0
ubn_add2'b (ae,[x]) (be,bm)    = (ae, x:0:(prefix0s (ae-be-2) bm))
        || c) general case
ubn_add2'b (ae,am) (be,bm)    
      || There can be no carry into the first digit if the difference
      || between the exponents is >= 2 and any of the intervening digits
      || are less than (base-1), 'cos they are sure to absorb any carry
      || of 1 from the first pair of overlapping digits.
    = (ae, ubn_add2'' (ae,am) (be,bm)), if any_lt_base_1 am (ae-be)
      || All the protruding digits of a are 9!  This is hard to believe...
    = (ae+1, m_addcarry0 am bm), otherwise

|| ubn_add2'' knows that ae-be >= 2 and that there can be no overflow
|| from the first digit of the result. Since its result always has
|| the same exponent as its first parameter, we just return the mantissa.
ubn_add2'' :: ubignum -> ubignum -> mantissa
ubn_add2'' (ae,[]) (be,bm)    = prefix0s (ae-be) bm
ubn_add2'' (ae,[a]) (be,bm)    = a:0:(prefix0s (ae-be-2) bm)
ubn_add2'' (ae,am) (be,bm)
    = ubn_add2''a (ae,am) (be,bm), if ae == be + 2    || End of recursion
    = ubn_add2''b (ae,am) (be,bm), otherwise     || This one may recurse
    
|| ae = be+2: End of recursion
ubn_add2''a (ae,(p:q:x)) (be,bm)
    = p : m_addcarry (q:x) (0:bm), if q < base_1    || WIN!
    = m_addcarry (p:q:x) (0:0:bm), otherwise    || q = base_1 :-(

|| ae > be+2: Can recurse
ubn_add2''b (ae,(a:x)) (be,bm)
    = a : (ubn_add2'' (ae-1,x) (be,bm)), if any_lt_base_1 x (ae-be-1)   || WIN AND PLAY AGAIN!
    = m_addcarry (a:x) (prefix0s (ae-be) bm), otherwise    || Boring case

|| Are any of the first n digits of a mantissa less than base-1 ?
|| (if there are less than n digits in the mantissa, then yes,
|| because the omitted digits are implicitly 0)
any_lt_base_1 x n = or (map (< base_1) (take n x))


|| ubn_sub performs subtraction of 2 unsigned bignums.
|| It's up to the caller to ensure that a >= b.
|| This uses the version of subcarry that notices when arg2 is shorter than
|| arg1, so that "n $bn_sub bn_1" costs almost nothing instead of running a
|| pointless carry down an infinite list.
|| TODO: take advantage of ae > be and return the initial part of am
|| unaltered rather than subtracting a string of zeroes from it.
ubn_sub :: ubignum -> ubignum -> ubignum
ubn_sub = ubn_sub1

ubn_sub0 :: ubignum -> ubignum -> ubignum
ubn_sub0 a b
    = (ae, m_subcarry2 am bm)
      where
      ((ae,am),(be,bm)) = ubn_same_exp (a, b)

ubn_sub1 :: ubignum -> ubignum -> ubignum
ubn_sub1 (ae,am) (be,bm)
    = error "Internal error in ubn_sub1: exponents are the wrong way round",
      if sanity_checks & (ae < be)
    = (ae, m_subcarry2 am (prefix0s (ae-be) bm)),
      otherwise

|| m_addcarry performs addition of 2 mantissas and carry simultaneously.
|| As usual, it must be guaranteed that there will be no carry from the
|| first digit.  Used in add_skew_carry.
|| It only applies carry1 to the part resulting from an addition.
m_addcarry :: mantissa -> mantissa -> mantissa
m_addcarry a b
    = m_carry1a tocarry ++ nocarry
      where
      (tocarry,nocarry) = m_add2 a b

|| m_addcarry0 is like m_addcarry except that it prefixes a zero to the
|| result before performing the carry (thereby guaranteeing no overflow)
m_addcarry0 :: mantissa -> mantissa -> mantissa
m_addcarry0 a b
    = m_carry1a (0:tocarry) ++ nocarry
      where
      (tocarry,nocarry) = m_add2 a b

|| m_subcarry subtracts b from a and propagates carry (ok, borrow)
m_subcarry :: mantissa -> mantissa -> mantissa
m_subcarry a b = m_borrow1 (m_sub a b)

|| If the second operand is shorter that the first, the final part is
|| returned "as is" since there's no need to do any borrowing from it.
m_subcarry2 a b
    = (m_borrow1a toborrow) ++ noborrow
      where
      (toborrow,noborrow) = m_sub2 a b

|| "m_add" adds two mantissas without performing carry across the result
|| leaving each digit with value from 0 to base*2
m_add :: mantissa -> mantissa -> mantissa
m_add (a:x) (b:y) = (a+b):m_add x y
m_add a [] = a
m_add [] b = b

|| m_add2 returns the mantissa in two parts: the first part is the one
|| resulting from addition (to which carry must be applied); the second
|| part is the part resulting from unequal lengths of a and b.
m_add2 :: mantissa -> mantissa -> (mantissa,mantissa)
m_add2 (a:x) (b:y)
    = ((a+b):tocarry, nocarry)
      where
      (tocarry,nocarry) = m_add2 x y
m_add2 a [] = ([],a)
m_add2 [] b = ([],b)

|| "m_sub" subtracts one mantissa from another without performing borrow,
|| leaving each digit with value from -(base-1) to +(base-1).
m_sub :: mantissa -> mantissa -> mantissa
m_sub (a:x) (b:y) = (a-b):m_sub x y
m_sub a [] = a
m_sub [] b = map neg b

|| m_sub2, like m_add2, returns the part that borrowing needs to be performed
|| on and the part it doesn't.
|| Unlike add2, we only win if the first param is longer than the second.
m_sub2 :: mantissa -> mantissa -> (mantissa,mantissa)
m_sub2 (a:x) (b:y)
    = ((a-b):tocarry, nocarry)
      where
      (tocarry,nocarry) = m_sub2 x y
m_sub2 a [] = ([], a)
m_sub2 [] b = (map neg b, [])

|| m_carry1 performs carry throughout a mantissa.
|| Each term must be <= (base-1)*2.
|| This means that the maximum carry from any digit to the next will be 1,
|| To determine the first digit, it inspects at least the first two elements.
|| It assumes that the digits are sufficient to hold the result, ie that
|| there will be no carry from the first digit.
m_carry1 :: [int] -> [int]
m_carry1 (a:b:x)
    = a : m_carry1 (b : x)              , if b < base_1
    = (a + 1) : m_carry1 ((b - base) : x)      , if b > base_1
    = (a + carry): m_carry1 ((b-carry*base):x), otherwise
      where carry = m_carry1from (b:x)
m_carry1 [a]
    = [a], if a > 0
    = [], otherwise
m_carry1 [] = []

|| m_carry1a is like m_carry1 except that it never removes trailing zeroes
m_carry1a :: [int] -> [int]
m_carry1a (a:b:x)
    = a : m_carry1a (b : x)              , if b < base_1
    = (a + 1) : m_carry1a ((b - base) : x)      , if b > base_1
    = (a + carry): m_carry1a ((b-carry*base):x), otherwise
      where carry = m_carry1from (b:x)
m_carry1a [a] = [a] 
m_carry1a [] = []


|| m_carry1from returns the carry from a list of digits
|| where each digit is <= (base-1)*2
|| To determine the carry from a list, it inspects at least the first element.
m_carry1from :: [int] -> int
m_carry1from (a:x)
    = 0        , if a < base_1
    = 1        , if a > base_1
    = m_carry1from x, otherwise
m_carry1from [] = 0

|| m_borrow1 takes a list of digits from -9 to +9 and performs the borrowing
|| necessary to bring them all in the range 0-9.  The first term of the list
|| must not be negative, and must be sufficient to provide any borrowing
|| required of it by the following digit(s).
m_borrow1 :: [int] -> [int]
m_borrow1 (a:b:x)
    = a : m_borrow1 (b:x)                  , if b > 0
    = (a-1) : m_borrow1 ((b+base):x)          , if b < 0
    = (a-borrow) : m_borrow1 ((b + borrow * base) : x), otherwise
      where borrow = m_borrow1from (b:x)
m_borrow1 [a]
    = error "Internal error: Negative digit in m_borrow1",
      if sanity_checks & a < 0
    = [], if a == 0
    = [a], otherwise
m_borrow1 [] = []

|| m_borrow1a is like m_borrow1 except that it doesn't strip trailing zeroes.
m_borrow1a :: [int] -> [int]
m_borrow1a (a:b:x)
    = a : m_borrow1a (b:x)                  , if b > 0
    = (a-1) : m_borrow1a ((b+base):x)          , if b < 0
    = (a-borrow) : m_borrow1a ((b + borrow * base) : x), otherwise
      where borrow = m_borrow1from (b:x)
m_borrow1a [a] = [a]
m_borrow1a [] = []

m_borrow1from (a:x)
    = 0         , if a > 0
    = 1         , if a < 0
    = m_borrow1from x, otherwise
m_borrow1from [] = 0

|| Simple doubler. It gains in lookahead because we know that, when you double
|| a number in an even number base, the value of input digit n+2 cannot affect
|| the result digit n.

bn_twice (s,e,m)
    = bn_0, if m_is_zero m
    = bn_add (s,e,m) (s,e,m), if base $mod 2 ~= 0
    = (s, e, m_twice m), if hd m < base_div_2
    = (s, e+1, m_twice (0:m)), otherwise

|| m_twice is always applied to a list whose first element is < base/2
m_twice :: [int] -> [int]
m_twice (a:b:x)
    = (a+a) : m_twice (b:x), if b < base_div_2
    = (a+a+1) : m_twice ((b - base_div_2) : x), otherwise
m_twice [0] = []    || Final digit 5 (ok, base/2) generates a trailing 0.
m_twice [a] = [a+a]
m_twice [] = []

bn_half (s,e,m)
    || m_half only works for even number bases
    = bn_normalise (s, e, m_half m), if base $mod 2 == 0
    = bn_quot (s,e,m) 2, otherwise

m_half (a:x)
    = (a $div 2) : m_half x, if a $mod 2 == 0
    = (a $div 2) : (m_add [base_div_2] (m_half x)), otherwise
m_half [] = []
      
||
|| --------- Simple comparison functions ----------
||

|| Is a bignum negative?
bn_is_neg (s,e,m) = (s < 0)

|| Is it zero?
bn_is_zero (s,e,m) = m_is_zero m

|| Is it a whole number?  Yes, if there are no significant digits after the .
bn_is_integer (s,e,m) = m_is_zero (drop e m)


|| Return 0 if the two numbers have the same value,
|| return 1 if the first number is greater than the second
|| return -1 if the second number is greater than the first.
|| Assumes that the parameters are normalised.
bn_cmp (as,ae,am) (bs,be,bm)
    = as * ubn_cmp (ae,am) (be,bm), if as == bs
    = as, otherwise

|| tests functions for equality, inequality, less then, less that or equal,
|| greater than, greater than or equal.
bn_eq a b = bn_cmp a b == 0
bn_ne a b = bn_cmp a b ~= 0
bn_lt a b = bn_cmp a b < 0
bn_le a b = bn_cmp a b <= 0
bn_gt a b = bn_cmp a b > 0
bn_ge a b = bn_cmp a b >= 0

|| Unsigned bignum comparison; result as per bn_cmp
ubn_cmp :: ubignum -> ubignum -> int
ubn_cmp (ae,am) (be,bm)
    = 1        , if ae > be
    = -1        , if ae < be
    = m_cmp am bm    , if ae == be

|| tests functions for equality, inequality, less then, less that or equal,
|| greater than, greater than or equal.
|| Type declarations are inherited from ubn_cmp.
ubn_eq a b = ubn_cmp a b == 0
ubn_ne a b = ubn_cmp a b ~= 0
ubn_lt a b = ubn_cmp a b < 0
ubn_le a b = ubn_cmp a b <= 0
ubn_gt a b = ubn_cmp a b > 0
ubn_ge a b = ubn_cmp a b >= 0

m_cmp :: mantissa -> mantissa -> int
|| Compare two mantissae.
m_cmp (a:x) (b:y)
    = 1        , if a > b
    = -1       , if a < b
    = m_cmp x y, if a == b
m_cmp a []
    = 0, if m_is_zero a
    = 1, otherwise
m_cmp [] b
    = 0    , if m_is_zero b
    = -1   , otherwise

|| equal, not equal, less then [or equal to], greater than [or equal to]
|| for mantissae.
m_eq :: mantissa -> mantissa -> bool
m_ne :: mantissa -> mantissa -> bool
m_lt :: mantissa -> mantissa -> bool
m_gt :: mantissa -> mantissa -> bool
m_le :: mantissa -> mantissa -> bool
m_ge :: mantissa -> mantissa -> bool

m_eq (a:x) (b:y)
    = False, if a ~= b
    = m_eq x y, otherwise
m_eq a [] = m_is_zero a
m_eq [] b = m_is_zero b

|| optimised m_lt, used in m_div
m_lt (a:x) (b:y)
    = (a < b), if a ~= b
    = m_lt x y, otherwise
m_lt a [] = False
m_lt [] b = ~ (m_is_zero b)

|| reuse code
m_ne a b = ~(m_eq a b)
m_gt a b = m_lt b a
m_ge a b = ~(m_lt a b)
m_le a b = ~(m_lt b a)

m_is_zero :: mantissa -> bool
|| Are all elements of a mantissa equal to zero?
|| It'd be nice to use a "fold" function here, but I've seen no guarantee that
|| the "&" and "\/" operators are lazy on the right hand parameter when the left
|| hand one determines the result (or vice versa), even though it seems so
|| experimentally.
|| Was:
|| m_is_zero [] = True
|| m_is_zero (a:x)
||     = False,    if a > 0
||     = m_is_zero x,    if a == 0
||     = error "negative number in mantissa!", otherwise
|| Note: The following version applied to [0,0..] provokes
|| "impossibile tag (-9) in reduce" rather than "Out of heap space" from the
|| Linux version 2.025 (22 mar 97) of the Miranda system but is much faster.
m_is_zero x = and (map (== 0) x)

||
|| ------------- Simple multiply by an integer -------------
||

|| bn_times multiplies bignum by an integer, done by repeated addition.
|| The *first* parameter is the integer, which seems more convenient for
|| partial parameterisation purposes.
|| bn_add normalises the result for us.
bn_times n b
    = bn_times' n b,        if n > 0
    = bn_neg (bn_times' (-n) b),    if n < 0
    = bn_0,                if n == 0

|| reduced case: n always >= 1
|| Optimisation: (2n) x b = (n x b) + (n x b)
|| Optimisation to do: multiply all digits by n, then do a super-carry.
bn_times' n b
    = bn_twice (bn_times' (n $div 2) b),    if n $mod 2 == 0
    = b $bn_add (bn_times' (n-1) b),    if n > 2
    = b,                    if n == 1

|| Same stuff for mantissae.  Caller must guarantee no overflow, and
|| maximum n is base-1.
m_times :: int -> mantissa -> mantissa
m_times n m
    = error ("m_times " ++ showint n), 
        if sanity_checks & ~(0 <= n < base)
    = m_carry2 (map (* n) m), otherwise

||
|| ------------- Long multiplication -------------
||

|| bn_mul: Multiply two bignums.  bn_sqr is a modified version of this.

bn_mul (as,ae,am) (bs,be,bm)
    = bn_normalise (as*bs, e, m)
      where (e,m) = ubn_mul (ae,am) (be,bm)

ubn_mul :: ubignum -> ubignum -> ubignum
|| Add a leading 0 so that there's somewhere for the carry to propagate into.
|| If we were to simply m_multiply the mantissas without prefixing a 0, the
|| exponent of the result would be (ae + be - 1) (think: 0.1 * 0.1 = 0.01).
|| The extra digit that m_mul prefixes to the result cancels the "- 1".
ubn_mul (ae,am) (be,bm) = (ae+be, m_mul am bm)

|| Split a and b into
||    a = (a0 + a1 * base^n) 
||    b = (b0 + b1 * base^n) 
|| then a * b = a0*b0 + (a0*b1+b0*a1)*base^n + (a1*b1)*base^2n
|| where n == -1
|| Use this hoping that ubn_add2'' can gain an advantage from the
|| difference of 2 in its parameters' exponents
||
|| This gives right results, but is many times slower than the hairy version,
|| and gets disproportionately slower as the parameters get longer.
||
|| There may be some mileage in using it with larger values of n.
ubn_mul2 (ae,[]) (be,bm) = ubn_0
ubn_mul2 (ae,am) (be,[]) = ubn_0
ubn_mul2 (ae,(a0:a1)) (be,(b0:b1))
    = (ae+be, ubn_add2''
        (ae+be, m_addcarry
         (m_carry2 [0,a0*b0])
         (m_addcarry0 (m_carry2 (0 : map (* a0) b1))
                    (m_carry2 (0 : map (* b0) a1)) ) )
        (ubn_mul2 (ae-1, a1) (be-1, b1))
      )
        
|| m_mul multiplies two mantissas.
||
|| NOTE! m_mul prefixes a digit to the result so that there's somewhere for
|| the carry from the first result digit to propagate into,
|| so callers must adjust result exponents accordingly.
||
|| Strategy:
|| sum the columns of the skewed cross product and then do an intelligent
|| super-carry on the result.  The super-carry works by applying a one-digit
|| carry to the list of column sums until we can be sure that the maximum
|| value of each column is (base * 2).  Then it applies m_carry.
|| [Alternate version to try: apply one-digit carry until maximum column value
|| is (base - 1) ^ 2, then apply m_carry2.]
||
|| We do this by giving two lists to the carry-preparation function:
|| - the list of column sums
|| - the list of maximum values for the column sums.
|| and applying the one-digit carry to both lists until the maximum value
|| of the leading digits is within the required maximum.

m_mul :: [int]->[int]->[int]
m_mul a b = supercarry (0:(partial_products a b)) max_mul_vals

supercarry list maxvals = m_carry1 (reduce_for_carry list maxvals)

partial_products :: [int]->[int]->[int]
partial_products a b = add_skew (cross_product a b)

|| cross_product:
|| given [A,B,C] [a,b,c]
|| calculate partial result
||        [ [Aa,Ab,Ac],
||        [Ba,Bb,Bc],
||        [Ca,Cb,Cc] ]
cross_product :: [int]->[int]->[[int]]
cross_product a b =
    map (ma_times b) a
    where
    ma_times l n = map (* n) l

|| add_skew: Return the sum of a potentially infinite number of mantissas,
|| offsetting the rows by one place:
|| given  [ [Aa,Ab,Ac],
||        [Ba,Bb,Bc],
||        [Ca,Cb,Cc] ]
|| return [ [Aa,Ab,Ac],
||       +   [Ba,Bb,Bc],
||       +      [Ca,Cb,Cc] ]
||          ================
||          [r1,r2,r3,r4,r5]

add_skew :: [[int]] -> [int]
add_skew ((a:x):y) = a : m_add x (add_skew y)
||add_skew ([]:y) = 0 : add_skew y  || never happens (?)

|| Sum of a single row is just that row.
add_skew [x] = x

|| We get handed an empty list when they multiply by bn_0.
add_skew [] = []

|| max_mul_vals gives the maximum possible value of a partial product term.
|| max_mul_vals = 0 : partial_products nines nines  where  nines = [9,9..]
|| Turns out to be equal to
max_mul_vals = [0, base_1 ^ 2 .. ]

|| reduce_for_carry takes a list of column sums and a list of the maximum
|| possible values of each column sum, and applies a one-digit carry until
|| the terms it returns are sure to be <= (base-1) * 2
|| The second list is always infinite.
|| In practice, repeated applications of carry1digit to max_mul_vals do this:
|| [0,81,162,243,324,405,486,567,648,729,810,891,972,1053,1134,1215,1296,1377..
|| [8,17,26,35,44,53,62,71,80,90,89,98,107,116,125,134,143,152,161,171,170,179..
|| [9,9,9,9,9,9,9,9,9,8,18,18,18,18,18,18,18,18,18,18,17,27,27,27,27,27,27,27..
|| [9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9..
|| and the ..9,9,9,8,18,18,18.. pattern always repeats itself further down the
|| list in successive iterations.
|| Thus we can tell when the current digit is gonna be ok by looking for when
|| the following digit's maximum possible value exceeds 9 (the 8,17 or 8,18
|| point); at this point a new invocation of carry1digit is necessary from the
|| "8" digit onwards.
|| Note! Although this reduces all terms in the 2nd parameter to the range
|| 0-base-1, it *does not* necessarily reduce all terms of the first parameter
|| to this range. For pathological cases like
||    bn_sqr (bn "999999999999999999999999999999999999999999"),
|| there is a "..9,9,9,9,9,9,10.." near the end that would require many
|| iterations of carry1digit to propagate the carry back to its proper place.
|| Only the final application of carry1 can resolve this (see m_mul above).
reduce_for_carry :: [int] -> [int] -> [int]
reduce_for_carry (a:b:x) (c:d:y)
    = a : reduce_for_carry (b:x) (d:y), if d < base
    = reduce_for_carry (carry1digit (a:b:x)) (carry1digit (c:d:y)), otherwise
||reduce_for_carry [a] y = [a]
||reduce_for_carry [] y = []
reduce_for_carry x y = x    || synthesis of the above two cases

|| carry1digit just performs the carry from each digit into the previous
|| one without progating further.
carry1digit (a:b:x) = (a + b $div base) : carry1digit (b $mod base : x)
||carry1digit [a] = [a]
||carry1digit [] = []
carry1digit x = x


|| Perform carry throughout number. Each term must be <= (base-1)^2.
|| This ensures that the maximum carry from any digit to the next will be
|| (base-2) because:
|| max_digit = (base - 1) * (base - 1) = (base^2 - 2 * base + 1)
|| max_carry = (max_digit + max_carry) $div base
|| If max_carry is (base - 2) then (max_digit + max_carry) $div base =
||    ( (base^2 - 2 * base + 1) + (base - 2) ) $div base =
||    ( (base^2 - 2 * base + (1 + (base - 2) ) $div base =
||    ( (base^2 - 2 * base + (base - 1) ) $div base =
||    ( (base - 2) + 0 ) = base - 2
|| Example of highest carry:
||    [0 81 81 81] -> [0 81 89 1] -> [0 89 9 1] -> [8 9 9 1]

m_carry2 :: [int] -> [int]
m_carry2 (a:b:c:x)

    || Max carry from b is (base - 2), so if b $mod base < 2, the carry
    || from x cannot affect a.  This turns out to be a win so rarely that
    || the extra checking slows us down more than we gain.
    || = (a + b $div base) : m_carry2 ((b $mod base) : c : x),
    ||    if b $mod base < 2    

    || Carry from x into c can only affect carry from c into b by 1,
    || so if (b + (c $div 10)) $mod 10 is less than 9,
    || x won't affect the carry from b into a.
    = (a + bc $div base) : m_carry2 (bc $mod base : c $mod base : x),
        if bc $mod base < base_1

    || general case
    = (a + carry) : m_carry2 ((b - carry * base) : c : x),
        otherwise
      where
      bc = b + c $div base
      carry = m_carry2from (b:c:x)

m_carry2 [a,b]
    = [a + b $div base, b $mod base], if b $mod base > 0
    = [a + b $div base], otherwise
m_carry2 [a]
    = [a], if a > 0
    = [], otherwise
m_carry2 [] = []

|| Return the carry from the list of numbers where each number <= (base-1)^2
|| Maximum carry from any digit into the previous one is (base-2).
|| To find the carry from A:B:C... in base 10:
|| If A $mod base < 2, carry = a $div base (carry from B cannot affect carry from A)
|| The carry from x into b can only affect the carry from b into a by one.
|| So if (a + (b $div 10)) $mod 10 is less than 9,
|| x won't affect the carry from a.

m_carry2from :: [int] -> int
m_carry2from (a:b:x)
|| This first case gains so infrequently that it ends up a net loss.
||    = a $div base            , if a $mod base < 2
    = ab $div base            , if ab $mod base < base_1
    = (a + m_carry2from(b:x)) $div base, otherwise
      where ab = a + (b $div base)
m_carry2from [a] = a $div base
m_carry2from [] = 0

|| ---------- end of long multiplication stuff ----------


||
|| bn_raise - simple power function by repeated multiplication
|| a is the bignum to raise to a power;
|| x is the integral power to raise it to.
|| Optimisation: bn_raise x (2*n) = sqr (bn_raise x n)
|| Unfortunately, bn_raise (bn_sqrt 2) 3 bottoms out (since it goes via 2)

bn_raise a x
    = error "Raising 0 to the 0th power is undefined", if x == 0 & bn_is_zero a
    = bn_0, if bn_is_zero a
    = bn_1, if x == 0
    = bn_div bn_1 (bn_raise' a (-x)), if x < 0
    = bn_raise' a x, otherwise

|| bn_raise' knows that a != 0 and x > 0
bn_raise' a x
    = bn_sqr (bn_raise' a (x $div 2)), if x $mod 2 == 0
    = bn_mul a (bn_raise' a (x-1)), if x > 2
    = a, if x == 1

|| bn_sqr - square a bignum, used frequently in the scientific functions,
|| hence optimised here to avoid pointlessly doing certain things twice.
|| was: bn_sqr x = x $bn_mul x
bn_sqr (as,ae,am)
    = bn_normalise (1, e, m)
      where (e,m) = ubn_sqr (ae,am)

ubn_sqr (ae,am)
    = (ae+ae, m_sqr am)

m_sqr :: [int]->[int]
m_sqr a = supercarry (0:(add_skew2 (square_product a))) max_mul_vals

|| square_product is like cross_product except that it folds the lower left
|| triangle of partial results onto the upper right triangle (since the
|| cross product of a single number is symmetrical about its main diagonal).
|| Given [a,b,c]
|| calculate partial result
||    [ [aa,2ab,2ac,..],
||      [bb,2bc,2bd,..],
||      [cc,2cd,2ce,..] ]
square_product :: [int]->[[int]]
square_product (a:x)
    = (a*a : map (* (a+a)) x) : square_product x
square_product [] = []

|| add_skew2: Return the sum of a potentially infinite number of mantissas,
|| offsetting the rows by two places:
|| given  [ [aa,2ab,2ac,..],
||        [bb,2bc,2bd,..],
||        [cc,2cd,2ce,..] ]
|| return [ [aa,2ab,2ac,..],
||       +        [bb,2bc,2bd,..],
||       +                [cc,2cd,2ce,..] ]
||          ================
||          [r1,r2,r3,r4,r5]

add_skew2 :: [[int]] -> [int]
add_skew2 ((a:b:x):y) = a : b : m_add x (add_skew2 y)

|| Sum of a single row is just that row.
add_skew2 [x] = x

|| This happens when squaring numbers with an odd number of digits
add_skew2 ([a]:y) = a : 0 : add_skew2 y

||add_skew2 ([]:y) = 0 : 0 : add_skew2 y  || never happens (?)

|| We get handed an empty list when they multiply by bn_0.
add_skew2 [] = []

||
|| bn_quot: Long division of a bignum by an integer.
|| Uses Miranda's marvellous infinitely-long integers to do all the hard work.
|| The time it takes is pretty much independent of the value of q.
||

bn_quot (s,e,m) q
    = error "bn_quot cannot divide by 0", if q == 0
    = bn_0, if m_is_zero m
    = (s,e,m),  if q == 1
    = (-s,e,m), if q == -1
    = bn_normalise (s, e, m_quot m q), if q > 1
    = bn_normalise (-s, e, m_quot m (-q)), if q < -1

m_quot m q
    = [], if m_is_zero m
    = (a $div q) : m_quot (shiftin (a $mod q:x)) q, otherwise
      where
      (a:x) = m
      shiftin (a:b:x) = (a * base + b) : x
      shiftin [a] = [a * base]

||
|| bn_div: Long division
||

bn_div (as,ae,am) (bs,be,bm)
    = bn_normalise (as*bs, ae-be+1, m_div am bm)

ubn_div (ae,am) (be,bm) = (ae-be+1, m_div am bm)

|| m_div takes two mantissae and divides the first by the second, returning a
|| mantissa.
|| div_loop requires divisor <= dividend
|| - zero divided by anything is 0.
|| - div_loop returns the first digit of the result and the remainder of the
||   dividend after the divisor has been subtracted from it "dig" times.
|| "dend" is the dividend; "sor" is the divisor.

m_div dend sor
    = [], if m_is_zero dend
    || The following line is logically ok but hits so rarely that
    || it slows things down more than it speeds them up.
    || = 0: m_div dend (0:sor),    if sor $m_gt dend
    = [dig],            if null dend2  || exact
    = dig : m_div (tl dend2) sor,    if hd dend2 == 0
    = dig : m_div dend2 (0:sor),    otherwise
      where
      (dig, dend2, sor2) = m_div_loop (0, dend, sor)

|| Work out how many times the divisor can be subtracted from the dividend
|| without the dividend going negative.
||
|| "hack" is our conservative first approximation for the first digit, formed
|| by dividing the first two digits of dend and sor.
|| The slow m_div_loop is then used to arrive at the exact result.
|| The >2-2-0-1 order is empirically the fastest using bn_pi as the test piece.
m_div_loop (0, dend, sor)
    = m_div_loop' (hack, dend $m_subcarry (m_times hack sor), sor)
        , if hack > 2
    = m_div_loop' (2, dend $m_subcarry (m_twice sor), sor)
        , if hack == 2
    = m_div_loop' (0, dend, sor)
        , if hack == 0
    = m_div_loop' (1, dend $m_subcarry sor, sor)
        , if hack == 1
      where
      || The "+ 1" makes sure we don't exceed in our estimate.
      hack = (take2 dend) $div (take2 sor + 1)

take2 (a:b:x) = a * base + b
take2 [a] = a * base
take2 [] = 0

until tst f x
    = case tst x of
        False -> until tst f (f x)
        True  -> x

|| This is the regular version, done by repeated subtraction.
|| Using m_subcarry2 here and above turns out to be >10% slower that m_subcarry
|| in this context, probably because the optimisation never comes into effect.

m_div_loop' x
    = until div_loop_done div_loop_body x
      where
      div_loop_done (dig, dend, sor) = dend $m_lt sor
      div_loop_body (dig, dend, sor)
        = (dig+1, m_subcarry dend sor, sor)


||
|| ---------------- Square root ---------------------
||

|| Square root adapted from Mr C. Woo's algorithm for abacus:
|| convert number in base 10 to base 100 by pairing digits,
|| then each application of sqrt_loop generates a digit of the result.
bn_sqrt (s,e,m)
    = error "bn_sqrt of negative number", if s ~= 1
    = bn_normalise (1, e $div 2, m_sqrt m)        , if e $mod 2 == 0
    = bn_normalise (1, (e+1) $div 2, m_sqrt (0:m))    , if e $mod 2 == 1

m_sqrt [] = []            || sqrt(0)
m_sqrt m = m_sqrt' (0, m_sqrbase m)

|| Convert mantissa in base b to mantissa in base b^2.
m_sqrbase (a:b:m) = (a * base + b) : m_sqrbase m
m_sqrbase [a] = [a * base]
m_sqrbase [] = []

||m_sqrt' :: (int,[int]) -> [int]
m_sqrt' (r1,(hd_o1:tl_o1))
    = []    , if hd_o1 == 0 & m_is_zero tl_o1
    = (r2 $mod base) : m_sqrt' (r2 * base, m_sqrt_shiftin (hd_o2:tl_o1))
        , otherwise
      where
      (r2,hd_o2) = m_sqrt_loop (r1,hd_o1)

|| Shift another term of the operand into the calculation.
m_sqrt_shiftin (a:b:x) = (a * base_2 + b) : x
m_sqrt_shiftin [a] = [a * base_2]

m_sqrt_loop :: (int,int) -> (int,int)
m_sqrt_loop ro
    = until sqrt_loop_done sqrt_loop_body ro
      where
      sqrt_loop_done (r, o) = 2*r >= o    || was: 2*r+1 > o
      sqrt_loop_body (r, o) = (r+1, o - (2*r+1))

||
|| ------------ Number generators ---------------------
||

||
|| bn_e: A minimally modified version of David Turner's edigits.
||
|| For commentary on how it works see miralib/ex/edigits.m in the
|| Miranda runtime system, or his paper "Recursion equations as a
|| Programming Language" in Functional Programming and its Applications,
|| pp 1-28, Cambridge University Press, January 1981.
||

bn_e = bn_make (1, 1, e_digits)

e_digits = (2: e_convert(repeat 1))

e_convert x
    = (hd x'):e_convert (tl x')
          where x' = e_norm 2 (0:map (base *) x)

e_norm c (d:e:x)
    = d + e $div c: e' $mod c : x', if e $mod c + 9 < c
    = d + e' $div c : e' $mod c : x', otherwise
      where
        (e':x') = e_norm (c+1) (e:x)

|| TEST VERSION OF BN_E
|| A better function may be the classic SUM (n <- 0..) (1 / n!)
|| Since running edigits for thousands of digits generates some enormous
|| Miranda integers which make the heap grow indefinitely.
|| The series converges fast enough for sum_series from 1/9! onwards.
|| (but it's not anything like as fast as the e_digits version, taking
|| nearly three times the reductions and over four times the CPU!)
    || Terms 0,1,2
bn_e2a = bn "2.5"
    || Terms 3..8
bn_e2b = foldr bn_add bn_0 (map (bn_1 $bn_quot) [6,24,120,720,5040,40320])
    || Terms 9..
bn_e2c = bn_sum_series (map (bn_1 $bn_quot) [ a | (a,n) <- (40320*9,10), (a*n,n+1) .. ])
    || e
bn_e2 = bn_e2a $bn_add bn_e2b $bn_add bn_e2c
    || Lazy man's version of terms 0-8
bn_e2ab = bn_make (1, 1, [2,7,1,8,2,7,8,7] ++ concat (repeat [6,9,8,4,1,2]))
bn_e3 = bn_e2ab $bn_add bn_e2c

||
|| ----------------- Logarithms and trigonometric functions -----------------
||
|| From libmath.b, part of the GNU "bc":
||
|| exp x = 1 + x + x^2/2! + x^3/3! ..., if x <= 1
||       = (exp (x/2))^2, otherwise
||
|| ln x = 2 * (a + a^3/3 + a^5/5 ...), if 0.5 < x < 2
||        where a = (x - 1) / (x + 1)
||      = 2 * ln (sqrt x), otherwise
||
|| sin x = x - x^3/3! + x^5/5! - x^7/7! ...
||
|| cos x = sin (x + pi/2) where pi/2 = (atan 1) * 2
||
|| atan x = x - x^3/3 + x^5/5 - x^7/7 ..., if x < c
||        = atan(c) + atan ( (x - c) / (1 + x * c) ), otherwise
||        where c = 0.2
||
|| http://www.shellandslate.com/computermath101.html says that:
|| cos(x) = 1 - x^2/2! + x^4/4! - ...
|| arcsin(x) = x + 1/2 (x^3)/3 + 1/2 3/4 (x^5)/5 + ...
||
|| http://www.math.wpi.edu/IQP/BVCalcHist/calc3.html says that Liebnitz' is:
|| arcsin(x) = x + x^3/6 + 3x^5/40 + 5x^7/112 + ...
||
|| http://mathworld.wolfram.com/InverseSine.html says that:
|| "The Maclaurin series for the inverse sine with -1<=x<=1 is given by
|| arcsin x = x + 1/6 x^3 + 3/40 x^5 + 5/112 x^7 + 35/1152 x^9 + ...
|| or
|| 1/2 SUM for n = 1 to infinity of (2x)^(2n) / n^2(2n C n)"
|| where a C b = (2a)! / ((a-b)! b!), ie  (4n)! / 2(n!)
|| and
|| "The Maclaurin series for the inverse cosine with -1<=x<=1 is
|| arccos x = PI/2 - x - 1/6 x^3 - 3/40 x^5 - 5/112 x^7 - ..."
||
|| There are also identities (from mathworld.wolfram.com) whereby
|| arcsin x = arctan (x / sqrt(1 - x^2)) and
|| arccos x = arctan (sqrt(1 - x^2) / x)

|| bn_exp
||
|| exp x = 1 + x + x^2/2! + x^3/3! ..., if x <= 1
||       = (exp (x/2))^2, otherwise
||
|| CONVERGANCE:
|| x <= 1, so we just need to be sure that the divisor gets multiplied by
|| at least 10 for each iteration.  This is true for the term x^10/10! onward.
|| If x <= 0.5 instead, it's true from the term x^5/5! onward.
|| For x <= 0.1, it's true for the whole series.
||
|| We use sum_series from the third term (x^2/2!) onwards.  For the 4th term
|| to be 1/10th of the 3rd one, x/3 must be <= 0.1
|| This is a shame because once the bottom reaches 10 * x, the series converges
|| ever faster.  A better strategy might be to sum the first N terms by hand
|| (where N is determined by the value of x) and then apply sum_series to the
|| rest, avoiding the call to bn_sqr.
||
|| An alternative strategy for negative values of x would be to reduce x
|| to -1 by recursion and then pair the terms before summing the series.
||
|| Optimise for integral powers by using fast bn_e ^ x

bn_exp x
    = bn_1, if bn_is_zero x
    = bn_1 $bn_div (bn_exp (bn_neg x)), if bn_is_neg x
    || = bn_e $bn_raise (bn2int x), if bn_is_integer x
    = bn_exp' x, otherwise 

|| bn_exp' knows that its argument is > 0
bn_exp' x
    = bn_sqr (bn_exp' (bn_half x)), if x $bn_gt (bn "0.3")
    = (bn_1 $bn_add x) $bn_add (bn_sum_series (map2 bn_div (exp_top x) exp_bot)), otherwise

|| exp_top x = [ x^2, x^3, x^4 .. ]
exp_top x = [ a | a <- bn_sqr x, bn_mul x a .. ]
|| exp_bot = [2!, 3!, 4! .. ]
exp_bot = drop 2 bn_factorials

factorial :: int -> int
factorial n = factorials ! n

|| Result cache for factorial function:
|| the list of all factorials: 0! 1! 2! ...
factorials :: [int]
factorials = [ a | (a,n) <- (1,1), (a*n,n+1) .. ]

|| bn_factorial, added since Miranda2 ints are only 64 bits
bn_factorial :: int -> bignum
bn_factorial n = bn_factorials ! n

bn_factorials :: [bignum]
bn_factorials = [a | (a, n) <- (bn_1, 1), (bn_times n a, n + 1) .. ]

|| bn_ln
||
|| From the math library of bc:
|| ln x = 2 * (a + a^3/3 + a^5/5 ...), if 0.5 < x < 2
||        where a = (x - 1) / (x + 1)
||      = 2 * ln (sqrt x), otherwise
||
|| CONVERGANCE:
|| 0.5 < x < 2, so (x-1)/(x+1) ranges from (-.5/1.5) to (1/3), ie -1/3 to +1/3.
|| Each term is slightly less than a^2 times the previous one...
|| unfortunately 1/9th is not quite 1/10th!
|| For the series to converge, we need
||    a^2 <= 1/10   which is to say   -1/sqrt(10) <= a <= 1/sqrt(10)
|| Lower limit:
||    (x-1)/(x+1) = -1/sqrt(10)
||    x-1 = -1/sqrt(10) * (x+1)
||    x-1 = (-1/sqrt(10))x + (-1/sqrt(10))
||    x (1 + 1/sqrt(10)) = 1 - 1/sqrt(10)
||    x = (1 - 1/sqrt(10)) / (1 + 1/sqrt(10))
|| Upper limit:
||    (x-1)/(x+1) = 1/sqrt(10)
||    x-1 = (1/sqrt(10)) x + 1/sqrt(10)
||    x (1 - 1/sqrt(10)) = 1 + 1/sqrt(10)
||    x = (1 + 1/sqrt(10)) / (1 - 1/sqrt(10))
||
|| one_root10 =  bn_1 $bn_div (bn_sqrt (bn "10"))
|| one_plus_root10 =  bn_1 $bn_add one_root10
|| one_minus_root10 =  bn_1 $bn_sub one_root10
|| lower_limit = one_minus_root10 $bn_div one_plus_root10
|| upper_limit = one_plus_root10 $bn_div one_minus_root10
||
|| Results:
||     .5194938532 (we use ...533 to be sure)
||    1.9249505911
||
|| Results:
||     .51949385329... (we use ...8533)
||    1.9249505911...
||
|| Fortunately, numbers very close to these limits calculate faster
|| than other numbers, since the resulting series converges faster.
|| We therefore keep these limits as wide as we can.
||
|| If we were to sum pairs of terms before calling sum_series, we would get a
|| convergance of order a^4 instead of a^2, which allows us to process a wider
|| range of values of x without having to recurse (.28013 - 3.56977). This
|| proves to be three or four times faster in cases where x has few digits and
|| is in the range .281-.519 or 1.925-3.569, because the time taken by bn_ln
|| depends on the number of digits in the operand more than anything else,
|| and a square root almost always gives an infinite number if digits.
|| In all other cases, this strategy turns out to be three or four times slower.

|| Limits within which the summation of the series will work
ln_lower_limit = bn ".5194938533"
ln_upper_limit = bn "1.9249505911"

bn_ln x
    = error "Can't take log of zero",
        if bn_is_zero x
    = error "Can't take log of negative numbers",
        if bn_is_neg x
    = bn_0,
        if x $bn_eq bn_1
    = bn_twice (bn_ln (bn_sqrt x)),
        if x $bn_lt ln_lower_limit \/ x $bn_gt ln_upper_limit
    = bn_ln' x,
        otherwise

|| Sum the series
bn_ln' x
    = bn_twice (bn_sum_series (map2 bn_quot (ln_top x) [1,3..]))

ln_top x
    = [n | n <- a, n $bn_mul asq ..]
      where
      asq = bn_sqr a
      a = (x $bn_sub bn_1) $bn_div (x $bn_add bn_1)

|| Now we can do a power function as we should.
|| Don't optimise integer cases because it makes things like
|| (sqrt 2) ^ 3  bottom out needlessly.
bn_pow a x = bn_exp (bn_ln a $bn_mul x)


|| bn_sin
||
|| sin x = x - x^3/3! + x^5/5! - x^7/7! ...
||
|| CONVERGANCE:
|| In each term, the top increases by a maximum factor of pi/2^2 (1.570796..)
|| and the bottom increases by 2*3 then 4*5 then 6*7, so the minimum
|| convergence factor is 1.57 / 6 = 0.1309.  For the series formed by summing
|| pairs of adjacent terms, it is always less than 0.1.
||
|| NOTE! Our value of bn_pi is calculated from the sum of a series,
|| so bn_sin will be at its fastest from -pi/2 to +pi/2
|| (Though you can't take the sin of exactly pi/2 because bn_gt in bn_sin'
|| goes off to infinity.)
||
|| bn_sum_series normalises the result for us.

|| A) reduce range of argument to first quadrant: 0..pi/2
||   1) reduce to positive numbers: sin(n) = -sin(-n)
bn_sin n = bn_0, if bn_is_zero n
     = bn_neg (bn_sin' (bn_neg n)), if bn_is_neg n
     = bn_sin' n, otherwise

||   2) reduce to 0..2*pi: for n > 2 * pi, sin(n) = sin(n - 2*pi)
||    Better: sin(n), n>2pi = sin(frac(n/2pi)*2pi)
bn_sin' n = bn_sin'' (bn_mul bn_2_pi (bn_frac (bn_div n bn_2_pi))), if n $bn_gt bn_2_pi
      = bn_sin'' n, otherwise

||   3) reduce to 0..pi: for pi < n <= 2*pi, sin(n) = -sin(n - pi)
bn_sin'' n = bn_neg (bn_sin''' (bn_sub n bn_pi)), if n $bn_gt bn_pi
       = bn_sin''' n, otherwise

||   4) reduce to 0..pi/2: for pi/2 < n <= pi, sin(n) = sin(pi - n)
bn_sin''' n = bn_sin'''' (bn_sub bn_pi n), if n $bn_gt bn_pi_2
        = bn_sin'''' n, otherwise

|| B) sum the series: sin x = x - x^3/3! + x^5/5! - x^7/7!
bn_sin'''' n = bn_sum_series (sin_series n)

|| -- Create the series for sin --

|| invert the sign of the even terms of the series
|| sin_top x = [ x, -x^3, x^5, -x^7 .. ]
|| NB: sin_top is re-used below in atan_series
sin_top x
    = [ a | a <- x, (bn_mul minus_x_squared) a .. ]
      where minus_x_squared = bn_neg (bn_mul x x)

|| The divisors of the terms of the series:
|| sin_bot = [ factorial n | n <- [1,3..] ]
sin_bot =
    everyother bn_factorials
    where
    everyother (a:b:x) = b : everyother x

sin_series x = addpairs (map2 bn_div (sin_top x) sin_bot)

|| Add pairs of terms in a series (also used in bn_atan, below)
addpairs :: [bignum] -> [bignum]
addpairs (a:b:x) = bn_add a b : addpairs x
addpairs [a] = [a]
addpairs [] = []

|| cosine - obvious enough, since we have sin: cos(x) = sin(x + pi/2)
|| We add special test for cos(0) since sin(pi/2) never returns.
||
|| http://www.shellandslate.com/computermath101.html says that:
|| cos(x) = 1 - x^2/2! + x^4/4! - ...

bn_cos x
    = bn_1, if bn_is_zero x        || avoid bottom-out case
    = bn_sin (x $bn_add bn_pi_2), otherwise


|| tangent

bn_tan x = (bn_sin x) $bn_div (bn_cos x)


|| arcsine
||
|| http://www.shellandslate.com/computermath101.html says that:
|| arcsin(x) = x + 1/2 (x^3)/3 + 1/2 3/4 (x^5)/5 + ...
|| and somewhere else says this is ok for -1 <= x <= 1
||
|| http://mathworld.wolfram.com/InverseSine.html says that:
|| "The Maclaurin series for the inverse sine with -1<=x<=1 is given by
|| arcsin x = x + 1/6 x^3 + 3/40 x^5 + 5/112 x^7 + 35/1152 x^9 + ...
|| or
|| 1/2 SUM for n = 1 to infinity of (2x)^(2n) / n^2(2n C n)"
|| where a C b = (2a)! / ((a-b)! b!), ie  (4n)! / 2(n!)
||
|| Convergence:
|| arcsin x = x + 1/6 x^3 + 3/40 x^5 + 5/112 x^7 + 35/1152 x^9 + ...

bn_asin x = bn_atan (x $bn_div (bn_sqrt (bn_1 $bn_sub (bn_sqr x))))


|| arccosine
||
|| http://mathworld.wolfram.com/InverseSine.html says that:
|| "The Maclaurin series for the inverse cosine with -1<=x<=1 is
|| arccos x = PI/2 - x - 1/6 x^3 - 3/40 x^5 - 5/112 x^7 - ..."

bn_acos x = bn_atan ((bn_sqrt (bn_1 $bn_sub (bn_sqr x))) $bn_div x)


|| arctangent
||
|| atan x = x - x^3/3 + x^5/5 - x^7/7 ..., if x < c
||        = atan(c) + atan ( (x - c) / (1 + x * c) ), otherwise
||        where c = 0.2
||
|| By inspection with a test function, the series converges faster than
|| one digit per term (paired terms, that is) with c = 0.5; even faster
|| with c = 0.2 (which is the value used inside "GNU bc" whence we robbed the
|| algorithm).  Since, for us, the cost of doing the mul - div etc involved
|| in recursing is higher than that of summing the series, we use 0.5.
|| Experimentally (with bc), .562341325 converges ok (by a factor of >10)
|| while .56241326 doesn't quite.

c_lim = bn ".562341325"

|| bn_atan2 takes this two steps further: It lets you specify c at runtime,
|| and applies two recursive calls in one go if appropriate.  This has three
|| advantages: it saves a humungous amount of multiplication and division,
|| lets us calculate atan(c) once instead of twice and avoids indefinite
|| recursion in intermediate results in many cases when the top and bottom
|| of the division attain unfortunately-related infinitely-long values.
|| In practice, with c = 0.5:
|| 0 <= x <= .5        atan(x)
|| .5 < x < 1.3XXX    atan(.5) + atan(d)
|| 1.3XXX < x < 5.5    atan(.5) + atan(.5) + atan(e)
|| 5.5 <= x        atan(.5) + atan(.5) + atan(.5) + atan(f)
||
|| 0.5 seemed a good general purpose value for c - other values run slower
|| except for some special cases - until I found out that it never gives an
|| answer to atan(7), atan(9), atan(12), atan(15) and probably others too.
||
|| In the case of atan(1), used for pi, c=0.4 runs staggeringly faster than 0.5:
|| atan2 1 0.5 => atan 0.5 + atan 0.33333333333..
|| atan2 1 0.4 => (2 * atan 0.4) + atan 0.02439.. which converges much faster
|| (requiring 345,003 reductions instead of 1,907,882 for 20 decimal places).
|| Your mileage may vary taking the arctan of other specific values.

bn_atan2 :: bignum -> bignum -> bignum
bn_atan2 x c
    = error "Internal error: bn_atan2 limit check", if c $bn_gt c_lim
    = bn_0, if bn_is_zero x
    = bn_neg (bn_atan2' (bn_neg x) c), if bn_is_neg x
    = bn_atan2' x c, otherwise

|| bn_atan
|| Specialised version of bn_atan2, using constant c_use for value of c
|| giving speedup for the constant calculations that drop out.
|| The speedup is large (more than twice) when several atans are performed
|| in one computation since atan(c) and derived calculation are only
|| done once.

|| For the generic constant-c version, the value of c to use.
|| No single digit version of c works for small positive integers:
|| they all (.2, .3, .4, .5) go into infinite regress in the summation
|| of the series, so we use a 2-digit value.
||
|| Experimentally, using values from .41 to .56 for c_use
|| .45 and .48 bottom out in atan(6)
|| .56 bottoms out on atan(37)
|| .49 was the fastest in tests from 1 to 101

c_use = bn ".49"

bn_atan x
    = bn_0, if bn_is_zero x
    = bn_neg (bn_atan' (bn_neg x)), if bn_is_neg x
    = bn_atan' x, otherwise

|| atan2 x c
||    = atan x            , if x <= c
||    = atan c + atan d         , if x > c & d <= c
||    = atan c + atan c + atan2 e c    , otherwise
||      where
||      d = (x - c) / (1 + x * c)
||      e = (d - c) / (1 + d * c)
||
||  e = (x - 2c - x c^2) / (1 + 2xc - c^2)
||  or  (x(1-c^2) - 2c) / (1 + 2xc - c^2)
||
|| bn_atan2' knows that its parameter x is not negative.

bn_atan2' :: bignum -> bignum -> bignum
bn_atan2' x c
    = bn_atan'' x, if x $bn_le c
    = bn_atan'' c $bn_add (bn_atan2' d c) , if d $bn_le c
    = bn_twice (bn_atan'' c) $bn_add (bn_atan2' e c), otherwise
      where
        d = (x $bn_sub c) $bn_div (bn_1 $bn_add x_mul_c)
      e = etop $bn_div ebot
      etop = (x $bn_mul (bn_1 $bn_sub csq)) $bn_sub (bn_twice c)
      ebot = bn_1 $bn_add (bn_twice x_mul_c) $bn_sub csq
      csq = bn_sqr c
      x_mul_c = x $bn_mul c

|| Now that we have arctangent, we can calculate pi as 4 * atan(1)
||
|| "Now I, even I would celebrate
||  In rhymes unapt the great
||  Immortal Syracusan, rivaled nevermore
||  Who, in his wondrous lore
||  Passed on before
||  Left men his guidance
||  How to circles mensurate."
|| bn_pi = bn "3.141592653589793238462643383279" :-)
bn_pi30 = bn "3.141592653589793238462643383279"

bn_2_pi = bn_twice bn_pi        || 2 * pi  (used in bn_sin)
bn_pi = bn_twice bn_pi_2        || pi
bn_pi_2 = bn_twice bn_pi_4        || pi / 2  (used in bn_sin)
|| bn_pi_4 = bn_atan2 bn_1 (bn "0.4")    || pi / 4

|| for pi/4, calculated with atan(1) using c = 0.4,
|| we expand the first iteration of the atan series with constant values:
|| d = (1 - .4) / (1 + .4) = .6/1.4 = 3/7 = 0.428571..., which is not < .4
|| so...
|| etop = (1 * (1 - .4^2)) - (2 * .4) = 1-.16 - .8 = 0.04
|| ebot = 1 + (2 * 1 * .4) - (.4^2) = 1.8 - .16 = 1.64
|| e = 0.04 / 1.64 = 4 / 164 = 2 / 82 = 1 / 41
|| pi/4 = (2 * atan(0.4)) + atan(1/41)
|| this is 4.5% better in space and speed than the bn_pi_4 above.

bn_pi_4 = bn_twice (bn_atan'' (bn ".4")) $bn_add (bn_atan'' (bn_quot bn_1 41))


|| Optimised version using constant c_use as the value of c
bn_atan' :: bignum -> bignum
bn_atan' x
    = bn_atan'' x, if x $bn_le c_use
    = atan_c $bn_add (bn_atan' d) , if d $bn_le c_use
    = twice_atan_c $bn_add (bn_atan' e), otherwise
      where
        d = (x $bn_sub c_use) $bn_div (bn_1 $bn_add x_mul_c)
      e = etop $bn_div ebot
      etop = (x $bn_mul one_sub_sqr_c) $bn_sub twice_c
      ebot = one_sub_sqr_c $bn_add (bn_twice x_mul_c)
      x_mul_c = x $bn_mul c_use

|| Miranda global constant functions seem to fare better than derived constant
|| calculations in where clauses.
atan_c = bn_atan'' c_use
twice_atan_c = bn_twice atan_c
sqr_c = bn_sqr c_use
twice_c = bn_twice c_use
one_sub_sqr_c = bn_1 $bn_sub sqr_c

|| START OF WORK IN PROGRESS ON OPTIMISED ATAN WITH FIXED C=0.5

|| This works and is 3% faster in space and speed than bn_atan,
|| but bottoms out on certain values (7, 9, 12, 15 ...)
|| probably because unfortunate pairs occur in the summing series:
|| infinitely-long terms that, when subtracted, would give a
|| finite-length sum.

bn_atan5 x
    = bn_0, if bn_is_zero x
    = bn_neg (bn_atan5' (bn_neg x)), if bn_is_neg x
    = bn_atan5' x, otherwise

|| Even more specialised version uses constant 0.5 for value of c
|| This lets us use bn_times, bn_quot, bn_half instead of mul and div
|| atan5 x
||    = atan x            , if x <= .5
||    = atan .5 + atan d         , if x > .5 & d <= .5
||    = atan .5 + atan .5 + atan5 e     , otherwise
||      where
||      d = (x - .5) / (1 + x * .5) = (x - .5) / (1 + x/2)
||      e = (d - .5) / (1 + d * .5)
||
||  e = (x - 2*.5 - x .5^2) / (1 + 2x*.5 - .5^2)
||    = (x(1-.5^2) - 2*.5) / (1 + x - .25)
||    = (.75*x - 1) / (.75 + x)
bn_atan5' :: bignum -> bignum
bn_atan5' x
    = bn_atan'' x, if x $bn_le bn_p5
    = atan_p5 $bn_add (bn_atan5' d) , if d $bn_le bn_p5
    = twice_atan_p5 $bn_add (bn_atan5' e), otherwise
      where
        d = (x $bn_sub bn_p5) $bn_div (bn_1 $bn_add (bn_half x))
      e = (bn_quot (bn_times 3 x) 4 $bn_sub bn_1) $bn_div (bn_p75 $bn_add x)

bn_p5 = bn(".5")
bn_p75 = bn(".75")
atan_p5 = bn_atan'' bn_p5
twice_atan_p5 = bn_twice atan_p5

|| END OF WORK IN PROGRESS ON OPTIMISED ATAN

|| This version always sums the series.  x must be <= c_lim
|| so that the series converges ok.
bn_atan'' x = bn_sum_series (atan_series x)

|| The terms of the series for atan(x).
|| The dividends are the same as for sin(), so we reuse them.
atan_series x = addpairs (map2 bn_quot (sin_top x) [1,3..])

|| Derived functions
|| Copied from the Hugs98 prelude, except for asin and acos from
|| http://mathworld.wolfram.com


|| Hyperbolic functions

bn_sinh x = bn_half ( bn_exp x $bn_sub (bn_exp (bn_neg x)) )
bn_cosh x = bn_half ( bn_exp x $bn_add (bn_exp (bn_neg x)) )
bn_tanh x = (bn_sinh x) $bn_div (bn_cosh x)
bn_asinh x = bn_ln ( x $bn_add (bn_sqrt (bn_sqr x $bn_add bn_1)) )
bn_acosh x = bn_ln ( x $bn_add (bn_sqrt (bn_sqr x $bn_sub bn_1)) )
bn_atanh x = bn_half ( bn_ln (bn_add bn_1 x) $bn_sub (bn_ln (bn_sub bn_1 x)) )


|| bn_sum_series
||
|| This is the low-level function that does all the hard work for the
|| scientific functions above.
||
|| bn_sum_series sums a converging series of bignum.  It requires that every
|| term in the series be less that 1/10th (ok, 1/baseth) of the previous one
|| and that all elements in the series be of the same sign (all +ve or all -ve).
|| ubn2bn normalises the result for us.
|| This copes with series in which the terms are all positive or all negative.
|| The check for bn_is_zero (hd s) is necessary because if the first element
|| is zero, we can't tell whether the elements are all positive or all negative.
|| If we are passed a series of mixed positive and negative terms by mistake,
|| the calculation will throw a fatal error in bn2ubn.
|| Every term in the series has already been normalised.
bn_sum_series :: [bignum] -> bignum
    || first, eliminate two simple cases
    || (These never happen because we are always handed infinite lists)
|| bn_sum_series [] = bn_0
|| bn_sum_series [a] = a
    || now reduce the job to summing a positive series
bn_sum_series s
    = bn_sum_series (tl s), if bn_is_zero (hd s)
    = bn_neg (bn_sum_series (map bn_neg s)), if bn_is_neg (hd s)
    = ubn2bn (ubn_sum_series (map bn2ubn s)), otherwise

|| unsigned version - tack a leading zero onto all terms so that we can be
|| sure that the sum of any term and all the following ones will have the
|| same exponent as the term itself.  The fact that the first digit of each
|| term will now be 0 is taken account of in the unequal-exponent lazy
|| lookahead code, so is not as much of a lossage as it might seem.
|| Without it, we produce "10" digits from time to time.
|| However it is much faster (2/3 of the reductions). Can we use this
|| by letting it produce 10 digits and then applying m_carry1?
ubn_sum_series :: [ubignum] -> ubignum
ubn_sum_series s
    = ubn_sum_series2 (map prefix_0 s)
      where
      prefix_0 (e,m) = (e+1, 0:m)

|| ubn_sum_series2 is the second version of the series summer that uses the same
|| kind of logic as ubn_add2 to return the initial digit from the first term
|| through inspection only of the first term and the exponent of the second.
||
|| When we can no longer be sure that the digits of the result will be the
|| same as the digits of the first term, we add the first and second terms
|| and repeat the process with the sum as the first term of the list.
||
|| We know that zeroes have been prefixed to all terms, guaranteeing that the
|| exponent of the sum from a particular term onwards will not grow as a result
|| of adding in the following terms, and that the maximum addition from the
|| first digit of the second term and all that follow it will be 1.  This
|| corresponds to the logic of seeing whether there are any non-9 digits in
|| ubn_add2.

ubn_sum_series2 :: [ubignum] -> ubignum
      || From now on we don't normalise or adjust exponents, so return
      || the exponent and work out the mantissa.
ubn_sum_series2 ((ae,am):x) = (ae, ubn_sum_series2' ((ae,am):x))
ubn_sum_series2 [] = ubn_0

|| ubn_sum_series2' just returns the mantissa of the sum, which has the same
|| exponent as the first term of the series.
ubn_sum_series2' :: [ubignum] -> mantissa
|| Most common case first...
ubn_sum_series2' ((ae,(a:ax)):(be,bm):x)
      || We can be sure of the first digit of a if ae-be >= 2 and there is a
      || digit between the first digit of a and the digit above the first of
      || b whose value is less that (base-1).  Remember that the first digit
      || of b will be 0, and so the most that the sum of b onwards could add
      || to the final result will be a 1 in the first position of b.
      || Note: the any_lt test *includes* the first digit of a that overlaps b.
      || I think this is right...  Here's the worst case:
      || a: 0 8 9 9 9 9 9 9 9 9 9 9
      || b:   0 9 9 9 9 9 9 9 9 9
      || c:     0 9 9 9 9 9 9 9 9
      || b+c: 1 0 9 9 9 9 9 9 9 9 
      || abc: 9 
      || .... so given the 8, the first digit cannot be affected by adding in
      || b,c...  ok! In fact, this is worse than the worst case because
      || .0099 is not < 1/10th of .0899
    = a : ( ubn_sum_series2' ((ae-1,ax):(be,bm):x) ), if ae-be >= 2 & any_lt_base_1 ax (ae-be)
      || Otherwise add the first two terms of the series together and repeat the
      || process with this as the new first term of the series, which will give
      || us more distance between the exponents.
    = ubn_sum_series2' ((ae, (m_addcarry (a:ax) (prefix0s (ae-be) bm))) : x), otherwise
|| If am is exhausted, eliminate the term and proceed with the rest
ubn_sum_series2' ((ae,[]):(be,bm):x)
    = prefix0s (ae-be) (ubn_sum_series2' ((be,bm):x))
|| Deal with terminal cases
ubn_sum_series2' [(e,m)] = m        || One item
ubn_sum_series2' [] = []        || No items

|| END OF WORK IN PROGRESS ON SERIES SUMMER

||
|| === End of bignum.m ===
||
