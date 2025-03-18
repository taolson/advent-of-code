|| variant of <v3> which uses the bignum module

%export +

%import <lens>
%import <maybe>
%import <mirandaExtensions>
%import "bignum"


v3 * ::= V3 * * *

|| lenses for v3
v3_x, v3_y, v3_z :: lens (v3 *) *
v3_x = Lens getf overf where getf (V3 x y z) = x; overf fn (V3 x y z) = V3 (fn x) y z
v3_y = Lens getf overf where getf (V3 x y z) = y; overf fn (V3 x y z) = V3 x (fn y) z
v3_z = Lens getf overf where getf (V3 x y z) = z; overf fn (V3 x y z) = V3 x y (fn z)


|| monad interface

v3_bind :: v3 * -> (* -> v3 **) -> v3 **
v3_bind (V3 a b c) f
    = V3 a' b' c'
      where
        V3 a' _  _  = f a
        V3 _  b' _  = f b
        V3 _  _  c' = f c

v3_pure :: * -> v3 *
v3_pure x = V3 x x x


|| functor interface
v3_fmap :: (* -> **) -> v3 * -> v3 **
v3_fmap f (V3 a b c) = V3 (f a) (f b) (f c)


|| applicative interface
v3_apply :: v3 (* -> **) -> v3 * -> v3 **
v3_apply (V3 fa fb fc) (V3 a b c) = V3 (fa a) (fb b) (fc c)

v3_liftA2 :: (* -> ** -> ***) -> v3 * -> v3 ** -> v3 ***
v3_liftA2 f (V3 a b c) (V3 g h i) = V3 (f a g) (f b h) (f c i)


|| foldable interface
v3_foldr :: (** -> * -> *) -> * -> v3 ** -> *
v3_foldr f z (V3 a b c) = f c z |> f b |> f a

v3_foldl :: (* -> ** -> *) -> * -> v3 ** -> *
v3_foldl f z (V3 a b c) = f z a |> ($f b) |> ($f c)

v3_abs, v3_neg :: v3 bignum -> v3 bignum
v3_abs    a = v3_fmap bn_abs a
v3_neg    a = v3_fmap bn_neg a

v3_add, v3_sub, v3_mul, v3_div :: v3 bignum -> v3 bignum -> v3 bignum
v3_add a b = v3_liftA2 bn_add a b
v3_sub a b = v3_liftA2 bn_sub a b
v3_mul a b = v3_liftA2 bn_mul a b
v3_div a b = v3_liftA2 bn_div a b

v3_max, v3_min :: v3 bignum -> v3 bignum -> v3 bignum
v3_max a b = v3_liftA2 (max2 cmp) a b where cmp a b = cmpint (bn_cmp a b) 0
v3_min a b = v3_liftA2 (min2 cmp) a b where cmp a b = cmpint (bn_cmp a b) 0

v3_sum, v3_product :: v3 bignum -> bignum
v3_sum     a = v3_foldl bn_add bn_0 a
v3_product a = v3_foldl bn_mul bn_1 a

|| geometric distance
v3_dist :: v3 bignum -> v3 bignum -> bignum
v3_dist a b = bn_sqrt . v3_sum $ v3_mul d d where d = v3_sub a b

|| cross product
v3_cross :: v3 bignum -> v3 bignum -> v3 bignum
v3_cross (V3 a b c) (V3 x y z)
    = V3 d e f
      where
        d = (b $bn_mul z) $bn_sub (c $bn_mul y)
        e = (c $bn_mul x) $bn_sub (a $bn_mul z)
        f = (a $bn_mul y) $bn_sub (b $bn_mul x)

|| dot product
v3_dot :: v3 bignum -> v3 bignum -> bignum
v3_dot a b = v3_sum $ v3_mul a b

|| testing
v3_any :: (* -> bool) -> v3 * -> bool
v3_any p
    = v3_foldr tst False
      where
        tst x b = b \/ p x

