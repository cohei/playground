-- http://neilmitchell.blogspot.com/2017/05/proving-stuff-with-idris.html

%default total

append : List a -> List a -> List a
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

proof_append_nil : (xs : List a) -> append xs [] = xs
proof_append_nil [] = Refl
-- proof_append_nil (x :: xs) = rewrite proof_append_nil xs in Refl
proof_append_nil (x :: xs) = cong (x ::) (proof_append_nil xs)

proof_append_associativity : (xs, ys, zs : List a) -> append xs (append ys zs) = append (append xs ys) zs
proof_append_associativity [] ys zs = Refl
proof_append_associativity (x :: xs) ys zs = rewrite proof_append_associativity xs ys zs in Refl

reverse1 : List a -> List a
reverse1 [] = []
reverse1 (x :: xs) = reverse1 xs `append` [x]

proof_reverse1_append : (xs, ys : List a) -> reverse1 (append xs ys) = append (reverse1 ys) (reverse1 xs)
proof_reverse1_append [] ys = rewrite proof_append_nil (reverse1 ys) in Refl
proof_reverse1_append (x :: xs) ys =
  rewrite proof_append_associativity (reverse1 ys) (reverse1 xs) [x] in
  rewrite proof_reverse1_append xs ys in
  Refl

proof_reverse1_reverse1 : (xs : List a) -> xs = reverse1 (reverse1 xs)
proof_reverse1_reverse1 [] = Refl
proof_reverse1_reverse1 (x :: xs) =
  rewrite proof_reverse1_append (reverse1 xs) [x] in
  rewrite sym (proof_reverse1_reverse1 xs) in
  Refl

rev2 : List a -> List a -> List a
rev2 acc [] = acc
rev2 acc (x :: xs) = rev2 (x :: acc) xs

-- rev2 xs ys = reverse ys ++ xs
proof_rev2_reverse1 : (xs, ys : List a) -> rev2 xs ys = append (reverse1 ys) xs
proof_rev2_reverse1 xs [] = Refl
proof_rev2_reverse1 xs (y :: ys) =
  rewrite sym (proof_append_associativity (reverse1 ys) [y] xs) in
  rewrite proof_rev2_reverse1 (y :: xs) ys in
  Refl

reverse2 : List a -> List a
reverse2 = rev2 []

-- rev2 [] (rev2 [x] xs)
-- reverse (rev2 [x] xs) ++ []
-- reverse (reverse xs ++ [x]) ++ []
-- reverse [x] ++ reverse (reverse xs) ++ []
-- reverse [x] ++ xs ++ []
-- reverse [x] ++ xs
-- [x] ++ xs
-- x :: xs
proof_reverse2_reverse2 : (xs : List a) -> xs = reverse2 (reverse2 xs)
proof_reverse2_reverse2 [] = Refl
proof_reverse2_reverse2 (x :: xs) =
  rewrite proof_rev2_reverse1 [] (rev2 [x] xs) in
  rewrite proof_append_nil (reverse1 (rev2 [x] xs)) in
  rewrite proof_rev2_reverse1 [x] xs in
  rewrite proof_reverse1_append (reverse1 xs) [x] in
  rewrite sym (proof_reverse1_reverse1 xs) in
  Refl

proof_reverse1_reverse2 : (xs : List a) -> reverse1 xs = reverse2 xs
proof_reverse1_reverse2 [] = Refl
proof_reverse1_reverse2 (x :: xs) =
  rewrite proof_rev2_reverse1 [x] xs in
  Refl
