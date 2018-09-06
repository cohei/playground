Axiom G:Set.
Axiom G_dec : forall a b:G, {a=b} + {a<>b}.
Axiom mult:G -> G -> G.
Notation "a * b" := (mult a b).
Axiom assoc : forall a b c:G, (a * b) * c = a * (b * c).
Axiom G1:G.
Notation "1" := G1.
Axiom id_l : forall a:G, 1 * a = a.
Axiom inv : G -> G.
Axiom inv_l : forall a:G, (inv a) * a = 1.

Theorem inv_r : forall a:G, a * (inv a) = 1.
intros.
replace (a * inv a) with (1 * a * inv a).
replace (1 * a * inv a) with ((inv (inv a) * inv a) * a * inv a).
replace (inv (inv a) * inv a * a * inv a) with (inv (inv a) * (inv a * a) * inv a).
rewrite inv_l.
rewrite assoc.
rewrite id_l.
rewrite inv_l.
reflexivity.
rewrite <- assoc.
reflexivity.
rewrite inv_l.
reflexivity.
rewrite id_l.
reflexivity.
Qed.
