(* Functions and key bindings (Learn more keys with `C-c C-h' or `C-x ? m')

C-c C-n 進む
C-c C-u 戻る
C-c C-BS 戻って削除
C-c C-return カーソルまで評価
C-c C-b バッファ全体を評価

C-c C-v ミニバッファでコマンドを実行 .はいらない
C-c C-a <C-return>	ミニバッファでコマンド(補完つき)を書いて挿入

C-c TAB カーソル下の語をCheck = C-c C-a C-c
C-c C-f 定理を検索(型で？)

コマンド
C-c C-a C-b	About 型や出所を表示
C-c C-a C-c	Check 定理の型を表示
C-c C-a C-p	Print 定義を表示
C-c C-a C-a	SearchAbout 定理の名前から検索
C-c C-a C-o	SearchPattern (_ + _ = _)のような情報から定理を検索
C-c C-a C-s	Show

Inspect n. 最近のn個の定理を表示
Eval なんちゃら in ( ... ). なんちゃら戦略で評価 cbv 値呼び compute とことん
Search SearchAboutとの違いがわからない
SearchRewrite (_ + _). 右辺か左辺に指定したパターンをもつものを検索
Locate これも検索 たしか""で中置演算子も検索できる

使い方がわからないコマンド
C-c C-a C-l	coq-LocateConstant
C-c C-a C-n	coq-LocateNotation
C-c C-a C-S-h	coq-PrintHint

C-c C-a TAB	coq-insert-intros
C-c C-a RET	coq-insert-match
C-c C-a C-r	proof-store-response-win
C-c C-a C-t	coq-insert-tactic
C-c C-a !	coq-insert-solve-tactic
C-c C-a C-S-g	proof-store-goals-win
C-c C-a C-S-t	coq-insert-tactical
C-c C-a C-SPC	coq-insert-term
C-c C-a C-(	coq-insert-section-or-module
C-c C-a C-)	coq-end-Section
*)

Require Import List.
Require Import Arith.

Fixpoint insert (a:nat) (l:list nat) : list nat :=
  match l with
    | nil => a :: nil
    | x :: xs => if leb a x then a :: l else x :: insert a xs
  end.

Fixpoint insertion_sort (l : list nat) : list nat :=
  match l with
    | nil => nil
    | x :: xs => insert x (insertion_sort xs)
  end.

Require Import Sorting.Permutation.
Require Import Sorting.Sorted.

Lemma insert_perm : forall (x:nat) (l:list nat), Permutation (x::l) (insert x l).
Proof.
induction l.
simpl.
apply Permutation_refl.
simpl.
destruct (leb x a).
apply Permutation_refl.
apply perm_trans with (a::x::l).
apply perm_swap.
apply perm_skip.
assumption.
Qed.

Theorem isort_permutation :
  forall (l:list nat), Permutation l (insertion_sort l).
Proof.
induction l.
simpl.
apply perm_nil.
simpl.
apply perm_trans with (a::insertion_sort l).
apply perm_skip.
assumption.
apply insert_perm.
Qed.

Lemma insert_sorted : forall (a:nat) (l:list nat),
                      LocallySorted le l -> LocallySorted le (insert a l).
induction l.
intros.
constructor.
intros.
simpl.
remember (leb a a0).
destruct b.
apply LSorted_consn.
assumption.
apply leb_complete.
congruence.
inversion H.
simpl.
apply LSorted_consn.
apply LSorted_cons1.
apply lt_le_weak.
apply leb_complete_conv.
congruence.
subst.
simpl.
simpl in IHl.
remember (leb a b).
destruct b0.
apply LSorted_consn.
apply IHl.
assumption.
apply lt_le_weak.
apply leb_complete_conv.
congruence.
apply LSorted_consn.
apply IHl.
apply H2.
apply H3.
Qed.

Theorem isort_sorted : forall (l:list nat), LocallySorted le (insertion_sort l).
induction l.
constructor.
simpl.
apply insert_sorted.
assumption.
Qed.

Extract Inductive  list => "list" ["[]" "(::)"].
Extraction map.

Extraction Language Haskell.
Extract Inductive list => "([])" ["[]" "(:)"].
Extract Inductive bool => "Bool" ["True" "False"].
Extract Inductive nat => Int ["0" "succ"] "(\fO fS n -> if n == 0 then fO () else fS (n-1))".
Extraction map.

Extraction Language Scheme.
Extraction map.

Variable dne : forall P : Prop, ~~P -> P.
Implicit Arguments dne [P].
Variable P : Prop.

Definition em : P \/ ~P.
apply dne.
intro.
apply H.
right.
intro.
apply H.
left.
assumption.
Defined.

Variable Q:Prop.
Definition peirce : ((P -> Q) -> P) -> P.
intro.
apply dne.
intro.
apply H0.
apply H.
intro.
contradiction.
Defined.
