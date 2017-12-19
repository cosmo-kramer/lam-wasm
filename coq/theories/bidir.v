From stdpp Require Import base stringmap.

Inductive type :=
| Nat
| Arrow (τ τ' : type)
| Un.

Inductive expr :=
| Var (x : string)
| Lam (x : string) (e : expr)
| App (e e' : expr)
| Lit (n : nat)
| Asc (e : expr) (τ : type).

Notation context := (stringmap type) (only parsing).

Reserved Notation "Γ ⊢ e ⇐ τ"
  (at level 99, no associativity, format "'[' Γ  ⊢  '/' e  ⇐  τ ']'").
Reserved Notation "Γ ⊢ e ⇒ τ"
  (at level 99, no associativity, format "'[' Γ  ⊢  '/' e  ⇒  τ ']'").

Inductive check : context → expr → type → Prop :=
| Csub Γ e τ : Γ ⊢ e ⇒ τ → Γ ⊢ e ⇐ τ

| Clit Γ n : Γ ⊢ (Lit n) ⇐ Nat
| Clitu Γ n : Γ ⊢ (Lit n) ⇐ Un

| Clam Γ x e τ1 τ2 : <[x:=τ1]>Γ ⊢ e ⇐ τ2 → Γ ⊢ Lam x e ⇐ Arrow τ1 τ2
| Clamu Γ x e : <[x:=Un]>Γ ⊢ e ⇐ Un → Γ ⊢ Lam x e ⇐ Un


with synth : context → expr → type → Prop :=
| Svar Γ x τ : Γ !! x = Some τ → Γ ⊢ (Var x) ⇒ τ
| Sasc Γ e τ : Γ ⊢ e ⇐ τ → Γ ⊢ (Asc e τ) ⇒ τ

| Sapp Γ e1 e2 τ1 τ2 : Γ ⊢ e1 ⇒ Arrow τ1 τ2 → Γ ⊢ e2 ⇐ τ1 → Γ ⊢ App e1 e2 ⇒ τ2
| Sappu Γ e1 e2 : Γ ⊢ e1 ⇒ Un → Γ ⊢ e2 ⇐ Un → Γ ⊢ App e1 e2 ⇒ Un

where "Γ ⊢ e ⇐ τ" := (check Γ e τ)
  and "Γ ⊢ e ⇒ τ" := (synth Γ e τ).
