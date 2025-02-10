module LambdaCalculus where

open import Data.String using (String; _≟_)
open import Data.Empty using (⊥)
open import Data.Product using (_×_; ∃; _,_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; _≢_)
open import Relation.Nullary using (yes; no)
open import Data.List using (List; []; _∷_)
open import Function.Base using (_∘_)

-- Definição da sintaxe do lambda calculus
data Expr : Set where
  Var : String → Expr
  Lam : String → Expr → Expr
  App : Expr → Expr → Expr

-- Definição de substituição livre de variáveis
data Subst : Expr → String → Expr → Expr → Set where
  
  -- Regra para variável igual
  var-eq : ∀ {x e}
    → Subst (Var x) x e e

  -- Regra para variável diferente
  var-neq : ∀ {x y e}
    → x ≢ y
    → Subst (Var y) x e (Var y)

  -- Regra para lambda com variável diferente
  lam : ∀ {x y e e′ e″}
    → x ≢ y
    → Subst e x e′ e″
    → Subst (Lam y e) x e′ (Lam y e″)

  -- Regra para aplicação
  app : ∀ {x e₁ e₂ e e₁′ e₂′}
    → Subst e₁ x e e₁′
    → Subst e₂ x e e₂′
    → Subst (App e₁ e₂) x e (App e₁′ e₂′)

-- Relação de redução beta
data _→β_ : Expr → Expr → Set where
  
  -- Regra (λx.e₁)e₂ →β e₁[x := e₂]
  β-red : ∀ {x e₁ e₂ e₃}
    → Subst e₁ x e₂ e₃
    → App (Lam x e₁) e₂ →β e₃

  -- Regras de contexto
  app-left : ∀ {e₁ e₁′ e₂}
    → e₁ →β e₁′
    → App e₁ e₂ →β App e₁′ e₂

  app-right : ∀ {e₁ e₂ e₂′}
    → e₂ →β e₂′
    → App e₁ e₂ →β App e₁ e₂′

-- Exemplos de provas que correspondem aos testes originais

-- Prova que (λx.x)y →β y
id-reduction : ∀ {y}
  → App (Lam "x" (Var "x")) (Var y) →β Var y
id-reduction = β-red var-eq

-- Prova que (λx.xx)y →β yy
self-app-reduction : ∀ {y}
  → App (Lam "x" (App (Var "x") (Var "x"))) (Var y) 
  →β App (Var y) (Var y)
self-app-reduction = β-red (app var-eq var-eq)

-- Prova para o primeiro passo de redução de (λx.λy.x)a →β λy.a
const-reduction-step1 : 
  App (Lam "x" (Lam "y" (Var "x"))) (Var "a") →β Lam "y" (Var "a")
const-reduction-step1 = β-red (lam (λ ()) var-eq)
