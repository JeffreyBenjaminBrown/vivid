-- | This adds VarList instances for synths with more than 26 parameters.
-- It goes up to 32.
--
-- With minor changes, this is a simple duplication of
-- Vivid.Synthdef.TypesafeArgs

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE NoIncoherentInstances #-}
-- {-# LANGUAGE ConstraintKinds -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE GADTSyntax, NoMonoLocalBinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies, NoMonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
-- Needed for a nested type family instance:
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Montevideo.Monome.Instances.TypesafeArgs where

import GHC.TypeLits
import Vivid


addVarToSet :: Variable sym -> VarSet syms -> VarSet (sym ': syms)
addVarToSet _ _ = VarSet

infixr ?>
(?>) :: (KnownSymbol a, VarList b) => I a -> b -> TypedVarList (a ': InnerVars b)
(?>) a b =
   let (re, name) = makeTypedVarList b
   in (iToLiteralVar a : re, addVarToSet {- (iToArg a) -} V name)

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p,KnownSymbol q,KnownSymbol r,KnownSymbol s,KnownSymbol t
   ,KnownSymbol u,KnownSymbol v,KnownSymbol w,KnownSymbol x,KnownSymbol y
   ,KnownSymbol z, KnownSymbol aa
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z, I aa) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p ?> q ?> r ?> s ?> t ?> u ?> v ?> w ?> x ?> y ?> z ?> aa

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p,KnownSymbol q,KnownSymbol r,KnownSymbol s,KnownSymbol t
   ,KnownSymbol u,KnownSymbol v,KnownSymbol w,KnownSymbol x,KnownSymbol y
   ,KnownSymbol z, KnownSymbol aa, KnownSymbol ab
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa, I ab)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa,I ab) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p ?> q ?> r ?> s ?> t ?> u ?> v ?> w ?> x ?> y ?> z ?> aa ?> ab

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p,KnownSymbol q,KnownSymbol r,KnownSymbol s,KnownSymbol t
   ,KnownSymbol u,KnownSymbol v,KnownSymbol w,KnownSymbol x,KnownSymbol y
   ,KnownSymbol z, KnownSymbol aa, KnownSymbol ab, KnownSymbol ac
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa,I ab,I ac)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa,I ab,I ac) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p ?> q ?> r ?> s ?> t ?> u ?> v ?> w ?> x ?> y ?> z ?> aa ?> ab ?> ac

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p,KnownSymbol q,KnownSymbol r,KnownSymbol s,KnownSymbol t
   ,KnownSymbol u,KnownSymbol v,KnownSymbol w,KnownSymbol x,KnownSymbol y
   ,KnownSymbol z, KnownSymbol aa, KnownSymbol ab, KnownSymbol ac
   , KnownSymbol ad
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa,I ab,I ac,I ad)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa,I ab,I ac,I ad) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p ?> q ?> r ?> s ?> t ?> u ?> v ?> w ?> x ?> y ?> z ?> aa ?> ab ?> ac ?> ad

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p,KnownSymbol q,KnownSymbol r,KnownSymbol s,KnownSymbol t
   ,KnownSymbol u,KnownSymbol v,KnownSymbol w,KnownSymbol x,KnownSymbol y
   ,KnownSymbol z, KnownSymbol aa, KnownSymbol ab, KnownSymbol ac
   , KnownSymbol ad, KnownSymbol ae
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa,I ab,I ac,I ad,I ae)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa,I ab,I ac,I ad,I ae) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p ?> q ?> r ?> s ?> t ?> u ?> v ?> w ?> x ?> y ?> z ?> aa ?> ab ?> ac ?> ad ?> ae

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p,KnownSymbol q,KnownSymbol r,KnownSymbol s,KnownSymbol t
   ,KnownSymbol u,KnownSymbol v,KnownSymbol w,KnownSymbol x,KnownSymbol y
   ,KnownSymbol z, KnownSymbol aa, KnownSymbol ab, KnownSymbol ac
   , KnownSymbol ad, KnownSymbol ae, KnownSymbol af
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa,I ab,I ac,I ad,I ae,I af)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa,I ab,I ac,I ad,I ae,I af) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p ?> q ?> r ?> s ?> t ?> u ?> v ?> w ?> x ?> y ?> z ?> aa ?> ab ?> ac ?> ad ?> ae ?> af
