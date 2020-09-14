-- | This adds VarList instances for synths with more than 26 parameters.
-- It goes up to 32.


{-# OPTIONS_GHC -fno-warn-orphans #-}

-- {-# LANGUAGE ConstraintKinds -} -- (was?) for a nested type family instance
-- {-# LANGUAGE GADTSyntax, NoMonoLocalBinds #-}
-- {-# LANGUAGE RankNTypes #-}

{-# LANGUAGE DataKinds
, ExistentialQuantification
, FlexibleContexts
, FlexibleInstances
, FunctionalDependencies
, InstanceSigs
, LambdaCase
, MultiParamTypeClasses
, NoIncoherentInstances
, NoMonomorphismRestriction
, PolyKinds
, ScopedTypeVariables
, StandaloneDeriving
, TypeFamilies, NoMonoLocalBinds
, TypeOperators
, UndecidableInstances
, ViewPatterns #-}


module Montevideo.Vivid.LongVarLists where

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

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p,KnownSymbol q,KnownSymbol r,KnownSymbol s,KnownSymbol t
   ,KnownSymbol u,KnownSymbol v,KnownSymbol w,KnownSymbol x,KnownSymbol y
   ,KnownSymbol z, KnownSymbol aa, KnownSymbol ab, KnownSymbol ac
   , KnownSymbol ad, KnownSymbol ae, KnownSymbol af, KnownSymbol ag
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa,I ab,I ac,I ad,I ae,I af,I ag)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa,I ab,I ac,I ad,I ae,I af, I ag) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p ?> q ?> r ?> s ?> t ?> u ?> v ?> w ?> x ?> y ?> z ?> aa ?> ab ?> ac ?> ad ?> ae ?> af ?> ag

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p,KnownSymbol q,KnownSymbol r,KnownSymbol s,KnownSymbol t
   ,KnownSymbol u,KnownSymbol v,KnownSymbol w,KnownSymbol x,KnownSymbol y
   ,KnownSymbol z, KnownSymbol aa, KnownSymbol ab, KnownSymbol ac
   , KnownSymbol ad, KnownSymbol ae, KnownSymbol af, KnownSymbol ag, KnownSymbol ah
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa,I ab,I ac,I ad,I ae,I af,I ag,I ah)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa,I ab,I ac,I ad,I ae,I af,I ag,I ah) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag,ah]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag,ah) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p ?> q ?> r ?> s ?> t ?> u ?> v ?> w ?> x ?> y ?> z ?> aa ?> ab ?> ac ?> ad ?> ae ?> af ?> ag ?> ah

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p,KnownSymbol q,KnownSymbol r,KnownSymbol s,KnownSymbol t
   ,KnownSymbol u,KnownSymbol v,KnownSymbol w,KnownSymbol x,KnownSymbol y
   ,KnownSymbol z, KnownSymbol aa, KnownSymbol ab, KnownSymbol ac
   , KnownSymbol ad, KnownSymbol ae, KnownSymbol af, KnownSymbol ag, KnownSymbol ah, KnownSymbol ai
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa,I ab,I ac,I ad,I ae,I af,I ag,I ah,I ai)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa,I ab,I ac,I ad,I ae,I af,I ag,I ah,I ai) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p ?> q ?> r ?> s ?> t ?> u ?> v ?> w ?> x ?> y ?> z ?> aa ?> ab ?> ac ?> ad ?> ae ?> af ?> ag ?> ah ?> ai

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p,KnownSymbol q,KnownSymbol r,KnownSymbol s,KnownSymbol t
   ,KnownSymbol u,KnownSymbol v,KnownSymbol w,KnownSymbol x,KnownSymbol y
   ,KnownSymbol z, KnownSymbol aa, KnownSymbol ab, KnownSymbol ac
   , KnownSymbol ad, KnownSymbol ae, KnownSymbol af, KnownSymbol ag, KnownSymbol ah, KnownSymbol ai, KnownSymbol aj
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa,I ab,I ac,I ad,I ae,I af,I ag,I ah,I ai,I aj)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa,I ab,I ac,I ad,I ae,I af,I ag,I ah,I ai,I aj) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p ?> q ?> r ?> s ?> t ?> u ?> v ?> w ?> x ?> y ?> z ?> aa ?> ab ?> ac ?> ad ?> ae ?> af ?> ag ?> ah ?> ai ?> aj

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p,KnownSymbol q,KnownSymbol r,KnownSymbol s,KnownSymbol t
   ,KnownSymbol u,KnownSymbol v,KnownSymbol w,KnownSymbol x,KnownSymbol y
   ,KnownSymbol z, KnownSymbol aa, KnownSymbol ab, KnownSymbol ac
   , KnownSymbol ad, KnownSymbol ae, KnownSymbol af, KnownSymbol ag, KnownSymbol ah, KnownSymbol ai, KnownSymbol aj, KnownSymbol ak
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa,I ab,I ac,I ad,I ae,I af,I ag,I ah,I ai,I aj,I ak)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa,I ab,I ac,I ad,I ae,I af,I ag,I ah,I ai,I aj,I ak) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p ?> q ?> r ?> s ?> t ?> u ?> v ?> w ?> x ?> y ?> z ?> aa ?> ab ?> ac ?> ad ?> ae ?> af ?> ag ?> ah ?> ai ?> aj ?> ak

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p,KnownSymbol q,KnownSymbol r,KnownSymbol s,KnownSymbol t
   ,KnownSymbol u,KnownSymbol v,KnownSymbol w,KnownSymbol x,KnownSymbol y
   ,KnownSymbol z, KnownSymbol aa, KnownSymbol ab, KnownSymbol ac
   , KnownSymbol ad, KnownSymbol ae, KnownSymbol af, KnownSymbol ag, KnownSymbol ah, KnownSymbol ai, KnownSymbol aj, KnownSymbol ak, KnownSymbol al
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa,I ab,I ac,I ad,I ae,I af,I ag,I ah,I ai,I aj,I ak,I al)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa,I ab,I ac,I ad,I ae,I af,I ag,I ah,I ai,I aj,I ak,I al) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p ?> q ?> r ?> s ?> t ?> u ?> v ?> w ?> x ?> y ?> z ?> aa ?> ab ?> ac ?> ad ?> ae ?> af ?> ag ?> ah ?> ai ?> aj ?> ak ?> al

instance
   (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbol d, KnownSymbol e,
    KnownSymbol f, KnownSymbol g, KnownSymbol h, KnownSymbol i, KnownSymbol j,
    KnownSymbol k, KnownSymbol l, KnownSymbol m, KnownSymbol n, KnownSymbol o
   ,KnownSymbol p,KnownSymbol q,KnownSymbol r,KnownSymbol s,KnownSymbol t
   ,KnownSymbol u,KnownSymbol v,KnownSymbol w,KnownSymbol x,KnownSymbol y
   ,KnownSymbol z, KnownSymbol aa, KnownSymbol ab, KnownSymbol ac
   , KnownSymbol ad, KnownSymbol ae, KnownSymbol af, KnownSymbol ag, KnownSymbol ah, KnownSymbol ai, KnownSymbol aj, KnownSymbol ak, KnownSymbol al, KnownSymbol am
   ) =>
   VarList
     (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa,I ab,I ac,I ad,I ae,I af,I ag,I ah,I ai,I aj,I ak,I al,I am)
   where
      type InnerVars (I a, I b, I c, I d, I e, I f, I g, I h, I i, I j, I k, I l, I m, I n, I o,I p,I q,I r,I s,I t,I u,I v,I w,I x,I y,I z,I aa,I ab,I ac,I ad,I ae,I af,I ag,I ah,I ai,I aj,I ak,I al,I am) =
         '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am]
      makeTypedVarList (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am) =
         a ?> b ?> c ?> d ?> e ?> f ?> g ?> h ?> i ?> j ?> k ?> l ?> m ?> n ?> o ?> p ?> q ?> r ?> s ?> t ?> u ?> v ?> w ?> x ?> y ?> z ?> aa ?> ab ?> ac ?> ad ?> ae ?> af ?> ag ?> ah ?> ai ?> aj ?> ak ?> al ?> am
