module Example.Monad.Fixedpoint
  ( run )
    where

import Control.Monad ( join )
import Control.Monad.IO.Class
import Z3.Monad

run :: IO ()
run = do res <- evalZ3 script
         print res

script :: Z3 Result
script = do
  intSort <- mkIntSort
  boolSort <- mkBoolSort
  iDec <- mkFreshFuncDecl "i" [] intSort
  nDec <- mkFreshFuncDecl "N" [] intSort
  r <- mkFreshFuncDecl "R" [intSort] boolSort
  q <- mkFreshFuncDecl "Q" [] boolSort

  i <- mkApp iDec []
  n <- mkApp nDec []

  _0  <- mkInteger 0
  _2  <- mkInteger 2
  _41 <- mkInteger 41

  lhs0 <- mkEq i _0
  rhs0 <- mkApp r [i]
  rule0 <- mkImplies lhs0 rhs0

  lhs1 <- mkAnd =<< sequence [mkApp r [i], mkLt i n]
  rhs1 <- mkApp r =<< sequence [mkAdd [i, _2]]
  rule1 <- mkImplies lhs1 rhs1

  lhs2 <- mkAnd =<< sequence [mkApp r [i], mkNot =<< mkLt i n]
  rhs2 <- mkNot =<< mkEq i _41
  rule2 <- mkImplies lhs2 rhs2

  query <- join $ mkImplies <$> mkNot rule2 <*> mkApp q []

  reps <- traverse astToString [rule0, rule1, query]
  liftIO $ mapM_ print reps

  r0 <- mkStringSymbol "rule0"
  r1 <- mkStringSymbol "rule1"
  r2 <- mkStringSymbol "rule2"

  pars <- mkParams
  engine <- mkStringSymbol "fixedpoint.engine"
  duality <- mkStringSymbol "duality"
  paramsSetSymbol pars engine duality

  fixedpointSetParams pars

  fixedpointRegisterVariable iDec
  fixedpointRegisterVariable nDec
  fixedpointRegisterRelation r
  fixedpointRegisterRelation q

  fixedpointAddRule rule0 r0
  fixedpointAddRule rule1 r1
  fixedpointAddRule query r2

  fixedpointQueryRelations [q]
