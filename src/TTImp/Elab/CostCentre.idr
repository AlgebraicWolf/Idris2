module TTImp.Elab.CostCentre

import Core.Context
import Core.Core
import Core.Env
import Core.Metadata
import Core.Normalise
import Core.Reflect
import Core.Unify
import Core.TT
import Core.Value

import Idris.REPL.Opts
import Idris.Syntax

import TTImp.Elab.Check
import TTImp.Elab.Delayed
import TTImp.TTImp

%default covering

export
checkCostCentre : {vars : _} ->
                  {auto c : Ref Ctxt Defs} ->
                  {auto m : Ref MD Metadata} ->
                  {auto u : Ref UST UState} ->
                  {auto e : Ref EST (EState vars)} ->
                  {auto s : Ref Syn SyntaxInfo} ->
                  {auto o : Ref ROpts REPLOpts} ->
                  RigCount -> ElabInfo ->
                  NestedNames vars -> Env Term vars ->
                  FC -> RawImp -> RawImp -> Maybe (Glued vars) ->
                  Core (Term vars, Glued vars)
checkCostCentre rig elabinfo nest env fc nm tm exp
    = do defs <- get Ctxt
         -- Check that the label is of type `Prelude.String`
         let n = (UN (Basic "String"))
         let labeltt = PrimVal emptyFC (PrT StringType)
         (nm', gnmty) <- check rig elabinfo nest env nm (Just (gnf env labeltt))
         -- Proceed with checking the body
         (tm', gty) <- check rig elabinfo nest env tm exp
         pure (CostCentre fc nm' tm', gty)
