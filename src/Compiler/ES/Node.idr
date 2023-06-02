||| The `Node` generator.
module Compiler.ES.Node

import Idris.Env
import Idris.Syntax

import Compiler.ES.Codegen

import Compiler.Common

import Core.Context
import Core.Options
import Core.TT
import Core.UnifyState
import Libraries.Utils.Path

import System

import Data.Maybe

%default covering

findNode : IO String
findNode = do
   Nothing <- idrisGetEnv "NODE"
      | Just node => pure node
   path <- pathLookup ["node"]
   pure $ fromMaybe "/usr/bin/env node" path

||| Compile a TT expression to Node
compileToNode :
  Ref Ctxt Defs ->
  Ref Syn SyntaxInfo ->
  Ref UST UState ->
  ClosedTerm -> Core String
compileToNode c s u tm = do
  js <- compileToES c s u Node tm ["node", "javascript"]
  pure $ shebang ++ js
  where
    shebang : String
    shebang = "#!/usr/bin/env node\n"

||| Node implementation of the `compileExpr` interface.
compileExpr :
  Ref Ctxt Defs ->
  Ref Syn SyntaxInfo ->
  Ref UST UState ->
  (tmpDir : String) ->
  (outputDir : String) ->
  ClosedTerm ->
  (outfile : String) ->
  Core (Maybe String)
compileExpr c s u tmpDir outputDir tm outfile =
  do es <- compileToNode c s u tm
     let out = outputDir </> outfile
     Core.writeFile out es
     pure (Just out)

||| Node implementation of the `executeExpr` interface.
executeExpr :
  Ref Ctxt Defs ->
  Ref Syn SyntaxInfo ->
  Ref UST UState ->
  (tmpDir : String) -> ClosedTerm -> Core ()
executeExpr c s u tmpDir tm =
  do let outn = tmpDir </> "_tmp_node.js"
     js <- compileToNode c s u tm
     Core.writeFile outn js
     node <- coreLift findNode
     quoted_node <- pure $ "\"" ++ node ++ "\"" -- Windows often have a space in the path.
     coreLift_ $ system (quoted_node ++ " " ++ outn)

||| Codegen wrapper for Node implementation.
export
codegenNode : Codegen
codegenNode = MkCG compileExpr executeExpr Nothing Nothing
