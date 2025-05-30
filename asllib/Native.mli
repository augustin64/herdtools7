(******************************************************************************)
(*                                ASLRef                                      *)
(******************************************************************************)
(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)
(******************************************************************************)
(* Disclaimer:                                                                *)
(* This material covers both ASLv0 (viz, the existing ASL pseudocode language *)
(* which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  *)
(* experimental, and as yet unreleased version of ASL.                        *)
(* This material is work in progress, more precisely at pre-Alpha quality as  *)
(* per Arm’s quality standards.                                               *)
(* In particular, this means that it would be premature to base any           *)
(* production tool development on this material.                              *)
(* However, any feedback, question, query and feature request would be most   *)
(* welcome; those can be sent to Arm’s Architecture Formal Team Lead          *)
(* Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    *)
(* herdtools7 github repository.                                              *)
(******************************************************************************)

(* Sequential ASL interpreter using straight OCaml values as backend. *)

type native_value =
  | NV_Literal of AST.literal
  | NV_Vector of native_value list
  | NV_Record of native_value ASTUtils.IMap.t

module NoScope : Backend.SCOPE with type t = unit

module StaticBackend :
  Backend.S
    with type value = native_value
     and type 'a m = 'a
     and module Scope = NoScope

module DeterministicBackend :
  Backend.S
    with type value = native_value
     and type 'a m = 'a
     and module Scope = NoScope

module DeterministicInterpreter (C : Interpreter.Config) :
  Interpreter.S with module B = DeterministicBackend

val interpret :
  ?instrumentation:bool ->
  StaticEnv.global ->
  AST.t ->
  int * Instrumentation.semantics_rule list
