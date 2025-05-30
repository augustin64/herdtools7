(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Answer of a test compilation or run *)


type hash = { filename : string ;  hash : string ; }
type hash_env = hash StringMap.t

type answer =
  | Completed of
      Archs.t * Name.t   (* Test arch and name *)
        * string         (* C source file of test *)
        * StringSet.t    (* cycles *)
        * hash_env       (* name -> hash *)
        * int            (* number of threads *)
        * bool           (* require Pointer Authentication Code *)
  | Interrupted of Archs.t * exn
  | Absent of Archs.t
