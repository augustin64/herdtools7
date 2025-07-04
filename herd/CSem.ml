(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
module type Config = sig
  include Sem.Config
end

module
  Make
    (Conf:Config)
    (V:Value.S with
      type Cst.Instr.t = CBase.instruction
      and type arch_extra_op = CBase.arch_extra_op
      and type 'a arch_constr_op = 'a CBase.arch_constr_op) = struct
    let unroll =
      match Conf.unroll with
      | None -> Opts.unroll_default `C
      | Some u -> u

    (*
       We check if the variant does not match any of the others rather than explicitly
       checking if it matchs v1 to get v1 as default even if no variant is given.
       If more variants are added, this condition needs to be updated.
    *)
    let lkmmv1 = not (Conf.variant (Variant.LKMMVersion `lkmmv2))

    module C = CArch_herd.Make(SemExtra.ConfigToArchConfig(Conf))(V)
    module Act = CAction.Make(C)
    include SemExtra.Make(Conf)(C)(Act)
    let barriers = []
    let isync = None
(* TODO: No real mixed size for C, as access sizes depend upon types... *)
    let nat_sz = V.Cst.Scalar.machsize

    let atomic_pair_allowed e1 e2 = match e1.E.iiid, e2.E.iiid with
    | E.IdSome i1,E.IdSome i2 -> i1 == i2
    | _,_ -> false


(****************************)
(* Build semantics function *)
(****************************)

    module Mixed(SZ : ByteSize.S) = struct

      let (>>=) = M.(>>=)
      let (>>==) = M.(>>==)
      let (>>*=) = M.(>>*=)
      let (>>|) = M.(>>|)
      let (>>::) = M.(>>::)
      let (>>!) = M.(>>!)
      let (>>>) = M.cseq
      let (>>>>) = M.(>>>>)
      let next0 = B.Next []

      module MOorAN = MemOrderOrAnnot
      let a_once = ["once"]
      let a_noreturn = ["noreturn"]
      let an_once = MOorAN.AN a_once
      let a_mb = ["mb"]
      let a_rb_dep = ["rb_dep"]
      let no_mo = MOorAN.AN []
      let mo_as_anmo mo = MOorAN.MO mo

      let mk_cutoff msg ii = M.mk_singleton_es (Act.CutOff msg) ii

      let read_loc port mo =
        M.read_loc port (fun loc v -> Act.Access (Dir.R, loc, v, mo, false, nat_sz))

      let  read_exchange port vstored mo a ii =
        M.read_loc port
          (fun loc v -> Act.RMW (loc,v,vstored,mo,nat_sz))
          (A.Location_global a)
          ii

      let read_reg port r ii =
        M.read_loc port
          (fun loc v -> Act.Access (Dir.R, loc, v, no_mo, false, nat_sz))
          (A.Location_reg (ii.A.proc,r)) ii

      let read_mem port mo a ii =
        M.read_loc port
          (fun loc v -> Act.Access (Dir.R, loc, v, mo, false, nat_sz))
          (A.Location_global a)
          ii

      let read_mem_atomic port a loc =
        M.read_loc port
          (fun loc v -> Act.Access (Dir.R, loc, v,  a, true, nat_sz))
          (A.Location_global loc)

      let read_mem_atomic_known port a loc v =
        M.read_loc port
          (fun loc _v -> Act.Access (Dir.R, loc, v,  a, true, nat_sz))
          (A.Location_global loc)

      let write_loc mo loc v ii =
        M.mk_singleton_es (Act.Access (Dir.W, loc, v, mo, false, nat_sz)) ii >>! v

      let write_reg r v ii = write_loc no_mo (A.Location_reg (ii.A.proc,r)) v ii
      let write_mem mo a  = write_loc mo (A.Location_global a)
      let write_mem_atomic a loc v ii =
        M.mk_singleton_es
          (Act.Access (Dir.W, A.Location_global loc, v, a, true,nat_sz)) ii >>! v


      let mk_fence_a a ii = M.mk_fence (Act.Fence  (MOorAN.AN a)) ii
      let mk_mb ii =  mk_fence_a a_mb ii
      let mk_rb_dep ii =   mk_fence_a a_rb_dep ii

      let xchg port rloc re a ii =
        let add_mb = match a with
        | ["mb"] -> true | _ -> false in
        let aw =
          (if lkmmv1 then
            match a with
            | ["release"] -> MOorAN.AN a
            | _ -> an_once
          else
            MOorAN.AN a)
        and ar =
          (if lkmmv1 then
            match a with
            | ["acquire"] -> MOorAN.AN a
            | _ -> an_once
          else
            MOorAN.AN a) in
        let rmem = fun loc -> read_mem_atomic port ar loc ii
        and wmem = fun loc v -> write_mem_atomic aw loc v ii >>! () in
        let exch = M.linux_exch rloc re rmem wmem in
        if lkmmv1 && add_mb then
          mk_fence_a a ii >>*=
          fun () -> exch >>*=
            fun v -> mk_fence_a a ii >>! v
        else exch

      let cxchg port rloc re mo v_loc ii =
        let m = match mo with
        | MemOrder.SC
        | MemOrder.Rlx -> (mo, mo)
        | MemOrder.Rel -> (MemOrder.Rlx, mo)
        | MemOrder.Acq -> (mo, MemOrder.Rlx)
        | MemOrder.Acq_Rel -> (MemOrder.Acq, MemOrder.Rel)
        | _ -> assert false in
        let rmem = match v_loc with
        | None -> fun loc -> read_mem_atomic port (MOorAN.MO (fst m)) loc ii
        | Some x -> fun loc -> read_mem_atomic_known port (MOorAN.MO (fst m)) loc x ii
        and wmem = fun loc v -> write_mem_atomic (MOorAN.MO (snd m)) loc v ii >>! () in
        let exch = M.linux_exch rloc re rmem wmem in
        exch

      let linux_lock loc ii =
        M.mk_singleton_es
          (Act.Lock (A.Location_global loc,Act.LockLinux Dir.R)) ii >>*= fun () ->
            M.mk_singleton_es
              (Act.Lock (A.Location_global loc,Act.LockLinux Dir.W)) ii

      let rec build_semantics_expr port e ii : V.v M.t = match e with
      | C.Const v -> M.unitT (V.maybevToV v)
      | C.LoadReg r -> read_reg port r ii
      | C.LoadMem(loc,mo) ->
          let port = Port.Addr in
          let open MemOrderOrAnnot in
          (match mo with
          | AN [] | MO _ -> build_semantics_expr port loc ii
          | AN (_::_) ->  begin match loc with
            | C.LoadMem (loc,AN []) ->
                build_semantics_expr port loc ii
            | _ ->
                Warn.user_error "Bad __load argument: %s"
                  (C.dump_expr loc) end) >>=
          fun l ->
            begin match mo with
            | AN [("deref"|"lderef")]   ->
(* Cannot do this with a macro, by lack of sequencing expression operator *)
                read_mem port an_once l ii >>*=
                fun v -> mk_rb_dep ii >>! v
            | _ ->
                read_mem port mo l ii
            end

      | C.TryLock (_,C.MutexC11) -> assert false
      | C.TryLock (loc,C.MutexLinux) ->
          build_semantics_expr port loc ii >>=
          fun l ->
            M.altT
              (linux_lock l ii >>! V.one)
              (M.mk_singleton_es
                 (Act.TryLock (A.Location_global l)) ii >>! V.zero)
      | C.IsLocked (_,C.MutexC11) -> assert false
      | C.IsLocked (loc,C.MutexLinux) ->
          build_semantics_expr port loc ii >>=
          fun l ->
            M.altT
              (M.mk_singleton_es (* Read from lock *)
                 (Act.ReadLock (A.Location_global l,true)) ii >>! V.one)
              (M.mk_singleton_es (* Read from a unlock *)
                 (Act.ReadLock (A.Location_global l,false)) ii >>! V.zero)
      | C.Op(op,e1,e2) ->
          (build_semantics_expr port e1 ii >>|
          build_semantics_expr port e2 ii) >>= fun (v1,v2) ->
            M.op op v1 v2

      | C.Exchange(l,e,(MOorAN.AN a)) ->
          let re = build_semantics_expr Port.Data e ii
          and rloc =  build_semantics_expr Port.Addr l ii in
          xchg port rloc re a ii


      | C.Exchange(l,e,MOorAN.MO mo) ->
          if Conf.variant Variant.NoRMW then
            let re = build_semantics_expr Port.Data e ii
            and rloc = build_semantics_expr Port.Addr l ii in
            cxchg port rloc re mo None ii
          else
            (build_semantics_expr Port.Data e ii >>|
            build_semantics_expr Port.Addr l ii)
              >>= (fun (v,l) ->
                read_exchange port v mo l ii)

      | C.CmpExchange (eloc,eold,enew,a) ->
          let mloc =  build_semantics_expr Port.Addr eloc ii
          and mold =  build_semantics_expr Port.No eold ii in
          let add_mb r =
            (if lkmmv1 then
              (match a with
              | ["mb"] ->
                  mk_mb ii >>*= fun () -> r >>*= fun v -> mk_mb ii >>! v
              | _ -> r)
            else
              r)
          and arok =
            (if lkmmv1 then
              (match a with
              | ["acquire"] -> MOorAN.AN a
              | _ -> an_once)
            else
              MOorAN.AN a)
          and awok =
            (if lkmmv1 then
              (match a with
              | ["release"] -> MOorAN.AN a
              | _ -> an_once)
            else
              MOorAN.AN a)
          and arnok =
            (if lkmmv1 then
              an_once
            else
              MOorAN.AN a) in
          M.altT
            (let r =
              let mnew = build_semantics_expr Port.Data enew ii
              and rmem vloc =
                read_mem_atomic Port.No arok vloc ii
              and wmem vloc w =
                write_mem_atomic awok vloc w ii >>! () in
              M.linux_cmpexch_ok mloc mold mnew rmem wmem M.assign in
            add_mb r)
            (M.linux_cmpexch_no mloc mold
               (fun vloc -> read_mem_atomic Port.Data arnok vloc ii)
               M.neqT)

      | C.Fetch(l,op,e,mo) ->
          (build_semantics_expr Port.Data e ii >>|
          build_semantics_expr Port.Addr l ii)
          >>= fun (v,l) -> fetch_op op v mo l ii


      | C.ECas(obj,exp,des,success,failure,strong) ->
          (* Obtain location of "expected" value *)
          build_semantics_expr Port.Addr exp ii >>= fun loc_exp ->
            (* Obtain location of object *)
            build_semantics_expr Port.Addr obj ii >>= fun loc_obj ->
              (* Non-atomically read the value at "expected" location *)
              read_mem Port.No no_mo loc_exp ii >>*= fun v_exp ->
                (* Non-deterministic choice *)
                M.altT
                  (read_mem Port.Data (mo_as_anmo failure) loc_obj ii >>*= fun v_obj ->
                    (* For "strong" cas: fail only when v_obj != v_exp *)
                    (if strong then M.neqT v_obj v_exp else M.unitT ()) >>= fun () ->
                      (* Non-atomically write that value into the "expected" location *)
                      write_mem no_mo loc_exp v_obj ii >>!
                      V.zero)
                  (* Obtain "desired" value *)
                  (build_semantics_expr Port.No des ii >>= fun v_des ->
                    if Conf.variant Variant.NoRMW then
                      let re = build_semantics_expr Port.Data des ii
                      and rloc = build_semantics_expr Port.Addr obj ii in
                      cxchg port rloc re success (Some v_exp) ii >>!
                      V.one
                    else
                      (* Do RMW action on "object", to change its value from "expected"
                         to "desired", using memory order "success" *)
                      M.mk_singleton_es
                        (Act.RMW (A.Location_global loc_obj,v_exp,v_des,success,nat_sz)) ii >>!
                      V.one)



      | C.AtomicOpReturn (eloc,op,e,ret,a) ->
        if lkmmv1 then
          begin match a with
          | ["mb"] ->
              mk_mb ii >>*=
              fun () -> build_atomic_op ret a_once a_once eloc op e ii >>*=
                fun v -> mk_mb ii >>! v
          | _ ->
              build_atomic_op ret
                (match a with ["acquire"] -> a | _ -> a_once)
                (match a with ["release"] -> a | _ -> a_once)
                eloc op e ii
          end
        else
          build_atomic_op ret a a eloc op e ii
      | C.AtomicAddUnless (eloc,ea,eu,retbool,a) ->
          (* read arguments *)
          let mloc = build_semantics_expr Port.Addr eloc ii
          and mu =  build_semantics_expr Port.No eu ii
          and mrmem loc =
            read_mem_atomic Port.No
              (if lkmmv1 then an_once else (MOorAN.AN a)) loc ii in
          M.altT
            (let r =
              M.linux_add_unless_ok mloc (build_semantics_expr Port.Addr ea ii)
                mu mrmem
                (fun loc v ->
                   write_mem_atomic
                     (if lkmmv1 then an_once else (MOorAN.AN a)) loc v ii
                   >>! ())
                M.neqT M.add (if retbool then Some V.one else None) in
            (if lkmmv1 then
              mk_mb ii >>*= fun () -> r >>*= fun v ->
                mk_mb ii >>! v
            else
              r))
            (M.linux_add_unless_no mloc mu mrmem M.assign
               (if retbool then Some V.zero else None))
      | C.ExpSRCU(eloc,a) ->
          let r = match a with
          |  ["srcu-lock"] ->
              Some (A.V.intToV ((ii.A.proc +1)* 307 + ii.A.program_order_index * 599))
          | _ -> None in
          build_semantics_expr Port.Addr eloc ii >>=
          fun (vloc) ->
            M.mk_singleton_es (Act.SRCU (A.Location_global vloc,a,r)) ii
              >>! (match r with None -> V.zero | Some v -> v)
      | C.ECall (f,_) -> Warn.fatal "Macro call %s in CSem" f

      and build_atomic_op ret a_read a_write eloc op e ii =
        build_semantics_expr Port.Data e ii >>|
        (build_semantics_expr Port.Addr eloc ii >>=
         fun loc ->
           (read_mem_atomic Port.Data
              (MOorAN.AN a_read) loc ii >>| M.unitT loc))
          >>== (* Notice '==' as atomic_op 'ouput' iico depends on R *)
        (fun (v,(vloc,loc)) ->
          M.op op vloc v >>=
          fun w ->
            let a = MOorAN.AN a_write in
            match ret with
            | C.OpReturn -> write_mem_atomic a loc w ii
            | C.FetchOp  -> write_mem_atomic a loc w ii >>! vloc)

      and fetch_op op v mo loc ii =
        if Conf.variant Variant.NoRMW then
          read_mem_atomic Port.Data
            (MOorAN.MO (MemOrder.extract_read mo)) loc ii >>= fun oldv ->
          M.op op oldv v
          >>= fun w ->
          write_mem_atomic
            (MOorAN.MO (MemOrder.extract_write mo)) loc w ii
          >>! oldv
        else
          M.fetch op v
            (fun v vstored ->
               Act.RMW (A.Location_global loc,v,vstored,mo,nat_sz))
            ii

      let zero = ParsedConstant.zero

      let build_cond e ii =
        let open Op in
        let e = match e with
        | C.Op ((Lt|Gt|Eq|Ne|Le|Ge),_,_) -> e
        | _ -> C.Op (Ne,e,C.Const zero) in
        build_semantics_expr Port.No e ii

      let rec build_semantics test ii : (A.program_order_index * B.t) M.t =
        let ii =
          {ii with A.program_order_index = A.next_po_index ii.A.program_order_index;} in
        match ii.A.inst with
        | C.Seq (insts,_) ->
            build_semantics_list test insts ii
        | C.If(c,t,Some e) ->
            build_cond c ii >>>> fun ret ->
              let ii' =
                {ii with A.program_order_index =
                 A.next_po_index ii.A.program_order_index;}
              in
              let then_branch = build_semantics test {ii' with A.inst = t} in
              let else_branch = build_semantics test {ii' with A.inst = e} in
              M.choiceT ret then_branch else_branch

        | C.If(c,t,None) ->
            build_cond c ii >>>> fun ret ->
              let ii' =
                {ii with A.program_order_index =
                 A.next_po_index ii.A.program_order_index;}
              in
              let then_branch = build_semantics test {ii' with A.inst = t} in
              M.choiceT ret then_branch (build_semantics_list test [] ii)
        | C.While(c,t,n) ->
            build_cond c ii >>>>
            begin
              let else_branch =
                M.unitT (ii.A.program_order_index, next0)
              and then_branch =
                if n >= unroll then
                  mk_cutoff "While" ii >>= fun () -> M.unitT (ii.A.program_order_index, B.Exit)
                else
                  build_semantics test
                    {ii with A.inst = t} >>> fun (prog_order, _branch) ->
                  build_semantics test
                    {ii with A.program_order_index = prog_order; A.inst = C.While(c,t,n+1);} in
              fun ret -> M.choiceT ret then_branch else_branch
            end
        | C.DeclReg _ ->  M.unitT (ii.A.program_order_index, next0)
        | C.CastExpr e ->
            build_semantics_expr Port.No e ii >>=
            fun _ ->  M.unitT (ii.A.program_order_index, next0)
        | C.StoreReg(_,Some r,e) ->
            build_semantics_expr Port.No e ii >>=
            fun v -> write_reg r v ii >>=
              fun _ ->  M.unitT (ii.A.program_order_index, next0)
        | C.StoreReg(_,None,e) ->
            build_semantics_expr Port.No e ii >>=
              fun _ ->  M.unitT (ii.A.program_order_index, next0)
        | C.StoreMem(loc,e,mo) ->
            (begin
              let open MemOrderOrAnnot in
              match mo with
              | AN [] | MO _ ->  build_semantics_expr Port.Addr loc ii
              | AN (_::_) -> match loc with
                | C.LoadMem (loc,AN []) -> build_semantics_expr Port.Addr loc ii
                | _ ->
                    Warn.user_error "Bad __store argument: %s"
                      (C.dump_expr loc)
            end >>|
            build_semantics_expr Port.Data e ii) >>=
            fun (l,v) -> write_mem mo l v ii >>=
              fun _ -> M.unitT (ii.A.program_order_index, next0)
(* C11 mutex, not sure about them... *)
        | C.Lock (l,k) ->
            build_semantics_expr Port.Addr l ii >>=
            fun l -> begin match k with
            | C.MutexC11 ->
                (* C11 Lock always successful, oversimplification?  *)
                (M.mk_singleton_es
                   (Act.Lock (A.Location_global l, Act.LockC11 false)) ii)
            | C.MutexLinux ->
                linux_lock l ii
            end
                >>= fun () -> M.unitT (ii.A.program_order_index, next0)
        | C.Unlock (l,k) ->
            build_semantics_expr Port.Addr l ii >>=
            fun l ->
              M.mk_singleton_es (Act.Unlock (A.Location_global l,k)) ii
                >>= fun _ -> M.unitT (ii.A.program_order_index, next0)
(********************)
        | C.AtomicOp  (eloc,op,e,a) ->
            let ar = (if lkmmv1 then a_noreturn else a)
            and aw = (if lkmmv1 then a_once else a) in
            build_atomic_op C.OpReturn ar aw eloc op e ii
              >>= fun _ -> M.unitT (ii.A.program_order_index, next0)
(********************)
        | C.Fence(mo) ->
            M.mk_fence (Act.Fence mo) ii
              >>= fun _ -> M.unitT (ii.A.program_order_index, next0)
(********************)
        | C.InstrSRCU(e,a,oe) ->
            build_semantics_expr Port.No e ii >>|
            (match oe with
            | None -> M.unitT None
            | Some e -> build_semantics_expr Port.Addr e ii >>= fun v -> M.unitT (Some v))
              >>=
            fun (l,v) ->
              M.mk_singleton_es (Act.SRCU (A.Location_global l,a,v)) ii
                >>= fun _ -> M.unitT (ii.A.program_order_index, next0)
(********************)
          | C.Symb _ -> Warn.fatal "No symbolic instructions allowed."
          | C.PCall (f,_) -> Warn.fatal "Procedure call %s in CSem" f

      and build_semantics_list test insts ii = match insts with
      | [] -> M.unitT (ii.A.program_order_index, next0)
      | inst :: insts ->
          let ii = {ii with A.inst=inst; } in
          build_semantics test ii >>> fun (prog_order, _branch) ->
            build_semantics_list test insts {ii with  A.program_order_index = prog_order;}

      let spurious_setaf _ = assert false

    end
  end
