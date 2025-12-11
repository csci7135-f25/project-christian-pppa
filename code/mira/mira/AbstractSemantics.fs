module mira.AbstractSemantics
open Plang.Compiler.TypeChecker.AST
open Plang.Compiler.TypeChecker.AST.Declarations
open Plang.Compiler.TypeChecker.Types
open mira.Domain
open mira.NumericDomain
open mira.CFG
open Plang.Compiler.TypeChecker.AST.Expressions

let progloc_to_cfa (p:Progloc) (c:CFG) : int =
    match p with
    | Progloc.Entry -> c.Initial
    | Progloc.Exit -> c.Final
    | Progloc.Progl i -> i
    
let cfa_to_progloc (p:int) (c:CFG): Progloc =
    if p = c.Initial
    then Progloc.Entry
    else if p = c.Final then
        Progloc.Exit
    else
        Progloc.Progl p 

let mach_sym_size (t: PLanguageType): NumAbs =
    Symbolic $"{t.CanonicalRepresentation}SIZE"

let get_cfg (l:Location) (p:ProgramFlow): CFG =
    let machine = List.filter (fun (m:MachineFlow)-> m.Name = l.ActorType) p.Machines |> List.head
    let evnt_handler = List.filter (fun h -> h.EventName = l.EventName && h.StateName = l.StateName) machine.EventHandlers
                       |> List.head
    match evnt_handler.Handler with
    | Handler h -> h.CFG
    | _ -> failwith "location unreachable"
    
let get_cmds (l:Location) (p:ProgramFlow): (CFGEdge list) =
    let machine = List.filter (fun (m:MachineFlow)-> m.Name = l.ActorType) p.Machines |> List.head
    let evnt_handler = List.filter (fun h -> h.EventName = l.EventName && h.StateName = l.StateName) machine.EventHandlers
                       |> List.head
    match evnt_handler.Handler with
    | Handler h -> List.filter (fun e -> e.Source = (progloc_to_cfa l.ProgLoc h.CFG)) h.CFG.Edges
    | _ -> failwith "location unreachable"

let rec abs_eval (e: IPExpr) (s:Store) (a:Address): AbsVal =
    match e with
    | :? IntLiteralExpr as i -> Int (Interval (Num i.Value,Num i.Value))
    | :? BoolLiteralExpr as i -> Bool (Bval i.Value)
    | :? VariableAccessExpr as v -> s[v.Variable.Name]
    | :? SizeofExpr as t ->
        let k = (mach_sym_size t.Expr.Type)
        Int (Interval (k,k))
    | :? ThisRefExpr as _  -> Addr a
    | :? BinOpExpr as b ->
        let left = abs_eval b.Lhs s a
        let right = abs_eval b.Rhs s a
        match b.Operation with
        | BinOpType.Add -> (match left, right with
                                | Int i1, Int i2 -> Int (interval_add i1 i2)
                                | _ -> Int IBot)
        | BinOpType.Eq -> if left = right then Bool (Bval true) else Bool (Bval false)
        | BinOpType.Ge -> (match left,right with
                           | Int i1, Int i2 -> Bool (interval_ge i1 i2)
                           | _ -> Bool BTop
                           )
        | BinOpType.Neq -> if left <> right then Bool (Bval true) else Bool (Bval false)
        | BinOpType.Div -> (match left, right with
                                | Int i1, Int i2 -> Int (interval_div i1 i2)
                                | _ -> Int IBot)
        | _ -> failwith "unsupported"
    // binary ops
    | :? UnaryOpExpr as u ->
        let inter = abs_eval u.SubExpr s a
        (match u.Operation, inter with
         | UnaryOpType.Not, Bool b -> Bool(bool_negate b )
         | _ ->failwith "un-implemented: negative numbers trigger this"
         )
    | :? NamedTupleAccessExpr as t ->
        let tup = abs_eval t.SubExpr s a
        (match tup with
         | Tuple tuple -> tuple[t.FieldName]
         | _ -> failwith "tuple access on non-tuple"
         )
    | _ -> failwith "unimplemented"

let transfer_fn (s:AbstractState) (a:Address) (c: CFGEdge) (cfg:CFG) : AbstractState option =
    let eta = s.Stores[a]
    match c.Label with
    | Epsilon -> Some {s with Loc = s.Loc.Add(a,{s.Loc[a] with ProgLoc = cfa_to_progloc c.Target cfg})}
    | AssignNode (x,_,e) ->
        let v = abs_eval e eta a
        let eta' = eta.Add(x,v)
        Some {s with Stores = s.Stores.Add(a,eta'); Loc = s.Loc.Add(a,{s.Loc[a] with ProgLoc = cfa_to_progloc c.Target cfg})}
    | AssertNode e ->
        let v = abs_eval e eta a
        match v with
        | Bool b -> (match b with
                     | BBot | Bval false -> None
                     | _ -> Some {s with Loc = s.Loc.Add(a, {s.Loc[a] with ProgLoc = cfa_to_progloc c.Target cfg})}
                     )
        | _ -> None 
    | SendNode (e,r,v) ->
        let recv = abs_eval r eta a
        let args = List.map (fun v -> abs_eval v eta a) v
        match recv with
        | Addr recv -> 
            let (msg:SingleMessage) = {Sender = a
                                       SenderEvent = s.Loc[a].EventName
                                       Event=e
                                       Receiver = recv
                                       Arg = if List.length args >0 then Some (List.head args) else None}
            Some {s with Loc = s.Loc.Add(a,{s.Loc[a] with ProgLoc = cfa_to_progloc c.Target cfg}); Net = (P2P msg)::s.Net}
        | _ -> None
        
    | BroadCast (e,t,v) ->
        let args = List.map (fun v -> abs_eval v eta a) v
        let (bcastmsg:BCastMsg) = {
                            Sender = a
                            SenderEvent = s.Loc[a].EventName
                            Event=e
                            Receiver = t
                            Arg = if List.length args >0 then Some (List.head args) else None
                        }
        Some {s with Loc = s.Loc.Add(a,{s.Loc[a] with ProgLoc = cfa_to_progloc c.Target cfg}); Net = (BCast bcastmsg)::s.Net}
    | GotoNode _ -> failwith "Not Currently Needed"

let deliver_msg (msg:Message) (s:AbstractState) (p:ProgramFlow) (config: Map<string,ConfigSize>): AbstractState =
    match msg with
    | BCast m ->
        let receiver = List.filter (fun addr -> s.Loc[addr].ActorType = m.Receiver) (Seq.toList s.Loc.Keys) |> List.head
        let recv_state = s.Stores[receiver]
        let recv_loc = s.Loc[receiver]
        let h = retrieve_handler p recv_loc.ActorType m.Event recv_loc.StateName
        let eta = (match m.Arg with
                   | Some x -> (match h.FnSignature with
                                | (_,[v]) -> recv_state.Add(v.Name,x) 
                               )
                   | None -> recv_state
            )
        let new_loc = {recv_loc with EventName = m.Event; ProgLoc = cfa_to_progloc h.CFG.Initial h.CFG}
        let (_,new_net) = (List.partition (fun x -> x = msg) s.Net)
        let senderVertex = (s.Loc[m.Sender].ActorType, m.SenderEvent)
        let receiverVertex = (recv_loc.ActorType, m.Event)
        let (pair,tot) = s.MsgGraph.Edges.TryFind((senderVertex,receiverVertex))|> Option.defaultValue (Num 0, Num 0)
        let in_degree_sum = 
            s.MsgGraph.Edges
            |> Map.toSeq
            |> Seq.filter (fun ((src, dst), _) -> dst = senderVertex)
            |> Seq.map (fun (_, (_, total)) -> total)
            |> Seq.fold (fun acc b -> num_add acc b) (Num 0)
        let num_send = 
            match config.TryFind(s.Loc[m.Sender].ActorType) with
            | Some(Number n) -> Num n
            | Some(SymbolicSize s) -> Symbolic s
            | None -> Num 1
        let in_degree_sum = if in_degree_sum = (Num 0) then num_send else in_degree_sum
            
        let num_recv = 
            match config.TryFind(recv_loc.ActorType) with
            | Some(Number n) -> Num n
            | Some(SymbolicSize s) -> Symbolic s
            | None -> Num 1 
        
        let num_exec = num_div in_degree_sum num_send  // number of times im executing: sum of in-degrees of sender's vertex, / number of participants.
        // new bound: add - num exec * 1, num_exec * num_target
        let pairwise = num_add pair num_exec
        let newtot = num_add tot (SymMult (num_exec,num_recv))
        let new_msg_graph = {s.MsgGraph with
                                Vertices = s.MsgGraph.Vertices.Add(senderVertex).Add(receiverVertex)
                                Edges = s.MsgGraph.Edges.Add((senderVertex,receiverVertex), (pairwise,newtot))
                            }
        {s with Loc = s.Loc.Add(receiver, new_loc); Stores = s.Stores.Add(receiver,eta); Net = new_net; MsgGraph = new_msg_graph}
    | P2P m ->
        let recv_state = s.Stores[m.Receiver]
        let recv_loc = s.Loc[m.Receiver]
        let h = retrieve_handler p recv_loc.ActorType m.Event recv_loc.StateName
        let eta = (match m.Arg with
                   | Some x -> (match h.FnSignature with
                                | (_,[v]) -> recv_state.Add(v.Name,x) 
                               )
                   | None -> recv_state
            )
        let new_loc = {recv_loc with EventName = m.Event; ProgLoc = cfa_to_progloc h.CFG.Initial h.CFG}
        let (_,new_net) = (List.partition (fun x -> x = msg) s.Net)
        
        let senderVertex = (s.Loc[m.Sender].ActorType, m.SenderEvent)
        let receiverVertex = (recv_loc.ActorType, m.Event)
        let (pair,tot) = s.MsgGraph.Edges.TryFind((senderVertex,receiverVertex))|> Option.defaultValue (Num 0, Num 0)
        let num_send = 
            match config.TryFind(s.Loc[m.Sender].ActorType) with
            | Some(Number n) -> Num n
            | Some(SymbolicSize s) -> Symbolic s
            | None -> Num 1
            
        let num_recv = 
            match config.TryFind(recv_loc.ActorType) with
            | Some(Number n) -> Num n
            | Some(SymbolicSize s) -> Symbolic s
            | None -> Num 1 
        let in_degree_sum = 
            s.MsgGraph.Edges
            |> Map.toSeq
            |> Seq.filter (fun ((src, dst), _) -> dst = senderVertex)
            |> Seq.map (fun (_, (_, total)) -> total)
            |> Seq.fold (fun acc b -> num_add acc b) (Num 0)
        let in_degree_sum = if in_degree_sum = (Num 0) then (Num 1) else in_degree_sum
        let num_exec = num_div in_degree_sum num_send 
        
        let new_bound = (num_add pair num_exec, num_add tot num_exec)
        let new_msg_graph = {s.MsgGraph with
                                Vertices = s.MsgGraph.Vertices.Add(senderVertex).Add(receiverVertex)
                                Edges = s.MsgGraph.Edges.Add((senderVertex,receiverVertex), new_bound)
                            }
        {s with Loc = s.Loc.Add(m.Receiver, new_loc); Stores = s.Stores.Add(m.Receiver,eta); Net = new_net; MsgGraph = new_msg_graph}
       
let step (s:AbstractState) (p: ProgramFlow) (config: Map<string,ConfigSize>): AbstractState list =
    //1. Pick process
    let nonIdle = List.filter (fun addr -> s.Loc[addr].ProgLoc <> Progloc.Exit ) (List.ofSeq s.Loc.Keys)
    if (List.length nonIdle > 0)
        then
            let proc = List.head nonIdle
            let cfg = get_cfg s.Loc[proc] p
            let cmds = get_cmds s.Loc[proc] p
            // for each cmd, generate a new state
            List.choose (fun cmd -> transfer_fn s proc cmd cfg ) cmds
        else
            List.map (fun msg ->
                deliver_msg msg s p config
                ) s.Net
            
let rec worklist_algo (queue: AbstractState list) (seen: AbstractState list) (p:ProgramFlow)  (config: Map<string,ConfigSize>)=
    if List.length queue = 0
    then
        printfn "Seen States: %A" (List.length seen) 
        if List.length seen > 0
        then
            let res = List.fold (fun acc elm -> join_state acc elm) (List.head seen) (List.tail seen)
            printfn "Message History Automaton:\n "
            printfn "Vertices: %A\n" res.MsgGraph.Vertices
            printfn "Edges: %A\n" res.MsgGraph.Edges
            Some(res)
        else
            None      
    else
        let next = List.head queue
        let next_states = step next p config
        if List.length next_states = 0
        then worklist_algo (List.tail queue) (next::seen) p config
        else 
            let new_states = List.filter (fun s -> not (List.contains s seen || List.contains s queue) ) next_states
            let newer_states = List.map (fun n ->
                                        let sameLocStates = List.filter (fun s ->  s.Loc = n.Loc) seen
                                        if List.length sameLocStates > 5
                                        then
                                           List.fold (fun acc s -> widen_state acc s) n sameLocStates
                                        else
                                            List.fold (fun acc s -> join_state acc s ) n sameLocStates
                                        ) new_states
            worklist_algo (List.append (List.tail queue) newer_states) (next::seen) p config
        
        
let front_end (file:string) (initialEvents: Map<string, string>) (config: Map<string,ConfigSize>): AbstractState option =
    let scope = test_loader file
    let p = program_flow scope
    let machines = List.map (fun (m:MachineFlow) -> m.Name) p.Machines
    let events = List.map (fun e -> e.Name ) p.EventDefs
    let vertices = List.allPairs machines events
    let empty_graph = {Vertices = Set.ofList vertices; Edges = Map.empty;}
    //TODO: generate initial state
    let empty_net: Network  = List.Empty
    let (init_loc: DistLoc) = Map.fold (fun a b c -> a.Add(b,{StateName="Init"; ProgLoc = Progloc.Entry; EventName = c; ActorType=b})) Map.empty initialEvents
    let (init_stores: DistStore) = List.fold (fun acc b ->
                                                let init_handler = retrieve_handler p b (initialEvents[b]) "Init"
                                                let (param_bindings:Map<string,AbsVal>) =
                                                    (match init_handler.FnSignature with
                                                    | (_, params) -> List.fold (fun store (v:Variable) -> store.Add(v.Name, type_to_top v.Type)) Map.empty params)
                                                
                                                acc.Add(b, param_bindings) ) Map.empty machines 
    let initial_state = {Net=empty_net; MsgGraph = empty_graph; Loc = init_loc; Stores = init_stores}
    worklist_algo [initial_state] [] p config
    