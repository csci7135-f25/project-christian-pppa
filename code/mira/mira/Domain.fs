module mira.Domain

open Plang.Compiler.TypeChecker.Types
open mira.NumericDomain

type Progloc = Entry | Exit | Progl of int
type ActorType = string 
type Location =
    {
        ActorType: ActorType
        StateName: string
        EventName: string
        ProgLoc: Progloc 
    }

type Address = string

type DistLoc = Map<Address, Location>
type Var = string

// Base value abstractions
type AbsVal = Int of IntAbs | Bool of BoolAbs | Tuple of Map<string, AbsVal> | Addr of Address | Set of ActorType

type ConfigSize = Number of int | SymbolicSize of string

let rec type_to_top (ty: PLanguageType): AbsVal =
    match ty with
    | :? SequenceType as t -> (Set t.ElementType.CanonicalRepresentation)
    | :? PrimitiveType as t -> (match t.CanonicalRepresentation with 
                                | "int" -> Int(ITop)
                                |  "bool" -> Bool(BTop)
                                | _ -> failwith "unhandled primitive encountered"
                                )
    | :? NamedTupleType as t -> Tuple (List.fold (fun acc (b: NamedTupleEntry) -> acc.Add(b.Name, type_to_top b.Type)) Map.empty (Seq.toList t.Fields))
    | :? PermissionType as t -> Addr t.CanonicalRepresentation
    | :? TypeDefType as t -> type_to_top t.TypeDefDecl.Type
    | _ -> failwith "unrecognized type found"


type Store = Map<Var, AbsVal>
type DistStore = Map<Address, Store>
type SingleMessage = {
    Sender: Address
    Receiver: Address
    SenderEvent: string 
    Event: string
    Arg: AbsVal option // can have one or zero values
}

type BCastMsg = {
    Sender: Address
    Receiver: ActorType
    SenderEvent: string 
    Event: string
    Arg: AbsVal option
}
type Message =  P2P of SingleMessage | BCast of BCastMsg

type Network = Message list
type MGVertex = (ActorType * string)
type Bound = NumAbs
type MessageGraph = {
    Vertices: Set<MGVertex>
    Edges: Map<MGVertex*MGVertex, Bound*Bound>
}

type AbstractState = {
    Loc: DistLoc
    Stores: DistStore
    Net: Network
    MsgGraph: MessageGraph
}

let rec join_bool (b1: BoolAbs) (b2: BoolAbs): BoolAbs =
    match b1,b2 with
    | BTop,_ -> BTop
    | BBot,_ -> b2
    | _,BTop -> BTop
    | _, BBot -> b1
    | (Bval b1, Bval b2) -> if b1 = b2 then Bval b1 else BTop
and 
    upper_join (n1:NumAbs) (n2:NumAbs): NumAbs =
    match n1,n2 with
    | Num v1, Num v2 -> Num (max v1 v2)
    | Symbolic s1, Symbolic s2 -> if s1 = s2 then n1 else NTop
    | _,_ -> NTop

and lower_join (n1:NumAbs) (n2:NumAbs): NumAbs =
    match n1,n2 with
    | Num v1, Num v2 -> Num (min v1 v2)
    | Symbolic s1, Symbolic s2 -> if s1 = s2 then n1 else NTop
    | _,_ -> NTop   
    
and join_int (i1: IntAbs) (i2:IntAbs): IntAbs =
    match i1, i2 with
    | ITop, _ -> ITop
    | IBot, _ -> i2
    | _, ITop -> ITop
    | _,IBot -> i1
    | Interval(l1,b1), Interval(l2,b2) -> Interval (lower_join l1 l2, upper_join b1 b2)
    | Interval(l1,b1), LInterval b2 -> LInterval (upper_join b1 b2)
    | Interval(l1,b1), UInterval b2 -> UInterval (lower_join l1 b2)
    | LInterval _, UInterval _ -> ITop
    | UInterval _, LInterval _ -> ITop
    | LInterval b2, Interval(l1,b1) -> LInterval (upper_join b1 b2)
    | UInterval b2 , Interval(l1,b1)-> UInterval (lower_join l1 b2)
    | UInterval b1, UInterval b2 -> UInterval (lower_join b1 b2)
    | LInterval b1, LInterval b2 -> LInterval (upper_join b1 b2)
    
and join_tuple (t1: Map<string,AbsVal>) (t2: Map<string,AbsVal>): Map<string,AbsVal> =
    let keys = Set.union (Set.ofSeq t1.Keys) (Set.ofSeq t2.Keys)
    Set.fold (fun acc k ->
                (match (t1.TryFind k, t2.TryFind k) with
                | None, None -> failwith "unreachable"
                | Some v1, Some v2 -> acc.Add(k, join_val v1 v2)
                | Some v1, None -> acc.Add(k, v1)
                | _, Some v2 -> acc.Add(k,v2)
                )
              ) Map.empty keys
  
and join_val (v1: AbsVal) (v2: AbsVal) : AbsVal =
    match v1, v2 with
    | Addr a1, Addr a2 -> if a1 = a2 then v1 else Int(ITop) // else shouldn't occur
    | Bool b1, Bool b2 -> Bool(join_bool b1 b2)
    | Int i1, Int i2 -> Int(join_int i1 i2)
    | Set s1, Set s2 -> v1
    | Tuple t1, Tuple t2 -> Tuple(join_tuple t1 t2)
    | _,_ -> Int(ITop)

let join_st (s1: Store) (s2:Store): Store =
    let keys = Set.union (Set.ofSeq s1.Keys) (Set.ofSeq s2.Keys)
    Set.fold (fun acc k ->
                (match (s1.TryFind k, s2.TryFind k) with
                | None, None -> failwith "unreachable"
                | Some v1, Some v2 -> acc.Add(k, join_val v1 v2)
                | Some v1, None -> acc.Add(k, v1)
                | _, Some v2 -> acc.Add(k,v2)
                )
              ) Map.empty keys

let join_stores (s1: DistStore) (s2:DistStore) : DistStore =
    let keys = Set.union (Set.ofSeq s1.Keys) (Set.ofSeq s2.Keys)
    Set.fold (fun acc k ->
                (match (s1.TryFind k, s2.TryFind k) with
                | None, None -> failwith "unreachable"
                | Some v1, Some v2 -> acc.Add(k, join_st v1 v2)
                | Some v1, None -> acc.Add(k, v1)
                | _, Some v2 -> acc.Add(k,v2)
                )
             ) Map.empty keys
    
let join_net (n1:Network) (n2:Network) =
    List.append n1 n2 |> List.distinct
    
let join_graph (g1: MessageGraph) (g2: MessageGraph): MessageGraph =
    let vertices = Set.union g1.Vertices g2.Vertices
    let edges:Map<MGVertex*MGVertex,Bound*Bound> = Map.fold (fun acc k (b2a,b2b) ->
                                                                let newpairwise = 
                                                                    (match acc.TryFind(k),b2a  with
                                                                     | Some(Num m,_), Num n -> Num (max m n)
                                                                     | Some(NTop,_), _ ->  NTop
                                                                     | _, NTop -> NTop
                                                                     | Some(a,_), b when a = b -> a
                                                                     | Some(a,_), b -> SymAdd(a,b)
                                                                     | None, b -> b
                                                                    )
                                                                let newtotal =
                                                                    (match acc.TryFind(k),b2b  with
                                                                     | Some(_,Num m), Num n -> Num (max m n)
                                                                     | Some(_,NTop), _ ->  NTop
                                                                     | _, NTop -> NTop
                                                                     | Some(_,a), b when a = b -> a
                                                                     | Some(_,a), b -> SymAdd(a,b)
                                                                     | None, b -> b 
                                                                    )
                                                                acc.Add(k,(newpairwise,newtotal))
                                                                ) g1.Edges g2.Edges
    {Vertices = vertices; Edges=edges }

let join_state (s1: AbstractState) (s2: AbstractState): AbstractState =
    let newloc = s2.Loc
    let new_store = join_stores s1.Stores s2.Stores
    let new_net = join_net s1.Net s2.Net
    let new_graph = join_graph s1.MsgGraph s2.MsgGraph
    {Loc = newloc; Stores = new_store; Net = new_net; MsgGraph = new_graph}
    
let widen_graph (g1:MessageGraph) (g2: MessageGraph): MessageGraph =
    let vertices = Set.union g1.Vertices g2.Vertices
    let edges:Map<MGVertex*MGVertex,Bound*Bound> = Map.fold (fun acc k (b2a,b2b) ->
                                                                let newpairwise = 
                                                                    (match acc.TryFind(k), b2a with
                                                                     | Some(a, _), b when a = b -> a
                                                                     | Some(NTop, _), _ -> NTop
                                                                     | _, b -> b
                                                                    )
                                                                let newtotal =
                                                                    (match acc.TryFind(k), b2b with
                                                                     | Some(_, a), b when a = b -> a
                                                                     | Some(_, NTop), _ -> NTop
                                                                     | _, b -> b
                                                                    )
                                                                acc.Add(k,(newpairwise,newtotal))
                                                                ) g1.Edges g2.Edges
    {Vertices = vertices; Edges=edges }
    
let widen_state (s1:AbstractState) (s2:AbstractState): AbstractState =
    let newloc = s2.Loc
    let new_store = join_stores s1.Stores s2.Stores // TODO - change when real loops exist, for now its alright - should have interval widen later.
    let new_net = join_net s1.Net s2.Net
    let new_graph = widen_graph s1.MsgGraph s2.MsgGraph // the 'core' widen is here
    {Loc = newloc; Stores = new_store; Net = new_net; MsgGraph = new_graph}
    