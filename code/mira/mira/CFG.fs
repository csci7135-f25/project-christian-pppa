module mira.CFG
open Plang.Compiler.TypeChecker.AST
open Plang.Compiler.TypeChecker.AST.Statements
open Plang.Compiler.TypeChecker.AST.Declarations
open Plang.Compiler.TypeChecker.AST.States
open Plang.Compiler.TypeChecker
open Plang.Compiler.TypeChecker.AST.Expressions
open Plang.Compiler.TypeChecker.Types
open Plang.Compiler
open System.IO

type CFGNode = 
    | AssignNode of string * PLanguageType * IPExpr
    | SendNode of string * IPExpr * IPExpr list
    | BroadCast of string * string * IPExpr list
    | GotoNode of IPStmt * State * IPExpr
    | AssertNode of IPExpr
    | Epsilon

type CFGEdge = {
    Source: int
    Target: int
    Label: CFGNode
}

type CFG = {
    Vertices: Set<int>
    Edges: CFGEdge list
    Initial: int
    Final: int
    }

let mutable nextVertexId = 0
let fresh_vertex () = 
    let id = nextVertexId
    nextVertexId <- nextVertexId + 1
    id

let add_vertex (g: CFG) = 
    let v = fresh_vertex()
    {g with Vertices = Set.add v g.Vertices}

let single_edge (c: CFGNode) = 
    let i = fresh_vertex()
    let f = fresh_vertex()
    let ve = Set.empty |> Set.add i |> Set.add f
    let edge = { Source = i; Target = f; Label = c}
    {Vertices = ve; Edges = [edge] ; Initial = i; Final = f}
    
let rec extract_var_expr (e:IPExpr) =
    match e with 
    | :? VariableAccessExpr as v -> v.Variable.Name
    | :? NamedTupleAccessExpr as t -> (extract_var_expr t.SubExpr + "." + t.FieldName)
    | _ -> failwith ("Unsupported expr found: " + e.ToString())
    
let concat_cfg (a:CFG)(b: CFG) =
    let a_edges= a.Edges |> List.map (fun edge ->
            { edge with 
                Source = if edge.Source = a.Final then b.Initial else edge.Source
                Target = if edge.Target = a.Final then b.Initial else edge.Target
            })
    {Vertices=Set.union a.Vertices b.Vertices
     Edges = List.concat [a_edges; b.Edges]
     Initial=a.Initial; Final=b.Final}

let rec stmt_to_cfg (s:IPStmt) =
 match s with
 | :? NoStmt as _ -> single_edge Epsilon
 | :? AssignStmt as t -> single_edge (AssignNode(extract_var_expr t.Location, t.Location.Type, t.Value))
 | :? AssertStmt as t -> single_edge (AssertNode t.Assertion)
 | :? GotoStmt as t -> single_edge (GotoNode(s, t.State, t.Payload))
 | :? SendStmt as t -> 
                            match t.Evt with 
                                | :? EventRefExpr as e ->
                                    single_edge (SendNode(e.Value.Name, t.MachineExpr, Seq.toList t.Arguments))
                                | _ -> failwith "Encountered send to non-event"
 | :? CompoundStmt as t -> 
            let stmts = Seq.toList t.Statements
            let cfgs = List.map (fun x -> stmt_to_cfg x) stmts
            if List.length cfgs > 0 then 
                List.fold (fun a b -> concat_cfg a b) (List.head cfgs) (List.tail cfgs)
            else 
                single_edge Epsilon
 | :? IfStmt as ite -> 
            let cond = ite.Condition 
            let tcfg = stmt_to_cfg ite.ThenBranch
            let ecfg = stmt_to_cfg ite.ElseBranch
            let initial_v = fresh_vertex()
            let end_v = fresh_vertex()
            let new_edges = [{Source=initial_v;Target=tcfg.Initial;Label =AssertNode cond }
                             {Source=initial_v;Target=ecfg.Initial;Label=AssertNode (UnaryOpExpr(cond.SourceLocation,UnaryOpType.Not,cond))}
                             {Source=tcfg.Final;Target=end_v;Label=Epsilon}
                             {Source=ecfg.Final;Target=end_v;Label=Epsilon}
                             ]
            {Vertices = Set.add end_v (Set.add initial_v (Set.union tcfg.Vertices ecfg.Vertices))
             Edges =  List.append (List.append tcfg.Edges ecfg.Edges) new_edges
             Initial = initial_v
             Final = end_v
             }
 | :? ForeachStmt as f ->
    match List.head (Seq.toList f.Body.Statements) with
    | :? SendStmt as t ->
            (match t.Evt with
             | :?  EventRefExpr as e -> single_edge (BroadCast(e.Value.Name,f.Item.Type.CanonicalRepresentation,Seq.toList t.Arguments))
             | _ -> failwith "encountered send to non-event"
                )
    | _ ->  let body_cfg = stmt_to_cfg f.Body
            let end_v = fresh_vertex()
            let init_v = fresh_vertex()
            let new_edges = [{Source=init_v;Target=end_v;Label=Epsilon}; {Source=init_v;Target=body_cfg.Initial;Label=Epsilon}; {Source=body_cfg.Final;Target=init_v;Label=Epsilon}]
            {Vertices = Set.add init_v (Set.add end_v body_cfg.Vertices)
             Edges = List.concat [body_cfg.Edges;new_edges]
             Initial = init_v
             Final = end_v
             }
 | _ -> failwith ("Unsupported statement encountered.")
 
type FnSig = string * (Variable list)

let p_sig_to_fnsig (f: FunctionSignature) (name: string) =
    FnSig(name,Seq.toList f.Parameters)
    
type FnCFG = {
    FnSignature : FnSig
    CFG: CFG
}

type ActionCFG = Deferred | Ignored | Handler of FnCFG

type HandlerCFG = {
    StateName : string;
    EventName : string; 
    Handler : ActionCFG

}

type MachineFlow = {
    Name : string
    EventHandlers : HandlerCFG list
    InitialState : string
    Fields : (string * PLanguageType) list
}

type Evnt = {Name: string; PayloadTy: PLanguageType}

type ProgramFlow = {
    Functions : FnCFG list ;
    Machines : MachineFlow list;
    TyDefs : (string * PLanguageType) list
    EventDefs : Evnt list
}

let fn_to_cfg (s:Function) (signature: FnSig) (fields : (string * PLanguageType) list) =
    let cfg = stmt_to_cfg s.Body
    let (_,p) = signature
    let params = List.map (fun (v:Variable) -> (v.Name,v.Type)) p
    (*Because PLanguageType is not comparable, have a manual uniqueness function. Somewhat inefficient*)
    {
        FnSignature = signature
        CFG = cfg
    }
    
let event_act_to_cfg (a: IStateAction) (fields: (string * PLanguageType) list) =
    match a with 
    | :? EventDefer -> Deferred 
    | :? EventIgnore -> Ignored 
    | :? EventDoAction as e ->
        let fnsig = FnSig(e.Trigger.Name, (Seq.toList e.Target.Scope.Variables |>
                                          List.filter (fun x -> x.Role = VariableRole.Param) ))
        Handler( fn_to_cfg e.Target fnsig fields)
    | _ -> failwith "Unimplemented"

let state_to_cfgs (s: State) (fields: (string * PLanguageType) list) = 
    let handlers = Seq.toList s.AllEventHandlers
    let cfgs = List.map (fun (h: System.Collections.Generic.KeyValuePair<PEvent,IStateAction>) -> {StateName = s.Name;EventName=h.Key.Name;Handler=event_act_to_cfg h.Value fields}) handlers 
    match s.Entry with 
    | null -> cfgs 
    | f -> List.append cfgs [{StateName=s.Name;EventName="_entry_";Handler=Handler(fn_to_cfg f (p_sig_to_fnsig f.Signature (s.Name + ":_entry_")) fields)}]
let machine_to_cfgs (m: Machine) =
    let fields = Seq.toList m.Fields |> List.map (fun v -> (v.Name, v.Type))
    let cfgs = Seq.toList m.States |> Seq.collect (fun s -> state_to_cfgs s fields) |>  Seq.toList
    {Name = m.Name ; EventHandlers = cfgs; InitialState = m.StartState.Name; Fields=fields}


let retrieve_handler (p:ProgramFlow) (t:string) (e:string) (s:string) =
    match ((List.filter (fun (m:MachineFlow) -> m.Name = t) p.Machines |> List.head).EventHandlers
    |> List.filter (fun h -> h.EventName = e && h.StateName = s) |> List.head).Handler with
    | Handler fn -> fn
    | _ -> failwith "no handler found"
let program_flow (p: Scope 
) = 
    let fncfgs = Seq.toList p.Functions |> List.map (fun x -> fn_to_cfg x (p_sig_to_fnsig x.Signature x.Name) []) 
    let machines = Seq.toList p.Machines |> List.map machine_to_cfgs
    let tydefs = Seq.toList p.Typedefs |> List.map (fun x -> (x.Name,x.Type) )
    let events = List.map (fun (e:PEvent) -> {Name = e.Name; PayloadTy = e.PayloadType} ) (Seq.toList p.Events)
    {ProgramFlow.Functions = fncfgs; ProgramFlow.Machines=machines; ProgramFlow.TyDefs =tydefs; ProgramFlow.EventDefs = events}

// Borrowed from disprove toplevel, to run tests
let fc_list_shim (l: 'a list) = ResizeArray<'a> l

let test_loader (file: string) : Scope =
    let outd = DirectoryInfo("./out")
    let cs = DefaultCompilerOutput(outputDirectory=outd)
    let conf = CompilerConfiguration(OutputDirectory=outd,Output=cs,OutputLanguages=(fc_list_shim [CompilerOutput.Symbolic]), ProjectName="toy", InputPFiles= fc_list_shim [file])
    let scope = Analyzer.AnalyzeCompilationUnit(conf, [|Compiler.Parse(conf,FileInfo(file))|])
    scope