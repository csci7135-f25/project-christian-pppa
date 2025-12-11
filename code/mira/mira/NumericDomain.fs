module mira.NumericDomain

//Symbolic Arithmetic
type NumAbs = Num of int | Symbolic of string | SymAdd of NumAbs * NumAbs | SymMult of NumAbs * NumAbs | NTop

//Intervals with Symbolic Arithmetic
type IntAbs = ITop | IBot | Interval of NumAbs * NumAbs | LInterval of NumAbs | UInterval of NumAbs

type ConfigSize = Number of int | SymbolicSize of string

type BoolAbs = BTop | BBot | Bval of bool
    
let bool_negate (b:BoolAbs): BoolAbs =
    match b with
    | Bval b -> Bval (not b)
    | _ -> b
    

let num_add (n1: NumAbs) (n2: NumAbs): NumAbs =
    match n1,n2 with
    | Num 0, _ -> n2 
    | Num v1, Num v2 -> Num (v1+v2)
    | NTop, _ -> NTop
    | _, NTop -> NTop
    | _ -> SymAdd(n1,n2) // contains a symbolic const somewhere
    
// this can be weak, i'd like more precise symbolic reasoning but its hard
let num_div (n1: NumAbs) (n2: NumAbs): NumAbs =
    match n1,n2 with 
    | _, Num 0 -> NTop
    | NTop, _ -> NTop
    | _, NTop -> NTop
    | Num a, Num b -> Num (a / b)
    | a, b when a = b -> Num 1
    | (SymMult(a,b),c) when c = a -> b
    | (SymMult(a,b),c) when c = b -> a 
    | _ -> NTop
    
let num_ge (n1: NumAbs) (n2: NumAbs): BoolAbs =
    match n1,n2 with
    | Num v1, Num v2 -> Bval (v1 >= v2)
    | NTop, NTop -> Bval true
    | NTop, _ -> Bval false
    | _ -> BTop
    
let interval_add (i1: IntAbs) (i2:IntAbs): IntAbs =
    match i1,i2 with
    | ITop, _ -> ITop
    | _, ITop -> ITop
    | IBot, _ -> IBot
    |_, IBot -> IBot
    | Interval (n1,n2), Interval (n3,n4) -> Interval ((num_add n1 n3),(num_add n2 n4))
    | Interval (_,n2), LInterval n3 -> LInterval(num_add n2 n3)
    | Interval (n1,_), UInterval n3 -> UInterval (num_add n1 n3)
    | UInterval n3, Interval (n1,n2) -> UInterval (num_add n1 n3)
    | LInterval n3,Interval (n1,n2) -> LInterval(num_add n2 n3)
    | LInterval n1, LInterval n2 -> LInterval (num_add n1 n2)
    | UInterval n1, UInterval n2 -> UInterval (num_add n1 n2)
    | _ -> ITop
    
let interval_div (i1: IntAbs) (i2: IntAbs): IntAbs =
  match i1, i2 with
  | ITop, _ -> ITop
  | _, ITop -> ITop
  | IBot, _ -> IBot
  | _, IBot -> IBot
  // Division is really weird, so we're super imprecise here
  | Interval (n1,n2), Interval (n3,n4) -> Interval (num_div n1 n3, num_div n2 n4)
  | Interval _, _ -> ITop
  | LInterval _, LInterval _ -> ITop
  | UInterval _, UInterval _ -> ITop
  | LInterval _, UInterval _ -> ITop
  | UInterval _, LInterval _ -> ITop

let interval_ge (i1:IntAbs) (i2: IntAbs): BoolAbs =
    match i1, i2 with
    | ITop, _ -> Bval true
    | _, ITop -> Bval false
    | IBot, _ -> BTop
    | _, IBot -> Bval true
    | Interval (l1,u1), Interval (l2, u2) ->
        (match num_ge l1 u2 with
        | Bval true -> Bval true
        | Bval false -> (match num_ge l2 u1 with 
                            | Bval true -> Bval false
                            | _ -> BTop
                        )
        | BTop  -> BTop
        | BBot -> BBot)
    | Interval (l1,_), LInterval l2  ->
        (match num_ge l1 l2 with 
                | Bval true -> Bval true
                | _ -> BTop 
        )
    | LInterval l1, Interval (l2,_ ) ->
        (match num_ge l2 l1 with 
                | Bval true -> Bval false
                | _ -> BTop 
        )
    | LInterval _, UInterval _ -> Bval false
    | UInterval l1, LInterval l2 ->
        (match num_ge l1 l2 with
            | Bval true -> Bval true
            | _ -> BTop 
        )
    | UInterval l1, Interval (_,u2) ->
        (match num_ge l1 u2 with
          | Bval true -> Bval true
          | _ -> BTop          
            )
    | Interval (_,u2) , UInterval l2 ->
        (match num_ge u2 l2 with
          | Bval true -> BTop
          | _ -> Bval false
         )
    | LInterval _, LInterval _ -> BTop
    | UInterval _, UInterval _ -> BTop 