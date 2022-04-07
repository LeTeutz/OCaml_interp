(* ------- TYPES ---------- *)

type exprC =
  | TrueC 
  | FalseC 
  | NilC 
  | NumC of int 
  | IdC of string
  | AppC of exprC * exprC list
  | PlusC of exprC * exprC
  | MultC of exprC * exprC
  | LamC of string list * exprC
  | IfC of exprC * exprC * exprC
  | EqNumC of exprC * exprC 
  | LtC of exprC * exprC
  | ConsC of exprC * exprC
  | HeadC of exprC
  | TailC of exprC 
  | IsNilC of exprC
  | IsListC of exprC
  | BoxC of exprC
  | UnboxC of exprC
  | SetboxC of exprC * exprC
  | SetC of string * exprC
  | SeqC of exprC * exprC
  | UninitializedC 
 
type value = 
  | NumV of int
  | BoolV of bool
  | NilV
  | ConsV of value * value
  | ClosV of string list * exprC * int
  | BoxV of int
  | ReplV of value list
  | NothingV

type slot = 
  | Slot of string * int

type link = 
  | Link of string * int

type frame = 
  | Frame of link list * slot list


module Heap = Map.Make(struct type t = int let compare = compare end)
module Store = Map.Make(struct type t = int let compare = compare end)

let oc = open_out "HeapStore.txt"

(* ------- REPL ------------- *)

type replCell =
  | RCell of string * exprC 

type replCom = 
  | ExecuteCom of replCell 

(*| DeleteCom of RCell *)
(*| BranchCom of RCell*)

(* ------- HELPERS ---------- *)

let fTuple (a, b) = a
let sTuple (a, b) = b
let frameLinks = function
  | Frame(links, _) -> links

let frameSlots = function
  | Frame(_, slots) -> slots

let slotName = function
  | Slot(name, _) -> name

let slotValue = function
  | Slot(_, value) -> value

let linkLabel = function
  | Link(label, _) -> label

let linkID = function
  | Link(_, id) -> id

let isNumV = function
  | NumV(_)-> true
  | _ -> false

let getNumV = function
  | NumV(value) -> value
  | _ -> failwith "Not a NumV"

let closureArg = function
  | ClosV(arg, _, _) -> arg
  | _ -> failwith "Not a Clos"

let closureBody = function
  | ClosV(_, body, _) -> body
  | _ -> failwith "Not a Clos"

let closureFrameId = function
  | ClosV(_, _, frameId) -> frameId
  | _ -> failwith "Not a Clos"

let boolToString = function 
  | true -> "true"
  | false -> "false"  

let getBoxLocation = function
  | BoxV(location) -> location
  | _ -> failwith "Not a Box"

let getReplV = function
  | ReplV(values) -> values
  | _ -> failwith "Not a ReplV"

let printValueUnit oc = function
  | BoolV b -> Printf.fprintf oc "Boolean: %s" (boolToString b)
  | NumV v -> Printf.fprintf oc "%d \n" v
  | NilV -> Printf.fprintf oc "Nil \n"
  | BoxV loc -> Printf.fprintf oc "Box loc%d\n" loc
  | ConsV(h, t) -> Printf.fprintf oc "Cons\n" 
  | ClosV(arg, body, id) -> Printf.fprintf oc "Closure"
  | ReplV(values) -> Printf.fprintf oc "Repl\n"
  | NothingV -> Printf.fprintf oc "Nothing"

let printValueString = function
  | BoolV b -> boolToString b
  | NumV v -> Int.to_string v
  | NilV -> "Nil"
  | BoxV loc -> Int.to_string loc
  | ConsV(h, t) -> "Cons"
  | ClosV(arg, body, id) -> "Arg"
  | ReplV values -> "Repl"
  | NothingV -> "Nothing"

let printValueStringExprC = function
  | TrueC -> "true"
  | FalseC -> "false"
  | NilC -> "Nil"
  | NumC(v) -> Int.to_string v
  | IdC(id) -> id
  | AppC(e1, e2) -> "App"
  | PlusC(e1, e2) -> "Plus"
  | MultC(e1, e2) -> "Mult"
  | LamC(arg, body) -> "Lam"
  | IfC(e1, e2, e3) -> "If"
  | EqNumC(e1, e2) -> "EqNum"
  | LtC(e1, e2) -> "Lt"
  | ConsC(e1, e2) -> "Cons"
  | HeadC(e) -> "Head"
  | TailC(e) -> "Tail"
  | IsNilC(e) -> "IsNil"
  | IsListC(e) -> "IsList"
  | BoxC(e) -> "Box"
  | UnboxC(e) -> "Unbox"
  | SetboxC(e1, e2) -> "Setbox"
  | SetC(name, e) -> "Set"
  | SeqC(e1, e2) -> "Seq"
  | UninitializedC -> "Uninitialized"

let containsValue = function 
  | None -> NothingV
  | Some x -> x

let containsInt = function
  | None -> -1
  | Some x -> x

let rcell_name = function
  | RCell(name, _) -> name

let rcell_contents = function
  | RCell(_, contents) -> contents

(* ------- DEBUGGING HELPERS ---- *)

let rec printFrameLinks oc = function
  | [] -> Printf.fprintf oc "\n"
  | head :: tail -> (
        Printf.fprintf oc "\t\t\t%s -> %d\n" (linkLabel head) (linkID head);
        printFrameLinks oc tail)

let rec printFrameSlots oc = function
  | [] -> Printf.fprintf oc "\n"
  | head :: tail -> (
        Printf.fprintf oc "\t\t\t%s -> %d\n " (slotName head) (slotValue head);
        printFrameSlots oc tail)  

let printCurrentFrame oc = function
  | Frame(links, slots) -> 
    Printf.fprintf oc "\t\tLinks:\n";
    printFrameLinks oc links;
    Printf.fprintf oc "\t\t------\n";
    Printf.fprintf oc "\t\tSlots:\n";
    printFrameSlots oc slots;
    Printf.fprintf oc "\n"

let rec printCurrentHeap oc heap = 
  match heap with
    | (key, frame) :: tail -> 
        Printf.fprintf oc "\tKey: %d\n" key;
        Printf.fprintf oc "\t\tFrame: \n";
        printCurrentFrame oc frame;
        printCurrentHeap oc tail
    | _ -> Printf.fprintf oc "\n"

let rec printCurrentStore oc store = 
  match store with
    | (loc, v) :: tail -> 
        Printf.fprintf oc "Loc: %d, Value: %s\n" loc (printValueString v);
        printCurrentStore oc tail
    | _ -> Printf.fprintf oc "\n"

let printHeapStore oc heap store = 
  Printf.fprintf oc "Heap:\n\n";
  printCurrentHeap oc (Heap.bindings heap);
  Printf.fprintf oc "----------------\n";    
  Printf.fprintf oc "Store:\n";
  printCurrentStore oc (Store.bindings store);
  Printf.fprintf oc "****************\n" 

(* ------- HEAP METHODS ---------- *)

let initFrame heap = 
  let frameID = Heap.cardinal heap in
  let frame = Frame([], []) in
  (Heap.add frameID frame heap, frameID)

let getFrame frameId heap = 
  try Heap.find frameId heap with Not_found -> (Printf.printf "Frame with id %d not found\n" frameId; failwith "NOT FOUND")

let setFrame frameId frame heap = 
  Heap.add frameId frame heap

let rec findSlot slots slotName = 
  match slots with 
  | [] -> None
  | Slot(name, value) :: rest -> 
    if name = slotName then Some(value)
    else findSlot rest slotName   

let getSlotValue frameId slotName heap = 
  let frame = getFrame frameId heap in
  let slots = frameSlots frame in
  findSlot slots slotName

let updateSlotValue frameId slotName newValue heap = 
  let frame = getFrame frameId heap in
  let slots = frameSlots frame in
  if (findSlot slots slotName) = None then
    let newSlots = Slot(slotName, newValue) :: slots in
    let newFrame = Frame(frameLinks frame, newSlots) in
    setFrame frameId newFrame heap
  else
    let slots = List.map (function
      | Slot(name, value) -> 
        if name = slotName then Slot(name, newValue) else Slot(name, value)) slots in
    let newFrame = Frame(frameLinks frame, slots) in
    setFrame frameId newFrame heap

let rec findLink links linkLabel = 
  match links with
  | [] -> None
  | Link(label, id) :: rest -> 
    if label = linkLabel then Some(id)
    else findLink rest label

let int_of_intoption = function None -> 0 | Some n -> n

let getLink frameId linkLabel heap = 
  let frame = getFrame frameId heap in
  let links = frameLinks frame in
  containsInt (findLink links linkLabel)

let createLink childFrameId parentFrameId linkLabel heap = 
  let frame = getFrame childFrameId heap in
  let links = frameLinks frame in
  let link = Link(linkLabel, parentFrameId) in
  let newLinks = link :: links in
  let newFrame = Frame(newLinks, frameSlots frame) in
  setFrame childFrameId newFrame heap

let rec lookup name frameId heap = 
  let frame = getFrame frameId heap in
  let slot = getSlotValue frameId name heap in
  let links = frameLinks frame in
  
  match slot with
    | None -> 
      if List.length links > 0 then 
        lookup name (getLink frameId "P" heap) heap
      else 
        let _ = Printf.printf "Error: %s not found in frame %d \n" name frameId in
        failwith "lookup: no slot"
    | Some v -> v

let rec lookupOrMinusOne name frameId heap = 
  let frame = getFrame frameId heap in
  let slot = getSlotValue frameId name heap in
  let links = frameLinks frame in
  
  match slot with
    | None -> 
      if List.length links > 0 then 
        lookupOrMinusOne name (getLink frameId "P" heap) heap
      else 
        -1 
    | Some v -> v

(* ------- STORE METHODS ---------- *)

let initStore =
  let store = Store.empty in
  store

let fetch loc sto = 
  try Store.find loc sto with Not_found -> (Printf.printf "Location %d not found\n" loc; failwith "NOT FOUND")
  
let updateCell loc v sto = 
  let newSto = Store.add loc v sto in
  newSto

let newLocation sto = 
  Store.cardinal sto

let extendStore v sto = 
  let newLoc = Store.cardinal sto in 
  let newSto = Store.add newLoc v sto in
  (newSto, newLoc)


(* ------- INTERPRETOR METHODS ---------- *)

let numPlus l r = 
  if isNumV(l) && isNumV(r) then
    NumV(getNumV(l) + getNumV(r))
  else
    failwith "One argument was not a number"

let numMult l r = 
  if isNumV(l) && isNumV(r) then
    NumV(getNumV(l) * getNumV(r))
  else
    failwith "One argument was not a number"

let ifc c t e = 
  match c with
    | BoolV(true) -> t
    | BoolV(false) -> e
    | _ -> failwith "If: invalid args"

let eqc l r = 
  if isNumV(l) && isNumV(r) then
    BoolV(getNumV(l) = getNumV(r))
  else
    failwith "Eq: invalid args"

let ltc l r = 
  if isNumV(l) && isNumV(r) then
    BoolV(getNumV(l) < getNumV(r))
  else
    failwith "Lt: invalid args"

let headc l = 
  match l with
    | ConsV(h, t) -> h
    | _ -> failwith "Head: invalid args"

let tailc l = 
  match l with
    | ConsV(h, t) -> t
    | _ -> failwith "Tail: invalid args"

let isheadc l = 
  match l with
    | ConsV(h, t) -> BoolV(true)
    | _ -> BoolV(false)

let istailc l = 
  match l with
    | ConsV(h, t) -> BoolV(false)
    | _ -> BoolV(true)

(* ------- INTERPRETOR ---------- *)

let heap = ref Heap.empty
let store: value Store.t ref = ref Store.empty

let rec interp expr frID = 
  (* Printf.fprintf oc "\t\t%s\n" (printValueStringExprC expr);
  Printf.fprintf oc "==========================\n";
  printHeapStore oc !heap !store; *)
  match expr with
  | TrueC -> BoolV true
  | FalseC -> BoolV false
  | NumC n -> NumV n
  | NilC -> NilV
  | UninitializedC -> NothingV
  | PlusC(left, right) -> numPlus (interp left frID) (interp right frID)
  | MultC(left, right) -> numMult (interp left frID) (interp right frID)
  | IfC(cond, thn, els) -> ifc (interp cond frID) (interp thn frID) (interp els frID)
  | EqNumC(left, right) -> eqc (interp left frID) (interp right frID)
  | LtC(left, right) -> ltc (interp left frID) (interp right frID)
  | ConsC(head, tail) -> ConsV((interp head frID), (interp tail frID))
  | HeadC cons -> headc (interp cons frID)
  | TailC cons -> tailc (interp cons frID)
  | IsNilC cons -> isheadc (interp cons frID)
  | IsListC cons -> istailc (interp cons frID)
  | IdC name -> 
      (let location = lookup name frID !heap in
      fetch location !store)
  | LamC(syms, e) -> ClosV(syms, e, frID)
  | BoxC(e) -> 
      (let ex = interp e frID in
       let newLoc = Store.cardinal !store in
       let newSto = Store.add newLoc ex !store in
       store := newSto;
       BoxV(newLoc)) 
  | UnboxC(box) -> 
      (let b = interp box frID in
      let v = fetch (getBoxLocation b) !store in
      v)
  | SetboxC(b, expr) -> 
      (let box = interp b frID in
      match box with 
      | BoxV loc ->
        let v = interp expr frID in
        store := updateCell loc v !store;
        v
      | _ -> failwith "Setbox: invalid args")
  (* Change related to Paret -> If name is not in environment it is created  *)
  | SetC(name, expr) -> 
      (
        (* Find otherwise create *)
        (* let loc = lookup name frID !heap in *)
        let loc = lookupOrMinusOne name frID !heap in
        let v = interp expr frID in
        if loc = -1 then
          let newLoc = newLocation !store in
          heap := updateSlotValue frID name newLoc !heap;
          store := updateCell newLoc v !store;
        else
          store := updateCell loc v !store;
        v
      )
  | SeqC(left, right) -> (
      let _ = interp left frID in
      interp right frID)
  | AppC(f, args) ->
      (match interp f frID with
        | ClosV(syms, expr, clFrID) -> 
            (* Update Heap and Store with list of interpreted args *)
            let (updatedHeap, newFrameID) = initFrame !heap in
            heap := createLink newFrameID clFrID "P" updatedHeap;

            (*  Interp args *)
            let processArgs argsym =
                (match argsym with
                | (arg, sym) ->  
                    let v = interp arg frID in
                    let newLoc = newLocation !store in 
                    heap := updateSlotValue newFrameID sym newLoc !heap;
                    store := updateCell newLoc v !store;
                    v) in

            let _ = List.map processArgs (List.combine args syms) in

            interp expr newFrameID
        | _ -> failwith "App: Error!")


(* ------------ REPL INITIALIZATION ----------- *)

(* http://www.codecodex.com/wiki/Generate_a_random_password_or_random_string#OCaml *)
let genereateString length =
  let gen() = match Random.int(52) with
    | n when n < 26 -> int_of_char 'a' + n
    | n when n < 52 -> int_of_char 'A' + n - 26 
    | _ -> failwith "Error generating random string - How did you get here?" in
  let gen _ = String.make 1 (char_of_int(gen())) in
  String.concat "" (Array.to_list (Array.init length gen))

let addToRepl repl cell = 
  repl :: [cell]

(* Delete everything after the appearance of the deletable cell *)
let deleteFromRepl repl cell = 
  let rec index_of cell = function
    | [] -> failwith "Cell not found"
    | head :: tail -> 
      if (rcell_name head = rcell_name cell) then 0
      else 1 + index_of cell tail
  in 
  let index = index_of cell repl in 
  let rec firstN n = function
    | [] -> failwith "N bigger than size of list"
    | head :: tail -> 
      if (n = 0) then [head]
      else head :: firstN (n - 1) tail
  in 
  firstN index repl

let rec rcellNames commands = 
    match commands with 
    | ExecuteCom rcell :: tail ->
        (rcell_name rcell) :: rcellNames tail
    | _ -> []

(* Create recursive FdC definition *)
let executeReplCommands commands = 
  let rec buildLamC commands =
    match commands with
    | ExecuteCom rcell :: [] -> 
        SetC ((rcell_name rcell), (rcell_contents rcell))
    | ExecuteCom rcell :: tail -> 
        SeqC (SetC ((rcell_name rcell), (rcell_contents rcell)), 
              buildLamC tail)        
    | _ -> UninitializedC
    in 
  let names = rcellNames commands in
  (* AppC ((LamC (names, (buildLamC commands))), (List.init (List.length names) (fun x -> UninitializedC))) *)
  buildLamC commands

(* Add more REPL features         *)
(* --- Deletion of cells          *)
(* --- Branching in REPL Tree     *)
(* --- Edit cell & change history *)


(* Blasfemous SetC:
  - It acts as a let for now
    - try to make a test for CELL1: int x = 5; CELL2: x 
    - Try to modify back to let to fix the heresy 
    - Which is the good one? 
        - buildLamC commands
        - AppC (LamC (names, buildLamC) Uninitialized) *)


(* ------- REPL TESTS ------------*)

let testREPL_1 = 
  let cell1 = RCell ((genereateString 5),
                    (PlusC (NumC 24, AppC( LamC( ["x"], NumC 5), [NumC 22])))) in 

  let cell2 = RCell ((genereateString 5),
                    (AppC( LamC( ["a"], AppC( LamC( ["b"], PlusC( IdC "a", IdC "b")), [NumC 4])), [NumC 3]))) in 
  
  let cell3 = RCell ((genereateString 5),
                    (AppC( LamC( ["a"; "b"], PlusC( IdC "a", IdC "b")), [NumC 3; NumC 4]))) in

  let replCommands = [
    ExecuteCom cell1;
    ExecuteCom cell2;
    ExecuteCom cell3
  ] in 
  executeReplCommands replCommands

let testREPL_2 = 
  let cell1 = RCell ((genereateString 5),
                    (SetC ("p", NumC 22))) in

  let cell2 = RCell ((genereateString 5),
                    IdC "p") in

  let replCommands = [
    ExecuteCom cell1;
    ExecuteCom cell2
  ] in
  executeReplCommands replCommands

let () = 
  let (hp, newID) = initFrame (Heap.empty) in
  store := Store.empty;
  heap := hp;
  let _ = interp testREPL_2 newID in
  printHeapStore oc !heap !store 

(* let () = 
  let (hp, newID) = initFrame (Heap.empty) in
  store := Store.empty;
  heap := hp;
  let input = AppC (
                LamC (["abc"; "def"], SeqC (SetC ("abc", SetC ("p", NumC 20)), SetC ("def", IdC "p"))),
                [UninitializedC; UninitializedC] ) in
  let result = interp input newID in
  Printf.fprintf oc "-----%s\n" (printValueString result);
  printHeapStore oc !heap !store  *)

(* let () = 
  let (hp, newID) = initFrame (Heap.empty) in
  store := Store.empty;
  heap := hp;
  let input = SeqC (SetC ("abc", SetC ("p", NumC 20)), SetC ("def", IdC "p")) in
  let result = interp input newID in
  Printf.fprintf oc "-----%s\n" (printValueString result);
  printHeapStore oc !heap !store  *)


 
(* ------------------------------ *)


   
(* OLD TESTS::: *)

(* ------------ TESTS ----------- *)

(* let noTest = ref 1

let test expected input = 
    let (hp, newID) = initFrame (Heap.empty) in
    store := Store.empty;
    heap := hp;
    Printf.fprintf oc "Result of test %d: \n\t" !noTest;
    let result = interp input newID in
    if (result = expected) then
        Printf.fprintf oc "PASS: "
    else
        Printf.fprintf oc "FAIL: ";
    printValueUnit oc result;
    Printf.fprintf oc "\n";
    printHeapStore oc !heap !store;
    noTest := !noTest + 1 

let () =
  test (NumV 29) (PlusC
            (NumC 24, 
            AppC( 
                LamC( 
                    ["x"], 
                    NumC 5), 
                [NumC 22])));

  test (NumV 7) (AppC( 
                LamC( 
                    ["a"], 
                    AppC(
                        LamC( 
                            ["b"], 
                            PlusC(
                                IdC "a", 
                                IdC "b")),
                        [NumC 4])),
                [NumC 3])); 

  test (NumV 7) (AppC( 
                LamC(  
                    ["a"; "b"], 
                    PlusC(
                        IdC "a", 
                        IdC "b")), 
                [NumC 3; NumC 4]));

  test (NumV 25) (AppC( 
                LamC( 
                    ["sq"], 
                    MultC(
                        NumC 5, 
                        NumC 5)), 
                [NumC 3]));

  test (NumV 9) (AppC( 
                LamC( 
                    ["sq"], 
                    AppC( 
                        LamC( 
                            ["x"], 
                            MultC(
                                IdC "x", 
                                IdC "x")),
                        [IdC "sq"])),
                [NumC 3]));

  test (NumV 4) (AppC(
                LamC(
                    ["a"], 
                    AppC(
                        LamC(
                            ["b"],
                            SeqC(
                                SetC(
                                    "a", 
                                    NumC 1),
                                MultC(
                                    IdC "a", 
                                    IdC "b")
                           )), 
                        [NumC 4])), 
                [NumC 3])); *)

(* ---------------------- *)

(* let noCell = ref 1

let rec printREPL l = 
  match l with
  | [] -> Printf.fprintf oc "\n"
  | head :: tail -> 
    Printf.fprintf oc "Result for cell %d: \n\t" !noCell;
    printValueUnit oc head;
    Printf.fprintf oc "\n";
    noCell := !noCell + 1;
    printREPL tail

let _ = 
  let cell1 = PlusC (NumC 24, AppC( LamC( ["x"], NumC 5), [NumC 22])) in 

  let cell2 =  AppC( 
                LamC( 
                    ["a"], 
                    AppC(
                        LamC( 
                            ["b"], 
                            PlusC(
                                IdC "a", 
                                IdC "b")),
                        [NumC 4])),
                [NumC 3]) in 
  
  let cell3 =  AppC( 
                LamC(  
                    ["a"; "b"], 
                    PlusC(
                        IdC "a", 
                        IdC "b")), 
                [NumC 3; NumC 4]) in
  
  let input = ReplC [cell1; cell2; cell3] in
  let (hp, newID) = initFrame (Heap.empty) in
  store := Store.empty;
  heap := hp;
  let result = interp input newID in
  printREPL (getReplV result);
  Printf.fprintf oc "\n------------\n";
  printHeapStore oc !heap !store *)


(* ---------------------- *)
