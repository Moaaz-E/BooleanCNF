let validVarNames = [
  "a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z";
  "A";"B";"C";"D";"E";"F";"G";"H";"I";"J";"K";"L";"M";"N";"O";"P";"Q";"R";"S";"T";"U";"V";"W";"X";"Y";"Z";
];;
let validLiterals = ["TRUE"; "FALSE"];;

let partition (input: string list) (bound : string) : string list list =
  let rec partitionHelper (input : string list) (bound : string) (cur : string list) (acc: string list list) : string list list =
    if List.length input = 0 then
      List.append acc [cur]
    else match List.hd input  with
    | b when b = bound -> partitionHelper (List.tl input) bound [] (List.append acc [cur])
    | _ -> partitionHelper (List.tl input) bound (List.append cur [List.hd input]) acc
  in
  partitionHelper input bound [] []
;;

let getVariables (input : string list) : string list =
  let rec getVariablesHelper (input : string list) (acc : string list) : string list =
    match input with
    | [] -> acc
    | h :: t -> if List.mem h validVarNames && not (List.mem h acc) then  getVariablesHelper t (List.append acc [h]) else getVariablesHelper t acc
  in
  getVariablesHelper input []
;;

let buildCNF (input : string list) : (string * string) list list =
  let segments = partition input "AND" in
  let vars = getVariables input in
  let rec buildCNFPartial (segment: string list) (acc : (string * string) list) (isNegated : bool) : (string * string) list =
    match segment with
    | [] -> acc
    | h :: t when List.mem h vars          -> if isNegated then buildCNFPartial t (List.append acc [(h, "NOT")]) false else buildCNFPartial t (List.append acc [(h, "")]) false
    | h :: t when List.mem h validLiterals -> if isNegated then buildCNFPartial t (List.append acc [(h, "NOT")]) false else buildCNFPartial t (List.append acc [(h, "")]) false
    | h :: t when h = "NOT" -> buildCNFPartial t acc true
    | _ :: t -> buildCNFPartial t acc isNegated
  in
  let rec buildCNFHelper (segments : string list list) (acc : (string * string) list list) : (string * string) list list =
    if List.length segments = 0 then acc
    else buildCNFHelper (List.tl segments) (List.append acc [(buildCNFPartial (List.hd segments) [] false)])
  in
  buildCNFHelper segments []
;;


let generateDefaultAssignments (varList : string list) : (string * bool) list =
  let rec generateDefaultAssignmentsHelper (varList : string list) (acc : (string * bool) list) : (string * bool) list =
    if List.length varList = 0 then acc
    else generateDefaultAssignmentsHelper (List.tl varList) (List.append acc [(List.hd varList, false)])
  in
  generateDefaultAssignmentsHelper varList []
;;

let generateNextAssignments (assignList : (string * bool) list) : (string * bool) list * bool =
  let rec generateNextAssignmentsHelper (assignList : (string * bool) list) (acc : (string * bool) list) (cur : bool) : (string * bool) list * bool =
    if List.length assignList = 0 then (List.rev acc, cur)
    else match (List.hd assignList, cur) with
    | ((h, false), true) -> generateNextAssignmentsHelper (List.tl assignList) (acc @ [(h, true)]) false
    | ((h, true) , true) -> generateNextAssignmentsHelper (List.tl assignList) (List.append acc [(h, false)]) true
    | ((h, c), _) -> generateNextAssignmentsHelper (List.tl assignList) (List.append acc [(h, c)]) false
  in
  generateNextAssignmentsHelper (List.rev assignList) [] true
;;

let lookupVar (assignList : (string * bool) list) (str : string) : bool =
  let rec lookupVarHelper (assignList : (string * bool) list) (str : string) : bool =
    match assignList with
    | [] -> false
    | (h, b) :: t -> if h = str then b else lookupVarHelper t str
  in
  lookupVarHelper assignList str
;;

let evaluateCNF (t : (string * string) list list) (assignList : (string * bool) list) : bool =
  let literalToBool (s : string) : bool = if s = "TRUE" then true else false in
  let rec evaluateCNFPartial (t : (string * string) list) (assignList : (string * bool) list) (cur : bool) : bool =
    match t with
    | [] -> cur
    | (h, s) :: t when List.mem h validVarNames -> if s = "NOT" then evaluateCNFPartial t assignList cur || (not (lookupVar assignList h)) else evaluateCNFPartial t assignList cur || (lookupVar assignList h)
    | (h, s) :: t  -> if s = "NOT" then evaluateCNFPartial t assignList cur || (not (literalToBool h)) else evaluateCNFPartial t assignList cur || (literalToBool h)
  in
  let rec evaluateCNFHelper (t : (string * string) list list) (assignList : (string * bool) list) : bool =
    if List.length t = 0 then true
    else (evaluateCNFPartial (List.hd t) assignList false) && evaluateCNFHelper (List.tl t) assignList
  in
  evaluateCNFHelper t assignList
;;

let satisfy (input : string list) : (string * bool) list =
  let cnf = buildCNF input in
  let default = (generateDefaultAssignments (getVariables input), false) in
  let rec satisfyHelper (cnf : (string * string) list list) (permut : (string * bool) list * bool) : (string * bool) list =
    match permut with
    | (_, b) when b = true -> [("error", false)]
    | (arr, _) -> if evaluateCNF cnf arr then arr else satisfyHelper cnf (generateNextAssignments arr)
  in
  satisfyHelper cnf default
;;