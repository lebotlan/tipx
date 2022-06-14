open Pcre

let error2s = function
  | Partial -> "[Partial] String only matched the pattern partially"
  | BadPartial -> "[BadPartial] Pattern contains items that cannot be used together with partial matching."
  | BadPattern (msg, _) -> "[BadPattern] expression is malformed : " ^ msg
  | BadUTF8 -> "[BadUTF8]"
  | BadUTF8Offset -> "[BadUTF8Offset] a UTF8 string being matched with offset is invalid."
  | MatchLimit -> "[MatchLimit] Maximum allowed number of match attempts with backtracking or recursion is reached during matching."
  | RecursionLimit -> "[RecursionLimit]"
  | WorkspaceSize -> "[WorkspaceSize] Raised by {!pcre_dfa_exec} when the provided workspace array is too small."
  | InternalError s -> "[InternalError] " ^ s

let () = Printexc.register_printer
    begin function
      | Error err -> Some ("Pcre.Error: " ^ error2s err)
      | _ -> None
    end

(* Force module loading. *)
let init = ()
           
