structure Format =
struct

structure D = Dynamic
structure P = FormatParser
structure M = CharMap

local
    open Formatter
in
val default = ((M.add #"s" formatString)
               o (M.add #"d" formatDigit)
               o (M.add #"x" formatHex)
               o (M.add #"c" formatChar)
               o (M.add #"f" formatReal))
                  M.empty
end                


val parseDefault = P.parse default


fun format str args =
  let
      fun aux (P.Const(t)::ts) args acc = aux ts args (t::acc)
        | aux (P.FormatFun(f)::ts) (arg::args) acc = 
          aux ts args ((f arg) :: acc)
        | aux [] [] acc = String.concat(List.rev acc)
        | aux _ [] _ = raise (Fail "args too short")
        | aux [] _ _ = raise (Fail "args too long")
      val t = parseDefault str
  in
      aux t args []
  end

fun printf str args = print (format str args)

end

