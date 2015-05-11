structure FormatParser =
struct

structure M = CharMap

datatype t
  = Const of string
  | FormatFun of Dynamic.t -> string

fun position str pred i =
  let
      fun loop i = (if pred (String.sub(str, i))
                    then SOME i
                    else loop (i + 1))
                   handle Subscript => NONE
  in
      loop i
  end


fun parsePercent t str i =
  let
      fun parseSpecifier flags width precision i =
        case M.find (String.sub(str, i)) t of
            SOME(formatter) => (FormatFun(formatter flags precision width), i + 1)
          | NONE => raise Fail("invalid formatter: " ^ (Char.toString (String.sub(str, i))))

      (* unimplemented *)
      fun parseLength flags width precision i = parseSpecifier flags width precision i

      fun parsePrecision flags width i = if String.sub(str, i) = #"."
                                         then case position str (not o Char.isDigit) (i+1) of
                                                  SOME(e) => parseSpecifier flags width (Int.fromString (String.substring(str, (i+1), e - i))) e
                                                | NONE => parseSpecifier flags width NONE (i+1)
                                         else parseSpecifier flags width NONE i

      fun parseWidth flags i =
        case position str (not o Char.isDigit) i of
            SOME(e) => parsePrecision flags (Int.fromString (String.substring(str, i, e - i))) e
          | NONE => parsePrecision flags NONE i

      fun parseFlag flags i = case String.sub(str, i) of
                                  #"-" => parseFlag (flags # {leftAlign = true})   (i + 1)
                                | #"+" => parseFlag (flags # {addPlus = true} )    (i + 1)
                                | #" " => parseFlag (flags # {addBrank = true})    (i + 1)
                                | #"#" => parseFlag (flags # {printRadix = true})  (i + 1)
                                | #"0" => parseFlag (flags # {padWithZero = true}) (i + 1)
                                | _ =>    parseWidth flags i
  in
      parseFlag {leftAlign = false,
                 addPlus = false,
                 addBrank = false,
                 printRadix = false,
                 padWithZero = false} i
  end

fun parse t str =
  let
      fun loop i acc =
        let
            val j = position str (fn c => c = #"%") i
        in
            case j of
                SOME(j) => let
                 val (arg, k) = parsePercent t  str (j+1)
             in
                 loop k (arg :: Const(String.extract(str, i, SOME(j - i))) :: acc)
             end
              | NONE => List.rev ((Const (String.extract(str, i, NONE))) :: acc)
        end
  in
      loop 0 []
  end

end
