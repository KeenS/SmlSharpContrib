structure Format =
struct

structure D = Dynamic
structure M = Map(struct open Char
                         type t = char
                  end)
structure S = StringCvt

val intKey: int D.key = D.mk()
val fromInt = D.emb intKey
val getInt = Option.valOf o (D.prj intKey)

val realKey: real D.key = D.mk()
val fromReal = D.emb realKey
val getReal = Option.valOf o (D.prj realKey)

val charKey: char D.key = D.mk()
val fromChar = D.emb charKey
val getChar = Option.valOf o (D.prj charKey)

val stringKey: string D.key = D.mk()
val fromString = D.emb stringKey
val getString = Option.valOf o (D.prj stringKey)

datatype formatter
  = Const of string
  | FormatFun of D.t -> string

type t = (Dynamic.t -> string) M.t

fun formatWidth str {leftAlign, padWithZero, ...} (SOME(width)) =
  let
      val char = if padWithZero then #"0" else #" "
  in
      if leftAlign
      then S.padRight char width str
      else S.padLeft char width str
  end
  | formatWidth str _ _ = str

fun formatString flags precision width arg =
  let
      val string = getString arg
  in
      formatWidth (String.extract(string, 0, precision)) flags width
  end

fun formatInt radix (flags as {addPlus, addBrank, printRadix, ...}) precision width arg =
  let
      val int = getInt arg
      val str = (String.extract(Int.fmt radix (abs int), 0, precision))
      val rad = if printRadix
                then case radix of
                            S.DEC => ""
                          | S.HEX => "0x"
                          | _   => ""
                else ""
      val prefix = if int < 0
                   then "-"
                   else if addPlus
                               (* addPlus overrides addBrank *)
                   then "+"
                   else if addBrank
                   then " "
                   else ""

  in
      formatWidth (String.extract(prefix ^ rad ^ str, 0, precision)) flags width
  end

val formatDigit = formatInt S.DEC
val formatHex   = formatInt S.HEX

fun formatChar flags precision width arg =
  let
      val char = getChar arg
  in
      formatWidth (Char.toString char) flags width
  end
fun formatReal flags precision width arg =
  let
      val real = getReal arg
  in
      formatWidth (Real.toString real) flags width
  end


val default = ((M.add #"s" formatString)
               o (M.add #"d" formatDigit)
               o (M.add #"x" formatHex)
               o (M.add #"c" formatChar)
               o (M.add #"f" formatReal))
                  M.empty
                  

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
      fun parseSpecifier flags precision width i =
        case M.find (String.sub(str, i)) t of
            SOME(formatter) => (FormatFun(formatter flags precision width), i + 1)
          | NONE => raise Fail("invalid formatter: " ^ (Char.toString (String.sub(str, i))))
      fun parseWidth flags precision i =
        case position str (not o Char.isDigit) i of
            SOME(e) => parseSpecifier flags precision (Int.fromString (String.substring(str, i, e - i))) e
          | NONE => parseSpecifier flags precision NONE i
      fun parsePrecision flags i = parseWidth flags NONE i
      fun parseFlag flags i = case String.sub(str, i) of
                            #"-" => parseFlag (flags # {leftAlign = true})   (i + 1)
                          | #"+" => parseFlag (flags # {addPlus = true} )    (i + 1)
                          | #" " => parseFlag (flags # {addBrank = true})    (i + 1)
                          | #"#" => parseFlag (flags # {printRadix = true})  (i + 1)
                          | #"0" => parseFlag (flags # {padWithZero = true}) (i + 1)
                          | _ =>    parsePrecision flags i
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

val parseDefault = parse default


fun format str args =
  let
      fun aux (Const(t)::ts) args acc = aux ts args (t::acc)
        | aux (FormatFun(f)::ts) (arg::args) acc = 
          aux ts args ((f arg) :: acc)
        | aux [] [] acc = String.concat(List.rev acc)
        | aux _ [] _ = raise (Fail "args too short")
        | aux [] _ _ = raise (Fail "args too long")
      val t = parseDefault str
  in
      aux t args []
  end
end

