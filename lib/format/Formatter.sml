structure Formatter =
struct

structure D = Dynamic
structure S = StringCvt

type t =  {leftAlign:bool, addPlus:bool, addBrank:bool, printRadix:bool, padWithZero:bool}
          -> int option -> int option -> Dynamic.t -> string


fun getOrFail msg opt = case opt of
                            SOME(v) => v
                          | NONE => raise (Fail ("expected " ^ msg ^ " but got other."))

val getString = getOrFail "string" o D.getString
val getInt = getOrFail "int" o D.getInt
val getReal = getOrFail "real" o D.getReal
val getChar = getOrFail "char" o D.getChar


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
      val string' =  getString arg
      val string = String.extract(string', 0, precision)
                   handle Subscript => string'
  in
      formatWidth string flags width
  end

fun formatInt radix (flags as {addPlus, addBrank, printRadix, ...}) precision width arg =
  let
      val int = getInt arg
      val str' = Int.fmt radix (abs int)
      val str = String.extract(str', 0, precision)
                handle Subscript => formatWidth str' {padWithZero = true, leftAlign = false} precision
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
      formatWidth (prefix ^ rad ^ str) flags width
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


end
