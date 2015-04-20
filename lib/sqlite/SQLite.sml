structure SQLite =
struct

type sqlite3  = unit ptr
type sqlite3_stmt = unit ptr
type char_ptr = unit ptr
type stringArray = (char_ptr ptr * int)
fun lazy f =
  let
      val r = ref NONE
  in
      fn () =>
         case !r of
             SOME x => x
           | NONE =>
             let val x = f ()
             in r := SOME x; x
             end
  end

val lib =
    lazy (fn _ => DynamicLink.dlopen "/home/kim/Sml/SmlSharpContrib/libsqlite3.so")

fun find s = DynamicLink.dlsym(lib (), s)


val sqlite3_open =
    lazy(fn _ => find "sqlite3_open"
                 :_import (string, sqlite3 ref) -> int)
val sqlite3_close =
    lazy(fn _ => find "sqlite3_close"
                 :_import (sqlite3) -> int)

val sqlite3_exec =
    lazy(fn _ => find "sqlite3_exec"
                 :_import (sqlite3, string, (unit ptr, int, char_ptr ptr, char_ptr ptr) -> int , unit ptr , string ref) -> int)

val sqlite3_prepare =
    lazy(fn _ => find "sqlite3_prepare"
                 :_import (sqlite3, string, int, sqlite3_stmt ref, unit ptr) -> int)
val sqlite3_step =
    lazy(fn _ => find "sqlite3_step"
                 :_import (sqlite3_stmt) -> int)
val sqlite3_bind_int =
    lazy(fn _ => find "sqlite3_bind_int"
                 :_import (sqlite3_stmt, int, int, int, (unit ptr)->unit ptr) -> int)
val sqlite3_bind_text =
    lazy(fn _ => find "sqlite3_bind_text"
                 :_import (sqlite3_stmt, int, string, int, (unit ptr)->unit ptr) -> int)
val sqlite3_column_int =
    lazy(fn _ => find "sqlite3_column_int"
                 :_import (sqlite3_stmt, int) -> int)
val sqlite3_column_text =
    lazy(fn _ => find "sqlite3_column_text"
                 :_import (sqlite3_stmt, int) -> unit ptr)
val sqlite3_column_double =
    lazy(fn _ => find "sqlite3_column_double"
                 :_import (sqlite3_stmt, int) -> real)
val sqlite3_finalize =
    lazy(fn _ => find "sqlite3_finalize"
                 :_import (sqlite3_stmt) -> ())
end
