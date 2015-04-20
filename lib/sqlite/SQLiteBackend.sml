structure SQLiteBackend  :> SMLSharp_SQL_SQLBACKEND =
struct
open SQLite

exception Exec = SMLSharp_SQL_Errors.Exec
exception Connect = SMLSharp_SQL_Errors.Connect
exception Format = SMLSharp_SQL_Errors.Format

type conn = sqlite3
type res = sqlite3_stmt
type value = (sqlite3_stmt * int)

val SQLITE_OK         =  0   (* Successful result *)
(* beginning-of-error-codes *)
val SQLITE_ERROR      =  1   (* SQL error or missing database *)
val SQLITE_INTERNAL   =  2   (* Internal logic error in SQLite *)
val SQLITE_PERM       =  3   (* Access permission denied *)
val SQLITE_ABORT      =  4   (* Callback routine requested an abort *)
val SQLITE_BUSY       =  5   (* The database file is locked *)
val SQLITE_LOCKED     =  6   (* A table in the database is locked *)
val SQLITE_NOMEM      =  7   (* A malloc() failed *)
val SQLITE_READONLY   =  8   (* Attempt to write a readonly database *)
val SQLITE_INTERRUPT  =  9   (* Operation terminated by sqlite3_interrupt()*)
val SQLITE_IOERR      = 10   (* Some kind of disk I/O error occurred *)
val SQLITE_CORRUPT    = 11   (* The database disk image is malformed *)
val SQLITE_NOTFOUND   = 12   (* Unknown opcode in sqlite3_file_control() *)
val SQLITE_FULL       = 13   (* Insertion failed because database is full *)
val SQLITE_CANTOPEN   = 14   (* Unable to open the database file *)
val SQLITE_PROTOCOL   = 15   (* Database lock protocol error *)
val SQLITE_EMPTY      = 16   (* Database is empty *)
val SQLITE_SCHEMA     = 17   (* The database schema changed *)
val SQLITE_TOOBIG     = 18   (* String or BLOB exceeds size limit *)
val SQLITE_CONSTRAINT = 19   (* Abort due to constraint violation *)
val SQLITE_MISMATCH   = 20   (* Data type mismatch *)
val SQLITE_MISUSE     = 21   (* Library used incorrectly *)
val SQLITE_NOLFS      = 22   (* Uses OS features not supported on host *)
val SQLITE_AUTH       = 23   (* Authorization denied *)
val SQLITE_FORMAT     = 24   (* Auxiliary database format error *)
val SQLITE_RANGE      = 25   (* 2nd parameter to sqlite3_bind out of range *)
val SQLITE_NOTADB     = 26   (* File opened that is not a database file *)
val SQLITE_NOTICE     = 27   (* Notifications from sqlite3_log() *)
val SQLITE_WARNING    = 28   (* Warnings from sqlite3_log() *)
val SQLITE_ROW        = 100  (* sqlite3_step() has another row ready *)
val SQLITE_DONE       = 101  (* sqlite3_step() has finished executing *)

fun compileQuery (conn, sql) =
  let
      val stmt = ref _NULL: sqlite3_stmt ref
      val ret = sqlite3_prepare()(conn, sql, String.size(sql), stmt, _NULL)
  in
      if ret = SQLITE_OK
      then !stmt
      else (print (sql ^ "\n");raise Format)
  end


fun execQuery (conn, sql) =
  let
      val stmt = compileQuery(conn, sql)
      (* val res =  sqlite3_step()(stmt) *)
  in
      (* if res = SQLITE_DONE *)
      (* then stmt *)
      (* else stmt *)
      stmt
  end

fun closeConn conn =
  case sqlite3_close()(conn) of
      _ => ()

fun closeRel stmt = sqlite3_finalize()(stmt)

fun connect str =
  let
      val sqlite_ref = ref _NULL : sqlite3 ref
  in
      if sqlite3_open()(str, sqlite_ref) = 0
      then !sqlite_ref
      else raise Connect str
  end

fun fetch res =
  let
      val ret = sqlite3_step()(res)
  in
      (* to use SQLITE_XXX variable, you cannot use `case .. of ...` *)
      if  SQLITE_ROW = ret
      then SOME(res)
      else if SQLITE_DONE = ret
      then NONE
      else if SQLITE_BUSY = ret
      then fetch(res)
      else if SQLITE_ERROR = ret
      then raise Exec("an error")
      else raise Exec("an error")
  end

fun getValue (stmt, i) = SOME(stmt, i)
fun intValue (stmt, i) = SOME(sqlite3_column_int()(stmt, i))
fun intInfValue value =  raise Fail "SQLite does'nt support intInf"
fun wordValue value = raise Fail "SQLite does'nt support word"
fun realValue (stmt, i) = SOME(sqlite3_column_double()(stmt, i))
fun stringValue (stmt, i) = let val str = sqlite3_column_text()(stmt, i) in
                                if Pointer.isNull str
                                then NONE
                                else SOME(Pointer.importString(SMLSharp_Builtin.Pointer.fromUnitPtr(str)))
                            end
fun charValue value = raise Fail "SQLite does'nt support char"
fun boolValue value = raise Fail "SQLite does'nt support boolean"
fun timestampValue value = raise Fail "SQLite does'nt support timestamp"
fun decimalValue value = raise Fail "SQLite does'nt support decimal"
fun floatValue value = raise Fail "SQLite does'nt support float"

fun getDatabaseSchema conn =
  let
      val res = execQuery(conn, "select * from sqlite_master")
      fun loop (SOME(res)) = loop(fetch(res))
        | loop NONE = ()
  in
      [("tbl_test", [{colname = "id", nullable = false, ty = SMLSharp_SQL_BackendTy.INT},
                     {colname = "name", nullable = false, ty = SMLSharp_SQL_BackendTy.STRING},
                     {colname = "weight", nullable = false, ty = SMLSharp_SQL_BackendTy.REAL}])]
  end


end
