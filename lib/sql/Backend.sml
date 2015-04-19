(* structure SQLBackendTypes = *)
(* struct *)

(*   type schema_column = SMLSharp_SQL_Backend.schema_column *)

(*   type schema_table = SMLSharp_SQL_Backend.schema_table *)
(*   type schema =  SMLSharp_SQL_Backend.schema *)

(*   datatype res_impl = datatype  *)
         
(*   type conn_impl = SMLSharp_SQL_Backend.conn_impl *)

(*   type server_impl = SMLSharp_SQL_Backend.server_impl *)


(*   datatype backend = datatype  *)

(* end *)

functor Backend(B : SMLSharp_SQL_SQLBACKEND) =
struct
  local

    fun getValue convFn x =
        case B.getValue x of
          NONE => NONE
        | SOME x =>
          case convFn x of
            NONE => raise SMLSharp_SQL_Errors.Format
          | SOME x => SOME x

    fun resImpl res =
        SMLSharp_SQL_Backend.R {
          closeRel = fn () => B.closeRel res,
          fetch = fn () => Option.map resImpl (B.fetch res),
          getInt = fn i => getValue B.intValue (res, i),
          getIntInf = fn i => getValue B.intInfValue (res, i),
          getWord = fn i => getValue B.wordValue (res, i),
          getReal = fn i => getValue B.realValue (res, i),
          getString = fn i => getValue B.stringValue (res, i),
          getChar = fn i => getValue B.charValue (res, i),
          getBool = fn i => getValue B.boolValue (res, i),
          getTimestamp = fn i => getValue B.timestampValue (res, i),
          getDecimal = fn i => getValue B.decimalValue (res, i),
          getFloat = fn i => getValue B.floatValue (res, i)
        }

    fun execQuery conn query =
        resImpl (B.execQuery (conn, query))

    fun connect serverDesc () =
        let
          val conn = B.connect serverDesc
        in
          {
            closeConn = fn () => B.closeConn conn,
            getDatabaseSchema = fn () => B.getDatabaseSchema conn,
            execQuery = execQuery conn
          }
        end

    fun prepare serverDesc =
        {
          connect = connect serverDesc
        }

  in

  fun backend serverDesc =
      SMLSharp_SQL_Backend.BACKEND (prepare serverDesc)

  end (* local *)
end
