
structure SQLiteTest = struct
  open SMLUnit
  open Assert
  structure S = Backend(SQLiteBackend)


  val assertEqualResult = assertEqualList(assertEqual2Tuple(assertEqualString, assertEqualReal))


  val sqlite = S.backend
  val server = _sqlserver(sqlite("test/sqlite/test.db")) : {tbl_test: {id: int, name: string, weight: real}}
  val conn = SQL.connect server
  (* val _ = _sqlexec (_sql db => delete from #db.myTable) conn *)
  val getAll = (_sql db =>select #T.name as 1, #T.weight as 2 from #db.tbl_test as T)

  fun suite _ = Test.labelTests [
        ("Null",
         fn () => let
             val res = _sqleval getAll conn
         in
             assertEqualResult [] (SQL.fetchAll res);
             SQL.closeRel res;
             ()
         end),
        ("Insert",
         fn () => let
             val _ = _sqlexec (_sql db => insert into #db.tbl_test(id, name, weight) values (0, "Asai", 60.0)) conn
             val res = _sqleval getAll conn
         in
             assertEqualResult [("Asai", 60.0)]  (SQL.fetchAll(res));
             SQL.closeRel res
         end),
        ("Update",
         fn () => let
             val _ = _sqlexec (_sql db => update #db.tbl_test as T set weight = SQL.+(#T.weight, 2.1)
                                                                                     where SQL.==(#T.id, SQL.toSQL 0)) conn
             val res = _sqleval getAll conn
         in
             assertEqualResult [("Asai", 62.1)]  (SQL.fetchAll(res));
             SQL.closeRel res
         end
        )
            
  ]
end

