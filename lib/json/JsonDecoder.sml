structure JsonDecoder = struct
  open Parser
  open JsonValue

  fun number input =
    let
      fun str s = pstring s input
      val positive = orElse (str "-") (return "")
      fun digit1_9 pos =
        if String.size input <= pos then NONE
        else
          let
            val i = String.substring (input, pos, 1)
          in
            case Char.fromString i of
              SOME c =>
                if Char.isDigit c andalso c <> #"0" then
                  SOME (Char.toString c, pos + 1)
                else NONE
              | NONE => NONE
          end
      fun digit pos =
        if String.size input <= pos then NONE
        else
          let
            val i = String.substring (input, pos, 1)
          in
            case Char.fromString i of
              SOME c =>
                if Char.isDigit c then
                  SOME (Char.toString c, pos + 1)
                else NONE
              | NONE => NONE
          end
      val digits = map String.concat (many digit)
      val pint =
        map (op ^)
          (andThen positive
            (orElse (str "0")
              (map (op ^)
                (andThen digit1_9 digits))))
      val frac = map (op ^) (andThen (str ".") digits)
      val e =
        map (op ^)
          (andThen
            (orElse (str "e") (str "E"))
            (orElse (orElse (str "-") (str "+")) (str "")))
      val exp = map (op ^) (andThen e digits)
      val fracExp = map (op ^) (andThen frac exp)
      val pnum =
        map (op ^)
          (andThen pint
            (orElse fracExp
            (orElse frac
            (orElse exp
            (fn pos => SOME("", pos))))))
    in
      mapPartial (fn n =>
        fn pos =>
          case Real.fromString n of
            SOME r => SOME(JsonNumber r, pos)
            | NONE => NONE) pnum
    end

  fun jstr input =
    let
      fun str s = pstring s input
      fun hex pos =
        if String.size input <= pos then NONE
        else
          let
            val i = String.substring (input, pos, 1)
          in
            case Char.fromString i of
              SOME c =>
                if Char.isHexDigit c then
                  SOME (Char.toString c, pos + 1)
                else NONE
              | NONE => NONE
          end
      val escChar =
        orElse (str "\\\"")
        (orElse (str "\\\\")
        (orElse (str "\\/")
        (orElse (map (fn _ => "\b") (str "\\b"))
        (orElse (map (fn _ => "\f") (str "\\f"))
        (orElse (map (fn _ => "\n") (str "\\n"))
        (orElse (map (fn _ => "\r") (str "\\r"))
        (orElse (map (fn _ => "\t") (str "\\t"))
        (map (fn (u, (h1, (h2, (h3, h4)))) => u ^ h1 ^ h2 ^ h3 ^ h4)
          (andThen (str "u")
            (andThen hex
              (andThen hex (andThen hex hex))))))))))))
      val jchar =
        orElse escChar (fn pos =>
          if String.size input <= pos then NONE
          else
            let
              val i = String.substring (input, pos, 1)
            in
              case Char.fromString i of
                SOME c =>
                  if c <> #"\\" andalso c <> #"\"" then
                    SOME (Char.toString c, pos + 1)
                  else NONE
                | NONE => NONE
            end)
      val internalString = map String.concat (many jchar)
      val jstring =
        mapPartial (fn _ =>
          mapPartial (fn x =>
            map (fn _ => x) (str "\"")) internalString) (str "\"")
    in
      jstring
    end

  fun decode input =
    let
      fun str s = pstring s input
      fun space pos =
        if String.size input <= pos then NONE
        else
          let
            val i = String.substring (input, pos, 1)
          in
            case Char.fromString i of
              SOME c =>
                if Char.isSpace c then
                  SOME (Char.toString c, pos + 1)
                else NONE
              | NONE => NONE
          end
      val spaces = many space
      fun ws p = mapPartial (fn _ => p) spaces
      val jnull = map (fn _ => JsonNull) (str "null")
      val jfalse = map (fn _ => JsonBool false) (str "false")
      val jtrue = map (fn _ => JsonBool true) (str "true")
      val jbool = orElse jtrue jfalse
      val jnumber = number input
      val jstring = map JsonString (jstr input)
      fun elements p =
        map (fn (x, xs) => x::xs)
          (andThen (ws p)
            (many (ws (mapPartial (fn _ => ws p) (str ",")))))
      fun containerBetweenStrings pOpen pClose pElem f =
        mapPartial (fn _ =>
          mapPartial (fn xs =>
            map (fn _ => f xs) (ws (str pClose)))
            (orElse (elements (pElem ()))
              (return []))) (str pOpen)
      fun jarray () = containerBetweenStrings "[" "]" jvalue JsonArray
      and
        pair () =
        ws (mapPartial (fn k =>
          ws (mapPartial (fn _ =>
            ws (map (fn v => (k, v)) (jvalue ())))
              (str ":"))) (jstr input))
      and
        jobject () = containerBetweenStrings "{" "}" pair JsonObject
      and
        jvalue () =
        orElse jstring
        (orElse jnumber
        (orElse (jobject ())
        (orElse (jarray ())
        (orElse jbool jnull))))
      val json =
        ws (mapPartial (fn x => ws (return x))
          (orElse (jobject ()) (jarray ())))
    in
      case parse json input of
        SOME (result, _) => SOME result
        | NONE => NONE
    end
end

