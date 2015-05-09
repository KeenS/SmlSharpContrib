structure Dynamic =
struct
datatype any = V of {
             disclose:   unit -> unit,
             undisclose: unit -> unit
         }
type t = any
type 'a key = ('a -> t) * (t -> 'a option)

fun mk () =
  let
      val box = ref NONE (* 通信チャネル *)
      fun discloser v () = box := SOME v
      fun undiscloser () = box := NONE
      fun mkV v = (* それぞれが box にアクセスするサンク *)
        V { disclose   = discloser v,
            undisclose = undiscloser }
      fun useV (V {disclose, undisclose}) =
        ( box := NONE;  (* mkV したときと同じ box にアクセスするなら *)
          disclose ();  (* 同じ box に v を書く *)
          let val v = !box (* すぐに取り出す *)
          in undisclose(); v end )
  in
      (mkV, useV)
  end

fun emb (f,_) = f
fun prj (_,f) = f
end
