structure FormatTest = struct
open SMLUnit
open Assert
open Format

fun suite _ = Test.labelTests [
      ("simple",
       fn () => assertEqualString "Hello, World" (format "Hello, World" [])),
      ("string args",
       fn () => assertEqualString "Hello, World" (format "%s, %s" [fromString "Hello", fromString "World"])),
      ("int args",
       fn () => assertEqualString "1 + 1 = 2" (format"%d + %d = %d" [fromInt 1, fromInt 1, fromInt 2])),
      ("char args",
       fn () => assertEqualString "The first letter of Hello is H" (format "The first letter of %s is %c" [fromString "Hello", fromChar (String.sub("Hello", 0))])),
      ("real args",
       fn () => assertEqualString "1.0 + 1.0 = 2.0" (format "%f + %f = %f" [fromReal 1.0, fromReal 1.0, fromReal 2.0]))
  ]

end
