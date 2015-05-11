structure FormatTest = struct
open SMLUnit
open Assert
open Format
open Dynamic

fun suite _ = Test.labelTests [
      ("simple",
       fn () => assertEqualString "Hello, World" (format "Hello, World" [])),
      ("%s",
       fn () => assertEqualString "Hello, World" (format "%s, %s" [fromString "Hello", fromString "World"])),
      ("%10s",
       fn () => assertEqualString "     Hello" (format "%10s" [fromString "Hello"])),
      ("%1s",
       fn () => assertEqualString "Hello" (format "%1s" [fromString "Hello"])),
      ("%-10s",
       fn () => assertEqualString "Hello     " (format "%-10s" [fromString "Hello"])),
      ("%010s",
       fn () => assertEqualString "00000Hello" (format "%010s" [fromString "Hello"])),
      ("%.3s",
       fn () => assertEqualString "Hel" (format "%.3s" [fromString "Hello"])),
      ("%10.3s",
       fn () => assertEqualString "       Hel" (format "%10.3s" [fromString "Hello"])),
      ("%d",
       fn () => assertEqualString "1 + 1 = 2" (format"%d + %d = %d" [fromInt 1, fromInt 1, fromInt 2])),
      ("%3d",
       fn () => assertEqualString "  1" (format"%3d" [fromInt 1])),
      ("%1d",
       fn () => assertEqualString "100" (format"%1d" [fromInt 100])),
      ("%-3d",
       fn () => assertEqualString "1  " (format"%-3d" [fromInt 1])),
      ("%+d",
       fn () => assertEqualString "+1" (format"%+d" [fromInt 1])),
      ("%+d",
       fn () => assertEqualString "-1" (format"%+d" [fromInt ~1])),
      ("% d",
       fn () => assertEqualString " 1" (format"% d" [fromInt 1])),
      ("% d",
       fn () => assertEqualString "-1" (format"% d" [fromInt ~1])),
      ("%03d",
       fn () => assertEqualString "001" (format"%03d" [fromInt 1])),
      ("%.3d",
       fn () => assertEqualString "123" (format"%.3d" [fromInt 1234])),
      ("%-03d",
       fn () => assertEqualString "100" (format"%-03d" [fromInt 1])),
      ("%#d",
       fn () => assertEqualString "1" (format"%#d" [fromInt 1])),
      ("char args",
       fn () => assertEqualString "The first letter of Hello is H" (format "The first letter of %s is %c" [fromString "Hello", fromChar (String.sub("Hello", 0))])),
      ("real args",
       fn () => assertEqualString "1.0 + 1.0 = 2.0" (format "%f + %f = %f" [fromReal 1.0, fromReal 1.0, fromReal 2.0]))
  ]

end
