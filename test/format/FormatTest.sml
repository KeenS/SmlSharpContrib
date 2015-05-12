structure FormatTest = struct
open SMLUnit
open Assert
open Format
open Dynamic

fun suite _ = Test.labelTests [
      ("%s: simple",
       fn () => assertEqualString "Hello, World" (format "%s, %s" [fromString "Hello", fromString "World"])),
      ("%10s: with width",
       fn () => assertEqualString "     Hello" (format "%10s" [fromString "Hello"])),
      ("%1s: with shorter width than string",
       fn () => assertEqualString "Hello" (format "%1s" [fromString "Hello"])),
      ("%-10s: width with left padding flag",
       fn () => assertEqualString "Hello     " (format "%-10s" [fromString "Hello"])),
      ("%010s: width with 0 fill flag",
       fn () => assertEqualString "00000Hello" (format "%010s" [fromString "Hello"])),
      ("%+s: plus flag (meaningless)",
       fn () => assertEqualString "Hello" (format "%+s" [fromString "Hello"])),
      ("%#s: # flag (meaningless)",
       fn () => assertEqualString "Hello" (format "%#s" [fromString "Hello"])),
      ("% s: space flag (meaningless)",
       fn () => assertEqualString "Hello" (format "% s" [fromString "Hello"])),
      ("%.3s: with precision",
       fn () => assertEqualString "Hel" (format "%.3s" [fromString "Hello"])),
      ("%.10s: with precision larger than string length",
       fn () => assertEqualString "Hello" (format "%.10s" [fromString "Hello"])),

      ("%d: simple",
       fn () => assertEqualString "1 + 1 = 2" (format"%d + %d = %d" [fromInt 1, fromInt 1, fromInt 2])),
      ("%3d: with width",
       fn () => assertEqualString "  1" (format"%3d" [fromInt 1])),
      ("%1d: with shorter width than printed number",
       fn () => assertEqualString "100" (format"%1d" [fromInt 100])),
      ("%-3d: width with left padding flag",
       fn () => assertEqualString "1  " (format"%-3d" [fromInt 1])),
      ("%03d: width with 0 fill flag",
       fn () => assertEqualString "001" (format"%03d" [fromInt 1])),
      ("%+d: + frag",
       fn () => assertEqualString "+1" (format"%+d" [fromInt 1])),
      ("%+d: + flag to negative number (meaningless)",
       fn () => assertEqualString "-1" (format"%+d" [fromInt ~1])),
      ("%+d: + flag to 0",
       fn () => assertEqualString "+0" (format"%+d" [fromInt 0])),
      ("%+d: + flag to ~0",
       fn () => assertEqualString "+0" (format"%+d" [fromInt ~0])),
      ("%#d: # flag",
       fn () => assertEqualString "1" (format"%#d" [fromInt 1])),
      ("% d: space flag",
       fn () => assertEqualString " 1" (format"% d" [fromInt 1])),
      ("% d: space flag to negative number (meaningless)",
       fn () => assertEqualString "-1" (format"% d" [fromInt ~1])),
      ("% d: space flag to 0",
       fn () => assertEqualString " 0" (format"% d" [fromInt 0])),
      ("% d: space flag to ~0",
       fn () => assertEqualString " 0" (format"% d" [fromInt ~0])),
      ("%.3d: with precision",
       fn () => assertEqualString "123" (format"%.3d" [fromInt 1234])),
      ("%.10d: with precision larger than printed number length",
       fn () => assertEqualString "1234" (format"%.10d" [fromInt 1234])),

      ("char args",
       fn () => assertEqualString "The first letter of Hello is H" (format "The first letter of %s is %c" [fromString "Hello", fromChar (String.sub("Hello", 0))])),
      ("real args",
       fn () => assertEqualString "1.0 + 1.0 = 2.0" (format "%f + %f = %f" [fromReal 1.0, fromReal 1.0, fromReal 2.0]))
  ]

end
