structure ReTest =
struct
open SMLUnit
open Re
open Assert

fun assertWork f =
  (f (); assert "function work" true)
  handle a => fail "function didn't work"

fun assertLexError f =
  (f (); fail "Exception `Re.Lex` wasn't raised")
  handle a => assertEqualExceptionName Lex a

fun assertParseError f =
  (f (); fail "Exception `Re.Parse` wasn't raised")
  handle a => assertEqualExceptionName Parse a

fun assertMatch expected actual =
  (assertSome actual;
   assertEqual3Tuple (assertEqualInt, assertEqualInt, assertEqualArray(assertEqual2Tuple(assertEqualInt, assertEqualInt))) expected (Option.valOf actual))
fun assertNotMatch actual =
  assertNone actual

fun assertMatchString expected actual =
  (assertSome actual;
   assertEqual2Tuple (assertEqualString, assertEqualArray assertEqualString) expected (Option.valOf actual))
(* re *)

fun re_simplest_test () =
  assertWork (fn _ => re "a")
fun re_paren_test () =
  (assertWork (fn _ => re "(a)");
   assertWork (fn _ => re "(a)(b)");
   assertWork (fn _ => re "((a)b)"))
fun re_charset_test () =
  (assertWork (fn _ => re "[a]");
   assertWork (fn _ => re "[ab]");
   assertWork (fn _ => re "[a]b");
   assertWork (fn _ => re "a[b]");
   assertWork (fn _ => re "([a])");
   assertWork (fn _ => re "[(]");
   assertWork (fn _ => re "[)]");
   assertWork (fn _ => re "[{]");
   assertWork (fn _ => re "[}]");
   assertWork (fn _ => re "[[]");
   assertWork (fn _ => re "[]]");
   assertWork (fn _ => re "[a-c]");
   assertWork (fn _ => re "[a-a]");
   assertWork (fn _ => re "[a-]");
   assertWork (fn _ => re "[-a]");
   assertWork (fn _ => re "[\\a]");
   assertWork (fn _ => re "[\\\\a]");
   assertWork (fn _ => re "[|]");
   assertWork (fn _ => re "[a^]");
   assertWork (fn _ => re "[-^]"))
fun re_charset_complement_test () =
  (assertWork (fn _ => re "[^a]");
   assertWork (fn _ => re "[^ab]");
   assertWork (fn _ => re "[^a-c]");
   assertWork (fn _ => re "[^a-a]");
   assertWork (fn _ => re "[^!]");
   assertWork (fn _ => re "[^|]");
   assertWork (fn _ => re "[^{]");
   assertWork (fn _ => re "[^}]");
   assertWork (fn _ => re "[^(]");
   assertWork (fn _ => re "[^)]");
   assertWork (fn _ => re "[^[]");
   assertWork (fn _ => re "[^]]");
   assertWork (fn _ => re "[^\\a]");
   assertWork (fn _ => re "[^\\\\a]");
   assertWork (fn _ => re "[^-]");
   assertWork (fn _ => re "[^-a]");
   assertWork (fn _ => re "[^,]"))
fun re_backslash_test () =
  assertWork (fn _ => re "a\\b")
fun re_backslash_metachars_test () =
  (assertWork (fn _ => re "\\.");
   assertWork (fn _ => re "\\*");
   assertWork (fn _ => re "\\+");
   assertWork (fn _ => re "\\?"))
fun re_backslash_leftparen_test () =
  (assertWork (fn _ => re "a\\(b");
   assertWork (fn _ => re "a\\(()b");
   assertWork (fn _ => re "a()\\(b");
   assertWork (fn _ => re "a(\\()b"))
fun re_backslash_rightparen_test () =
  (assertWork (fn _ => re "a\\)b");
   assertWork (fn _ => re "a\\)()b");
   assertWork (fn _ => re "a()\\)b");
   assertWork (fn _ => re "a(\\))b"))
fun re_linestart_test () =
  (assertWork (fn _ => re "^");
   assertWork (fn _ => re "^a");
   assertWork (fn _ => re "a^");
   assertWork (fn _ => re "^[^a]");
   assertWork (fn _ => re "\\^");
   assertWork (fn _ => re "a^");
   assertWork (fn _ => re "(^)"))
fun re_lineend_test () =
  (assertWork (fn _ => re "$");
   assertWork (fn _ => re "\\$");
   assertWork (fn _ => re "$a");
   assertWork (fn _ => re "^$");
   assertWork (fn _ => re "[$]");
   assertWork (fn _ => re "($)"))
fun re_curly_brace_test () =
  (assertWork (fn _ => re "a{0,1}");
   assertWork (fn _ => re "a{0,0}");
   assertWork (fn _ => re "a{0,10}");
   assertWork (fn _ => re "a{10,11}");
   assertWork (fn _ => re "a{,1}");
   assertWork (fn _ => re "a{0,10}");
   assertWork (fn _ => re "a{0,}");
   assertWork (fn _ => re "a{10,}");
   assertWork (fn _ => re "(a){10,}");
   assertWork (fn _ => re "(a|b){10,}");
   assertWork (fn _ => re "[a-b]{10,}"))

fun re_lex_error_backslash_test () =
  assertLexError (fn _ => re "\\")
fun re_parse_error_star_no_leading_char_test () =
  assertParseError (fn _ => re "*")
fun re_parse_error_plus_no_leading_char_test () =
  assertParseError (fn _ => re "+")
fun re_parse_error_question_no_leading_char_test () =
  assertParseError (fn _ => re "?")
fun re_parse_error_bar_test () =
  (assertParseError (fn _ => re "|");
   assertParseError (fn _ => re "a|");
   assertParseError (fn _ => re "|b");
   assertParseError (fn _ => re "a(|)b");
   assertParseError (fn _ => re "(a|)b");
   assertParseError (fn _ => re "a(|b)"))
fun re_parse_error_paren () =
  (assertParseError (fn _ => re "(");
   assertParseError (fn _ => re ")(");
   assertParseError (fn _ => re "(\\)");
   assertParseError (fn _ => re "([)]"))
fun re_parse_error_char_set () =
  (assertParseError (fn _ => re "[");
   assertParseError (fn _ => re "[\\]");
   assertParseError (fn _ => re "][");
   assertParseError (fn _ => re "[z-a]")
  )
fun re_parse_error_curly_brace () =
  (assertParseError (fn _ => re "{0,1}");
   assertParseError (fn _ => re "a{2,1}");
   assertParseError (fn _ => re "a{-1,1}");
   assertParseError (fn _ => re "a{a,1}");
   assertParseError (fn _ => re "a{0,a}");
   assertParseError (fn _ => re "a{a,b}");
   assertParseError (fn _ => re "a{0-1}");
   assertParseError (fn _ => re "a{,a}");
   assertParseError (fn _ => re "a{a,}");
   assertParseError (fn _ => re "a{,}")
  )
(* match *)
fun match_simplest_test () =
  (assertMatch (0, 1, Array.fromList []) (match(re "a", "a", 0));
   assertNotMatch     (match(re "a", "b", 0));
   assertMatch (1, 2, Array.fromList []) (match(re "a", "ba", 0)))
fun match_start_from_i_test () =
  (assertMatch (0, 1, Array.fromList []) (match(re "a", "ab", 0));
   assertNotMatch     (match(re "a", "ab", 1)))
fun match_any_test () =
  (assertMatch (0, 1, Array.fromList []) (match(re ".", "a", 0));
   assertMatch (0, 1, Array.fromList []) (match(re ".", "b", 0));
   assertMatch (0, 1, Array.fromList []) (match(re ".", ".", 0));
   assertMatch (0, 1, Array.fromList []) (match(re ".", "*", 0));
   assertMatch (0, 1, Array.fromList []) (match(re ".", "+", 0));
   assertMatch (0, 1, Array.fromList []) (match(re ".", "?", 0));
   assertMatch (0, 1, Array.fromList []) (match(re ".", "\\", 0));
   assertMatch (0, 1, Array.fromList []) (match(re ".", "|", 0));
   assertMatch (0, 1, Array.fromList []) (match(re ".", "(", 0));
   assertMatch (0, 1, Array.fromList []) (match(re ".", ")", 0)))

fun match_star_test () =
  (assertMatch (0, 0, Array.fromList []) (match(re "a*", "", 0));
   assertMatch (0, 1, Array.fromList []) (match(re "a*", "a", 0));
   assertMatch (0, 2, Array.fromList []) (match(re "a*", "aa", 0));
   assertMatch (0, 0, Array.fromList []) (match(re "a*", "b", 0));
   assertMatch (0, 0, Array.fromList []) (match(re "a*", "ba", 0)))

fun match_plus_test () =
  (assertNotMatch     (match(re "a+", "", 0));
   assertMatch (0, 1, Array.fromList []) (match(re "a+", "a", 0));
   assertMatch (0, 2, Array.fromList []) (match(re "a+", "aa", 0));
   assertNotMatch     (match(re "a+", "b", 0));
   assertMatch (1, 2, Array.fromList []) (match(re "a+", "ba", 0)))
fun match_option_test () =
  (assertMatch (0, 1, Array.fromList []) (match(re "a?", "a", 0));
   assertMatch (0, 0, Array.fromList []) (match(re "a?", "", 0));
   assertMatch (0, 0, Array.fromList []) (match(re "a?", "ba", 0)))
fun match_linestart_test () =
  (assertMatch (0, 0, Array.fromList []) (match(re "^", "", 0));
   assertMatch (0, 0, Array.fromList []) (match(re "^", "a", 0));
   assertNotMatch                        (match(re "a^", "a", 0));
   assertMatch (0, 2, Array.fromList []) (match(re "a^", "a^b", 0));
   assertMatch (2, 3, Array.fromList []) (match(re "^a", "b\na", 0)))
fun match_lineend_test () =
  (assertMatch (0, 0, Array.fromList []) (match(re "$", "", 0));
   assertMatch (1, 1, Array.fromList []) (match(re "$", "a", 0));
   assertNotMatch                        (match(re "$a", "a", 0));
   assertMatch (1, 3, Array.fromList []) (match(re "$a", "b$a", 0));
   assertMatch (0, 1, Array.fromList []) (match(re "a$", "a\nb", 0)))
fun match_charset_test () =
  (assertMatch (0, 1, Array.fromList []) (match(re "[a]", "a", 0));
   assertNotMatch                        (match(re "[a]", "b", 0));
   assertMatch (0, 1, Array.fromList []) (match(re "[ab]", "a", 0));
   assertMatch (0, 1, Array.fromList []) (match(re "[ab]", "b", 0));
   assertMatch (0, 6, Array.fromList []) (match(re "[ab]+", "abbaab", 0));
   assertMatch (0, 3, Array.fromList []) (match(re "[a-c]+", "abc", 0));
   assertNotMatch                        (match(re "[a-c]+", "-", 0));
   assertMatch (0, 1, Array.fromList []) (match(re "[a-]", "-", 0));
   assertMatch (0, 1, Array.fromList []) (match(re "[-a]", "-", 0));
   assertMatch (3, 4, Array.fromList []) (match(re "[^abc]", "abcd", 0));
   assertMatch (3, 4, Array.fromList []) (match(re "[^a-c]", "abcd", 0));
   assertNotMatch                        (match(re "[^a-c]", "abc", 0));
   assertMatch (1, 2, Array.fromList []) (match(re "[^a-]", "-b", 0));
   assertMatch (1, 2, Array.fromList []) (match(re "[^-a]", "-b", 0))
  )
fun match_curlybraces_test () =
  ((assertMatch (0, 2, Array.fromList []) (match(re "a{0,2}", "aaa", 0)));
   (assertMatch (0, 2, Array.fromList []) (match(re "a{0,2}", "aaaa", 0)));
   (assertMatch (0, 1, Array.fromList []) (match(re "a{0,2}", "a", 0)));
   (assertMatch (0, 1, Array.fromList []) (match(re "a{1,2}", "a", 0)));
   (assertNotMatch                        (match(re "a{2,2}", "a", 0)));
   (assertMatch (0, 1, Array.fromList []) (match(re "a{1,}", "a", 0)));
   (assertMatch (0, 2, Array.fromList []) (match(re "a{1,}", "aa", 0)));
   (assertMatch (0, 5, Array.fromList []) (match(re "a{1,}", "aaaaa", 0)));
   (assertMatch (0, 0, Array.fromList []) (match(re "a{,2}", "", 0)));
   (assertMatch (0, 1, Array.fromList []) (match(re "a{,2}", "a", 0)));
   (assertMatch (0, 2, Array.fromList []) (match(re "a{,2}", "aa", 0)));
   (assertMatch (0, 2, Array.fromList []) (match(re "a{,2}", "aaa", 0)));
   (assertMatch (0, 2, Array.fromList []) (match(re "ab{1,2}", "ab", 0)));
   (assertMatch (0, 2, Array.fromList []) (match(re "ab{1,2}", "abab", 0)));
   (assertMatch (0, 1, Array.fromList []) (match(re "[ab]{,2}", "a", 0)))
  )
(* matchString *)
fun matchString_simple_test () =
  assertMatchString ("a", Array.fromList []) (matchString(re "a", "a", 0))
(* matchstrings *)
fun matchStrings_simple_test () =
  assertEqualStringList ["a", "a"] (matchStrings(re "a", "aa", 0))
(* doesMatch *)
fun doesMatch_simple_test () =
  assertTrue (doesMatch(re "a", "a", 0))
(* split *)
fun split_simpl_test () =
  assertEqualStringList ["a", "b", "c"] (split(re " ", "a b c", 0))
(* replace *)
fun replace_simple_test () =
  assertEqualString "b" (replace(re"a", "a", 0, "b"))
(* replaceAll *)
fun replaceAll_simple_test () =
  assertEqualString "b" (replace(re"a", "a", 0, "b"))
      

fun suite _ =Test.labelTests [
      ("re: simple regexp", re_simplest_test),
      ("re: contianing parens", re_paren_test),
      ("re: charset", re_charset_test),
      ("re: charset complement", re_charset_complement_test),
      ("re: line start", re_linestart_test),
      ("re: line end", re_lineend_test),
      ("re: number specified matchng", re_curly_brace_test),
      ("re: simple quote", re_backslash_test),
      ("re: quoting meta chars", re_backslash_metachars_test),
      ("re: quoting left paren", re_backslash_leftparen_test),
      ("re: lex error against last #\"\\\"", re_lex_error_backslash_test),
      ("re: parse error against #\"*\" without any leading chars", re_parse_error_star_no_leading_char_test),
      ("re: parse error against #\"+\" without any leading chars", re_parse_error_plus_no_leading_char_test),
      ("re: parse error against #\"?\" without any leading chars", re_parse_error_question_no_leading_char_test),
      ("re: parse error against illeagal #\"|\"", re_parse_error_bar_test),
      ("re: parse error against unmatching parens", re_parse_error_paren),
      ("re: parse error against charset", re_parse_error_char_set),
      ("re: parse error against curly braces", re_parse_error_curly_brace),
      ("match: simple string", match_simplest_test),
      ("match: match start from non-zero", match_start_from_i_test),
      ("match: /./", match_any_test),
      ("match: /a*/", match_star_test),
      ("match: /a+/", match_plus_test),
      ("match: /a?/", match_option_test),
      ("match: /^/", match_linestart_test),
      ("match: /$/", match_lineend_test),
      ("match: /[]/", match_charset_test),
      ("match: /{,}/", match_curlybraces_test),
      
      ("matchString: simple string", matchString_simple_test),

      ("matchStrings: simple string", matchStrings_simple_test),

      ("doesMatch: simple string", doesMatch_simple_test),

      ("split: simple string", split_simpl_test),

      ("replace: simple string" , replace_simple_test),

      ("replaceAll: simple string" , replaceAll_simple_test)
  ]
end
