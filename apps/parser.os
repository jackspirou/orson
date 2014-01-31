!
!  APPS/PARSER. Recursive descent parser for Orson.
!
!  Copyright © 2012 James B. Moen.
!
!  This  program is free  software: you  can redistribute  it and/or  modify it
!  under the terms  of the GNU General Public License as  published by the Free
!  Software Foundation,  either version 3 of  the License, or  (at your option)
!  any later version.
!
!  This program is distributed in the  hope that it will be useful, but WITHOUT
!  ANY  WARRANTY;  without even  the  implied  warranty  of MERCHANTABILITY  or
!  FITNESS FOR  A PARTICULAR PURPOSE.  See  the GNU General  Public License for
!  more details.
!
!  You should have received a copy of the GNU General Public License along with
!  this program.  If not, see <http://www.gnu.org/licenses/>.
!

!  This is a recursive descent parser for Orson. It might be used as a starting
!  point for programs like cross-referencers or pretty-printers that must parse
!  Orson source programs. It doesn't do error recovery, but writes a diagnostic
!  message and halts on the first error it finds. It writes an indented message
!  when entering and exiting each major procedure. This can be turned on or off
!  by rebinding TRACING PARSER below.

(load ''lib.ascii'')    !  Operations on ASCII characters.
(load ''lib.buffer'')   !  Fixed length linear queues.
(load ''lib.convert'')  !  Convert a string to an integer or a real.
(load ''lib.file'')     !  Input and output on file streams.
(load ''lib.plain'')    !  Operations on Orson plain names.
(load ''lib.select'')   !  Simulate a CASE clause whose labels are strings.

(prog

!  Constants.

  char   eofChar       :− '\#FF'      !  End of file sentinel.
  char   eolChar       :− '\#00'      !  End of line sentinel.
  int    maxBoldLength :− 5           !  Chars in longest bold name.
  int    maxLineLength :− 1024        !  Maximum length of LINE. 
  int    maxRadix      :− 36          !  Max radix in #'d integer token.
  int    minBoldLength :− 2           !  Chars in shortest bold name.
  int    minRadix      :− 2           !  Min radix in #'d integer token.
  string self          :− ''Parser''  !  Name of this program.
  bool   tracingParser :− true        !  Are we tracing the parser?
 
!  Types.

  buf0 :− buffer(maxLineLength, char0)  !  Temporarily hold CHAR0s.
  buf1 :− buffer(maxLineLength, char1)  !  Temporarily hold CHAR1s.

!  Variables.

  var char   ch             !  Last char from LINE.
  var int    indent :− 0    !  Current indent while tracing.
  var buf1   line           !  Last line from SOURCE.
  var int    lineNumber     !  Sequence number of LINE.
  var string path           !  Pathname to SOURCE file.
  var stream source         !  Source file to read LINEs from.
  var int    token          !  Current token.
  var buf0   tokenChars     !  Chars making up TOKEN.
  var bool   tokenEndsTerm  !  Does TOKEN end a term?
  var int    tokenRadix     !  Radix of numeric TOKEN.

!  ERROR. Write an error message about the current line showing elements of the
!  list OBJECTS according to FORMAT. Show the current LINE NUMBER and PATH too.
!  Then EXIT.

  error :−
   (alt
    (form (string format) void:
      error(format:)),
    (form (string format, list objects) void:
      write(errput, self & '': '')
      write(errput, format, objects)
      writeln(errput, '' at line %i of "%s".'': lineNumber, path)
      exit(1)))

!  IS CONTROL CHAR. Test if CH is an illegal control char.

  isControlChar :−
   (form (char ch) bool:
    (with char ch :− (past ch)
     do '\#00' ≤ ch ≤ '\#1F' ∨
        '\#7F' = ch ∨
        '\#80' ≤ ch ≤ '\#9F' ∨
        '\#2028' = ch ∨
        '\#2029' = ch))

!  LB. Return the lower byte of CH.

  lb :−
   (form (char ch) int:
     #FF & ch)

!  UB. Return the upper three bytes of CH.

  ub :−
   (form (char ch) int:
     ch >> 8)

!  NEXT CHAR. Copy the next char from LINE into CH. If LINE is empty, then read
!  the next line before we do it.

  nextChar :−
   (proc () void:
    (if atEnd(line)
     then nextLine())
    ch := start(line)
    advance(line))

!  NEXT NEXT CHAR. Return the char that will follow CH in LINE. If we're at the
!  end of LINE, then return a blank.

  nextNextChar :−
   (proc () char:
    (if atEnd(line)
     then ' '
     else start(line)))

!  NEXT LINE. Read the next line from SOURCE into LINE. Lines may be terminated
!  by L, R, L R, or R L, where L is the ASCII line feed char and R is the ASCII
!  return char. The last line in SOURCE may be terminated by an end of file.

  nextLine :−
   (proc () void:
    (with
      var char temp

!  NEXT END FILE. Read an EOF CHAR into LINE and stop reading.

      nextEndFile :−
       (form () bool:
        (if atStart(line)
         then append(line, eofChar))
        append(line, ' ')
        false)

!  NEXT LINEFEED. Read an EOL CHAR into LINE and stop reading.

      nextLinefeed :−
       (form () bool:
         temp := read(source)
         (if temp ≠ eos ∧ temp ≠ '\R'
          then unget(source, temp))
         append(line, eolChar)
         append(line, ' ')
         false)

!  NEXT RETURN. Read an EOL CHAR into LINE and stop reading.

      nextReturn :−
       (form () bool:
         temp := read(source)
         (if temp ≠ eos ∧ temp ≠ '\N'
          then unget(source, temp))
         append(line, eolChar)
         append(line, ' ')
         false)

!  NEXT OTHER. Read a visible char into LINE and keep reading.

      nextOther :−
       (form () bool:
        (if isFull(line)
         then error(''More than %i chars'': maxLineLength)
         else if isControlChar(temp)
              then error(''Illegal char '\\#%02X''': temp)
              else append(line, temp))
        true)

!  This is NEXT LINE's body. 

     do empty(line)
        lineNumber += 1
        (while
          temp := read(source)
          (case temp
           of eos{char}: nextEndFile()
                   '\N': nextLinefeed()
                   '\R': nextReturn()
                   none: nextOther()))))

!  Tokens.

  makeToken :− enum()

  assignerToken     :− makeToken()  !  An operator of the form "⊗=".
  blankToken        :− makeToken()  !  Whitespace. It's ignored.
  boldAlsoToken     :− makeToken()  !  The name "also".
  boldAltToken      :− makeToken()  !  The name "alt".
  boldAltsToken     :− makeToken()  !  The name "alts".
  boldAndToken      :− makeToken()  !  The name "and".
  boldCaseToken     :− makeToken()  !  The name "case".
  boldCatchToken    :− makeToken()  !  The name "catch".
  boldDoToken       :− makeToken()  !  The name "do".
  boldElseToken     :− makeToken()  !  The name "else".
  boldForToken      :− makeToken()  !  The name "for".
  boldFormToken     :− makeToken()  !  The name "form".
  boldGenToken      :− makeToken()  !  The name "gen".
  boldIfToken       :− makeToken()  !  The name "if".
  boldInToken       :− makeToken()  !  The name "in".
  boldLoadToken     :− makeToken()  !  The name "load".
  boldNoneToken     :− makeToken()  !  The name "none".
  boldOfToken       :− makeToken()  !  The name "of".
  boldOrToken       :− makeToken()  !  The name "or".
  boldPastToken     :− makeToken()  !  The name "past".
  boldProcToken     :− makeToken()  !  The name "proc".
  boldProgToken     :− makeToken()  !  The name "prog".
  boldRefToken      :− makeToken()  !  The name "ref".
  boldRowToken      :− makeToken()  !  The name "row".
  boldThenToken     :− makeToken()  !  The name "then".
  boldTupleToken    :− makeToken()  !  The name "tuple".
  boldTypeToken     :− makeToken()  !  The name "type".
  boldVarToken      :− makeToken()  !  The name "var".
  boldWhileToken    :− makeToken()  !  The name "while".
  boldWithToken     :− makeToken()  !  The name "with".
  charToken         :− makeToken()  !  A character literal.
  closeBraceToken   :− makeToken()  !  The symbol "}".
  closeBracketToken :− makeToken()  !  The symbol "]".
  closeParenToken   :− makeToken()  !  The symbol ")".
  colonToken        :− makeToken()  !  The symbol ":".
  colonDashToken    :− makeToken()  !  The symbol ":-" or ":−".
  commaToken        :− makeToken()  !  The symbol ",".
  comparisonToken   :− makeToken()  !  A comparison operator like "<" or "≥".
  dollarToken       :− makeToken()  !  The symbol "$".
  dotToken          :− makeToken()  !  The symbol ".".
  endFileToken      :− makeToken()  !  End of source file.
  hookToken         :− makeToken()  !  A plain name that starts with "?".
  integerToken      :− makeToken()  !  A nonnegative integer literal.
  newlineToken      :− makeToken()  !  Whitespace. It's ignored.
  openBraceToken    :− makeToken()  !  The symbol "{".
  openBracketToken  :− makeToken()  !  The symbol "[".
  openParenToken    :− makeToken()  !  The symbol "(".
  plainNameToken    :− makeToken()  !  A plain name.
  postfixToken      :− makeToken()  !  A postfix operator like "^" or "↓".
  prefixToken       :− makeToken()  !  A prefix operator like "+" or "~".
  productToken      :− makeToken()  !  A product operator like "*" or "×".
  quotedNameToken   :− makeToken()  !  A quoted name.
  realToken         :− makeToken()  !  A nonnegative real literal.
  secretNameToken   :− makeToken()  !  A secret name.
  semicolonToken    :− makeToken()  !  The symbol ";".
  stringToken       :− makeToken()  !  A string literal.
  sumToken          :− makeToken()  !  A sum operator not a prefix like "|".
  sumPrefixToken    :− makeToken()  !  A sum operator or a prefix like "−".

!  NEXT TOKEN. Advance TOKEN to the next token. TOKEN ENDS TERM is true iff the
!  next token can end a term. A NEWLINE TOKEN immediately following such a term
!  will be treated as a SEMICOLON TOKEN. The current token must equal EXPECTED,
!  or else an error will result.

  nextToken :−
   (alt
    (form () void:
      NextToken()),
    (form (int expected, string message) void:
     (if token = expected
      then NextToken()
      else error(message))))

!  The procedure NEXT TOKEN does all the work for the form NEXT TOKEN.

  NextToken :−
   (proc () void:
    (with

!  NEXT APOSTROPHE. Scan a string or character literal.

      nextApostrophe :−
       (form () void:
        (with

!  NEXT ESCAPED CHAR. Scan a character that might begin with "\".

          nextEscapedChar :−
           (proc () void:
            (with

!  NEXT HEXADECIMAL CHAR. Scan "#" followed by one or more radix 16 digits.

              nextHexadecimalChar :−
               (form () void:
                (with var buf0 digits
                 do nextChar()
                    empty(digits)
                    (while isDigit(ch, 16) ∨ ch = '_'
                     do (if ch ≠ '_'
                         then append(digits, ch{char0}))
                        nextChar())
                    (if ch = '\\' ∧ nextNextChar() = '_'
                     then nextChar()
                          nextChar())
                    (for bool ok, int code in convert(int, digits{string}, 16)
                     do (if ok ∧ 0 ≤ code ≤ high(char)
                         then append(tokenChars, code{char})
                         else error(''Illegal character code'')))))

!  NEXT SIMPLE CHAR. Append CH to TOKEN CHARS and skip the current char.

              nextSimpleChar :−
               (form (char ch) void:
                 append(tokenChars, ch)
                 nextChar())

!  This is NEXT ESCAPED CHAR's body.

             do (if ch = '\\'
                 then nextChar()
                      (case ch
                       of 'A', 'a': nextSimpleChar('\A')
                          'B', 'b': nextSimpleChar('\B')
                          'E', 'e': nextSimpleChar('\E')
                          'F', 'f': nextSimpleChar('\F')
                          'N', 'n': nextSimpleChar('\N')
                          'R', 'r': nextSimpleChar('\R')
                          'T', 't': nextSimpleChar('\T')
                          'V', 'v': nextSimpleChar('\V')
                               '0': nextSimpleChar('\0')
                               '#': nextHexadecimalChar()
                        '\'', '\\': nextSimpleChar(ch)
                              none: error(''Illegal \\ char''))
                 else nextSimpleChar(ch))))

!  This is NEXT OPEN QUOTE's body. If the token begins with "''" then we scan a
!  string literal. If it begins with "'", then we scan a character literal.

         do nextChar()
            empty(tokenChars)
            (if ch = '\''
             then nextChar()
                  (while
                   (if ch = '\''
                    then nextChar()
                         (if ch = '\''
                          then nextChar()
                               (if ch = '\''
                                then append(tokenChars, '\'')
                                     nextChar())
                               false
                          else if ch = eolChar
                               then error(''Missing close quote'')
                               else append(tokenChars, '\'')
                                    nextEscapedChar()
                                    true)
                    else if ch = eolChar
                         then error(''Missing close quote'')
                         else nextEscapedChar()
                              true))
                  token := stringToken
             else (if ch = '\'' ∨ ch = eolChar
                   then error(''Missing character'')
                   else nextEscapedChar())
                  (if ch = '\''
                   then nextChar()
                   else error(''Missing close quote''))
                  token := charToken)
            tokenEndsTerm := true))

!  NEXT BLANK. Scan a blank without advancing TOKEN.

      nextBlank :−
       (form () void:
         nextChar())

!  NEXT CLOSE BRACE. Scan "}".

      nextCloseBrace :−
       (form () void:
         nextChar()
         token := closeBraceToken
         tokenEndsTerm := true)

!  NEXT CLOSE BRACKET. Scan "]".

      nextCloseBracket :−
       (form () void:
         nextChar()
         token := closeBracketToken
         tokenEndsTerm := true)

!  NEXT CLOSE PAREN. Scan ")".

      nextCloseParen :−
       (form () void:
         nextChar()
         token := closeParenToken
         tokenEndsTerm := true)

!  NEXT COLON. Scan ":", ":-", ":−", or ":=".

      nextColon :−
       (form () void:
         empty(tokenChars)
         append(tokenChars, ch)
         nextChar()
         (if ch = '-' ∨ ch = '−'
          then token := colonDashToken
               nextChar()
          else if ch = '='
               then token := assignerToken
                    append(tokenChars, ch)
                    nextChar()
               else token := colonToken)
         tokenEndsTerm := false)

!  NEXT COMMA. Scan ",".

      nextComma :−
       (form () void:
         nextChar()
         token := commaToken
         tokenEndsTerm := false)

!  NEXT COMMENT. Scan a comment without advancing TOKEN, so it is equivalent to
!  a blank. We skip characters until we encounter the end of the line. However,
!  we must not skip the end of line character, because it might be treated as a
!  semicolon token, depending on TOKEN ENDS TERM.

      nextComment :−
       (form () void:
        (while
         (if ch = eofChar ∨ ch = eolChar
          then false
          else nextChar()
               true)))

!  NEXT DOLLAR. Scan "$".

      nextDollar :−
       (form () void:
         nextChar()
         token := dollarToken
         tokenEndsTerm := false)

!  NEXT DOT. Scan ".".

      nextDot :−
       (form () void:
         nextChar()
         token := dotToken
         tokenEndsTerm := false)

!  NEXT END FILE. Scan the end of the program.

      nextEndFile :−
       (form () void:
         token := endFileToken
         tokenEndsTerm := false)

!  NEXT END LINE. Scan the end of a line. If the previous token can be the last
!  token of a term, then we treat the end of the line as a NEWLINE TOKEN. If it
!  cannot be, then we treat it as a blank.

      nextEndLine :−
       (form () void:
         nextChar()
         (if tokenEndsTerm
          then token := newlineToken
               tokenEndsTerm := false))

!  NEXT ENDER. Scan a one-char token that ends a term.

      nextEnder :−
       (form (int enderToken) void:
         empty(tokenChars)
         append(tokenChars, ch)
         nextChar()
         token := enderToken
         tokenEndsTerm := true)

!  NEXT GREATER. Scan ">", ">>", ">>=", or ">=".

      nextGreater :−
       (form () void:
         empty(tokenChars)
         append(tokenChars, ch)
         nextChar()
         (if ch = '>'
          then append(tokenChars, ch)
               nextChar()
               (if ch = '='
                then token := assignerToken
                     append(tokenChars, ch)
                     nextChar()
                else token := productToken)
          else if ch = '='
               then token := comparisonToken
                    append(tokenChars, ch)
                    nextChar()
               else token := comparisonToken)
         tokenEndsTerm := false)

!  NEXT HEXADECIMAL NUMBER. Scan "#" followed by one or more radix 16 digits.

      nextHexadecimalNumber :−
       (form () void:
         nextChar()
         empty(tokenChars)
         (while isDigit(ch, 16) ∨ ch = '_'
          do (if ch ≠ '_'
              then append(tokenChars, ch))
             nextChar())
         tokenRadix := 16
         token := integerToken
         tokenEndsTerm := true)

!  NEXT ILLEGAL. Scan an illegal token.

      nextIllegal :−
       (form () void:
         error(''Illegal token starting with '%c''': ch))

!  NEXT INFIX. Scan an infix operator, whose token is INFIX TOKEN. The operator
!  may be followed by "=", turning it into an ASSIGNER TOKEN.

      nextInfix :−
       (proc (int infixToken) void:
         empty(tokenChars)
         append(tokenChars, ch)
         nextChar()
         (if ch = '='
          then append(tokenChars, ch)
               nextChar()
               token := assignerToken
          else token := infixToken)
         tokenEndsTerm := false)

!  NEXT LESS. Scan "<", "<<", "<<=", or "<=".

      nextLess :−
       (form () void:
         empty(tokenChars)
         append(tokenChars, ch)
         nextChar()
         (if ch = '<'
          then append(tokenChars, ch)
               nextChar()
               (if ch = '='
                then token := assignerToken
                     append(tokenChars, ch)
                     nextChar()
                else token := productToken)
          else if ch = '='
               then token := comparisonToken
                    append(tokenChars, ch)
                    nextChar()
               else if ch = '>'
                    then token := comparisonToken
                         append(tokenChars, ch)
                         nextChar()
                    else token := comparisonToken)
         tokenEndsTerm := false)

!  NEXT NONENDER. Scan a one-char token that doesn't end a term.

      nextNonender :−
       (form (int nonenderToken) void:
         empty(tokenChars)
         append(tokenChars, ch)
         nextChar()
         token := nonenderToken
         tokenEndsTerm := false)

!  NEXT NUMBER. Scan an integer or real literal.

      nextNumber :−
       (proc () void:
        (with

!  NEXT DIGITS. Scan one or more digits of RADIX.

          nextDigits :−
           (alt
            (form () void:
              nextDigits(10)),
            (form (int radix) void:
             (while
              (if isDigit(ch, radix)
               then append(tokenChars, ch)
                    nextChar()
                    true
               else if ch = '_'
                    then nextChar()
                         true
                    else false))))

!  This is NEXT NUMBER's body. First scan a series of one or more digits. If it
!  is followed by "#", then it is the radix of an integer literal, so we scan a
!  series of digits in that radix. If the series is followed by ".", then it is
!  the whole part of a real literal, so we scan its fractional part followed by
!  its optional scale factor. If the series is followed by any other character,
!  then is it is a radix 10 integer literal.

         do empty(tokenChars)
            nextDigits()
            (if ch = '#'
             then nextChar()
                  tokenRadix := 0
                  restart(tokenChars)
                  (while ¬ atEnd(tokenChars) ∧ tokenRadix ≤ maxRadix
                   do tokenRadix := 10 × tokenRadix + (start(tokenChars) − '0')
                      advance(tokenChars))
                  (if tokenRadix < minRadix ∨ tokenRadix > maxRadix
                   then error(''Radix out of range''))
                  empty(tokenChars)
                  nextDigits(tokenRadix)
                  (if isPlain(ch)
                   then error(''Illegal number''))
                  token := integerToken
             else if ch = '.' ∧ isDigit(nextNextChar())
                  then nextChar()
                       append(tokenChars, '.')
                       nextDigits()
                       (if ch = 'E' ∨ ch = 'e'
                        then append(tokenChars, 'E')
                             nextChar()
                             (if ch = '+'
                              then nextChar()
                              else if ch = '-' ∨ ch = '−'
                                   then append(tokenChars, '-')
                                        nextChar())
                             nextDigits())
                       token := realToken
                  else token := integerToken
                       tokenRadix := 10)
            tokenEndsTerm := true))

!  NEXT OPEN BRACE. Scan "{".

      nextOpenBrace :−
       (form () void:
         nextChar()
         token := openBraceToken
         tokenEndsTerm := false)

!  NEXT OPEN BRACKET. Scan "[".

      nextOpenBracket :−
       (form () void:
         nextChar()
         token := openBracketToken
         tokenEndsTerm := false)

!  NEXT OPEN PAREN. Scan "(".

      nextOpenParen :−
       (form () void:
         nextChar()
         token := openParenToken
         tokenEndsTerm := false)

!  NEXT PLAIN NAME. Scan a name that starts with a lower case letter. It may be
!  a reserved name.

      nextPlainName :−
       (proc () void:
         empty(tokenChars)
         (while isPlain(ch)
          do append(tokenChars, ch)
             nextChar())
         (if minBoldLength ≤ length(tokenChars) ≤ maxBoldLength
          then token :=
                select(tokenChars{string}, plainNameToken:
                 (: ''also'',  boldAlsoToken),
                 (: ''alt'',   boldAltToken),
                 (: ''alts'',  boldAltsToken),
                 (: ''and'',   boldAndToken),
                 (: ''case'',  boldCaseToken),
                 (: ''catch'', boldCatchToken),
                 (: ''do'',    boldDoToken),
                 (: ''else'',  boldElseToken),
                 (: ''for'',   boldForToken),
                 (: ''form'',  boldFormToken),
                 (: ''gen'',   boldGenToken),
                 (: ''if'',    boldIfToken),
                 (: ''in'',    boldInToken),
                 (: ''load'',  boldLoadToken),
                 (: ''mod'',   productToken),
                 (: ''none'',  boldNoneToken),
                 (: ''not'',   prefixToken),
                 (: ''of'',    boldOfToken),
                 (: ''or'',    boldOrToken),
                 (: ''past'',  boldPastToken),
                 (: ''proc'',  boldProcToken),
                 (: ''prog'',  boldProgToken),
                 (: ''ref'',   boldRefToken),
                 (: ''row'',   boldRowToken),
                 (: ''then'',  boldThenToken),
                 (: ''tuple'', boldTupleToken),
                 (: ''type'',  boldTypeToken),
                 (: ''var'',   boldVarToken),
                 (: ''while'', boldWhileToken),
                 (: ''with'',  boldWithToken))
          else token := plainNameToken) 
         tokenEndsTerm := (token = plainNameToken))

!  NEXT QUESTION. Scan a hook, which begins with a question mark.

      nextQuestion :−
       (form () void:
         nextChar()
         empty(tokenChars)
         (while isPlain(ch)
          do append(tokenChars, ch)
             nextChar())
         token := hookToken
         tokenEndsTerm := true)

!  NEXT QUOTED NAME. Scan a quoted name, beginning with a double quote.

      nextQuotedName :−
       (form () void:
         nextChar()
         empty(tokenChars)
         append(tokenChars, '"')
         (while ch ≠ '"' ∧ ch ≠ eolChar
          do (if ch ≠ ' ' ∨ start(tokenChars) ≠ ' '
              then append(tokenChars, ch))
             nextChar())
         (if ch = '"'
          then append(tokenChars, '"')
               nextChar()
          else error(''Missing close quote''))
         token := quotedNameToken
         tokenEndsTerm := true)

!  NEXT SECRET NAME. Scan a secret name, beginning with an upper case letter.

      nextSecretName :−
       (proc () void:
         empty(tokenChars)
         (while isPlain(ch)
          do append(tokenChars, ch)
             nextChar())
         token := secretNameToken
         tokenEndsTerm := true)

!  NEXT SEMICOLON. Scan ";".

      nextSemicolon :−
       (form () void:
         nextChar()
         token := semicolonToken
         tokenEndsTerm := false)

!  NEXT ARROW TOKEN. Scan a token that begins with an arrow.

      nextArrowToken :−
       (form () void:
        (case lb(ch)
         of lb('←'): nextInfix(productToken)
            lb('↑'): nextEnder(postfixToken)
            lb('→'): nextInfix(productToken)
            lb('↓'): nextEnder(postfixToken)
               none: nextIllegal()))

!  NEXT GREEK TOKEN. Scan a token that begins with a Greek letter.

      nextGreekToken :−
       (form () void:
        (case lb(ch)
         of lb('Γ'): nextPlainName()
            lb('Δ'): nextPlainName()
            lb('Θ'): nextPlainName()
            lb('Λ'): nextPlainName()
            lb('Ξ'): nextPlainName()
            lb('Π'): nextPlainName()
            lb('Σ'): nextPlainName()
            lb('Φ'): nextPlainName()
            lb('Ω'): nextPlainName()
            lb('α'): nextPlainName()
            lb('β'): nextPlainName()
            lb('γ'): nextPlainName()
            lb('δ'): nextPlainName()
            lb('ε'): nextPlainName()
            lb('ζ'): nextPlainName()
            lb('η'): nextPlainName()
            lb('θ'): nextPlainName()
            lb('ι'): nextPlainName()
            lb('κ'): nextPlainName()
            lb('λ'): nextPlainName()
            lb('μ'): nextPlainName()
            lb('ν'): nextPlainName()
            lb('ξ'): nextPlainName()
            lb('π'): nextPlainName()
            lb('ρ'): nextPlainName()
            lb('ς'): nextPlainName()
            lb('σ'): nextPlainName()
            lb('τ'): nextPlainName()
            lb('υ'): nextPlainName()
            lb('ϕ'): nextPlainName()
            lb('χ'): nextPlainName()
            lb('ψ'): nextPlainName()
            lb('ω'): nextPlainName()
            lb('ϑ'): nextPlainName()
            lb('ϒ'): nextPlainName()
            lb('φ'): nextPlainName()
            lb('ϖ'): nextPlainName()
            lb('ϱ'): nextPlainName()
            lb('ϵ'): nextPlainName()
               none: nextIllegal()))

!  NEXT LATIN TOKEN. Scan a token that begins with an ASCII character.

      nextLatinToken :−
       (form () void:
        (case ch
         of ' ': nextBlank()
            '!': nextComment()
            '"': nextQuotedName()
            '#': nextHexadecimalNumber()
            '$': nextDollar()
            '&': nextInfix(productToken)
           '\'': nextApostrophe()
            '(': nextOpenParen()
            ')': nextCloseParen()
            '*': nextInfix(productToken)
            '+': nextInfix(sumPrefixToken)
            ',': nextComma()
            '-': nextInfix(sumPrefixToken)
            '.': nextDot()
            '/': nextInfix(productToken)
            '0': nextNumber()
            '1': nextNumber()
            '2': nextNumber()
            '3': nextNumber()
            '4': nextNumber()
            '5': nextNumber()
            '6': nextNumber()
            '7': nextNumber()
            '8': nextNumber()
            '9': nextNumber()
            ':': nextColon()
            ';': nextSemicolon()
            '<': nextLess()
            '=': nextNonender(comparisonToken)
            '>': nextGreater()
            '?': nextQuestion()
            '@': nextEnder(postfixToken)
            'A': nextSecretName()
            'B': nextSecretName()
            'C': nextSecretName()
            'D': nextSecretName()
            'E': nextSecretName()
            'F': nextSecretName()
            'G': nextSecretName()
            'H': nextSecretName()
            'I': nextSecretName()
            'J': nextSecretName()
            'K': nextSecretName()
            'L': nextSecretName()
            'M': nextSecretName()
            'N': nextSecretName()
            'O': nextSecretName()
            'P': nextSecretName()
            'Q': nextSecretName()
            'R': nextSecretName()
            'S': nextSecretName()
            'T': nextSecretName()
            'U': nextSecretName()
            'V': nextSecretName()
            'W': nextSecretName()
            'X': nextSecretName()
            'Y': nextSecretName()
            'Z': nextSecretName()
            '[': nextOpenBracket()
            ']': nextCloseBracket()
            '^': nextEnder(postfixToken)
            'a': nextPlainName()
            'b': nextPlainName()
            'c': nextPlainName()
            'd': nextPlainName()
            'e': nextPlainName()
            'f': nextPlainName()
            'g': nextPlainName()
            'h': nextPlainName()
            'i': nextPlainName()
            'j': nextPlainName()
            'k': nextPlainName()
            'l': nextPlainName()
            'm': nextPlainName()
            'n': nextPlainName()
            'o': nextPlainName()
            'p': nextPlainName()
            'q': nextPlainName()
            'r': nextPlainName()
            's': nextPlainName()
            't': nextPlainName()
            'u': nextPlainName()
            'v': nextPlainName()
            'w': nextPlainName()
            'x': nextPlainName()
            'y': nextPlainName()
            'z': nextPlainName()
            '{': nextOpenBrace()
            '|': nextInfix(sumToken)
            '}': nextCloseBrace()
            '~': nextInfix(sumPrefixToken)
            '¬': nextInfix(prefixToken)
            '×': nextInfix(productToken)
        eofChar: nextEndFile()
        eolChar: nextEndLine()
           none: nextIllegal()))

!  NEXT MATH TOKEN. Scan a token that begins with a mathematical symbol.

      nextMathToken :−
       (form () void:
        (case lb(ch)
         of lb('∀'): nextPlainName()
            lb('∃'): nextPlainName()
            lb('∅'): nextPlainName()
            lb('∊'): nextNonender(comparisonToken)
            lb('∍'): nextNonender(comparisonToken)
            lb('−'): nextInfix(sumPrefixToken)
            lb('∘'): nextInfix(sumToken)
            lb('∞'): nextPlainName()
            lb('∧'): nextInfix(boldAndToken)
            lb('∨'): nextInfix(boldOrToken)
            lb('∩'): nextInfix(productToken)
            lb('∪'): nextInfix(sumToken)
            lb('≠'): nextNonender(comparisonToken)
            lb('≡'): nextNonender(comparisonToken)
            lb('≢'): nextNonender(comparisonToken)
            lb('≤'): nextNonender(comparisonToken)
            lb('≥'): nextNonender(comparisonToken)
            lb('≺'): nextNonender(comparisonToken)
            lb('≻'): nextNonender(comparisonToken)
            lb('≼'): nextNonender(comparisonToken)
            lb('≽'): nextNonender(comparisonToken)
            lb('⊂'): nextNonender(comparisonToken)
            lb('⊃'): nextNonender(comparisonToken)
            lb('⊆'): nextNonender(comparisonToken)
            lb('⊇'): nextNonender(comparisonToken)
            lb('⊏'): nextNonender(comparisonToken)
            lb('⊐'): nextNonender(comparisonToken)
            lb('⊑'): nextNonender(comparisonToken)
            lb('⊒'): nextNonender(comparisonToken)
            lb('⊓'): nextInfix(productToken)
            lb('⊔'): nextInfix(sumToken)
            lb('⊕'): nextInfix(sumPrefixToken)
            lb('⊖'): nextInfix(sumPrefixToken)
            lb('⊗'): nextInfix(productToken)
            lb('⊘'): nextInfix(productToken)
            lb('⊙'): nextInfix(productToken)
            lb('⊥'): nextPlainName()
            lb('⋅'): nextInfix(productToken)
               none: nextIllegal()))

!  This is NEXT TOKEN's body. We dispatch on the first character of each token,
!  CH, to a scanner for the token. We keep on scanning tokens until we find one
!  that is not a BLANK TOKEN. This skips blanks, comments, and newlines that do
!  not turn into SEMICOLON TOKENs.

     do token := blankToken
        (while token = blankToken
         do (case ub(ch)
             of ub('a'): nextLatinToken()
                ub('α'): nextGreekToken()
                ub('↑'): nextArrowToken()
                ub('∀'): nextMathToken()
                   none: nextIllegal()))))

!  NEXT NEWLINE. If TOKEN is NEWLINE TOKEN, then skip it. We use this in places
!  where a NEWLINE TOKEN should never be treated as a SEMICOLON TOKEN.

  nextNewline :−
   (form () void:
    (if token = newlineToken
     then nextToken()))

!  PARSED. Write an indented message containing OBJECTS, displayed according to
!  the constant string FORMAT. If TRACING PARSER is false, then do nothing.

  parsed :−
   (alt
    (form (string format) void:
      parsed(format:)),
    (form (string format, list objects) void:
     (if tracingParser
      then (in indent
            do write(' '))
           writeln(format, objects))))

!  PARSING. Wrapper. Write a message like that of PARSED that says a parser has
!  been entered. Call BODY. Then write a similar message that says a parser has
!  been executed. If TRACING PARSER is false, then simply call BODY.

  parsing :−
   (alt
    (form (string format) foj:
      parsing(format:)),
    (form (string format, list objects) foj:
     (form (form () obj body) obj:
      (if tracingParser
       then (in indent
             do write(' '))
            writeln(''enter '' & format, objects)
            indent += 1
            body()
            indent −= 1
            (in indent
             do write(' '))
            writeln(''exit '' & format, objects)
       else body()))))

!  AT ASSIGNER. Test if TOKEN is an assignment operator, such as ":=".

  atAssigner :−
   (form () bool:
     token = assignerToken)

!  AT COMPARISON. Test if TOKEN is a comparison operator, such as "<".

  atComparison :−
   (form () bool:
     token = comparisonToken)

!  AT SUM. Test if TOKEN is a sum operator, such as "+".

  atSum :−
   (form () bool:
     token = sumToken ∨ token = sumPrefixToken)

!  AT NAME. Test if TOKEN is an unreserved name token.

  atName :−
   (form () bool:
     token = plainNameToken ∨
     token = quotedNameToken ∨
     token = secretNameToken)

!  AT POSTFIX. Test if TOKEN begins a postfix operator, such as "@".

  atPostfix :−
   (form () bool:
     token = dotToken ∨
     token = openBraceToken ∨
     token = openBracketToken ∨
     token = openParenToken ∨
     token = postfixToken)

!  AT PRODUCT. Test if TOKEN is a product operator, such as "*".

  atProduct :−
   (form () bool:
     token = productToken)

!  AT TERM. Test if TOKEN begins a term. Since terms begin expressions, we also
!  test if TOKEN begins an expression. It's convenient to imagine that NONE can
!  begin a term, even though it really can't.

  atTerm :−
   (proc () bool:
     token = charToken ∨
     token = boldFormToken ∨
     token = boldGenToken ∨
     token = boldNoneToken ∨
     token = boldProcToken ∨
     token = boldRefToken ∨
     token = boldRowToken ∨
     token = boldTypeToken ∨
     token = boldVarToken ∨
     token = dollarToken ∨
     token = hookToken ∨
     token = integerToken ∨
     token = openBracketToken ∨
     token = openParenToken ∨
     token = plainNameToken ∨
     token = prefixToken ∨
     token = quotedNameToken ∨
     token = realToken ∨
     token = secretNameToken ∨
     token = stringToken ∨
     token = sumPrefixToken)

!  NEXT ARGUMENTS. Parse zero or more expressions separated by commas, followed
!  optionally by a colon and another series of zero or more expressions.

  nextArguments :−
   (form () void:
     nextExpressions()
     (if token = colonToken
      then nextToken()
           nextExpressions()))

!  NEXT EQUATE. Parse an equate. If T is a term, N is a name, and E is a single
!  expression, then an equate can be T N, T N :− E, or N :− E. Note that we use
!  a trick to parse the initial term, since we don't know if it will be an N or
!  a T.

  nextEquate :−
   (proc () void:
    (in parsing(''equate'')
     do (if atName()
         then nextName()
              nextNewline()
              (if atPostfix()
               then nextPostfix()
                    nextNewline()
                    nextName()
                    (if token = colonDashToken
                     then nextToken()
                          nextExpression())
               else if atName()
                    then nextName()
                         (if token = colonDashToken
                          then nextToken()
                               nextExpression())
                    else nextToken(colonDashToken, ''":−" expected'')
                         nextExpression())
         else nextTerm()
              nextNewline()
              nextName()
              (if token = colonDashToken
               then nextToken()
                    nextExpression()))))

!  NEXT EXPRESSIONS. Parse zero or more single expressions separated by commas.

  nextExpressions :−
   (form () void:
    (if atTerm()
     then nextNeededExpressions()))

!  NEXT NAME. Parse a plain, quoted, or secret name.

  nextName :−
   (proc () void:
    (with string name :− tokenChars{string}
     do (case token
         of plainNameToken: parsed(''plain name (%s)'':  name)
           quotedNameToken: parsed(''quoted name (%s)'': name)
           secretNameToken: parsed(''secret name (%s)'': name)
                      none: error(''Name expected''))
        nextToken()))

!  NEXT NEEDED EXPRESSIONS. Parse one or more expressions separated by commas.

  nextNeededExpressions :−
   (form () void:
     nextExpression()
     nextNewline()
     (while token = commaToken
      do nextToken()
         nextExpression()
         nextNewline()))

!  NEXT PARAMETERS. Parse zero or more parameters separated by commas.

  nextParameters :−
   (form () void:
    (with

!  NEXT PARAMETER. Parse a parameter: either a term followed by a name, or else
!  just a term.

      nextParameter :−
       (form () void:
        (in parsing(''parameter'')
         do nextTerm()
            nextNewline()
            (if atName()
             then nextName()
                  nextNewline())))

!  This is NEXT PARAMETERS's body.

     do (in parsing(''parameters'')
         do (if atTerm()
             then nextParameter()
                  (while token = commaToken
                   do nextToken()
                      nextParameter())))))

!  NEXT SEQUENCE. Parse a subsequence, optionally followed by ALSO, and another
!  SUBSEQUENCE.

  nextSequence :−
   (proc () void:
    (in parsing(''sequence'')
     do nextSubsequence()
        (if token = boldAlsoToken
         then parsed(''infix (boldAlso)'')
              nextToken()
              nextSubsequence())))

!  NEXT SUBSEQUENCE. Parse one or more expressions separated by semicolons.

  nextSubsequence :−
   (proc () void:
    (in parsing(''subsequence'')
     do nextExpression()
        (while
         (if token = newlineToken ∨ token = semicolonToken
          then parsed(''infix (semicolon)'')
               nextToken()
               (if atTerm()
                then nextExpression()
                     true
                else false)
          else false))))

!  NEXT EXPRESSION. Parse one or more disjunctions separated by assigners.

  nextExpression :−
   (proc () void:
    (in parsing(''expression'')
     do nextDisjunction()
        (if atAssigner()
         then parsed(''infix (%s)'': tokenChars{string})
              nextToken()
              nextDisjunction())))

!  NEXT DISJUNCTION. Parse one or more conjunctions separated by ORs.

  nextDisjunction :−
   (proc () void:
    (in parsing(''disjunction'')
     do nextConjunction()
        (while token = boldOrToken
         do parsed(''infix (boldOr)'')
            nextToken()
            nextConjunction())))

!  NEXT CONJUNCTION. Parse one or more comparsions separated by ANDs.

  nextConjunction :−
   (proc () void:
    (in parsing(''conjunction'')
     do nextComparison()
        (while token = boldAndToken
         do parsed(''infix (boldAnd)'')
            nextToken()
            nextComparison())))

!  NEXT COMPARISON. Parse one or more sums separated by comparison operators.

  nextComparison :−
   (proc () void:
    (in parsing(''comparison'')
     do nextSum()
        (while atComparison()
         do parsed(''infix (%s)'': tokenChars{string})
            nextToken()
            nextSum())))

!  NEXT SUM. Parse one or more products separated by sum operators.

  nextSum :−
   (proc () void:
    (in parsing(''sum'')
     do nextProduct()
        (while atSum()
         do parsed(''infix (%s)'': tokenChars{string})
            nextToken()
            nextSum())))

!  NEXT PRODUCT. Parse one or more terms separated by product operators.

  nextProduct :−
   (proc () void:
    (in parsing(''product'')
     do nextTerm()
        (while atProduct()
         do parsed(''infix (%s)'': tokenChars{string})
            nextToken()
            nextTerm())))

!  NEXT TERM. Parse a term.

  nextTerm :−
   (proc () void:
    (with

!  NEXT FORM TERM. Parse a FORM type.

      nextFormTerm :−
       (form () void:
        (in parsing(''form term'')
         do nextToken()
            nextToken(openParenToken, ''"(" expected'')
            nextParameters()
            nextToken(closeParenToken, ''")" expected'')
            nextNewline()
            nextTerm()))

!  NEXT GEN TERM. Parse a GENeric form type.

      nextGenTerm :−
       (form () void:
        (in parsing(''gen term'')
         do (while
              nextToken()
              nextToken(openParenToken, ''"(" expected'')
              nextParameters()
              nextToken(closeParenToken, ''")" expected'')
              nextNewline()
              token = boldGenToken)
            (if token = boldFormToken
             then nextFormTerm()
             else error(''"form" expected''))))

!  NEXT PROC TERM. Parse a PROC type.

      nextProcTerm :−
       (form () void:
        (in parsing(''proc term'')
         do nextToken()
            nextToken(openParenToken, ''"(" expected'')
            nextParameters()
            nextToken(closeParenToken, ''")" expected'')
            nextNewline()
            nextTerm()))

!  NEXT OPEN BRACE TERM. Parse { A } T, where A is a series of arguments, and T
!  is a term.

      nextOpenBraceTerm :−
       (form () void:
        (in parsing(''open brace term'')
         do nextToken()
            nextArguments()
            nextToken(closeBraceToken, ''Close brace expected'')
            nextNewline()
            nextTerm()))

!  NEXT OPEN BRACKET TERM. Parse [ A ] T, where A is a series of arguments, and
!  T is aterm.

      nextOpenBracketTerm :−
       (form () void:
        (in parsing(''open bracket term'')
         do nextToken()
            nextArguments()
            nextToken(closeBracketToken, ''Close bracket expected'')
            nextNewline()
            nextTerm()))

!  NEXT PREFIX TERM. Parse a term preceded by a prefix operator.

      nextPrefixTerm :−
       (proc () void:
        (in parsing(''prefix term'')
         do parsed(''prefix (%s)'': tokenChars{string})
            nextToken()
            nextTerm()))

!  NEXT REF TERM. Parse a term preceded by REF.

      nextRefTerm :−
       (form () void:
        (in parsing(''ref term'')
         do nextToken()
            nextTerm()))

!  NEXT ROW TERM. Parse a term preceded by ROW.

      nextRowTerm :−
       (form () void:
        (in parsing(''row term'')
         do nextToken()
            nextTerm()))

!  NEXT TYPE TERM. Parse a term preceded by TYPE.

      nextTypeTerm :−
       (form () void:
        (in parsing(''type term'')
         do nextToken()
            nextTerm()))

!  NEXT VAR TERM. Parse a term preceded by VAR.

      nextVarTerm :−
       (form () void:
        (in parsing(''var term'')
         do nextToken()
            nextTerm()))

!  This is NEXT TERM's body.

     do (in parsing(''term'')
         do (case token
             of boldFormToken: nextFormTerm()
                 boldGenToken: nextGenTerm()
                boldProcToken: nextProcTerm()
                 boldRefToken: nextRefTerm()
                 boldRowToken: nextRowTerm()
                boldTypeToken: nextTypeTerm()
                 boldVarToken: nextVarTerm()
               openBraceToken: nextOpenBraceTerm()
             openBracketToken: nextOpenBracketTerm()
                  prefixToken: nextPrefixTerm()
               sumPrefixToken: nextPrefixTerm()
                         none: nextUnit()))))

!  NEXT UNIT. Parse a unit.

  nextUnit :−
   (proc () void:
    (with

!  NEXT NONE UNIT. Parse a NONE outside a CASE label. It's always an error.

      nextNoneUnit :−
       (form () void:
         error(''"none" outside label''))

!  NEXT CHAR UNIT. Parse a character literal.

      nextCharUnit :−
       (form () void:
        (if tracingParser
         then (in indent
               do write(' '))
              write('' char ('')
              write(tokenChars{string})
              writeln(')'))
         nextToken())

!  NEXT DOLLAR UNIT. Parse $ N, where N is a name.

      nextDollarUnit :−
       (form () void:
         parsed(''dollar'')
         nextToken()
         nextName())

!  NEXT HOOK UNIT. Parse a hook.

      nextHookUnit :−
       (form () void:
         parsed(''hook (?%s)'': tokenChars)
         nextToken())

!  NEXT INTEGER UNIT. Parse an integer literal.

      nextIntegerUnit :−
       (form () void:
         parsed(''integer (%i#%s)'': tokenRadix, tokenChars{string})
         nextToken())

!  NEXT PLAIN NAME UNIT. Parse a plain name.

      nextPlainNameUnit :−
       (form () void:
         parsed(''plain name (%s)'': tokenChars{string})
         nextToken())

!  NEXT REAL UNIT. Parse a real literal.

      nextRealUnit :−
       (form () void:
         parsed(''real (%s)'': tokenChars{string})
         nextToken())

!  NEXT QUOTED NAME UNIT. Parse a quoted name.

      nextQuotedNameUnit :−
       (form () void:
         parsed(''quoted name ("%s")'': tokenChars{string})
         nextToken())

!  NEXT SECRET NAME UNIT. Parse a secret name.

      nextSecretNameUnit :−
       (form () void:
         parsed(''secret name (%s)'': tokenChars{string})
         nextToken())

!  NEXT STRING UNIT. Parse a string literal.

      nextStringUnit :−
       (form () void:
         parsed(''string (%s)'': tokenChars{string})
         nextToken())

!  This is NEXT UNIT's body.

     do (in parsing(''unit'')
         do (case token
             of boldNoneToken: nextNoneUnit()
                    charToken: nextCharUnit()
                  dollarToken: nextDollarUnit()
                    hookToken: nextHookUnit()
                 integerToken: nextIntegerUnit()
               openParenToken: nextClause()
               plainNameToken: nextPlainNameUnit()
              quotedNameToken: nextQuotedNameUnit()
                    realToken: nextRealUnit()
              secretNameToken: nextSecretNameUnit()
                  stringToken: nextStringUnit()
                         none: error(''Unit expected''))
            nextPostfix())))

!  NEXT POSTFIX. Parse one or more postfix operators that can follow a unit.

  nextPostfix :−
   (form () void:
    (with

!  NEXT DOT POSTFIX. Parse . N, where N is a name.

      nextDotPostfix :−
       (form () bool:
         parsed(''dot postfix'')
         nextToken()
         nextName()
         true)

!  NEXT OPEN BRACE POSTFIX. Parse [ A ], where A is a series of arguments.

      nextOpenBracePostfix :−
       (form () bool:
        (in parsing(''open brace postfix'')
         do nextToken()
            nextArguments()
            nextToken(closeBraceToken, ''Close brace expected''))
         true)

!  NEXT OPEN BRACKET POSTFIX. Parse { A }, where A is a series of arguments.

      nextOpenBracketPostfix :−
       (form () bool:
        (in parsing(''open bracket postfix'')
         do nextToken()
            nextArguments()
            nextToken(closeBracketToken, ''Close bracket expected''))
        true)

!  NEXT OPEN PAREN POSTFIX. Parse ( A ), where A is a series of arguments.

      nextOpenParenPostfix :−
       (form () bool:
        (in parsing(''open paren postfix'')
         do nextToken()
            nextArguments()
            nextToken(closeParenToken, ''Close paren expected''))
        true)

!  NEXT SIMPLE POSTFIX. Parse a one-token postfix operator.

      nextSimplePostfix :−
       (form () bool:
         parsed(''postfix (%s)'': tokenChars{string})
         nextToken()
         true)         

!  This is NEXT POSTFIX's body.

     do (while
         (case token
          of dotToken:         nextDotPostfix()
             openBraceToken:   nextOpenBracePostfix()
             openBracketToken: nextOpenBracketPostfix()
             openParenToken:   nextOpenParenPostfix()
             postfixToken:     nextSimplePostfix()
             none:             false))))

!  NEXT CLAUSE. Parse a clause, or a sequence in parentheses.

  nextClause :−
   (proc () void:
    (with

!  NEXT ALT CLAUSE. Parse an ALT clause.

      nextAltClause :−
       (form () void:
        (in parsing(''alt clause'')
         do nextToken()
            nextExpressions()))

!  NEXT ALTS CLAUSE. Parse an ALTS clause.

      nextAltsClause :−
       (form () void:
        (in parsing(''alts clause'')
         do nextToken()
            nextExpressions()))

!  NEXT CASE CLAUSE. Parse a CASE clause.

      nextCaseClause :−
       (form () void:
        (with

!  NEXT LABEL. Parse a label, either NONE or a series of expressions.

          nextLabel :−
           (form () void:
            (if token = boldNoneToken
             then parsed(''none'')
                  nextToken()
             else nextNeededExpressions()))

!  This is NEXT CASE CLAUSE's body.

         do (in parsing(''case clause'')
             do nextToken()
                nextSequence()
                nextToken(boldOfToken, ''"of" expected'')
                (while
                 (if atTerm()
                  then nextLabel()
                       nextToken(colonToken, ''":" expected'')
                       nextExpression()
                       (if token = newlineToken ∨ token = semicolonToken
                        then nextToken()
                             true
                        else false)
                  else false)))))

!  NEXT CATCH CLAUSE. Parse a CATCH clause.

      nextCatchClause :−
       (form () void:
        (in parsing(''catch clause'')
         do nextToken()
            nextSequence()))

!  NEXT FOR CLAUSE. Parse a FOR clause.

      nextForClause :−
       (form () void:
        (in parsing(''for clause'')
         do nextToken()
            nextParameters()
            (if token = boldInToken
             then nextToken()
                  nextNeededExpressions())
            nextToken(boldDoToken, ''"do" expected'')
            nextSequence()))

!  NEXT FORM CLAUSE. Parse a FORM clause or a FORM term in parentheses.

      nextFormClause :−
       (form () void:
        (in parsing(''form clause'')
         do nextToken()
            nextToken(openParenToken, ''"(" expected'')
            nextParameters()
            nextToken(closeParenToken, ''")" expected'')
            nextNewline()
            nextTerm()
            nextNewline()
            (if token = colonToken
             then nextToken()
                  nextSequence())))

!  NEXT GEN CLAUSE. Parse a GENeric form clause or a GEN term in parentheses.

      nextGenClause :−
       (form () void:
        (in parsing(''gen clause'')
         do (while
              nextToken()
              nextToken(openParenToken, ''"(" expected'')
             nextParameters()
              nextToken(closeParenToken, ''")" expected'')
              nextNewline()
              token = boldGenToken)
            nextToken(boldFormToken, ''"form" expected'')
            nextToken(openParenToken, ''"(" expected'')
            nextParameters()
            nextToken(closeParenToken, ''")" expected'')
            nextNewline()
            nextTerm()
            nextNewline()
            (if token = colonToken
             then nextToken()
                  nextSequence())))

!  NEXT IF CLAUSE. Parse an IF clause.

      nextIfClause :−
       (form () void:
        (in parsing(''if clause'')
         do (while
              nextToken()
              nextSequence()
              nextToken(boldThenToken, ''"then" expected'')
              nextSequence()
              (if token = boldElseToken
               then nextToken()
                    (if token = boldIfToken
                     then true
                     else nextSequence()
                          false)
               else false))))

!  NEXT IN CLAUSE. Parse a FOR clause without its FOR part.

      nextInClause :−
       (form () void:
        (in parsing(''in clause'')
         do nextToken()
            nextNeededExpressions()
            nextNewline()
            nextToken(boldDoToken, ''"do" expected'')
            nextSequence()))

!  NEXT PAST CLAUSE. Parse a PAST clause.

      nextPastClause :−
       (form () void:
        (in parsing(''past clause'')
         do nextToken()
            (if atName()
             then nextName()
             else error(''Name expected.''))))

!  NEXT PROC CLAUSE. Parse a PROC clause or a PROC term in parentheses.

      nextProcClause :−
       (form () void:
        (in parsing(''proc clause'')
         do nextToken()
            nextToken(openParenToken, ''"(" expected'')
            nextParameters()
            nextToken(closeParenToken, ''")" expected'')
            nextNewline()
            nextTerm()
            nextNewline()
            (if token = colonToken
             then nextToken()
                  nextSequence())))

!  NEXT TUPLE CLAUSE. Parse a TUPLE clause.

      nextTupleClause :−
       (form () void:
        (in parsing(''tuple clause'')
         do nextToken()
            nextParameters()))

!  NEXT WITH CLAUSE. Parse a WITH clause.

      nextWithClause :−
       (form () void:
        (in parsing(''with clause'')
         do nextToken()
            (while token ≠ boldDoToken
             do nextEquate()
                (if token = newlineToken ∨ token = semicolonToken
                 then nextToken()
                 else if atTerm()
                      then error(''";" or newline expected'')))
            nextToken(boldDoToken, ''"do" expected'')
            nextSequence()))

!  NEXT WHILE CLAUSE. Parse a WHILE clause.

      nextWhileClause :−
       (form () void:
        (in parsing(''while clause'')
         do nextToken()
            nextSequence()
            (if token = boldDoToken
             then nextToken()
                  nextSequence())))

!  NEXT COLON CLAUSE. Parse a list constructor.

      nextColonClause :−
       (form () void:
        (in parsing(''colon clause'')
         do nextToken()
            nextExpressions()))

!  This is NEXT CLAUSE's body.

     do (in parsing(''clause'')
         do nextToken()
            (case token
             of boldAltToken: nextAltClause()
               boldAltsToken: nextAltsClause()
               boldCaseToken: nextCaseClause()
              boldCatchToken: nextCatchClause()
                boldForToken: nextForClause()
               boldFormToken: nextFormClause()
                boldGenToken: nextGenClause()
                 boldIfToken: nextIfClause()
                 boldInToken: nextInClause()
               boldPastToken: nextPastClause()
               boldProcToken: nextProcClause()
              boldTupleToken: nextTupleClause()
               boldWithToken: nextWithClause()
              boldWhileToken: nextWhileClause()
                  colonToken: nextColonClause()
                        none: nextSequence())
            nextToken(closeParenToken, ''")" expected''))))

!  NEXT PROGRAM. Parse a program.

  nextProgram :−
   (proc () void:
    (with

!  INITIALIZE. Initialize the character reader and token scanner.

      initialize :−
       (form () void:
         lineNumber := 0
         empty(tokenChars)
         tokenEndsTerm := false
         nextLine()
         nextChar()
         nextToken())

!  NEXT LOAD. Parse a LOAD clause.

      nextLoad :−
       (form () void:
        (in parsing(''load clause'')
         do nextToken()
            nextSequence()))

!  NEXT PROG. Parse a PROG clause.

      nextProg :−
       (form () void:
        (in parsing(''prog clause'')
         do nextToken()
            (while token ≠ closeParenToken
             do nextEquate()
                (if token = newlineToken ∨ token = semicolonToken
                 then nextToken()
                 else if atTerm()
                      then error(''";" or newline expected'')))))

!  This is NEXT PROGRAM's body.

     do (in parsing(''file ("%s")'': path)
         do initialize()
            (while token ≠ endFileToken
             do (if token = openParenToken
                 then nextToken()
                      (case token
                       of boldLoadToken: nextLoad()
                          boldProgToken: nextProg()
                                   none: error(''"load" or "prog" expected''))
                      nextToken(closeParenToken, ''")" expected'')
                      (if token = newlineToken ∨ token = semicolonToken
                       then nextToken()
                       else if atTerm()
                            then error(''";" or newline expected''))
                 else error(''"(" expected''))))))

!  MAIN. If the command line has no arguments, then parse a program from INPUT.
!  Otherwise the arguments are taken to be file pathnames and we parse programs
!  from the named files, in order of appearance.

  main :−
   (if argc() = 1
    then path := ''input''
         source := input
         nextProgram()
    else (for int index in 1, argc() − 1
          do path := argv()[index]
             (if ¬ open(source, path, ''r'')
              then writeln(''%s: Can't open "%s".'': self, path)
                   exit(1))
             nextProgram()
             (if ¬ close(source)
              then writeln(''%s: Can't open "%s".'': self, path)
                   exit(1))))
)
