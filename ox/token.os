!
!  OX/TOKEN. Group characters into tokens.
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

(prog
  char "'" :− '\''              !  Apostrophe.
  char "\" :− '\\'              !  Backslash.
  char eos :− (past eos){char}  !  End of source file.

  foj makeToken         :− enum()       !  Make tokens with this.
  inj assignerToken     :− makeToken()  !  A symbol of the form "⊗=".
  inj boldAlsoToken     :− makeToken()  !  The name "ALSO".
  inj boldAltToken      :− makeToken()  !  The name "ALT".
  inj boldAltsToken     :− makeToken()  !  The name "ALTS".
  inj boldCaseToken     :− makeToken()  !  The name "CASE".
  inj boldCatchToken    :− makeToken()  !  The name "CATCH".
  inj boldDoToken       :− makeToken()  !  The name "DO".
  inj boldElseToken     :− makeToken()  !  The name "ELSE".
  inj boldForToken      :− makeToken()  !  The name "FOR".
  inj boldFormToken     :− makeToken()  !  The name "FORM".
  inj boldGenToken      :− makeToken()  !  The name "GEN".
  inj boldIfToken       :− makeToken()  !  The name "IF".
  inj boldInToken       :− makeToken()  !  The name "IN".
  inj boldLoadToken     :− makeToken()  !  The name "LOAD".
  inj boldNoneToken     :− makeToken()  !  The name "NONE".
  inj boldOfToken       :− makeToken()  !  The name "OF".
  inj boldPastToken     :− makeToken()  !  The name "PAST".
  inj boldProcToken     :− makeToken()  !  The name "PROC".
  inj boldProgToken     :− makeToken()  !  The name "PROG".
  inj boldThenToken     :− makeToken()  !  The name "THEN".
  inj boldTupleToken    :− makeToken()  !  The name "TUPLE".
  inj boldWhileToken    :− makeToken()  !  The name "WHILE".
  inj boldWithToken     :− makeToken()  !  The name "WITH".
  inj closeBraceToken   :− makeToken()  !  The symbol "}".
  inj closeBracketToken :− makeToken()  !  The symbol "]".
  inj closeParenToken   :− makeToken()  !  The symbol ")".
  inj colonToken        :− makeToken()  !  The symbol ":".
  inj colonDashToken    :− makeToken()  !  The symbol ":-" or ":−".
  inj commaToken        :− makeToken()  !  The symbol ",".
  inj comparisonToken   :− makeToken()  !  A comparison operator symbol.
  inj conjunctionToken  :− makeToken()  !  The name "AND" or the symbol "∧".
  inj constantToken     :− makeToken()  !  A CHAR, INT, or STRING constant.
  inj disjunctionToken  :− makeToken()  !  The name "OR" or the symbol "∨".
  inj dollarToken       :− makeToken()  !  The symbol "$".
  inj dotToken          :− makeToken()  !  The symbol ".".
  inj endToken          :− makeToken()  !  End of file.
  inj hookToken         :− makeToken()  !  A name prefixed by "?".
  inj ignoredToken      :− makeToken()  !  A token ignored by NEXT TOKEN.
  inj nameToken         :− makeToken()  !  A plain, quoted, or secret name.
  inj newlineToken      :− makeToken()  !  End of line.
  inj openBraceToken    :− makeToken()  !  The symbol "{".
  inj openBracketToken  :− makeToken()  !  The symbol "[".
  inj openParenToken    :− makeToken()  !  The symbol "(".
  inj postfixToken      :− makeToken()  !  A postfix operator symbol.
  inj prefixToken       :− makeToken()  !  A prefix operator symbol.
  inj productToken      :− makeToken()  !  A product operator symbol.
  inj semicolonToken    :− makeToken()  !  The symbol ";".
  inj sumToken          :− makeToken()  !  A sum operator symbol.
  inj sumPrefixToken    :− makeToken()  !  A sum or prefix operator symbol.

  var int                   token       !  Current token.
  var buffer(maxNameLength) tokenChars  !  Chars in current token.
  var bool                  tokenEnds   !  Does TOKEN end a unit?

!  NEXT TOKEN. Token scanner. Advance TOKEN to the next token in SOURCE. If the
!  token corresponds to a name, then record the name and its TABLE NUMBER.

  nextToken :−
   (alt
    (form () void:
      NextToken()),
    (form (int whatToken) void:
     (if token = whatToken
      then nextToken()
      else syntaxError())))

  NextToken :−
   (proc () void:
    (with

!  IS DIGIT. Extend IS DIGIT so that '_' is a digit.

      isDigit :−
       (alt
        (form (char ch) bool:
         (past isDigit)(ch) ∨ ch = '_'),
        (form (char ch, int radix) bool:
         (past isDigit)(ch, radix) ∨ ch = '_'))

!  NEXT APOSTROPHE. Scan a char constant or a string constant.

      nextApostrophe :−
       (form () void:
        (with

!  NEXT SLASHABLE. Advance CH past the next char in a char or string constant.

          nextSlashable :−
           (form () void:
            (if ch = "\"
             then nextChar()
                  (if ch = '#'
                   then nextChar()
                        (while isDigit(ch, 16)
                         do nextChar())
                   else nextChar())
             else if ch ≠ eol
                  then nextChar()))

!  This is NEXT APOSTROPHE's body.

         do token := constantToken
            tokenEnds := true
            nextChar()
            (if ch = "'"
             then nextChar()
                  (while
                   (if ch = "'"
                    then nextChar()
                         (if ch = "'"
                          then nextChar()
                               (if ch = "'"
                                then nextChar())
                               false
                          else if ch = eol
                               then false
                               else nextSlashable()
                                    true)
                    else if ch = eol
                         then false
                         else nextSlashable()
                              true))
             else if ch ≠ eol
                  then nextSlashable()
                       nextChar())))

!  NEXT COLON. Scan ":", ":-", ":=", or ":−". The dashes are U+002D and U+2212.

      nextColon :−
       (form () void:
         tokenEnds := false
         nextChar()
         (if ch = '-' ∨ ch = '−'
          then token := colonDashToken
               nextChar()
          else if ch = '='
               then token := assignerToken
                    append(tokenChars, ''":="'')
                    recordName(tokenChars, tableNumber)
                    nextChar()
               else token := colonToken))

!  NEXT COMMENT. Scan a comment without advancing TOKEN, so it is equivalent to
!  a blank. We skip chars until we find the end of the line. However, we do not
!  skip the end of line char, because it might be treated as a semicolon token,
!  depending on TOKEN ENDS.

      nextComment :−
       (form () void:
        (while
         (if ch = eol ∨ ch = eos
          then false
          else nextChar()
               true)))

!  NEXT COMPARER. Scan a comparison operator that's a single char.

      nextComparer :−
       (proc () void:
         token := comparisonToken
         append(tokenChars, ch)
         nextChar())

!  NEXT DELIMITER. Scan a delimiter that's a single char.

      nextDelimiter :−
       (form (int token', bool tokenEnds') void:
         token := token'
         tokenEnds := tokenEnds'
         nextChar())

!  NEXT END FILE. Scan an end of file.

      nextEndFile :−
       (form () void:
         token := endToken
         tokenEnds := false)

!  NEXT END LINE. Scan the end of a line. If the previous token can be the last
!  token of a unit, then we treat the end of the line as a NEWLINE TOKEN. If it
!  cannot be, then we treat it as an IGNORED TOKEN.

      nextEndLine :−
       (form () void:
         nextChar()
         (if tokenEnds
          then tokenEnds := false
               token := newlineToken))

!  NEXT GREATER. Scan ">", ">>", ">>=", or ">=".

      nextGreater :−
       (form () void:
         tokenEnds := false
         nextChar()
         (if ch = '>'
          then append(tokenChars, ''">>'')
               nextChar()
               (if ch = '='
                then token := assignerToken
                     append(tokenChars, '=')
                     nextChar()
                else token := productToken)
               append(tokenChars, '"')
               recordName(tokenChars, tableNumber)
          else token := comparisonToken
               append(tokenChars, '>')
               (if ch = '='
                then append(tokenChars, '=')
                     nextChar())))

!  NEXT HEXADECIMAL NUMBER. Scan a hexadecimal numeric constant.

      nextHexadecimalNumber :−
       (proc () void:
         token := constantToken
         tokenEnds := true
         (while
           nextChar()
           isDigit(ch, 16)))

!  NEXT HOOK. Scan a hook.

      nextHook :−
       (proc () void:
         token := hookToken
         tokenEnds := true
         append(tokenChars, '?')
         nextChar()
         (while
           append(tokenChars, ch)
           nextChar()
           isPlain(ch))
         recordName(tokenChars, tableNumber))

!  NEXT INFIX. Scan an operator that's a single char, maybe followed by '='.

      nextInfix :−
       (proc (int whatToken) void:
         tokenEnds := false
         append(tokenChars, '"')
         append(tokenChars, ch)
         nextChar()
         (if ch = '='
          then token := assignerToken
               append(tokenChars, '=')
               nextChar()
          else token := whatToken)
         append(tokenChars, '"')
         recordName(tokenChars, tableNumber))

!  NEXT JUNCTION. Scan "∧" or "∨". We treat these symbols as names because they
!  are abbreviations for "AND" and "OR".

      nextJunction :−
       (proc (int whatToken) void:
         token := whatToken
         tokenEnds := false
         append(tokenChars, ch)
         recordName(tokenChars, tableNumber)
         nextChar())

!  NEXT LESS. Scan "<", "<<", "<>", "<<=", or "<=".

      nextLess :−
       (form () void:
         tokenEnds := false
         nextChar()
         (if ch = '<'
          then append(tokenChars, ''"<<'')
               nextChar()
               (if ch = '='
                then token := assignerToken
                     append(tokenChars, '=')
                     nextChar()
                else token := productToken)
               append(tokenChars, '"')
               recordName(tokenChars, tableNumber)
          else token := comparisonToken
               append(tokenChars, '<')
               (if ch = '=' ∨ ch = '>'
                then append(tokenChars, ch)
                     nextChar())))

!  NEXT NAME. Scan a name starting with a lower case Roman letter.

      nextName :−
       (proc () void:
        (while
         append(tokenChars, ch)
         nextChar()
         isPlain(ch))
        token :=
         select(tokenChars{string}, nameToken:
          (:  ''also'', boldAlsoToken),
          (:   ''alt'', boldAltToken),
          (:  ''alts'', boldAltsToken),
          (:   ''and'', conjunctionToken),
          (:  ''case'', boldCaseToken),
          (: ''catch'', boldCatchToken),
          (:    ''do'', boldDoToken),
          (:  ''else'', boldElseToken),
          (:   ''for'', boldForToken),
          (:  ''form'', boldFormToken),
          (:   ''gen'', boldGenToken),
          (:    ''if'', boldIfToken),
          (:    ''in'', boldInToken),
          (:  ''load'', boldLoadToken),
          (:   ''mod'', productToken),
          (:  ''none'', boldNoneToken),
          (:   ''not'', prefixToken),
          (:    ''of'', boldOfToken),
          (:    ''or'', disjunctionToken),
          (:  ''past'', boldPastToken),
          (:  ''proc'', boldProcToken),
          (:  ''prog'', boldProgToken),
          (:   ''ref'', prefixToken),
          (:   ''row'', prefixToken),
          (:  ''then'', boldThenToken),
          (: ''tuple'', boldTupleToken),
          (:  ''type'', prefixToken),
          (:   ''var'', prefixToken),
          (: ''while'', boldWhileToken),
          (:  ''with'', boldWithToken))
        tokenEnds := (token = nameToken)
        recordName(tokenChars, tableNumber))

!  NEXT NON INFIX. Scan an operator that's a single char, not followed by '='.

      nextNonInfix :−
       (proc (int token', bool tokenEnds') void:
         token := token'
         tokenEnds := tokenEnds'
         append(tokenChars, '"')
         append(tokenChars, ch)
         append(tokenChars, '"')
         nextChar()
         recordName(tokenChars, tableNumber))

!  NEXT NUMBER. Scan a numeric constant. The dashes are U+002D and U+2212.

      nextNumber :−
       (proc () void:
         token := constantToken
         tokenEnds := true
         (while
           nextChar()
           isDigit(ch))
         (if ch = '#'
          then nextChar()
               (while isDigit(ch, 36)
                do nextChar())
          else if ch = '.'
               then nextChar()
                    (while isDigit(ch)
                     do nextChar())
                    (if ch = 'e' ∨ ch = 'E'
                     then nextChar()
                          (if ch = '+' ∨ ch = '-' ∨ ch = '−'
                           then nextChar())
                          (while isDigit(ch)
                           do nextChar()))))

!  NEXT QUOTED NAME. Scan a quoted name.

      nextQuotedName :−
       (form () void:
         token := nameToken
         tokenEnds := true
         append(tokenChars, '"')
         nextChar()
         (while
          (if ch = eol ∨ ch = '"'
           then nextChar()
                false
           else (if ch ≠ ' ' ∨ start(tokenChars) ≠ ' '
                 then append(tokenChars, ch))
                nextChar()
                true))
         append(tokenChars, '"')
         recordName(tokenChars, tableNumber))

!  This is NEXT TOKEN's body. Initialize NAME and scan tokens until we find one
!  that's not IGNORED TOKEN.

     do empty(tokenChars)
        token := ignoredToken
        (while
         (case ch
          of eol: nextEndLine()
             eos: nextEndFile()
             ' ': nextChar()
             '!': nextComment()
             '"': nextQuotedName()
             '#': nextHexadecimalNumber()
             '$': nextDelimiter(dollarToken, false)
             '&': nextInfix(productToken)
             "'": nextApostrophe()
             '(': nextDelimiter(openParenToken, false)
             ')': nextDelimiter(closeParenToken, true)
             '*': nextInfix(productToken)
             '+': nextInfix(sumPrefixToken)
             ',': nextDelimiter(commaToken, false)
             '-': nextInfix(sumPrefixToken)
             '.': nextNonInfix(dotToken, false)
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
             ';': nextDelimiter(semicolonToken, false)
             '<': nextLess()
             '=': nextComparer()
             '>': nextGreater()
             '?': nextHook()
             '@': nextNonInfix(postfixToken, true)
             'A': nextName()
             'B': nextName()
             'C': nextName()
             'D': nextName()
             'E': nextName()
             'F': nextName()
             'G': nextName()
             'H': nextName()
             'I': nextName()
             'J': nextName()
             'K': nextName()
             'L': nextName()
             'M': nextName()
             'N': nextName()
             'O': nextName()
             'P': nextName()
             'Q': nextName()
             'R': nextName()
             'S': nextName()
             'T': nextName()
             'U': nextName()
             'V': nextName()
             'W': nextName()
             'X': nextName()
             'Y': nextName()
             'Z': nextName()
             '[': nextDelimiter(openBracketToken, false)
             ']': nextDelimiter(closeBracketToken, true)
             '^': nextNonInfix(postfixToken, true)
             'a': nextName()
             'b': nextName()
             'c': nextName()
             'd': nextName()
             'e': nextName()
             'f': nextName()
             'g': nextName()
             'h': nextName()
             'i': nextName()
             'j': nextName()
             'k': nextName()
             'l': nextName()
             'm': nextName()
             'n': nextName()
             'o': nextName()
             'p': nextName()
             'q': nextName()
             'r': nextName()
             's': nextName()
             't': nextName()
             'u': nextName()
             'v': nextName()
             'w': nextName()
             'x': nextName()
             'y': nextName()
             'z': nextName()
             '{': nextDelimiter(openBraceToken, false)
             '|': nextInfix(sumToken)
             '}': nextDelimiter(closeBraceToken, true)
             '~': nextInfix(sumPrefixToken)
             '¬': nextNonInfix(prefixToken, false)
             '×': nextInfix(productToken)
             'Γ': nextName()
             'Δ': nextName()
             'Θ': nextName()
             'Λ': nextName()
             'Ξ': nextName()
             'Π': nextName()
             'Σ': nextName()
             'Φ': nextName()
             'Ψ': nextName()
             'Ω': nextName()
             'α': nextName()
             'β': nextName()
             'γ': nextName()
             'δ': nextName()
             'ε': nextName()
             'ζ': nextName()
             'η': nextName()
             'θ': nextName()
             'ι': nextName()
             'κ': nextName()
             'λ': nextName()
             'μ': nextName()
             'ν': nextName()
             'ξ': nextName()
             'π': nextName()
             'ρ': nextName()
             'ς': nextName()
             'σ': nextName()
             'τ': nextName()
             'υ': nextName()
             'φ': nextName()
             'χ': nextName()
             'ψ': nextName()
             'ω': nextName()
             'ϑ': nextName()
             'ϒ': nextName()
             'ϕ': nextName()
             'ϖ': nextName()
             'ϱ': nextName()
             'ϵ': nextName()
             '₀': nextName()
             '₁': nextName()
             '₂': nextName()
             '₃': nextName()
             '₄': nextName()
             '₅': nextName()
             '₆': nextName()
             '₇': nextName()
             '₈': nextName()
             '₉': nextName()
             '←': nextInfix(productToken)
             '↑': nextNonInfix(postfixToken, true)
             '→': nextInfix(productToken)
             '↓': nextNonInfix(postfixToken, true)
             '∀': nextName()
             '∃': nextName()
             '∅': nextName()
             '∊': nextComparer()
             '∍': nextComparer()
             '−': nextInfix(sumPrefixToken)
             '∘': nextInfix(sumToken)
             '∞': nextName()
             '∧': nextJunction(conjunctionToken)
             '∨': nextJunction(disjunctionToken)
             '∩': nextInfix(productToken)
             '∪': nextInfix(sumToken)
             '≠': nextComparer()
             '≡': nextComparer()
             '≢': nextComparer()
             '≤': nextComparer()
             '≥': nextComparer()
             '≺': nextComparer()
             '≻': nextComparer()
             '≼': nextComparer()
             '≽': nextComparer()
             '⊂': nextComparer()
             '⊃': nextComparer()
             '⊆': nextComparer()
             '⊇': nextComparer()
             '⊏': nextComparer()
             '⊐': nextComparer()
             '⊑': nextComparer()
             '⊒': nextComparer()
             '⊓': nextInfix(productToken)
             '⊔': nextInfix(sumToken)
             '⊕': nextInfix(sumPrefixToken)
             '⊖': nextInfix(sumPrefixToken)
             '⊗': nextInfix(productToken)
             '⊘': nextInfix(productToken)
             '⊙': nextInfix(productToken)
             '⊥': nextName()
             '⋅': nextInfix(productToken)
            none: syntaxError())
          token = ignoredToken)))

!  SYNTAX ERROR. Terminate Ox with a message about a syntax error.

  syntaxError :−
   (form () void:
     flush(output)
     fail(''Syntax error in '%s' at line %05i.'': sourcePath, sourceNumber))

!  WRITE TOKEN. Write a representation of TOKEN to ERRPUT. This is never called
!  except when we're debugging.

  writeToken :−
   (form (int token) void:
    (case token
     of assignerToken: writeln(errput, ''assigner %s'': tokenChars{string})
        boldAlsoToken: writeln(errput, ''also'')
         boldAltToken: writeln(errput, ''alt'')
        boldAltsToken: writeln(errput, ''alts'')
        boldCaseToken: writeln(errput, ''case'')
       boldCatchToken: writeln(errput, ''catch'')
          boldDoToken: writeln(errput, ''do'')
        boldElseToken: writeln(errput, ''else'')
         boldForToken: writeln(errput, ''for'')
        boldFormToken: writeln(errput, ''form'')
         boldGenToken: writeln(errput, ''gen'')
          boldIfToken: writeln(errput, ''if'')
          boldInToken: writeln(errput, ''in'')
        boldLoadToken: writeln(errput, ''load'')
        boldNoneToken: writeln(errput, ''none'')
          boldOfToken: writeln(errput, ''of'')
        boldPastToken: writeln(errput, ''past'')
        boldProcToken: writeln(errput, ''proc'')
        boldProgToken: writeln(errput, ''prog'')
        boldThenToken: writeln(errput, ''then'')
       boldTupleToken: writeln(errput, ''tuple'')
       boldWhileToken: writeln(errput, ''while'')
        boldWithToken: writeln(errput, ''with'')
      closeBraceToken: writeln(errput, ''close brace'')
    closeBracketToken: writeln(errput, ''close bracket'')
      closeParenToken: writeln(errput, ''close paren'')
           colonToken: writeln(errput, ''colon'')
       colonDashToken: writeln(errput, ''colon dash'')
           commaToken: writeln(errput, ''comma'')
      comparisonToken: writeln(errput, ''comparison %s'': tokenChars{string})
     conjunctionToken: writeln(errput, ''conjunction %s'': tokenChars{string})
        constantToken: writeln(errput, ''constant'')
     disjunctionToken: writeln(errput, ''disjunction %s'': tokenChars{string})
          dollarToken: writeln(errput, ''dollar'')
             dotToken: writeln(errput, ''dot'')
             endToken: writeln(errput, ''end'')
            hookToken: writeln(errput, ''hook %s'': tokenChars{string})
         ignoredToken: writeln(errput, ''ignored'')
            nameToken: writeln(errput, ''name %s'': tokenChars{string})
         newlineToken: writeln(errput, ''newline'')
       openBraceToken: writeln(errput, ''open brace'')
     openBracketToken: writeln(errput, ''open bracket'')
       openParenToken: writeln(errput, ''open paren'')
         postfixToken: writeln(errput, ''postfix %s'': tokenChars{string})
          prefixToken: writeln(errput, ''prefix %s'': tokenChars{string})
         productToken: writeln(errput, ''product %s'': tokenChars{string})
       semicolonToken: writeln(errput, ''semicolon'')
             sumToken: writeln(errput, ''sum %s'': tokenChars{string})
       sumPrefixToken: writeln(errput, ''sum prefix %s'': tokenChars{string})
                 none: writeln(errput, ''unknown %i'': token)))
)
