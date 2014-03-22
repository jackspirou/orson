!
!  OX/PARSE. Recursive descent parser.
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

!  An expression like A < B < C is an abbreviation for the call "<␣<"(A, B, C),
!  and an expression like A [ B ] is an abbreviation for "␣[]"(A, B). To detect
!  names like "<␣<" and "␣[]", so we can add them to the cross reference table,
!  we must be able to parse expressions. Here we use a recursive descent parser
!  based on the complete Orson grammar, even though a much simpler parser would
!  work as well. We did this because (1) we had a complete Orson parser already
!  written, (2) using a complete parser ensures that we will not miss detecting
!  any names, and (3) using a complete parser allows extending Ox to detect how
!  names are used, not just where they appear.

(prog

!  AT POSTFIX. Test if TOKEN begins a postfix operator.

  atPostfix :−
   (form () bool:
     token = dotToken ∨
     token = openBraceToken ∨
     token = openBracketToken ∨
     token = openParenToken ∨
     token = postfixToken)

!  AT TERM. Test if TOKEN begins a term. Since terms begin expressions, we also
!  test if TOKEN begins an expression. It's convenient to imagine that NONE can
!  begin a term, even though it really can't.

  atTerm :−
   (proc () bool:
     token = boldFormToken ∨
     token = boldGenToken ∨
     token = boldNoneToken ∨
     token = boldProcToken ∨
     token = constantToken ∨
     token = dollarToken ∨
     token = hookToken ∨
     token = nameToken ∨
     token = openBraceToken ∨
     token = openBracketToken ∨
     token = openParenToken ∨
     token = prefixToken ∨
     token = sumPrefixToken)

!  TRACE. Wrapper. If TRACING is true, then write a MESSAGE before and after we
!  call BODY. Each MESSAGE is indented according to the number of nested TRACEs
!  there are. If TRACING is false, then just call BODY.

  var int indent  :− 0      !  How many columns to indent messages.
  bool    tracing :− false  !  Are we tracing?

  trace :−
   (alt
    (form (string message) foj:
      trace(message:)),
    (form (string message, list objects) foj:
     (form (form () obj body) obj:
      (if tracing
       then (in indent
             do write(errput, '' ''))
            writeln(errput, ''enter '' & message, objects)
            indent += 1
            body() also
            indent −= 1
            (in indent
             do write(errput, '' ''))
            writeln(errput, ''exit '' & message, objects)
       else body()))))

!  NEXT ARGUMENTS. Parse zero or more expressions separated by commas, followed
!  optionally by a colon and zero or more expressions separated by commas.

  nextArguments :−
   (form () void:
    (in trace(''arguments'')
     do nextExpressions()
        (if token = colonToken
         then nextToken()
              nextExpressions())))

!  NEXT EQUATE. Parse an equate. If T is a term, N is a name, and E is a single
!  expression, then an equate can be T N, T N :− E, or N :− E. Note that we use
!  a trick to parse the initial term, since we don't know if it's N or T.

  nextEquate :−
   (proc () void:
    (in trace(''equate'')
     do (if token = nameToken
         then nextToken()
              nextNewline()
              (if atPostfix()
               then nextPostfix()
                    nextNewline()
                    nextToken(nameToken)
                    (if token = colonDashToken
                     then nextToken()
                          nextExpression())
               else if token = nameToken
                    then nextToken()
                         (if token = colonDashToken
                          then nextToken()
                               nextExpression())
                    else nextToken(colonDashToken)
                         nextExpression())
         else nextTerm()
              nextNewline()
              nextToken(nameToken)
              (if token = colonDashToken
               then nextToken()
                    nextExpression()))))

!  NEXT EXPRESSIONS. Parse zero or more expressions separated by commas.

  nextExpressions :−
   (form () void:
    (in trace(''expressions'')
     do (if atTerm()
         then nextNeededExpressions())))

!  NEXT NEEDED EXPRESSIONS. Parse one or more expressions separated by commas.

  nextNeededExpressions :−
   (form () void:
    (in trace(''needed expressions'')
     do nextExpression()
        nextNewline()
        (while token = commaToken
         do nextToken()
            nextExpression()
            nextNewline())))

!  NEXT NEWLINE. If TOKEN is NEWLINE TOKEN, then skip it. We use this in places
!  where a NEWLINE TOKEN should never be treated as a SEMICOLON TOKEN.

  nextNewline :−
   (form () void:
    (if token = newlineToken
     then nextToken()))

!  NEXT PARAMETERS. Parse zero or more parameters separated by commas.

  nextParameters :−
   (form () void:
    (with

!  NEXT PARAMETER. Parse a parameter: either a term followed by a name, or else
!  simply a term.

      nextParameter :−
       (form () void:
         nextTerm()
         nextNewline()
         (if token = nameToken
          then nextToken()
               nextNewline()))

!  This is NEXT PARAMETERS's body.

     do (in trace(''parameters'')
         do (if atTerm()
             then nextParameter()
                  (while token = commaToken
                   do nextToken()
                      nextParameter())))))

!  NEXT SEQUENCE. Parse a subsequence, optionally followed by ALSO, and another
!  subsequence.

  nextSequence :−
   (proc () void:
    (in trace(''sequence'')
     do nextSubsequence()
        (if token = boldAlsoToken
         then nextToken()
              nextSubsequence())))

!  NEXT SUBSEQUENCE. Parse one or more expressions separated by semicolons.

  nextSubsequence :−
   (proc () void:
    (in trace(''subsequence'')
     do nextExpression()
        (while
         (if token = newlineToken ∨ token = semicolonToken
          then nextToken()
               (if atTerm()
                then nextExpression()
                     true
                else false)
          else false))))

!  NEXT EXPRESSION. Parse a disjunction, or exactly two disjunctions, separated
!  by an assigner.

  nextExpression :−
   (proc () void:
    (in trace(''expression'')
     do nextDisjunction()
        (if token = assignerToken
         then nextToken()
              nextDisjunction())))

!  NEXT DISJUNCTION. Parse one or more conjunctions separated by ORs or ∨'s.

  nextDisjunction :−
   (proc () void:
    (in trace(''disjunction'')
     do nextConjunction()
        (while token = disjunctionToken
         do nextToken()
            nextConjunction())))

!  NEXT CONJUNCTION. Parse one or more disjunctions separated by ANDs or ∧'s.

  nextConjunction :−
   (proc () void:
    (in trace(''conjunction'')
     do nextComparison()
        (while token = conjunctionToken
         do nextToken()
            nextComparison())))

!  NEXT COMPARISON. Parse a series of one or more sums, separated by comparison
!  operators. Concatenate the comparison operators into a quoted name, and then
!  report the name at the line where the first comparison operator appeared.

  nextComparison :−
   (proc () void:
    (in trace(''comparison'')
     do nextSum()
        (if token = comparisonToken
         then (with
                int number :− tableNumber
                var buffer(maxNameLength) quotedChars
               do empty(quotedChars)
                  append(quotedChars, '"')
                  append(quotedChars, tokenChars{string})
                  nextToken()
                  nextSum()
                  (while token = comparisonToken
                   do append(quotedChars, ' ')
                      append(quotedChars, tokenChars{string})
                      nextToken()
                      nextSum())
                  append(quotedChars, '"')
                  recordName(quotedChars, number)))))

!  NEXT SUM. Parse one or more products separated by sum operators.

  nextSum :−
   (proc () void:
    (in trace(''sum'')
     do nextProduct()
        (while token = sumPrefixToken ∨ token = sumToken
         do nextToken()
            nextSum())))

!  NEXT PRODUCT. Parse one or more terms separated by product operators.

  nextProduct :−
   (proc () void:
    (in trace(''product'')
     do nextTerm()
        (while token = productToken
         do nextToken()
            nextTerm())))

!  NEXT TERM. Parse a term.

  nextTerm :−
   (proc () void:
    (with

!  NEXT FORM TERM. Parse a FORM type.

      nextFormTerm :−
       (form () void:
        (in trace(''form term'')
         do nextToken()
            nextToken(openParenToken)
            nextParameters()
            nextToken(closeParenToken)
            nextNewline()
            nextTerm()))

!  NEXT GEN TERM. Parse a GEN type.

      nextGenTerm :−
       (form () void:
        (in trace(''gen term'')
         do (while
              nextToken()
              nextToken(openParenToken)
              nextParameters()
              nextToken(closeParenToken)
              nextNewline()
              token = boldGenToken)
            (if token = boldFormToken
             then nextFormTerm()
             else syntaxError())))

!  NEXT PROC TERM. Parse a PROC type.

      nextProcTerm :−
       (form () void:
        (in trace(''proc term'')
         do nextToken()
            nextToken(openParenToken)
            nextParameters()
            nextToken(closeParenToken)
            nextNewline()
            nextTerm()))

!  NEXT OPEN BRACE TERM. Parse { A } T, where A is a series of arguments, and T
!  is a term.

      nextOpenBraceTerm :−
       (form () void:
        (in trace(''open brace term'')
         do recordName(''"{} "'', tableNumber)
            nextToken()
            nextArguments()
            nextToken(closeBraceToken)
            nextNewline()
            nextTerm()))

!  NEXT OPEN BRACKET TERM. Parse [ A ] T, where A is a series of arguments, and
!  T is a term.

      nextOpenBracketTerm :−
       (form () void:
        (in trace(''open bracket term'')
         do recordName(''"[] "'', tableNumber)
            nextToken()
            nextArguments()
            nextToken(closeBracketToken)
            nextNewline()
            nextTerm()))

!  NEXT PREFIX TERM. Parse a term preceded by a prefix operator.

      nextPrefixTerm :−
       (form () void:
        (in trace(''prefix term'')
         do nextToken()
            nextTerm()))

!  This is NEXT TERM's body.

     do (case token
         of boldFormToken: nextFormTerm()
             boldGenToken: nextGenTerm()
            boldProcToken: nextProcTerm()
           openBraceToken: nextOpenBraceTerm()
         openBracketToken: nextOpenBracketTerm()
              prefixToken: nextPrefixTerm()
           sumPrefixToken: nextPrefixTerm()
                     none: nextUnit())))

!  NEXT UNIT. Parse a unit.

  nextUnit :−
   (proc () void:
    (with

!  NEXT DOLLAR UNIT. Parse $ N, where N is a name.

      nextDollarUnit :−
       (form () void:
         nextToken()
         nextToken(nameToken))

!  This is NEXT UNIT's body. NONE outside a CASE clause is an error.

     do (in trace(''unit'')
         do (case token
             of boldNoneToken: syntaxError()
                constantToken: nextToken()
                  dollarToken: nextDollarUnit()
                    hookToken: nextToken()
               openParenToken: nextClause()
                    nameToken: nextToken()
                         none: syntaxError())
            nextPostfix())))

!  NEXT POSTFIX. Parse zero or more postfix operators following a unit.

  nextPostfix :−
   (form () void:
    (with

!  NEXT DOT POSTFIX. Parse . N, where N is a name.

      nextDotPostfix :−
       (form () bool:
        (in trace(''dot postfix'')
         do nextToken()
            nextToken(nameToken)
            true))

!  NEXT OPEN BRACE POSTFIX. Parse { A }, where A is a series of arguments.

      nextOpenBracePostfix :−
       (form () bool:
        (in trace(''open brace postfix'')
         do recordName(''" {}"'', tableNumber)
            nextToken()
            nextArguments()
            nextToken(closeBraceToken)
            true))

!  NEXT OPEN BRACKET POSTFIX. Parse [ A ], where A is a series of arguments.

      nextOpenBracketPostfix :−
       (form () bool:
        (in trace(''open bracket postfix'')
         do recordName(''" []"'', tableNumber)
            nextToken()
            nextArguments()
            nextToken(closeBracketToken)
            true))

!  NEXT OPEN PAREN POSTFIX. Parse ( A ), where A is a series of arguments.

      nextOpenParenPostfix :−
       (form () bool:
        (in trace(''open paren postfix'')
         do nextToken()
            nextArguments()
            nextToken(closeParenToken)
            true))

!  NEXT SIMPLE POSTFIX. Parse a single token postfix operator.

      nextSimplePostfix :−
       (form () bool:
        (in trace(''simple postfix'')
         do nextToken()
            true))

!  This is NEXT POSTFIX's body.

     do (while
         (case token
          of dotToken:         nextDotPostfix()
             openBraceToken:   nextOpenBracePostfix()
             openBracketToken: nextOpenBracketPostfix()
             openParenToken:   nextOpenParenPostfix()
             postfixToken:     nextSimplePostfix()
             none:             false))))

!  NEXT CLAUSE. Parse a clause.

  nextClause :−
   (proc () void:
    (with

!  NEXT ALT CLAUSE. Parse an ALT clause.

      nextAltClause :−
       (form () void:
        (in trace(''alt clause'')
         do nextToken()
            nextExpressions()))

!  NEXT ALTS CLAUSE. Parse an ALTS clause.

      nextAltsClause :−
       (form () void:
        (in trace(''alts clause'')
         do nextToken()
            nextExpressions()))

!  NEXT CASE CLAUSE. Parse a CASE clause.

      nextCaseClause :−
       (form () void:
        (with

!  NEXT LABEL. Parse a label.

          nextLabel :−
           (form () void:
            (if token = boldNoneToken
             then nextToken()
             else nextNeededExpressions()))

!  This is NEXT CASE CLAUSE's body.

         do (in trace(''case clause'')
             do nextToken()
                nextSequence()
                nextToken(boldOfToken)
                (while
                 (if atTerm()
                  then nextLabel()
                       nextToken(colonToken)
                       nextExpression()
                       (if token = newlineToken ∨ token = semicolonToken
                        then nextToken()
                             true
                        else false)
                  else false)))))

!  NEXT CATCH CLAUSE. Parse a CATCH clause.

      nextCatchClause :−
       (form () void:
        (in trace(''catch clause'')
         do nextToken()
            nextSequence()))

!  NEXT FOR CLAUSE. Parse a FOR clause.

      nextForClause :−
       (form () void:
        (in trace(''for clause'')
         do nextToken()
            nextParameters()
            (if token = boldInToken
             then nextToken()
                  nextNeededExpressions())
            nextToken(boldDoToken)
            nextSequence()))

!  NEXT FORM CLAUSE. Parse a FORM clause, or a FORM term in parentheses.

      nextFormClause :−
       (form () void:
        (in trace(''form clause'')
         do nextToken()
            nextToken(openParenToken)
            nextParameters()
            nextToken(closeParenToken)
            nextNewline()
            nextTerm()
            nextNewline()
            (if token = colonToken
             then nextToken()
                  nextSequence())))

!  NEXT GEN CLAUSE. Parse a GEN clause, or a GEN term in parentheses.

      nextGenClause :−
       (form () void:
        (in trace(''gen clause'')
         do (while
              nextToken()
              nextToken(openParenToken)
              nextParameters()
              nextToken(closeParenToken)
              nextNewline()
              token = boldGenToken)
            nextToken(boldFormToken)
            nextToken(openParenToken)
            nextParameters()
            nextToken(closeParenToken)
            nextNewline()
            nextTerm()
            nextNewline()
            (if token = colonToken
             then nextToken()
                  nextSequence())))

!  NEXT IF CLAUSE. Parse an IF clause.

      nextIfClause :−
       (form () void:
        (in trace(''if clause'')
         do (while
              nextToken()
              nextSequence()
              nextToken(boldThenToken)
              nextSequence()
              (if token = boldElseToken
               then nextToken()
                    (if token = boldIfToken
                     then true
                     else nextSequence()
                          false)
               else false))))

!  NEXT IN CLAUSE. Parse a FOR clause, but without its FOR part.

      nextInClause :−
       (form () void:
        (in trace(''in clause'')
         do nextToken()
            nextNeededExpressions()
            nextNewline()
            nextToken(boldDoToken)
            nextSequence()))

!  NEXT PAST CLAUSE. Parse a PAST clause.

      nextPastClause :−
       (form () void:
        (in trace(''past clause'')
         do nextToken()
            nextToken(nameToken)))

!  NEXT PROC CLAUSE. Parse a PROC clause, or a PROC term in parentheses.

      nextProcClause :−
       (form () void:
        (in trace(''proc clause'')
         do nextToken()
            nextToken(openParenToken)
            nextParameters()
            nextToken(closeParenToken)
            nextNewline()
            nextTerm()
            nextNewline()
            (if token = colonToken
             then nextToken()
                  nextSequence())))

!  NEXT TUPLE CLAUSE. Parse a TUPLE clause.

      nextTupleClause :−
       (form () void:
        (in trace(''tuple clause'')
         do nextToken()
            nextParameters()))

!  NEXT WITH CLAUSE. Parse a WITH clause.

      nextWithClause :−
       (form () void:
        (in trace(''with clause'')
         do nextToken()
            (while token ≠ boldDoToken
             do nextEquate()
                (if token = newlineToken ∨ token = semicolonToken
                 then nextToken()
                 else if atTerm()
                      then syntaxError()))
            nextToken(boldDoToken)
            nextSequence()))

!  NEXT WHILE CLAUSE. Parse a WHILE clause.

      nextWhileClause :−
       (form () void:
        (in trace(''while clause'')
         do nextToken()
            nextSequence()
            (if token = boldDoToken
             then nextToken()
                  nextSequence())))

!  NEXT COLON CLAUSE. Parse a list constructor.

      nextColonClause :−
       (form () void:
        (in trace(''colon clause'')
         do nextToken()
            nextExpressions()))

!  This is NEXT CLAUSE's body.

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
          boldWhileToken: nextWhileClause()
           boldWithToken: nextWithClause()
              colonToken: nextColonClause()
                    none: nextSequence())
        nextToken(closeParenToken)))

!  NEXT PROGRAM. Parse a program.

  nextProgram :−
   (proc () void:
    (with

!  NEXT PROGRAM CLAUSE. Parse a LOAD or PROG clause.

      nextProgramClause :−
       (form () void:
         nextToken()
         (if token = boldLoadToken
          then nextLoadClause()
          else if token = boldProgToken
               then nextProgClause()
               else syntaxError())
         nextToken(closeParenToken))

!  NEXT LOAD CLAUSE. Parse a LOAD clause.

      nextLoadClause :−
       (form () void:
        (in trace(''load clause'')
         do nextToken()
            nextSequence()))

!  NEXT PROG CLAUSE. Parse a PROG clause.

      nextProgClause :−
       (form () void:
        (in trace(''prog clause'')
         do nextToken()
            (while token ≠ closeParenToken
             do nextEquate()
                (if token = newlineToken ∨ token = semicolonToken
                 then nextToken()
                 else if atTerm()
                      then syntaxError()))))

!  This is NEXT PROGRAM's body.

     do (while token ≠ endToken
         do (if token = openParenToken
             then nextProgramClause()
                  (if token = newlineToken ∨ token = semicolonToken
                   then nextToken()
                   else if atTerm()
                        then syntaxError())
             else syntaxError()))))
)
