package wacc

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.descriptions._

import parsley.token.predicate._
import parsley.token.text.Escape
import parsley.token.descriptions.text.EscapeDesc

object lexer {
    private val desc = LexicalDesc.plain.copy(
        // your configuration goes here
        nameDesc = NameDesc.plain.copy(
            identifierStart = predicate.Basic(_.isLetter), // check for "_"
            idetifierLetter = predicate.Basic(_.isLetterOrDigit), // check for "_"
        ),
        symbolDesc = SymbolDesc.plain,
        numericDesc = NumericDesc.plain,
        textDesc = TextDesc.plain.copy(
            escapeSequences = new EscapeDesc.plain.copy(
                escBegin = '\\',
                literals = Set("0","b","t","n","f","r",'"',"'","\\"),
                
            ),
        ),
        spaceDesc = SpaceDesc.plain,
    )
    private val lexer = new Lexer(desc)

    val integer = lexer.lexeme.integer.decimal
    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
