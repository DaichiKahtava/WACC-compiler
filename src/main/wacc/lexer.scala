package wacc

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.descriptions._

import parsley.token.predicate._
import parsley.token.text.Escape
import parsley.token.descriptions.text.EscapeDesc
import parsley.token.descriptions.numeric.NumericDesc
import parsley.token.descriptions.text.TextDesc

object lexer {
    private val desc = LexicalDesc.plain.copy(
        nameDesc = NameDesc.plain.copy(
            identifierStart = Basic(c => c.isLetter || c == '_'),
            identifierLetter = Basic(c => c.isLetterOrDigit || c == '_'),
        ),

        symbolDesc = SymbolDesc.plain.copy(
            hardKeywords = Set(
                "begin", "end", "skip", "read", "free", "return", "exit", "print", "println",
                "if", "then", "else","fi", "while", "do", "done", "fst", "snd", "int", "bool",
                "char", "string", "pair", "null", "true", "false"
            ),
            hardOperators = Set(
                "!", "-", "len", "ord", "chr", "*", "/", "%", "+", "-", ">", ">=", "<", "<=", "==", "!=", "&&", "||" 
            )
        ),

        numericDesc = NumericDesc.plain,

        textDesc = TextDesc.plain.copy(
            escapeSequences = EscapeDesc.plain.copy(
                escBegin = '\\',
                mapping = Map(
                    "0" -> 0x0,
                    "b" -> 0x8,
                    "t" -> 0x9,
                    "n" -> 0xa,
                    "f" -> 0xc,
                    "r" -> 0xd,
                    "\"" -> 0x22,
                    "\'" -> 0x27,
                    "\\" -> 0x5c),
            ),
            characterLiteralEnd = '\'',
            stringEnds = Set(("\"", "\"")),
            graphicCharacter = Basic(c => c != '\\' && c != '\'' && c != '"' && c >= ' '),
        ),

        spaceDesc = SpaceDesc.plain.copy(
            lineCommentStart = "#"
        ),
    )
    private val lexer = new Lexer(desc)

    // Literals +- keywords
    val ident = lexer.lexeme.names.identifier
    val intLit = lexer.lexeme.integer.decimal32
    val charLit = lexer.lexeme.character.ascii
    val strLit = lexer.lexeme.string.ascii
    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
