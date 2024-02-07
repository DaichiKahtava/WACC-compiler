package wacc

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.descriptions._

import parsley.token.predicate._
import parsley.token.text.Escape
import parsley.token.descriptions.text.EscapeDesc
import parsley.token.descriptions.numeric.NumericDesc
import parsley.token.descriptions.text.TextDesc
import parsley.token.errors._

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
                "char", "string", "pair", "null", "true", "false", "newpair", "call"
            ),
            hardOperators = Set(
                "!", "-", "len", "ord", "chr", "*", "/", "%", "+", ">", ">=", "<", "<=", "==", "!=", "&&", "||" 
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
                    "r" -> 0xd),
                literals = Set('\\' , '\'', '\"')
            ),
            characterLiteralEnd = '\'',
            stringEnds = Set(("\"", "\"")),
            graphicCharacter = Basic(c => c != '\\' && c != '\'' && c != '"' && c >= ' '),
        ),

        spaceDesc = SpaceDesc.plain.copy(
            lineCommentStart = "#"
        ),
    )

    // Somehow need to differentiate unary "-" and binary "-"
    // This may need to be defined somewhere else (info in lectures?)
    // len, ord, chr, may able to be just themselves unless they can be categorised nicely
    private val errConfig = new ErrorConfig{
        override def labelSymbol = Map(
            "+" -> Label("arithmetic operator"),
            "*" -> Label("arithmetic operator"),
            "/" -> Label("arithmetic operator"),
            "%" -> Label("arithmetic operator"),

            ">" -> Label("relational operator"),
            ">=" -> Label("relational operator"),
            "<" -> Label("relational operator"),
            "<=" -> Label("relational operator"),
            "==" -> Label("relational operator"),
            "!=" -> Label("relational operator"),

            "!" -> Label("negation operator"),
            "&&" -> Label("binary logical operator"),
            "||" -> Label("binary logical operator")

        )
    }
    private val lexer = new Lexer(desc, errConfig)

    // Literals +- keywords
    val ident = lexer.lexeme.names.identifier
    val intLit = lexer.lexeme.integer.decimal32
    val charLit = lexer.lexeme.character.ascii
    val strLit = lexer.lexeme.string.ascii
    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
