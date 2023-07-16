package io.github.colinmd

import scala.io.Source.fromFile
import scala.collection.mutable.Queue
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet

def tokenize(filePath: String): Queue[Token] =
  val lines = fromFile(filePath).getLines
  val chunks = dissolve(lines)
  val tokens = identify(chunks)
  for (token <- tokens) {
    if (token.data.isDefined) {
      println("Type: %s, data: %s".format(token.t.toString(), token.data))
    } else {
      println("Type: %s".format(token.t.toString()))
    }
  }
  return tokens

val tokenMap = HashMap[String, TokenType](
  "(" -> TokenType.LEFT_PAREN,
  ")" -> TokenType.RIGHT_PAREN,
  "{" -> TokenType.LEFT_BRACE,
  "}" -> TokenType.RIGHT_BRACE,
  "," -> TokenType.COMMA,
  "if" -> TokenType.IF,
  "else" -> TokenType.ELSE,
  "fn" -> TokenType.FN,
  "true" -> TokenType.TRUE,
  "false" -> TokenType.FALSE,
  "while" -> TokenType.WHILE,
  "or" -> TokenType.OR,
  "and" -> TokenType.AND,
  "let" -> TokenType.LET,
  "const" -> TokenType.CONST,
)

val oneCharTokenSet = HashSet[Char](
  '(', ')', '{', '}', ','
)

def dissolve(lines: Iterator[String]): Queue[String] =
  var chunks = Queue[String]()
  var inString = false
  for (line <- lines)
    var chunk = String()
    for (c <- line)
      if (oneCharTokenSet.contains(c))
        if (!chunk.isEmpty())
          chunks.addOne(chunk)
          chunk = String()
        chunks.addOne(c.toString())
      else if (c == '"')
        inString = !inString
        chunk += c
      else if (c == ' ' && !inString)
        if (!chunk.isEmpty())
          chunks.addOne(chunk)
          chunk = String()
      else
        chunk += c
    if !chunk.isEmpty() then chunks.addOne(chunk)
  return chunks

def identify(chunks: Queue[String]): Queue[Token] =
  return chunks.map(chunk =>
    chunk match
      case builtin if tokenMap.contains(chunk) =>
        Token(tokenMap.get(builtin).get)
      case number if chunk forall Character.isDigit =>
        Token(TokenType.NUMBER, Option(number.toInt))
      case string if chunk.length() >= 2 && chunk(0) == '"' && chunk.last == '"' => 
        Token(TokenType.STRING, Option(string.slice(1, string.length()-1)))
      case _ =>
        Token(TokenType.IDENTIFIER, Option(chunk))
  )

enum TokenType {
  case
  LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
  COMMA, EOL,

  // Keywords.
  AND, ELSE, FALSE, FN, IF, NIL, OR, LET, CONST,
  PRINT, RETURN, THIS, TRUE, VAR, WHILE,

  // Literals.
  IDENTIFIER, STRING, NUMBER,
}

case class Token(t: TokenType, data: Option[String | Int] = None)
