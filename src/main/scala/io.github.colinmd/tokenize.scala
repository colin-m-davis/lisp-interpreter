package io.github.colinmd

import scala.io.Source.fromFile
import scala.collection.mutable.Queue
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet

def tokenize(filePath: String): Queue[Token] =
  val tokens = identify(dissolve(fromFile(filePath).getLines))
  // for (token <- tokens)
  //   println("Type: %s, data: %s".format(token.t.toString(), token.data.getOrElse("Null")))
  return tokens

val tokenMap = HashMap[String, TokenType](
  "(" -> TokenType.LEFT_PAREN,
  ")" -> TokenType.RIGHT_PAREN,
  "{" -> TokenType.LEFT_BRACE,
  "}" -> TokenType.RIGHT_BRACE,
  "," -> TokenType.COMMA,
  ";" -> TokenType.EOL,
  "if" -> TokenType.IF,
  "else" -> TokenType.ELSE,
  "fn" -> TokenType.FN,
  "true" -> TokenType.TRUE,
  "false" -> TokenType.FALSE,
  "while" -> TokenType.WHILE,
  "or" -> TokenType.OR,
  "and" -> TokenType.AND,
)

val oneCharTokenSet = Vector[Char](
  '(', ')', '{', '}', ',', ';'
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
      case identifier if chunk forall Character.isAlphabetic =>  // bruh
        Token(TokenType.IDENTIFIER, Option(identifier))
      case hexString =>
        Token(TokenType.NUMBER, Option(Integer.parseInt(hexString, 16)))
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
