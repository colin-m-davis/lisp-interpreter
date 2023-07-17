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
  chunks.map(chunk =>
    chunk match
      case builtin if tokenMap.contains(chunk) =>
        Token(tokenMap.get(builtin).get)
      case chunk => getLiteral(chunk)
  )

def getLiteral(chunk: String): Token =
  try
    try
      Token(TokenType.NUMBER, Option(Integer.parseInt(chunk, 10)))
    catch NumberFormatException =>
      if chunk.size <= 2 then throw new NumberFormatException
      val base =
        chunk.slice(0, 2) match
          case "0b" => 2
          case "0d" => 10
          case "0x" => 16
          case _ => throw new NumberFormatException
      Token(TokenType.NUMBER, Option(Integer.parseInt(chunk.takeRight(chunk.size - 2), base)))
  catch NumberFormatException =>
    Token(TokenType.IDENTIFIER, Option(chunk))

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
