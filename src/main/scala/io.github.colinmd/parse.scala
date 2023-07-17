package io.github.colinmd

import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue
import scala.collection.immutable.HashSet

sealed trait Expr;
case class Literal(data: String | Int) extends Expr
case class FnCall(fn: String, args: List[Expr]) extends Expr
case class FnDef(fn: String, params: List[String]) extends Expr

case class Environment(
  functions: HashMap[String, (Tree, List[String])] = new HashMap[String, (Tree, List[String])](

  ),
  numbers: HashMap[String, Int] = new HashMap[String, Int]
)

val builtInFns = List(
  "add", "+",
  "sub", "-",
  "div", "/",
  "mul", "*",
  "mod", "%"
)

def evalBuiltInFn(fnName: String, args: List[Int]): Int =
  println("evalBuiltInFn")
  fnName match
    case "add" | "+" => args.sum
    case "sub" | "-" => args.reduce((accum, x) => accum - x)
    case "mul" | "*" => args.reduce((accum, x) => accum * x)
    case "div" | "/" => args.reduce((accum, x) => accum / x)
    case "mod" | "%" => args.reduce((accum, x) => accum % x)
    case _ => throw new Exception("Nahhhh!")

def eval(tree: Tree, env: Environment): Option[Expr] =
  // this function is too long
  tree match
    case Node(children) =>
      // TODO: add fn def and extract fn def/call to other functions
      val head = children.dequeue
      println("Evaluating node with children size %s".formatted(children.size + 1))
      val args = children.toList.map(childTree =>
          eval(childTree, env).get match
            case Literal(data: Int) =>
              data
            case x =>
              println(x)
              throw new Exception("Shit!")
      )
      head match
        case LeafNode(Token(_, Some(fnName: String))) =>
          if builtInFns.contains(fnName) then Some(Literal(evalBuiltInFn(fnName, args)))
          else None
        case LeafNode(Token(_, Some(glorbular: Int))) => throw new Exception("caught a glorbular: %s".formatted(glorbular))
        case Node(children) =>
          for (child <- children)
            println(child)
          throw new Exception("got NODE with children size %d".formatted(children.size))
        case _ => throw new Exception("bad fn data")
    case LeafNode(token) =>
      Some(Literal(token.data.get))

def parse(tokens: Queue[Token]): Int = 
  eval(treeify(tokens), new Environment) match
    case Some(Literal(data: Int)) => data
    case _ => 0

def isLiteral(top: TokenType) =
  top match
    case TokenType.IDENTIFIER | TokenType.NUMBER => true
    case _ => false
