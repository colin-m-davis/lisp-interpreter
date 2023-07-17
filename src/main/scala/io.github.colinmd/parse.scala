package io.github.colinmd

import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue

sealed trait Expr;
case class Literal(data: String | Int) extends Expr
case class FnCall(fn: String, args: List[Expr]) extends Expr
case class FnDef(fn: String, params: List[String]) extends Expr

case class Environment(
  functions: HashMap[String, (Tree, List[String])] = new HashMap[String, (Tree, List[String])],
  numbers: HashMap[String, Int] = new HashMap[String, Int]
)

def eval(tree: Tree, env: Environment): Option[Expr] =
  // this function is too long
  tree match
    case Node(children) =>
      // TODO: add fn def and extract fn def/call to other functions
      val (body: Tree, params: List[String]) = 
        children.dequeue match
          case LeafNode(token) =>
            token.data match
              case Some(contents: String) => env.functions.get(contents).get
              case _ => throw new Exception("Wrongg!!!")
          case _ => throw new Exception("wrong!!!")
      val args = children.toList.map(childTree =>
          eval(childTree, env).get match
            case Literal(data: Int) => data
            case _ => throw new Exception("Shit!")
      )
      val innerEnv = Environment(env.functions, env.numbers ++ params.zip(args).toMap)
      eval(body, innerEnv)
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
