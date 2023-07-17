package io.github.colinmd

import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue
import scala.collection.immutable.HashSet

sealed trait Expr;
case class Value(data: Int | List[Value]) extends Expr
case class Handle(name: String) extends Expr
case class FnCall(fn: String, args: List[Expr]) extends Expr
case class FnDef(fn: String, params: List[String]) extends Expr

case class Environment(
  functions: HashMap[String, (Tree, List[String])] = new HashMap[String, (Tree, List[String])],
  handles: HashMap[String, Value] = new HashMap[String, Value]
)

extension (v: Value)
  def getInt = v match
    case Value(data: Int) => data
    case _ => throw new IllegalArgumentException

val builtInFns = List(
  "add", "+",
  "sub", "-",
  "div", "/",
  "mul", "*",
  "mod", "%",
  "fn"
)

def evalBuiltInFn(fnName: String, args: List[Value]): Value =
  fnName match
    case "add" | "+" => Value(args.map(getInt).reduce((accum: Int, x: Int) => accum + x))
    case "sub" | "-" => Value(args.map(getInt).reduce((accum: Int, x: Int) => accum - x))
    case "mul" | "*" => Value(args.map(getInt).reduce((accum: Int, x: Int) => accum * x))
    case "div" | "/" => Value(args.map(getInt).reduce((accum: Int, x: Int) => accum / x))
    case "mod" | "%" => Value(args.map(getInt).reduce((accum: Int, x: Int) => accum % x))
    case "group" => Value(args)
    case _ => throw new IllegalArgumentException("Nahhhh!")

def eval(tree: Tree, env: Environment): Option[Expr] =
  // this function is too long
  tree match
    case Node(children) =>
      // TODO: add fn def and extract fn def/call to other functions
      val head = children.dequeue
      val args = children.toList.map(childTree =>
        eval(childTree, env).get match
          case v: Value => v
          case x => throw new Exception("Crap!")
      )
      head match
        case LeafNode(Token(_, Some(fnName: String))) =>
          if builtInFns.contains(fnName) then
            Some(evalBuiltInFn(fnName, args))
          else if env.functions.contains(fnName) then
            val (body, params) = env.functions.get(fnName).get
            val innerEnv = env.copy(handles = env.handles ++ params.zip(args))
            eval(body, innerEnv)
          else
            None
        case _ => throw new Exception("Malformed function call")
    case LeafNode(Token(_, Some(name: String))) =>
      Some(Handle(name))
    case LeafNode(Token(_, Some(data: Int))) =>
      Some(Value(data))
    case x =>
      println(x)
      None

def parse(tokens: Queue[Token]): Value = 
  eval(treeify(tokens), new Environment) match
    case Some(v: Value) => v
    case _ => Value(-1)

def isValue(top: TokenType) =
  top match
    case TokenType.IDENTIFIER | TokenType.NUMBER => true
    case _ => false
