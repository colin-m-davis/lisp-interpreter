package io.github.colinmd

import scala.collection.mutable.HashMap


sealed trait Expr;
case class Literal(data: Int) extends Expr
case class FnCall(fn: String, args: List[(String, Expr)]) extends Expr

class Environment(
  values: HashMap[String, Literal] = HashMap[String, Literal](),
  functions: HashMap[String, Expr] = HashMap[String, Expr]()
)

def eval(env: HashMap[String, Expr], exp: Expr): Literal =
  exp match
    case FnCall(fn, args) =>
      val argsEvaluated: List[(String, Literal)] = args.map((id, exp) =>
        (id, eval(env, exp))
      )
      for ((id, value) <- argsEvaluated) {
        env.put(id, value)
      }
      val result = Literal(5);
      for ((id, value) <- argsEvaluated) {
        env.remove(id)
      }
      result
    case lit: Literal => lit

// fn apply (Int x) (Int z) { println (+ x z) }

def parse(tokens: Vector[Token]): Option[Expr] = 
  tokens.size match
    case 1 => if isLiteral(tokens.head) then Some(Literal(tokens.head)) else None
    case _ => Some(Fn)
  
def isLiteral(top: tokenType) =
  top match
    case TokenType.IDENTIFIER | TokenType.NUMBER => true
    case _ => false
