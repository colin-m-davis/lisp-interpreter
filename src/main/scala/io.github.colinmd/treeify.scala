package io.github.colinmd

import scala.collection.mutable.Queue
import scala.collection.mutable.Stack


// parse (treeify and verify)
sealed trait Tree
case class Node(children: Queue[Tree] = Queue[Tree]()) extends Tree
case class LeafNode(token: Token) extends Tree

// replace parens with tree structure
def treeify(tokensQueue: Queue[Token]) =
  var stack = new Stack[Tree]
  stack.addOne(Node())
  for (token <- tokensQueue)
    token.t match
      case TokenType.LEFT_PAREN =>
        stack.addOne(Node())
      case TokenType.RIGHT_PAREN =>
        val top = stack.pop()
        stack.top match
          case Node(children) => children.addOne(top)
          case _ =>
      case x =>
        stack.top match
          case Node(children) => children.addOne(LeafNode(token))
          case _ =>
