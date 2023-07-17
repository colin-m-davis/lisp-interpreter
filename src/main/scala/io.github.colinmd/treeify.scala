package io.github.colinmd

import scala.collection.mutable.Queue
import scala.collection.mutable.Stack

// parse (treeify and verify)
sealed trait Tree
case class Node(children: Queue[Tree] = Queue[Tree]()) extends Tree
case class LeafNode(token: Token) extends Tree

// replace parens with tree structure
def treeify(tokensQueue: Queue[Token]): Tree =
  var stack = new Stack[Node]
  stack.addOne(Node())
  for (token <- tokensQueue)
    token.t match
      case TokenType.LEFT_PAREN =>
        stack.push(Node())
      case TokenType.RIGHT_PAREN =>
        val oldTop = stack.pop
        stack.top.children.enqueue(oldTop)
      case x =>
        stack.top.children.enqueue(LeafNode(token))
  stack.top
