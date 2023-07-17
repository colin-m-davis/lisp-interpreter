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
  var depth = 0
  for (token <- tokensQueue)
    token.t match
      case TokenType.LEFT_PAREN =>
        depth += 1
        stack.push(Node())
      case TokenType.RIGHT_PAREN =>
        depth -= 1
        val oldTop = stack.pop
        stack.top.children.enqueue(oldTop)
      case x =>
        assert(stack.size == depth + 1)
        1 to 2 * depth foreach { _ =>
          print(' ')
        }
        println("Item: %s, Stack Top Length: %s".formatted(x, stack.top.children.size))
        stack.top.children.enqueue(LeafNode(token))
  stack.top
