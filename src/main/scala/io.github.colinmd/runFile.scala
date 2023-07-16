package io.github.colinmd

import scala.collection.mutable.Queue

def runFile(filePath: String) =
  val tokens = tokenize(filePath)
  val tree = treeify(tokens)
