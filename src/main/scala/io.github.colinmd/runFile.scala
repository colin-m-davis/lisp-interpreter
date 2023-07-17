package io.github.colinmd

import scala.collection.mutable.Queue

def runFile(filePath: String) =
  val tokens = tokenize(filePath)
  println(parse(tokens))
