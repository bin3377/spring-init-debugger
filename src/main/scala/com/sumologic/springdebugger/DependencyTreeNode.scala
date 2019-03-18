package com.sumologic.springdebugger

import scala.collection.mutable

class DependencyTreeNode(val beanEntry: BeanEntry, val parentNode: DependencyTreeNode) {
  val dependencies: mutable.ListBuffer[DependencyTreeNode] = new mutable.ListBuffer[DependencyTreeNode]()
  val typeString: String = beanEntry.typeString
  var warning: Boolean = false
}

object DependencyTreeNode {

  def apply(beanEntry: BeanEntry, parentNode: DependencyTreeNode): DependencyTreeNode = {
    val newNode = new DependencyTreeNode(beanEntry, parentNode)
    nameToNodeLookup.put(beanEntry.fullName, newNode)
    newNode
  }

  val nameToNodeLookup = new mutable.HashMap[String, DependencyTreeNode]()

  private def printTree(builder: StringBuilder,
                        root: DependencyTreeNode,
                        indent: Int = 0,
                        format: DependencyTreeNode => String = { node =>
                          s"(${node.typeString})${node.beanEntry.fullName}"
                        }): Unit = {
    builder.append("  " * indent).append(format(root)).append("\n")
    root.dependencies.foreach(n => printTree(builder, n, indent + 1, format))
  }

  def getDependencyTree(root: DependencyTreeNode): String = {
    val builder = new mutable.StringBuilder()
    printTree(builder, root)
    builder.toString()
  }

  def getDependencyPath(node: DependencyTreeNode): String = {
    var currentNode = node
    val builder = new mutable.StringBuilder()
    while (null != currentNode) {
      builder
        .append(String.format("%1$-8s|", currentNode.typeString))
        .append(currentNode.beanEntry.fullName).append("\n")
      currentNode = currentNode.parentNode
    }
    builder.toString()
  }
}
