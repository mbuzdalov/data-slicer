package ru.ifmo.ds.util

sealed trait PartitionTree[+N, +L] {
  def mapLeaves[A](leafFunction: L => A): PartitionTree[N, A]
  def foldAll[A](leafFunction: L => A, nodeFunction: Seq[(N, A)] => A): A
}

object PartitionTree {
  case class Leaf[+L](data: L) extends PartitionTree[Nothing, L] {
    override def mapLeaves[A](leafFunction: L => A): PartitionTree[Nothing, A] = Leaf(leafFunction(data))
    override def foldAll[A](leafFunction: L => A, nodeFunction: Seq[(Nothing, A)] => A): A = leafFunction(data)
  }
  case class Node[+N, +L](children: Seq[(N, PartitionTree[N, L])]) extends PartitionTree[N, L] {
    override def mapLeaves[A](leafFunction: L => A): PartitionTree[N, A] = {
      Node(children.map(p => p._1 -> p._2.mapLeaves(leafFunction)))
    }
    override def foldAll[A](leafFunction: L => A, nodeFunction: Seq[(N, A)] => A): A = {
      nodeFunction(children.map(p => p._1 -> p._2.foldAll(leafFunction, nodeFunction)))
    }
  }
}
