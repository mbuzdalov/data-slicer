package ru.ifmo.ds.gui.async.value
import javax.swing.SwingUtilities

import ru.ifmo.ds.gui.async.node.{MutableNode, Node, Workload}

class BoundSwingValue[I, R](packer: => I, executor: I => R, dependencies: Node*) extends SwingValue[R] {
  require(SwingUtilities.isEventDispatchThread)

  private[this] var theValue: Option[R] = None
  private[this] val workload: Workload = new Workload {
    override type MainInput = I
    override type MainOutput = R
    override def onError(th: Throwable): Unit = theValue = None
    override def main(input: I): R = executor(input)
    override def afterMain(input: R): Unit = theValue = Some(input)
    override def beforeMain(): I = {
      theValue = None
      packer
    }
  }
  private[this] val theNode = new MutableNode(workload)

  dependencies.foreach(_.addListener(theNode))
  theNode.completeInitialization()

  override def value: R = theValue.get
  override protected def node: Node = theNode
  override def close(): Unit = dependencies.foreach(_.removeListener(theNode))
}
