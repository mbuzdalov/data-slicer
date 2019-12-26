package ru.ifmo.ds.gui.async.value
import javax.swing.SwingUtilities

import ru.ifmo.ds.gui.async.node.{MutableNode, Node, Workload}

class BoundSwingValue[I, +R](packer: => I, executor: I => R, dependencies: Node*) extends SwingValue[R] {
  require(SwingUtilities.isEventDispatchThread)

  private[this] val workload = new BoundSwingValue.PackerExecutorWorkload(packer, executor)
  private[this] val theNode = new MutableNode(workload)

  dependencies.foreach(_.addListener(theNode))
  theNode.completeInitialization()

  override def value: R = workload.value
  override protected[value] def node: Node = theNode
  override def close(): Unit = dependencies.foreach(_.removeListener(theNode))
}

object BoundSwingValue {
  private class PackerExecutorWorkload[I, R](packer: => I, executor: I => R) extends Workload {
    private[this] var theValue: Option[R] = None
    def value: R = theValue.get

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
}
