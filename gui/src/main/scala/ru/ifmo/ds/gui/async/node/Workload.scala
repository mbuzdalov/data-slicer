package ru.ifmo.ds.gui.async.node

trait Workload {
  type MainInput
  type MainOutput

  def onError(th: Throwable): Unit        /* called on the Swing event dispatch thread */
  def beforeMain(): MainInput             /* called on the Swing event dispatch thread */
  def main(input: MainInput): MainOutput  /* called outside of Swing in a dedicated thread */
  def afterMain(input: MainOutput): Unit  /* called on the Swing event dispatch thread */
}
