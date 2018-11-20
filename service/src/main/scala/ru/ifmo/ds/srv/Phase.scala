package ru.ifmo.ds.srv

import java.nio.file.Path

abstract class Phase(final val key: String) {
  def execute(projectRoot: Path, curr: Path, prev: Option[Path]): Unit
}
