package ru.ifmo.ds.gui.util

import javax.swing.JTable

object TableUtils {
  def alignTable(table: JTable): Unit = {
    val colModel = table.getColumnModel
    val spacing = table.getIntercellSpacing.width + 5
    for (columnIndex <- 0 until table.getColumnCount) {
      val column = colModel.getColumn(columnIndex)
      val renderer0 = column.getHeaderRenderer
      val renderer = if (renderer0 == null) table.getTableHeader.getDefaultRenderer else renderer0
      var width = renderer
        .getTableCellRendererComponent(table, column.getHeaderValue, false, false, -1, columnIndex)
        .getPreferredSize
        .width
      for (rowIndex <- 0 until table.getRowCount) {
        val rowWidth = table
          .prepareRenderer(table.getCellRenderer(rowIndex, columnIndex), rowIndex, columnIndex)
          .getPreferredSize
          .width
        width = math.max(width, rowWidth)
      }
      width += spacing
      if (width < column.getMaxWidth) {
        table.getTableHeader.setResizingColumn(column)
        column.setWidth(width)
      }
    }
  }
}
