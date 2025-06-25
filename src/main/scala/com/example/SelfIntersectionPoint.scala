package com.example

case class SelfIntersectionPoint(
                                  _x: Double,
                                  _y: Double,
                                  _i: Int,
                                  _j: Int,
                                  _toiI: Boolean,
                                  _tojI: Boolean,
                                  _toi: Int,
                                  _toj: Int,
                                  var _stateI: Boolean = false,
                                  var _stateJ: Boolean = false,
                                  _sameindex: Vector[Int] = Vector.empty
                                ) {
  def canTraceFromI: Boolean = !_stateI && _toiI
  def canTraceFromJ: Boolean = !_stateJ && _tojI

  def traceFromI(): Unit = {
    if (canTraceFromI) _stateI = true
    else throw new IllegalStateException("Invalid trace from _i")
  }

  def traceFromJ(): Unit = {
    if (canTraceFromJ) _stateJ = true
    else throw new IllegalStateException("Invalid trace from _j")
  }
}

