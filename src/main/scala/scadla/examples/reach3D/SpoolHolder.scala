package scadla.examples.reach3D

import math._
import scadla._
import utils._
import InlineOps._
import scadla.examples.fastener.StructuralNutPlaceHolder

object SpoolHolder {

  val t = Thread.ISO.M6
  
  val looseTolerance = 0.2

  val bbRadius = 3.0
  val bearingGap = 1.0
  val grooveDepth = bbRadius / cos(Pi/4)

  // make sures the BBs fit nicely
  def adjustGrooveRadius(radius: Double): Double = {
    assert(radius > bbRadius)
    // find n such that the circumscribed radius is the closest to the given radius
    val sideLength = 2*bbRadius + looseTolerance
    val nD = Pi / asin(sideLength / (2 *radius))
    val n = nD.toInt // rounding to nearest regular polygon
    circumscribedRadius(n, sideLength)
  }

  def flatGroove(radius: Double, depth: Double, angle: Double = Pi/2, undercut: Double = 0.5) = {
    val width = depth / tan(Pi/2 - angle/2)
    val outer = Cylinder(radius + width, radius, depth)
    val inner = Cylinder(radius - width, radius, depth)
    (outer - inner) * Cylinder(radius + width, depth - undercut)
  }

  def radialGroove(radius: Double, depth: Double, angle: Double = Pi/2, undercut: Double = 0.5) = {
    val width = (depth / tan(Pi/2 - angle/2)).abs
    val middleRadius = radius - depth
    val body = Cylinder(radius, 2*width)
    val top = Cylinder(middleRadius, radius, width).moveZ(width)
    val bot = Cylinder(radius, middleRadius, width)
    val shape =
      if (depth >= 0) body - top - bot - Cylinder(middleRadius + undercut, 2*width)
      else (top + bot - body) * Cylinder(middleRadius - undercut, 2*width)
    shape.moveZ(-width)
  }

  def steppedCone(baseRadius: Double, steps: Int, stepRadiusDec: Double, stepHeight: Double) = {
    Union((0 until steps).map( i => Cylinder(baseRadius - i * stepRadiusDec, stepHeight).moveZ(i * stepHeight) ): _*)
  }

  def stemShape(radius1: Double, height1: Double, radius2: Double, height2: Double, bevel: Double) = {
    val c1 = Cylinder(radius1, height1)
    val c2 = Cylinder(radius2, height2).moveZ(height1)
    val bev = if (radius1 > radius2) Cylinder(radius2 + bevel, radius2, bevel).moveZ(height1)
              else Cylinder(radius1, radius1 + bevel, bevel).moveZ(height1 - bevel)
    c1 + c2 + bev - Cylinder(t, height1 + height2)
  }

  ////////////////////////
  // hard-coded numbers //
  ////////////////////////

  // bearings
  val radialBearingRadius = adjustGrooveRadius(t + 6 + bearingGap / 2)
  val flatBearingRadius = adjustGrooveRadius(25)
  val groove1 = flatGroove(flatBearingRadius, grooveDepth - bearingGap / 2)
  val groove2a = radialGroove(radialBearingRadius,  grooveDepth)
  val groove2b = radialGroove(radialBearingRadius, -grooveDepth)

  // cone dimensions
  val coneLength = 18
  val coneMaxRadius = 45
  val coneSteps = 14
  val stepHeight = 1.2 //1.1171875
  val stepRadius = 2

  val radialBearingPos1 = 4.8 //3.6 // or 4.8
  val radialBearingPos2 = coneLength - radialBearingPos1

  val stem = {
    val base = stemShape(30, 5, radialBearingRadius - bearingGap / 2, coneLength + bearingGap, 1)
    val grooves = List(
      groove1.mirror(0,0,1),
      groove2a.moveZ(bearingGap + radialBearingPos1),
      groove2a.moveZ(bearingGap + radialBearingPos2)
    )
    base -- grooves.map(_.moveZ(5)) - (new StructuralNutPlaceHolder).M6
  }

  val cone = {
    val screw = (Cylinder(1.25,16) + Cylinder(3,8).moveZ(16)).moveZ(1)
    val screws = (0 until 3).map( i => { screw.moveX(radialBearingRadius + grooveDepth + 2).rotateZ(2 * Pi / 3 * i) })
    val toRemove = screws ++ Seq(
        Cylinder(radialBearingRadius + bearingGap/2, coneLength),
        groove1,
        groove2b.moveZ(radialBearingPos1),
        groove2b.moveZ(radialBearingPos2)
      )
    val baseHeight = coneLength - coneSteps * stepHeight
    val base = Cylinder(coneMaxRadius, baseHeight + 0.001) + steppedCone(coneMaxRadius, coneSteps, stepRadius, stepHeight).moveZ(baseHeight)
    base -- toRemove
  }

  val conePart1 = cone * Cylinder(coneMaxRadius, radialBearingPos1)
  val conePart2 = cone * Cylinder(coneMaxRadius, radialBearingPos2 - radialBearingPos1 - 0.01).moveZ(radialBearingPos1 + 0.005)
  val conePart3 = cone * Cylinder(coneMaxRadius, radialBearingPos1).moveZ(radialBearingPos2 + 0.005)
  
  def main(args: Array[String]) {
    //backends.Renderer.default.view(stem)
    Seq(
      stem -> "stem.stl",
      conePart1 -> "conePart1.stl",
      conePart2 -> "conePart2.stl",
      conePart3 -> "conePart3.stl"
    ).par.foreach{ case (obj, name) =>
      backends.Renderer.default.toSTL(obj, name)
    }
  }

}
