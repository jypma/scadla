package scadla.examples

import math._
import scadla._
import utils._
import utils.gear._
import InlineOps._

//inspired by Emmet's Gear Bearing (http://www.thingiverse.com/thing:53451)
object GearBearing {

  def apply(outerRadius: Double,
            height: Double,
            nbrPlanets: Int,
            nbrTeethPlanet: Int,
            nbrTeethSun: Int,
            helixAngleOuter: Double,
            centerHexagonMinRadius: Double,
            backlash: Double) = {
    new GearBearing(outerRadius, height, nbrPlanets, nbrTeethPlanet, nbrTeethSun,
                    helixAngleOuter, centerHexagonMinRadius, backlash)
  }

  def main(args: Array[String]) {
    val gears = apply(35, 10, 5, 10, 15, 0.02, 5, 0.1)
    backends.OpenSCAD.toSTL(gears.outer,  "outer.stl")
    backends.OpenSCAD.toSTL(gears.planet, "planet.stl")
    backends.OpenSCAD.toSTL(gears.sun,    "sun.stl")
  }

}

class GearBearing(outerRadius: Double,
                  height: Double,
                  nbrPlanets: Int,
                  nbrTeethPlanet: Int,
                  nbrTeethSun: Int,
                  helixAngleOuter: Double,
                  centerHexagonMinRadius: Double,
                  backlash: Double) {
    
  //constants
  val sunToPlanetRatio = nbrTeethSun.toDouble / nbrTeethPlanet
  val planetRadius = outerRadius / (2 + sunToPlanetRatio)
  val sunRadius = planetRadius * sunToPlanetRatio
  val nbrTeethOuter = 2 * nbrTeethPlanet + nbrTeethSun
  val helixAnglePlanet = helixAngleOuter * outerRadius / planetRadius
  val helixAngleSun = helixAngleOuter * outerRadius / sunRadius

  //TODO check for interferences

  def outer = Gear.herringbone(-outerRadius, nbrTeethOuter, height, helixAngleOuter, backlash)
  def planet = Gear.herringbone(planetRadius, nbrTeethPlanet, height, helixAnglePlanet, backlash) 
  def sun = {
    val sunCenter = Hexagon(centerHexagonMinRadius, height).rotateX(Pi).moveZ(10)
    Gear.herringbone(sunRadius, nbrTeethSun, height, helixAngleSun, backlash) - sunCenter
  }

  def all = {
    val p = planet
    val r = sunRadius+planetRadius
    val α = 2 * Pi / nbrTeethPlanet
    val β = -α * outerRadius / planetRadius
    val planets = for (i <- 0 until nbrPlanets) yield p.rotateZ(i*β).moveX(r).rotateZ(i*α)
    outer ++ planets + sun //TODO the sun should also rotate a bit ???
  }

}