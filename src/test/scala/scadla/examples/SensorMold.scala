package scadla.examples

import scadla.Cylinder
import scadla.InlineOps._
import scadla.Cube
import scadla.backends.OpenSCAD
import scala.language.postfixOps
import org.scalatest.FunSuite

class SensorMold extends FunSuite {
  implicit class IntOps(i: Int) {
    def ° = Math.toRadians(i)
  }
  
  test("SensorMold") {
    val r = 15.0
    val wall = 3.0
    val height = 100.0
    
    val mountSize = 8.0
    val mountDepth = 4.0
    val mountHole = 1.75
    
    val tolerance = 0.4 // hole is 0.4mm bigger than bevel 
    
    val shape = Cylinder(r + wall, height + wall) -
                Cylinder(r, height+1).moveZ(wall)
    val bottom = shape - Cube((r+wall+1)*2, (r+wall+1)*2, wall+height+2).moveX(-r-wall-1).moveZ(-1)
    
    val b = wall / 3.0
    val bevel = Cube(b,b+1,height+wall-b).moveY(-1).moveZ(b).moveX(r+b) +
                Cube(b,b+1,height+wall-b).moveY(-1).moveZ(b).moveX(-r-b*2) +
                Cube(r*2+b*4,b+1,b).moveY(-1).moveZ(b).moveX(-r-b*2)
    
    val hole =  Cube(b+tolerance,b+0.1,height+wall-b+0.1).moveY(0.1-b).moveZ(b).moveX(r+b-0.5*tolerance) +
                Cube(b+tolerance,b+0.1,height+wall-b+0.1).moveY(0.1-b).moveZ(b).moveX(-r-b*2-0.5*tolerance) +
                Cube(r*2+b*4+tolerance,b+0.1,b+tolerance).moveY(0.1-b).moveZ(b-0.5*tolerance).moveX(-r-b*2-0.5*tolerance)
                
    val mount = (
      Cube(mountSize, mountDepth, mountSize) - 
      Cylinder(mountHole, mountDepth+2).rotateX(-90°).moveX(mountSize / 2).moveY(-1).moveZ(mountSize / 2)
    ).moveY(-mountDepth).moveX(-mountSize/2)
    
    val mounts = mount.moveX(r*2/3).moveZ(1-mountSize) +
                 mount.moveX(-(r*2/3)).moveZ(1-mountSize) +
                 mount.moveX(r+wall+mountSize/2-1).moveZ(height*1/7) +
                 mount.moveX(-(r+wall+mountSize/2-1)).moveZ(height*1/7) +
                 mount.moveX(r+wall+mountSize/2-1).moveZ(height*3.5/7) +
                 mount.moveX(-(r+wall+mountSize/2-1)).moveZ(height*3.5/7) +
                 mount.moveX(r+wall+mountSize/2-1).moveZ(height*6/7) +
                 mount.moveX(-(r+wall+mountSize/2-1)).moveZ(height*6/7)

    val foot = Cylinder(0.5*mountSize, mountSize+0.1).moveZ(-mountSize).moveY(-r)                 
                 
    OpenSCAD.toSCAD(bottom + foot + mounts + bevel, "two")
    OpenSCAD.toSTL(bottom + foot + mounts + bevel, "two.stl")
    OpenSCAD.toSCAD(bottom + foot + mounts - hole, "out")
    OpenSCAD.toSTL(bottom + foot + mounts - hole, "one.stl")
  }
}