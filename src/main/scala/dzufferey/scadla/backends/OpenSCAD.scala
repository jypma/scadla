package dzufferey.scadla.backends

import dzufferey.scadla._
import dzufferey.utils.SysCmd
import java.io._

object OpenSCAD {

  protected def getMultiplicity(s: Solid): Map[Solid, Int] = {
    var map = Map[Solid, Int]()
    def incr(s: Solid) {
      val mult = map.getOrElse(s, 0) + 1
      map += (s -> mult)
    }
    utils.traverse(incr, s)
    map
  }
  
  protected def decreaseMultiplicity(_map: Map[Solid, Int], s: Solid, n: Int): Map[Solid, Int] = {
    var map = _map
    def decr(s: Solid) {
      val mult = map(s) - n
      assert(mult >= 0)
      map += (s -> mult)
    }
    utils.traverse(decr, s)
    map
  }

  protected def printWithModules(obj: Solid, writer: BufferedWriter) {
    var mult = getMultiplicity(obj)
    var modules = Map[Solid, String]()
    var cnt = 0
    def prnt(obj: Solid, indent: Int): Unit = {
      if (modules.contains(obj)) {
        spaces(indent)(writer)
        writer.write(modules(obj) + "();")
        writer.newLine
      } else if (mult(obj) > 1) {
        val name = "m_" + cnt
        cnt += 1
        mult = decreaseMultiplicity(mult, obj, mult(obj) - 1)
        modules += (obj -> name)
        spaces(indent)(writer)
        writer.write(name + "();")
        writer.newLine
      } else {
        prnt2(obj, indent)
      }
    }
    def prnt2(obj: Solid, indent: Int): Unit = {
      spaces(indent)(writer)
      obj match {
        case Empty =>
          writer.newLine
        case Cube(width, depth, height) =>
          writer.write("cube([ " + width + ", " + depth + ", " + height + "]);")
          writer.newLine
        case Sphere(radius) =>
          writer.write("sphere( " + radius + ");")
          writer.newLine
        case Cylinder(radiusBot, radiusTop, height) =>
          writer.write("cylinder( r1 = " + radiusBot + ", r2 = " + radiusTop + ", h = " + height + ");")
          writer.newLine
        case Polyhedron(triangles) =>
          val points = triangles.foldLeft(Set[Point]())( (acc, face) => acc + face.p1 + face.p2 + face.p3 )
          val indexed = points.toSeq.zipWithIndex
          val idx: Map[Point, Int] = indexed.toMap
          writer.write("polyhedron( points=[ ")
          writer.write(indexed.map{ case (Point(x,y,z), _) => "["+x+","+y+","+z+"]" }.mkString(", "))
          writer.write(" ], faces=[ ")
          writer.write(triangles.map{ case Face(a,b,c) => "["+idx(a)+","+idx(b)+","+idx(c)+"]" }.mkString(", "))
          writer.write(" ]);")
          writer.newLine
        case FromFile(path, format) =>
          writer.write("import(\"")
          writer.write(path)
          writer.write("\");")
          writer.newLine
        //operations
        case Union(objs @ _*) =>
          writer.write("union(){")
          writer.newLine
          objs.foreach(prnt(_, indent+2))
          writer.write("}")
          writer.newLine
        case Intersection(objs @ _*) =>
          writer.write("intersection(){")
          writer.newLine
          objs.foreach(prnt(_, indent+2))
          writer.write("}")
          writer.newLine
        case Difference(pos, negs @ _*) =>
          writer.write("difference(){")
          writer.newLine
          prnt(pos, indent+2)
          negs.foreach(prnt(_, indent+2))
          writer.write("}")
          writer.newLine
        case Minkowski(objs @ _*) =>
          writer.write("minkowski(){")
          writer.newLine
          objs.foreach(prnt(_, indent+2))
          writer.write("}")
          writer.newLine
        case Hull(objs @ _*) =>
          writer.write("hull(){")
          writer.newLine
          objs.foreach(prnt(_, indent+2))
          writer.write("}")
          writer.newLine
        //transforms
        case Scale(x, y, z, obj) =>
          writer.write("scale(["+x+","+y+","+z+"])")
          writer.newLine
          prnt(obj, indent+2)
        case Rotate(x, y, z, obj) =>
          writer.write("rotate(["+math.toDegrees(x)+","+math.toDegrees(y)+","+math.toDegrees(z)+"])")
          writer.newLine
          prnt(obj, indent+2)
        case Translate(x, y, z, obj) =>
          writer.write("translate(["+x+","+y+","+z+"])")
          writer.newLine
          prnt(obj, indent+2)
        case Mirror(x, y, z, obj) =>
          writer.write("mirror(["+x+","+y+","+z+"])")
          writer.newLine
          prnt(obj, indent+2)
        case Multiply(m, obj) =>
          writer.write("multmatrix([["+m.m00+","+m.m01+","+m.m02+","+m.m03+"],["+m.m10+","+m.m11+","+m.m12+","+m.m13+"],["+m.m20+","+m.m21+","+m.m22+","+m.m23+"],["+m.m30+","+m.m31+","+m.m32+","+m.m33+"],])")
          writer.newLine
          prnt(obj, indent+2)
      }
    }
    assert(mult(obj) == 1)
    prnt(obj, 0)
    def printModules(printed: Set[String]) {
      modules.find{ case (_, name) => !printed(name) } match {
        case Some((obj, name)) =>
          writer.newLine
          writer.newLine
          writer.write("module " + name + "() {")
          writer.newLine
          prnt2(obj, 2)
          writer.write("}")
          writer.newLine
          printModules(printed + name)
        case None =>
      }
    }
    printModules(Set[String]())
  }
  
  protected def spaces(n: Int)(implicit writer: BufferedWriter): Unit = n match {
    case 0 => ()
    case 1 => writer write " ";
    case 2 => writer write "  ";
    case 3 => writer write "   ";
    case 4 => writer write "    ";
    case 5 => writer write "     ";
    case 6 => writer write "      ";
    case 7 => writer write "       ";
    case _ =>
    assert(n >= 8)
    writer write "        ";
    spaces(n - 8)
  }

  def print(obj: Solid, writer: BufferedWriter, header: Iterable[String] = Nil) {
    for (h <- header) {
      writer.write(h)
      writer.newLine
    }
    printWithModules(obj, writer)
  }
  
  val defaultHeader = List("$fa=4;", "$fs=0.5;")


  protected def writeInFile(file: java.io.File, obj: Solid, header: Iterable[String] = defaultHeader) = {
    val writer = new BufferedWriter(new PrintWriter(file))
    print(obj, writer, header)
    writer.close
  }

  protected def toTmpFile(obj: Solid, header: Iterable[String]) = {
    val tmpFile = java.io.File.createTempFile("scadlaModel", ".scad")
    writeInFile(tmpFile, obj, header)
    tmpFile
  }

  def saveFile(fileName: String, obj: Solid, header: Iterable[String] = defaultHeader) = {
    val file = new java.io.File(fileName)
    writeInFile(file, obj, header)
  }

  def toSTL(obj: Solid, outputFile: String, header: Iterable[String] = defaultHeader, options: Iterable[String] = Nil) = {
    val tmpFile = toTmpFile(obj, header)
    val cmd = Array("openscad", tmpFile.getPath, "-o", outputFile) ++ options
    val res = SysCmd(cmd)
    tmpFile.delete
    res
  }

  def view(obj: Solid, header: Iterable[String] = defaultHeader, optionsRender: Iterable[String] = Nil, optionsView: Iterable[String] = Nil) = {
    val tmpFile = java.io.File.createTempFile("scadlaModel", ".stl")
    toSTL(obj, tmpFile.getPath, header, optionsRender)
    val cmd = Array("meshlab", tmpFile.getPath) ++ optionsView
    val res = SysCmd(cmd)
    tmpFile.delete
    res
  }

  def runOpenSCAD(obj: Solid, header: Iterable[String] = defaultHeader, options: Iterable[String] = Nil) = {
    val tmpFile = toTmpFile(obj, header)
    val cmd = Array("openscad", tmpFile.getPath) ++ options
    val res = SysCmd(cmd)
    tmpFile.delete
    res
  }

  def getResult(obj: Solid, outputFile: String, header: Iterable[String] = defaultHeader, options: Iterable[String] = Nil) = {
    val tmpFile = java.io.File.createTempFile("scadlaModel", ".stl")
    toSTL(obj, tmpFile.getPath, header, options)
    val parsed = StlParser(tmpFile.getPath)
    tmpFile.delete
    parsed
  }

}
