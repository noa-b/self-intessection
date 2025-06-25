package com.example

import io.circe.parser.decode
import io.circe.generic.auto._

object Main extends App {
  private case class GeoJsonPolygon(`type`: String, coordinates: Seq[Seq[Seq[Double]]])
  case class GeoJsonDf(geoJson: String, id: String)

  val geoJSons = Seq(
    GeoJsonDf("""{"type":"Polygon","coordinates":[[[0,0],[2,2],[0,2],[2,0]]]}""", "Bowtie Hourglass"),
    GeoJsonDf("""{"type":"Polygon","coordinates":[[[1,1],[5,1],[3,4],[2,0],[4,0],[3,5]]]}""", "Complex Star-Like Polygon"),
    GeoJsonDf("""{"type":"Polygon","coordinates":[[[1,1],[5,1],[5,5],[1,5]]]}""", "Simple Square No Intersections"),
    GeoJsonDf("""{"type":"Polygon","coordinates":[[[-2,6],[3,7],[0,3],[-1,-1],[2,0]]]}""", "Bowtie Special from Shir"),
    GeoJsonDf("""{"type":"Polygon","coordinates":[[[0,3],[1,1],[3,1],[1.5,0],[2.5,-2],[0,-1],[-2.5,-2],[-1.5,0],[-3,1],[-1,1]]]}""", "star"),
    GeoJsonDf("""{"type":"Polygon","coordinates":[[[0,0],[4,3],[8,0],[4,-3],[0,0],[-4,3],[-8,0],[-4,-3],[0,0]]]}""", "two diamonds"),
    GeoJsonDf("""{"type":"Polygon","coordinates":[[[0,0],[4,2],[8,-1],[12,3],[16,0],[12,-3],[8,1],[4,-2],[0,1],[4,3],[8,0],[12,4],[16,2]]]}""", "Zig-Zag Loop"),
    GeoJsonDf("""{"type":"Polygon","coordinates":[[[0,0],[10,0],[10,10],[0,10],[0,0],[4,4],[6,4],[6,6],[4,6],[4,4]]]}""", "Square with Hole‑like Self Crossing"),
    GeoJsonDf("""{"type":"Polygon","coordinates":[[[0,0],[5,10],[10,0],[6,6],[14,14],[8,8],[19,4],[12,5],[20,0],[0,0]]]}""", "Nested Star Intersections"),
    GeoJsonDf("""{"type":"Polygon","coordinates":[[[0,0],[3,1],[6,0],[8,4],[6,7],[4,4],[2,7],[0,4],[2,1],[4,3],[6,2],[8,5],[6,8]]]}""", "Winding Wisp"),
    GeoJsonDf("""{"type":"Polygon","coordinates":[[[0,0],[4,0],[4,3],[2,5],[0,3],[0,0],[1,2],[3,2],[2,4],[1,2]]]}""", "House with Attic Loop"),
    //    GeoJsonDf("""{"type":"Polygon","coordinates":[[[2,2],[8,3],[9,7],[6,9],[3,8],[7,5],[5,2],[4,6],[1,9],[0,5],[2,7],[5,10],[8,9],[10,6],[11,2]]]}""", "Chaotic Irregular"), // wrong output
    //    GeoJsonDf("""{"type":"Polygon","coordinates":[[[-3,0],[3,6],[3,-6],[-3,6],[3,0],[-3,-6],[-3,0]]]}""", "Keyhole Bow-Loop"), // wrong output
    //    GeoJsonDf("""{"type":"Polygon","coordinates":[[[0,0],[10,0],[10,10],[5,10],[5,5],[10,5],[10,15],[0,15],[0,5],[5,5],[5,0],[0,0]]]}""", "Hole-Touching Loop"), // wrong output
    //    GeoJsonDf("""{"type":"Polygon","coordinates":[[[0,0],[4,8],[8,0],[4,4],[6,2],[4,6],[2,2],[4,4],[0,0]]]}""", "Nested Keyhole"), // wrong output

    GeoJsonDf("{\"type\":\"Polygon\",\"coordinates\":[[[45,6],[35,9],[50,16],[54,3],[50,16],[52.5,6],[54.5,17],[45,6],[27,15.5],[42,13],[45,6],[0,34],[36,9],[50,15],[45,6]]]}", "complicated"),
    GeoJsonDf("{\"type\":\"Polygon\",\"coordinates\":[[[-6,0],[-4,4],[0,0],[4,-6],[6,1],[10,7],[13,4],[11,2],[6,1],[5,3],[1,2],[0,0],[-6,0]]]}", "multi_intersection_points_x1"),
//    GeoJsonDf("{\"type\":\"Polygon\",\"coordinates\":[[[0,0],[-6,0],[-4,4],[0,0],[1,2],[5,3],[6,1],[10,7],[13,4],[11,2],[6,1],[4,-6],[0,0]]]}", "multi_intersection_points_x2"), // wrong output
    GeoJsonDf("{\"type\":\"Polygon\",\"coordinates\":[[[0,0],[-6,0],[-4,4],[0,0],[4,-6],[6,1],[10,7],[13,4],[11,2],[6,1],[5,3],[1,2],[0,0]]]}", "multi_intersection_points_x3"),
    GeoJsonDf("{\"type\":\"Polygon\",\"coordinates\":[[[0,0],[4,-6],[6,1],[10,7],[13,4],[11,2],[6,1],[5,3],[1,2],[0,0],[-6,0],[-4,4],[0,0]]]}", "multi_intersection_points_x4"),
    GeoJsonDf("{\"type\":\"Polygon\",\"coordinates\":[[[1.0,-1.0],[0.0,-2.0],[-1.0,-1.0],[0.0,0.0],[1.0,0.0],[0.0,2.0],[-1.0,1.0],[0.0,0.0],[1.0,-1.0]]]}", "polygon1"),
    GeoJsonDf("{\"type\":\"Polygon\",\"coordinates\":[[[0.0,-1.0],[-1.0,-1.0],[-1.0,0.0],[0.0,0.0],[1.0,0.0],[1.0,1.0],[0.0,1.0],[0.0,0.0],[0.0,-1.0]]]}", "polygon2"),

//    GeoJsonDf("{\"type\":\"MultiPolygon\",\"coordinates\":[[[[1.0,-1.0],[0.0,-2.0],[-1.0,-1.0],[0.0,0.0],[1.0,0.0],[0.0,2.0],[-1.0,1.0],[0.0,0.0],[1.0,-1.0]]]]}", "multipolygon1"), // wrong output
//    GeoJsonDf("{\"type\":\"MultiPolygon\",\"coordinates\":[[[[0.0,-1.0],[-1.0,-1.0],[-1.0,0.0],[0.0,0.0],[1.0,0.0],[1.0,1.0],[0.0,1.0],[0.0,0.0],[0.0,-1.0]]]]}", "multipolygon2"), // wrong output
//    GeoJsonDf("{\"type\":\"MultiPolygon\",\"coordinates\":[[[[1.0,-1.0],[0.0,-2.0],[-1.0,-1.0],[0.0,0.0],[1.0,0.0],[0.0,2.0],[-1.0,1.0],[0.0,0.0],[1.0,-1.0]]],[[[0.0,-1.0],[-1.0,-1.0],[-1.0,0.0],[0.0,0.0],[1.0,0.0],[1.0,1.0],[0.0,1.0],[0.0,0.0],[0.0,-1.0]]]]}", "multipolygon_1_2"), // wrong output

    GeoJsonDf("{\"type\":\"Polygon\",\"coordinates\":[[[1.0,-1.0],[0.0,-2.0],[-1.0,-1.0],[0.0,0.0],[1.0,0.0],[0.0,2.0],[-1.0,1.0],[0.0,0.0]]]}", "polygon1_missing_closing_ring"), 
    GeoJsonDf("{\"type\":\"Polygon\",\"coordinates\":[[[-1.0,-1.0],[-1.0,0.0],[0.0,0.0],[1.0,0.0],[1.0,1.0],[0.0,1.0],[0.0,0.0],[0.0,-1.0]]]}", "polygon2_missing_closing_ring"), 
//
//    GeoJsonDf("{\"type\":\"MultiPolygon\",\"coordinates\":[[[[1.0,-1.0],[0.0,-2.0],[-1.0,-1.0],[0.0,0.0],[1.0,0.0],[0.0,2.0],[-1.0,1.0],[0.0,0.0]]]]}", "multipolygon1_missing_closing_ring"), // wrong output
//    GeoJsonDf("{\"type\":\"MultiPolygon\",\"coordinates\":[[[[-1.0,-1.0],[-1.0,0.0],[0.0,0.0],[1.0,0.0],[1.0,1.0],[0.0,1.0],[0.0,0.0],[0.0,-1.0]]]]}", "multipolygon2_missing_closing_ring"), // wrong output
//    GeoJsonDf("{\"type\":\"MultiPolygon\",\"coordinates\":[[[[1.0,-1.0],[0.0,-2.0],[-1.0,-1.0],[0.0,0.0],[1.0,0.0],[0.0,2.0],[-1.0,1.0],[0.0,0.0],[1.0,-1.0]]],[[[0.0,-1.0],[-1.0,-1.0],[-1.0,0.0],[0.0,0.0],[1.0,0.0],[1.0,1.0],[0.0,1.0],[0.0,0.0]]]]}", "multipolygon_1_2_missing_closing_ring"), // wrong output
//
    GeoJsonDf("{\"type\":\"Polygon\",\"coordinates\":[[[0,-2],[-1.5,-0.5],[0,0],[1.5,0.5],[0,2],[-1.6667,1.6667],[0,0],[1.6,-1.6],[0,-2]]]}", "bowtie_2"),
    GeoJsonDf("{\"type\":\"Polygon\",\"coordinates\":[[[0,-2],[-1.6,-0.4],[0,0],[1.5,0.5],[0,2],[-1.6667,1.6667],[0,0],[1.6,-1.6],[0,-2]]]}", "wierd_bowtie_1"),
    GeoJsonDf("{\"type\":\"Polygon\",\"coordinates\":[[[0,-2],[-1.5,-0.5],[0,0],[1.6,0.4],[0,2],[-1.6667,1.6667],[0,0],[1.6,-1.6],[0,-2]]]}", "wierd_bowtie_2"),
    GeoJsonDf("{\"type\":\"Polygon\",\"coordinates\":[[[0,0],[1,8],[5,4],[4,1],[0,0],[-3,0],[-6,-3],[-5,-5],[-2,-4],[0,0],[-4,4],[-1,5],[0,0]]]}", "extra_polygon"),
//    GeoJsonDf("{\"type\":\"Polygon\",\"coordinates\":[[[-6,0],[-4,4],[0,0],[5,3],[6,1],[10,7],[11,2],[6,1],[0,0],[-6,0]]]}", "multi_intersection_points_1"), // wrong output
//    GeoJsonDf("{\"type\":\"Polygon\",\"coordinates\":[[[-6,0],[-4,4],[0,0],[1,2],[5,3],[6,1],[10,7],[11,2],[6,1],[0,0],[-6,0]]]}", "multi_intersection_points_2"), // wrong output
//    GeoJsonDf("{\"type\":\"Polygon\",\"coordinates\":[[[-6,0],[-4,4],[0,0],[1,2],[5,3],[6,1],[10,7],[13,4],[11,2],[6,1],[0,0],[-6,0]]]}", "multi_intersection_points_3"), // wrong output
//    GeoJsonDf("{\"type\":\"Polygon\",\"coordinates\":[[[-6,0],[-4,4],[0,0],[1,2],[5,3],[6,1],[10,7],[13,4],[11,2],[6,1],[4,-6],[0,0],[-6,0]]]}", "multi_intersection_points_4") // wrong output
  )

  geoJSons.foreach { geoDf =>
    decode[GeoJsonPolygon](geoDf.geoJson) match {
      case Right(polygon) =>
        println(s"ID: ${geoDf.id} - Decoded Polygon: $polygon")

        val coordinates = polygon.coordinates.flatten.map {
          case Seq(x, y) => Point(x, y)
        }
        println(s"Coordinates: $coordinates")

        val intersections = SelfIntersectionFinder.findSelfIntersections(coordinates)
        println(s"Found ${intersections.size} self-intersections for ID: ${geoDf.id}")
        intersections.foreach { p =>
          println(f"(${p._x}%.2f, ${p._y}%.2f) from edge ${p._i} to ${p._j}")
        }

        PolygonVisualizer.show(coordinates, intersections)
        PolygonVisualizer.saveImage(geoDf.id, coordinates, intersections)

      case Left(error) =>
        println(s"ID: ${geoDf.id} - Failed to decode GeoJSON: $error")
    }
  }
}
