package function


final case class Coordinate(range: (Int, Int)) { self =>

  def needsCombining(point: Coordinate): Boolean = {
    areIntersecting(point) || areAdjacent(point)
  }

  def areAdjacent(t1: Coordinate): Boolean = {
    val pointRange = t1.range

    !((range._1 - pointRange._1).abs == 1 && (range._2 - pointRange._2).abs == 1) &&    // check not diagonals
      ((range._1 - pointRange._1).abs == 1) || ((range._2 - pointRange._1).abs == 1) || // check points are 1 apart
        ((pointRange._2 - range._1).abs == 1) || ((range._2 - pointRange._2).abs == 1)
  }

  def pointIntersection(int: Int): Boolean = {
    if (range._1 <= int && range._2 >= int) true
    else false
  }

  def areIntersecting(point: Coordinate): Boolean = {
    val pointRange = point.range
    if (pointRange._1 <= range._1 && pointRange._2 >= range._1) true        // check if current Coordinate's range is between given point
    else if (range._1 <= pointRange._1 && range._2 >= pointRange._1) true   // check if point is between current Coordinate's range
    else false
  }

  def tupleCombine(point: Coordinate): Option[Coordinate] = {
    val pointRange = point.range
    
    needsCombining(point) match {
      case true => Some(Coordinate(range._1.min(pointRange._1),range._2.max(pointRange._2)))
      case false => None
    }
  }

  def compareWith(point: Coordinate): Int = {
    import scala.math.Ordering.Implicits._
    val pointRange = point.range

    if (range < pointRange) -1
    else if (range == pointRange) 0
    else 1
  }
}