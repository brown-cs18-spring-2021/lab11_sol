/**
 * A class representing a graph in array form
 * @param graph the array graph to check
 */

class ArrayGraph(graph: Array[Any]) {

  /**
   *
   * @return true if an array is a valid representation of a graph, false if
   *         otherwise
   */
  def isValid(): Boolean = {
    for (element <- graph) {
      element match {
        case vertex: String =>
        case edge: Int => graph(edge) match {
          case vertex: String =>
          case edge: Int => return false
        }
      }
    }
    true
  }

  /**
   *
   * @param start the index to start at
   * @return a set of indices representing vertices that are accessible to start
   */
  def markGraph(start: Int): Set[Int] = {
    graph(start) match {
      case edge: Int => throw new IllegalArgumentException
      case vertex: String => {
        var results: Set[Int] = Set()
        var queue: List[Int] = List(start)
        while (!queue.isEmpty) {
          val top :: newQueue = queue
          queue = newQueue
          if (!results.contains(top)) {
            results += top
            var acc = 1
            var go = true
            while (go && (top + acc) < graph.length) {
              graph(top + acc) match {
                case edge: Int => queue = edge :: queue
                case vertex: String => go = false
              }
              acc += 1
            }
          }
        }
        results
      }
    }
  }

  /**
   *
   * @param startLocations list of starting locations
   * @return a set of indices representing vertices accessible to the
   *         starting locations
   */
  def usedLocations(startLocations: List[Int]): Set[Int] = {
    var results: Set[Int] = Set()
    for (location <- startLocations) {
      results ++= markGraph(location)
    }
    results
  }

  // this might be helpful to write for debugging!
  override def toString: String = {
    var output = ""
    for (item <- graph) {
      item match {
        case vertex: String => output += (vertex + "\n")
        case edge: Int => output += (" => " + graph(edge) + "\n")
      }
    }
    output
  }
}