import scala.collection.mutable

class Graph(val nodes: mutable.Map[Int, mutable.Set[Int]]) {

	override def toString = nodes toString

	override def hashCode = nodes hashCode

    override def equals(other: Any) = other match { 
      case that: Graph => this.nodes == that.nodes
      case _ => false 
    }
	
}