import scala.collection.mutable

object GraphBuilder {
	
	def build(lines: List[String]): Graph = {
		val nodes = mutable.Map.empty[Int, mutable.Set[Int]]
		for(line <- lines) {
			val parts = line split "\\s"
			val (tail, head) = (parts(0) toInt, parts(1) toInt)
			if(nodes contains tail) {
				nodes(tail) = (nodes(tail) += head)
			} else {
				nodes(tail) = mutable.Set(head)
			}

		}
		new Graph(nodes withDefaultValue mutable.Set.empty[Int])
	}

	def reverse(g: Graph): Graph = {
		val grev = mutable.Map.empty[Int, mutable.Set[Int]]
		for((tail, heads) <- g.nodes) {
			for(head <- heads) {
				if(grev contains head) {
					grev(head) = (grev(head) += tail)
				} else {
					grev(head) = mutable.Set(tail)
				}	
			}
		}
		new Graph(grev withDefaultValue mutable.Set.empty[Int])
	}

}