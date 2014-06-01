import scala.io.Source
import scala.collection.mutable.ListBuffer

object ArrayBuilder {
	
	def build(fileName: String): Array[Int] = {
		val l = ListBuffer.empty[Int]
		val lines = Source.fromURL(ArrayBuilder.getClass.getResource(fileName)).getLines
		for(line <- lines) {
			l += (line toInt)
		}
		(l toArray) sorted
	}
}