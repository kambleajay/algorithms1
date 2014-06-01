class Solution(val target: Int, val first: Int, val second: Int) {
	
	override def toString = first +" + " +second +" = " +target

	override def equals(other: Any) = other match {
			case that: Solution => (target == that.target)
			case _ => false
		}

	override def hashCode = target hashCode

}