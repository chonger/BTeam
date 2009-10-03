package parse

class ParseTree {
	var root : NonTerminalNode = null
 
	def this(aroot : NonTerminalNode) = {this() ; root = aroot}
 
 	def invalidateNodelists() = {storedNodes = null} 
 	private var storedNodes : List[TreeNode] = null 
 	def terminals : List[TreeNode] = nodes filter (_.isInstanceOf[TerminalNode])
  	def nodes : List[TreeNode] = {
 	  if(storedNodes != null) return storedNodes else {
 	    storedNodes = recGetNodes(root)
 	    storedNodes
      }
    }
 	def recGetNodes(n : TreeNode) : List[TreeNode]= {
 	  n match { 
 	    case nt : NonTerminalNode => nt :: nt.children.flatMap(recGetNodes(_))
 	    case _ => List(n)
 	  }
 	}
}


