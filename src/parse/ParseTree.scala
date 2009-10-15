package parse

class ParseTree {
	var root : NonTerminalNode = null
 
	def this(aroot : NonTerminalNode) = {this() ; root = aroot}
 
 	def terminals : List[TerminalNode] = (nodes filter (_.isInstanceOf[TerminalNode])).asInstanceOf[List[TerminalNode]]
  	def nodes : List[TreeNode] = recGetNodes(root)
   
 	def recGetNodes(n : TreeNode) : List[TreeNode]= {
 	  n match { 
 	    case nt : NonTerminalNode => nt :: nt.children.flatMap(recGetNodes(_))
 	    case _ => List(n)
 	  }
 	}
  
 
}


