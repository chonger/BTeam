package parse.generate

import collection.mutable.Stack


class FuncTreeException extends Exception
class FuncTreeApplyException extends FuncTreeException

class FuncTree extends ParseTree {
  
  val aroot = new UnderspecifiedNode(Root())
  root = aroot
  val stack = new Stack[UnderspecifiedNode]() //in Depth First traversal order, the underspecified symbols
  stack += aroot
 
  def this(aroot : NonTerminalNode) = {
	  this()
	  root = aroot
	  stack.clear
	  stack ++= underspecs
  }
  
  def deepCopy() = new FuncTree(TreeNode.deepCopy(root))
 
  def underspecs : List[UnderspecifiedNode] = 
    (nodes filter (n => n.isInstanceOf[UnderspecifiedNode] && 
                     n.asInstanceOf[UnderspecifiedNode].assignment == null)).asInstanceOf[List[UnderspecifiedNode]]
  
  /**
   * Not sure why i wrote this...
   */
  def findParents(n : TreeNode, above : TreeNode) : List[NonTerminalNode] = {
    above match {
      case t : TerminalNode => Nil
      case nt : NonTerminalNode => {
    	  if(nt.children.contains(n))
    		  List(nt)
    	  else
    		  nt.children.flatMap(c => findParents(n,c))
      }
    }
  }

  def regenStack() = {stack.clear ; stack ++= underspecs}
  
  /**
   *  This does not put the underspec nodes of t onto the stack
   *  To do this, run regenStack
   */
  def apply(t : FuncTree) : FuncTree = {
    if(stack.isEmpty)
      throw new FuncTreeApplyException
    if(stack.top.symbol == t.root.symbol) {
      var n = TreeNode.deepCopy(t.root)
      stack.top.assignment = n
      stack.pop
      return this
    }
    println(stack.top.symbol + " " + t.root.symbol)
    throw new FuncTreeApplyException 
  }
  
  def collapseUNodes() = root = recCollapse(root).asInstanceOf[NonTerminalNode]
  
  def recCollapse(n : NonTerminalNode) : TreeNode = {
    n match {
      case BinaryTreeNode(s,l,r) => BinaryTreeNode(s,
                  recCollapse(l).asInstanceOf[NonTerminalNode],
                  recCollapse(r).asInstanceOf[NonTerminalNode])
      case UnaryTreeNode(s,k) => UnaryTreeNode(s,recCollapse(k).asInstanceOf[NonTerminalNode])
      case PreTerminalNode(s,k) => n
      case ProtoTreeNode(s,k) => ProtoTreeNode(s,k.map(c => recCollapse(c).asInstanceOf[NonTerminalNode]))
      case un : UnderspecifiedNode => {
        if(un.assignment == null)
          UnderspecifiedNode(un.symbol)
        else
          recCollapse(un.assignment)
      }
    }
  }
  
  def sentence(pcfg : PCFG with PrintablePCFG) : String = {
    var utree = deepCopy()
    utree.collapseUNodes
    println(pcfg.printTree(utree))
    val yld = nodes filter (n => n.isInstanceOf[TerminalNode] || 
                              (n.isInstanceOf[UnderspecifiedNode] && 
                               n.asInstanceOf[UnderspecifiedNode].assignment == null))
    
    ("" /: yld)((a,b) => {
                            var str = b match {
                            	case TerminalNode(sym) => pcfg.terminalStrings(sym.id)
                            	case UnderspecifiedNode(sym) => "*" + pcfg.symbolStrings(sym.id) + "*"
    						}
                            a + " " + str
    })
  } 
}
