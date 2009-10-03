package parse

import collection.mutable.HashMap

abstract class PCFG() {
  val rules = HashMap[TreeRule,Double]()
  var symbolStrings = List[String]("ROOT")
  val symbolIDs = new HashMap[String,ParseTypes.Symbol]()
  symbolIDs += ("ROOT" -> ParseTypes.Root)
  var terminalStrings = List[String]("Empty","UNK")
  val terminalIDs = HashMap[String,ParseTypes.Terminal]()
  terminalIDs += ("UNK" -> ParseTypes.UNK, "Empty" -> ParseTypes.Empty)
  def clear = rules.clear
}

trait PreProcessor extends PCFG {
  var nextSymID : ParseTypes.Symbol = ParseTypes.Root
  var nextTermID : ParseTypes.Terminal = ParseTypes.UNK
  
  def doTransform(data : List[ParseTreeData]) = {
    transform(data)
    reproc(data)
  }
  
  def transform(data : List[ParseTreeData]) = {}
  def revert(data : List[ParseTreeData]) = {}
  
  def addSymbol(s : String) = symbolIDs.getOrElseUpdate(s,{
	  //println("adding symbol " + s)
	  symbolStrings =  (symbolStrings.reverse.::(s)).reverse //TODO this sucks
	  NonTerminal.splits = NonTerminal.splits ++ List(List(ParseTypes.Unsplit))
	  nextSymID += 1
	  nextSymID
  	})
  
  def addTerm(s : String) : ParseTypes.Symbol = terminalIDs.getOrElseUpdate(s,{
	  //println("adding terminal " + s + " " + (nextTermID + 1))
	  terminalStrings =  (terminalStrings.reverse.::(s)).reverse //TODO see above note of suckage
	  nextTermID += 1
	  nextTermID
  	})
  
  val terminalCounts = HashMap[ParseTypes.Symbol,Int]() 
  val nonterminalCounts = HashMap[ParseTypes.Symbol,Int]()
  val ruleCounts = HashMap[TreeRule,Int]()
  
  def reproc(data : List[ParseTreeData]) = {
    terminalCounts.clear
  	nonterminalCounts.clear
  	ruleCounts.clear
  	data.foreach(d => getCounts(d.tree))
    initRules
  }
  
  def init(data : List[ParseTreeData]) = {
    clear
    data.foreach(d=> d.init(this))
    reproc(data)
  }
  
  override def clear = {
    super.clear
  	terminalCounts.clear
  	nonterminalCounts.clear
  	ruleCounts.clear
  }
  
  def getCounts(tree : ParseTree) = {
	  recGetCounts(tree.root)
  }	
  
  def recGetCounts(n : TreeNode) : Unit = {
    n match {
	  case t : TerminalNode => { //inc the count of this terminal
		  var count = terminalCounts.getOrElse(t.symbol.id,0) + 1
  	   	  terminalCounts += (t.symbol.id -> count)	  
  	  }
	  case nt : NonTerminalNode => {
  	   	  //inc the nt's count
  	   	  var ntCount = nonterminalCounts.getOrElse(nt.symbol.id,0) + 1
  	   	  nonterminalCounts += (nt.symbol.id -> ntCount)
         
  	   	  //inc the rule's count
  	   	  nt.rules.foreach(r => {
  	   	  	var rCount = ruleCounts.getOrElse(r,0) + 1
  	   	  	ruleCounts += (r -> rCount)
          })
         
  	   	  //recursively continue committing data
  	   	  nt.children.foreach(recGetCounts(_))
  	   	}
	  }
  }
  
  //used when all trees are loaded in to estimate pcfg probabilities
  def initRules() = {
	  rules.clear
	  ruleCounts.foreach(a => rules += (a._1 -> a._2.toDouble / 
                                   nonterminalCounts(a._1.lhs).toDouble))   
  }
  
  /**
   This stuff is needed, but not yet
  //remove all Terminals not in the set of most common n Terminals
  def takeTopNTerminals(n : Int, trees : List[ParseTree]) = {
	  var sortedTerminals = terminalCounts.toList sort ((a,b) => a._2 > b._2)
	  if(n < sortedTerminals.length) {
		  terminalCounts.clear
		  sortedTerminals.drop(sortedTerminals.length - n).map(terminalCounts += _)
	  } 
  }

  //remove all Terminals with count less than n
  def takeTermsWithCount(n : Int) = {
	  terminalCounts.filterKeys(terminalCounts(_) < n).map(terminalCounts -= _._1)
  }
  */
  
  
}
  