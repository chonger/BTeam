package parse

import collection.mutable.HashMap
import io.Source
import java.io.StringReader

abstract class PCFG() {
  var rules = HashMap[NTRule,Double]()
  var lexiconRules = HashMap[TerminalRule,Double]()
  var symbolStrings = List[String](ParseTypes.RootString)
  val symbolIDs = new HashMap[String,ParseTypes.Symbol]()
  symbolIDs += (ParseTypes.RootString -> ParseTypes.Root)
  var terminalStrings = List[String](ParseTypes.EmptyString,ParseTypes.UnkString)
  val terminalIDs = HashMap[String,ParseTypes.Terminal]()
  terminalIDs += (ParseTypes.UnkString -> ParseTypes.UNK, 
                  ParseTypes.EmptyString -> ParseTypes.Empty)
  def clear = rules.clear
}

trait FileBased extends PreProcessor {
	
	def read(filename : String) = {
	  readSplits(filename + ".splits")
	  //all NT's are entered by now, no grammar rules yet
	  readGrammar(filename + ".grammar")
	  println("grammar rules - " + rules.size)
	  readLexicon(filename + ".lexicon")
	  println("grammar rules - " + rules.size)
	}
 
	def readSplits(filename : String) = {	
		val splitstrs = Source.fromFile(filename).getLines.toList //toList is needed to know length
  
		//use an ArrayBuffer based object before setting the final split tree
		import scala.collection.mutable.ArrayBuffer
		//plus one b/c root is not in the splits file and is never split
		val readerTree = new Array[ArrayBuffer[ArrayBuffer[Tuple2[ParseTypes.Split,Int]]]](splitstrs.length + 1)
  
		//fill in the first tree with an unsplit root
		readerTree(ParseTypes.Root) = new ArrayBuffer[ArrayBuffer[Tuple2[ParseTypes.Split,Int]]]()
		readerTree(ParseTypes.Root) += (new ArrayBuffer[Tuple2[ParseTypes.Split,Int]]())
		readerTree(ParseTypes.Root)(0) += (ParseTypes.Unsplit,0)
  
		splitstrs.map(_.split("\t")).foreach(spl => {
			val sym = NonTerminal(addSymbol(spl(0)))
			readerTree(sym.id) = new ArrayBuffer[ArrayBuffer[Tuple2[ParseTypes.Split,Int]]]()

			val stream = new StringReader(spl(1))
			stream.read
 
			def readSplitTreeNode(pindex : Int, depth : Int) : Unit = {			
				if(readerTree(sym.id).size <= depth)
					readerTree(sym.id) += (new ArrayBuffer[Tuple2[ParseTypes.Split,Int]]())
    
			    var c = stream.read.toChar
			    var index = readerTree(sym.id)(depth).size
			    var idStr = ""
			    var idList = List[ParseTypes.Split]() 
       
				while(stream.ready) {
					c match {
						case '(' => readSplitTreeNode(index, depth + 1)
						case ')' => {
						  if(idStr.length != 0) {
							  idList ++= List(ParseTypes.parseSplit(idStr))
							  idStr = ""
						  }
						  
						  readerTree(sym.id)(depth) += (idList(0),pindex)     
						  
						  NonTerminal.splitTrees = readerTree.flatMap(a => {
							  if(a == null)
								  Nil
							  else		
								  List(a.toList.map(b => b.toList))
						  })
						  idList.drop(1).foreach(e => {
							  if(readerTree(sym.id).size <= depth + 1)
								  readerTree(sym.id) += (new ArrayBuffer[Tuple2[ParseTypes.Split,Int]]())
							  readerTree(sym.id)(depth + 1) += (e,index)
						  })
						  return
						}
						case _ if (c == ' ') => {
							if(idStr.length != 0) {
								idList ++= List(ParseTypes.parseSplit(idStr))
								idStr = ""
							}
						}
						case _ => {
							idStr += c
						}
					}
					c = stream.read.toChar
				}
			}
			readSplitTreeNode(0,0)
		})
		NonTerminal.splitTrees = readerTree.map(a => a.toList.map(b => b.toList))
		NonTerminal.maxSplit = NonTerminal.splitTrees(1).size
	}
 
	def readGrammar(filename : String) = {
		def getSplitNT(s : String) = {
			var parts = s.split("\\^g_")
			if(parts.length == 1) //it seems that if a symbol is never split, ^g is not used
				parts = s.split("_")
			SplitNT(addSymbol(parts(0)),ParseTypes.parseSplit(parts(1)))
		}
	  
		val frules = Source.fromFile(filename).getLines
		frules.map(_.split("\\s")).foreach(r => {
			val len = r.length
			val lhs = getSplitNT(r(0))
			val rhs = r.slice(2,len - 1).map(a => {val b = getSplitNT(a) ; (b.id,b.split)})	
			val prob = r(len - 1).toDouble
			if(rhs.size == 2)
				rules += (BinaryRule(lhs.id,lhs.split,rhs(0),rhs(1)) -> prob)
			else {
				if(!((lhs.id == rhs(0)._1) && (lhs.split == rhs(0)._2) && (prob == 1.0)))
					rules += (UnaryRule(lhs.id,lhs.split,rhs(0)) -> prob)
			}
		})
	}
 
	def readLexicon(filename : String) = {
	  def tidy(s : String) = "[,\\[\\]]".r.replaceAllIn(s,"")
	  val frules = Source.fromFile(filename).getLines
	  frules.map(_.split("\\s")).foreach(r => {
		  val lhs = addSymbol(r(0))
		  val rhs = addTerm(r(1))

		  val tidied = r.drop(2).map(tidy(_)).toList
		  tidied.zipWithIndex.foreach(v => {
			  lexiconRules += (TerminalRule(lhs,ParseTypes.parseSplit(v._2.toString),rhs) -> v._1.toDouble)
		  })
	  })
	}  
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
	  nextSymID += 1
	  nextSymID
  	})
  
  def addTerm(s : String) : ParseTypes.Terminal = terminalIDs.getOrElseUpdate(s,{
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
    var i = 0
    data.foreach(d=> {i = i + 1; if(i % 1000 == 0) {println(i)} ; d.init(this)})
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
    /**
	  rules.clear
	  ruleCounts.foreach(a => rules += (a._1 -> a._2.toDouble / 
                                   nonterminalCounts(a._1.lhs).toDouble))
   */
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
  