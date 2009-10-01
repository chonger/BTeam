package parse.train

import algorithm.BTeamAlgorithm
import collection.mutable.HashMap
import corpora.BTeamData
import parse._

class InsideOutsideAlgorithm(data : List[ParseTreeData], val pcfg : PCFG with PrintablePCFG) extends BTeamAlgorithm[ParseTreeData](data) {
  
	var logProb = 0.0
	val expectations = new HashMap[TreeRule,Double]()
	val expectationTotals = new HashMap[(ParseTypes.Symbol,ParseTypes.Split),Double]()
 
	def map() = {}
	def reduce() = {}
	def run(args : List[String]) = {
	  expectations.clear
	  expectationTotals.clear
	  logProb = 0.0
	  data.map(d => processTree(d.tree))
	}
  
	//def pickChildSplits(n : TreeNode, r : TreeRule) = n.children zip (r.rhs.map(_._2))
 
	def addToMap[A](map : HashMap[A,Double], key : A, update : Double) = {
	  val upd = map.getOrElse(key,0.0) + update
	  map += (key -> upd)
	}
 
	type IOType = HashMap[(TreeNode,ParseTypes.Split),Double]

	def getIOProbs(tree : ParseTree) = {
		val insideProbs = new IOType()
		val outsideProbs = new IOType()
		
		def getInside(n : TreeNode) : Unit = {
			n match {
			  case ptn : PreTerminalNode => ptn.rules.foreach(rule => {
				  var inprob = 0.0
				  try {
				    inprob = pcfg.rules(rule)
				  } catch {
				    case e : NoSuchElementException => println(pcfg.ruleString(rule))
				  }
				  addToMap(insideProbs,(ptn,rule.split),inprob)
			  })  
			  case un : UnaryTreeNode => {
			    getInside(un.kid)
			    un.rules.foreach(rule => {
			    	var inprob = pcfg.rules(rule) * insideProbs(un.kid,rule.kid._2)
			    	addToMap(insideProbs,(un,rule.split),inprob)
			    })
			  }
			  case bn : BinaryTreeNode => {
				  getInside(bn.left)
				  getInside(bn.right)
				  bn.rules.foreach(rule => {
			    	var inprob = pcfg.rules(rule) * 
			    				insideProbs(bn.left,rule.left._2) * 
			    				insideProbs(bn.right,rule.right._2)
			    	addToMap(insideProbs,(bn,rule.split),inprob)
				  })
			  }
			}
		}
	
		def getOutside(n : NonTerminalNode) : Unit = {
			n match {
			  case ptn : PreTerminalNode => {/*do nothing*/} 
			  case un : UnaryTreeNode => {
			    un.rules.map(rule => {
			    	var update = outsideProbs((un,rule.split)) * pcfg.rules(rule)
				    addToMap(outsideProbs,(un.kid,rule.kid._2),update)
			    })
			    getOutside(un.kid)
			  }
			  case bn : BinaryTreeNode => {
				  bn.rules.map(rule => {
					  var update = outsideProbs((bn,rule.split)) * pcfg.rules(rule)
					  addToMap(outsideProbs,(bn.left,rule.left._2),update * insideProbs(bn.right,rule.right._2))
					  addToMap(outsideProbs,(bn.right,rule.right._2),update * insideProbs(bn.left,rule.left._2))
				  })
				  getOutside(bn.left)
				  getOutside(bn.right)
			  }
			}		
		}
  
		getInside(tree.root)
		outsideProbs += ((tree.root,ParseTypes.Unsplit) -> 1.0)
		getOutside(tree.root.asInstanceOf[NonTerminalNode])
	
		(insideProbs,outsideProbs)
	}
 
	def processTree(tree : ParseTree) : Unit = {
		
		val (insideProbs,outsideProbs) = getIOProbs(tree)
			
		//println("INSIDE")
		//insideProbs.filter(_._1._1.symbol == 1).foreach(k => println("IN" + pcfg.nodeString(k._1._1) + "-" + k._1._2.toInt + " -- " + k._2))
		//insideProbs.foreach(k => println("" + pcfg.nodeString(k._1._1) + "-" + k._1._2.toInt + " -- " + k._2))
		//println("OUTSIDE")
		//outsideProbs.filter(a => pcfg.symbolStrings(a._1._1.symbol.asInstanceOf[NonTerminal].id) == "NNP").foreach(k => println("" + pcfg.nodeString(k._1._1) + "-" + k._1._2.toInt + " -- " + k._2))
		//outsideProbs.foreach(k => println("" + pcfg.nodeString(k._1._1) + "-" + k._1._2.toInt + " -- " + k._2))
		
		def getTotalProb(n : TreeNode) : Double = {
		  n match {
		    case nt : NonTerminalNode => 
		      (0.0 /: nt.symbol.splits)((a,b) => a + insideProbs((n,b)) * outsideProbs((n,b)))
		    case _ => 0.0 //meaningless, but satisfies check 
		  }
		}
  
		var totalProb = getTotalProb(tree.root)
		if(totalProb == 0) {
		  /*
		  println("PROB = 0 ")
		  println(pcfg.printTree(tree))
		  println("INSIDE")
		  insideProbs.filter(_._1._1.symbol == 1).foreach(k => println("IN" + pcfg.nodeString(k._1._1) + "-" + k._1._2.toInt + " -- " + k._2))
		  insideProbs.foreach(k => println("" + pcfg.nodeString(k._1._1) + "-" + k._1._2.toInt + " -- " + k._2))
		  println("OUTSIDE")
		  outsideProbs.filter(a => pcfg.symbolStrings(a._1._1.symbol.asInstanceOf[NonTerminal].id) == "NNP").foreach(k => println("" + pcfg.nodeString(k._1._1) + "-" + k._1._2.toInt + " -- " + k._2))
		  outsideProbs.foreach(k => println("" + pcfg.nodeString(k._1._1) + "-" + k._1._2.toInt + " -- " + k._2))
		  throw new Exception("ZEROPROB")
		  */
		  println("ZEROPROB")
		  return
		}
		logProb += Math.log(totalProb)
		def check(tree : ParseTree) = {
			var ok = true
			tree.nodes.filter(_.isInstanceOf[NonTerminalNode]).foreach(n => {
				if(Math.abs(getTotalProb(n) - totalProb) > 1e-16) {
					println("BAD CHECK " + pcfg.nodeString(n) + " -> " + getTotalProb(n) + "/" + totalProb + " -- " + (totalProb - getTotalProb(n)))
					ok = false
				}
			})
			ok
		}
		check(tree)
			
  
		var expects = tree.nodes.flatMap(n => 
		  	n match {
		  	  case t : TerminalNode => Nil
		  	  case p : PreTerminalNode => {
		  		  for{rule <- p.rules} yield {
		  		    //println(" P " + pcfg.nodeString(n) + " - " + rule.split.toInt + " --> " + pcfg.rules(rule) * outsideProbs((n,rule.split)))
		  		    (rule,pcfg.rules(rule) * outsideProbs((n,rule.split)))
		  		  }

		  	  }
		  	  case u : UnaryTreeNode => {
		  		  //println(" U " + pcfg.nodeString(n))
		  		  for{rule <- u.rules} yield 
		  		  (rule,pcfg.rules(rule) * outsideProbs((n,rule.split)) * insideProbs((u.kid,rule.kid._2)))
		  	  }
		  	  case b : BinaryTreeNode => {
		  		  //println(" B " + pcfg.nodeString(n))
		  		  for{rule <- b.rules} yield
		  		  (rule,pcfg.rules(rule) * outsideProbs(n,rule.split) * 
		  				  insideProbs((b.left,rule.left._2)) * insideProbs((b.right,rule.right._2))) 
		  	  }
		  	})
		//println("TOTAL = " + totalProb)
	  	expects.foreach(e => {
	  		/**
	  		if(e._1.lhs == 9) {
	  			println("\tUPDATE "  + pcfg.ruleString(e._1) + " - " + e._2 )
	  			println("\tTOTALP = " + totalProb)
	  			println("\tRULEP = "  + pcfg.rules(e._1))
	  			outsideProbs.filter(a => pcfg.symbolStrings(a._1._1.symbol.asInstanceOf[NonTerminal].id) == "NN").foreach(k => println("" + pcfg.nodeString(k._1._1) + "-" + k._1._2.toInt + " -- " + k._2))
	  		}
            */
	  	  	addToMap(expectations,e._1,e._2 / totalProb)
	  		addToMap(expectationTotals,(e._1.lhs,e._1.split),e._2 / totalProb)
	  	}) 
	}	 
}
