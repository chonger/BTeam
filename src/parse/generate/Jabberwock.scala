package parse.generate


import collection.mutable.HashMap
import scala.util.Random

class Jabberwock(val pcfg : PCFG with PrintablePCFG) {
  
  val myRand = new Random(12345)
  
  var ntRuleMap = new HashMap[SplitNT,List[NTRule]]()
  pcfg.rules.foreach(r => {
    var rKey = SplitNT(r._1.lhs,r._1.split)
    var lst : List[NTRule] = ntRuleMap.getOrElse(rKey,List[NTRule]())
    var upd : List[NTRule] = r._1 :: lst
    ntRuleMap += (rKey -> upd)
  })
  /**
  //CHECK that probabilities sum to one
  ntRuleMap.keys.foreach(k => {
	  var sum = (0.0 /: ntRuleMap(k))((a,b) => a + pcfg.rules(b))
	  if(Math.abs(sum - 1.0) > .01) {
		  ntRuleMap(k).foreach(r => println(pcfg.ruleString(r) + " ---- " + pcfg.rules(r)))
		  throw new Exception("BAD SUM " + sum + "for " + pcfg.symbolStrings(k.id) + "/" + k.split.toInt)
	  } else
		  println("GOODSUM - " + pcfg.symbolStrings(k.id) + "/" + k.split.toInt)
    })
  */
  var termRuleMap = new HashMap[SplitNT,List[TerminalRule]]()
  pcfg.lexiconRules.foreach(r => {
    var rKey = SplitNT(r._1.lhs,r._1.split)
    var lst = termRuleMap.getOrElse(rKey,List[TerminalRule]())
    var upd = r._1 :: lst
    termRuleMap += (rKey -> upd)
  }) 
  
  //CHECK that probabilities sum to one
  termRuleMap.keys.foreach(k => {
	  var sum = (0.0 /: termRuleMap(k))((a,b) => a + pcfg.lexiconRules(b))
	  if(Math.abs(sum - 1.0) > .1)
		  println("BAD TSUM " + sum + " for " + pcfg.symbolStrings(k.id) + "/" + k.split)
	  else
		  println("GOODTSUM "+ sum + " - " + pcfg.symbolStrings(k.id) + "/" + k.split.toInt)
    })
  
  def genLex(nt : SplitNT) : TerminalRule = {
	  var possibles = termRuleMap(nt)
	  var rando = myRand.nextDouble
	  for {rule <- possibles} {
		  rando -= pcfg.lexiconRules(rule)
		  if(rando <= 0) return rule
	  }
	  println("RANDO = " + rando + " too small... stupid floating points.")	  
	  genLex(nt)
	  //throw new Exception("Some numerical underflow? " + pcfg.symbolStrings(nt.id) + "/" + nt.split)
	  
  }
  
  
  
  def genNT(nt : SplitNT) : NTRule = {
	  var possibles = ntRuleMap(nt)
	  var rando = myRand.nextDouble
	  for {rule <- possibles} {
		  rando -= pcfg.rules(rule)
		  if(rando <= 0) return rule
	  }
	  throw new Exception("Some numerical underflow?")
  }
  
  //Assumes that preterminals and nonterminals are distinct sets  
  def genRule(nt : SplitNT) : TreeRule = {
    if(ntRuleMap.get(nt).isDefined)
      genNT(nt)
    else
      genLex(nt)  
  }
  
  
  /**
   * Maximally unconstrained generation of a string of text
   */
  def speak() : String = {
    var tree = fillCanopy(genCanopy)
    tree.sentence(pcfg)
  }
  
  /*
   * What criteria is best for determining when to switch to a topic model?
   * The motivation is that the canopy only contains syntactic and
   * topic-independant semantic information.  We can limit the amount of specification
   * of a tree by setting a minimum canopy probability and expanding nodes until
   * the canopy slips below this threshold.  Unfortunately, this will be heavily biased
   * by our tree traversal strategy.  If we go DFS, we will only expand the left side
   * If we go BFS, we will not expand to high depths anywhere.
   * Our goal is to be able to make decisions at very 
   * low depths as well as high ones, and so as we increase in tree depth, we must only
   * expand nodes which vary less across domains. 
   * 
   * My first thought is to use the backward probability of a node.  
   * We can estimate this BP measure in many ways, the simplest perhaps
   * being the expected value. The expected value or
   * average over empirical occurances is a measure of how much more probability mass we expect
   * to lose after this node, and requires partitioning the nodes into classes over which
   * to calculate this measure.  The SM parser gives us some classes, but we might further
   * condition on other features which are readily available in the generative process, like
   * depth, ancestors, and siblings.  We would probably want to backoff to the class expected
   * values.
   * 
   * Another measure is entropy of a node.  This is presented in Samuelsson 1994.  
   * The entropy represents the amount of variability in outcomes from a node.  Here there are
   * two levels of decision to be made: 1 - as above, what id our class of node and 2 -
   * what constitutes an outcome?  2 can be anything from a per rule basis to a per child basis 
   * to an entire subtree basis.  In order to maintain a tractable solution i think using
   * the first n nodes in a BFS traversal of a node's subtree is a good metric, and as for 1
   * i believe the same distinction should be made as in above.
   * 
   * ----------------------------
   * 
   * We also might want to limit the overall complexity of our tree.  This can be done 
   * using our same BP measure.  If we bound the estimated probablilty in the tree then 
   * when we randomly choose skeletal constructions which are less common, we will leave less variability
   * for the topic model to fill in, while if we choose very common sentence patterns we will
   * allow the topic model more room in the tree.  This bound can be achieved by considering
   * the information in the the tree to consist of the expansion of a node, the extant specified canopy,
   * and the remaining underspecified nodes.
   * 
   * ------------------------------------------------------
   * 
   * Other goals:
   * Make sure open and close quotes come in pairs - generate tree segments, but how do we find them
   * 
   */
  def generate_?(fTree : FuncTree, node : TreeNode) = {
      //should we generate a new node at node for fTree 
    
        
    
      true 
  }
  
  def genCanopy() : FuncTree = {
    var fTree = new FuncTree()
    
    while(!fTree.stack.isEmpty) {
      var unode = fTree.stack.top
      if(generate_?(fTree,unode)) {
    	  val newRule = genRule(unode.symbol.asInstanceOf[SplitNT]) match {
    	    case BinaryRule(lhs,split,left,right) => {
    	    	new FuncTree(BinaryTreeNode(SplitNT(lhs,split),
                         UnderspecifiedNode(SplitNT(left._1,left._2)),
                         UnderspecifiedNode(SplitNT(right._1,right._2))))
    	    }
    	    case UnaryRule(lhs,split,kid)=> new FuncTree(UnaryTreeNode(SplitNT(lhs,split),
    	    										 UnderspecifiedNode(SplitNT(kid._1,kid._2))))
         	case TerminalRule(lhs,split,term) => new FuncTree(PreTerminalNode(SplitNT(lhs,split),
                                                              TerminalNode(Terminal(term))))
    	  }
    	  //println(pcfg.printTree(newRule))
    	  fTree(newRule)
    	  fTree.regenStack
      } else {
        fTree.stack.pop
      }
    }
    fTree
  }
  
  /**
   * Maybe this should take a topic model as an argument as well
   */
  def fillCanopy(fTree : FuncTree) : FuncTree = {
	fTree
  } 
}
