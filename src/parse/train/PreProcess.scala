package parse.train

class PreProcess(data : List[ParseTreeData]) {
	
	var pcfg = new PCFG with PreProcessor with XBar with PrintablePCFG
  
	var ind = 0
	data.foreach(a => {a.init(pcfg)})

  	println("Reforming trees")
  	data.foreach(d => pcfg.xbar(d.tree)) //reform and annotate trees
	
  	println("Estimating from trees")
  	data.foreach(a => pcfg.getCounts(a.tree)) //estimate data for transform
  	
	//pcfg.takeTopNTerminals(3,data.map(_.tree)) //reduce terminal list
	
	//data.foreach(t => println("TREE\n" + pcfg.printTree(t.tree)))
 
	println("Re-estimating data")
	pcfg.clear //we have changes the trees, so clear estimated data
	data.foreach(a => pcfg.getCounts(a.tree)) //collect estimation data
 
	println("Initializing rules")
	pcfg.initRules //estimate rule probs from data
	//pcfg.printRules	
}
