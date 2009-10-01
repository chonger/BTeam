package algorithm

import corpora._

abstract class BTeamAlgorithm[D <: BTeamData](val data : List[D]) {

  def run(args : List[String]) 
  def map()
  def reduce()
  def main(args : List[String]) : Unit = {
	  run(args)
	  1
  }
  
}
