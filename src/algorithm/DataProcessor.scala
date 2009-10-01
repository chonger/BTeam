package algorithm

import java.io.File
import corpora._

abstract class DataProcessor[D <: BTeamData, R <: BTeamData](val data : List[D]) {
  
  def run(d : List[D]) : R 
  def main(args : Array[String]) : Unit = {  
    val file = new File(args(0))
    0
  }
  
}