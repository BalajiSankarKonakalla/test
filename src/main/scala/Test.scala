import scala.annotation.tailrec
class Test {

  @tailrec
  private def itrLst(a: List[String], b: List[String] = List[String]()): List[String] ={

    if(a.isEmpty) return b.reverse
    else {
      val current = a.head

      if(isList(current)){
        if(b.nonEmpty){
          val newEle = b.head
          if(isList(newEle)){   // scenario "p-q" and "r-s"
            val initNum = newEle.split('-')
            val x = initNum.last.toInt
            val y = current.split('-')(0).toInt
            if( x+1 == y ){
              itrLst(a.drop(1), initNum(0) + '-' + current.split('-').last :: b.drop(1))
            }else {
              itrLst(a.drop(1), current :: b)
            }
          } else {                 // scenario "p-q" and r
            val x: Int = newEle.toInt
            val y = current.split('-')(0).toInt
            if(x+1 == y){
              itrLst(a.drop(1), x.toString + '-' + current.split('-').last :: b.drop(1))
            }else {
              itrLst(a.drop(1), current :: b)
            }
          }
        } else {
          itrLst(a.drop(1), a.head :: Nil)   // scenario "p-q" and ""
        }

      } else { // current is not list
        if(b.nonEmpty){
          val newEle = b.head
          if(isList(newEle)){                  // scenario "p" and "r-s"
            val x = newEle.split('-').last.toInt
            val y = current.toInt
            if(x+1 == y){
              itrLst(a.drop(1), newEle.split('-')(0) + '-' + y :: b.drop(1))
            }else{
              itrLst(a.drop(1), current :: b)
            }
          }else {                               // scenario "p" and "r"
            val x = newEle.toInt
            val y = current.toInt
            if(x+1 == y){
              itrLst(a.drop(1), x.toString + '-' + y :: b.drop(1))
            }else {
              itrLst(a.drop(1), current :: b)
            }
          }
        }else{
          itrLst(a.drop(1), a.head :: Nil)            // scenario "p" and ""
        }
      }
    }

  }

  def createRange(sample_str: String): String = {
    val input_str: List[String] = sample_str.split(',').toList

    itrLst(input_str).mkString(",")
  }

  def isList(ele: String):Boolean ={
    ele.contains('-')
  }

}
