package org.apache.spark.examples

object TailRec extends App {

  case class UT(u: Long, st: String, et: String)
  val list = List(UT(15335,"08:08","09:25"),UT(15330,"07:08","08:40"), UT(15335,"08:08","09:20"),UT(15332,"07:08","09:40"),  UT(15334,"08:38","09:40"),UT(15332,"07:08","09:50"))
  @annotation.tailrec
  def func1(list: List[UT], head: List[UT]): List[UT] = {
    val sortList = list.sortWith((a,b)=>{
      if(a.st < b.st){
        true
      }else if(a.st==b.st){
        if(a.et>b.et){
          true
        }else{
          false
        }
      }else{
        false
      }
    })
    val newHead = sortList.head
    val nHead = newHead :: head
    val tList = sortList.tail.filter(_.et > newHead.et)
    if (tList.isEmpty) {
      nHead.sortWith(_.st < _.st)
    }
    else {
      func1(tList, nHead)
    }
  }
  @annotation.tailrec
  def func2(list: List[UT], head: List[UT]): List[UT] = {
    val sortList = list.sortWith(_.et > _.et)
    val newHead = sortList.head
    val nHead = newHead :: head
    val tList = sortList.tail.filter(_.st < newHead.st)
    if (tList.isEmpty) {
      nHead.sortWith(_.et > _.et)
    }
    else {
      func2(tList, nHead)
    }
  }
  println(func1(list, List[UT]()))
}
