
import scala.io.Source
class TimeSeries(csvFileName:String) {

  val csvFileName2 =csvFileName
  // val features=???
  var l = List[String]()
  val source=Source.fromFile(csvFileName)
  source.getLines().foreach(line=>{
    l = l:+(line)
  })
  source.close()
  //val p = source.getLines().map(_.split(","))
  val p = Source.fromFile(csvFileName).getLines().map(_.split(","))
  var hagrid = l.map(_.split(","))


  val features= p.toList(0)
  //val iter = source.getLines().drop(0).map(_.split(","))
  var iter = Source.fromFile(csvFileName).getLines().drop(1).map(_.split(","))
  var l2 = l.drop(1)
  var iter_col = iter.toList.transpose
  var iter_col2 = iter.toList

  val length: Int = l.length -1
  //val l_2 =  List(l).transpose

  var original_ts :TimeSeries = null
  var i_futures :Int = 0
  var chunks :Int = 0

  // given name of a feature return in O(1) its value series
  def getValues(feature:String):Option[Vector[Double]]= {
      if(features.contains(feature)) {
        val i = features.indexOf(feature)
        return Some((iter_col(i).flatMap(s => scala.util.Try(s.toDouble).toOption)).toVector)
      }
      else{
        None
      }
  }

  // given name of a feature return in O(1) its value at the given time step
  def getValue(feature:String,timeStep:Int):Option[Double]={

    if(getValues(feature)!= None){
      val h :Vector[Double] = getValues(feature).get
      if(timeStep < h.length && timeStep>=0) return  Some(h(timeStep))
      else return None
    }
    else{
      return None
    }
  }

  // given name of a feature return its value series in the range of indices
  def getValues(feature:String,r:Range):Option[Vector[Double]]={

    def ts_helper(feature:String,v:Vector[Double],index_list:List[Int],index:Int,i :Int):Option[Vector[Double]] ={
      //index == index_list.length+1 ||
      if(getValue(feature,index)== None){
        return None
      }
      else{
        val apend = getValue(feature,index).get
        var v2 = v:+apend
        if(i+1 == index_list.length) return  Some(v2)
        else ts_helper(feature, v2, index_list, index_list(i+1),i+1)
      }
    }

    val index_list : List[Int] = r.toList
    if (getValues(feature)!= None){
      var h :Vector[Double] = Vector[Double]()
      var p :Option[Vector[Double]] = ts_helper(feature, h, index_list, index_list(0),0)
      return p
    }
    else{
      return None
    }
  }

  def split(n: Int):List[TimeSeries] = {
    var pp_ap = this.l2.grouped(n).toList
    def add_features(pp_ap : List[List[String]],empty_list :List[List[String]],features: Array[String],index: Int): List[List[String]] = {
      if (index == pp_ap.size){
        return empty_list
      }
      else {
        if(index == pp_ap.size-2){
          if(pp_ap(pp_ap.size-1).size < pp_ap(pp_ap.size-2).size){
            var eml2 = empty_list
            var k = pp_ap(index)
            var k2 = pp_ap(index+1)
            var k3 = l(0)
            var k4 = k:::k2
            var k5 = k3 +: k4
            eml2 = eml2 :+ k5
            return eml2
          }
        }
        var eml2 = empty_list
        var k = pp_ap(index)
        var k2 = l(0)
        var k3 = k2 +: k
        eml2  = eml2:+k3
        add_features(pp_ap,eml2,features,index+1)
      }
    }
    var chunks = add_features(pp_ap,List[List[String]](),this.features,0)

    def make_chunks(chunks: List[List[String]],index:Int, ts_chunks: List[TimeSeries]): List[TimeSeries] = {
      if(index == chunks.size) return ts_chunks
      else{
        var ts_ = new TimeSeries(this.csvFileName)
        var a = chunks(index).drop(1).map(_.split(","))
        var b = a.transpose

        ts_.iter_col = b
        var ts_chunks2 = ts_chunks
        ts_chunks2 =  ts_chunks2 :+ ts_
        make_chunks(chunks,index+1,ts_chunks2)
      }
    }

    var list_of_ts = make_chunks(chunks,0,List[TimeSeries]())

    return list_of_ts
  }

}
