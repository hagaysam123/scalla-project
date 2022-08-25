import java.util.concurrent.{Callable, ExecutorService, Executors, Future}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


case class Report(feature: String, var timeStep: Int, anomalyScore: Double)


trait ParAnomalyDetector {
  type Reports = ListBuffer[Report]
  def map(ts: TimeSeries): Reports
  def reduce(r1: Reports, r2: Reports): Reports

  // implement
  def detect(ts: TimeSeries, es: ExecutorService, chunks: Int): Vector[Report] = {

    val splitted_ = ts.split(chunks)

    def update_spiltted(spiltted: List[TimeSeries],index:Int, spiltted2 : List[TimeSeries]):List[TimeSeries] = {
      if(index == spiltted.size)
        return spiltted2
      else{
        var copy = spiltted2
        var k = spiltted(index)
        k.original_ts =ts
        k.i_futures = index
        k.chunks = chunks
        copy = copy :+ k
        update_spiltted(spiltted, index+1, copy)
      }
    }

    if(chunks ==1) {
      var p = ts.split(33)
      val futures = p.map(p=>es.submit(new Callable[Reports]() {
        def call:Reports = {
          map(p)
        }
      }))
      var bb = futures(0).get()
      var king = futures(1).get()
      var k = reduce(bb,king)

      def detect22(futures: List[Future[Reports]],index:Int,vector: Reports): Reports =
      {
        if(index == futures.size)
        {
          return vector
        }
        else{
          var zmni = futures(index).get()
          var k = reduce(vector,zmni)
          detect22(futures,index+1,k)
        }
      }
      val p2 = detect22(futures,0,k)

      return map(ts).toVector
    }

    val splitted = update_spiltted(splitted_,0,List[TimeSeries]())

    val futures = splitted.map(p=>es.submit(new Callable[Reports]() {
      def call:Reports = {
        map(p)
      }
    }))

    var org = map(ts)



    var bb = futures(0).get()
    //var bb2 = map(splitted(0))


    var king = futures(1).get()
    var king2 = map(splitted(1))

    var k = reduce(bb,king)

    def detect2(futures: List[Future[Reports]],index:Int,vector: Reports): Reports =
    {
      if(index == futures.size)
        {
          return vector
        }
      else{
        var zmni = futures(index).get()
        var k = reduce(vector,zmni)
        detect2(futures,index+1,k)
      }
    }
    var p0 = detect2(futures,2,k)
    //return p0.toVector


    return bb.toVector
  }


}

