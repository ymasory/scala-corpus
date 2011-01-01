package blueeyes.json.xschema {

import blueeyes.json.JsonAST._
import java.util.{Date => JDate}

/** Decomposers for all basic types.
 */
trait DefaultDecomposers {
  implicit val JValueDecomposer: Decomposer[JValue] = new Decomposer[JValue] {
    def decompose(tvalue: JValue): JValue = tvalue
  }
  
  implicit val StringDecomposer: Decomposer[String] = new Decomposer[String] {
    def decompose(tvalue: String): JValue = JString(tvalue)
  }
  
  implicit val BooleanDecomposer: Decomposer[Boolean] = new Decomposer[Boolean] {
    def decompose(tvalue: Boolean): JValue = JBool(tvalue)
  }
  
  implicit val IntDecomposer: Decomposer[Int] = new Decomposer[Int] {
    def decompose(tvalue: Int): JValue = JInt(BigInt(tvalue))
  }
  
  implicit val LongDecomposer: Decomposer[Long] = new Decomposer[Long] {
    def decompose(tvalue: Long): JValue = JInt(BigInt(tvalue))
  }
  
  implicit val FloatDecomposer: Decomposer[Float] = new Decomposer[Float] {
    def decompose(tvalue: Float): JValue = JDouble(tvalue.toDouble)
  }

  implicit val DoubleDecomposer: Decomposer[Double] = new Decomposer[Double] {
    def decompose(tvalue: Double): JValue = JDouble(tvalue)
  }
  
  implicit val DateDecomposer: Decomposer[JDate] = new Decomposer[JDate] {
    def decompose(date: JDate): JValue = JInt(date.getTime)
  }
  
  implicit def OptionDecomposer[T](implicit decomposer: Decomposer[T]): Decomposer[Option[T]] = new Decomposer[Option[T]] {
    def decompose(tvalue: Option[T]): JValue = tvalue match {
      case None    => JNull
      case Some(v) => decomposer.decompose(v)
    }
  }
  
  implicit def Tuple2Decomposer[T1, T2](implicit decomposer1: Decomposer[T1], decomposer2: Decomposer[T2]): Decomposer[(T1, T2)] = new Decomposer[(T1, T2)] {
    def decompose(tvalue: (T1, T2)) = JArray(decomposer1(tvalue._1) :: decomposer2(tvalue._2) :: Nil)
  }
  
  implicit def Tuple3Decomposer[T1, T2, T3](implicit decomposer1: Decomposer[T1], decomposer2: Decomposer[T2], decomposer3: Decomposer[T3]): Decomposer[(T1, T2, T3)] = new Decomposer[(T1, T2, T3)] {
    def decompose(tvalue: (T1, T2, T3)) = JArray(decomposer1(tvalue._1) :: decomposer2(tvalue._2) :: decomposer3(tvalue._3) :: Nil)
  }
  
  implicit def Tuple4Decomposer[T1, T2, T3, T4](implicit decomposer1: Decomposer[T1], decomposer2: Decomposer[T2], decomposer3: Decomposer[T3], decomposer4: Decomposer[T4]): Decomposer[(T1, T2, T3, T4)] = new Decomposer[(T1, T2, T3, T4)] {
    def decompose(tvalue: (T1, T2, T3, T4)) = JArray(decomposer1(tvalue._1) :: decomposer2(tvalue._2) :: decomposer3(tvalue._3) :: decomposer4(tvalue._4) :: Nil)
  }
  
  implicit def Tuple5Decomposer[T1, T2, T3, T4, T5](implicit decomposer1: Decomposer[T1], decomposer2: Decomposer[T2], decomposer3: Decomposer[T3], decomposer4: Decomposer[T4], decomposer5: Decomposer[T5]): Decomposer[(T1, T2, T3, T4, T5)] = new Decomposer[(T1, T2, T3, T4, T5)] {
    def decompose(tvalue: (T1, T2, T3, T4, T5)) = JArray(decomposer1(tvalue._1) :: decomposer2(tvalue._2) :: decomposer3(tvalue._3) :: decomposer4(tvalue._4) :: decomposer5(tvalue._5) :: Nil)
  }
  
  implicit def ArrayDecomposer[T](implicit elementDecomposer: Decomposer[T]): Decomposer[Array[T]] = new Decomposer[Array[T]] {
    def decompose(tvalue: Array[T]): JValue = JArray(tvalue.toList.map(elementDecomposer.decompose _))
  }
  
  implicit def SetDecomposer[T](implicit elementDecomposer: Decomposer[T]): Decomposer[Set[T]] = new Decomposer[Set[T]] {
    def decompose(tvalue: Set[T]): JValue = JArray(tvalue.toList.map(elementDecomposer.decompose _))
  }
  
  implicit def ListDecomposer[T](implicit elementDecomposer: Decomposer[T]): Decomposer[List[T]] = new Decomposer[List[T]] {
    def decompose(tvalue: List[T]): JValue = JArray(tvalue.toList.map(elementDecomposer.decompose _))
  }
  
  implicit def MapDecomposer[K, V](implicit keyDecomposer: Decomposer[K], valueDecomposer: Decomposer[V]): Decomposer[Map[K, V]] = new Decomposer[Map[K, V]] {
    def decompose(tvalue: Map[K, V]): JValue = ListDecomposer(Tuple2Decomposer(keyDecomposer, valueDecomposer)).decompose(tvalue.toList)
  }
  
  implicit def StringMapDecomposer[V](implicit valueDecomposer: Decomposer[V]): Decomposer[Map[String, V]] = new Decomposer[Map[String, V]] {
    def decompose(tvalue: Map[String, V]): JValue = JObject(tvalue.keys.toList.map { key =>
      JField(key, valueDecomposer(tvalue.apply(key)))
    })
  }
}
object DefaultDecomposers extends DefaultDecomposers

}