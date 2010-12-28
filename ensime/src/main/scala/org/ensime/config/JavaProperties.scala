package org.ensime.config
import java.io.File
import java.io.FileInputStream
import scala.collection.JavaConversions._
import java.util.Properties


object JavaProperties {

  def load(file:File):Map[Any,Any] = {
    val props = new Properties()
    try{
      val fis = new FileInputStream(file)
      try{
	props.load(fis);    
	props.toMap
      }
      finally{
	fis.close();
      }
    }
    catch{
      case e => Map()
    }
  }

}
