

package com.lagdaemon.scala.blockchain.test

import com.lagdaemon.scala.blockchain.BlockChain
import com.lagdaemon.scala.blockchain.Data
import java.util.Date

class MyData(val value:String) extends Data {
  val date: Date = new Date
  def getDate: Date = date
  def getValue: String = value
  override def toString = value + date.toString()
}

object BlockChainMain {
  
  
  def main(args: Array[String]): Unit = {
    
    val blockChain = new BlockChain[MyData](sha256Hash)
    
    for (i <- 1 to 100) {
      blockChain.mineBlock(new MyData("Some Data"))
    }
    
    for (block <- blockChain.getChain) {
      var id = block.getId
      var myData: Option[MyData] = {
        if (block.getData.isInstanceOf[MyData])
          Some( block.getData.asInstanceOf[MyData] )
        else 
          None
      }
      myData match {
        case Some(x) => {
          var date = x.getDate
          var value = x.getValue
          var hash = block.getHash
          println(s"---------- $id ------------")
          println(s"Date : $date")
          println(s"Value: $value")
          println(s"Hash : $hash")
        }
        case None => {
          var info = block.getData.toString
          var hash = block.getHash
          println(s"---------- $id ------------")
          println(s"Data : $info")
          println(s"Hash : $hash")
          
        }
      }
      if (block.getPrevHash == "")
        println("Prev Hash: --- no prev hash ---")
      else
        println("Prev Hash: " + block.getPrevHash )
      println("Nonce: " + block.getNonce )
    }
  
  }


  def sha256Hash(text: String) : String = String.format("%064x", new java.math.BigInteger(1, java.security.MessageDigest.getInstance("SHA-256").digest(text.getBytes("UTF-8"))))

}