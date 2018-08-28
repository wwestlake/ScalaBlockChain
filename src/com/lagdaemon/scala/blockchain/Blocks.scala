

package com.lagdaemon.scala.blockchain

import com.lagdaemon.scala.blockchain._

trait Data {
  def toString(): String
}

class Payload[Tdata <: Data](data:Tdata, prevHash:String, hashFunction: (String) => String, difficulty:Int = 0) {
      var id:Int = 0
      var nonce = 0;
      val checkString = List.fill(difficulty)("0").mkString
      private var hash = mine
      def getHash = hash
      def getPrevHash = prevHash
      def getNonce = nonce
      def getData = data
      private def mine: String = {
        var result = hashFunction(data.toString + prevHash + nonce)
        while (! result.startsWith(checkString)) {
            nonce = nonce + 1
            result = hashFunction(data.toString + prevHash + nonce)
        }
        result
      }
      def getId = id
      def setId(num:Int) = id = num
}

class GenesisData(marker:String) extends Data {
  override def toString = marker
}

class BlockChain[Tdata <: Data](hashFunction: (String) => String, difficulty:Int = 0) {
      var nextBlockId = 0
      val genesisData = new GenesisData("Genesis Block")
      val genesisBlock = new Payload[Data](genesisData,"", hashFunction, difficulty)
      genesisBlock.setId(nextBlockId)
      nextBlockId = nextBlockId + 1
      var blockChain = List[Payload[Data]](genesisBlock)
      
      def mineBlock(data:Tdata) {
        val lastBlock = blockChain.head
        val newBlock = new Payload[Data](data, lastBlock.getHash, hashFunction, difficulty)
        newBlock.setId(nextBlockId)
        nextBlockId = nextBlockId + 1
        blockChain = newBlock :: blockChain
      }
      
      def getChain = blockChain
}