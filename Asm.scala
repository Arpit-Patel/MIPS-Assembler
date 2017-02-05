import Scanning._
import scala.collection.mutable.ArrayBuffer

object Asm {

  type input = ArrayBuffer[Token]
  type lA = ArrayBuffer[(String, Int)]

  val tokenLines: Seq[Seq[Token]] = io.Source.stdin.getLines.map(scan).toSeq

  def toBin(i: Long): String = {
    var s = i.toBinaryString
    val r = 16 - s.length()
    if (r > 0){
      s = "0" + r + s
    }
    if (s.length() > 16){
      s = s.drop(s.length() - 16)
    }
    return s
  }

  def parseBin(s: String): Int = {
    var i = 0
    var newS = ""
    var hex = ""
    var newI = 0
    while (i < s.length){
      newS = s.substring(i, i + 4)
      newI = Integer.parseInt(newS, 2)
      hex = hex + Integer.toString(newI, 16)
      i = i + 4
    }
try { 
    newI = Integer.parseInt(hex, 16)
   } catch {
     case ioe: IOException => ... // more specific cases first !
     case e: Exception => ...
   }
    newI = Integer.parseInt(hex, 16)
    return newI
  }

  def getCode(arr: input, lArr: lA, curB: Int): Int = {
    var x = 0
    var inst = arr(0).lexeme
    var reg = arr(1).toLong.toInt

    if (inst == "jr"){
      var s = reg.toBinaryString
      var i = s.length
      while (i < 5){
        s = "0" + s
        i = i + 1
      }
      s = "0000" + "00" + s + "0" + "00000000000000001000"
      x = parseBin(s)
    }

    else if(inst == "jalr"){
      var s = reg.toBinaryString
      var i = s.length
      while (i < 5){
        s = "0" + s
        i = i + 1
      }
      s = "0000" + "00" + s + "0" + "00000000000000001001"
      x = parseBin(s)
    }

    else if(inst == "add"){
      var d = reg.toBinaryString
      reg = arr(3).toLong.toInt
      var s = reg.toBinaryString
      reg = arr(5).toLong.toInt
      var t = reg.toBinaryString
      var i = s.length
      while (i < 5){
        s = "0" + s
        i = i + 1
      }
      i = d.length
      while (i < 5){
        d = "0" + d
        i = i + 1
      }
      i = t.length
      while (i < 5){
        t = "0" + t
        i = i + 1
      }
      s = "0000" + "00" + s + t + d + "00000100000"
      x = parseBin(s)
    }

    else if(inst == "sub"){
      var d = reg.toBinaryString
      reg = arr(3).toLong.toInt
      var s = reg.toBinaryString
      reg = arr(5).toLong.toInt
      var t = reg.toBinaryString
      var i = s.length
      while (i < 5){
        s = "0" + s
        i = i + 1
      }
      i = d.length
      while (i < 5){
        d = "0" + d
        i = i + 1
      }
      i = t.length
      while (i < 5){
        t = "0" + t
        i = i + 1
      }
      s = "0000" + "00" + s + t + d + "00000100010"
      x = parseBin(s)
    }

    else if(inst == "slt"){
      var d = reg.toBinaryString
      reg = arr(3).toLong.toInt
      var s = reg.toBinaryString
      reg = arr(5).toLong.toInt
      var t = reg.toBinaryString
      var i = s.length
      while (i < 5){
        s = "0" + s
        i = i + 1
      }
      i = d.length
      while (i < 5){
        d = "0" + d
        i = i + 1
      }
      i = t.length
      while (i < 5){
        t = "0" + t
        i = i + 1
      }
      s = "0000" + "00" + s + t + d + "00000101010"
      x = parseBin(s)
    }

    else if(inst == "sltu"){
      var d = reg.toBinaryString
      reg = arr(3).toLong.toInt
      var s = reg.toBinaryString
      reg = arr(5).toLong.toInt
      var t = reg.toBinaryString
      var i = s.length
      while (i < 5){
        s = "0" + s
        i = i + 1
      }
      i = d.length
      while (i < 5){
        d = "0" + d
        i = i + 1
      }
      i = t.length
      while (i < 5){
        t = "0" + t
        i = i + 1
      }
      s = "0000" + "00" + s + t + d + "00000101011"
      x = parseBin(s)
    }

    else if(inst == "beq"){
      var i = 0
      var s = reg.toBinaryString
      var t = arr(3).toLong.toInt.toBinaryString
      var i16 = ""
      var targ = arr(5)

      if (targ.kind == "ID" && lArr.toMap.exists(_._1 == targ.lexeme + ":")){
        var tmp = (lArr.toMap.getOrElse(targ.lexeme + ":", 0) - curB - 4)/4

        if (tmp < 0){
          i16 = toBin(tmp.toLong)
        }

        else{
          i16 = tmp.toBinaryString
          i = i16.length
          while (i < 16){
            i16 = "0" + i16
            i = i + 1
          }
        }

      }

      else{
        var req = arr(5).toLong
        reg = req.toInt

        if (reg < 0){
          i16 = toBin(req)
        }

        else{
          i16 = reg.toBinaryString
          i = i16.length
          while (i < 16){
            i16 = "0" + i16
            i = i + 1
          }
        }

      }

      i = s.length
      while (i < 5){
        s = "0" + s
        i = i + 1
      }
      i = t.length
      while (i < 5){
        t = "0" + t
        i = i + 1
      }
      s = "0001" + "00" + s + t + i16
      x = parseBin(s)
    }

    else if(inst == "bne"){
      var i = 0
      var s = reg.toBinaryString
      var t = arr(3).toLong.toInt.toBinaryString
      var i16 = ""
      var targ = arr(5)

      if (targ.kind == "ID" && lArr.toMap.exists(_._1 == targ.lexeme + ":")){
        var tmp = (lArr.toMap.getOrElse(targ.lexeme + ":", 0) - curB - 4)/4

        if (tmp < 0){
          i16 = toBin(tmp.toLong)
        }

        else{
          i16 = tmp.toBinaryString
          i = i16.length
          while (i < 16){
            i16 = "0" + i16
            i = i + 1
          }
        }
        
      }

      else{
        var req = arr(5).toLong
        reg = req.toInt

        if (reg < 0){
          i16 = toBin(req)
        }

        else{
          i16 = reg.toBinaryString
          i = i16.length
          while (i < 16){
            i16 = "0" + i16
            i = i + 1
          }
        }

      }

      i = s.length
      while (i < 5){
        s = "0" + s
        i = i + 1
      }
      i = t.length
      while (i < 5){
        t = "0" + t
        i = i + 1
      }
      s = "0001" + "01" + s + t + i16
      x = parseBin(s)
    }

    else if(inst == "lis"){
      var s = reg.toBinaryString
      var i = s.length
      while (i < 5){
        s = "0" + s
        i = i + 1
      }
      s = "0000000000000000" + s + "00000010100"
      x = parseBin(s)
    }

    else if(inst == "mflo"){
      var s = reg.toBinaryString
      var i = s.length
      while (i < 5){
        s = "0" + s
        i = i + 1
      }
      s = "0000000000000000" + s + "00000010010"
      x = parseBin(s)
    }

    else if(inst == "mfhi"){
      var s = reg.toBinaryString
      var i = s.length
      while (i < 5){
        s = "0" + s
        i = i + 1
      }
      s = "0000000000000000" + s + "00000010000"
      x = parseBin(s)
    }

    else if(inst == "mult"){
      var d = reg.toBinaryString
      reg = arr(3).toLong.toInt
      var s = reg.toBinaryString
      var i = s.length
      while (i < 5){
        s = "0" + s
        i = i + 1
      }
      i = d.length
      while (i < 5){
        d = "0" + d
        i = i + 1
      }
      s = "000000" + d + s + "0000000000011000"
      x = parseBin(s)
    }

    else if(inst == "multu"){
      var d = reg.toBinaryString
      reg = arr(3).toLong.toInt
      var s = reg.toBinaryString
      var i = s.length
      while (i < 5){
        s = "0" + s
        i = i + 1
      }
      i = d.length
      while (i < 5){
        d = "0" + d
        i = i + 1
      }
      s = "000000" + d + s + "0000000000011001"
      x = parseBin(s)
    }

    else if(inst == "div"){
      var d = reg.toBinaryString
      reg = arr(3).toLong.toInt
      var s = reg.toBinaryString
      var i = s.length
      while (i < 5){
        s = "0" + s
        i = i + 1
      }
      i = d.length
      while (i < 5){
        d = "0" + d
        i = i + 1
      }
      s = "000000" + d + s + "0000000000011010"
      x = parseBin(s)
    }
    else if(inst == "divu"){
      var d = reg.toBinaryString
      reg = arr(3).toLong.toInt
      var s = reg.toBinaryString
      var i = s.length
      while (i < 5){
        s = "0" + s
        i = i + 1
      }
      i = d.length
      while (i < 5){
        d = "0" + d
        i = i + 1
      }
      s = "000000" + d + s + "0000000000011011"
      x = parseBin(s)
    }

    else if(inst == "lw"){
      var t = reg.toBinaryString
      reg = arr(5).toLong.toInt
      var s = reg.toBinaryString
      reg = arr(3).toLong.toInt
      var i16 = ""
      var i = 0

      if (reg < 0){
          i16 = toBin(reg.toLong)
      }

      else{
        i16 = reg.toBinaryString
        i = i16.length
        while (i < 16){
          i16 = "0" + i16
          i = i + 1
        }
      }

      i = s.length
      while (i < 5){
        s = "0" + s
        i = i + 1
      }
      i = t.length
      while (i < 5){
        t = "0" + t
        i = i + 1
      }
      s = "100011" + s + t + i16
      x = parseBin(s)
    }

    else if(inst == "sw"){
      var t = reg.toBinaryString
      reg = arr(5).toLong.toInt
      var s = reg.toBinaryString
      reg = arr(3).toLong.toInt
      var i16 = ""
      var i = 0

      if (reg < 0){
          i16 = toBin(reg.toLong)
      }

      else{
        i16 = reg.toBinaryString
        i = i16.length
        while (i < 16){
          i16 = "0" + i16
          i = i + 1
        }
      }
      i = s.length
      while (i < 5){
        s = "0" + s
        i = i + 1
      }
      i = t.length
      while (i < 5){
        t = "0" + t
        i = i + 1
      }
      s = "101011" + s + t + i16
      x = parseBin(s)
    }

    return x

  }

  def assemble(): Unit = {
    var label = ""
    var x = 0
    var i = 0
    var arr = ArrayBuffer[(String, Int)]()
    var dontDo = false
    var wordArr = ArrayBuffer[ArrayBuffer[Token]]()
    var bL = ArrayBuffer[Int]()

    for (tokenLine <- tokenLines){

     if (tokenLine.length == 1 && tokenLine(0).kind == "LABEL"){

      if (arr.toMap.exists(_._1 == tokenLine(0).lexeme)){
       System.err.println("ERROR")
      }

      else{
        label = tokenLine(0).lexeme
        arr += label -> i
        System.err.println(label.replace(":", "" + " " + i.toString))
      }

    }

    else if (tokenLine.length == 0){}

    else{
      var tmpArr = ArrayBuffer[(String, Int)]()
      var at = 0
      var countT = 0

      while(at < tokenLine.length - 2){
        if (tokenLine(at).kind == "LABEL" && !(arr.toMap.exists(_._1 == tokenLine(at).lexeme)) && !(tmpArr.toMap.exists(_._1 == tokenLine(at).lexeme))){
          countT = countT + 1
          tmpArr += tokenLine(at).lexeme -> i
        }
        at = at + 1
      }

      if ((tokenLine(at).lexeme == ".word" && ((tokenLine(at + 1).kind == "ID") || (tokenLine(at + 1).kind == "HEXINT" || tokenLine(at + 1).kind == "REG" || tokenLine(at + 1).kind == "INT"))) || (tokenLine(at).kind == "LABEL" && tokenLine(at + 1).kind == "LABEL")){
        countT = countT + 2
      }

      if (countT == tokenLine.length || (tokenLine(0).lexeme == "jr" || tokenLine(0).lexeme == "jalr" || tokenLine(0).lexeme == "add" || tokenLine(0).lexeme == "sub" || tokenLine(0).lexeme == "slt" || tokenLine(0).lexeme == "sltu" || tokenLine(0).lexeme == "beq" || tokenLine(0).lexeme == "bne" || tokenLine(0).lexeme == "mflo" || tokenLine(0).lexeme == "lis" || tokenLine(0).lexeme == "mfhi" || tokenLine(0).lexeme == "mult" || tokenLine(0).lexeme == "multu" || tokenLine(0).lexeme == "div" || tokenLine(0).lexeme == "divu" || tokenLine(0).lexeme == "lw" || tokenLine(0).lexeme == "sw")){
        at = 0
        var row = ArrayBuffer[Token]()

        if (tokenLine(at).lexeme == "jr" || tokenLine(at).lexeme == "jalr" || tokenLine(at).lexeme == "add" || tokenLine(at).lexeme == "sub" || tokenLine(at).lexeme == "slt" || tokenLine(at).lexeme == "sltu" || tokenLine(at).lexeme == "beq" || tokenLine(at).lexeme == "bne" || tokenLine(at).lexeme == "mflo" || tokenLine(at).lexeme == "mfhi" || tokenLine(at).lexeme == "lis" || tokenLine(at).lexeme == "divu" || tokenLine(at).lexeme == "div" || tokenLine(at).lexeme == "multu" || tokenLine(at).lexeme == "mult" || tokenLine(at).lexeme == "lw" || tokenLine(at).lexeme == "sw"){
          
          if (tokenLine(at).lexeme == "bne" || tokenLine(at).lexeme == "beq"){
            bL += i
          }

          while(at < tokenLine.length){
            row += tokenLine(at)
            at = at + 1
          }
          wordArr += row
          i = i + 4
        }

        else{
          while(at < tokenLine.length - 2){
            label = tokenLine(at).lexeme
            System.err.println(label.replace(":", "" + " " + i.toString))
            arr += label -> i
            at = at + 1
          }

          if (tokenLine(at).lexeme == ".word"){
            row += tokenLine(at)
            row += tokenLine(at + 1)
            wordArr += row
            i = i + 4
          }

          else{
            while(at < tokenLine.length){
              label = tokenLine(at).lexeme
              System.err.println(label.replace(":", "" + " " + i.toString))
              arr += label -> i
              at = at + 1
            }
          }
        }
      }

      else{
        System.err.println("ERROR 1")
      }

  }
 }
 i = 0
 var curB = 0
 while (i < wordArr.length){

  if ((wordArr(i)(1)).kind == "ID" && arr.toMap.exists(_._1 == wordArr(i)(1).lexeme + ":")){
    x = arr.toMap.getOrElse(wordArr(i)(1).lexeme + ":", 0)
  }

  else if(wordArr(i)(1).kind == "ID" && !(arr.toMap.exists(_._1 == (wordArr(i)(1).lexeme + ":")))){
    dontDo = true
  }

  else if(wordArr(i)(1).kind == "REG"){
    if (((wordArr(i)(0).lexeme == "bne") || (wordArr(i)(0).lexeme == "beq")) && (wordArr(i)(3).kind == "ID" && !(arr.toMap.exists(_._1 == wordArr(i)(3).lexeme + ":")))){
      dontDo = true
    }
    else{
      if (bL.length == 0){
        x = getCode(wordArr(i), arr, 0)
      }
      else{
        x = getCode(wordArr(i), arr, bL(curB))
      }
      if ((wordArr(i)(0).lexeme == "bne") || (wordArr(i)(0).lexeme == "beq")){
        curB = curB + 1
      }
    }
  }

  else if(wordArr(i)(1).kind == "HEXINT"){
    x = wordArr(i)(1).lexeme.toLong.toInt
  }

  else{
    x = wordArr(i)(1).lexeme.toLong.toInt
  }

  if (!dontDo){
     System.out.write(x >> 24)
     System.out.write(x >> 16)
     System.out.write(x >> 8)
     System.out.write(x)
  }

  else{
    System.err.println("ERROR 2")
    dontDo = false
  }

  i = i + 1
 }

 System.out.close()
 }

  def main(args: Array[String]): Unit = assemble()
}
