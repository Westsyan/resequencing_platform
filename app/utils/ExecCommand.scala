package utils

import java.io.File

import scala.sys.process._

class ExecCommand {
  var isSuccess: Boolean = false
  val err = new StringBuilder
  val out = new StringBuilder
  val log = ProcessLogger(out append _ append "\n", err append _ append "\n")

  def exec(command: String) = {
    val exitCode = command ! log
    if (exitCode == 0) isSuccess = true
  }

  def exec(command1: String, command2: String) = {
    val exitCode = (command1 #&& command2) ! log
    if (exitCode == 0) isSuccess = true
  }

  def exec(command1: String, command2: String, command3:String) = {
    val exitCode = (command1 #&& command2 #&& command3) ! log
    if (exitCode == 0) isSuccess = true
  }

  def exec(command1: String, command2: String, command3:String, command4: String) = {
    val exitCode = (command1 #&& command2 #&& command3 #&& command4) ! log
    if (exitCode == 0) isSuccess = true
  }

  def exec(command1: String, command2: String, command3:String, command4: String, command5: String) = {
    val exitCode = (command1 #&& command2 #&& command3 #&& command4 #&& command5) ! log
    if (exitCode == 0) isSuccess = true
  }

  def exec(command1: String, command2: String, command3:String, command4: String, command5: String, command6: String) = {
    val exitCode = (command1 #&& command2 #&& command3 #&& command4 #&& command5 #&& command6) ! log
    if (exitCode == 0) isSuccess = true
  }

  def exec(command1: String, command2: String, command3:String, command4: String, command5: String, command6: String, command7:String) = {
    val exitCode = (command1 #&& command2 #&& command3 #&& command4 #&& command5 #&& command6 #&& command7) ! log
    if (exitCode == 0) isSuccess = true
  }

  def execs(command1: String, command2: String, command3:String) = {
    val exitCode = (command1 #&& command2 #&& command3) ! log
    if (exitCode == 0) isSuccess = true
  }

  def execo(command: String, outFile: File) = {
    val exitCode = (command #> outFile) ! log
    if (exitCode == 0) isSuccess = true
  }


  def exect(command:String, tmpDir:String) = {
    val exitCode = Process(command , new File(tmpDir)) ! log
    println(getInfo(command, exitCode))
    if (exitCode == 0) isSuccess = true
  }

  def exect(command1:String,command2:String,tmpDir:String) = {
    val exitCode1 = Process(command1 , new File(tmpDir)) ! log
    println(getInfo(command1, exitCode1))
    val exitCode2 = Process(command2 , new File(tmpDir)) ! log
    println(getInfo(command2, exitCode2))
    if(exitCode1 == 0 && exitCode2 == 0) isSuccess = true
  }

  def exect(command1:String,command2:String ,command3:String,tmpDir:String) = {
    val exitCode1 = Process(command1 , new File(tmpDir)) ! log
    println(getInfo(command1, exitCode1))
    val exitCode2 = Process(command2 , new File(tmpDir)) ! log
    println(getInfo(command2, exitCode2))
    val exitCode3 = Process(command3 , new File(tmpDir)) ! log
    println(getInfo(command3, exitCode3))
    if(exitCode1 == 0 && exitCode2 == 0 && exitCode3 == 0) isSuccess = true
  }

  def exect(command1:String,command2:String ,command3:String,command4:String,tmpDir:String) = {
    val exitCode1 = Process(command1 , new File(tmpDir)) ! log
    println(getInfo(command1, exitCode1))
    val exitCode2 = Process(command2 , new File(tmpDir)) ! log
    println(getInfo(command2, exitCode2))
    val exitCode3 = Process(command3 , new File(tmpDir)) ! log
    println(getInfo(command3, exitCode3))
    val exitCode4 = Process(command4 , new File(tmpDir)) ! log
    println(getInfo(command4, exitCode4))
    if(exitCode1 == 0 && exitCode2 == 0 && exitCode3 == 0 && exitCode4 == 0) isSuccess = true
  }

  def exect(command1:String,command2:String ,command3:String,command4:String,command5: String,tmpDir:String) = {
    val exitCode1 = Process(command1 , new File(tmpDir)) ! log
    println(getInfo(command1, exitCode1))
    val exitCode2 = Process(command2 , new File(tmpDir)) ! log
    println(getInfo(command2, exitCode2))
    val exitCode3 = Process(command3 , new File(tmpDir)) ! log
    println(getInfo(command3, exitCode3))
    val exitCode4 = Process(command4 , new File(tmpDir)) ! log
    println(getInfo(command4, exitCode4))
    val exitCode5 = Process(command5 , new File(tmpDir)) ! log
    println(getInfo(command5, exitCode5))
    if(exitCode1 == 0 && exitCode2 == 0 && exitCode3 == 0 && exitCode4 == 0 && exitCode5 ==0) isSuccess = true
  }

  def exect(command1:String,command2:String ,command3:String,command4:String,command5: String,command6: String,tmpDir:String) = {
    val exitCode1 = Process(command1 , new File(tmpDir)) ! log
    println(getInfo(command1, exitCode1))
    val exitCode2 = Process(command2 , new File(tmpDir)) ! log
    println(getInfo(command2, exitCode2))
    val exitCode3 = Process(command3 , new File(tmpDir)) ! log
    println(getInfo(command3, exitCode3))
    val exitCode4 = Process(command4 , new File(tmpDir)) ! log
    println(getInfo(command4, exitCode4))
    val exitCode5 = Process(command5 , new File(tmpDir)) ! log
    println(getInfo(command5, exitCode5))
    val exitCode6 = Process(command6 , new File(tmpDir)) ! log
    println(getInfo(command6, exitCode6))
    if(exitCode1 == 0 && exitCode2 == 0 && exitCode3 == 0 && exitCode4 == 0 && exitCode5 ==0 && exitCode6 == 0) isSuccess = true
  }

  def exect(command1:String,command2:String ,command3:String,command4:String,command5: String,command6: String, command7:String,tmpDir:String) = {
    val exitCode1 = Process(command1 , new File(tmpDir)) ! log
    println(getInfo(command1, exitCode1))
    val exitCode2 = Process(command2 , new File(tmpDir)) ! log
    println(getInfo(command2, exitCode2))
    val exitCode3 = Process(command3 , new File(tmpDir)) ! log
    println(getInfo(command3, exitCode3))
    val exitCode4 = Process(command4 , new File(tmpDir)) ! log
    println(getInfo(command4, exitCode4))
    val exitCode5 = Process(command5 , new File(tmpDir)) ! log
    println(getInfo(command5, exitCode5))
    val exitCode6 = Process(command6 , new File(tmpDir)) ! log
    println(getInfo(command6, exitCode6))
    val exitCode7 = Process(command7 , new File(tmpDir)) ! log
    println(getInfo(command7, exitCode7))
    if(exitCode1 == 0 && exitCode2 == 0 && exitCode3 == 0 && exitCode4 == 0 && exitCode5 ==0 && exitCode6 == 0 && exitCode7 == 0) isSuccess = true
  }

  def getInfo(command:String,exitCode:Int) : String = {
    val commands = command.split(" ").take(3)
    val proname = if(commands.head == "python" || commands.head == "perl" || commands.head == "sh"){
      commands.drop(1).head.split("/").last
    }else if(commands.head == "java"){
      commands.drop(2).head.split("/").last
    }else{
      commands.head.split("/").last
    }
    val info = if(exitCode ==0){
      s"[info] Programe ${proname} run success!"
    }else{
      s"[error] Programe ${proname} run falied!"
    }

    info
  }

  def getErrStr = {
    err.toString()
  }

  def getOutStr = {
    out.toString()
  }


}
