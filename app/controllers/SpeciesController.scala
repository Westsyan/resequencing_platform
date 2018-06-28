package controllers

import java.io.File
import javax.inject.Inject

import akka.stream.IOResult
import akka.stream.scaladsl.{FileIO, Sink}
import akka.util.ByteString
import dao._
import models.Tables.SpeciesRow
import org.apache.commons.io.FileUtils
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.libs.streams.Accumulator
import play.api.mvc.MultipartFormData.FilePart
import play.api.mvc._
import play.core.parsers.Multipart.{FileInfo, FilePartHandler}
import utils.Utils.windowsPath
import utils.{ExecCommand, Utils}

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class SpeciesController @Inject()(admindao: adminDao, projectdao: projectDao, sampledao: sampleDao,
                                  speciesdao: speciesDao) extends Controller {


  def toSpecies = Action { implicit request =>
    val user = getProname(request.session)
    Ok(views.html.species.addSpecies(user))
  }

  def getProname(session: Session): Seq[String] = {
    val userId = session.get("id").head.toInt
    val proname = Await.result(projectdao.getAllProject(userId), Duration.Inf)
    proname
  }

  case class speciesData(speciesname: String)

  val speciesForm = Form(
    mapping(
      "speciesname" -> text
    )(speciesData.apply)(speciesData.unapply)
  )

  private def handleFilePartAsFile: FilePartHandler[File] = {
    case FileInfo(partName, filename, contentType) =>

      val file = new File(Utils.tmpPath, Utils.random)
      val path = file.toPath
      val fileSink: Sink[ByteString, Future[IOResult]] = FileIO.toPath(path)
      val accumulator: Accumulator[ByteString, IOResult] = Accumulator(fileSink)
      accumulator.map {
        case IOResult(count, status) =>
          FilePart(partName, filename, contentType, file)
      }
  }

  def uploadSpecies = Action(parse.multipartFormData(handleFilePartAsFile)) { implicit request =>
    val userId = request.session.get("id").head.toInt
    val file = request.body.files
    val speciesname = speciesForm.bindFromRequest.get.speciesname
    val date = Utils.date
    val row = SpeciesRow(0, speciesname, userId, date, 0)
    Await.result(speciesdao.addSpecies(row), Duration.Inf)
    val s = Await.result(speciesdao.getByPosition(speciesname, userId), Duration.Inf)
    try {
      val run = Future {
        val id = s.head.id
        new File(Utils.speciesPath, id.toString).mkdir()
        val outPath = Utils.speciesPath + "/" + id
        val in1 = file.head.ref.getPath
        val in2 = file(1).ref.getPath
        val out1 = outPath + "/sequences.fa"
        val out2 = outPath + "/genes.gtf"
        FileUtils.moveFile(new File(in1), new File(out1))
        FileUtils.moveFile(new File(in2), new File(out2))
        runCmd(id)
      }
    } catch {
      case e: Exception => Await.result(speciesdao.updateState(s.head.id, 2), Duration.Inf)
    }

    Ok(Json.obj("valid" -> "true"))
  }


  def speciesInfo = Action { implicit request =>
    val user = getProname(request.session)
    Ok(views.html.species.speciesInfo(user))
  }

  def isRunCmd(speciesId: Int) = Action.async { implicit request =>
    speciesdao.getById(speciesId).map { x =>
      if (x.state == 0) {
        runCmd(x.id)
      }
      Ok(Json.obj("valid" -> "true"))
    }
  }

    def toAdminSpecies = Action { implicit request =>
    Ok(views.html.adminSpecies.addSpecies())
  }

    def adminSpeciesInfo = Action { implicit request =>
    Ok(views.html.adminSpecies.speciesInfo())
  }

  def adminHome =  Action.async { implicit request =>
    val id = request.session.get("id").head.toInt
    speciesdao.getByUserId(id).map { y =>
      if (y.size < 2) {
        Redirect(routes.SpeciesController.toAdminSpecies())
      } else {
        Redirect(routes.SpeciesController.adminSpeciesInfo())
      }
    }
  }

  def home = Action.async { implicit request =>
    val id = request.session.get("id").head.toInt
    speciesdao.getByUserId(id).map { y =>
      if (y.size < 2) {
        Redirect(routes.SpeciesController.toSpecies())
      } else {
        Redirect(routes.SpeciesController.speciesInfo())
      }
    }
  }

  def runCmd(speciesId: Int) = {
    val path = Utils.speciesPath + "/" + speciesId
    val outpath = path + "/sequences.fa"
    val dictpath = path + "/sequences.dict"
    val (command1, command2) = if (new File(windowsPath).exists()) {
      (s"${Utils.toolPath}/bwa-0.7.12/bwa index ${outpath}",
        s"${Utils.toolPath}/samtools-0.1.19/samtools faidx ${outpath}")
    } else {
      (s"bwa index ${outpath}", s"samtools faidx ${outpath}")
    }
    FileUtils.write(new File(Utils.toolPath + "/snpEff/snpEff.config"), "\n" + speciesId + ".genome : Yeast", true)
    val command3 = s"""java -jar ${Utils.toolPath}/CreateSequenceDictionary.jar R=${outpath} O=${dictpath} QUIET=true"""
    val command4 = s"java -Xmx10G -jar ${Utils.toolPath}/snpEff/snpEff.jar build -gtf22 ${speciesId}"

    val command = new ExecCommand
    command.exec(command1, command2, command3, command4)
    if (command.isSuccess) {
      Await.result(speciesdao.updateState(speciesId, 1), Duration.Inf)
      FileUtils.writeStringToFile(new File(path, "log.txt"), command.getErrStr)
    } else {
      Await.result(speciesdao.updateState(speciesId, 2), Duration.Inf)
      val log = command.getErrStr
      if (new File(path).exists()) {
        FileUtils.writeStringToFile(new File(path, "log.txt"), log)
      }
    }
  }

  def getAllSpecies = Action { implicit request =>
    val json = dealWithSpecies(request.session)
    Ok(Json.toJson(json))
  }

  def dealWithSpecies(session: Session) = {
    val userId = session.get("id").head.toInt
    val userS = Await.result(speciesdao.getByUserId(userId), Duration.Inf)
    val json = userS.map { x =>
      val speciesname = x.speciesname
      val date = x.createdata.toLocalDate
      val state = if (x.state == 0) {
        "正在运行 <img src='/assets/images/timg.gif'  style='width: 20px; height: 20px;'><input class='state' value='" + x.state + "'>"
      } else if (x.state == 1) {
        "成功<input class='state' value='" + x.state + "'>"
      } else {
        "失败<input class='state' value='" + x.state + "'>"
      }
      val operation = if (x.state == 1) {
        s"""
           |  <button class="update" onclick="updateSpecies(this)" value="${x.speciesname}" id="${x.id}" title="修改物种名"><i class="fa fa-pencil"></i></button>
           |  <button class="update" onclick="openLog(this)" value="${x.id}" title="查看日志"><i class="fa fa-file-text"></i></button>
           |  <button class="delete" onclick="openDelete(this)" value="${x.speciesname}" id="${x.id}" title="删除物种"><i class="fa fa-trash"></i></button>
           """.stripMargin
      } else if (x.state == 2) {
        s"""<button class="delete" onclick="openDelete(this)" value="${x.speciesname}" id="${x.id}" title="删除任务"><i class="fa fa-trash"></i></button>
           |<button class="update" onclick="openLog(this)" value="${x.id}" title="查看日志"><i class="fa fa-file-text"></i></button>
         """.stripMargin
      } else {
        s"""<button class="delete" onclick="openDelete(this)" value="${x.speciesname}" id="${x.id}" title="删除任务"><i class="fa fa-trash"></i></button>"""
      }
      Json.obj("speciesname" -> speciesname, "state" -> state, "date" -> date, "operation" -> operation)
    }
    json

  }

  def deleteSpecies(id: Int) = Action.async { implicit request =>
    speciesdao.deleteById(id).map { x =>
      val run = Future {
        FileUtils.deleteDirectory(new File(Utils.speciesPath, id.toString))
      }
      Ok(Json.toJson("success"))
    }
  }

  def getLog(id: Int) = Action { implicit request =>
    val log = FileUtils.readLines(new File(Utils.speciesPath, id + "/log.txt")).asScala
    var html =
      """
        |<style>
        |   .logClass{
        |       font-size : 16px;
        |       font-weight:normal;
        |   }
        |</style>
      """.stripMargin
    html += "<b class='logClass'>" + log.mkString("</b><br><b class='logClass'>") + "</b>"
    val json = Json.obj("log" -> html)
    Ok(Json.toJson(json))
  }

  case class updateData(speciesId: Int, newspecies: String)

  val updateForm = Form(
    mapping(
      "speciesId" -> number,
      "newspecies" -> text
    )(updateData.apply)(updateData.unapply)
  )

  def updateSpeciesname = Action.async { implicit request =>
    val data = updateForm.bindFromRequest.get
    val speciesId = data.speciesId
    val newspecies = data.newspecies
    speciesdao.updateSpeciesname(speciesId, newspecies).map { x =>
      Ok(Json.toJson("success"))
    }
  }

  def checkSpecies = Action.async { implicit request =>
    val id = request.session.get("id").head.toInt
    val speciesname = speciesForm.bindFromRequest.get.speciesname
    if (id == 1) {
      speciesdao.getByName(speciesname).map { y =>
        val valid = if (y.size == 0) {
          "true"
        } else {
          "false"
        }
        Ok(Json.obj("valid" -> valid))
      }
    } else {
      speciesdao.getByPosition(speciesname, 1).flatMap { y =>
        speciesdao.getByPosition(speciesname, id).map { z =>
          val valid = if (y.size == 0 && z.size == 0) {
            "true"
          } else {
            "false"
          }
          Ok(Json.obj("valid" -> valid))
        }
      }

    }
  }

  def getAllSpeciesname = Action.async { implicit request =>
    val id = request.session.get("id").head.toInt
    speciesdao.getByUserId(id).flatMap { y =>
      speciesdao.getByUserId(1).map { z =>
        val all = y ++ z
        val name = all.map(_.speciesname)
        Ok(Json.toJson(name))
      }
    }

  }

}
