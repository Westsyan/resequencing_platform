package controllers

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import javax.inject.Inject

import dao._
import models.Tables._
import org.apache.commons.io.FileUtils
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.mvc._
import utils.{ExecCommand, Utils}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class TaskController @Inject()(admindao: adminDao, projectdao: projectDao, sampledao: sampleDao, taskdao: taskDao) extends Controller {


  def toTaskPage(proname: String): Action[AnyContent] = Action { implicit request =>
    val userId = request.session.get("id").head.toInt
    val allName = Await.result(projectdao.getAllProject(userId), Duration.Inf)
    Ok(views.html.task.taskPage(allName, proname))
  }


  def taskPage(proname: String) = Action { implicit request =>
    val ses = getUserIdAndProId(request.session, proname)
    val allName = Await.result(projectdao.getAllProject(ses._1), Duration.Inf)
    Ok(views.html.task.taskData(allName, proname))
  }

  case class taskData(proname: String, taskname: String, sample: Seq[String], genotype_likelihoods_model: String,
                      standard_min_confidence_threshold_for_calling: Option[String], standard_min_confidence_threshold_for_emitting: Option[String],
                      sample_ploidy: Option[Int], minQ: Option[Int], MinDepth: Option[Int], MaxDepth: Option[Int], FS: Option[Int],
                      RPRS: Option[String], udLength: String, spliceSiteSize: String, noLog: String, setSpliceRegions: String,
                      spliceRegionExonSize: Option[Int], spliceRegionIntronMin: Option[Int], spliceRegionIntronMax: Option[Int])

  val taskForm = Form(
    mapping(
      "proname" -> text,
      "taskname" -> text,
      "sample" -> seq(text),
      "genotype_likelihoods_model" -> text,
      "standard_min_confidence_threshold_for_calling" -> optional(text),
      "standard_min_confidence_threshold_for_emitting" -> optional(text),
      "sample_ploidy" -> optional(number),
      "minQ" -> optional(number),
      "MinDepth" -> optional(number),
      "MaxDepth" -> optional(number),
      "FS" -> optional(number),
      "RPRS" -> optional(text),
      "udLength" -> text,
      "spliceSiteSize" -> text,
      "noLog" -> text,
      "setSpliceRegions" -> text,
      "spliceRegionExonSize" -> optional(number),
      "spliceRegionIntronMin" -> optional(number),
      "spliceRegionIntronMax" -> optional(number)
    )(taskData.apply)(taskData.unapply)
  )

  def saveDeploy = Action { implicit request =>
    val data = taskForm.bindFromRequest.get
    val proname = data.proname
    val taskname = data.taskname
    val ses = getUserIdAndProId(request.session, proname)
    val date = Utils.date
    val row = TaskRow(0, taskname, ses._1, ses._2, date, 0)
    Await.result(taskdao.addTaskInfo(Seq(row)), Duration.Inf)
    val run = Future {
      val task = Await.result(taskdao.getAllByPosition(ses._1, ses._2, taskname), Duration.Inf)
      val otupath = Utils.taskPath(ses._1, ses._2, task.id)
      new File(otupath).mkdirs()
      val deploy = mutable.Buffer(proname, taskname, data.sample.mkString(","), data.genotype_likelihoods_model,
        data.standard_min_confidence_threshold_for_calling.getOrElse(30.0),
        data.standard_min_confidence_threshold_for_emitting.getOrElse(30.0),
        data.sample_ploidy.getOrElse(2), data.minQ.getOrElse(30), data.MinDepth.getOrElse(5),
        data.MaxDepth.getOrElse(10000000), data.FS.getOrElse(0), data.RPRS.getOrElse(2.5),
        data.udLength, data.spliceSiteSize, data.noLog, data.setSpliceRegions, data.spliceRegionExonSize.getOrElse(3),
        data.spliceRegionIntronMin.getOrElse(3), data.spliceRegionIntronMax.getOrElse(8))
      FileUtils.writeLines(new File(otupath + "/deploy.txt"), deploy.asJava)
      runCmd(task.id)
    }
    val json = Json.obj("valid" -> "true")
    Ok(Json.toJson(json))
  }

  def getUserIdAndProId(session: Session, proname: String): (Int, Int) = {
    val userId = session.get("id").head.toInt
    val proId = Await.result(projectdao.getIdByProjectname(userId, proname), Duration.Inf)
    (userId, proId)
  }

  def getTime = Action { implicit request =>
    val now = new Date()
    val dateFormat = new SimpleDateFormat("yyMMddHHmmss")
    val date = dateFormat.format(now)
    Ok(Json.obj("date" -> date))
  }

  case class checkTasknameData(taskname: String)

  val checkTasknameForm = Form(
    mapping(
      "taskname" -> text
    )(checkTasknameData.apply)(checkTasknameData.unapply)
  )

  def checkName(proname: String) = Action.async { implicit request =>
    val data = checkTasknameForm.bindFromRequest.get
    val ses = getUserIdAndProId(request.session, proname)
    taskdao.getAllByPosi(ses._1, ses._2, data.taskname).map { x =>
      val valid = if (x.size == 0) {
        "true"
      } else {
        "false"
      }
      Ok(Json.obj("valid" -> valid))
    }
  }

  def runCmd(id: Int) = {
    val row = Await.result(taskdao.getById(id), Duration.Inf)
    val userId = row.accountid
    val proId = row.projectid
    val path = Utils.taskPath(userId, proId, id)
    val deploy = FileUtils.readLines(new File(path, "deploy.txt")).asScala
    val command = new ExecCommand
    val com = new ExecCommand

    val samples = deploy(2).split(",").map { x =>
      val sample = Await.result(sampledao.getByPosition(userId, proId, x), Duration.Inf)
      val bam = Utils.outPath(userId, proId, sample.id) + "/sorted.bam "
      "-I " + bam
    }
    val project = Await.result(projectdao.getById(proId), Duration.Inf)
    val species = project.speciesid
    val fasta = Utils.speciesPath + "/" + species + "/sequences.fa"
    val command1 = gatkCmd(samples, fasta, deploy, path)

    val tmp = path + "/tmp"
    new File(tmp).mkdir()

    com.exect(command1,tmp)

    val command2 = sampleToVcf(path)
    val command3 = vcfCmd(deploy, path)
    val command4 = snpCmd(deploy, path, species)
    command.exect(command2, command3, command4 , tmp)
    if (com.isSuccess && command.isSuccess) {
      new File(path, "snp.sh").delete()
      new File(path, "vcf.sh").delete()
      new File(path, "variant.vcf").delete()
      Await.result(taskdao.updateState(id, 1), Duration.Inf)
      val log = com.getErrStr ++ command.getErrStr
      FileUtils.writeStringToFile(new File(path, "log.txt"), log)
      FileUtils.deleteDirectory(new File(tmp))
    } else {
      Await.result(taskdao.updateState(id, 2), Duration.Inf)
      val log = com.getErrStr ++ command.getErrStr
      if (new File(path).exists()) {
        FileUtils.writeStringToFile(new File(path, "log.txt"), log)
      }
      FileUtils.deleteDirectory(new File(tmp))
    }
  }

  def gatkCmd(samples: Array[String], fasta: String, deploy: mutable.Buffer[String], outPath: String): String = {
    val command = s"java -jar ${Utils.toolPath}/GenomeAnalysisTK.jar -T UnifiedGenotyper --num_threads 1 -R ${fasta} " +
      s" ${samples.mkString(" ")} --genotype_likelihoods_model ${deploy(3)} " +
      s"--standard_min_confidence_threshold_for_calling ${deploy(4)} " +
      s"--standard_min_confidence_threshold_for_emitting ${deploy(5)} --sample_ploidy ${deploy(6)} " +
      s"--out  ${outPath}/variant.vcf"
    command
  }

  def sampleToVcf(outPath:String) : String = {
    val path = outPath + "/" + "variant.vcf"
    val buffer = FileUtils.readLines(new File(path)).asScala
    val deploy = FileUtils.readLines(new File(outPath,"deploy.txt")).asScala
    val sampleNumber = deploy(2).split(",").size
    val head = buffer.filter(_.head == '#')
    val line = head.last.split("\t")
    val size = head.size
    val samples = line.takeRight(sampleNumber).map{x=>
      val row = Await.result(sampledao.getAllById(x.toInt),Duration.Inf)
      row.sample
    }
    val last = line.dropRight(sampleNumber) ++ samples
    val ref = last.mkString("\t")
    val command = s"sed '${size}c ${ref}' ${path} > ${outPath}/variants.vcf"
    FileUtils.writeStringToFile(new File(outPath, "vcf.sh"), command)
    val command1 = s"sh ${outPath}/vcf.sh"
    command1
  }

  def vcfCmd(deploy: mutable.Buffer[String], outPath: String): String = {
    val command = s"perl  ${Utils.toolPath}/myVcfUtils.pl -i ${outPath}/variants.vcf -q ${deploy(7)} -mind ${deploy(8)}" +
      s" -maxD ${deploy(9)} -fs ${deploy(10)} -rprs ${deploy(11)} -o  ${outPath}/filtered.vcf"
    command
  }

  def snpCmd(deploy: mutable.Buffer[String], path: String, speciesId: Int): String = {
    var command = s"java -jar ${Utils.toolPath}/snpEff/snpEff.jar -i vcf -o vcf " +
      s"-upDownStreamLen ${deploy(12)} -spliceSiteSize ${deploy(13)} "
    if (deploy(15) == "yes") {
      command += s" -spliceRegionExonSize ${deploy(16)} -spliceRegionIntronMin ${deploy(17)} -spliceRegionIntronMax ${deploy(18)}"
    }
    command += s" ${deploy(14)} ${speciesId} ${path}/filtered.vcf > ${path}/snp.vcf "
    FileUtils.writeStringToFile(new File(path, "snp.sh"), command)
    val command1 = s"sh ${path}/snp.sh"
    command1
  }

  def isRunCmd(id: Int) = Action.async { implicit request =>
    taskdao.getById(id).map { x =>
      runCmd(x.id)
      Ok(Json.toJson("success"))
    }
  }

  def getAllTask(proname: String) = Action { implicit request =>
    val json = dealWithSample(proname, request.session)
    Ok(Json.toJson(json))
  }

  def dealWithSample(proname: String, session: Session) = {
    val id = getUserIdAndProId(session, proname)
    val tasks = Await.result(taskdao.getAllTaskByPosition(id._1, id._2), Duration.Inf)
    val json = tasks.sortBy(_.id).reverse.map { x =>
      val taskname = x.taskname
      val date = x.createdata.toLocalDate
      val state = if (x.state == 0) {
        "正在运行 <img src='/assets/images/timg.gif'  style='width: 20px; height: 20px;'><input class='state' value='" + x.state + "'>"
      } else if (x.state == 1) {
        "成功<input class='state' value='" + x.state + "'>"
      } else {
        "失败<input class='state' value='" + x.state + "'>"
      }
      val results = if (x.state == 1) {
        s"""
           |<a class="fastq" href="/resequencing/task/download?id=${x.id}&code=1" title="变异检测结果（SNP InDel）"><b>output.vcf</b></a>,&nbsp;
           |<a class="fastq" href="/resequencing/task/download?id=${x.id}&code=2" title="变异注释结果（SNP InDel）"><b>ann.vcf</b></a>&nbsp;
           """.stripMargin
      } else {
        ""
      }
      val operation = if (x.state == 1) {
        s"""
           |  <button class="update" onclick="restart(this)" value="${x.id}" title="重新运行"><i class="fa fa-repeat"></i></button>
           |  <button class="update" onclick="openLog(this)" value="${x.id}" title="查看日志"><i class="fa fa-file-text"></i></button>
           |  <button class="delete" onclick="openDelete(this)" value="${x.taskname}" id="${x.id}" title="删除任务"><i class="fa fa-trash"></i></button>
           """.stripMargin
      } else if (x.state == 2) {
        s"""<button class="delete" onclick="openDelete(this)" value="${x.taskname}" id="${x.id}" title="删除任务"><i class="fa fa-trash"></i></button>
           |<button class="update" onclick="openLog(this)" value="${x.id}" title="查看日志"><i class="fa fa-file-text"></i></button>
         """.stripMargin
      } else {
        s"""<button class="delete" onclick="openDelete(this)" value="${x.taskname}" id="${x.id}" title="删除任务"><i class="fa fa-trash"></i></button>"""
      }
      Json.obj("taskname" -> taskname, "state" -> state, "createdate" -> date, "results" -> results, "operation" -> operation)
    }
    json

  }

  def deleteTask(id: Int) = Action.async { implicit request =>
    taskdao.getById(id).flatMap { x =>
      taskdao.deleteTask(id).map { y =>
        val run = Future {
          val path = Utils.taskPath(x.accountid, x.projectid, id)
          FileUtils.deleteDirectory(new File(path))
        }
        Ok(Json.toJson("success"))
      }
    }
  }

  def getLog(id: Int) = Action { implicit request =>
    val row = Await.result(taskdao.getById(id), Duration.Inf)
    val path = Utils.taskPath(row.accountid, row.projectid, row.id)
    val log = FileUtils.readLines(new File(path, "log.txt")).asScala
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

  def download(id: Int, code: Int) = Action { implicit request =>
    val row = Await.result(taskdao.getById(id), Duration.Inf)
    val path = Utils.taskPath(row.accountid, row.projectid, id)
    val (file, name) = if (code == 1) {
      (new File(path, "filtered.vcf"), "output.vcf")
    } else {
      (new File(path, "snp.vcf"), "ann.vcf")
    }
    Ok.sendFile(file).withHeaders(
      CACHE_CONTROL -> "max-age=3600",
      CONTENT_DISPOSITION -> ("attachment; filename=" + name),
      CONTENT_TYPE -> "application/x-download"
    )
  }

  case class updateTasknameData(taskId: Int, newtaskname: String)

  val updateTasknameForm = Form(
    mapping(
      "taskId" -> number,
      "newtaskname" -> text
    )(updateTasknameData.apply)(updateTasknameData.unapply)
  )

  def updateTaskname = Action.async { implicit request =>
    val data = updateTasknameForm.bindFromRequest.get
    val id = data.taskId
    val name = data.newtaskname
    taskdao.updateTaskName(id, name).map { x =>
      Ok(Json.obj("valid" -> "true"))
    }
  }

  def getDeploy(id: Int) = Action.async { implicit request =>
    val x = Await.result(taskdao.getById(id), Duration.Inf)
    val path = Utils.taskPath(x.accountid, x.projectid, x.id)
    val deploy = FileUtils.readLines(new File(path, "deploy.txt")).asScala
    val sample = deploy(2).split(",")
    taskdao.getById(id).flatMap { x =>
      val userId = x.accountid
      val proId = x.projectid
      sampledao.checkByPosition(userId, proId, deploy(2)).map { y =>
        val (valid, message) = if (sample.size == y.size) {
          ("true", "success")
        } else {
          val validSample = y.map(_.sample)
          val s = sample.diff(validSample)
          ("false", "样品" + s.mkString(",") + "已被删除")
        }
        val json = Json.obj("sample" -> deploy(2), "genotype_likelihoods_model" -> deploy(3),
          "standard_min_confidence_threshold_for_calling" -> deploy(4),
          "standard_min_confidence_threshold_for_emitting" -> deploy(5), "sample_ploidy" -> deploy(6),
          "minQ" -> deploy(7), "MinDepth" -> deploy(8), "MaxDepth" -> deploy(9), "FS" -> deploy(10), "RPRS" -> deploy(11),
          "udLength" -> deploy(12), "spliceSiteSize" -> deploy(13), "noLog" -> deploy(14), "setSpliceRegions" -> deploy(15),
          "spliceRegionExonSize" -> deploy(16), "spliceRegionIntronMin" -> deploy(17), "spliceRegionIntronMax" -> deploy(18),
          "valid" -> valid, "message" -> message)
        Ok(Json.toJson(json))
      }
    }
  }


  case class resetData(taskIds: Int, genotype_likelihoods_model: String, standard_min_confidence_threshold_for_calling: Option[String],
                       standard_min_confidence_threshold_for_emitting: Option[String], sample_ploidy: Option[Int],
                       minQ: Option[Int], MinDepth: Option[Int], MaxDepth: Option[Int], FS: Option[Int], RPRS: Option[String],
                       udLength: String, spliceSiteSize: String, noLog: String, setSpliceRegions: String,
                       spliceRegionExonSize: Option[Int], spliceRegionIntronMin: Option[Int], spliceRegionIntronMax: Option[Int])

  val resetForm = Form(
    mapping(
      "taskIds" -> number,
      "genotype_likelihoods_model" -> text,
      "standard_min_confidence_threshold_for_calling" -> optional(text),
      "standard_min_confidence_threshold_for_emitting" -> optional(text),
      "sample_ploidy" -> optional(number),
      "minQ" -> optional(number),
      "MinDepth" -> optional(number),
      "MaxDepth" -> optional(number),
      "FS" -> optional(number),
      "RPRS" -> optional(text),
      "udLength" -> text,
      "spliceSiteSize" -> text,
      "noLog" -> text,
      "setSpliceRegions" -> text,
      "spliceRegionExonSize" -> optional(number),
      "spliceRegionIntronMin" -> optional(number),
      "spliceRegionIntronMax" -> optional(number)
    )(resetData.apply)(resetData.unapply)
  )

  def resetTask = Action.async { implicit request =>
    val data = resetForm.bindFromRequest.get
    val taskid = data.taskIds
    taskdao.getById(taskid).flatMap { x =>
      val path = Utils.taskPath(x.accountid, x.projectid, x.id)
      val buffer = FileUtils.readLines(new File(path, "deploy.txt")).asScala
      val b = mutable.Buffer(buffer(0), buffer(1), buffer(2), data.genotype_likelihoods_model,
        data.standard_min_confidence_threshold_for_calling.getOrElse(30.0),
        data.standard_min_confidence_threshold_for_emitting.getOrElse(30.0),
        data.sample_ploidy.getOrElse(2), data.minQ.getOrElse(30), data.MinDepth.getOrElse(5),
        data.MaxDepth.getOrElse(10000000), data.FS.getOrElse(0), data.RPRS.getOrElse(2.5),
        data.udLength, data.spliceSiteSize, data.noLog, data.setSpliceRegions, data.spliceRegionExonSize.getOrElse(3),
        data.spliceRegionIntronMin.getOrElse(3), data.spliceRegionIntronMax.getOrElse(8))
      new File(path, "deploy.txt").delete()
      FileUtils.writeLines(new File(path, "deploy.txt"), b.asJava)
      taskdao.updateState(x.id, 0).map { y =>
        Ok(Json.obj("valid" -> "true", "id" -> x.id))
      }
    }
  }

  def runResetCmd(id: Int) = Action.async { implicit request =>
    taskdao.getById(id).map { x =>
      runCmd(x.id)
      Ok(Json.toJson("success"))
    }
  }

  case class checkNewnameData(newtaskname: String)

  val checkNewnameForm = Form(
    mapping(
      "newtaskname" -> text
    )(checkNewnameData.apply)(checkNewnameData.unapply)
  )

  def checkNewname(proname: String) = Action.async { implicit request =>
    val data = checkNewnameForm.bindFromRequest.get
    val ses = getUserIdAndProId(request.session, proname)
    taskdao.getAllByPosi(ses._1, ses._2, data.newtaskname).map { x =>
      val valid = if (x.size == 0) {
        "true"
      } else {
        "false"
      }
      Ok(Json.obj("valid" -> valid))
    }
  }

}


