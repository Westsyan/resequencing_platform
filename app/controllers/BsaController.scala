package controllers

import java.io.File
import javax.inject.Inject

import dao._
import models.Tables._
import org.apache.commons.io.FileUtils
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, Controller, Session}
import utils.{ExecCommand, Utils}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class BsaController @Inject()(admindao: adminDao, projectdao: projectDao, sampledao: sampleDao, bsadao : bsaDao) extends Controller{

  def toBsaPage(proname: String): Action[AnyContent] = Action { implicit request =>
    val userId = request.session.get("id").head.toInt
    val allName = Await.result(projectdao.getAllProject(userId), Duration.Inf)
    Ok(views.html.bsa.bsaPage(allName, proname))
  }


  def bsaPage(proname: String) = Action { implicit request =>
    val ses = getUserIdAndProId(request.session, proname)
    val allName = Await.result(projectdao.getAllProject(ses._1), Duration.Inf)
    Ok(views.html.bsa.bsaData(allName, proname))
  }

  case class bsaData(proname: String, taskname: String, sample1: String, sample2 : String, genotype_likelihoods_model: String,
                      standard_min_confidence_threshold_for_calling: Option[String], standard_min_confidence_threshold_for_emitting: Option[String],
                      sample_ploidy: Option[Int],type1:String,type2:String,w:Option[Int],s:Option[Int],v:Option[Int],
                      h:Option[Int],q:Option[String],type3:String)

  val bsaForm = Form(
    mapping(
      "proname" -> text,
      "taskname" -> text,
      "sample1" -> text,
      "sample2" -> text,
      "genotype_likelihoods_model" -> text,
      "standard_min_confidence_threshold_for_calling" -> optional(text),
      "standard_min_confidence_threshold_for_emitting" -> optional(text),
      "sample_ploidy" -> optional(number),
      "type1" -> text,
      "type2" -> text,
      "w" -> optional(number),
      "s" -> optional(number),
      "v" -> optional(number),
      "h" -> optional(number),
      "q" -> optional(text),
      "type3" -> text
    )(bsaData.apply)(bsaData.unapply)
  )

  case class snpData(udLength: String, spliceSiteSize: String, noLog: String, setSpliceRegions: String,
                     spliceRegionExonSize: Option[Int], spliceRegionIntronMin: Option[Int], spliceRegionIntronMax: Option[Int])

  val snpForm = Form(
    mapping(
      "udLength" -> text,
      "spliceSiteSize" -> text,
      "noLog" -> text,
      "setSpliceRegions" -> text,
      "spliceRegionExonSize" -> optional(number),
      "spliceRegionIntronMin" -> optional(number),
      "spliceRegionIntronMax" -> optional(number)
    )(snpData.apply)(snpData.unapply)
  )


  def saveDeploy = Action { implicit request =>
    val data = bsaForm.bindFromRequest.get
    val snpdata = snpForm.bindFromRequest.get
    val proname = data.proname
    val taskname = data.taskname
    val ses = getUserIdAndProId(request.session, proname)
    val date = Utils.date
    val row = BsaRow(0, taskname, ses._1, ses._2, date, 0)
    Await.result(bsadao.addBsaInfo(Seq(row)), Duration.Inf)
    val run = Future {
      val bsa = Await.result(bsadao.getAllByPosition(ses._1, ses._2, taskname), Duration.Inf)
      val otupath = Utils.bsaPath(ses._1, ses._2, bsa.id)
      new File(otupath).mkdirs()
      val deploy = mutable.Buffer(proname, taskname, data.sample1+","+data.sample2, data.genotype_likelihoods_model,
        data.standard_min_confidence_threshold_for_calling.getOrElse(30.0),
        data.standard_min_confidence_threshold_for_emitting.getOrElse(30.0),
        data.sample_ploidy.getOrElse(2),data.type1,data.type2,data.w.getOrElse(1000000),data.s.getOrElse(10000),
        data.v.getOrElse(12),data.h.getOrElse(9),data.q.getOrElse(1000000),data.type3,snpdata.udLength,
        snpdata.spliceSiteSize, snpdata.noLog, snpdata.setSpliceRegions, snpdata.spliceRegionExonSize.getOrElse(3),
        snpdata.spliceRegionIntronMin.getOrElse(3), snpdata.spliceRegionIntronMax.getOrElse(8))
      FileUtils.writeLines(new File(otupath + "/deploy.txt"), deploy.asJava)
      runCmd(bsa.id)
    }
    val json = Json.obj("valid" -> "true")
    Ok(Json.toJson(json))
  }

  def getUserIdAndProId(session: Session, proname: String): (Int, Int) = {
    val userId = session.get("id").head.toInt
    val proId = Await.result(projectdao.getIdByProjectname(userId, proname), Duration.Inf)
    (userId, proId)
  }

  case class checkBsanameData(taskname: String)

  val checkBsanameForm = Form(
    mapping(
      "taskname" -> text
    )(checkBsanameData.apply)(checkBsanameData.unapply)
  )

  def checkName(proname: String) = Action.async { implicit request =>
    val data = checkBsanameForm.bindFromRequest.get
    val ses = getUserIdAndProId(request.session, proname)
    bsadao.getAllByPosi(ses._1, ses._2, data.taskname).map { x =>
      val valid = if (x.size == 0) {
        "true"
      } else {
        "false"
      }
      Ok(Json.obj("valid" -> valid))
    }
  }

  def runCmd(id: Int) = {
    val row = Await.result(bsadao.getById(id), Duration.Inf)
    val userId = row.accountid
    val proId = row.projectid
    val path = Utils.bsaPath(userId, proId, id)
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
    val command3 = cleanVcfCmd(path, deploy)
    val command4 = caculationCmd(path, deploy)
    val command5 = plotSnpCmd(path, deploy)
    val command6 = regionCmd(path, deploy)
    val command7 = variantCmd(path)
    val command8 = snpCmd(deploy,path,species)

    command.exect(command2, command3, command4, command5, command6, command7, command8,  tmp)
    if (com.isSuccess && command.isSuccess) {
      new File(path, "snp.sh").delete()
      new File(path, "vcf.sh").delete()
      new File(path, "variant.vcf").delete()
      Await.result(bsadao.updateState(id, 1), Duration.Inf)
      val log = com.getErrStr ++ command.getErrStr
      FileUtils.writeStringToFile(new File(path, "log.txt"), log)
      FileUtils.deleteDirectory(new File(tmp))
    } else {
      Await.result(bsadao.updateState(id, 2), Duration.Inf)
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

  def cleanVcfCmd(outPath:String,deploy:mutable.Buffer[String]) : String ={
    var command = s"perl ${Utils.toolPath}/filterMissingData.pl -i ${outPath}/variants.vcf -o " +
                  s"${outPath}/bsa_clean_data.vcf -o1 ${outPath}/scaffold_name_list.txt "
    if(deploy(7) == "yes"){
      command += "-b T "
    }
    if(deploy(8) == "yes"){
      command += "-c T "
    }
    command
  }

  def caculationCmd(outPath:String,deploy: mutable.Buffer[String]) : String = {
    val command = s"perl ${Utils.toolPath}/deltaSnpIndexStatistics.pl -i ${outPath}/bsa_clean_data.vcf -o " +
                  s"${outPath}/snp_index.txt -w ${deploy(9)} -s ${deploy(10)}"
    command
  }

  def plotSnpCmd(outPath:String,deploy:mutable.Buffer[String]) : String = {
    val command = s"perl ${Utils.toolPath}/plotBSA.pl -i ${outPath}/snp_index.txt -o ${outPath}/snp_index_plot.pdf " +
                  s"-w ${deploy(11)} -h ${deploy(12)}"
    command
  }

  def regionCmd(outPath:String,deploy:mutable.Buffer[String]) : String = {
    var command = s"perl ${Utils.toolPath}/filterInterval.pl -i ${outPath}/snp_index.txt -o ${outPath}/candidate_region.txt " +
                  s" -o1 ${outPath}/continuous_candidate_region.txt "
    if(deploy(13) == "yes"){
      command += "-b T "
    }
    command += s"-q ${deploy(14)}"
    command
  }

  def variantCmd(outPath:String) : String = {
    val command = s"perl ${Utils.toolPath}/filterVcfFromInterval.pl -i ${outPath}/bsa_clean_data.vcf -i1 " +
                  s"${outPath}/continuous_candidate_region.txt -o ${outPath}/candidate_variants.vcf"
    command
  }

  def annoCmd(outPath:String) : String = {
    val command = s"perl ${Utils.toolPath}/annotate_variation_fromvcf.pl -i ${outPath}/candidate_variants.vcf " +
                  s"-o ${outPath}/annotated_variation.txt -g OS"
    command
  }

  def snpCmd(deploy: mutable.Buffer[String], path: String, speciesId: Int): String = {
    var command = s"java -jar ${Utils.toolPath}/snpEff/snpEff.jar -i vcf -o vcf " +
      s"-upDownStreamLen ${deploy(15)} -spliceSiteSize ${deploy(16)} "
    if (deploy(18) == "yes") {
      command += s" -spliceRegionExonSize ${deploy(19)} -spliceRegionIntronMin ${deploy(20)} -spliceRegionIntronMax ${deploy(21)}"
    }
    command += s" ${deploy(17)} ${speciesId} ${path}/candidate_variants.vcf  > ${path}/snp.vcf "
    FileUtils.writeStringToFile(new File(path, "snp.sh"), command)
    val command1 = s"sh ${path}/snp.sh"
    command1
  }

  def isRunCmd(id: Int) = Action.async { implicit request =>
    bsadao.getById(id).map { x =>
      runCmd(x.id)
      Ok(Json.toJson("success"))
    }
  }

  def getAllBsa(proname: String) = Action { implicit request =>
    val json = dealWithSample(proname, request.session)
    Ok(Json.toJson(json))
  }

  def dealWithSample(proname: String, session: Session) = {
    val id = getUserIdAndProId(session, proname)
    val tasks = Await.result(bsadao.getAllBsaByPosition(id._1, id._2), Duration.Inf)
    val json = tasks.sortBy(_.id).reverse.map { x =>
      val taskname = x.bsaname
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
           |<a class="fastq" href="/resequencing/bsa/download?id=${x.id}&code=1" title="SNP InDel 染色体"><b>snp_index_plot.pdf</b></a>,&nbsp;
           |<a class="fastq" href="/resequencing/bsa/download?id=${x.id}&code=2" title="注释结果"><b>candidate_variants.vcf</b></a>,&nbsp;
           |<a class="fastq" href="/resequencing/bsa/download?id=${x.id}&code=3" title="注释结果"><b>bsa_clean_data.vcf</b></a>,&nbsp;
           |<a class="fastq" href="/resequencing/bsa/download?id=${x.id}&code=4" title="注释结果"><b>candidate_region.txt</b></a>,&nbsp;
           |<a class="fastq" href="/resequencing/bsa/download?id=${x.id}&code=5" title="注释结果"><b>ann.vcf</b></a>&nbsp;
           """.stripMargin
      } else {
        ""
      }
      val operation = if (x.state == 1) {
        s"""
           |  <button class="update" onclick="restart(this)" value="${x.id}" title="重新运行"><i class="fa fa-repeat"></i></button>
           |  <button class="update" onclick="openLog(this)" value="${x.id}" title="查看日志"><i class="fa fa-file-text"></i></button>
           |  <button class="delete" onclick="openDelete(this)" value="${x.bsaname}" id="${x.id}" title="删除任务"><i class="fa fa-trash"></i></button>
           """.stripMargin
      } else if (x.state == 2) {
        s"""<button class="delete" onclick="openDelete(this)" value="${x.bsaname}" id="${x.id}" title="删除任务"><i class="fa fa-trash"></i></button>
           |<button class="update" onclick="openLog(this)" value="${x.id}" title="查看日志"><i class="fa fa-file-text"></i></button>
         """.stripMargin
      } else {
        s"""<button class="delete" onclick="openDelete(this)" value="${x.bsaname}" id="${x.id}" title="删除任务"><i class="fa fa-trash"></i></button>"""
      }
      Json.obj("taskname" -> taskname, "state" -> state, "createdate" -> date, "results" -> results, "operation" -> operation)
    }
    json

  }

  def deleteBsa(id: Int) = Action.async { implicit request =>
    bsadao.getById(id).flatMap { x =>
      bsadao.deleteBsa(id).map { y =>
        val run = Future {
          val path = Utils.bsaPath(x.accountid, x.projectid, id)
          FileUtils.deleteDirectory(new File(path))
        }
        Ok(Json.toJson("success"))
      }
    }
  }

  def getLog(id: Int) = Action { implicit request =>
    val row = Await.result(bsadao.getById(id), Duration.Inf)
    val path = Utils.bsaPath(row.accountid, row.projectid, row.id)
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
    val row = Await.result(bsadao.getById(id), Duration.Inf)
    val path = Utils.bsaPath(row.accountid, row.projectid, id)
    val (file, name) = if (code == 1) {
      (new File(path, "snp_index_plot.pdf"), "snp_index_plot.pdf")
    } else if(code == 2){
      (new File(path, "candidate_variants.vcf"), "candidate_variants.vcf")
    } else if(code == 3){
      (new File(path, "bsa_clean_data.vcf"), "bsa_clean_data.vcf")
    } else if(code == 4){
      (new File(path, "candidate_region.txt"), "candidate_region.txt")
    } else{
      (new File(path, "snp.vcf"), "ann.vcf")
    }
    Ok.sendFile(file).withHeaders(
      CACHE_CONTROL -> "max-age=3600",
      CONTENT_DISPOSITION -> ("attachment; filename=" + name),
      CONTENT_TYPE -> "application/x-download"
    )
  }

  case class updateBsanameData(taskId: Int, newtaskname: String)

  val updateBsanameForm = Form(
    mapping(
      "taskId" -> number,
      "newtaskname" -> text
    )(updateBsanameData.apply)(updateBsanameData.unapply)
  )

  def updateBsaname = Action.async { implicit request =>
    val data = updateBsanameForm.bindFromRequest.get
    val id = data.taskId
    val name = data.newtaskname
    bsadao.updateBsaName(id, name).map { x =>
      Ok(Json.obj("valid" -> "true"))
    }
  }

  def getDeploy(id: Int) = Action.async { implicit request =>
    val x = Await.result(bsadao.getById(id), Duration.Inf)
    val path = Utils.bsaPath(x.accountid, x.projectid, x.id)
    val deploy = FileUtils.readLines(new File(path, "deploy.txt")).asScala
    val sample = deploy(2).split(",")
    bsadao.getById(id).flatMap { x =>
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
          "standard_min_confidence_threshold_for_emitting" -> deploy(5), "sample_ploidy" -> deploy(6),"type1" -> deploy(7),
          "type2" -> deploy(8), "w" -> deploy(9), "s" -> deploy(10), "v" -> deploy(11), "h" -> deploy(12), "q" -> deploy(13),
          "type3" -> deploy(14), "udLength" -> deploy(15), "spliceSiteSize" -> deploy(16), "noLog" -> deploy(17),
          "setSpliceRegions" -> deploy(18), "spliceRegionExonSize" -> deploy(19), "spliceRegionIntronMin" -> deploy(20),
          "spliceRegionIntronMax" -> deploy(21), "valid" -> valid, "message" -> message)
        Ok(Json.toJson(json))
      }
    }
  }


  case class resetData(taskIds: Int, genotype_likelihoods_model: String, standard_min_confidence_threshold_for_calling: Option[String],
                       standard_min_confidence_threshold_for_emitting: Option[String], sample_ploidy: Option[Int],
                       type1:String,type2:String,w:Option[Int],s:Option[Int],v:Option[Int],
                       h:Option[Int],q:Option[String],type3:String)

  val resetForm = Form(
    mapping(
      "taskIds" -> number,
      "genotype_likelihoods_model" -> text,
      "standard_min_confidence_threshold_for_calling" -> optional(text),
      "standard_min_confidence_threshold_for_emitting" -> optional(text),
      "sample_ploidy" -> optional(number),
      "type1" -> text,
      "type2" -> text,
      "w" -> optional(number),
      "s" -> optional(number),
      "v" -> optional(number),
      "h" -> optional(number),
      "q" -> optional(text),
      "type3" -> text
    )(resetData.apply)(resetData.unapply)
  )

  def resetBsa = Action.async { implicit request =>
    val data = resetForm.bindFromRequest.get
    val snpdata = snpForm.bindFromRequest.get
    val taskid = data.taskIds
    bsadao.getById(taskid).flatMap { x =>
      val path = Utils.bsaPath(x.accountid, x.projectid, x.id)
      val buffer = FileUtils.readLines(new File(path, "deploy.txt")).asScala
      val b = mutable.Buffer(buffer.head, buffer(1), buffer(2), data.genotype_likelihoods_model,
        data.standard_min_confidence_threshold_for_calling.getOrElse(30.0),
        data.standard_min_confidence_threshold_for_emitting.getOrElse(30.0),
        data.sample_ploidy.getOrElse(2),data.type1,data.type2,data.w.getOrElse(1000000),data.s.getOrElse(10000),
      data.v.getOrElse(12),data.h.getOrElse(9),data.q.getOrElse(1000000),data.type3,snpdata.udLength,
        snpdata.spliceSiteSize, snpdata.noLog, snpdata.setSpliceRegions, snpdata.spliceRegionExonSize.getOrElse(3),
        snpdata.spliceRegionIntronMin.getOrElse(3), snpdata.spliceRegionIntronMax.getOrElse(8))
      new File(path, "deploy.txt").delete()
      FileUtils.writeLines(new File(path, "deploy.txt"), b.asJava)
      bsadao.updateState(x.id, 0).map { y =>
        Ok(Json.obj("valid" -> "true", "id" -> x.id))
      }
    }
  }

  def runResetCmd(id: Int) = Action.async { implicit request =>
    bsadao.getById(id).map { x =>
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
    bsadao.getAllByPosi(ses._1, ses._2, data.newtaskname).map { x =>
      val valid = if (x.size == 0) {
        "true"
      } else {
        "false"
      }
      Ok(Json.obj("valid" -> valid))
    }
  }


}
