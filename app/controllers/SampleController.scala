package controllers

import java.io.File
import javax.inject.Inject

import akka.stream.IOResult
import akka.stream.scaladsl.{FileIO, Sink}
import akka.util.ByteString
import dao._
import models.Tables._
import org.apache.commons.io.FileUtils
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.libs.streams.Accumulator
import play.api.mvc.MultipartFormData.FilePart
import play.api.mvc._
import play.core.parsers.Multipart.{FileInfo, FilePartHandler}
import utils._

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}


class SampleController @Inject()(admindao: adminDao, projectdao: projectDao, sampledao: sampleDao) extends Controller {

  def enterHome(projectname: String): Action[AnyContent] = Action { implicit request =>
    val userId = request.session.get("id").head.toInt
    val projectId = Await.result(projectdao.getIdByProjectname(userId, projectname), Duration.Inf)
    val data = new File(Utils.path + "/" + userId + "/" + projectId)
    val allName = Await.result(projectdao.getAllProject(userId), Duration.Inf)
    if (data.listFiles().size < 2) {
      Redirect(routes.SampleController.loadData(projectname))
    } else {
      Redirect(routes.SampleController.dataPage(projectname))
    }
  }


  def loadData(proname: String): Action[AnyContent] = Action { implicit request =>
    val userId = request.session.get("id").head.toInt
    val allName = Await.result(projectdao.getAllProject(userId), Duration.Inf)
    Ok(views.html.fileupload.uploadPE(allName, proname))
  }

  def toSE(proname: String): Action[AnyContent] = Action { implicit request =>
    val userId = request.session.get("id").head.toInt
    val allName = Await.result(projectdao.getAllProject(userId), Duration.Inf)
    Ok(views.html.fileupload.uploadSE(allName, proname))
  }

  def home = Action { implicit request =>
    val userId = request.session.get("id").head.toInt
    val all = Await.result(projectdao.getAll(userId), Duration.Inf)
    val projectname = all.map(_.projectname)
    Ok(views.html.background.home(all, projectname))
  }

  case class paraData(proname: String, sample: String, encondingType: String, stepMethod: String, adapter: String,
                      seed_mismatches: Option[Int], palindrome_clip_threshold: Option[Int],
                      simple_clip_threshold: Option[Int], trimMethod: String, window_size: Option[Int],
                      required_quality: Option[Int], minlenMethod: String, minlen: Option[Int],
                      leadingMethod: String, leading: Option[Int], trailingMethod: String, trailing: Option[Int],
                      cropMethod: String, crop: Option[Int], headcropMethod: String, headcrop: Option[Int])


  val paraForm = Form(
    mapping(
      "proname" -> text,
      "sample" -> text,
      "encondingType" -> text,
      "stepMethod" -> text,
      "adapter" -> text,
      "seed_mismatches" -> optional(number),
      "palindrome_clip_threshold" -> optional(number),
      "simple_clip_threshold" -> optional(number),
      "trimMethod" -> text,
      "window_size" -> optional(number),
      "required_quality" -> optional(number),
      "minlenMethod" -> text,
      "minlen" -> optional(number),
      "leadingMethod" -> text,
      "leading" -> optional(number),
      "trailingMethod" -> text,
      "trailing" -> optional(number),
      "cropMethod" -> text,
      "crop" -> optional(number),
      "headcropMethod" -> text,
      "headcrop" -> optional(number)
    )(paraData.apply)(paraData.unapply)
  )

  case class bwaData(adv_pe_options_selector: String, aa: Option[Int], ao: Option[Int], an: Option[Int], aN: Option[Int],
                     analysis_type_selector: String, n: Option[String], o: Option[Int], e: Option[Int], i: Option[Int], d: Option[Int],
                     l: Option[Int], k: Option[Int], m: Option[Int], M: Option[Int], O: Option[Int], E: Option[Int], R: Option[Int],
                     q: Option[Int], L: String, mapq: Option[Int])

  val bwaForm = Form(
    mapping(
      "adv_pe_options_selector" -> text,
      "aa" -> optional(number),
      "ao" -> optional(number),
      "an" -> optional(number),
      "aN" -> optional(number),
      "analysis_type_selector" -> text,
      "n" -> optional(text),
      "o" -> optional(number),
      "e" -> optional(number),
      "i" -> optional(number),
      "d" -> optional(number),
      "l" -> optional(number),
      "k" -> optional(number),
      "m" -> optional(number),
      "M" -> optional(number),
      "O" -> optional(number),
      "E" -> optional(number),
      "R" -> optional(number),
      "q" -> optional(number),
      "L" -> text,
      "mapq" -> optional(number)
    )(bwaData.apply)(bwaData.unapply)
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


  def uploadPE = Action(parse.multipartFormData(handleFilePartAsFile)) { implicit request =>
    val path = Utils.path
    val file = request.body.files
    val paradata = paraForm.bindFromRequest.get
    val bwadata = bwaForm.bindFromRequest.get
    val proname = paradata.proname
    val userId = request.session.get("id").head.toInt
    val project = Await.result(projectdao.getProject(userId, proname), Duration.Inf)
    val sample = paradata.sample
    val date = Utils.date
    val sa = SampleRow(0, sample, userId, project.id, date, "PE", 0)
    Await.result(sampledao.addSample(Seq(sa)), Duration.Inf)
    val row = Await.result(sampledao.getByPosition(userId, project.id, sample), Duration.Inf)

    try {
      val run = Future {
        val outPath = Utils.outPath(userId, project.id, row.id)
        val in1 = file.head.ref.getPath
        val name1 = file.head.filename
        val in2 = file(1).ref.getPath
        val name2 = file(1).filename
        val out1 = outPath + "/raw.data_1.fastq"
        val out2 = outPath + "/raw.data_2.fastq"
        getFastq(in1, out1, name1)
        getFastq(in2, out2, name2)
        new File(outPath + "/tmp").mkdir()
        saveDeploy(row.id, paradata, bwadata, outPath)
        runCmd1(row, "PE")
      }
    }catch {
      case e : Exception => Await.result(sampledao.updateState(row.id,2),Duration.Inf)
    }

    Ok(Json.obj("valid" -> "true"))
  }

  def uploadSE = Action(parse.multipartFormData(handleFilePartAsFile)) { implicit request =>
    val path = Utils.path
    val file = request.body.file("file").get
    val paradata = paraForm.bindFromRequest.get
    val bwadata = bwaForm.bindFromRequest.get
    val proname = paradata.proname
    val userId = request.session.get("id").head.toInt
    val project = Await.result(projectdao.getProject(userId, proname), Duration.Inf)
    val sample = paradata.sample
    val date = Utils.date
    val sa = SampleRow(0, sample, userId, project.id, date, "SE", 0)
    Await.result(sampledao.addSample(Seq(sa)), Duration.Inf)
    val row = Await.result(sampledao.getByPosition(userId, project.id, sample), Duration.Inf)
    try {
      val run = Future {
        val outPath = Utils.outPath(userId, project.id, row.id)
        val in = file.ref.getPath
        val name = file.filename
        val out = outPath + "/raw.data.fastq"
        getFastq(in, out, name)
        new File(outPath + "/tmp").mkdir()
        saveDeploy(row.id, paradata, bwadata, outPath)
        runCmd1(row, "SE")
      }
    }catch{
      case e :Exception => Await.result(sampledao.updateState(row.id,2),Duration.Inf)
    }
    Ok(Json.obj("valid" -> "true"))
  }

  def saveDeploy(id: Int, paradata: paraData, bwadata: bwaData, outPath: String) = {
    val type1 = paradata.encondingType
    val deploy = mutable.Buffer(id, type1, paradata.stepMethod, paradata.adapter,
      paradata.seed_mismatches.getOrElse(2), paradata.palindrome_clip_threshold.getOrElse(30),
      paradata.simple_clip_threshold.getOrElse(10), paradata.trimMethod, paradata.window_size.getOrElse(50),
      paradata.required_quality.getOrElse(20), paradata.minlenMethod, paradata.minlen.getOrElse(50),
      paradata.leadingMethod, paradata.leading.getOrElse(3), paradata.trailingMethod, paradata.trailing.getOrElse(3),
      paradata.cropMethod, paradata.crop.getOrElse(0), paradata.headcropMethod, paradata.headcrop.getOrElse(0),
      bwadata.adv_pe_options_selector, bwadata.aa.getOrElse(500), bwadata.ao.getOrElse(100000), bwadata.an.getOrElse(3),
      bwadata.aN.getOrElse(10), bwadata.analysis_type_selector, bwadata.n.getOrElse(0.04), bwadata.o.getOrElse(1),
      bwadata.e.getOrElse(-1), bwadata.i.getOrElse(5), bwadata.d.getOrElse(10), bwadata.l.getOrElse(32), bwadata.k.getOrElse(2),
      bwadata.m.getOrElse(200000), bwadata.M.getOrElse(3), bwadata.O.getOrElse(11), bwadata.E.getOrElse(4),
      bwadata.R.getOrElse(30), bwadata.q.getOrElse(0), bwadata.L, bwadata.mapq.getOrElse(0))
    FileUtils.writeLines(new File(outPath + "/deploy.txt"), deploy.asJava)
  }

  def resetPE = Action { implicit request =>
    val path = Utils.path
    val paradata = paraForm.bindFromRequest.get
    val bwadata = bwaForm.bindFromRequest.get
    val proname = paradata.proname
    val userId = request.session.get("id").head.toInt
    val project = Await.result(projectdao.getProject(userId, proname), Duration.Inf)
    val sample = paradata.sample
    val sampleRow = Await.result(sampledao.getByPosition(userId, project.id, sample), Duration.Inf)
    Await.result(sampledao.updateState(sampleRow.id, 0), Duration.Inf)
    val outPath = Utils.outPath(userId, project.id, sampleRow.id)
    new File(outPath + "/tmp").mkdir()

    saveDeploy(sampleRow.id, paradata, bwadata, outPath)

    Ok(Json.obj("valid" -> "true"))
  }

  def runCmd1(row: SampleRow, inputType: String) = {
    val outPath = Utils.outPath(row.accountid, row.projectid, row.id)
    val deploy = FileUtils.readLines(new File(outPath, "deploy.txt")).asScala
    val project = Await.result(projectdao.getById(row.projectid), Duration.Inf)
    val speciesid = project.speciesid
    val speciesPath = Utils.speciesPath + "/" + speciesid
    val (command1, command2, command3) = if (inputType == "PE") {
      val fastqc = s"${Utils.toolPath}/FastQC/fastqc ${outPath}/raw.data_1.fastq ${outPath}/raw.data_2.fastq -o ${outPath}/"
      (PETrimmomatic(outPath, deploy), BWACmd(outPath, speciesPath, deploy, "PE",row.id), fastqc)
    } else {
      val fastqc = s"${Utils.toolPath}/FastQC/fastqc ${outPath}/raw.data.fastq -o ${outPath}/"
      (SETrimmomatic(outPath, deploy), BWACmd(outPath, speciesPath, deploy, "SE",row.id), fastqc)
    }
    val command4 = SAMtoBAM(outPath)
    val command5 = bamFilter(outPath, deploy(40))
    val command6 = sortBam(outPath)
    val command7 = baiCmd(outPath)

    val command = new ExecCommand

    val tmp = outPath + "/tmp"
    command.exect(command1, command2 ,command3, command4, command5, command6,command7, tmp)
    val samples = Await.result(sampledao.getAllSample(row.accountid, row.projectid), Duration.Inf)
    Await.result(projectdao.updateCount(row.projectid, samples.size), Duration.Inf)
    if (command.isSuccess) {
      val com = new ExecCommand
      val command8 = flagCmd(outPath)
      com.exec(command8)
      FileUtils.writeStringToFile(new File(outPath,"result.txt"),com.getOutStr)
      FileUtils.deleteDirectory(new File(tmp))
      Await.result(sampledao.updateState(row.id, 1), Duration.Inf)
      FileUtils.writeStringToFile(new File(outPath, "log.txt"), command.getErrStr)
    } else {
      FileUtils.deleteDirectory(new File(tmp))
      Await.result(sampledao.updateState(row.id, 2), Duration.Inf)
      if (new File(outPath).exists()) {
        FileUtils.writeStringToFile(new File(outPath, "log.txt"), command.getErrStr)
      }
    }
  }

  def getFastq(path: String, outputPath: String, name: String): Unit = {
    val suffix = name.split('.').last
    if (suffix == "gz") {
      FileUtils.writeStringToFile(new File(outputPath), "")
      CompactAlgorithm.unGzipFile(path, outputPath)
      new File(path).delete()
    } else {
      FileUtils.moveFile(new File(path), new File(outputPath))
    }
  }



  def PETrimmomatic(outPath: String, data: mutable.Buffer[String]): String = {
    val path = Utils.toolPath
    val in1 = outPath + "/raw.data_1.fastq"
    val in2 = outPath + "/raw.data_2.fastq"
    val tmpDir = outPath + "/tmp"
    val out1 = outPath + "/r1_paired_out.fastq"
    val unout1 = tmpDir + "/r1_unpaired_out.fastq"
    val out2 = outPath + "/r2_paired_out.fastq"
    val unout2 = tmpDir + "/r2_unpaired_out.fastq"
    var command = s"java -jar ${path}/Trimmomatic-0.32/trimmomatic-0.32.jar PE -threads 1 " +
      s"${data(1)} ${in1} ${in2} ${out1} ${unout1} ${out2} ${unout2} "
    if (data(2) == "yes") {
      val adapter = path + "/Trimmomatic-0.32/adapters/" + data(3)
      command += s"ILLUMINACLIP:${adapter}:${data(4)}:${data(5)}:${data(6)} "
    }
    if (data(7) == "yes") {
      command += s"SLIDINGWINDOW:${data(8)}:${data(9)} "
    }
    if (data(10) == "yes") {
      command += s"MINLEN:${data(11)} "
    }
    if (data(12) == "yes") {
      command += s"LEADING:${data(13)} "
    }
    if (data(14) == "yes") {
      command += s"TRAILING:${data(15)} "
    }
    if (data(16) == "yes") {
      command += s"CROP:${data(17)} "
    }
    if (data(18) == "yes") {
      command += s"HEADCROP:${data(19)} "
    }
    command
  }

  def BWACmd(outPath: String, speciesPath: String, data: mutable.Buffer[String], t: String,id:Int): String = {
    var command = s"perl ${Utils.toolPath}/bwa.pl -ref ${speciesPath}/sequences.fa -output ${outPath}/aln_map.sam -sample_name ${id} "
    if (t == "PE") {
      command += s"-in1 ${outPath}/r1_paired_out.fastq -in2 ${outPath}/r2_paired_out.fastq "
      if (data(20) == "set") {
        command += s"-sampe_param __start__ -a ${data(21)} -o ${data(22)} -n ${data(23)} -N ${data(24)} __end__ "
      }
    } else {
      command += s"-in1 ${outPath}/raw.data.fastq "
      if (data(20) == "set") {
        command += s"-sampe_param __start__  -n ${data(23)} __end__ "
      }
    }
    if (data(25) == "full") {
      command += s"-aln_param __start__ -n ${data(26)} -o ${data(27)} -e ${data(28)} -i ${data(29)} -d ${data(30)} " +
        s"-l ${data(31)} -k ${data(32)} -m ${data(33)} -M ${data(34)} -O ${data(35)} -E ${data(36)} " +
        s"-R ${data(37)} -q ${data(38)}  ${data(39)} __end__"
    }
    command
  }

  def SAMtoBAM(outPath: String): String = {
    val command = s"${Utils.samtools} view -bS -o ${outPath}/output.bam ${outPath}/aln_map.sam"

    command
  }

  def bamFilter(outPath: String, mapq: String): String = {
    val command = s"${Utils.samtools} view -b -h -q ${mapq} -o ${outPath}/filtered.bam ${outPath}/output.bam"

    command
  }

  def sortBam(outPath: String): String = {
    val command = s"${Utils.samtools} sort ${outPath}/filtered.bam ${outPath}/sorted"

    command
  }

  def flagCmd(outPath:String) : String = {
    val command = s"${Utils.samtools} flagstat ${outPath}/sorted.bam"
    command
  }

  def baiCmd(outPath:String) : String = {
    val command = s"${Utils.samtools} index ${outPath}/sorted.bam"
    command
  }

  def SETrimmomatic(outPath: String, data: mutable.Buffer[String]): String = {
    val path = Utils.toolPath
    val in = outPath + "/raw.data.fastq"
    val out = outPath + "/raw_se_out.fastq"
    var command = s"java -jar ${path}/Trimmomatic-0.32/trimmomatic-0.32.jar SE -threads 1 " +
      s"${data(1)} ${in}  ${out}  "
    if (data(2) == "yes") {
      val adapter = path + "/Trimmomatic-0.32/adapters/" + data(3)
      command += s"ILLUMINACLIP:${adapter}:${data(4)}:${data(5)}:${data(6)} "
    }
    if (data(7) == "yes") {
      command += s"SLIDINGWINDOW:${data(8)}:${data(9)} "
    }
    if (data(10) == "yes") {
      command += s"MINLEN:${data(11)} "
    }
    if (data(12) == "yes") {
      command += s"LEADING:${data(13)} "
    }
    if (data(14) == "yes") {
      command += s"TRAILING:${data(15)} "
    }
    if (data(16) == "yes") {
      command += s"CROP:${data(17)} "
    }
    if (data(18) == "yes") {
      command += s"HEADCROP:${data(19)} "
    }
    command
  }

  def dataPage(proname: String) = Action { implicit request =>
    val id = getUserIdAndProId(request.session, proname)
    val allName = Await.result(projectdao.getAllProject(id._1), Duration.Inf)
    Ok(views.html.fileupload.data(allName, proname))
  }

  def isRunCmd(id: Int): Action[AnyContent] = Action.async { implicit request =>
    sampledao.getAllById(id).map { x =>
      var valid = "true"
      runCmd1(x, x.inputType)
      Ok(Json.obj("valid" -> valid))
    }
  }

  case class updateSampleData(sampleId: Int, newsample: String)

  val updateSampleForm = Form(
    mapping(
      "sampleId" -> number,
      "newsample" -> text
    )(updateSampleData.apply)(updateSampleData.unapply)
  )

  def updateSample: Action[AnyContent] = Action { implicit request =>
    val data = updateSampleForm.bindFromRequest.get
    val newsample = data.newsample
    val sampleId = data.sampleId
    Await.result(sampledao.updateSample(sampleId, newsample), Duration.Inf)
    val json = Json.obj("valid" -> "true")
    Ok(Json.toJson(json))
  }

  def deleteSample(id: Int): Action[AnyContent] = Action { implicit request =>
    val ses = Await.result(sampledao.getAllById(id), Duration.Inf)
    Await.result(sampledao.deleteSample(id), Duration.Inf)
    val count = Await.result(sampledao.getAllSample(ses.accountid, ses.projectid), Duration.Inf)
    Await.result(projectdao.updateCount(ses.projectid, count.size), Duration.Inf)
    val run = Future{
      val path = Utils.outPath(ses.accountid, ses.projectid, id)
      FileUtils.deleteDirectory(new File(path))
    }
    val json = Json.obj("valid" -> "true")
    Ok(Json.toJson(json))
  }

  def deployGet(id: Int) = Action { implicit request =>
    val row = Await.result(sampledao.getAllById(id), Duration.Inf)
    val path = Utils.outPath(row.accountid, row.projectid, row.id)
    val pro = Await.result(projectdao.getById(row.projectid), Duration.Inf)
    val deploy = FileUtils.readLines(new File(path, "deploy.txt")).asScala
    val json = Json.obj("proname" -> pro.projectname, "id" -> deploy.head, "sample" -> row.sample, "encondingType" -> deploy(1),
      "stepMethod" -> deploy(2), "adapter" -> deploy(3), "seed_mismatches" -> deploy(4),
      "palindrome_clip_threshold" -> deploy(5), "simple_clip_threshold" -> deploy(6), "trimMethod" -> deploy(7),
      "window_size" -> deploy(8), "required_quality" -> deploy(9), "minlenMethod" -> deploy(10),
      "minlen" -> deploy(11), "leadingMethod" -> deploy(12), "leading" -> deploy(13), "trailingMethod" -> deploy(14),
      "trailing" -> deploy(15), "cropMethod" -> deploy(16), "crop" -> deploy(17), "headcropMethod" -> deploy(18),
      "headcrop" -> deploy(19), "adv_pe_options_selector" -> deploy(20), "aa" -> deploy(21), "ao" -> deploy(22),
      "an" -> deploy(23), "aN" -> deploy(24), "adv_se_options_selector" -> deploy(25), "n" -> deploy(26), "o" -> deploy(27),
      "e" -> deploy(28), "i" -> deploy(29), "d" -> deploy(30), "l" -> deploy(31), "k" -> deploy(32), "m" -> deploy(33),
      "M" -> deploy(34), "O" -> deploy(35), "E" -> deploy(36), "R" -> deploy(37), "q" -> deploy(38), "L" -> deploy(39),
      "mapq" -> deploy(40), "inputType" -> row.inputType)

    Ok(Json.toJson(json))
  }

  def downloadPE(id: Int, code: Int) = Action { implicit request =>
    val row = Await.result(sampledao.getAllById(id), Duration.Inf)
    val path = Utils.outPath(row.accountid, row.projectid, id)
    val (file, name) = if (code == 1) {
      (new File(path, "raw.data_1.fastq"), row.sample + "_1.fastq")
    } else if (code == 2) {
      (new File(path, "raw.data_2.fastq"), row.sample + "_2.fastq")
    } else {
      (new File(path, "sorted.bam"), row.sample + ".bam")
    }
    Ok.sendFile(file).withHeaders(
      CACHE_CONTROL -> "max-age=3600",
      CONTENT_DISPOSITION -> ("attachment; filename=" + name),
      CONTENT_TYPE -> "application/x-download"
    )
  }

  def downloadSE(id: Int, code: Int) = Action { implicit request =>
    val row = Await.result(sampledao.getAllById(id), Duration.Inf)
    val path = Utils.outPath(row.accountid, row.projectid, id)
    val (file, name) = if (code == 1) {
      (new File(path, "raw.data.fastq"), row.sample + ".fastq")
    } else {
      (new File(path, "sorted.bam"), row.sample + ".bam")
    }
    Ok.sendFile(file).withHeaders(
      CACHE_CONTROL -> "max-age=3600",
      CONTENT_DISPOSITION -> ("attachment; filename=" + name),
      CONTENT_TYPE -> "application/x-download"
    )
  }

  def openHtml(id: Int, code: Int) = Action { implicit request =>
    val row = Await.result(sampledao.getAllById(id), Duration.Inf)
    val path = Utils.outPath(row.accountid, row.projectid, id)
    val html = if (row.inputType == "PE") {
      if (code == 1) {
        FileUtils.readLines(new File(path, "raw.data_1_fastqc.html")).asScala
      } else {
        FileUtils.readLines(new File(path, "raw.data_2_fastqc.html")).asScala
      }
    } else {
      FileUtils.readLines(new File(path, "raw.data_fastqc.html")).asScala
    }
    Ok(html.mkString("\n")).as(HTML)
  }

  def getUserIdAndProId(session: Session, proname: String): (Int, Int) = {
    val userId = session.get("id").head.toInt
    val proId = Await.result(projectdao.getIdByProjectname(userId, proname), Duration.Inf)
    (userId, proId)
  }

  def openLogFile(id: Int): Action[AnyContent] = Action { implicit request =>
    val row = Await.result(sampledao.getAllById(id), Duration.Inf)
    val path = Utils.outPath(row.accountid, row.projectid, id)
    val log = FileUtils.readLines(new File(path, "/log.txt")).asScala
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

  def openResultFile(id: Int): Action[AnyContent] = Action { implicit request =>
    val row = Await.result(sampledao.getAllById(id), Duration.Inf)
    val path = Utils.outPath(row.accountid, row.projectid, id)
    val result = FileUtils.readLines(new File(path, "/result.txt")).asScala
    var html =
      """
        |<style>
        |   .logClass{
        |       font-size : 16px;
        |       font-weight:normal;
        |   }
        |</style>
      """.stripMargin
    html += "<b class='logClass'>" + result.mkString("</b><br><b class='logClass'>") + "</b>"
    val json = Json.obj("result" -> html)
    Ok(Json.toJson(json))
  }

  case class newsampleData(newsample: String)

  val newsampleForm = Form(
    mapping(
      "newsample" -> text
    )(newsampleData.apply)(newsampleData.unapply)
  )

  def checkNewsample(proname: String) = Action.async { implicit request =>
    val ses = getUserIdAndProId(request.session, proname)
    val data = newsampleForm.bindFromRequest.get
    val newsample = data.newsample
    sampledao.getByP(ses._1, ses._2, newsample).map { y =>
      val valid = if (y.size == 0) {
        "true"
      } else {
        "false"
      }
      Ok(Json.obj("valid" -> valid))
    }
  }

  case class sampleData(sample: String)

  val sampleForm = Form(
    mapping(
      "sample" -> text
    )(sampleData.apply)(sampleData.unapply)
  )

  def checkSample(proname: String) = Action.async { implicit request =>
    val ses = getUserIdAndProId(request.session, proname)
    val data = sampleForm.bindFromRequest.get
    val sample = data.sample
    sampledao.getByP(ses._1, ses._2, sample).map { y =>
      val valid = if (y.size == 0) {
        "true"
      } else {
        "false"
      }
      Ok(Json.obj("valid" -> valid))
    }
  }

  def checkRef(proname: String) = Action.async { implicit request =>
    val id = getUserIdAndProId(request.session, proname)
    sampledao.getAllSample(id._1, id._2).flatMap { x =>
      Thread.sleep(2000)
      sampledao.getAllSample(id._1, id._2).map { y =>
        val s = x.diff(y)
        val valid = if (s.size != 0) {
          "true"
        } else {
          "false"
        }
        Ok(Json.obj("valid" -> valid))
      }
    }
  }

  def getAllSample(proname: String) = Action { implicit request =>
    val json = dealWithSample(proname, request.session)
    Ok(Json.toJson(json))
  }

  def dealWithSample(proname: String, session: Session) = {
    val id = getUserIdAndProId(session, proname)
    val samples = Await.result(sampledao.getAllSample(id._1, id._2), Duration.Inf)
    val json = samples.sortBy(_.id).reverse.map { x =>
      val sample = x.sample
      val inputtype = x.inputType
      val date = x.createdata.toLocalDate
      val state = if (x.state == 0) {
        "正在运行 <img src='/assets/images/timg.gif'  style='width: 20px; height: 20px;'><input class='state' value='" + x.state + "'>"
      } else if (x.state == 1) {
        "成功<input class='state' value='" + x.state + "'>"
      } else {
        "失败<input class='state' value='" + x.state + "'>"
      }
      val results = if (x.state == 1) {
        if (x.inputType == "PE") {
          s"""
             |<a class="fastq" href="/resequencing/sample/downloadPE?id=${x.id}&code=1" title="原始数据"><b>${x.sample}</b><b>_1.fastq</b></a>
             |<a class="fastq" target="_blank" href="/resequencing/sample/openHtml?id=${x.id}&code=1" title="查看数据统计报告"><i class="fa fa-eye"></i></a>,
             |<a class="fastq" href="/resequencing/sample/downloadPE?id=${x.id}&code=2" title="原始数据"><b>${x.sample}</b><b>_2.fastq</b></a>
             |<a class="fastq" target="_blank" href="/resequencing/sample/openHtml?id=${x.id}&code=2" title="查看数据统计报告"><i class="fa fa-eye"></i></a>,
             |<a class="fastq" href="/resequencing/sample/downloadPE?id=${x.id}&code=3" title="整合结果"><b>${x.sample}</b><b>.bam</b></a>
             |<button class="update" onclick="openResult(this)" value="${x.id}" title="查看比对结果统计信息"><i class="fa fa-eye"></i></button>
           """.stripMargin
        } else {
          s"""
             |<a class="fastq" href="/resequencing/sample/downloadSE?id=${x.id}&code=1" title="原始数据"><b>${x.sample}</b><b>.fastq</b></a>
             |<a class="fastq" target="_blank" href="/resequencing/sample/openHtml?id=${x.id}&code=1" title="查看数据统计报告"><i class="fa fa-eye"></i></a>,
             |<a class="fastq" href="/resequencing/sample/downloadSE?id=${x.id}&code=3" title="整合结果"><b>${x.sample}</b><b>.bam</b></a>
             |<button class="update" onclick="openResult(this)" value="${x.id}" title="查看比对结果统计信息"><i class="fa fa-eye"></i></button>
           """.stripMargin
        }

      } else {
        ""
      }
      val operation = if (x.state == 1) {
        s"""
           |  <button class="update" onclick="updateSample(this)" value="${x.sample}" id="${x.id}" title="修改样品名"><i class="fa fa-pencil"></i></button>
           |  <button class="update" onclick="restart(this)" value="${x.id}" title="重新运行"><i class="fa fa-repeat"></i></button>
           |  <button class="update" onclick="openLog(this)" value="${x.id}" title="查看日志"><i class="fa fa-file-text"></i></button>
           |  <button class="delete" onclick="openDelete(this)" value="${x.sample}" id="${x.id}" title="删除样品"><i class="fa fa-trash"></i></button>
           """.stripMargin
      } else if (x.state == 2) {
        s"""<button class="delete" onclick="openDelete(this)" value="${x.sample}" id="${x.id}" title="删除样品"><i class="fa fa-trash"></i></button>
           | <button class="update" onclick="openLog(this)" value="${x.id}" title="查看日志"><i class="fa fa-file-text"></i></button>
         """.stripMargin
      } else {
        s"""<button class="delete" onclick="openDelete(this)" value="${x.sample}" id="${x.id}" title="删除样品"><i class="fa fa-trash"></i></button>"""
      }
      Json.obj("sample" -> sample, "input_type" -> inputtype, "state" -> state, "createdate" -> date, "results" -> results, "operation" -> operation)
    }
    json

  }

  def getAllSampleName(proname: String) = Action.async { implicit request =>
    val ses = getUserIdAndProId(request.session, proname)
    sampledao.getAllSample(ses._1, ses._2).map { x =>
      val sample = x.map { y =>
        val validSample = if (y.state == 1) {
          y.sample
        } else {
          "0"
        }
        validSample
      }.distinct.diff(Array("0")).sorted
      Ok(Json.toJson(sample))
    }
  }
}
