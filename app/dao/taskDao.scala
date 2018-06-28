package dao

import javax.inject.Inject

import models.Tables._
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile

import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global

class taskDao @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends
  HasDatabaseConfigProvider[JdbcProfile] {

  import profile.api._

  def addTaskInfo(taskRow:Seq[TaskRow]) : Future[Unit] = {
    db.run(Task ++= taskRow).map(_ =>())
  }

  def getAllByPosition(userId:Int,proId:Int,taskname:String) : Future[TaskRow] = {
    db.run(Task.filter(_.accountid === userId).filter(_.projectid === proId).filter(_.taskname === taskname).result.head)
  }

  def getAllByPosi(userId:Int,proId:Int,taskname:String) : Future[Seq[TaskRow]] = {
    db.run(Task.filter(_.accountid === userId).filter(_.projectid === proId).filter(_.taskname === taskname).result)
  }

  def updateState(id:Int,state:Int) : Future[Unit] = {
    db.run(Task.filter(_.id === id).map(_.state).update(state)).map(_=>())
  }

  def getAllTaskByPosition(userId:Int,proId:Int) : Future[Seq[TaskRow]] = {
    db.run(Task.filter(_.accountid === userId).filter(_.projectid === proId).result)
  }

  def deleteTask(id:Int) : Future[Unit] = {
    db.run(Task.filter(_.id === id).delete).map(_ =>())
  }

  def deleteByUserid(id:Int) : Future[Unit] = {
    db.run(Task.filter(_.accountid === id).delete).map(_ => ())
  }


  def getById(id:Int) : Future[TaskRow] ={
    db.run(Task.filter(_.id === id).result.head)
  }

  def updateTaskName(id:Int,newName:String) : Future[Unit] = {
    db.run(Task.filter(_.id === id).map(_.taskname).update(newName)).map(_ =>())
  }
}
