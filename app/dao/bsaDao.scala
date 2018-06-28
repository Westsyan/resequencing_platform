package dao

import javax.inject.Inject

import models.Tables._
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile

import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global

class bsaDao @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends
  HasDatabaseConfigProvider[JdbcProfile]  {

  import profile.api._

  def addBsaInfo(bsaRow:Seq[BsaRow]) : Future[Unit] = {
    db.run(Bsa ++= bsaRow).map(_ =>())
  }

  def getAllByPosition(userId:Int,proId:Int,bsaname:String) : Future[BsaRow] = {
    db.run(Bsa.filter(_.accountid === userId).filter(_.projectid === proId).filter(_.bsaname === bsaname).result.head)
  }

  def getAllByPosi(userId:Int,proId:Int,bsaname:String) : Future[Seq[BsaRow]] = {
    db.run(Bsa.filter(_.accountid === userId).filter(_.projectid === proId).filter(_.bsaname === bsaname).result)
  }

  def updateState(id:Int,state:Int) : Future[Unit] = {
    db.run(Bsa.filter(_.id === id).map(_.state).update(state)).map(_=>())
  }

  def getAllBsaByPosition(userId:Int,proId:Int) : Future[Seq[BsaRow]] = {
    db.run(Bsa.filter(_.accountid === userId).filter(_.projectid === proId).result)
  }

  def deleteBsa(id:Int) : Future[Unit] = {
    db.run(Bsa.filter(_.id === id).delete).map(_ =>())
  }

  def getById(id:Int) : Future[BsaRow] ={
    db.run(Bsa.filter(_.id === id).result.head)
  }

  def updateBsaName(id:Int,newName:String) : Future[Unit] = {
    db.run(Bsa.filter(_.id === id).map(_.bsaname).update(newName)).map(_ =>())
  }

  def deleteByUserid(id:Int) : Future[Unit] = {
    db.run(Bsa.filter(_.accountid === id).delete).map(_ => ())
  }

}
