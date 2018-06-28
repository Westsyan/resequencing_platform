package dao

import javax.inject.Inject

import models.Tables._
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class speciesDao@Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends
  HasDatabaseConfigProvider[JdbcProfile] {

  import profile.api._

  def addSpecies(row:SpeciesRow):Future[Unit] = {
    db.run(Species ++= Seq(row)).map(_=>())
  }

  def getByPosition(name:String,userId:Int) : Future[Seq[SpeciesRow]] = {
    db.run(Species.filter(_.accountid === userId).filter(_.speciesname === name).result)
  }

  def getById(id:Int) : Future[SpeciesRow] = {
    db.run(Species.filter(_.id === id).result.head)
  }

  def updateState(id:Int,state:Int) : Future[Unit] = {
    db.run(Species.filter(_.id === id).map(_.state).update(state)).map(_=>())
  }

  def getByUserId(userId:Int): Future[Seq[SpeciesRow]] = {
    db.run(Species.filter(_.accountid === userId).result)
  }

  def deleteById(id:Int) : Future[Unit] = {
    db.run(Species.filter(_.id === id).delete).map(_=>())
  }

  def deleteByUserid(id:Int) : Future[Unit] = {
    db.run(Species.filter(_.accountid === id).delete).map(_ => ())
  }

  def updateSpeciesname(id:Int,name:String) : Future[Unit] = {
    db.run(Species.filter(_.id === id).map(_.speciesname).update(name)).map(_=>())
  }

  def getIdByPosition(name:String,userId:Seq[Int]) : Future[SpeciesRow] ={
    db.run(Species.filter(_.accountid.inSetBind(userId)).filter(_.speciesname === name).result.head)
  }

  def getByName(name:String) : Future[Seq[SpeciesRow]] = {
    db.run(Species.filter(_.speciesname === name).result)
  }
}
