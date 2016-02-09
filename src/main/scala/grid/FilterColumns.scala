package grid

import play.api.libs.json.{Writes, Json}
import play.api.mvc.QueryStringBindable
import slick.lifted.{ColumnOrdered, Rep, Query}
import play.api.libs.json._
import slick.model.Table

sealed trait FilterType
case class Equals() extends FilterType
case class Like() extends FilterType

case class GridResponse[T](items: Seq[T], totalCount: Int)
case class ColForFrontend(label: String, name: String)

trait GridColumnTrait {
  val label: String
}
case class GridColumn[T, C](label: String, column: T => Rep[C], filterType: FilterType = Like()) extends GridColumnTrait
case class GridOptionColumn[T, C](label: String, restParamName: String, column: Option[T] => Option[Rep[C]], filterType: FilterType = Like()) extends GridColumnTrait

trait DefaultGridColumnTrait
case class DefaultGridColumn[T, C](column: T => ColumnOrdered[C]) extends DefaultGridColumnTrait
case class DefaultGridOptionColumn[T, C](column: Rep[Option[T]] => ColumnOrdered[Option[C]]) extends DefaultGridColumnTrait

case class FilterColumns(columns: Map[String, String])
case class GridConditions(page: Int, perPage: Int, sortBy: String, sortDir: String, filterBy: FilterColumns)

object FilterColumns {
  implicit def queryStringBinder(implicit binder: QueryStringBindable[String]) = new QueryStringBindable[FilterColumns] {
    override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, FilterColumns]] = {
      val filterKeys = params.keys.filter(_.matches("""filterBy\[(.*?)\]""")).toList.distinct
      val parsedKeys = filterKeys.map(_.drop(9).dropRight(1))
//      println(parsedKeys.filter(_.matches(""".*?\[(.*?)\]""")).map(x => """\[(.*?)\]""".r.findFirstMatchIn(x).get).map(_.toString().drop(1).dropRight(1)))
      val values = filterKeys.map(k => params(k).head)
      val res = FilterColumns(parsedKeys.zip(values).toMap)

      Option(try {
        Right(res)
      } catch {
        case e: Exception => Left("Cannot parse parameter " + key + " as Map[String, String]")
      })
    }
    override def unbind(key: String, params: FilterColumns): String = ""
  }
}

trait GridJsonMappings {
  implicit def gridResponseWrites[E](implicit eWrites: Writes[E]) = new Writes[GridResponse[E]]{
    def writes(value: GridResponse[E]): JsValue = Json.obj(
      "items" -> value.items,
      "totalCount" -> value.totalCount
    )
  }

  implicit object colForFrontendWrites extends OWrites[ColForFrontend] {
    def writes(col: ColForFrontend) = Json.obj(
      "label" -> col.label,
      "name" -> col.name
    )
  }

  implicit object filterTypeWrites extends OWrites[FilterType] {
    def writes(filterType: FilterType) = Json.obj()
  }
}
//TODO zrobic zeby typ a to byl podtyp TABLE
trait Grid[A,B,C[_]]{
  val query: Query[A,B,C]
  val columns: Seq[GridColumnTrait]
  val defaultSortBy: DefaultGridColumnTrait
}