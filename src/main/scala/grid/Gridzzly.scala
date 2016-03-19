package grid

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

class Gridzzly extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro Gridzzly.impl
}

object Gridzzly {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    type QueryYieldParameter = String
    //holds info about the fields of the annotee
    trait Field
    case class IndexedField(name: TermName, tpe: Type, defaultValue: Tree, index: Int) extends Field
    case class IndexedRestParam(label: String, value: Tree, filterType: FilterType)

    def buildFilterField(filter: Tree): FilterType = {
      filter match {
        case q"Like()" => Like()
        case q"Equals()" => Equals()
        case _ => c.abort(c.enclosingPosition, "Filter types should be Like() or Equals()")
      }
    }

    def extractFieldTrees(col: Tree, paramNamesWithTypes: Map[QueryYieldParameter, Tree]): IndexedRestParam = {
       col match {
          case q"GridColumn[$a, $b]($label, $argument => $body)" => {
            buildRestParam(argument, a, paramNamesWithTypes, body, Like())
          }
          case q"GridColumn[$a, $b]($label, $argument => $body, $filterType)" => {
            buildRestParam(argument, a, paramNamesWithTypes, body, buildFilterField(filterType))
          }
          case q"GridOptionColumn[$a, $b]($label, ${restParamName}, $argument => $body)" => {
            buildOptionalRestParam(argument, a, paramNamesWithTypes, body , restParamName, Like())
          }
          case q"GridOptionColumn[$a, $b]($label, $restParamName, $argument => $body, $filterType)" => {
            buildOptionalRestParam(argument, a, paramNamesWithTypes, body, restParamName, buildFilterField(filterType))
          }
          case _ => c.abort(c.enclosingPosition, "Cols elements are not specified properly. They should be blabla1")
        }
    }

    def buildRestParam(argument: Tree, tableType: Tree, paramNamesWithTypes: Map[String, Tree], body: Tree, filterType: FilterType): IndexedRestParam = {
      val argName = argument match{
        case q"$mods val $pat = $expr" => pat.toString
      }

      val argumentType = paramNamesWithTypes.getOrElse(argName, c.abort(c.enclosingPosition, s"function argument $argName is wrong. Possible names: ${paramNamesWithTypes.keys.mkString(", ")}"))
      val isTheSame = tableType.toString() == paramNamesWithTypes(argName).toString()

      isTheSame match{
        case true => IndexedRestParam(body.toString, body, filterType)
        case false => c.abort(c.enclosingPosition, s"$argName should be $argumentType and it is defined as ${tableType.toString}")
      }
    }

    def buildOptionalRestParam(argument: Tree, tableType: Tree, paramNamesWithTypes: Map[String, Tree], body: Tree, restParamName: Tree, filterType: FilterType): IndexedRestParam = {
      val argName = argument match{ case q"$mods val $pat = $expr" => pat.toString }

      val argumentType = paramNamesWithTypes.getOrElse(argName, c.abort(c.enclosingPosition, s"function argument $argName is wrong. Possible names: ${paramNamesWithTypes.keys.mkString(", ")}"))
      val isTheSame = "Rep[Option[" + tableType.toString() + "]]" ==  paramNamesWithTypes(argName).toString()

      isTheSame match{
        case true => IndexedRestParam(restParamName.toString().replaceAll("\"", ""), body, filterType)
        case false => c.abort(c.enclosingPosition, s"$argName should be $argumentType and it is defined as Rep[Option[${tableType.toString}]]. So probably you have to use GridColumn/DefaultGridColumn, not GridOptionColumn/GridDefaultOptionColumn for $argName")
      }
    }

    def extractDefaultFieldTrees(col: Tree, paramNamesWithTypes: Map[String, Tree]): IndexedRestParam = {
      col match {
        case q"DefaultGridColumn[$a, $b]($argument => $body)" => {
          buildRestParam(argument, a, paramNamesWithTypes, body, Like())
        }
        case q"DefaultGridOptionColumn[$a, $b]($argument => $body)" => {
          buildOptionalRestParam(argument, a, paramNamesWithTypes, body, Literal(Constant("")), Like())
        }
        case _ => c.abort(c.enclosingPosition, "defaultSortBy  is not specified properly")
      }
    }

    def extractColForFrontend(col: Tree): (Tree, String) = {
      col match {
        case q"GridColumn[..$tpes]($label, $table => $body)" => (label, body.toString())
        case q"GridColumn[..$tpes]($label, $table => $body, $filterType)" => (label, body.toString())
        case q"GridOptionColumn[..$tpes]($label,$restParamName ,$table => $body)" => (label, restParamName.toString)
        case q"GridOptionColumn[..$tpes]($label,$restParamName ,$table => $body, $filterType)" => (label, restParamName.toString)
        case _ => c.abort(c.enclosingPosition, "Cols elements are not specified properly.")
      }
    }

    def indexField(f: ValDef, elems: List[ValDef]) = {
      val fieldName = f.name
      val fieldType = c.typecheck(q"type T = ${f.tpt}") match {
        case x @ TypeDef(mods, typeName, tparams, rhs)  => rhs.tpe
      }
      val defaultValue = f.rhs
      val position = elems.indexWhere(f => f.name == fieldName)
      IndexedField(fieldName, fieldType, defaultValue, position)
    }

    val result = {
      annottees.map(_.tree).toList match {
        case q"case class $name(..$classParams) extends $extendParam[$firstType, $secondType, $thirdType] { ..$body }" :: Nil =>{
          val bodyOfValDefs = {
            if (body.length >= 3){
              body.take(3).map(_.asInstanceOf[ValDef])
            } else {
              c.abort(c.enclosingPosition, s"You have to specify 3 vals: query, columns and defaultSortBy")
            }
          }
          val bodyParams: List[IndexedField] = bodyOfValDefs.map{tree => indexField(tree, bodyOfValDefs)}

          val queryValue = bodyParams.find(_.name == TermName("query")).getOrElse(c.abort(c.enclosingPosition, "value query is not specified properly"))
          val queryYield = queryValue.defaultValue match {case q"""for(..$queryBody) yield (..$result)""" => result}

          val mapOfYieldParamsWithTypes: Map[QueryYieldParameter, Tree] = {
            val extractedTableTypes = firstType match{
              case multiple: AppliedTypeTree => multiple.args
              case single: Ident => List(single)
            }
            queryYield.map(_.toString).zip(extractedTableTypes).toMap
          }

          val listOfYieldNames = queryYield.map(element => TermName(element.toString))
          val tupleElements = listOfYieldNames.map(name => Bind(TermName(name.toString), Ident(termNames.WILDCARD)))
          val argumentOfPartialFunction = Apply(Select(Ident(TermName("scala")) , TermName("Tuple"+listOfYieldNames.length.toString)), tupleElements)

          def createCaseForSortBy(colName: Tree, dir: TermName) =
            listOfYieldNames match {
              case List(a) => cq"$a => $colName.$dir "
              case _ => cq"""$argumentOfPartialFunction => $colName.$dir"""
            }

          def createDefaultCaseForSortBy(colName: Tree) =
            listOfYieldNames match {
              case List(a) => cq"$a => $colName"
              case _ => cq"""$argumentOfPartialFunction => $colName"""
            }

          val colParam = bodyParams.find(_.name.toString == "columns").getOrElse(c.abort(c.enclosingPosition, "value columns is not specified properly"))
          val defaultSortByParam = bodyParams.find(_.name.toString == "defaultSortBy").getOrElse(c.abort(c.enclosingPosition, "value defaultSortBy is not specified properly"))

          val sortableFields = colParam.defaultValue match {
            case q"Seq(..${listOfCols})" => listOfCols.map(x => {
              extractFieldTrees(x, mapOfYieldParamsWithTypes)
            })
            case q"Seq($col)" => extractFieldTrees(col, mapOfYieldParamsWithTypes)
            case _ => c.abort(c.enclosingPosition, "Cols should be Seq[GridColumn]")
          }


          val colsForFrontend = (colParam.defaultValue match {
            case q"Seq(..$listOfCols)" => listOfCols.map(col => extractColForFrontend(col))
            case q"Seq($col)"          => extractColForFrontend(col)
            case _ => c.abort(c.enclosingPosition, "Columns should be Seq[GridColumn]")
          }).asInstanceOf[List[scala.Tuple2[Tree, String]]]

          val defaultSortByField = defaultSortByParam.defaultValue match {
            case q"$col" => extractDefaultFieldTrees(col, mapOfYieldParamsWithTypes)
            case _ => c.abort(c.enclosingPosition, "Problem with defaultSortBy")
          }
          lazy val defaultSortByCase = cq"""GridConditions(_, _, _, _, _) => query.sortBy{case ${createDefaultCaseForSortBy(defaultSortByField.value)} }"""
          lazy val cases = sortableFields.asInstanceOf[List[IndexedRestParam]].flatMap(colName =>
            cq"""GridConditions(_, _, ${colName.label}, "asc", _) => query.sortBy{case ${createCaseForSortBy(colName.value, TermName("asc"))}}"""
              :: cq"""GridConditions(_, _, ${colName.label}, "desc", _) => query.sortBy{case ${createCaseForSortBy(colName.value, TermName("desc"))}}"""
              :: Nil) ::: List(defaultSortByCase)

          lazy val filterOptions = sortableFields.asInstanceOf[List[IndexedRestParam]].flatMap(colName => {
              colName.filterType match {
                case Like() => q"""Option(filterBy.getOrElse(${colName.label}, null)).map(col => ${colName.value}.asColumnOf[String].toLowerCase.like("%" + col.toLowerCase + "%"))"""::Nil
                case Equals() =>q"""Option(filterBy.getOrElse(${colName.label}, null)).map(col => ${colName.value}.asColumnOf[String].toLowerCase === col.toLowerCase)"""::Nil
              }
            }
          )

          val caseForFilter = listOfYieldNames match{
            case List(a) => cq""" $a => $filterOptions.collect({case Some(criteria) => criteria}).reduceLeftOption(_ && _).getOrElse(true: Rep[Boolean])"""
            case _ => cq"""$argumentOfPartialFunction => $filterOptions.collect({case Some(criteria) => criteria}).reduceLeftOption(_ && _).getOrElse(true: Rep[Boolean])"""
          }

          val resultCode = q"""case class $name(..$classParams) extends $extendParam[$firstType, $secondType, $thirdType] {
              ..$body
              type MyQuery = slick.lifted.Query[$firstType, $secondType, $thirdType]

              def run(conditions: GridConditions)(implicit dbConnection: DBConnection) = {
                val filteredQuery = filter(query, conditions.filterBy.columns)
                val sorted = sortBy(conditions, filteredQuery).drop((conditions.page - 1) * conditions.perPage).take(conditions.perPage).result
                val count = countFiltered(conditions).result
                (dbConnection.db.run(sorted), dbConnection.db.run(count))
              }

              def run(conditions: GridConditions, initialFilter: ${firstType} => Rep[Boolean] )(implicit dbConnection: DBConnection) = {
                val initialFiltered = query.filter(initialFilter)
                val filteredQuery = filter(initialFiltered, conditions.filterBy.columns)
                val sorted = sortBy(conditions, filteredQuery).drop((conditions.page - 1) * conditions.perPage).take(conditions.perPage).result
                val count = countInitiallyFiltered(conditions, initialFiltered).result
                (dbConnection.db.run(sorted), dbConnection.db.run(count))
              }

              private def countInitiallyFiltered(conditions: GridConditions, filteredQuery: MyQuery)= {
                filter(filteredQuery, conditions.filterBy.columns).length
              }

              val colsForFrontend = ($colsForFrontend).map{case (label, name) => ColForFrontend(label, name)}

              private def countFiltered(conditions: GridConditions)= {
                filter(query, conditions.filterBy.columns).length
              }

              private def sortBy(conditions: GridConditions, query: MyQuery) = {
                 conditions match {
                    case ..$cases
                 }
              }

              private def filter(query: MyQuery, filterBy: Map[String, String]): MyQuery = {
                query.filter { case $caseForFilter }
              }
          }
            """
          println(resultCode)
          resultCode
        }
      }
    }
    c.Expr[Any](result)
  }
}