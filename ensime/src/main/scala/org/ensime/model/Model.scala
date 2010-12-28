package org.ensime.model
import scala.collection.mutable.{ HashMap, ArrayBuffer }
import scala.tools.nsc.interactive.{ Global, CompilerControl }
import scala.tools.nsc.symtab.{ Symbols, Types }
import scala.tools.nsc.util.{ NoPosition, Position }

abstract class EntityInfo(val name: String, val members: Iterable[EntityInfo]) {}

class PackageInfo(override val name: String, val fullname: String, override val members: Iterable[EntityInfo]) extends EntityInfo(name, members) {}

class SymbolInfo(
  val name: String,
  val declPos: Position,
  val tpe: TypeInfo,
  val isCallable: Boolean) {}

case class SymbolInfoLight(
  val name: String,
  val tpeSig: String,
  val tpeId: Int,
  val isCallable: Boolean) {}

case class ImportSuggestions(symLists: Iterable[Iterable[SymbolInfo]])

class NamedTypeMemberInfo(override val name: String, val tpe: TypeInfo, val pos: Position, val declaredAs: scala.Symbol) extends EntityInfo(name, List()) {}

class NamedTypeMemberInfoLight(override val name: String, val tpeSig: String, val tpeId: Int, val isCallable: Boolean) extends EntityInfo(name, List()) {}

class PackageMemberInfoLight(val name: String) {}

class TypeInfo(
  name: String,
  val id: Int,
  val declaredAs: scala.Symbol,
  val fullName: String,
  val args: Iterable[TypeInfo],
  members: Iterable[EntityInfo],
  val pos: Position,
  val outerTypeId: Option[Int]) extends EntityInfo(name, members) {}

class ArrowTypeInfo(
  override val name: String,
  override val id: Int,
  val resultType: TypeInfo,
  val paramSections: Iterable[ParamSectionInfo]) extends TypeInfo(name, id, 'nil, name, List(), List(), NoPosition, None) {}

class CallCompletionInfo(
  val resultType: TypeInfo,
  val paramSections: Iterable[ParamSectionInfo]) {}

class ParamSectionInfo(
  val params: Iterable[(String, TypeInfo)],
  val isImplicit: Boolean)

class InterfaceInfo(val tpe: TypeInfo, val viaView: Option[String]) {}

class TypeInspectInfo(val tpe: TypeInfo, val companionId: Option[Int], val supers: Iterable[InterfaceInfo]) {}

trait ModelBuilders { self: Global =>

  import self._
  import definitions.{ ObjectClass, ScalaObjectClass, RootPackage, EmptyPackage, NothingClass, AnyClass, AnyRefClass }

  private val typeCache = new HashMap[Int, Type]
  private val typeCacheReverse = new HashMap[Type, Int]

  def clearTypeCache() {
    typeCache.clear
    typeCacheReverse.clear
  }
  def typeById(id: Int): Option[Type] = {
    typeCache.get(id)
  }
  def cacheType(tpe: Type): Int = {
    if (typeCacheReverse.contains(tpe)) {
      typeCacheReverse(tpe)
    } else {
      val id = typeCache.size + 1
      typeCache(id) = tpe
      typeCacheReverse(tpe) = id
      id
    }
  }

  object Helpers {

    import scala.tools.nsc.symtab.Flags._

    /* See source at root/scala/trunk/src/compiler/scala/tools/nsc/symtab/Symbols.scala  
    for details on various symbol predicates. */
    def declaredAs(sym: Symbol): scala.Symbol = {
      if (sym.isMethod)
      'method
      else if (sym.isTrait)
      'trait
      else if (sym.isTrait && sym.hasFlag(JAVA))
      'interface
      else if (sym.isInterface)
      'interface
      else if (sym.isModule)
      'object
      else if (sym.isModuleClass)
      'object
      else if (sym.isClass)
      'class
      else if (sym.isPackageClass)
      'class

      // check this last so objects are not
      // classified as fields
      else if (sym.isValue || sym.isVariable)
      'field
      else 'nil
    }

    def isArrowType(tpe: Type) = {
      tpe match {
        case _: MethodType => true
        case _: PolyType => true
        case _ => false
      }
    }

    def isNoParamArrowType(tpe: Type) = {
      tpe match {
        case t: MethodType => t.paramss.flatten.isEmpty
        case t: PolyType => t.paramss.flatten.isEmpty
        case t: Type => false
      }
    }

    def typeOrArrowTypeResult(tpe: Type) = {
      tpe match {
        case t: MethodType => t.finalResultType
        case t: PolyType => t.finalResultType
        case t: Type => t
      }
    }

    /**
    * Convenience method to generate a String describing the type. Omit
    * the package name. Include the arguments postfix.
    * 
    * Used for type-names of symbol and member completions
    */
    def typeShortNameWithArgs(tpe: Type): String = {
      if (isArrowType(tpe)) {
        (tpe.paramss.map { sect =>
            "(" +
            sect.map { p => typeShortNameWithArgs(p.tpe) }.mkString(", ") +
            ")"
          }.mkString(" => ")
          + " => " +
          typeShortNameWithArgs(tpe.finalResultType))
      } else {
        (typeShortName(tpe) + (if (tpe.typeArgs.length > 0) {
              "[" +
              tpe.typeArgs.map(typeShortNameWithArgs).mkString(", ") +
              "]"
            } else { "" }))
      }
    }

    /** 
    * Generate qualified name, without args postfix.
    */
    def typeFullName(tpe: Type): String = {
      def nestedClassName(sym: Symbol): String = {
        outerClass(sym) match {
          case Some(outerSym) => {
            nestedClassName(outerSym) + "$" + typeShortName(sym)
          }
          case None => typeShortName(sym)
        }
      }
      val typeSym = tpe.typeSymbol
      if (typeSym.isNestedClass) {
        typeSym.enclosingPackage.fullName + "." + nestedClassName(typeSym)
      } else {
        typeSym.enclosingPackage.fullName + "." + typeShortName(typeSym)
      }
    }

    def typeShortName(tpe: Type): String = {
      if (tpe.typeSymbol != NoSymbol) typeShortName(tpe.typeSymbol)
      else tpe.toString
    }

    def typeShortName(sym: Symbol): String = {
      if (sym.isModule || sym.isModuleClass) sym.nameString + "$"
      else sym.nameString
    }

    /* Give the outerClass of a symbol representing a nested type */
    def outerClass(typeSym: Symbol): Option[Symbol] = {
      try {
        if (typeSym.isNestedClass) {
          Some(typeSym.outerClass)
        } else None
      } catch {
        // TODO accessing outerClass sometimes throws java.lang.Error
        // Notably, when tpe = scala.Predef$Class
        case e: java.lang.Error => None
      }
    }

    def companionTypeOf(tpe: Type): Option[Type] = {
      val sym = tpe.typeSymbol
      if (sym != NoSymbol) {
        if (sym.isModule || sym.isModuleClass) {
          val comp = sym.companionClass
          if (comp != NoSymbol && comp.tpe != tpe) {
            Some(comp.tpe)
          } else None
        } else if (sym.isTrait || sym.isClass || sym.isPackageClass) {
          val comp = sym.companionModule
          if (comp != NoSymbol && comp.tpe != tpe) {
            Some(comp.tpe)
          } else None
        } else None
      } else None
    }

    // When inspecting a type, transform a raw list of TypeMembers to a sorted
    // list of InterfaceInfo objects, each with its own list of sorted member infos.
    def prepareSortedInterfaceInfo(members: Iterable[Member]): Iterable[InterfaceInfo] = {
      // ...filtering out non-visible and non-type members
      val visMembers: Iterable[TypeMember] = members.flatMap {
        case m@TypeMember(sym, tpe, true, _, _) => List(m)
        case _ => List()
      }

      // Create a list of pairs [(typeSym, membersOfSym)]
      val membersByOwner = visMembers.groupBy {
        case TypeMember(sym, _, _, _, _) => {
          sym.owner
        }
      }.toList.sortWith {
        // Sort the pairs on the subtype relation
        case ((s1, _), (s2, _)) => s1.tpe <:< s2.tpe
      }

      membersByOwner.map {
        case (ownerSym, members) => {

          // If all the members in this interface were
          // provided by the same view, remember that 
          // view for later display to user.
          val byView = members.groupBy(_.viaView)
          val viaView = if (byView.size == 1) {
            byView.keys.headOption.filter(_ != NoSymbol)
          } else { None }

          // Do one top level sort by name on members, before
          // subdividing into kinds of members.
          val sortedMembers = members.toList.sortWith { (a, b) =>
            a.sym.nameString <= b.sym.nameString
          }

          // Convert type members into NamedTypeMemberInfos
          // and divided into different kinds..

          val nestedTypes = new ArrayBuffer[NamedTypeMemberInfo]()
          val constructors = new ArrayBuffer[NamedTypeMemberInfo]()
          val fields = new ArrayBuffer[NamedTypeMemberInfo]()
          val methods = new ArrayBuffer[NamedTypeMemberInfo]()

          for (tm <- sortedMembers) {
            val info = NamedTypeMemberInfo(tm)
            val decl = info.declaredAs
            if (decl == 'method) {
              if (info.name == "this") {
                constructors += info
              } else {
                methods += info
              }
            } else if (decl == 'field) {
              fields += info
            } else if (decl == 'class || decl == 'trait ||
              decl == 'interface || decl == 'object) {
              nestedTypes += info
            }
          }

          val sortedInfos = nestedTypes ++ fields ++ constructors ++ methods

          new InterfaceInfo(TypeInfo(ownerSym.tpe, sortedInfos),
            viaView.map(_.name.toString))
        }
      }
    }

    def packageSymFromPath(path: String): Option[Symbol] = {
      val candidates = symsAtQualifiedPath(path, RootPackage)
      candidates.find { s => s.isPackage }
    }

    // Where path is the qualified name of a symbol that is a direct or
    // indirect member of rootSym, without containing the name of rootSym.
    def symsAtQualifiedPath(path: String, rootSym: Symbol): List[Symbol] = {
      def memberSymsNamed(sym: Symbol, name: String) = {
        (sym.info.members ++ sym.info.decls).filter { s =>
          s.nameString == name && s != EmptyPackage && s != RootPackage
        }
      }
      if (path == "") List(rootSym)
      else {
        val pathSegs = path.split("\\.")
        pathSegs.foldLeft(List(rootSym)) { (baseSyms, seg) =>
          baseSyms.flatMap { s => memberSymsNamed(s, seg) }
        }
      }
    }

    /*
    * Get the valid member symbols of the package denoted by aSym.
    */
    def packageMembers(parent: Symbol): Iterable[Symbol] = {

      def isRoot(s: Symbol) = s.isRoot || s.isRootPackage

      def filterAndSort(symbols: Iterable[Symbol]) = {
        val validSyms = symbols.filter { s =>
          s != EmptyPackage && !isRoot(s) &&
          // This check is necessary to prevent infinite looping..
          ((isRoot(s.owner) && isRoot(parent)) || (s.owner.fullName == parent.fullName))
        }
        validSyms.toList.sortWith { (a, b) => a.nameString <= b.nameString }
      }

      if (isRoot(parent)) {
        filterAndSort(parent.info.members ++ EmptyPackage.info.members)
      } else {
        filterAndSort(parent.info.members)
      }
    }

  }

  import Helpers._

  object PackageInfo {

    def root: PackageInfo = fromSymbol(RootPackage)

    def fromPath(path: String): PackageInfo = {
      val pack = packageSymFromPath(path)
      pack match {
        case Some(packSym) => fromSymbol(packSym)
        case None => nullInfo
      }
    }

    def nullInfo = {
      new PackageInfo("NA", "NA", List())
    }

    def fromSymbol(sym: Symbol): PackageInfo = {
      if (sym.isRoot || sym.isRootPackage) {
        new PackageInfo(
          "root",
          "_root_",
          packageMembers(sym).flatMap(packageMemberInfoFromSym)
        )
      } else {
        new PackageInfo(
          sym.name.toString,
          sym.fullName,
          packageMembers(sym).flatMap(packageMemberInfoFromSym)
        )
      }
    }

    def packageMemberInfoFromSym(sym: Symbol): Option[EntityInfo] = {
      try {
        if (sym == RootPackage) {
          Some(root)
        } else if (sym.isPackage) {
          Some(fromSymbol(sym))
        } else if (!(sym.nameString.contains("$")) && (sym != NoSymbol) && (sym.tpe != NoType)) {
          if (sym.isClass || sym.isTrait || sym.isModule ||
            sym.isModuleClass || sym.isPackageClass) {
            Some(TypeInfo(sym.tpe))
          } else {
            None
          }
        } else {
          None
        }
      } catch {
        case e => None
      }
    }
  }

  object TypeInfo {

    def apply(t: Type, members: Iterable[EntityInfo] = List()): TypeInfo = {
      val tpe = t match {
        // TODO: Instead of throwing away this information, would be better to 
        // alert the user that the type is existentially quantified.
        case et: ExistentialType => et.underlying
        case t => t
      }
      tpe match {
        case tpe: MethodType => ArrowTypeInfo(tpe)
        case tpe: PolyType => ArrowTypeInfo(tpe)
        case tpe: Type =>
        {
          val args = tpe.typeArgs.map(TypeInfo(_))
          val typeSym = tpe.typeSymbol
          val outerTypeId = outerClass(typeSym).map(s => cacheType(s.tpe))
          new TypeInfo(
            typeShortName(tpe),
            cacheType(tpe),
            declaredAs(typeSym),
            typeFullName(tpe),
            args,
            members,
            typeSym.pos,
            outerTypeId
          )
        }
        case _ => nullInfo
      }
    }

    def nullInfo() = {
      new TypeInfo("NA", -1, 'nil, "NA", List(), List(), NoPosition, None)
    }
  }


  object ParamSectionInfo{
    def apply(params: Iterable[Symbol]):ParamSectionInfo = {
      new ParamSectionInfo(params.map{s => (s.nameString, TypeInfo(s.tpe))}, 
	params.forall{s => s.isImplicit})
    }
  }

  object CallCompletionInfo {

    def apply(tpe: Type): CallCompletionInfo = {
      tpe match {
        case tpe: MethodType => apply(tpe.paramss.map(ParamSectionInfo.apply), tpe.finalResultType)
        case tpe: PolyType => apply(tpe.paramss.map(ParamSectionInfo.apply), tpe.finalResultType)
        case _ => nullInfo
      }
    }

    def apply(paramSections: List[ParamSectionInfo], finalResultType: Type): CallCompletionInfo = {
      new CallCompletionInfo(
        TypeInfo(finalResultType),
        paramSections
      )
    }

    def nullInfo() = {
      new CallCompletionInfo(TypeInfo.nullInfo, List())
    }
  }

  object SymbolInfo {

    def apply(sym: Symbol): SymbolInfo = {
      new SymbolInfo(
        sym.name.toString,
        sym.pos,
        TypeInfo(sym.tpe),
        Helpers.isArrowType(sym.tpe)
      )
    }

    def nullInfo() = {
      new SymbolInfo("NA", NoPosition, TypeInfo.nullInfo, false)
    }

  }

  object SymbolInfoLight {

    /** 
    *  Return symbol infos for any like-named constructors.
    */
    def constructorSynonyms(sym: Symbol): List[SymbolInfoLight] = {
      val members = if (sym.isClass || sym.isPackageClass) {
        sym.tpe.members
      } else if (sym.isModule || sym.isModuleClass) {
        sym.companionClass.tpe.members
      } else { List() }

      members.flatMap { member: Symbol =>
        if (member.isConstructor) {
          Some(SymbolInfoLight(sym, member.tpe))
        } else { None }
      }
    }

    /** 
    *  Return symbol infos for any like-named apply methods.
    */
    def applySynonyms(sym: Symbol): List[SymbolInfoLight] = {
      val members = if (sym.isModule || sym.isModuleClass) {
        sym.tpe.members
      } else if (sym.isClass || sym.isPackageClass) {
        sym.companionModule.tpe.members
      } else { List() }
      members.flatMap { member: Symbol =>
        if (member.name.toString == "apply") {
          Some(SymbolInfoLight(sym, member.tpe))
        } else { None }
      }
    }

    def apply(sym: Symbol): SymbolInfoLight = SymbolInfoLight(sym, sym.tpe)

    def apply(sym: Symbol, tpe: Type): SymbolInfoLight = {
      new SymbolInfoLight(
        sym.nameString,
        typeShortNameWithArgs(tpe),
        cacheType(tpe.underlying),
        Helpers.isArrowType(tpe.underlying)
      )
    }

    def nullInfo() = {
      new SymbolInfoLight("NA", "NA", -1, false)
    }
  }

  object NamedTypeMemberInfo {
    def apply(m: TypeMember): NamedTypeMemberInfo = {
      val decl = declaredAs(m.sym)
      new NamedTypeMemberInfo(m.sym.nameString, TypeInfo(m.tpe), m.sym.pos, decl)
    }
  }

  object NamedTypeMemberInfoLight {
    def apply(m: TypeMember): NamedTypeMemberInfoLight = {
      new NamedTypeMemberInfoLight(m.sym.nameString,
        typeShortNameWithArgs(m.tpe),
        cacheType(m.tpe),
        isArrowType(m.tpe))
    }
  }

  object ArrowTypeInfo {

    def apply(tpe: Type): ArrowTypeInfo = {
      tpe match {
        case tpe: MethodType => apply(tpe, tpe.paramss.map(ParamSectionInfo.apply), tpe.finalResultType)
        case tpe: PolyType => apply(tpe, tpe.paramss.map(ParamSectionInfo.apply), tpe.finalResultType)
        case _ => nullInfo
      }
    }

    def apply(tpe: Type, paramSections: List[ParamSectionInfo], finalResultType: Type): ArrowTypeInfo = {
      new ArrowTypeInfo(
        tpe.toString,
        cacheType(tpe),
        TypeInfo(tpe.finalResultType),
        paramSections)
      }

      def nullInfo() = {
	new ArrowTypeInfo("NA", -1, TypeInfo.nullInfo, List())
      }
    }

    object TypeInspectInfo {
      def nullInfo() = {
	new TypeInspectInfo(TypeInfo.nullInfo(), None, List())
      }
    }

  }

