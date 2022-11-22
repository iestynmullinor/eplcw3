/* --------------------------------------------------------------------------------------- *
 * --------------------------------------------------------------------------------------- *
 *                            EPL ASSIGNMENT 3 - VERSION 1.4                              *
 * --------------------------------------------------------------------------------------- *
 * --------------------------------------------------------------------------------------- */

package Assignment3.RabbitStandalone
import Assignment3.RabbitParser.RabbitParser
import Assignment3.RabbitSyntax.Syntax._

import scala.collection.immutable.ListMap

import java.io.{FileWriter, File}

object Assignment3Standalone {

  /****************
   *  Exercise 2  *
   ****************/
  def isSimpleType(ty: Type): Boolean = ty match {
    case SignalTy(_) => false
    case FunTy(a, b) => isSimpleType(a) && isSimpleType(b)
    case PairTy(a, b) => isSimpleType(a) && isSimpleType(b)
    case ListTy(a) => isSimpleType(a)
    case _ => true
  }
  // ----------------------------------------------------------------
  // Typechecker
  def valueTy(v: Value): Type = v match {
    case UnitV => UnitTy
    case IntV(_) => IntTy
    case BoolV(_) => BoolTy
    case StringV(_) => StringTy
    case ListV(_) => sys.error("Impossible case: ListTy(xs) only introduced at runtime")
    case PairV(_, _) => sys.error("Impossible case: PairV is only introduced at runtime")
    case FunV(_, _, _) => sys.error("Impossible case: FunV is only introduced at runtime")
    case RecV(_, _, _, _, _) => sys.error("Impossible case: FunV is only introduced at runtime")
    case _ => sys.error("Impossible case: signal values are is only introduced at runtime")
  }
  // typing: calculate the return type of e, or throw an error
  def tyOf(ctx: Env[Type], e: Expr): Type = {
    e match {
      // Values
      case v: Value => valueTy(v)
      // BEGIN ANSWER
      //arithmetic expressions
      
      case Plus(e1,e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
        case (IntTy, IntTy) => IntTy
        case _ => sys.error("non-integer arguments to +") 
    }

      case Minus(e1,e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
        case (IntTy, IntTy) => IntTy
        case _ => sys.error("non-integer arguments to -") 
    }

      case Times(e1,e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
        case (IntTy, IntTy) => IntTy
        case _ => sys.error("non-integer arguments to *") 
    }

      case Div(e1,e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
        case (IntTy, IntTy) => IntTy
        case _ => sys.error("non-integer arguments to /") 
    }

    //booleans

      case Eq(e1,e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
        case (a, b) => if (a == b) {
        BoolTy
      } else {
        sys.error("types of eq must be equal")
      }
    }

      case LessThan(e1,e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
        case (a, b) => if (a == b) {
        BoolTy
      } else {
        sys.error("types of less than must be equal")
      }
    }

      case GreaterThan(e1,e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
        case (a, b) => if (a == b) {
        BoolTy
      } else {
        sys.error("types of greater than must be equal")
      }
    }

      case IfThenElse(e,e1,e2) =>
      (tyOf(ctx,e),tyOf(ctx,e1),tyOf(ctx,e2)) match {
        case (BoolTy,a,b) => if (a == b) {
          a
        }
        else {
          sys.error("types of branches must be equal")
        }
        case (_,a,b) => sys.error("type of conditional must be boolean")
      }

    //variables and let bindings

      case Var(x) => ctx(x)

      case Let(x,e1,e2) => tyOf(ctx + (x -> (tyOf(ctx,e1))), e2)

      case LetPair(x,y,e1,e2) => tyOf(ctx, e1) match {
        case PairTy(a,b) => tyOf(ctx + (x -> a) + (y -> b), e2)
        case _ => sys.error("Let pair's first argument must be a pair")
    }

      case LetFun(f,x,ty,e1,e2) => {
        val ty2 = tyOf(ctx + (x -> ty), e1);
        tyOf(ctx + (f -> FunTy(ty,ty2)), e2)
    }

      case LetRec(f,x,ty1,ty2,e1,e2) => {
        val fty = FunTy(ty1,ty2);
        if (tyOf(ctx + (x -> ty1) + (f -> fty), e1) == ty2) {
          tyOf(ctx + (f -> fty), e2)
        }else {
          sys.error("Type of recursive function does not match specification")
        } 
    }

    //pairs
    case Pair(e1,e2) => PairTy(tyOf(ctx,e1),tyOf(ctx,e2))

    case Fst(e) => tyOf(ctx,e) match {
      case PairTy(a,b) => a
      case _ => sys.error("First must be applied to a pair")
    }

    case Snd(e) => tyOf(ctx,e) match {
      case PairTy(a,b) => b
      case _ => sys.error("Second must be applied to a pair")
    }

    //functions

    case Lambda(x,ty,e) => FunTy(ty,tyOf(ctx + (x -> ty),e))

    case Rec(f,x,xty,ty,e) => tyOf(ctx + (f -> FunTy(xty,ty)) + (x -> xty),e) match {
      case body => if (ty == body) {
        FunTy(xty,ty)
      } else {
        sys.error("Function body type does not match that specified")
      }
    }

    case App(e1,e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
      case (FunTy(a,b),c) => if (a == c) {
        b
      } else {
        sys.error("Argument type does not match fuction input")
      }
      case (a,c) => sys.error("Function type not found!\n" +
          "Typing " + e1.toString + "type is: " + a.toString + "\n" +
          "Typing " + e2.toString + "type is: " + c.toString + "\n")

    }

    //lists
    //emptylist
    case EmptyList(ty) => ListTy(ty)

    //cons
    case Cons(e1,e2) => (tyOf(ctx, e1), tyOf(ctx, e2)) match {
      case (a,b) => if (ListTy(a) == b) {b} else {sys.error("Argument to add to list does not match type of list")}
    }

    //listcase (VERY WRONG) TODO!!!
    case ListCase(l,e1,x,y,e2) => {
      var sigma1 = tyOf(ctx, e1)
      var listSigma1 = tyOf(ctx, l)
      var sigma2 = tyOf(ctx,e2)
      return tyOf(ctx + (x -> (sigma1)) + (y -> listSigma1), e1)
    }


    //sequencing 
    case Seq(e1,e2) => tyOf(ctx,e1) match { 
      case  UnitTy => tyOf(ctx, e2)
      case _ => sys.error("must be unit for e1 in Seq")
    }

    //signals
    //time
    case Time => SignalTy(IntTy) //should be SignalTy(IntTy)

    //pure
    case Pure(e) => if (isSimpleType(tyOf(ctx, e))) {SignalTy(tyOf(ctx,e))} else {sys.error("type of expression must be simple type")}  //need to make sure e is simple

    //apply
    case Apply(e1,e2) => {

      (tyOf(ctx,e1),tyOf(ctx,e2)) match {
      case (SignalTy(FunTy(a,b)),SignalTy(c)) => if (a == c) {
        SignalTy(b)
      } else {
        sys.error("Argument type does not match fuction input")
      }
      case (a,c) => sys.error("Function type not found!\n" +
          "Typing " + e1.toString + "type is: " + a.toString + "\n" +
          "Typing " + e2.toString + "type is: " + c.toString + "\n") 

    } } 

    //read
    case Read(e) => tyOf(ctx, e) match {
      case StringTy => SignalTy(FrameTy)
      case _ => sys.error("argument is not string")
    }

    //moveXY
    case MoveXY(x,y,a) => (tyOf(ctx, x), tyOf(ctx,y), tyOf(ctx, a)) match {
      case (SignalTy(IntTy), SignalTy(IntTy), SignalTy(FrameTy)) => SignalTy(FrameTy)
      case _ => sys.error("x and y must be signal(int), a must be signal(frame): " + e.toString + "   for this case, the 3 types are: " + tyOf(ctx,x).toString + " " + tyOf(ctx,y).toString + " " + tyOf(ctx,a).toString)
    }

    //blank
    case Blank => SignalTy(FrameTy)

    //over
    case Over(e1,e2) => (tyOf(ctx, e1), tyOf(ctx, e2)) match {
      case (SignalTy(FrameTy), SignalTy(FrameTy)) => SignalTy(FrameTy)
      case _ => sys.error("arguments not both of type signal[frame]")
    }

    //when
    case When(e1,e2,e3) => (tyOf(ctx,e1), tyOf(ctx,e2), tyOf(ctx,e3)) match {
      case (SignalTy(BoolTy),SignalTy(tau1),SignalTy(tau2)) => if (tau1 == tau2) {SignalTy(tau1)} else {sys.error("e2 and e3 must both have same type")}
      case _ => sys.error("e1 must be Signal[Boolean]")
    }


    //SignalBlock
    case SignalBlock(se) => SignalTy(tyOfSignal(ctx,se)) //should be SignalTy(tyOfSignal(ctx,se))

      case _ => sys.error("todo typechecker ex2")
      // END ANSWER
    }
  }

  /****************
   *  Exercise 3  *
   ****************/
  def tyOfSignal(ctx: Env[Type], e: Expr): Type = {    

    e match {
      // Values
      case v: Value => valueTy(v)
      // BEGIN ANSWER

      //arithmetic expressions

      case Plus(se1,se2) => (tyOfSignal(ctx,se1),tyOfSignal(ctx,se2)) match {
        case (IntTy, IntTy) => IntTy
        case _ => sys.error("non-integer arguments to +") 
    }

      case Minus(se1,se2) => (tyOfSignal(ctx,se1),tyOfSignal(ctx,se2)) match {
        case (IntTy, IntTy) => IntTy
        case _ => sys.error("non-integer arguments to -") 
    }

      case Times(se1,se2) => (tyOfSignal(ctx,se1),tyOfSignal(ctx,se2)) match {
        case (IntTy, IntTy) => IntTy
        case _ => sys.error("non-integer arguments to *") 
    }

      case Div(se1,se2) => (tyOfSignal(ctx,se1),tyOfSignal(ctx,se2)) match {
        case (IntTy, IntTy) => IntTy
        case _ => sys.error("non-integer arguments to /") 
    }


    //booleans

    case Eq(se1,se2) => (tyOfSignal(ctx,se1),tyOfSignal(ctx,se2)) match {
        case (a, b) => if ((a == b) && ((a == IntTy) || (a == BoolTy))) {
        BoolTy
      } else {
        sys.error("types of eq must be equal")
      }
    }

      case LessThan(se1,se2) => (tyOfSignal(ctx,se1),tyOfSignal(ctx,se2)) match {
        case (a, b) => if (a == b) {
        BoolTy
      } else {
        sys.error("types of less than must be equal")
      }
    }

      case GreaterThan(se1,se2) => (tyOfSignal(ctx,se1),tyOfSignal(ctx,se2)) match {
        case (a, b) => if (a == b) {
        BoolTy
      } else {
        sys.error("types of greater than must be equal")
      }
    }

      case IfThenElse(se,se1,se2) =>
      (tyOf(ctx,se),tyOfSignal(ctx,se1),tyOfSignal(ctx,se2)) match {
        case (BoolTy,a,b) => if (a == b) {
          a
        }
        else {
          sys.error("types of branches must be equal")
        }
        case (_,a,b) => sys.error("type of conditional must be boolean")
      }

      //strings and rest
      //the weierd none thats after string (PROBABLY VIOLENTLY WRONG)
      //case x : Expr => if (isSimpleType(tyOf(ctx,x))) {tyOfSignal(ctx,x)} else {sys.error("Must be simple type")}

      //app
      case App(se1,se2) => (tyOfSignal(ctx,se1),tyOfSignal(ctx,se2)) match {
        case (FunTy(a,b),c) => if (a == c) {
        b
        } else {
        sys.error("Argument type does not match fuction input")
        }
        case (a,c) => sys.error("Function type not found!\n" +
          "Typing " + se1.toString + "type is: " + a.toString + "\n" +
          "Typing " + se2.toString + "type is: " + c.toString + "\n")

      }

      //time 
      case Time => IntTy

      //blank
      case Blank => FrameTy

      //over
      case Over(se1,se2) => (tyOfSignal(ctx, se1), tyOfSignal(ctx, se2)) match {
        case (FrameTy, FrameTy) => FrameTy
        case _ => sys.error("arguments not both of type frame")
    }

      //moveXY
      case MoveXY(x,y,a) => (tyOfSignal(ctx, x), tyOfSignal(ctx,y), tyOfSignal(ctx, a)) match {
        case (IntTy, IntTy, FrameTy) => FrameTy
        case _ => sys.error("x and y must be int, a must be frame")
    }

    //read
      case Read(e) => tyOf(ctx, e) match {
        case StringTy => FrameTy
        case _ => sys.error("argument is not string")
    }

    //escape
      case Escape(e) => tyOf(ctx, e) match {
        case SignalTy(tau) => tau
        case _ => sys.error("Must be signal")
        
      }

    //get rid of this it is only here for  a test \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    //  case When(e1,e2,e3) => (tyOfSignal(ctx,e1), tyOfSignal(ctx,e2), tyOfSignal(ctx,e3)) match {
    //  case (BoolTy,tau1,tau2) => if (tau1 == tau2) {tau1} else {sys.error("e2 and e3 must both have same type")}
    //  case _ => sys.error("e1 must be Signal[Boolean]")
    //}

      //idk
      case Var(x) => if (isSimpleType(ctx(x))) {ctx(x)} else {sys.error("must be simple type")}


      case _ => {
        sys.error("todo signal typechecker ex3 this is the case " + e.toString)}
      // END ANSWER
    }
  }

  // ----------------------------------------------------------------
  // Swapping (provided)
  def swap(e: Expr, y: Variable, z: Variable): Expr = {
    def swapVar(x: Variable, y: Variable, z: Variable): Variable =
      if (x == y) {
        z
      } else if (x == z) {
        y
      } else {
        x
      }

    def go(e: Expr): Expr = e match {
      // Values are closed
      case v: Value => v

      // Arithmetic expressions
      case Plus(t1,t2) => Plus(go(t1),go(t2))
      case Minus(t1,t2) => Minus(go(t1),go(t2))
      case Times(t1,t2) => Times(go(t1),go(t2))
      case Div(t1,t2) => Div(go(t1),go(t2))

      // Booleans
      case Eq(t1,t2) => Eq(go(t1),go(t2))
      case IfThenElse(t,t1,t2) => IfThenElse(go(t),go(t1),go(t2))
      case GreaterThan(t1, t2) => GreaterThan(go(t1), go(t2))
      case LessThan(t1, t2) => LessThan(go(t1), go(t2))

      // Variables and let-binding
      case Var(x) => Var(swapVar(x,y,z))
      case Let(x,t1,t2) => Let(swapVar(x,y,z),go(t1),go(t2))
      case LetFun(f,x,ty,t1,t2) => LetFun(swapVar(f,y,z),swapVar(x,y,z),ty,go(t1),go(t2))
      case LetRec(f,x,xty,ty,t1,t2) => LetRec(swapVar(f,y,z),swapVar(x,y,z),xty,ty,go(t1),go(t2))
      case LetPair(x1, x2, t1, t2) =>
        LetPair(swapVar(x1, y, z), swapVar(x2, y, z), go(t1), go(t2))
      
      // Pairs
      case Pair(t1, t2) => Pair(go(t1), go(t2))
      case Fst(t) => Fst(go(t))
      case Snd(t) => Snd(go(t))

      // Functions
      case Lambda(x,ty,t) => Lambda(swapVar(x,y,z),ty,go(t))
      case App(t1,t2) => App(go(t1),go(t2))
      case Rec(f,x,xty,ty,t) => Rec(swapVar(f,y,z), swapVar(x,y,z), xty,ty,go(t))

      // Lists
      case EmptyList(ty) => EmptyList(ty)
      case Cons(t1, t2) => Cons(go(t1), go(t2))
      case ListCase(l, t1, consVar1, consVar2, t2) =>
        ListCase(go(l), go(t1), swapVar(consVar1, y, z), swapVar(consVar2, y, z), go(t2))

      // Sequencing
      case Seq(t1,t2) => Seq(go(t1),go(t2))

      // Signals
      case Time => Time
      case Pure(t) => Pure(go(t))
      case Apply(t1, t2) => Apply(go(t1), go(t2))
      case Read(t) => Read(go(t))
      case MoveXY(x, y, a) => MoveXY(go(x), go(y), go(a))
      case Blank => Blank
      case Over(t1,t2) => Over(go(t1),go(t2))
      case When(t1,t2,t3) => When(go(t1),go(t2),go(t3))
      case SignalBlock(t) => SignalBlock(go(t))
      case Escape(t) => Escape(go(t))

      }
    go(e)
  }

    /****************
   *  Exercise 4  *
   ****************/


  // ----------------------------------------------------------------
  // Substitution e1 [e2 / x]
  def subst(e1:Expr, e2:Expr, x: Variable): Expr = {

    e1 match {
      // Values are closed so substitution has no effect
      case v: Value => v
      // BEGIN ANSWER

      //arithmetic
      //case Num(e) => Num(e)
      case Plus(t1,t2) => Plus(subst(t1,e2,x),subst(t2,e2,x))
      case Minus(t1,t2) => Minus(subst(t1,e2,x),subst(t2,e2,x))
      case Times(t1,t2) => Times(subst(t1,e2,x),subst(t2,e2,x))
      case Div(t1,t2) => Div(subst(t1,e2,x),subst(t2,e2,x))

      //Booleans 
      case Eq(t1,t2) => Eq(subst(t1,e2,x),subst(t2,e2,x))
      case IfThenElse(t0,t1,t2) => IfThenElse(subst(t0,e2,x),subst(t1,e2,x),subst(t2,e2,x))
      case GreaterThan(t1,t2) => GreaterThan(subst(t1,e2,x),subst(t2,e2,x))
      case LessThan(t1,t2) => LessThan(subst(t1,e2,x),subst(t2,e2,x))

      //variables and let binding
      case Var(y) =>
        if (x == y) {
          e2
        } else {
          Var(y)
        }

      case Let(y,t1,t2) => {
        val z = Gensym.gensym(y);
        Let(z,subst(t1,e2,x),subst(swap(t2,y,z),e2,x))
      }

      case LetPair(y1,y2,t1,t2) => {
        val y1z = Gensym.gensym(y1);
        val y2z = Gensym.gensym(y2);
        LetPair(y1z,y2z,subst(t1,e2,x),
          subst(swap(swap(t2,y1z,y1), y2z, y2), e2,x))
      }

      case LetFun(f,y,ty,t1,t2) => {
        val fz = Gensym.gensym(f);
        val yz = Gensym.gensym(y);
        LetFun(fz,yz,ty,subst(swap(t1,yz,y),e2,x),
          subst(swap(t2,fz,f), e2,x))
      }

      case LetRec(f,y,ty1,ty2,t1,t2) => {
        val fz = Gensym.gensym(f);
        val yz = Gensym.gensym(y);
        LetRec(fz,yz,ty1,ty2,subst(swap(swap(t1,fz,f),yz,y),e2,x),
          subst(swap(t2,fz,f), e2,x))
      }

      //pairs
      case Pair(t1,t2) => Pair(subst(t1,e2,x),subst(t2,e2,x))
      case Fst(t0) => Fst(subst(t0,e2,x))
      case Snd(t0) => Snd(subst(t0,e2,x))

      //functiona
      case Lambda(y,ty,t0) => {
        val z = Gensym.gensym(y);
        Lambda(z,ty,subst(swap(t0,y,z),e2,x))
      }
      case App(t1,t2) => App(subst(t1,e2,x),subst(t2,e2,x))
      case Rec(f,y,ty1,ty2,t0) => { 
        val g = Gensym.gensym(f);
        val z = Gensym.gensym(y);
        Rec(g,z,ty1,ty2,subst(swap(swap(t0,f,g),y,z),e2,x))
      }

      // Lists
      case EmptyList(ty) => EmptyList(ty)

      case Cons(t1, t2) => Cons(subst(t1,e2,x),subst(t2,e2,x))

      case ListCase(l, t1, consVar1, consVar2, t2) => {
        val consVar1Z = Gensym.gensym(consVar1);
        val consVar2Z = Gensym.gensym(consVar2);
        ListCase(subst(l,e2,x),subst(t1,e2,x),consVar1Z,consVar2Z,subst(t2,e2,x))
      }



      // Sequencing 
      case Seq(t1,t2) => Seq(subst(t1,e2,x), subst(t2,e2,x))


      // Signals
      case Time => Time

      case Pure(t) => Pure(subst(t,e2,x))

      case Apply(t1, t2) => Apply(subst(t1,e2,x), subst(t2,e2,x))

      case Read(t) => Read(subst(t,e2,x))

      case MoveXY(x1, y1, a) => MoveXY(subst(x1,e2,x), subst(y1,e2,x), subst(a,e2,x))

      case Blank => Blank

      case Over(t1,t2) => Over(subst(t1,e2,x),subst(t2,e2,x))

      case When(t1,t2,t3) => When(subst(t1,e2,x),subst(t2,e2,x),subst(t3,e2,x))

      case SignalBlock(t) => SignalBlock(subst(t,e2,x))

      case Escape(t) => Escape(subst(t,e2,x))

      case _ => sys.error("todo substitution ex4")
      // END ANSWER
    }
  }


  /****************
   *  Exercise 5  *
   ****************/

  // ----------------------------------------------------------------
  // Desugaring
  def desugar(e: Expr): Expr = {
    def desugarVal(v: Value): Value = v match {
      case PairV(v1, v2) => PairV(desugarVal(v1), desugarVal(v2))
      case FunV(x, ty, e) => FunV(x, ty, desugar(e))
      case RecV(f, x, tyx, ty, e) => RecV(f, x, tyx, ty, desugar(e))
      case ListV(vs) => ListV(vs.map(desugarVal))
      // Signal values do not appear before evaluation happens so do not need to be desugared
      case v => v
    }

    e match {
      case v: Value => desugarVal(v)
      // BEGIN ANSWER

      // arithmetic expressions

      case Plus(e1,e2) => Plus(desugar(e1),desugar(e2))
      case Minus(e1,e2) => Minus(desugar(e1),desugar(e2))
      case Times(e1,e2) => Times(desugar(e1),desugar(e2))
      case Div(e1,e2) => Div(desugar(e1),desugar(e2))

      // booleans

      case Eq(e1,e2) => Eq(desugar(e1),desugar(e2))
      case IfThenElse(cond,e1,e2) => IfThenElse(desugar(cond),desugar(e1),desugar(e2))
      case GreaterThan(e1,e2) => GreaterThan(desugar(e1),desugar(e2))
      case LessThan(e1,e2) => LessThan(desugar(e1),desugar(e2))

      //variables and let bindings
      case Let(x,e1,e2) => Let(x,desugar(e1),desugar(e2))
      case LetFun(f,arg,ty,e1,e2) =>
        Let(f,Lambda(arg,ty,desugar(e1)),desugar(e2))
      case LetRec(f,arg,xty,ty,e1,e2) => {
        Let(f,
          Rec(f,arg,xty,ty,desugar(e1)),
          desugar(e2))
      }
      case LetPair(x,y,e1,e2) => {
        val p = Gensym.gensym("p")
        Let(p,desugar(e1),subst(subst(desugar(e2),Fst(Var(p)),x),Snd(Var(p)),y))
      }

      // pairs
      case Pair(e1,e2) => Pair(desugar(e1),desugar(e2))
      case Fst(e) => Fst(desugar(e))
      case Snd(e) => Snd(desugar(e))

      //functions
      case Lambda(x,ty,e) => Lambda(x,ty,desugar(e))
      case App(e1,e2) => App(desugar(e1),desugar(e2))
      case Rec(f,x,tyx,ty,e) => Rec(f,x,tyx,ty,desugar(e))

      // lists
      case EmptyList(ty) => EmptyList(ty)

      case Cons(e1, e2) => Cons(desugar(e1),desugar(e2))

      case ListCase(l, t1, consVar1, consVar2, t2) => {
        
        ListCase(desugar(l),desugar(t1),consVar1,consVar2,desugar(t2))
      }


      // Sequencing 
      case Seq(e1,e2) => Seq(desugar(e1), desugar(e2))

      //Signal case probably wrong 
      case SignalBlock(e) => desugarBlock(e) //maybe change this to SignalBlock(desugarBlock(e))
    
      case e => e // Num, bool, str, var
      //case _ => sys.error("todo ex5 " + e.toString)
      // END ANSWER
    }
  }

  /****************
   *  Exercise 6  *
   ****************/
  def binaryOperation(e: (Expr,Expr)) : Boolean = {
    e match {
      case (t1 : IntV, t2 : IntV) => true
      case _ => false
    }
  }

  def desugarBlock(e: Expr): Expr = {
    e match {
      case v: Value => Pure(desugar(v))

      // BEGIN ANSWER
      //apply
      case Apply(se1,se2) =>Apply(desugarBlock(se1),desugarBlock(se2))

      //ifthenelse
      case IfThenElse(se,se1,se2) => When(desugarBlock(se),desugarBlock(se1),desugarBlock(se2))

      //over
      case Over(se1,se2) => Over(desugarBlock(se1),desugarBlock(se2))

      //ARITHMETIC ONES NO IDEA probably wrong find where to fit in lambda //somehow figure out how to turn that into an expr
      //case Plus(se1,se2) => if (binaryOperation((se1,se2))) {Pure({x:Int => y:Int => x + y}) <*> desugarBlock(se1) <*> desugarBlock(se2)} else {sys.error("Must be int for binary operation")}
      case Plus(se1,se2) => {Apply(Apply(Pure(Lambda("x", IntTy, Lambda("y", IntTy, Plus(Var("x"),Var("y") )) )),desugarBlock(se1)), desugarBlock(se2)) } //<*> desugarBlock(se1) <*> desugarBlock(se2)}
      // else {sys.error("Must be int for binary operation")}
       
      case Minus(se1,se2) =>  {Apply(Apply(Pure(Lambda("x", IntTy, Lambda("y", IntTy, Minus(Var("x"),Var("y") )) )),desugarBlock(se1)), desugarBlock(se2)) } 

      case Times(se1,se2) =>  {Apply(Apply(Pure(Lambda("x", IntTy, Lambda("y", IntTy, Times(Var("x"),Var("y") )) )),desugarBlock(se1)), desugarBlock(se2)) } 
     
      case Div(se1,se2) =>  {Apply(Apply(Pure(Lambda("x", IntTy, Lambda("y", IntTy, Div(Var("x"),Var("y") )) )),desugarBlock(se1)), desugarBlock(se2)) } 
       

      //case Div(se1,se2) => if (binaryOperation((se1,se2))) {Pure(Lambda("x", IntTy, Lambda("y", IntTy, Div(desugarBlock(se1),desugarBlock(se2)) ) ))}
      // else {sys.error("Must be int for binary operation")}

      //case Minus(se1,se2) => if (binaryOperation((se1,se2))) {Minus(desugarBlock(se1), desugarBlock(se2))} else {sys.error("Must be int for binary operation")}
      //case Times(se1,se2) => if (binaryOperation((se1,se2))) {Times(desugarBlock(se1), desugarBlock(se2))} else {sys.error("Must be int for binary operation")}
      //case Div(se1,se2) => if (binaryOperation((se1,se2))) {Div(desugarBlock(se1), desugarBlock(se2))} else {sys.error("Must be int for binary operation")}

      //less than
      case LessThan(se1,se2) =>  {Apply(Apply(Pure(Lambda("x", IntTy, Lambda("y", IntTy, LessThan(Var("x"),Var("y") )) )),desugarBlock(se1)), desugarBlock(se2)) } 
       

      //greater than
      case GreaterThan(se1,se2) =>  {Apply(Apply(Pure(Lambda("x", IntTy, Lambda("y", IntTy, GreaterThan(Var("x"),Var("y") )) )),desugarBlock(se1)), desugarBlock(se2)) } 
       

      //equals
      case Eq(se1,se2) => {Apply(Apply(Pure(Lambda("x", IntTy, Lambda("y", IntTy, Eq(Var("x"),Var("y") )) )),desugarBlock(se1)), desugarBlock(se2)) } 
    

      //when
      case When(se1,se2,se3) => When(desugarBlock(se1),desugarBlock(se2),desugarBlock(se3))

      //time
      case Time => Time

    

      //read 
      case Read(e) => Read(desugar(e))

      //moveXY
      case MoveXY(x,y,a) => MoveXY(desugarBlock(x),desugarBlock(y),desugarBlock(a))

      //escape
      case Escape(e) => desugar(e)

      //WRONG HERE FOR TESTING PURPOSES
      //case App(e1,e2) => App(e1,e2)


      
      case _ => sys.error("todo ex6: " + e.toString)
      // END ANSWER
    }
  }



  // ----------------------------------------------------------------
  // Evaluation Stage 1
  object Eval {

    /****************
     *  Exercise 7  *
     ****************/

    // Some helper functions to simplify cases
    // It is also OK to analyze each case without using these helper functions
    def extractConstructor(expr: Expr): (Expr, Expr) => Expr = expr match {
      case Plus(_, _) => Plus
      case Minus(_, _) => Minus
      case Times(_, _) => Times
      case Div(_, _) => Div
      case Eq(_, _) => Eq
      case GreaterThan(_, _) => GreaterThan
      case LessThan(_, _) => LessThan
      case Pair(_, _) => Pair
      case Cons(_, _) => Cons
    }
    def extractFstArg(expr: Expr): Expr = expr match {
      case Plus(e, _) => e
      case Minus(e, _) => e
      case Times(e, _) => e
      case Div(e, _) => e
      case Eq(e, _) => e
      case GreaterThan(e, _) => e
      case LessThan(e, _) => e
      case Pair(e, _) => e
      case Cons(e, _) => e
    }
    def extractSndArg(expr: Expr): Expr = expr match {
      case Plus(_, e) => e
      case Minus(_, e) => e
      case Times(_, e) => e
      case Div(_, e) => e
      case Eq(_, e) => e
      case GreaterThan(_, e) => e
      case LessThan(_, e) => e
      case Pair(_, e) => e
      case Cons(_, e) => e
    }
    def extractOperation(expr: Expr): Value => Value => Value = expr match {
      case Plus(_, _) => v1: Expr => v2: Expr => (v1, v2) match {
        case (IntV(v1), IntV(v2)) => IntV(v1 + v2)
      }
      case Minus(_, _) => v1: Expr => v2: Expr => (v1, v2) match {
        case (IntV(v1), IntV(v2)) => IntV(v1 - v2)
      }
      case Times(_, _) => v1: Expr => v2: Expr => (v1, v2) match {
        case (IntV(v1), IntV(v2)) => IntV(v1 * v2)
      }
      case Div(_, _) => v1: Expr => v2: Expr => (v1, v2) match {
        case (IntV(v1), IntV(v2)) => IntV(v1 / v2)
      }
      case Eq(_, _) => v1: Expr => v2: Expr => BoolV(v1 == v2)
      case GreaterThan(_, _) => v1: Expr => v2: Expr => (v1, v2) match {
        case (IntV(v1), IntV(v2)) => BoolV(v1 > v2)
      }
      case LessThan(_, _) => v1: Expr => v2: Expr => (v1, v2) match {
        case (IntV(v1), IntV(v2)) => BoolV(v1 < v2)
      }
      case Pair(_, _) => v1: Expr => v2: Expr => (v1, v2) match {
        case (v1: Value, v2: Value) => PairV(v1, v2) // must be values
      }
      case Cons(_, _) => v1: Expr => v2: Expr => (v1, v2) match {
        case (v1: Value, ListV(v2)) => ListV(v1 :: v2) // must be values
      }
    }


    def eval(expr: Expr): Value = {

    //testing purposes
    //println(expr.toString);
    
    expr match {

      
      
      // Values
      case v: Value => v
      
      // arithmetic operations
      //case Plus(e1,e2) => extractOperation(expr)(eval(e1))(eval(e2))
      case Plus(e1,e2) => (e1,e2) match {
        case (IntV(e1),IntV(e2)) => IntV(e1+e2)
        case _ => eval(Plus(eval(e1),eval(e2))) 
      }
      //case Minus(e1,e2) => extractOperation(expr)(eval(e1))(eval(e2))
      case Minus(e1,e2) => (e1,e2) match {
        case (IntV(e1),IntV(e2)) => IntV(e1-e2)
        case _ => eval(Minus(eval(e1),eval(e2)))
      }
      //case Times(e1,e2) => extractOperation(expr)(eval(e1))(eval(e2))
      case Times(e1,e2) => (e1,e2) match {
        case (IntV(e1),IntV(e2)) => IntV(e1*e2)
        case _ => eval(Times(eval(e1),eval(e2)))
      }
      //case Div(e1,e2) => extractOperation(expr)(eval(e1))(eval(e2))
      case Div(e1,e2) => (e1,e2) match {
        case (IntV(e1),IntV(e2)) => IntV(e1/e2)
        case _ => eval(Div(eval(e1),eval(e2))) 
      }

      //booleans
      //case Eq(e1,e2) => extractOperation(expr)(eval(e1))(eval(e2))
      case Eq(e1,e2) => (e1,e2) match {
        case (IntV(e1),IntV(e2)) => BoolV(e1 == e2)
        case _ => eval(Eq(eval(e1),eval(e2))) 
      }
      //case LessThan(e1,e2) => extractOperation(expr)(eval(e1))(eval(e2))
      case LessThan(e1,e2) => (e1,e2) match {
        case (IntV(e1),IntV(e2)) => BoolV(e1 < e2)
        case _ => eval(LessThan(eval(e1),eval(e2))) 
      }
      //case GreaterThan(e1,e2) => extractOperation(expr)(eval(e1))(eval(e2))
      case GreaterThan(e1,e2) => (e1,e2) match {
        case (IntV(e1),IntV(e2)) => BoolV(e1 > e2)
        case _ => eval(GreaterThan(eval(e1),eval(e2))) 
      }

      case IfThenElse(e,e1,e2) =>
        eval(e) match {
          case BoolV(true) => eval(e1)
          case BoolV(false) => eval(e2)
          case _ =>  sys.error("conditional must evaluate to a boolean")
        }

      //pairs
      case Pair(e1,e2) => PairV(eval(e1),eval(e2))

      case Fst(e) => eval(e) match {
        case PairV(x,_) => eval(x)
        case _ => sys.error("first must be applied to a pair")
      }
      
      case Snd(e) => eval(e) match {
        case PairV(_,y) => eval(y)
        case _ => sys.error("second must be applied to a pair")
      }


      //lists
      case Cons(e1,e2) => (e1,e2) match { 
        case (x : Value, ListV(y)) => ListV(x :: y)
        case _ => sys.error("cons must be values") }
      //list case (what it evaluates to is in figure 10)
      //empty list?


      //functions //Substitution e1 [e2 / x]
      //maybe this
      case App(e1,e2) => eval(e1) match {
        case FunV(x,ty,e) => eval(subst(e, eval(e2), x))
        case RecV(f, x, tyx, ty, e) =>  eval(subst(subst(e,RecV(f,x,tyx,ty,e), f),eval(e2),x))//eval(RecV(f,x,tyx,ty,subst(subst(e,eval(e2),x),eval(e1),f)))
        case _ => sys.error("first argument must be function")
      }

      //case class FunV(x: Variable, ty: Type, e: Expr) extends Value
      //case class RecV(f: Variable, x: Variable, tyx: Type, ty: Type, e: Expr) extends Value

      //let
      case Let(x,e1,e2) => eval(subst(e2, eval(e1), x))

      //rec
      //lambda //think these are included in app

      //signals
      case Pure(e) => PureV(eval(e)) 

      //apply
      case Apply(e1,e2) => ApplyV(eval(e1),eval(e2))

      //moveXY
      case MoveXY(e1,e2,e3) => MoveXYV(eval(e1),eval(e2),eval(e3))

      //when
      case When(e1,e2,e3) => WhenV(eval(e1),eval(e2),eval(e3))

      //read
      case Read(e) => ReadV(eval(e))
      
      //over
      case Over(e1,e2) => OverV(eval(e1),eval(e2))

      //time //obvously change this just for a test //this is wrong
      case Time => TimeV

      //leftover from assn2
      //functions
      case Lambda(x,ty,e) =>
        FunV(x,ty,e)
      case Rec(f,x,tyx,ty,e) =>
        RecV(f,x,tyx,ty,e)
      
      case _ => sys.error("todo ex7: " + expr.toString)
      // END ANSWER
    }
    }
    
  }
  ////////////////////////////////////////////////////////////////////
  // ************************************************************** //
  // *            DO NOT CHANGE CODE BELOW THIS POINT             * //
  // ************************************************************** //
  ////////////////////////////////////////////////////////////////////
  
  // ----------------------------------------------------------------
  // Evaluation Stage 1
  object Translation {
    // import rabbitDSL._
    def paren(s: String) = "(" + s + ")"
    def brace(s: String) = "{" + s + "}"

    def tr(expr: Expr): String = expr match {
      // Arithmetic expressions
      case Plus(e1, e2)  => paren(tr(e1)) + " + " + paren(tr(e2))
      case Minus(e1, e2) => paren(tr(e1)) + " - " + paren(tr(e2))
      case Times(e1, e2) => paren(tr(e1)) + " * " + paren(tr(e2))
      case Div(e1, e2)   => paren(tr(e1)) + " / " + paren(tr(e2))

      // Booleans
      case Eq(e1, e2) => paren(tr(e1)) + " == " + paren(tr(e2))
      case GreaterThan(e1, e2) => paren(tr(e1)) + " > " + paren(tr(e2))
      case LessThan(e1, e2) => paren(tr(e1)) + " < " + paren(tr(e2))
      case IfThenElse(e, e1, e2) => brace("if" + paren(tr(e)) + " {" + tr(e1) + "} else {" + tr(e2) + "}")

      // Variables and let-binding
      case Var(x) => x
      case Let(x, e1, e2) => brace("val " + x + " = " + tr(e1) + "; " + tr(e2))

      // Pairs
      case Pair(e1, e2) => paren(tr(e1) + ", " + tr(e2))
      case Fst(e1) => tr(e1) + "._1"
      case Snd(e1) => tr(e1) + "._2"

      // Functions
      case Lambda(x, ty, e) => brace(x + ": " + trty(ty) + " => " + tr(e))
      case Rec(f, x, tyx, ty, e)
        => paren("new(" + paren(trty(tyx)) + " => " + trty(ty) + "){def apply" + paren(x + ": " + trty(tyx)) + ": " + trty(ty) + " = " + tr(swap(e, f, "apply")) + "}")
      case App(e1, e2) => paren(tr(e1)) + paren(tr(e2))

      // Lists
      case EmptyList(ty) => "Nil"
      case Cons(e1, e2) => paren(tr(e1) + "::" + tr(e2))
      case ListCase(l, e1, x, y, e2) => paren(tr(l) + " match " + brace(
        "case Nil => " + tr(e1) + "; " + "case " + x + "::" + y + " => " + tr(e2) ))
      
      // Signals
      case Time => "time"
      case Read(e) => "read" + paren(tr(e))
      case Pure(e) => "pure" + paren(tr(e))
      case Apply(e1, e2) => paren(tr(e1)) + " <*> " + paren(tr(e2))
      case MoveXY(e1, e2, e3) => "moveXY" + paren(tr(e1) + ", " + tr(e2) + ", " + tr(e3))
      case When(e1, e2, e3) => "when" + paren(tr(e1) + ", " + tr(e2) + ", " + tr(e3))
      case Blank => "blank"
      case Over(e1,e2) => paren(tr(e1) + "<+>" + tr(e2))
      
      case TimeV => "time"
      case ReadV(e) => "read" + paren(tr(e))
      case PureV(e) => "pure" + paren(tr(e))
      case ApplyV(e1, e2) => paren(tr(e1)) + " <*> " + paren(tr(e2))
      case MoveXYV(e1, e2, e3) => "moveXY" + paren(tr(e1) + ", " + tr(e2) + ", " + tr(e3))
      case WhenV(e1, e2, e3) => "when" + paren(tr(e1) + ", " + tr(e2) + ", " + tr(e3))
      case BlankV => "blank"
      case OverV(e1,e2) => paren(tr(e1) + "<+>" + tr(e2))
      
      // Values
      case UnitV => "()"
      case IntV(n) => n.toString
      case BoolV(b) => b.toString
      case ListV(l) => l.map({e:Expr => tr(e)}).toString
      case StringV(s) => "\"" + s + "\""
      case PairV(v1, v2) => (tr(v1), tr(v2)).toString
      case FunV(x, ty, e) => tr(Lambda(x, ty, e))
      case RecV(f, x, tyx, ty, e) => tr(Rec(f, x, tyx, ty, e))
    }

    def trty(ty: Type): String = ty match {
      case UnitTy => "Unit"
      case IntTy => "Int"
      case BoolTy => "Boolean"
      case StringTy => "String"
      case FrameTy => "Frame"
      case ListTy(ty) => "List[" + trty(ty) + "]"
      case PairTy(ty1, ty2) => paren(trty(ty1) + ", " + trty(ty2))
      case FunTy(ty1, ty2) => paren(trty(ty1) + " => " + trty(ty2))
      case SignalTy(ty) => "Signal[" + trty(ty) + "]"
    }
  }


  val parser = new RabbitParser
  object Main {
    def typecheck(ast: Expr):Type =
      tyOf(Map.empty,ast);

  def showResult(ast: Expr, outputFilename: String, test: Boolean, sampleSolution: Boolean) {
      println("AST:  " + ast.toString + "\n")
      try {
        print("Type Checking...");
        val ty = typecheck(ast);
        println("Done!");
        println("Type of Expression: " + ty.toString + "\n") ;
	(test, ty) match {
	  case (true,_) => ()
	  case (false,SignalTy(FrameTy)) => ()
	  case (false,ty) => {
	    sys.error("Can only run animations of type signal[frame], not " + ty.toString)
	  }
        }
      } catch {
          case e:Throwable => {
            println("Error: " + e)
	  }
      } 
      try{
        println("Desugaring...");
        val core_ast = desugar(ast);
        println("Done!");
        println("Desugared AST: " + core_ast.toString + "\n") ;
        try {
          println("Evaluating...");
          var result = Eval.eval(core_ast)
          println("Done!");
          println("Evaluated AST: " + result.toString + "\n") ;
          if (test) {
            println(result)
            println(Translation.tr(result))
          } else {
            println("Writing to Scala file...");
            val fileWriter = new FileWriter(new File("RunRabbit.scala"))
            if (sampleSolution) {
	      fileWriter.write("import Assignment3.RabbitEDSL.Assignment3Embedded.DeepRabbitDSL._\n\n")
	    } else {
              fileWriter.write("import Assignment3.RabbitEDSL.Assignment3Embedded.RabbitDSLImpl._\n\n")
	    }
            fileWriter.write("def anim = " + Translation.tr(result) + " \n")
            fileWriter.write("saveToFile(anim, 20, \"" + outputFilename + "\")\n")
            fileWriter.close()
          }
        } catch {
          case e:Throwable => println("Error: " + e)
        }
      } catch {
        case e: Throwable =>  println("Error: " + e)
          println("Evaluating original AST...");
          print(Translation.tr(Eval.eval(ast)))
      }
   
    }
  }

  val FILENAME = "filename"
  val OUTPUT = "output"
  val TEST = "test"
  val SAMPLE = "sample"

  val defaultArgs = ListMap (
    FILENAME -> "",
    OUTPUT -> "output.gif",
    TEST -> "false",
    SAMPLE -> "false"
  )

  def main( args:Array[String] ):Unit = {
    val argList = args.toList

    def readArgs(argList: List[String], optMap: ListMap[String, String]):
      ListMap[String, String] = argList match {
        case Nil => optMap
        case "-o" :: outputName :: tail =>
          readArgs(tail, optMap + (OUTPUT -> outputName))
        case "-t" :: tail =>
          readArgs(tail, optMap + (TEST -> "true"))
        case "-s" :: tail =>
          readArgs(tail, optMap + (SAMPLE -> "true"))
        case fn :: _ => optMap + (FILENAME -> fn)
    }

  if (args.length == 0) {
      print("Usage: [-o output_filename] [-t] filename\n")
    } else {
      print("Parsing...");
      val argMap = readArgs(args.toList, defaultArgs)
      val ast = parser.parse(argMap(FILENAME))
      println("Done!");
      Main.showResult(ast, argMap(OUTPUT), argMap(TEST).toBoolean, argMap(SAMPLE).toBoolean)
    }
  }

}