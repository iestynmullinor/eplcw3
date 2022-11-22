/* --------------------------------------------------------------------------------------- *
 * --------------------------------------------------------------------------------------- *
 *                            EPL ASSIGNMENT 3 - VERSION 1.0                               *
 * --------------------------------------------------------------------------------------- *
 * --------------------------------------------------------------------------------------- */
 
package Assignment3.RabbitEDSL
import math.{cos, sin, Pi}
import Assignment3.AnimatedGif.AnimatedGif._
import Assignment3.FRP.Signal
import Assignment3.FRP.Signal._

object Assignment3Embedded {

  trait RabbitDSL {
    type RabbitAnimation[T]

    // Environment time signal
    def time: RabbitAnimation[Time]

    // Create a constant animation from a picture given by `name`
    def read(name: String): RabbitAnimation[Frame]

    // Blank animation
    def blank: RabbitAnimation[Frame]

    // Lift a value to a constant signal
    def pure[A](t: A): RabbitAnimation[A]

    // Apply a signal of a unary function to another signal
    def app[A, B](f: RabbitAnimation[A => B], t: RabbitAnimation[A])
                 : RabbitAnimation[B]

    // If signal `b` is true then return signal `t1`, else return signal `t2`
    def when[A]( b: RabbitAnimation[Boolean], t1: RabbitAnimation[A]
               , t2: RabbitAnimation[A]): RabbitAnimation[A]

    // Move the animation `a` according to the coordinates given by signals `dx` and `dy`
    def moveXY( dx: RabbitAnimation[Int], dy: RabbitAnimation[Int]
              , a: RabbitAnimation[Frame]): RabbitAnimation[Frame]

    // Scale the animation `a` according to the scale factor given by signal `factor`.
    def scale(factor: RabbitAnimation[Double], a: RabbitAnimation[Frame])
             : RabbitAnimation[Frame]

    // Rotate the animation `a` according to the angle given by signal `angle`.
    def rotate(angle: RabbitAnimation[Double], a: RabbitAnimation[Frame])
              : RabbitAnimation[Frame]

    // Combine two animations
    def over(x: RabbitAnimation[Frame], y: RabbitAnimation[Frame])
            : RabbitAnimation[Frame]

    // Run a Rabbit DSL program to get the result
    def runRabbitAnimation[T](term: RabbitAnimation[T]): Signal[T]

    // Saves the Animation given by `anim` of duration `timeEnd` to `filename`.
    def saveToFile( anim: RabbitAnimation[Frame], timeEnd: Time
                  , filename: String) : Unit = {
      generateGif(runRabbitAnimation(anim), timeEnd, filename)
    }

    // Scala magic to allow <*> and <+> infix operators. No implementation needed here -- you can ignore this.
    class LiftAssoc[A,B](x: RabbitAnimation[A => B]) {
      def <*> (y: RabbitAnimation[A]): RabbitAnimation[B] = app(x,y)
    }
    implicit def ra2LiftAssoc[A, B](x: RabbitAnimation[A => B]): LiftAssoc[A, B] = new LiftAssoc(x)
    class OverAssoc[A,B](x: RabbitAnimation[Frame]) {
      def <+> (y: RabbitAnimation[Frame]): RabbitAnimation[Frame] = over(x, y)
    }
    implicit def ra2OverAssoc[A, B](x: RabbitAnimation[Frame]): OverAssoc[A, B] = new OverAssoc(x)
  }




  object Testing extends RabbitDSL {
      // This implementation simply prints out each command as it is encountered.
      // It is NOT a good model to use for implementing the EDSL, but might be useful
      // for debugging.
    type RabbitAnimation[T] = Unit

    def time(): RabbitAnimation[Time]
      = println("time()")

    def read(name: String): RabbitAnimation[Frame]
      = println("read(\"" + name + "\")")

    def blank(): RabbitAnimation[Frame]
      = println("blank()")

    def pure[A](t: A): RabbitAnimation[A]
      = println("pure(_)")

    def app[A, B](f: RabbitAnimation[A => B], t: RabbitAnimation[A])
                 : RabbitAnimation[B]
      = println("app(_, _)");

    def when[A]( b: RabbitAnimation[Boolean], t1: RabbitAnimation[A]
               , t2: RabbitAnimation[A]): RabbitAnimation[A]
      = println("when(_, _, _)")

    def moveXY( dx: RabbitAnimation[Int], dy: RabbitAnimation[Int]
              , a: RabbitAnimation[Frame]): RabbitAnimation[Frame]
      = println("move(_, _, _)")

    def scale(factor: RabbitAnimation[Double], a: RabbitAnimation[Frame])
             : RabbitAnimation[Frame]
      = println("scale(_, _)")

    def rotate(angle: RabbitAnimation[Double], a: RabbitAnimation[Frame])
              : RabbitAnimation[Frame]
      = println("rotate(_, _)")

    def over(x: RabbitAnimation[Frame], y: RabbitAnimation[Frame])
            : RabbitAnimation[Frame]
      = println("over(_, _)")

    def runRabbitAnimation[T](term: RabbitAnimation[T]): Signal[T]
      = sys.error("imposssible :(")
    
    override def saveToFile( anim: RabbitAnimation[Frame], timeEnd: Time
                           , filename: String) = {}
  }

   /****************
   *  Exercise 1  *
   ****************/

  // BEGIN ANSWER
  //attempt1: shallow embeddings

  object RabbitDSLImpl extends RabbitDSL {
    type RabbitAnimation[T] = Signal[T] // TODO: change this

    def time(): RabbitAnimation[Time]
      = envtime

    def read(name: String): RabbitAnimation[Frame]
      = Signal(List(Picture(name))) //maybe (name,0,0,0.5,0)

    def blank(): RabbitAnimation[Frame]
      = Signal(List())

    def pure[A](t: A): RabbitAnimation[A]
      = Signal(t)

    def app[A, B](f: RabbitAnimation[A => B], t: RabbitAnimation[A])
                 : RabbitAnimation[B]
      = Signal(f()(t()))

    //helper for when
    //def check(x: Boolean) : Boolean = x  

    def when[A]( b: RabbitAnimation[Boolean], t1: RabbitAnimation[A]
               , t2: RabbitAnimation[A]): RabbitAnimation[A] = 
               lift3(b)(t1)(t2)({(b: Boolean) => (t1: A) => (t2: A) => if (b) {t1} else {t2}})

    //helper for moveXY  
    def changeCoords(pic : Picture, dx: Int, dy: Int) : Picture
      = Picture(pic.name, pic.x+dx, pic.y+dy)

    def moveXY( dx: RabbitAnimation[Int], dy: RabbitAnimation[Int]
              , a: RabbitAnimation[Frame]): RabbitAnimation[Frame]
      = lift3(a)(dx)(dy)({(a: Frame) => (dx: Int) => (dy: Int) => a.map(pic => changeCoords(pic,dx,dy))})

    //helper for scale 
    def scalePicture(pic: Picture, factor: Double) : Picture
        = Picture(pic.name, (pic.x*factor).toInt, (pic.y*factor).toInt, pic.scaleFactor*factor) 

    def scale(factor: RabbitAnimation[Double], a: RabbitAnimation[Frame])
             : RabbitAnimation[Frame]
      = lift2(a)(factor)({(a: Frame) => (factor: Double) => a.map(pic => scalePicture(pic,factor))})

    //helper for rotate 
    def rotatePicture(pic: Picture, angle: Double) : Picture
        = Picture(pic.name, (pic.x*cos(angle)+pic.y*sin(angle)).toInt, (-pic.x*sin(angle)+pic.y*cos(angle)).toInt, pic.scaleFactor, pic.angle + angle)

    def rotate(angle: RabbitAnimation[Double], a: RabbitAnimation[Frame])
              : RabbitAnimation[Frame]
      = lift2(a)(angle)({(a: Frame) => (angle: Double) => a.map(pic => rotatePicture(pic,angle))})

    def over(x: RabbitAnimation[Frame], y: RabbitAnimation[Frame])
            : RabbitAnimation[Frame]
      = {lift2(x)(y)({(x : Frame) => (y : Frame) => y ++ x} )}

    def runRabbitAnimation[T](term: RabbitAnimation[T]): Signal[T]
      = term

  }
  // END ANSWER


  ///////////////////////////////////////////////////////////////////////////
  // Test code - nothing to implement below this point but you may want to //
  // add more tests of your own.                                           //
  ///////////////////////////////////////////////////////////////////////////

  // change the comments to test RabbitDSLImpl instead
  //import Testing._
   import RabbitDSLImpl._


  //def testRead() = {read("turtle")}

  def turtleAndRabbit() =
    moveXY(pure({x:Int => x*50-500}) <*> time, pure(200), read("turtle")) <+>
    moveXY(when( pure({x:Int => x < 5}) <*> time
               , pure({x:Int => x*100-500}) <*> time
               , pure(0)) , pure(-200), read("rabbit"))

  def animalRace() = {
    def animalRun(name: String, speed: Int, pos: Int) =
      moveXY( pure({x:Int => x*speed-500}) <*> time, pure(pos)
            , scale(pure(0.4), read(name)))
    val players= List( ("turtle", 10, 400)
                     , ("cat", 40, 200)
                     , ("rabbit", 50, 0)
                     , ("elephant", 20, -200)
                     , ("bee", 30, -400)
                     )
    def f(anim: RabbitAnimation[Frame], tuple: (String, Int, Int)) = tuple match {
        case (n, s, p) =>
          (anim <+> animalRun(n, s, p))
      }
    players.foldLeft[RabbitAnimation[Frame]](blank)(f _)
  }

  def turtleRabbitTest() =
    (moveXY(((pure({x_0: Int => {y_1: Int => (x_0) - (y_1)}})) <*> (((pure({x_2: Int => {y_3: Int => (x_2) * (y_3)}})) <*> (time)) <*> (pure(50)))) <*> (pure(500)), pure(200), read("turtle"))<+>moveXY(when(((pure({x_4: Int => {y_5: Int => (x_4) < (y_5)}})) <*> (time)) <*> (pure(5)), ((pure({x_6: Int => {y_7: Int => (x_6) - (y_7)}})) <*> (((pure({x_8: Int => {y_9: Int => (x_8) * (y_9)}})) <*> (time)) <*> (pure(100)))) <*> (pure(500)), pure(0)), ((pure({x_10: Int => {y_11: Int => (x_10) - (y_11)}})) <*> (pure(0))) <*> (pure(200)), read("rabbit")))

  def slowTurtleTest() =
    moveXY(((pure({x_0_2: Int => {y_1_3: Int => (x_0_2) * (y_1_3)}})) <*> (time)) <*> (pure(20)), pure(100), read("turtle"))

  def diagTurtleTest() =
    moveXY(time, time, read("turtle"))

  object animalDance {
    val wiggle = pure({x: Int => sin(x * unit * Pi)}) <*> time
    val waggle = pure({x: Int => cos(x * unit * Pi)}) <*> time
    val iwiggle = pure({x: Double => (x * 500).toInt}) <*> wiggle
    val iwaggle = pure({x: Double => (x * 500).toInt}) <*> waggle
    def dance(a1: RabbitAnimation[Frame], a2: RabbitAnimation[Frame]) = {
      moveXY(pure(0), iwiggle, a1) <+> moveXY(iwaggle, pure(0), a2)
    }
    val cat = read("cat")
    val rabbit = read("rabbit")
    def catRabbitDance() = dance(cat, rabbit)
    def catRabbitDoubleDance() = {
      val scaled = scale(pure(0.5), catRabbitDance)
      dance(scaled, scaled)
    }
    def catRabbitZoomDance() = {
      scale(pure({x: Double => x.abs}) <*> wiggle, catRabbitDance)
    }
    def catRabbitCircleDance() = {
      val t = moveXY(pure(0), pure(300), scale(pure(0.5), catRabbitDance))
      rotate(pure({x: Int => x * unit * Pi}) <*> time, t)
    }
  }

  def testingCatRabbitDance() =
  moveXY(pure(0), ((pure({x_0_13_25: Int => {y_1_14_26: Int => (x_0_13_25) - (y_1_14_26)}})) <*> ((pure({x_11: Int => {if((((new(((Int, Int)) => Int){def apply(pa: (Int, Int)): Int = {val x = pa._1; {val y = pa._2; {if((x) < (y)) {x} else {(apply)(((x) - (y), y))}}}}}))(((x_11) / (1000), 2))) == (0)) {((new(((Int, Int)) => Int){def apply(pa: (Int, Int)): Int = {val x = pa._1; {val y = pa._2; {if((x) < (y)) {x} else {(apply)(((x) - (y), y))}}}}}))((x_11, 1000))} else {(1000) - (((new(((Int, Int)) => Int){def apply(pa: (Int, Int)): Int = {val x = pa._1; {val y = pa._2; {if((x) < (y)) {x} else {(apply)(((x) - (y), y))}}}}}))((x_11, 1000)))}}})) <*> (((pure({x_2_15_27: Int => {y_3_16_28: Int => (x_2_15_27) * (y_3_16_28)}})) <*> (time)) <*> (pure(100))))) <*> (pure(500)), read("cat"))<+>moveXY(((pure({x_4_18_30_37: Int => {y_5_19_31_38: Int => (x_4_18_30_37) - (y_5_19_31_38)}})) <*> ((pure({x_11: Int => {if((((new(((Int, Int)) => Int){def apply(pa: (Int, Int)): Int = {val x = pa._1; {val y = pa._2; {if((x) < (y)) {x} else {(apply)(((x) - (y), y))}}}}}))(((x_11) / (1000), 2))) == (0)) {((new(((Int, Int)) => Int){def apply(pa: (Int, Int)): Int = {val x = pa._1; {val y = pa._2; {if((x) < (y)) {x} else {(apply)(((x) - (y), y))}}}}}))((x_11, 1000))} else {(1000) - (((new(((Int, Int)) => Int){def apply(pa: (Int, Int)): Int = {val x = pa._1; {val y = pa._2; {if((x) < (y)) {x} else {(apply)(((x) - (y), y))}}}}}))((x_11, 1000)))}}})) <*> (((pure({x_6_20_32_39: Int => {y_7_21_33_40: Int => (x_6_20_32_39) * (y_7_21_33_40)}})) <*> (((pure({x_8_22_34_41: Int => {y_9_23_35_42: Int => (x_8_22_34_41) + (y_9_23_35_42)}})) <*> (time)) <*> (pure(5)))) <*> (pure(100))))) <*> (pure(500)), pure(0), read("rabbit"))

  object animalDance2 {
    def roundTrip(x: Int) = {if ((x/1000)%2 == 0) x%1000 else 1000-x%1000}
    // val wiggle = pure({x: Int => sin(x * unit * Pi)}) <*> time
    // val waggle = pure({x: Int => cos(x * unit * Pi)}) <*> time
    val iwiggle = pure({x: Int => roundTrip(x * 100) - 500}) <*> time
    val iwaggle = pure({x: Int => roundTrip((x+5) * 100) - 500}) <*> time
    def dance(a1: RabbitAnimation[Frame], a2: RabbitAnimation[Frame]) = {
      moveXY(pure(0), iwiggle, a1) <+> moveXY(iwaggle, pure(0), a2)
    }
    val cat = read("cat")
    val rabbit = read("rabbit")
    def catRabbitDance() = dance(cat, rabbit)
    def catRabbitDoubleDance() = {
      val scaled = scale(pure(0.5), catRabbitDance)
      dance(scaled, scaled)
    }
  }

  type Filename = String
  val toRun = List(
    (turtleAndRabbit(), 20, "turtleAndRabbit.gif"),
    (animalRace(), 20, "animalRace.gif"),
    (animalDance.catRabbitDance(), 20, "catRabbitDance.gif"),
    (animalDance.catRabbitDoubleDance(), 20, "catRabbitDoubleDance.gif"),
    (animalDance.catRabbitZoomDance(), 20, "catRabbitZoomDance.gif"),
    (animalDance.catRabbitCircleDance(), 20, "catRabbitCircleDance.gif"),
    (animalDance2.catRabbitDance(), 20, "catRabbitDance2.gif"),
    (animalDance2.catRabbitDoubleDance(), 20, "catRabbitDoubleDance2.gif"),
    (turtleRabbitTest(), 20, "turtleRabbitTest.gif"),
    (slowTurtleTest(), 20, "slowTurtleTest.gif"),
    (testingCatRabbitDance(), 20, "catRabbitDanceTest.gif"),
    (diagTurtleTest(),20,"diagTurtleTest.gif")

  )

  //val testToRun = List ((testRead, 20, "testRead.gif"))

  def drawFiles(xs: List[(RabbitAnimation[Frame], Time, Filename)]) = {
    xs.foreach (p => {
        val (a, t, fn) = p
        print("Processing " + fn + "...\n")
        saveToFile(a, t, fn)
      })
  }

  def main(args: Array[String]): Unit = {
    drawFiles(toRun)
  }

}

