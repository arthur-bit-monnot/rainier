package com.stripe.rainier.compute

import org.scalatest._
import com.stripe.rainier.core._
import scala.util.{Try, Success, Failure}
class RealTest extends FunSuite {
  def run(description: String,
          defined: Double => Boolean = _ => true,
          derivable: Double => Boolean = _ => true,
          reference: Double => Double = null)(fn: Real => Real): Unit = {
    test(description) {
      val x = new Variable
      val result = fn(x)
      val deriv = result.gradient.head

      def evalAt(d: Double): Double = Try { fn(Constant(d)) } match {
        case Success(Infinity)                 => 1.0 / 0.0
        case Success(NegInfinity)              => -1.0 / 0.0
        case Success(Constant(bd))             => bd.toDouble
        case Failure(_: ArithmeticException)   => 0.0 / 0.0
        case Failure(_: NumberFormatException) => 0.0 / 0.0
        case Failure(e)                        => throw e
        case x                                 => sys.error("Non-constant value " + x)
      }

      val c = Compiler(200, 100).compile(List(x), result)
      val dc = Compiler(200, 100).compile(List(x), deriv)
      List(1.0, 0.0, -1.0, 2.0, -2.0, 0.5, -0.5).filter(defined).foreach { n =>
        val constant = evalAt(n)
        if (reference != null) {
          assertWithinEpsilon(constant, reference(n), s"[c/ref, n=$n]")
        }
        val eval = new Evaluator(Map(x -> n))
        val withVar = eval.toDouble(result)
        assertWithinEpsilon(constant, withVar, s"[c/ev, n=$n]")
        val compiled = c(Array(n))
        assertWithinEpsilon(withVar, compiled, s"[ev/ir, n=$n]")

        // derivatives of automated differentiation vs numeric differentiation
        if (derivable(n)) {
          val dx = 10E-6
          val numDiff = (evalAt(n + dx) - evalAt(n - dx)) / (dx * 2)
          val diffWithVar = eval.toDouble(deriv)
          assertWithinEpsilon(numDiff,
                              diffWithVar,
                              s"[numDiff/diffWithVar, n=$n]")
          val diffCompiled = dc(Array(n))
          assertWithinEpsilon(diffWithVar,
                              diffCompiled,
                              s"[diffWithVar/diffCompiled, n=$n]")
        }
      }
    }
  }

  def assertWithinEpsilon(x: Double, y: Double, clue: String): Unit = {
    if (x.abs < 10E-8 && y.abs < 10E-8)
      // both values really close to 0, consider equal to
      // account for inaccuracies in numeric differentiation
      return ()
    val relativeError = ((x - y) / x).abs
    if (!(x.isNaN && y.isNaN || relativeError < 0.001))
      assert(x == y, clue)
    ()
  }

  run("plus") { x =>
    x + 1
  }
  run("exp") { x =>
    x.exp
  }
  run("square") { x =>
    x * x
  }
  run("log") { x =>
    x.abs.log
  }
  run("sin", reference = math.sin) { x =>
    x.sin
  }
  run("cos", reference = math.cos) { x =>
    x.cos
  }
  run("tan", reference = math.tan) { x =>
    x.tan
  }
  run("asin", defined = x => x > -1 && x < 1, reference = math.asin) { x =>
    x.asin
  }
  run("acos", defined = x => x > -1 && x < 1, reference = math.acos) { x =>
    x.acos
  }
  run("atan", reference = math.atan) { x =>
    x.atan
  }
  run("cos(x^2)") { x =>
    (x * x).cos
  }
  run("temp") { x =>
    val t = x * 3
    t + t
  }
  run("abs") { x =>
    x.abs
  }
  run("max(x, 0)", derivable = _ != 0) { x =>
    x.max(0)
  }
  run("max(x, x)") { x =>
    x.max(x)
  }
  run("x > 0 ? x^2 : 1", derivable = _ != 0) { x =>
    Real.gt(x, 0, x * x, 1)
  }
  run("x > 0 ? 1 : x + 1", derivable = _ != 0) { x =>
    Real.gt(x, 0, 1, x + 1)
  }
  run("x > 0 ? x^2 : x + 1", derivable = _ != 0) { x =>
    Real.gt(x, 0, x * x, x + 1)
  }

  run("normal") { x =>
    Real.sum(Range.BigDecimal(0d, 2d, 1d).toList.map { y =>
      Normal(x, 1).logDensity(Real(y))
    })
  }

  run("logistic") { x =>
    val logistic = Real.one / (Real.one + (x * -1).exp)
    (logistic * (Real.one - logistic)).log
  }

  run("minimal logistic") { x =>
    Real.one / (x.exp + 1)
  }

  run("log x^2", derivable = _ != 0) { x =>
    x.pow(2).log
  }

  run("poisson") { x =>
    Real.sum(0.to(10).toList.map { y =>
      Poisson(x.abs + 1).logDensity(y)
    })
  }

  run("4x^3") { x =>
    (((((x + x) * x) +
      (x * x)) * x) +
      (x * x * x))
  }

  run("lookup", derivable = _ => false) { x => // not derivable
    val i = x.abs * 2 //should be a non-negative whole number
    Lookup(i, Real.seq(List(0, 1, 2, 3, 4)))
  }

  val exponents = scala.util.Random.shuffle(-40.to(40))
  run("exponent sums", derivable = _ != 0) { x =>
    exponents.foldLeft(x) {
      case (a, e) =>
        (a + x.pow(e)) * x
    }
  }

  run("pow", derivable = _ >= 0) { x =>
    x.pow(x)
  }

  run("gamma fit") { x =>
    Real.sum(List(1d, 2d, 3d).map { y =>
      Gamma.standard(x.abs).logDensity(y)
    })
  }
}
