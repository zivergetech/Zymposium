package testing

import zio._

object Example extends ZIOAppDefault {

  // Gen type that describes a "generator" of values of a given type
  // check method that "runs" one or more generators and uses the values to test some predicate

  // more generator constructors and operators
  // better error reporting

  lazy val testResult = check(Gen.int, Gen.int, Gen.int) { (x, y, z) =>
    val left = x + (y + z)
    val right = (x + y) + z
    left == right
  }

  sealed trait Animal { self =>
    def feedAppropriateFood: Animal =
      self match {
        case Animal.Cat(name, age, _) => Animal.Cat(name, age, Fish)
        case animal            => animal
      }
    def isPoisoned: Boolean =
      self match {
        case Animal.Cat(_, _, Brocolli) => true
        case _               => false
      }
  }

  object Animal {
    case class Dog(name: String, age: Int) extends Animal
    case class Cat(name: String, age: Int, food: Food) extends Animal
    case object Bird extends Animal
  }

  sealed trait Food

  case object Fish extends Food
  case object Meat extends Food
  case object Brocolli extends Food

  import Animal._

  val genDog: Gen[Dog] =
    for {
      name <- Gen.elements("Fido", "Rex", "Snoopy")
      age  <- Gen.intBetween(0, 10)
    } yield Dog(name, age)

  val genCat: Gen[Cat] =
    for {
      name <- Gen.elements("Garfield", "Tom", "Felix")
      age  <- Gen.intBetween(0, 10)
      food <- Gen.elements(Fish, Meat, Brocolli)
    } yield Cat(name, age, food)

  val genBird: Gen[Bird.type] =
    Gen.constant(Bird)

  val genAnimal: Gen[Animal] =
    Gen.oneOf(genDog, genCat, genBird)

  lazy val animalSafetyTest = check(genAnimal) { (animal) =>
    animal.feedAppropriateFood.isPoisoned == false
  }

  val run =
   animalSafetyTest.flatMap(report)
}