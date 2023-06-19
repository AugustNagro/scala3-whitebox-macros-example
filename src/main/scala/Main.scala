case class User(firstName: String, age: Int)

@main def main: Unit =
  val user = Builder[User]
    .withFirstName("Athena")
    .withAge(22)
    .build
  println(user.age)
  println(user)
