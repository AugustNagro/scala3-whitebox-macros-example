case class User(firstName: String, age: Int)

@main def main: Unit =
  // This works
  /*
  val user = Builder[User]
    .withFirstName("Athena")
    .withAge(22)
    .build
  println(user.age)
  println(user)
   */

  // But use the wrong method name, and scalac stack overflows.
  // I believe this is because the RecursiveType usage.
  val user = Builder[User].wrongName(22).build
  println(user)
