package bigtop.json

case class JsonException(val errors: JsonErrors) extends Exception
