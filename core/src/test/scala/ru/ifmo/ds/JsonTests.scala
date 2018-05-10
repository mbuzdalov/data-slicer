package ru.ifmo.ds

import org.scalatest.{FlatSpec, Matchers}
import ru.ifmo.ds.io.JsonDatabaseLoader

class JsonTests extends FlatSpec with Matchers {
  "an empty JSON" should "fail to be read" in {
    a [JsonDatabaseLoader.ParseException] should be thrownBy {
      JsonDatabaseLoader.loadFromString("")
    }
  }

  "an empty JSON array" should "transform into an empty database" in {
    val db = JsonDatabaseLoader.loadFromString("[]")
    db.possibleKeys shouldBe empty
    db.entries shouldBe empty
  }

  "an empty JSON object" should "transform into a database with an empty object" in {
    val db = JsonDatabaseLoader.loadFromString("{}")
    db.possibleKeys shouldBe empty
    db.entries.size shouldEqual 1
  }

  "a JSON object with a single key-value pair without the array key" should "be parsed OK with one record and some keys" in {
    val db = JsonDatabaseLoader.loadFromString("""{"key":"value"}""")
    db.possibleKeys shouldEqual Set("key")
    db.entries.size shouldEqual 1
    db.entries.head("key") shouldEqual "value"
  }

  "a JSON object with multiple single key-value pairs without the array key" should "be parsed OK with one record and some keys" in {
    val db = JsonDatabaseLoader.loadFromString("""{"key1":"value", "key2":42, "key3":"not a value", "key4":4.5, "key5":false, "key6":null}""")
    db.possibleKeys shouldEqual Set("key1", "key2", "key3", "key4", "key5", "key6")
    db.entries.size shouldEqual 1
    val entry = db.entries.head
    entry("key1") shouldEqual "value"
    entry("key2") shouldEqual "42"
    entry("key3") shouldEqual "not a value"
    entry("key4") shouldEqual "4.5"
    entry("key5") shouldEqual "false"
    entry("key6") shouldBe null
  }

  "a typical JSON input" should "be parsed OK" in {
    val db = JsonDatabaseLoader.loadFromString(
      """{
        |  "author":"John Smith",
        |  "date":"2018.05.10",
        |  "os":"Linux",
        |  "jvm":"1.8.0_172",
        |  "measurements":[
        |    {
        |      "generator":{
        |         "type":"uniform.hypercube"
        |      },
        |      "runsForGenerator":[
        |        {
        |          "nPoints":100,
        |          "dimension":4,
        |          "runsForDataset":[
        |            {
        |              "algorithm":"jfb.rbtree",
        |              "runsForAlgorithm":[
        |                { "fork":0, "time":1.534436e-6 },
        |                { "fork":1, "time":1.567734e-6 },
        |                { "fork":2, "time":1.548824e-6 }
        |              ]
        |            }, {
        |              "algorithm":"ens.ndt",
        |              "thisCanBeAnything":[
        |                { "fork":0, "time":1.134325e-6 },
        |                { "fork":1, "time":1.122452e-6 },
        |                { "fork":2, "time":1.114536e-6 }
        |              ]
        |            }
        |          ]
        |        }, {
        |          "nPoints":200,
        |          "dimension":4,
        |          "doNotCareForNames":[
        |            {
        |              "algorithm":"jfb.rbtree",
        |              "foo":[
        |                { "fork":0, "time":3.125346e-6 },
        |                { "fork":1, "time":3.145246e-6 },
        |                { "fork":2, "time":3.136284e-6 }
        |              ]
        |            }, {
        |              "algorithm":"ens.ndt",
        |              "bar":[
        |                { "fork":0, "time":2.372635e-6 },
        |                { "fork":1, "time":2.418476e-6 },
        |                { "fork":2, "time":2.387264e-6 }
        |              ]
        |            }
        |          ]
        |        }
        |      ]
        |    }
        |  ]
        |}
      """.stripMargin)
    db.possibleKeys shouldEqual Set("author", "date", "os", "jvm", "generator.type",
                                    "nPoints", "dimension", "algorithm", "fork", "time")
    val entries = db.entries
    entries.size shouldEqual 12

    entries foreach { e =>
      e("os") shouldEqual "Linux"
      e("jvm") shouldEqual "1.8.0_172"
      e("date") shouldEqual "2018.05.10"
      e("author") shouldEqual "John Smith"
      e("dimension") shouldEqual "4"
      e("generator.type") shouldEqual "uniform.hypercube"
    }

    entries.slice(0, 6).foreach(_("nPoints") shouldEqual "100")
    entries.slice(6,12).foreach(_("nPoints") shouldEqual "200")
    entries.slice(0, 3).foreach(_("algorithm") shouldEqual "jfb.rbtree")
    entries.slice(3, 6).foreach(_("algorithm") shouldEqual "ens.ndt")
    entries.slice(6, 9).foreach(_("algorithm") shouldEqual "jfb.rbtree")
    entries.slice(9,12).foreach(_("algorithm") shouldEqual "ens.ndt")

    val expectedTimes = Seq("1.534436e-6", "1.567734e-6", "1.548824e-6",
                            "1.134325e-6", "1.122452e-6", "1.114536e-6",
                            "3.125346e-6", "3.145246e-6", "3.136284e-6",
                            "2.372635e-6", "2.418476e-6", "2.387264e-6")

    entries.zipWithIndex foreach { case (e, i) =>
      e("fork") shouldEqual String.valueOf(i % 3)
      e("time") shouldEqual expectedTimes(i)
    }
  }

  "a JSON input with an array with non-objects" should "be parsed OK" in {
    val db = JsonDatabaseLoader.loadFromString("""{"author":"me","measurements":[1,2,3,4,5]}""")
    db.possibleKeys shouldEqual Set("author", "measurements")
    db.entries.size shouldEqual 5
    db.entries.zipWithIndex foreach { case (e, i) =>
      e("author") shouldEqual "me"
      e("measurements") shouldEqual String.valueOf(i + 1)
    }
  }

  "a JSON input with extra keys" should "be loaded and parsed OK" in {
    val db = JsonDatabaseLoader.loadFromString(
      contents = """{"author":"me","measurements":[1,2,3,4,5]}""",
      moreKeys = Map("tag" -> "42")
    )
    db.possibleKeys shouldEqual Set("author", "measurements", "tag")
    db.entries.size shouldEqual 5
    db.entries.zipWithIndex foreach { case (e, i) =>
      e("tag") shouldEqual "42"
      e("author") shouldEqual "me"
      e("measurements") shouldEqual String.valueOf(i + 1)
    }
  }
}
