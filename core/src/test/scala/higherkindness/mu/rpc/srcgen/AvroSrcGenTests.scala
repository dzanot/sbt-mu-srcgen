/*
 * Copyright 2020 47 Degrees <https://www.47deg.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package higherkindness.mu.rpc.srcgen

import cats.data.Validated
import cats.data.Validated.Valid
import higherkindness.mu.rpc.srcgen.AvroScalaGeneratorArbitrary._
import higherkindness.mu.rpc.srcgen.Model.SerializationType.Avro
import higherkindness.mu.rpc.srcgen.Model._
import higherkindness.mu.rpc.srcgen.avro._
import higherkindness.skeuomorph.mu.CompressionType
import org.scalacheck.Prop.forAll
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.Checkers
import scala.io._

class AvroSrcGenTests extends AnyWordSpec with Matchers with OneInstancePerTest with Checkers {

  "Avro Scala Generator" should {

    "generate correct Scala classes" in {
      check {
        forAll { scenario: Scenario => test(scenario) }
      }
    }

    "return a non-empty list of errors instead of generating code from an invalid IDL file" in {
      val response = {
        AvroSrcGenerator(
          List(BigDecimalAvroMarshallers),
          ScalaBigDecimalTaggedGen,
          CompressionType.Identity
        ).generateFrom(
          Source.fromInputStream(getClass.getResourceAsStream("/avro/Invalid.avdl")).mkString,
          Avro
        )
      }

      response shouldBe Some(
        (
          "foo/bar/MyGreeterService.scala",
          Validated.invalidNel(
            "RPC method response parameter has non-record response type 'STRING'"
          )
        )
      )
    }
  }

  private def test(scenario: Scenario): Boolean = {
    val output =
      AvroSrcGenerator(
        scenario.marshallersImports,
        ScalaBigDecimalTaggedGen,
        scenario.compressionType,
        scenario.useIdiomaticEndpoints
      ).generateFrom(
        Source.fromInputStream(getClass.getResourceAsStream(scenario.inputResourcePath)).mkString,
        scenario.serializationType
      )
    output should not be empty
    output forall { case (filePath, contents) =>
      filePath shouldBe scenario.expectedOutputFilePath
      contents.map(_.filter(_.length > 0)) shouldBe Valid(scenario.expectedOutput)
      true
    }
  }

}
