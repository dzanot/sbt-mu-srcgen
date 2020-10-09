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

import java.io.File

//import cats.data.Validated
import higherkindness.mu.rpc.srcgen.AvroScalaGeneratorArbitrary._
import higherkindness.mu.rpc.srcgen.Generator.Result
import higherkindness.mu.rpc.srcgen.Model.MonixObservable
//import higherkindness.mu.rpc.srcgen.Model.SerializationType.Avro
//import higherkindness.mu.rpc.srcgen.Model.{MonixObservable, NoCompressionGen, UseIdiomaticEndpoints}
import higherkindness.mu.rpc.srcgen.avro._
import org.scalacheck.Prop.forAll
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.Checkers

class AvroSrcGenTests extends AnyWordSpec with Matchers with OneInstancePerTest with Checkers {

  "Avro Scala Generator" should {

    "generate correct Scala classes" in {
      check {
        forAll { scenario: Scenario => {
          pprint.pprintln(s"Checking Scenario ${scenario}")
          test(scenario)
        } }
      }
    }

//    "return a non-empty list of errors instead of generating code from an invalid IDL file" in {
//      val response =
//        AvroSrcGeneratorSkeuomorph
//          .build(NoCompressionGen, UseIdiomaticEndpoints.trueV, MonixObservable)
//          .generateFrom(Set(new File(getClass.getResource("/avro/Invalid.avdl").toURI)), Avro)
//
//      response shouldBe List(
//        Some(
//          (
//            "foo/bar/MyGreeterService.scala",
//            Validated.invalidNel(
//              "Encountered an unsupported response type: Skeuomorph only supports Record types for Avro responses. Encountered response schema with type STRING"
//            )
//          )
//        )
//      )
//    }
  }

  private def test(scenario: Scenario): Boolean = {
    val results =
      AvroSrcGeneratorSkeuomorph
        .build(
          scenario.compressionTypeGen,
          scenario.useIdiomaticEndpoints,
          MonixObservable
        )
        .generateFrom(
          Set(new File(getClass.getResource(scenario.inputResourcePath).toURI)),
          scenario.serializationType
        )
    results should not be empty
    results forall { case Result(_, errorsOrOutput) =>
      errorsOrOutput.map(output => {
        output.path.toString shouldBe scenario.expectedOutputFilePath
        output.contents shouldBe scenario.expectedOutput
      })
      true
    }
  }

}
