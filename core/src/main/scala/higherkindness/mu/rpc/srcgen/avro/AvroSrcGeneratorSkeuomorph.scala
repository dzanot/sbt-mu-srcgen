package higherkindness.mu.rpc.srcgen.avro

import java.io.File

import avrohugger.format.Standard
import avrohugger.input.parsers.FileInputParser
import avrohugger.stores.ClassStore
import higherkindness.mu.rpc.srcgen.Model.{CompressionTypeGen, Fs2Stream, GzipGen, MonixObservable, NoCompressionGen, StreamingImplementation, UseIdiomaticEndpoints}
import higherkindness.mu.rpc.srcgen.{ErrorsOr, Model, ScalaFileExtension, SrcGenerator}
import org.apache.avro.Protocol
import cats.implicits._
import higherkindness.droste.data.Mu
import higherkindness.skeuomorph.avro.{AvroF, Protocol => AvroProtocol}
import higherkindness.skeuomorph.mu.{CompressionType, MuF, codegen, Protocol => MuProtocol}

import scala.meta._

object AvroSrcGeneratorSkeuomorph {
  def build(
             compressionTypeGen: CompressionTypeGen,
             useIdiomaticEndpoints: UseIdiomaticEndpoints,
             streamingImplementation: StreamingImplementation,
             idlTargetDir: File
           ): SrcGenerator = new SrcGenerator {
    private val _ = idlTargetDir
    private val classStore = new ClassStore
    private val classLoader = getClass.getClassLoader
    override def idlType: Model.IdlType = Model.IdlType.Avro

    override protected def inputFiles(files: Set[File]): List[File] =
      files.filter(_.getName.endsWith(AvdlExtension)).toList

    override protected def generateFrom(inputFile: File, serializationType: Model.SerializationType): Option[(String, ErrorsOr[List[String]])] = {
      val nativeAvroProtocol: ErrorsOr[Protocol] =
        (new FileInputParser)
          .getSchemaOrProtocols(inputFile, Standard, classStore, classLoader)
        .collect{ case Right(protocol) => protocol } match {
          case first :: _ => first.validNel //multiple protocols are returned when imports are present. We assume the first one is the one defined in our file
          case Nil => s"No protocol definition found in ${inputFile}".invalidNel
        }

      val skeuomorphAvroProtocol: ErrorsOr[AvroProtocol[Mu[AvroF]]] =
        nativeAvroProtocol.map(p => AvroProtocol.fromProto[Mu[AvroF]](p))

      val skeuomorphCompression: CompressionType = compressionTypeGen match {
        case GzipGen          => CompressionType.Gzip
        case NoCompressionGen => CompressionType.Identity
      }

      val source = skeuomorphAvroProtocol.map { sap =>
        val muProtocol: MuProtocol[Mu[MuF]] =
          MuProtocol.fromAvroProtocol(skeuomorphCompression, useIdiomaticEndpoints)(sap)

        val outputFilePath = getPath(sap)

        val streamCtor: (Type, Type) => Type.Apply = streamingImplementation match {
          case Fs2Stream => {
            case (f, a) => t"_root_.fs2.Stream[$f, $a]"
          }
          case MonixObservable => {
            case (_, a) => t"_root_.monix.reactive.Observable[$a]"
          }
        }


        val stringified: ErrorsOr[List[String]] =
          codegen.protocol(muProtocol, streamCtor).toValidatedNel
          .map(_.syntax.split("\n").toList)

        (outputFilePath, stringified)
      }

      source.toOption
    }
  }
  private def getPath(p: AvroProtocol[Mu[AvroF]]): String = {
    val path: List[String] = p.namespace.map(_.split('.').toList).toList.flatten :+ s"${p.name}${ScalaFileExtension}"
    path.mkString(File.separator)
  }

}
