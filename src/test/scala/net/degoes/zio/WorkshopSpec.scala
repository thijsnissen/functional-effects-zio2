package net.degoes.zio

import zio.*
import zio.test.*
import zio.test.TestAspect.{ ignore, timeout }

object WorkshopSpec extends ZIOSpecDefault {

  def spec = suite("WorkshopSpec")()
}
