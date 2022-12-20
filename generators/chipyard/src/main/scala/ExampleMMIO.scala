package chipyard.example

import chisel3._
import chisel3.util._
import chisel3.experimental.{IntParam, BaseModule}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.UIntIsOneOf

case class VecAddParams(
  address: BigInt = 0x2000,
  width: Int = 32,
  useAXI4: Boolean = false,
  useBlackBox: Boolean = true)

case object VecAddKey extends Field[Option[VecAddParams]](None)

class VecAddIO(val w: Int) extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val input_ready = Output(Bool())
  val input_valid = Input(Bool())
  val x = Input(UInt(w.W))
  val y = Input(UInt(w.W))
  val output_ready = Input(Bool())
  val output_valid = Output(Bool())
  val vec_add = Output(UInt(w.W))
  val busy = Output(Bool())
}

trait VecAddTopIO extends Bundle {
  val vec_add_busy = Output(Bool())
}

trait HasVecAddIO extends BaseModule {
  val w: Int
  val io = IO(new VecAddIO(w))
}

/* Where the Accelerator Logic Goes */
class VecAddMMIOChiselModule(val w: Int) extends Module
  with HasVecAddIO
{
  val s_idle :: s_run :: s_done :: Nil = Enum(3)

  val state = RegInit(s_idle)
  val temp_x   = Reg(UInt(w.W))
  val temp_y   = Reg(UInt(w.W))
  val vec_add   = Reg(UInt(w.W))
  val des = Wire(Vec(4, UInt(8.W)))
  for (i <- 0 until 4) {
      des(i) := 0.U
    }
  

  io.input_ready := state === s_idle
  io.output_valid := state === s_done
  io.vec_add := vec_add

  when (state === s_idle && io.input_valid) {
    state := s_run
  } .elsewhen (state === s_run) {
    state := s_done
  } .elsewhen (state === s_done && io.output_ready) {
    state := s_idle
  }

  when (state === s_idle && io.input_valid) {
    temp_x := io.x
    temp_y := io.y
  } .elsewhen (state === s_run) {
    for (i <- 0 until 4) {
        val byte = temp_x((i + 1)*4 - 1, 4 * i) + temp_y((i + 1)*4 - 1, 4 * i)
        des(i) := Mux(byte > 255.U, 255.U, byte)
    }
  }

  io.busy := state =/= s_idle
}

/* Wrap ChiselModule & regmaps */
trait VecAddModule extends HasRegMap {
  val io: VecAddTopIO

  implicit val p: Parameters
  def params: VecAddParams
  val clock: Clock
  val reset: Reset


  // How many clock cycles in a PWM cycle?
  val x = Reg(UInt(params.width.W))
  val y = Wire(new DecoupledIO(UInt(params.width.W)))
  val vec_add = Wire(new DecoupledIO(UInt(params.width.W)))
  val status = Wire(UInt(2.W))

  val impl =  Module(new VecAddMMIOChiselModule(params.width))

  impl.io.clock := clock
  impl.io.reset := reset.asBool

  impl.io.x := x
  impl.io.y := y.bits
  impl.io.input_valid := y.valid
  y.ready := impl.io.input_ready

  vec_add.bits := impl.io.vec_add
  vec_add.valid := impl.io.output_valid
  impl.io.output_ready := vec_add.ready

  status := Cat(impl.io.input_ready, impl.io.output_valid)
  io.vec_add_busy := impl.io.busy

  regmap(
    0x00 -> Seq(
      RegField.r(2, status)), // a read-only register capturing current status
    0x04 -> Seq(
      RegField.w(params.width, x)), // a plain, write-only register
    0x08 -> Seq(
      RegField.w(params.width, y)), // write-only, y.valid is set on write
    0x0C -> Seq(
      RegField.r(params.width, vec_add))) // read-only, vec_add.ready is set on read
}


/* Connecting by TileLink */
class VecAddTL(params: VecAddParams, beatBytes: Int)(implicit p: Parameters)
  extends TLRegisterRouter(
    params.address, "vecadd", Seq("ucbbar,vecadd"),
    beatBytes = beatBytes)(
      new TLRegBundle(params, _) with VecAddTopIO)(
      new TLRegModule(params, _, _) with VecAddModule)

class VecAddAXI4(params: VecAddParams, beatBytes: Int)(implicit p: Parameters)
  extends AXI4RegisterRouter(
    params.address,
    beatBytes=beatBytes)(
      new AXI4RegBundle(params, _) with VecAddTopIO)(
      new AXI4RegModule(params, _, _) with VecAddModule)

/* Top-level traits */
trait CanHavePeripheryVecAdd { this: BaseSubsystem =>
  private val portName = "vecadd"

  val vecadd = p(VecAddKey) match {
    case Some(params) => {
      if (params.useAXI4) {
        val vecadd = LazyModule(new VecAddAXI4(params, pbus.beatBytes)(p))
        pbus.toSlave(Some(portName)) {
          vecadd.node :=
          AXI4Buffer () :=
          TLToAXI4 () :=
          TLFragmenter(pbus.beatBytes, pbus.blockBytes, holdFirstDeny = true)
        }
        Some(vecadd)
      } else {
        val vecadd = LazyModule(new VecAddTL(params, pbus.beatBytes)(p))
        pbus.toVariableWidthSlave(Some(portName)) { vecadd.node }
        Some(vecadd)
      }
    }
    case None => None
  }
}

trait CanHavePeripheryVecAddModuleImp extends LazyModuleImp {
  val outer: CanHavePeripheryVecAdd
  val vecadd_busy = outer.vecadd match {
    case Some(vecadd) => {
      val busy = IO(Output(Bool()))
      busy := vecadd.module.io.vec_add_busy
      Some(busy)
    }
    case None => None
  }
}

/* config fragment definition */
class WithVecAdd(useAXI4: Boolean = false, useBlackBox: Boolean = false) extends Config((site, here, up) => {
  case VecAddKey => Some(VecAddParams(useAXI4 = useAXI4, useBlackBox = useBlackBox))
})
