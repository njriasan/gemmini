package gemmini

import chisel3._
import chisel3.util._
import GemminiISA._
import Util._
import freechips.rocketchip.config.Parameters


// TODO deal with errors when reading scratchpad responses
class LoadController[T <: Data](config: GemminiArrayConfig[T], coreMaxAddrBits: Int, local_addr_t: LocalAddr)
                               (implicit p: Parameters) extends Module {
  import config._

  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new GemminiCmd(rob_entries))) // Gets commands from CPU

    val dma = new ScratchpadReadMemIO(sp_bank_entries, local_addr_t) // Make DMA requests based on those commands

    val completed = Decoupled(UInt(log2Up(rob_entries).W))

    val busy = Output(Bool())
  })

  // Multiple states. First is just waiting. Second is to move in rows

  val waiting_for_command :: waiting_for_dma_req_ready :: sending_rows :: Nil = Enum(3)
  val control_state = RegInit(waiting_for_command)

  val stride = RegInit((sp_width / 8).U(coreMaxAddrBits.W))
  val precision_bits = RegInit((log2Ceil(config.inputType.getWidth)).U(3.W))

  val block_rows = meshRows * tileRows
  val block_cols = meshColumns * tileColumns
  val row_counter = RegInit(0.U(log2Ceil(block_rows).W))

  val cmd = Queue(io.cmd, ld_queue_length)
  val vaddr = cmd.bits.cmd.rs1
  val localaddr = cmd.bits.cmd.rs2.asTypeOf(local_addr_t)
  // RS2 is how many bits in rows and columns
  // Pulls out the actual register
  val cols = cmd.bits.cmd.rs2(32 + mvin_len_bits - 1, 32) // TODO magic numbers
  val rows = cmd.bits.cmd.rs2(48 + mvin_rows_bits, 48) // TODO magic numbers
  val config_stride = cmd.bits.cmd.rs2
  val config_precision_bits = cmd.bits.cmd.rs1(4, 2)
  val mstatus = cmd.bits.cmd.status

  io.busy := cmd.valid

  val DoConfig = cmd.bits.cmd.inst.funct === CONFIG_CMD
  val DoLoad = !DoConfig // TODO change this if more commands are added

  cmd.ready := false.B

  // Command tracker instantiation
  val nCmds = 2 // TODO make this a config parameter

  val deps_t = new Bundle {
    val rob_id = UInt(log2Up(rob_entries).W)
  }

  val maxBytesInRowRequest = config.dma_maxbytes max (block_cols * config.inputType.getWidth / 8) max
    (block_cols * config.accType.getWidth / 8)
  val maxBytesInMatRequest = block_rows * maxBytesInRowRequest

  val cmd_tracker = Module(new DMAReadCommandTracker(nCmds, maxBytesInMatRequest, deps_t))

  // DMA IO wiring
  io.dma.req.valid := (control_state === waiting_for_command && cmd.valid && DoLoad && cmd_tracker.io.alloc.ready) ||
    control_state === waiting_for_dma_req_ready ||
    (control_state === sending_rows && row_counter =/= 0.U)
  io.dma.req.bits.vaddr := vaddr + row_counter * stride
  io.dma.req.bits.laddr := localaddr
  io.dma.req.bits.offset := row_counter
  // Gives the number of elements we want to read from one row
  // Want this to reduce length to correspond to new bitwidth
  // Will just store a smaller length in the scratchpad

  io.dma.req.bits.len := cols >> (log2Ceil(config.inputType.getWidth).U - precision_bits) // PROJECT
  io.dma.req.bits.precision_bits := precision_bits
  io.dma.req.bits.status := mstatus

  val precision = 1.U(8.W) << precision_bits
  // Command tracker IO
  cmd_tracker.io.alloc.valid := control_state === waiting_for_command && cmd.valid && DoLoad
  cmd_tracker.io.alloc.bits.bytes_to_read :=
    Mux(localaddr.is_acc_addr, cols * rows * config.accType.getWidth.U, cols * rows * precision) / 8.U // PROJECT
  cmd_tracker.io.alloc.bits.tag.rob_id := cmd.bits.rob_id
  cmd_tracker.io.request_returned.valid := io.dma.resp.fire() // TODO use a bundle connect
  cmd_tracker.io.request_returned.bits.cmd_id := io.dma.resp.bits.cmd_id // TODO use a bundle connect
  cmd_tracker.io.request_returned.bits.bytes_read := io.dma.resp.bits.bytesRead
  cmd_tracker.io.cmd_completed.ready := io.completed.ready

  val cmd_id = RegEnableThru(cmd_tracker.io.alloc.bits.cmd_id, cmd_tracker.io.alloc.fire()) // TODO is this really better than a simple RegEnable?
  io.dma.req.bits.cmd_id := cmd_id

  io.completed.valid := cmd_tracker.io.cmd_completed.valid
  io.completed.bits := cmd_tracker.io.cmd_completed.bits.tag.rob_id

  // Row counter
  when (io.dma.req.fire()) {
    // Loop through all of our rows 1 by 1
    // Put 16 rows in 16 adjacent addresses in the scratchpad
    row_counter := wrappingAdd(row_counter, 1.U, rows)
  }

  // Control logic
  switch (control_state) {
    is (waiting_for_command) {
      when (cmd.valid) {
        // when(DoConfig && !cmd_tracker.io.cmd_completed.valid) {
        when(DoConfig) {
          stride := config_stride
          precision_bits := config_precision_bits
          cmd.ready := true.B
        }
        // Do Load is a move in command
        .elsewhen(DoLoad && cmd_tracker.io.alloc.fire()) {
          // This actually changes our state
          control_state := Mux(io.dma.req.fire(), sending_rows, waiting_for_dma_req_ready)
        }
      }
    }

    is (waiting_for_dma_req_ready) {
      when (io.dma.req.fire()) {
        control_state := sending_rows
      }
    }

    is (sending_rows) {
      val last_row = row_counter === 0.U || (row_counter === rows-1.U && io.dma.req.fire())

      when (last_row) {
        control_state := waiting_for_command
        cmd.ready := true.B
      }
    }
  }
}
