package gemmini

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink.{TLIdentityNode, TLXbar}
import Util._


// class ScratchpadMemReadRequest(val nBanks: Int, val nRows: Int, val acc_rows: Int)
class ScratchpadMemReadRequest(local_addr_t: LocalAddr)
                              (implicit p: Parameters) extends CoreBundle {
  val vaddr = UInt(coreMaxAddrBits.W)
  val laddr = local_addr_t.cloneType

  val len = UInt(8.W) // TODO don't use a magic number for the width here

  val cmd_id = UInt(8.W) // TODO don't use a magic number here

  val precision_bits = UInt(3.W)

  val status = new MStatus

  // val offset = UInt(n.W)

  override def cloneType: this.type = new ScratchpadMemReadRequest(local_addr_t).asInstanceOf[this.type]
}

// class ScratchpadMemWriteRequest(val nBanks: Int, val nRows: Int, val acc_rows: Int)
class ScratchpadMemWriteRequest(local_addr_t: LocalAddr)
                              (implicit p: Parameters) extends CoreBundle {
  val vaddr = UInt(coreMaxAddrBits.W)
  val laddr = local_addr_t.cloneType

  val len = UInt(8.W) // TODO don't use a magic number for the width here

  val cmd_id = UInt(8.W) // TODO don't use a magic number here

  val status = new MStatus

  val precision_bits = UInt(3.W)


  override def cloneType: this.type = new ScratchpadMemWriteRequest(local_addr_t).asInstanceOf[this.type]
}

class ScratchpadMemWriteResponse extends Bundle {
  val cmd_id = UInt(8.W) // TODO don't use a magic number here
}

class ScratchpadMemReadResponse extends Bundle {
  val bytesRead = UInt(16.W) // TODO magic number here
  val cmd_id = UInt(8.W) // TODO don't use a magic number here
}

// class ScratchpadReadMemIO(val nBanks: Int, val nRows: Int, val acc_rows: Int)
class ScratchpadReadMemIO(local_addr_t: LocalAddr)
                         (implicit p: Parameters) extends CoreBundle {
  val req = Decoupled(new ScratchpadMemReadRequest(local_addr_t.cloneType))
  val resp = Flipped(Valid(new ScratchpadMemReadResponse))

  override def cloneType: this.type = new ScratchpadReadMemIO(local_addr_t.cloneType).asInstanceOf[this.type]
}

// class ScratchpadWriteMemIO(val nBanks: Int, val nRows: Int, val acc_rows: Int)
class ScratchpadWriteMemIO(local_addr_t: LocalAddr)
                         (implicit p: Parameters) extends CoreBundle {
  val req = Decoupled(new ScratchpadMemWriteRequest(local_addr_t.cloneType))
  val resp = Flipped(Valid(new ScratchpadMemWriteResponse))

  override def cloneType: this.type = new ScratchpadWriteMemIO(local_addr_t.cloneType).asInstanceOf[this.type]
}

class ScratchpadReadReq(val n: Int, val input_width: Int) extends Bundle {
  val addr = UInt(log2Ceil(n).W)
  val fromDMA = Bool()
  // Add starting and ending precision
  val precision_bits = UInt(3.W)
  val subrow = UInt(input_width.W) // which subrow to write to
}

// New Class for pipelining precision bits
class PipelinedPrecisionBits(val n: Int, val input_width: Int) extends Bundle {
  val fromDMA = Bool()
  val addr = UInt(log2Ceil(n).W)
  val starting_precision_bits = UInt(3.W)
  val ending_precision_bits = UInt(3.W)
  val subrow = UInt(input_width.W) // which subrow to write to
  // val offset = UInt(n.W) // offset from the base addr
}

// New Class for pipelining memory to later edit
class PipelinedScratchpadReadResp(val w: Int, val input_width: Int) extends Bundle {
  val body = new ScratchpadReadResp(w)
  val starting_precision_bits = UInt(3.W)
  val ending_precision_bits = UInt(3.W)
  val subrow = UInt(input_width.W) // which subrow to write to
}

class ScratchpadReadResp(val w: Int) extends Bundle {
  val data = UInt(w.W)
  val fromDMA = Bool()
}

class ScratchpadReadIO(val n: Int, val w: Int, val input_width: Int) extends Bundle {
  val req = Decoupled(new ScratchpadReadReq(n, input_width))
  val resp = Flipped(Decoupled(new ScratchpadReadResp(w)))
}

class ScratchpadWriteIO(val n: Int, val w: Int, val mask_len: Int, val input_width: Int) extends Bundle {
  val en = Output(Bool())
  val addr = Output(UInt(log2Ceil(n).W))
  val mask = Output(Vec(mask_len, Bool()))
  val data = Output(UInt(w.W))
  val precision_bits = Output(UInt(3.W)) // Project. Magic Number. In theory this should be able to support up to 128 bit precision
  val subrow = Output(UInt(input_width.W)) // which subrow to write to
}

class ScratchpadBank(n: Int, w: Int, mem_pipeline: Int, aligned_to: Int, max_precision: Int, input_width: Int) extends Module {
  // This is essentially a pipelined SRAM with the ability to stall pipeline stages

  require(w % aligned_to == 0 || w < aligned_to)
  val mask_len = (w / (aligned_to * 8)) max 1 // How many mask bits are there?
  val mask_elem = UInt((w min (aligned_to * 8)).W) // What datatype does each mask bit correspond to?

  val io = IO(new Bundle {
    val read = Flipped(new ScratchpadReadIO(n, w, input_width))
    val write = Flipped(new ScratchpadWriteIO(n, w, mask_len, input_width))
  })

  // val mem = SyncReadMem(n, UInt(w.W))
  val mem = SyncReadMem(n, Vec(mask_len, mask_elem))
  val precision_bits = SyncReadMem(n, UInt(3.W))

  when (io.write.en) {
    if (aligned_to >= w) {
      mem.write(io.write.addr, io.write.data.asTypeOf(Vec(mask_len, mask_elem)))
    }
    else {
      // Calculate number of subrows but in terms of 2^n
      val num_subrow_bits = log2Ceil(input_width).U - io.write.precision_bits
      val subrow = io.write.subrow >> io.write.precision_bits
      /*
       * num_subrows  subrow  1 << sr  mask (for mask_len=16)
       * 1            0       0001     1111111111111111
       * 2            0       0001     1111111100000000
       * 2            1       0010     0000000011111111
       * 4            0       0001     1111000000000000
       * 4            1       0010     0000111100000000
       * 4            2       0100     0000000011110000
       * 4            3       1000     0000000000001111
       */
      // Nick's changes to split the code
      // Divide mask len by num subrows to get the len of each segment
      val mask_segment_len = mask_len.U >> num_subrow_bits
      // Calculate the smallest value that should be in the mask
      val mask_bottom = subrow * mask_segment_len
      // Calculate the smallest value that should not be included
      val mask_top = (subrow + 1.U) * mask_segment_len
      val mask = VecInit(Seq.fill(mask_len)(false.B))
      var index = log2Ceil(input_width)
      while (index >= 0) {
        when (index.U === num_subrow_bits) {
          val modulo = mask_len >> index 
          for (i <- 0 until mask_len) {
            mask(i) := io.write.mask(i % modulo) & ((i.U >= mask_bottom) && (i.U < mask_top)) 
          }
        }
        index = index - 1
      }
      mem.write(io.write.addr, io.write.data.asTypeOf(Vec(mask_len, mask_elem)), mask)
    }
    // TODO should this be inside the if?
    precision_bits.write(io.write.addr, io.write.precision_bits)
  }
  /*
  // Make a queue for the precision bits access
  val early_q = Module(new Queue(new PipelinedPrecisionBits(n, log2Ceil(max_precision)), 1, true, true))
  early_q.io.enq.valid := RegNext(ren)
  early_q.io.enq.bits.fromDMA := RegNext(fromDMA)
  early_q.io.enq.bits.addr := RegNext(raddr)
  early_q.io.enq.bits.starting_precision_bits := precision_bits.read(raddr, ren)
  early_q.io.enq.bits.ending_precision_bits := RegNext(io.read.req.bits.precision_bits)
  early_q.io.enq.bits.subrow := RegNext(io.read.req.bits.subrow)
  // early_q.io.enq.bits.subrow := RegNext(io.read.req.bits.offset)
  val early_q_will_be_empty = (early_q.io.count +& early_q.io.enq.fire()) - early_q.io.deq.fire() === 0.U
  io.read.req.ready := early_q_will_be_empty

  // Build the rest of the resp pipeline
  val precision_p = Pipeline(early_q.io.deq, mem_pipeline)

  // val newAddr = precision_p.bits.addr + (precision_p.bits.offset >> (log2Ceil(max_precision) -  precision_p.bits.starting_precision_bits))

  // val rdata = mem.read(newAddr, precision_p.fire()).asUInt()
  val rdata = mem.read(precision_p.bits.addr, precision_p.fire()).asUInt()
  // Make a queue which buffers the result of an SRAM read if it can't immediately be consumed
  val q = Module(new Queue(new PipelinedScratchpadReadResp(w, log2Ceil(max_precision)), 1, true, true))
  q.io.enq.valid := precision_p.valid
  q.io.enq.bits.body.fromDMA := precision_p.bits.fromDMA
  q.io.enq.bits.body.data := rdata
  q.io.enq.bits.starting_precision_bits := precision_p.bits.starting_precision_bits

  q.io.enq.bits.ending_precision_bits := precision_p.bits.ending_precision_bits
  q.io.enq.bits.subrow := precision_p.bits.subrow

  val q_will_be_empty = (q.io.count +& q.io.enq.fire()) - q.io.deq.fire() === 0.U
  precision_p.ready := q_will_be_empty
  */

  val fromDMA = io.read.req.bits.fromDMA
  val raddr = io.read.req.bits.addr
  val ren = io.read.req.fire()
  val rdata = mem.read(raddr, ren).asUInt()

  // Make a queue which buffers the result of an SRAM read if it can't immediately be consumed
  val q = Module(new Queue(new PipelinedScratchpadReadResp(w, log2Ceil(max_precision)), 1, true, true))
  q.io.enq.valid := RegNext(ren)
  q.io.enq.bits.body.fromDMA := RegNext(fromDMA)
  q.io.enq.bits.body.data := rdata
  q.io.enq.bits.starting_precision_bits := precision_bits.read(raddr, ren)
  q.io.enq.bits.ending_precision_bits := RegNext(io.read.req.bits.precision_bits)
  q.io.enq.bits.subrow := RegNext(io.read.req.bits.subrow)

  val q_will_be_empty = (q.io.count +& q.io.enq.fire()) - q.io.deq.fire() === 0.U
  io.read.req.ready := q_will_be_empty

  // Build the rest of the resp pipeline
  val rdata_p = Pipeline(q.io.deq, mem_pipeline)
  // TODO do expansion on rdata_p instead
  io.read.resp.valid := rdata_p.valid
  io.read.resp.bits.fromDMA := rdata_p.bits.body.fromDMA
  rdata_p.ready := io.read.resp.ready

  // Calculate subrow
  val read_subrow = rdata_p.bits.subrow >> rdata_p.bits.starting_precision_bits

  // TODO Move the compression/decompression after the pipeline stage
  val stored_precision = 1.U(8.W) << rdata_p.bits.starting_precision_bits
  val output_precision = 1.U(8.W) << rdata_p.bits.ending_precision_bits
  val output_data = VecInit(Seq.fill(w)(0.U(1.W)))
  var output_ctr = max_precision
  while (output_ctr > 0) { // Replace this magic number.
    val max_val = Wire(SInt(output_ctr.W))
    max_val := ((1.U << (output_ctr - 1).U) - 1.U).asSInt()
    val min_val = ~max_val
    var input_ctr = max_precision
    while (input_ctr > 0) { // Replace this magic number.
      when(input_ctr.U === stored_precision && output_ctr.U === output_precision) {
        val num_subrow_bits = log2Ceil(input_width/ input_ctr)
        val segment_len = w >> num_subrow_bits
        val data_intermediate = WireDefault(0.U(w.W))
        for (i <- 0 until (1 << num_subrow_bits)) {
          when (i.U === read_subrow) {
            data_intermediate := Cat(0.U(w), rdata_p.bits.body.data(((i + 1) * segment_len) - 1, i * segment_len))
          }
        } 
        val max_bits = if (input_ctr > output_ctr) input_ctr else output_ctr
        for (i <- 0 until w / max_precision) {
          val element = (data_intermediate(((i + 1) * input_ctr) - 1, i * input_ctr)).asSInt()
          val threshold_output = Wire(SInt(max_bits.W)) 
          when (element > max_val) {
            threshold_output := max_val
          }.elsewhen (element < min_val) {
            threshold_output := min_val
          }.otherwise{
            threshold_output := element
          }
          // Data bits will give us an expand if input_ctr < output_ctr otherwise a compress
          for (l <- 0 until output_ctr) {
            output_data((i * output_ctr) + l) := threshold_output(l)
          }
        }
      }
      input_ctr = input_ctr / 2
    }
    output_ctr = output_ctr / 2
  }
  io.read.resp.bits.data := output_data.asUInt()
  // Project end

}

// TODO find a more elegant way to move data into accumulator
// TODO replace the SRAM types with Vec[Vec[inputType]], rather than just simple UInts
// TODO support unaligned accesses, for both multiple and single matrix loads
// TODO scratchpad is currently broken when one row is larger than dataBits. The requests arrive out-of-order, meaning that half of one row might arrive after the first have of another row. Some kind of re-ordering buffer may be needed
class Scratchpad[T <: Data: Arithmetic](config: GemminiArrayConfig[T])
    (implicit p: Parameters) extends LazyModule {

  import config._

  val maxBytes = dma_maxbytes
  val dataBits = dma_buswidth

  val block_rows = meshRows * tileRows
  val block_cols = meshColumns * tileColumns
  val spad_w = inputType.getWidth *  block_cols
  val acc_w = accType.getWidth * block_cols

  val id_node = TLIdentityNode()
  val xbar_node = TLXbar()

  val reader = LazyModule(new StreamReader(config, max_in_flight_reqs, dataBits, maxBytes, spad_w, acc_w, aligned_to,
    sp_banks * sp_bank_entries, acc_banks * acc_bank_entries, block_rows))
  val writer = LazyModule(new StreamWriter(max_in_flight_reqs, dataBits, maxBytes, spad_w, aligned_to))

  // TODO make a cross-bar vs two separate ports a config option
  // id_node :=* reader.node
  // id_node :=* writer.node

  xbar_node := reader.node // TODO
  xbar_node := writer.node
  id_node := xbar_node

  lazy val module = new LazyModuleImp(this) with HasCoreParameters {
    val io = IO(new Bundle {
      // DMA ports
      val dma = new Bundle {
        val read = Flipped(new ScratchpadReadMemIO(local_addr_t))
        val write = Flipped(new ScratchpadWriteMemIO(local_addr_t))
      }

      // SRAM ports
      val srams = new Bundle {
        val read = Flipped(Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, spad_w, log2Ceil(config.inputType.getWidth))))
        val write = Flipped(Vec(sp_banks, new ScratchpadWriteIO(sp_bank_entries, spad_w, (spad_w / (aligned_to * 8)) max 1, config.inputType.getWidth)))
      }

      // Accumulator ports
      // val acc = new AccumulatorMemIO(acc_bank_entries, Vec(meshColumns, Vec(tileColumns, accType)), Vec(meshColumns, Vec(tileColumns, inputType)))
      val acc = new Bundle {
        val read = Flipped(Vec(acc_banks, new AccumulatorReadIO(acc_bank_entries, log2Up(accType.getWidth), Vec(meshColumns, Vec(tileColumns, inputType)))))
        val write = Flipped(Vec(acc_banks, new AccumulatorWriteIO(acc_bank_entries, Vec(meshColumns, Vec(tileColumns, accType)))))
      }

      // TLB ports
      val tlb = Vec(2, new FrontendTLBIO)

      // Misc. ports
      val busy = Output(Bool())
      val flush = Input(Bool())
    })

    val write_dispatch_q = Queue(io.dma.write.req)

    write_dispatch_q.ready := false.B

    val write_issue_q = Module(new Queue(new ScratchpadMemWriteRequest(local_addr_t), mem_pipeline+1, pipe=true))
    // this is just the queue of DMA requests
    val read_issue_q = Module(new Queue(new ScratchpadMemReadRequest(local_addr_t), mem_pipeline+1, pipe=true)) // TODO can't this just be a normal queue?

    write_issue_q.io.enq.valid := false.B
    write_issue_q.io.enq.bits := write_dispatch_q.bits

    val writeData = Wire(Valid(UInt((spad_w max acc_w).W)))
    writeData.valid := false.B
    writeData.bits := DontCare

    writer.module.io.req.valid := write_issue_q.io.deq.valid && writeData.valid
    write_issue_q.io.deq.ready := writer.module.io.req.ready && writeData.valid
    writer.module.io.req.bits.vaddr := write_issue_q.io.deq.bits.vaddr
    writer.module.io.req.bits.len := write_issue_q.io.deq.bits.len * (inputType.getWidth / 8).U
    writer.module.io.req.bits.data := writeData.bits
    writer.module.io.req.bits.status := write_issue_q.io.deq.bits.status

    io.dma.write.resp.valid := false.B
    io.dma.write.resp.bits.cmd_id := write_dispatch_q.bits.cmd_id

    read_issue_q.io.enq <> io.dma.read.req

    reader.module.io.req.valid := read_issue_q.io.deq.valid
    read_issue_q.io.deq.ready := reader.module.io.req.ready
    reader.module.io.req.bits.vaddr := read_issue_q.io.deq.bits.vaddr
    reader.module.io.req.bits.spaddr := Mux(read_issue_q.io.deq.bits.laddr.is_acc_addr,
      read_issue_q.io.deq.bits.laddr.full_acc_addr(), read_issue_q.io.deq.bits.laddr.sp_row_addr())
    reader.module.io.req.bits.len := read_issue_q.io.deq.bits.len
    reader.module.io.req.bits.is_acc := read_issue_q.io.deq.bits.laddr.is_acc_addr
    reader.module.io.req.bits.status := read_issue_q.io.deq.bits.status
    reader.module.io.req.bits.cmd_id := read_issue_q.io.deq.bits.cmd_id
    // PROJECT TODO wire up precision here
    reader.module.io.req.bits.precision_bits := read_issue_q.io.deq.bits.precision_bits
    reader.module.io.req.bits.subrow := read_issue_q.io.deq.bits.laddr.sp_subrow()

    reader.module.io.resp.ready := false.B
    io.dma.read.resp.valid := reader.module.io.resp.fire() && reader.module.io.resp.bits.last
    io.dma.read.resp.bits.cmd_id := reader.module.io.resp.bits.cmd_id
    io.dma.read.resp.bits.bytesRead := reader.module.io.resp.bits.bytes_read

    io.tlb(0) <> writer.module.io.tlb
    io.tlb(1) <> reader.module.io.tlb

    writer.module.io.flush := io.flush
    reader.module.io.flush := io.flush

    io.busy := writer.module.io.busy || reader.module.io.busy || write_issue_q.io.deq.valid

    {
      val banks = Seq.fill(sp_banks) { Module(new ScratchpadBank(sp_bank_entries, spad_w, mem_pipeline, aligned_to, inputType.getWidth, inputType.getWidth)) }
      val bank_ios = VecInit(banks.map(_.io))

      // Getting the output of the bank that's about to be issued to the writer
      val bank_issued_io = bank_ios(write_issue_q.io.deq.bits.laddr.sp_bank())

      when (!write_issue_q.io.deq.bits.laddr.is_acc_addr) {
        writeData.valid := bank_issued_io.read.resp.valid && bank_issued_io.read.resp.bits.fromDMA
        // Here is where data is transferred
        writeData.bits := bank_issued_io.read.resp.bits.data
      }

      // Reading from the SRAM banks
      bank_ios.zipWithIndex.foreach { case (bio, i) =>
        val ex_read_req = io.srams.read(i).req
        val exread = ex_read_req.valid

        // TODO we tie the write dispatch queue's, and write issue queue's, ready and valid signals together here
        val dmawrite = write_dispatch_q.valid && write_issue_q.io.enq.ready &&
          !write_dispatch_q.bits.laddr.is_acc_addr && write_dispatch_q.bits.laddr.sp_bank() === i.U

        bio.read.req.valid := exread || dmawrite
        ex_read_req.ready := bio.read.req.ready

        // The ExecuteController gets priority when reading from SRAMs
        when (exread) {
          bio.read.req.bits.addr := ex_read_req.bits.addr
          bio.read.req.bits.fromDMA := false.B
          bio.read.req.bits.precision_bits := ex_read_req.bits.precision_bits
          bio.read.req.bits.subrow := ex_read_req.bits.subrow
        }.elsewhen (dmawrite) {
          bio.read.req.bits.addr := write_dispatch_q.bits.laddr.sp_row()
          bio.read.req.bits.fromDMA := true.B
          bio.read.req.bits.precision_bits := write_dispatch_q.bits.precision_bits
          bio.read.req.bits.subrow := write_dispatch_q.bits.laddr.sp_subrow()
          // bio.read.req.bits.offset := write_dispatch_q.offset
          when (bio.read.req.fire()) {
            write_dispatch_q.ready := true.B
            write_issue_q.io.enq.valid := true.B

            io.dma.write.resp.valid := true.B
          }
        }.otherwise {
          bio.read.req.bits := DontCare
        }

        val ex_read_resp = io.srams.read(i).resp
        val dma_resp_ready = writer.module.io.req.ready &&
          !write_issue_q.io.deq.bits.laddr.is_acc_addr && write_issue_q.io.deq.bits.laddr.sp_bank() === i.U // I believe we don't need to check that write_issue_q is valid here, because if the SRAM's resp is valid, then that means that the write_issue_q's deq should also be valid

        bio.read.resp.ready := Mux(bio.read.resp.bits.fromDMA, dma_resp_ready, ex_read_resp.ready)
        ex_read_resp.valid := bio.read.resp.valid // TODO should we AND this with fromDMA?
        ex_read_resp.bits := bio.read.resp.bits
      }

      // Writing to the SRAM banks
      bank_ios.zipWithIndex.foreach { case (bio, i) =>
        val exwrite = io.srams.write(i).en
        val dmaread = reader.module.io.resp.valid &&
          !reader.module.io.resp.bits.is_acc && reader.module.io.resp.bits.addr.asTypeOf(local_addr_t).sp_bank() === i.U

        bio.write.en := exwrite || dmaread

        when (exwrite) {
          bio.write.addr := io.srams.write(i).addr
          bio.write.data := io.srams.write(i).data
          bio.write.mask := io.srams.write(i).mask
          bio.write.precision_bits := io.srams.write(i).precision_bits
          bio.write.subrow := io.srams.write(i).subrow
        }.elsewhen (dmaread) {
          bio.write.addr := reader.module.io.resp.bits.addr
          bio.write.data := reader.module.io.resp.bits.data
          bio.write.mask := reader.module.io.resp.bits.mask take ((spad_w / (aligned_to * 8)) max 1)
          bio.write.precision_bits := reader.module.io.resp.bits.precision_bits
          bio.write.subrow := reader.module.io.resp.bits.subrow

          reader.module.io.resp.ready := true.B // TODO we combinationally couple valid and ready signals
        }.otherwise {
          bio.write.addr := DontCare
          bio.write.data := DontCare
          bio.write.mask := DontCare
          bio.write.precision_bits := DontCare
          bio.write.subrow := DontCare
        }
      }
    }

    {
      val acc_row_t = Vec(meshColumns, Vec(tileColumns, accType))
      val spad_row_t = Vec(meshColumns, Vec(tileColumns, inputType))

      val banks = Seq.fill(acc_banks) { Module(new AccumulatorMem(acc_bank_entries, acc_row_t, spad_row_t, mem_pipeline)) }
      val bank_ios = VecInit(banks.map(_.io))

      // Getting the output of the bank that's about to be issued to the writer
      val bank_issued_io = bank_ios(write_issue_q.io.deq.bits.laddr.acc_bank())

      when (write_issue_q.io.deq.bits.laddr.is_acc_addr) {
        writeData.valid := bank_issued_io.read.resp.valid && bank_issued_io.read.resp.bits.fromDMA
        writeData.bits := bank_issued_io.read.resp.bits.data.asUInt()
      }

      // Reading from the Accumulator banks
      bank_ios.zipWithIndex.foreach { case (bio, i) =>
        val ex_read_req = io.acc.read(i).req
        val exread = ex_read_req.valid

        // TODO we tie the write dispatch queue's, and write issue queue's, ready and valid signals together here
        val dmawrite = write_dispatch_q.valid && write_issue_q.io.enq.ready &&
          write_dispatch_q.bits.laddr.is_acc_addr && write_dispatch_q.bits.laddr.acc_bank() === i.U

        bio.read.req.valid := exread || dmawrite
        bio.read.req.bits.shift := ex_read_req.bits.shift
        bio.read.req.bits.relu6_shift := ex_read_req.bits.relu6_shift
        bio.read.req.bits.act := ex_read_req.bits.act
        ex_read_req.ready := bio.read.req.ready

        // The ExecuteController gets priority when reading from accumulator banks
        when (exread) {
          bio.read.req.bits.addr := ex_read_req.bits.addr
          bio.read.req.bits.fromDMA := false.B
        }.elsewhen (dmawrite) {
          bio.read.req.bits.addr := write_dispatch_q.bits.laddr.acc_row()
          bio.read.req.bits.fromDMA := true.B

          when (bio.read.req.fire()) {
            write_dispatch_q.ready := true.B
            write_issue_q.io.enq.valid := true.B

            io.dma.write.resp.valid := true.B
          }
        }.otherwise {
          bio.read.req.bits := DontCare
        }

        val ex_read_resp = io.acc.read(i).resp
        val dma_resp_ready = writer.module.io.req.ready &&
          write_issue_q.io.deq.bits.laddr.is_acc_addr && write_issue_q.io.deq.bits.laddr.acc_bank() === i.U // I believe we don't need to check that write_issue_q is valid here, because if the accumulator bank's resp is valid, then that means that the write_issue_q's deq should also be valid

        bio.read.resp.ready := Mux(bio.read.resp.bits.fromDMA, dma_resp_ready, ex_read_resp.ready)
        ex_read_resp.valid := bio.read.resp.valid // TODO should we AND this with fromDMA?
        ex_read_resp.bits := bio.read.resp.bits
      }

      // Writing to the accumulator banks
      bank_ios.zipWithIndex.foreach { case (bio, i) =>
        val exwrite = io.acc.write(i).en
        val dmaread = reader.module.io.resp.valid &&
          reader.module.io.resp.bits.is_acc && reader.module.io.resp.bits.addr.asTypeOf(local_addr_t).acc_bank() === i.U

        bio.write.en := exwrite || dmaread

        when (exwrite) {
          bio.write.addr := io.acc.write(i).addr
          bio.write.data := io.acc.write(i).data
          bio.write.acc := io.acc.write(i).acc
          bio.write.mask := io.acc.write(i).mask // TODO add wmask to AccumulatorMem as well, so that we support non-aligned accesses
        }.elsewhen (dmaread) {
          bio.write.addr := reader.module.io.resp.bits.addr
          bio.write.data := reader.module.io.resp.bits.data.asTypeOf(acc_row_t)
          bio.write.acc := false.B
          bio.write.mask := reader.module.io.resp.bits.mask // TODO add wmask to AccumulatorMem as well, so that we support non-aligned accesses

          reader.module.io.resp.ready := true.B // TODO we combinationally couple valid and ready signals
        }.otherwise {
          bio.write.addr := DontCare
          bio.write.data := DontCare
          bio.write.acc := DontCare
          bio.write.mask := DontCare // TODO add wmask to AccumulatorMem as well, so that we support non-aligned accesses
        }
      }
    }

    /*
    {
      val acc_row_t = Vec(meshColumns, Vec(tileColumns, accType))
      val spad_row_t = Vec(meshColumns, Vec(tileColumns, inputType))

      val accumulator = Module(new AccumulatorMem(acc_bank_entries, acc_row_t, spad_row_t, mem_pipeline))

      when (write_issue_q.io.deq.bits.laddr.is_acc_addr) {
        writeData.valid := accumulator.io.read.resp.valid && accumulator.io.read.resp.bits.fromDMA
        writeData.bits := accumulator.io.read.resp.bits.data.asUInt()
      }

      // Accumulator reads
      {
        val exread = io.acc.read.req.valid

        // TODO we tie the write dispatch queue's, and write issue queue's, ready and valid signals together here
        val dmawrite = write_dispatch_q.valid && write_issue_q.io.enq.ready && write_dispatch_q.bits.laddr.is_acc_addr

        accumulator.io.read.req.valid := exread || dmawrite
        accumulator.io.read.req.bits.shift := io.acc.read.req.bits.shift
        accumulator.io.read.req.bits.relu6_shift := io.acc.read.req.bits.relu6_shift
        accumulator.io.read.req.bits.act := io.acc.read.req.bits.act
        io.acc.read.req.ready := accumulator.io.read.req.ready

        // The ExecuteController gets priority when reading from the accumulator
        when (exread) {
          accumulator.io.read.req.bits.addr := io.acc.read.req.bits.addr
          accumulator.io.read.req.bits.fromDMA := false.B
        }.elsewhen(dmawrite) {
          accumulator.io.read.req.bits.addr := write_dispatch_q.bits.laddr.acc_row()
          accumulator.io.read.req.bits.fromDMA := true.B

          when (accumulator.io.read.req.fire()) {
            write_dispatch_q.ready := true.B
            write_issue_q.io.enq.valid := true.B

            io.dma.write.resp.valid := true.B
          }
        }.otherwise {
          accumulator.io.read.req.bits.addr := DontCare
          accumulator.io.read.req.bits.fromDMA := DontCare
        }

        val ex_resp_ready = io.acc.read.resp.ready
        val dma_resp_ready = writer.module.io.req.ready && write_issue_q.io.deq.bits.laddr.is_acc_addr // I believe we don't need to check that write_issue_q is valid here, because if the SRAM's resp is valid, then that means that the write_issue_q's deq should also be valid

        accumulator.io.read.resp.ready := Mux(accumulator.io.read.resp.bits.fromDMA, dma_resp_ready, ex_resp_ready)
        io.acc.read.resp.valid := accumulator.io.read.resp.valid // TODO should we AND this with fromDMA?
        io.acc.read.resp.bits := accumulator.io.read.resp.bits
      }

      // Accumulator writes
      {
        val exwrite = io.acc.write.en
        val dmaread = reader.module.io.resp.valid && reader.module.io.resp.bits.is_acc

        accumulator.io.write.en := exwrite || dmaread

        when (exwrite) {
          accumulator.io.write.addr := io.acc.write.addr
          accumulator.io.write.data := io.acc.write.data
          accumulator.io.write.acc := io.acc.write.acc
        }.elsewhen (dmaread) {
          accumulator.io.write.addr := reader.module.io.resp.bits.addr
          accumulator.io.write.data := reader.module.io.resp.bits.data.asTypeOf(acc_row_t)
          accumulator.io.write.acc := false.B

          reader.module.io.resp.ready := true.B // TODO we combinationally couple valid and ready signals
        }.otherwise {
          accumulator.io.write.addr := DontCare
          accumulator.io.write.data := DontCare
          accumulator.io.write.acc := DontCare
        }
      }
    }
    */
  }
}
