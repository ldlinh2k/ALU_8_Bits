package vexriscv.plugin

import spinal.core._
import spinal.lib._
import vexriscv.{DecoderService, Stageable, VexRiscv}

case class MY_ALU_PLUGIN(encoding : MaskedLiteral = M"-----------------111-----0101011") extends Plugin[VexRiscv]{ // Fixed

  object IS_ALU extends Stageable(Bool)
  object ALU_CALC extends Stageable(Bits(32 bits))


  //Callback to setup the plugin and ask for different services
  override def setup(pipeline: VexRiscv): Unit = {
    import pipeline.config._

    val decoderService = pipeline.service(classOf[DecoderService])

//FIX - Name
    decoderService.addDefault(IS_ALU, False)
    decoderService.add(
      key = encoding,
      List(
        IS_ALU                   -> True,   //FIX - NAME
        REGFILE_WRITE_VALID      -> True,
        BYPASSABLE_EXECUTE_STAGE -> True,
        BYPASSABLE_MEMORY_STAGE  -> True, //Late result
        RS1_USE                  -> True,
        RS2_USE                  -> True
      )
    )
  }

  override def build(pipeline: VexRiscv): Unit = {
    import pipeline._
    import pipeline.config._

    val onExecute = execute plug new Area{
      import execute._
    //FIX - New variable 
      val rs1 = input(RS1).asUInt
      val rs2 = input(RS2).asUInt
      val rd = U(16 bits,default ->False)
      val operation = input(INSTRUCTION)(31 downto 25).asUInt

      switch(operation)
      {
        is(0){
            rd(8 downto 0) := (rs1(7 downto 0) + rs2 (7 downto 0)).resized //ADD
        }
        is(1){
            rd(7 downto 0) := rs1(7 downto 0) - rs2 (7 downto 0) //SUB
        }
        is(2){
            rd :=(rs1(7 downto 0) << rs2(2 downto 0)).resized // SLL, rd= rs1<<rs2[2:0]
        }
        is(3){
            rd :=(rs1(7 downto 0) >> rs2(2 downto 0)).resized  //SRL, rd =rs1>>rs2[2:0]
        }
      }
      execute.insert(ALU_CALC):=rd.asBits.resized
    }

    writeBack plug new Area {
      import writeBack._

      when(input(IS_ALU)) {
        output(REGFILE_WRITE_DATA) := input(ALU_CALC)
      }
    }
  }
}
