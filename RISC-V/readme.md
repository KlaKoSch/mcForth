# RISC-V (RV32IMC) Assembler, Disassembler, and Simulator

This folder collect a set of programs used to simulate a RV32IMC RISC-V processor like GD32VF103CB.

mcForth Simulator and image of mcForth for virtual VF32 processor - see mcForth.net for documentation.
- mcFSimVP32.exe : VP32-Simulator - see mcForth.net for documentation
- mcf32d.mcf : Image of direct threaded mcForth

Assembler and disassembler are available as a direct loadable version for mcForth using real memory address.
- rv32imc_sim.f : Simulator
- rv32imc_asm.f : Assembler
- rv32imc_dasm.f : Disassembler

The Target version of assembler and disassembler are designed in combination with the simulator.
- rv32imc_sim : Simulator for GD32VF103CB - adaptable to other processors with 32-bit RISC-V
- rv32imc_tasm : Assembler adapted for use of simulator memory
- rv32imc_tdasm : Disassembler using simulator memory
- mq.bin : Image of mecrisp for GD32VF103CB (binary)

Usage:
   mcFSimVP32.exe mcf32d.mcf include rv32imc_tsim.f
   sim
   
Maybe command  help  to see whats possible
   
   read mq.bin
   run

Please be patient - a simulator in an simulator even on fast PC need a few second to react.
Then you can use all words as long you need no interrupt or special register.
Press ESC key to stop simulator
Command  x  leaves the simulator
