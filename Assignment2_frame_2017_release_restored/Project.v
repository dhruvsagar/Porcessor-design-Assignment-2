module Project(
	input        CLOCK_50,
	input        RESET_N,
	input  [3:0] KEY,
	input  [9:0] SW,
	output [6:0] HEX0,
	output [6:0] HEX1,
	output [6:0] HEX2,
	output [6:0] HEX3,
	output [6:0] HEX4,
	output [6:0] HEX5,
	output [9:0] LEDR
);

  parameter DBITS    =32;
  parameter INSTSIZE =32'd4;
  parameter INSTBITS =32;
  parameter REGNOBITS=4;
  parameter IMMBITS  =16;
  parameter STARTPC  =32'h0;//100 #modelsim;
  parameter ADDRHEX  =32'hFFFFF000;
  parameter ADDRLEDR =32'hFFFFF020;
  parameter ADDRKEY  =32'hFFFFF080;
  parameter ADDRSW   =32'hFFFFF090;
  // Change this to fmedian.mif before submitting
  parameter IMEMINITFILE="test.mif";
  parameter IMEMADDRBITS=16;
  parameter IMEMWORDBITS=2;
  parameter IMEMWORDS=(1<<(IMEMADDRBITS-IMEMWORDBITS));
  parameter DMEMADDRBITS=16;
  parameter DMEMWORDBITS=2;
  parameter DMEMWORDIDXBITS = DMEMADDRBITS-DMEMWORDBITS;
  parameter DMEMWORDS=(1<<(DMEMADDRBITS-DMEMWORDBITS));
  
  parameter OP1BITS  =6;
  parameter OP1_EXT  =6'b000000;
  parameter OP1_BEQ  =6'b001000;
  parameter OP1_BLT  =6'b001001;
  parameter OP1_BLE  =6'b001010;
  parameter OP1_BNE  =6'b001011;
  parameter OP1_JAL  =6'b001100;
  parameter OP1_LW   =6'b010010;
  parameter OP1_SW   =6'b011010;
  parameter OP1_ADDI =6'b100000;
  parameter OP1_ANDI =6'b100100;
  parameter OP1_ORI  =6'b100101;
  parameter OP1_XORI =6'b100110;

  
  // Add parameters for secondary opcode values
    
  /* OP2 */
  parameter OP2BITS  = 8;
  parameter OP2_EQ   = 8'b00001000;
  parameter OP2_LT   = 8'b00001001;
  parameter OP2_LE   = 8'b00001010;
  parameter OP2_NE   = 8'b00001011;

  parameter OP2_ADD  = 8'b00100000;
  parameter OP2_AND  = 8'b00100100;
  parameter OP2_OR   = 8'b00100101;
  parameter OP2_XOR  = 8'b00100110;
  parameter OP2_SUB  = 8'b00101000;
  parameter OP2_NAND = 8'b00101100;
  parameter OP2_NOR  = 8'b00101101;
  parameter OP2_NXOR = 8'b00101110;
  parameter OP2_RSHF = 8'b00110000;
  parameter OP2_LSHF = 8'b00110001;
  
  /* ALUFUNC */
  parameter ALUFUNC_EQ   = 6'b000000;
  parameter ALUFUNC_LT   = 6'b000001;
  parameter ALUFUNC_LE   = 6'b000010;
  parameter ALUFUNC_NE   = 6'b000011;
  parameter ALUFUNC_ADD  = 6'b000100;
  parameter ALUFUNC_AND  = 6'b000101;
  parameter ALUFUNC_OR   = 6'b000110;
  parameter ALUFUNC_XOR  = 6'b000111;
  parameter ALUFUNC_SUB  = 6'b001000;
  parameter ALUFUNC_NAND = 6'b001001;
  parameter ALUFUNC_NOR  = 6'b001010;
  parameter ALUFUNC_NXOR = 6'b001011;
  parameter ALUFUNC_RSHF = 6'b001100;
  parameter ALUFUNC_LSHF = 6'b001101;
  
  
  parameter HEXBITS  = 24;
  parameter LEDRBITS = 10;
  
  // The reset signal comes from the reset button on the DE0-CV board
  // RESET_N is active-low, so we flip its value ("reset" is active-high)
  wire clk,locked;
  // The PLL is wired to produce clk and locked signals for our logic
  
  Pll myPll(
    .refclk(CLOCK_50),
	 .rst      (!RESET_N),
	 .outclk_0 (clk),
    .locked   (locked)
  );
  wire reset = !locked;
  //CANNOT USE PLL SO USING FOLLOWING CODE
  //assign	clk	=	CLOCK_50;
  //assign	reset	=	!RESET_N;
 
  /*************** BUS *****************/
  // Create the processor's bus
  tri [(DBITS-1):0] thebus;
  parameter BUSZ={DBITS{1'bZ}};  

  /*************** PC *****************/
  // Create PC and connect it to the bus
  reg [(DBITS-1):0] PC;
  reg LdPC, DrPC, IncPC;
     
  //Data path
  always @(posedge clk or posedge reset) begin
    if(reset)
	   PC<=STARTPC;
	 else if(LdPC)
      PC<=thebus;
    else if(IncPC)
      PC<=PC+INSTSIZE;
    else
	   PC<=PC;
  end
  assign thebus=DrPC?PC:BUSZ;

  /*************** Fetch - Instruction memory *****************/  
  //(* ram_init_file = IMEMINITFILE *)
  reg [(DBITS-1):0] imem[(IMEMWORDS-1):0];
  always @(posedge reset) begin
  imem[0] <= 32'h801D6B98	;   //addi	s2,Zero,7531
  imem[1] <= 32'h80E29588	;		 //add  s1,s1,s2
  imem[2] <= 32'h20000589	;   //beq  s1, s2, skip 2
  imem[3] <= 32'h00980000	;	  //	xor		fp,fp,fp
  imem[4] <= 32'h68F00009	; 	 //add   Zero,fp,fp
  imem[5] <= 32'h00B00500	;   //sw    s1, 5(Zero)
  imem[6] <= 32'h68F02005	;   //lw    t0, 5(Zer0)
  imem[7] <= 32'h20FFFB00	;   //addi	s2,Zero,7531
  imem[8] <= 32'h00980DDD		;		 //add  s1,s1,s2
  imem[9] <= 32'h008000DD		;   //beq  s1, s2, skip 2
  imem[10] <= 32'h80000107		;	  //	xor		fp,fp,fp
  imem[11] <= 32'h68F020D7		; 	 //add   Zero,fp,fp
  imem[12] <= 32'h80000177		;   //sw    s1, 5(Zero)
  imem[13] <= 32'h68F020D7		;   //lw    t0, 5(Zer0)
  end
  wire [(DBITS-1):0] iMemOut;
  
  assign iMemOut=imem[PC[(IMEMADDRBITS-1):IMEMWORDBITS]];
  
  /*************** Fetch - Instruction Register *****************/    
  // Create the IR (feeds directly from memory, not from bus)
  reg [(INSTBITS-1):0] IR;
  reg LdIR;
  
  //Data path
  always @(posedge clk or posedge reset)
  begin
    if(reset)
	   IR<=32'hDEADDEAD;
	 else if(LdIR)
      IR <= iMemOut;
  end
  
  
  /*************** Decode *****************/ 
  // Put the code for getting op1, rd, rs, rt, imm, etc. here 
  wire [(OP1BITS-1)    : 0] op1;
  wire [(OP2BITS-1)    : 0] op2;
  wire [(REGNOBITS-1)  : 0] rs;
  wire [(REGNOBITS-1)  : 0] rd;
  wire [(REGNOBITS-1)  : 0] rt;
  wire [(IMMBITS-1)    : 0] imm;

  //TODO: Implement instruction decomposition logic
  assign op1 = IR[(DBITS-1): (DBITS - OP1BITS)];
  assign op2 = IR[(DBITS-1): (DBITS - OP1BITS - OP2BITS)];
  assign rt = IR[(REGNOBITS - 1) : 0];
  assign rs = IR[(REGNOBITS*2 - 1) : REGNOBITS];
  assign rd = IR[(REGNOBITS*3 - 1) : REGNOBITS*2];
  assign imm = IR[(REGNOBITS*2 + IMMBITS - 1) : REGNOBITS*2];
   
  /*************** sxtimm *****************/   
  wire [(DBITS-1)      : 0] sxtimm;
  reg DrOff, ShOff;
  wire [(DBITS-1)      : 0] sxtimmTimesFour;
  assign sxtimmTimesFour = sxtimm << 2;
  assign thebus = ShOff? sxtimmTimesFour:BUSZ;
  assign thebus = DrOff? sxtimm:BUSZ;  

  /*************** Register file *****************/ 		
  // Create the registers and connect them to the bus
  reg [(DBITS-1):0] regs[15:0];

  //Control signals
  reg WrReg,DrReg;
  
  //Data signals
  reg  [(REGNOBITS-1):0] regno;
  wire [(DBITS-1)    :0] regOut;
     
  integer r;
  always @(posedge clk or posedge reset)
  begin: REG_WRITE
	 if (reset) begin
      regs[0] = 0;
      regs[1] = 0;
      regs[2] = 0;
      regs[3] = 0;
      regs[4] = 0;
      regs[5] = 0;
      regs[6] = 0;
      regs[7] = 0;
      regs[8] = 0;
      regs[9] = 0;
      regs[10] = 0;
      regs[11] = 0;
      regs[12] = 0;
      regs[13] = 0;
      regs[14] = 0;
      regs[15] = 0;
	 end
    else if(WrReg&&!reset)
      regs[regno]<=thebus;
  end  
  
  assign regOut= WrReg?{DBITS{1'bX}}:regs[regno];
  assign thebus= DrReg?regOut:BUSZ;

  /***********************************************/ 

  /******************** ALU **********************/
  // Create ALU unit and connect to the bus
  //Data signals
  reg signed [(DBITS-1):0] A,B;
  reg signed [(DBITS-1):0] ALUout;
  //Control signals
  reg LdA,LdB,DrALU;
 
  //Data path
  // Receive data from bus
  always @(posedge clk) begin
    if(LdA)
      A <= thebus;
    if(LdB)
      B <= thebus;
  end  

  //TODO: Implement ALU functionality
  
  //ALU results
  reg [5:0] ALUfunc;
	always @ (*) begin
		case(ALUfunc)
		  ALUFUNC_EQ: 
			  ALUout = A == B;
		  
		  ALUFUNC_LT: 
			  ALUout = A < B;
		  
		  ALUFUNC_LE: 
			  ALUout = A <= B;
		  
		  ALUFUNC_NE: 
			  ALUout = A != B;
		  
		  ALUFUNC_ADD: 
			  ALUout = A + B;
		  
		  ALUFUNC_AND: 
			  ALUout = A & B;
		  
		  ALUFUNC_OR: 
			  ALUout = A | B;
		  
		  ALUFUNC_XOR: 
			  ALUout = A ^ B;
		  
		  ALUFUNC_SUB: 
			  ALUout = A - B;
		  
		  ALUFUNC_NAND:
			  ALUout = ~(A & B);
		  
		  ALUFUNC_NOR: 
			  ALUout = ~(A | B);
		  
		  ALUFUNC_NXOR:
			  ALUout = ~(A ^ B);
		  
		  ALUFUNC_RSHF:
			  ALUout = A >> B;
		  
		  ALUFUNC_LSHF:
			  ALUout = A << B;
		  
		  default:
			  ALUout = 0;
		endcase
	end

  // Connect ALU output to the bus (controlled by DrALU)
  assign thebus=DrALU?ALUout:BUSZ;
  
  // BRANCHING REGISTER
  reg branchReg;
  always @ (ALUout) begin
    case(op1)
      OP1_BEQ, OP1_BLT, OP1_BLE, OP1_BNE:
      		branchReg <= ALUout[0];
		endcase
  end
  
  
  /*************** Data Memory *****************/    
  // TODO: Put the code for data memory and I/O here  
  //Data memory
  reg [(DBITS-1):0] MAR;
  reg [(DBITS-1):0] dmem[(DMEMWORDS-1):0];
  
  //Data signals
  wire [(DBITS-1):0] memin, MemVal;
  wire [(DMEMWORDIDXBITS-1):0] dmemAddr;
  
  //Control singals
  reg DrMem, WrMem, LdMAR; 
  wire MemEnable, MemWE;

  assign MemEnable = !(MAR[(DBITS-1):DMEMADDRBITS]);
  assign MemWE     = WrMem & MemEnable & !reset;

  always @(posedge clk or posedge reset)
  begin: LOAD_MAR
    if(reset) begin
      MAR<=32'b0;
    end
    else if(LdMAR) begin
      MAR<=thebus;
    end
  end
  
  
  //Data path
  assign dmemAddr = MAR[(DMEMADDRBITS-1):DMEMWORDBITS];
  assign MemVal  = MemWE? {DBITS{1'bX}} : dmem[dmemAddr];
  assign memin   = thebus;   //Snoop the bus
  
  always @(posedge clk)
  begin: DMEM_STORE
    if(MemWE) begin
      dmem[dmemAddr] <= memin;
    end
  end
  assign thebus=DrMem? MemVal:BUSZ;
  
  /******************IO input********************************/
  //INPUT
  reg DrKey, DrSwitch;
  wire [31:0] key_value;
  wire [31:0] switch_value;
  assign key_value = ~KEY;
  assign switch_value[9:0] = SW;
  
  assign thebus=DrKey? key_value:BUSZ;
  assign thbus=DrSwitch? switch_value :BUSZ;  
  
  //OUTPUT
  wire [9:0] ledr_in;
  wire [23:0] hex_in;
  reg [9:0] ledr_reg;
  reg [23:0] hex_reg;
  reg WrLedr, WrHex;
  always @(posedge clk or posedge reset) begin
	 if (reset) begin
		ledr_reg <= 10'h2ff;
		hex_reg <= 24'hffffff;
	 end
    else if (WrLedr) begin
      ledr_reg <= ledr_in;
    end
    else if (WrHex) begin
      hex_reg <= hex_in;
    end
  end
  assign LEDR = ledr_reg;
  assign ledr_in = thebus[9:0];
  assign hex_in = thebus[23:0];
    
  /******************** Processor state **********************/
  parameter S_BITS=5;
  parameter [(S_BITS-1):0]
    S_ZERO        = {(S_BITS){1'b0}},
    S_ONE         = {{(S_BITS-1){1'b0}},1'b1},
    S_FETCH1      = S_ZERO,
	 S_FETCH2      = S_FETCH1+S_ONE,
    S_ALUR1       = S_FETCH2+S_ONE,
	 //TODO: Define your processor states here
	 S_ALUR2			= S_ALUR1 + S_ONE,
	 S_ALUR3			= S_ALUR2 + S_ONE,
	 S_ALUI1			= S_ALUR3 + S_ONE,
	 S_ALUI2			= S_ALUI1 + S_ONE,
	 S_ALUI3			= S_ALUI2 + S_ONE,
	 S_BR1 			= S_ALUI3 + S_ONE,
	 S_BR2			= S_BR1 	 + S_ONE,
	 S_BR3			= S_BR2 	 + S_ONE,
	 S_BR4			= S_BR3 	 + S_ONE,
	 S_BR5			= S_BR4 	 + S_ONE,
	 S_BR6			= S_BR5 	 + S_ONE,
	 S_BR7			= S_BR6 	 + S_ONE,
	 S_JAL1 			= S_BR7 	 + S_ONE,
	 S_JAL2			= S_JAL1	 + S_ONE,
	 S_JAL3			= S_JAL2	 + S_ONE,
	 S_JAL4			= S_JAL3	 + S_ONE,
	 S_LW1			= S_JAL4	 + S_ONE,
	 S_LW2			= S_LW1	 + S_ONE,
	 S_LW3			= S_LW2	 + S_ONE,
	 S_LW4			= S_LW3	 + S_ONE,
	 S_SW1			= S_LW4	 + S_ONE,
	 S_SW2			= S_SW1	 + S_ONE,
	 S_SW3			= S_SW2	 + S_ONE,
	 S_SW4			= S_SW3	 + S_ONE,
	 S_ERROR       = S_SW4   + S_ONE;

 reg [(S_BITS-1):0] state,next_state;
  always @(state or op1 or rs or rt or rd or op2 or ALUout[0]) begin
    {LdPC,DrPC,IncPC,LdMAR,WrMem,DrMem,LdIR,DrOff,ShOff, LdA, LdB,ALUfunc,DrALU,regno   ,DrReg,WrReg,next_state,DrKey,DrSwitch,WrLedr,WrHex}=
    {1'b0,1'b0, 1'b0, 1'b0, 1'b0, 1'b0,1'b0, 1'b0, 1'b0,1'b0,1'b0,   6'bX,1'b0,6'b000000, 1'b0, 1'b0,state+S_ONE,1'b0,    1'b0,  1'b0, 1'b0};
    case(state)
      S_FETCH1: {LdIR,IncPC}={1'b1,1'b1};
      S_FETCH2: begin
	           case(op1)
					     OP1_EXT: begin
					     case(op2)
					       OP2_SUB,
				  		    OP2_NAND,OP2_NOR,OP2_NXOR,
						    OP2_EQ,OP2_LT,OP2_LE,OP2_NE,
						    OP2_ADD,
						    OP2_AND,OP2_OR,OP2_XOR:
						         next_state=S_ALUR1;		//why not <=
						    default: next_state=S_ERROR;
						  endcase
				      end
						 OP1_BEQ, OP1_BLT, OP1_BLE, OP1_BNE:
						   next_state = S_BR1;
					   OP1_ADDI,OP1_ANDI,OP1_ORI,OP1_XORI:
						   next_state=S_ALUI1;
						 OP1_JAL:
						   next_state = S_JAL1;
						 OP1_LW:
						   next_state = S_LW1;
						 OP1_SW:
						 
  next_state = S_SW1;
						 default: next_state=S_ERROR;
					   endcase
					  end
		S_ALUR1: begin
						regno = rs;		//blocking or non-blocking?
						DrReg = 1'b1;
						LdA = 1'b1;
						next_state = S_ALUR2;
					end					//is default needed?
		S_ALUR2:	begin
						regno = rt;
						{DrReg, LdB} = {1'b1,1'b1};
						next_state = S_ALUR3;
					end
		S_ALUR3: begin
						case(op2)
							OP2_SUB: ALUfunc = ALUFUNC_SUB;
							OP2_NAND: ALUfunc = ALUFUNC_NAND;
							OP2_NOR: ALUfunc = ALUFUNC_NOR;
							OP2_NXOR: ALUfunc = ALUFUNC_NXOR;
							OP2_EQ: ALUfunc = ALUFUNC_EQ;
							OP2_LT: ALUfunc = ALUFUNC_LT;
							OP2_LE: ALUfunc = ALUFUNC_LE;
							OP2_NE: ALUfunc = ALUFUNC_NE;
							OP2_ADD: ALUfunc = ALUFUNC_ADD;
							OP2_AND: ALUfunc = ALUFUNC_AND;
							OP2_OR: ALUfunc = ALUFUNC_OR;
							OP2_XOR: ALUfunc = ALUFUNC_XOR;
						endcase
						DrALU = 1'b1;
						regno = rd;
						WrReg = 1'b1;
						next_state = S_FETCH1;
					end
		S_ALUI1: begin
						regno = rs;
						DrReg = 1'b1;
						LdA = 1'b1;
						next_state = S_ALUI2;
					end
		S_ALUI2: begin
						DrOff = 1'b1;
						LdB = 1'b1;
						next_state = S_ALUI3;
					end
		S_ALUI3: begin
						case(op1)
							OP1_ADDI: ALUfunc = ALUFUNC_ADD;
							OP1_ANDI: ALUfunc = ALUFUNC_AND;
							OP1_ORI: ALUfunc = ALUFUNC_OR;
							OP1_XORI: ALUfunc = ALUFUNC_XOR;
						endcase
						DrALU = 1'b1;
						regno = rt;
						WrReg = 1'b1;
						next_state = S_FETCH1;
					end
		S_BR1: begin
						regno = rs;
						DrReg = 1'b1;
						LdA = 1'b1;
						next_state = S_BR2;
					end
		S_BR2: begin
						regno = rt;
						DrReg = 1'b1;
						LdB = 1'b1;
						next_state = S_BR3;
					end
		S_BR3: begin
						case(op1)
							OP1_BEQ: ALUfunc = ALUFUNC_EQ;
							OP1_BLT: ALUfunc = ALUFUNC_LT;
							OP1_BLE: ALUfunc = ALUFUNC_LE;
							OP1_BNE: ALUfunc = ALUFUNC_NE;
						endcase
						DrALU = 1'b1;
						next_state = S_BR4;
					 end  
	  S_BR4: begin
						if(branchReg) begin
							next_state = S_BR5;
						end
						else begin
							next_state = S_FETCH1;
						end
					end
		S_BR5: begin
						{DrPC, LdA} = {1'b1, 1'b1};
						next_state = S_BR6;
				 end
		S_BR6: begin	
						{ShOff, LdB} = {1'b1, 1'b1};
						next_state = S_BR7;
				 end
		S_BR7: begin		
						ALUfunc = ALUFUNC_ADD;
						DrALU = 1'b1;
						LdPC = 1'b1;
						next_state = S_FETCH1;
				 end
		S_JAL1: begin
						regno = rt;
						{DrPC, WrReg} = {1'b1, 1'b1};
						next_state = S_JAL2;
				  end
		S_JAL2: begin
						regno = rs;
						{DrReg, LdA} = {1'b1, 1'b1};
						next_state = S_JAL3;
				  end
		S_JAL3: begin
						{ShOff, LdB} = {1'b1, 1'b1};
						next_state = S_JAL4;
				  end
		S_JAL4: begin
						ALUfunc = ALUFUNC_ADD;
						{DrALU, LdPC} = {1'b1, 1'b1};
						next_state = S_FETCH1;
				  end
		S_LW1: begin
						regno = rs;
						{DrReg, LdA} = {1'b1, 1'b1};
						next_state = S_LW2;
				 end
		S_LW2: begin
						{DrOff, LdB} = {1'b1, 1'b1};
						next_state = S_LW3;
				 end
		S_LW3: begin
						ALUfunc = ALUFUNC_ADD;
						{DrALU, LdMAR} = {1'b1, 1'b1};
						next_state = S_LW4;
				 end
		S_LW4: begin
						if (MemEnable) begin
						  DrMem = 1'b1;
						end
						else begin
              case(MAR)
                ADDRKEY: DrKey = 1;
                ADDRSW:  DrSwitch = 1;
              endcase				
						end
						regno = rt;
						WrReg = 1'b1;
						next_state = S_FETCH1;
				 end
		S_SW1: begin
						regno = rs;
						{DrReg, LdA} = {1'b1, 1'b1};
						next_state = S_SW2;
				 end
		S_SW2: begin
						{DrOff, LdB} = {1'b1, 1'b1};
						next_state = S_SW3;
				 end
		S_SW3: begin
						ALUfunc = ALUFUNC_ADD;
						{DrALU, LdMAR} = {1'b1, 1'b1};
						next_state = S_SW4;
				 end
		S_SW4: begin
						regno = rt;
						DrReg = 1'b1;
		        if (MemEnable) begin
		          WrMem = 1'b1;
		        end
		        else begin
		          case(MAR)
		            ADDRHEX: WrHex = 1;
		            ADDRLEDR: WrLedr = 1;
		          endcase
		        end
						next_state = S_FETCH1;
				 end
				
	  // Put the code for the rest of the "dispatch" here	
	  // Put the rest of the "microcode" here
      default:  next_state=S_ERROR;
    endcase
  end

  //TODO: Implement your processor state transition machine	 
  always @(posedge clk or posedge reset) begin
    if(reset) begin
      state<=S_FETCH1;
    end
    else begin 
		state<=next_state;
	 end
  end
  
	  
  /*************** sign-extend (SXT) *****************/       
  //TODO: Instantiate SXT module
	SXT signExtend(imm, sxtimm);
  
  /*************** HEX/LEDR Output *****************/    
  //TODO: Implement output logic
  //      store to HEXADDR or LEDR addr should display given values to HEX or LEDR

  //TODO: Utilize seven segment display decoders to convert hex to actual seven-segment display control signal
  
  SevenSeg ss0(.IN(hex_reg[3:0]),.OFF(1'b0),.OUT(HEX0));
	SevenSeg ss1(.IN(hex_reg[7:4]),.OFF(1'b0),.OUT(HEX1));
	SevenSeg ss2(.IN(hex_reg[11:8]),.OFF(1'b0),.OUT(HEX2));
	SevenSeg ss3(.IN(hex_reg[15:12]),.OFF(1'b0),.OUT(HEX3));
	SevenSeg ss4(.IN(hex_reg[19:16]),.OFF(1'b0),.OUT(HEX4));
	SevenSeg ss5(.IN(hex_reg[23:20]),.OFF(1'b0),.OUT(HEX5));
	
endmodule


//been edited here
module SXT(IN,OUT);
  parameter IBITS = 16;
  parameter OBITS = 32;
  input  [(IBITS-1):0] IN;
  output [(OBITS-1):0] OUT;
  assign OUT={{(OBITS-IBITS){IN[IBITS-1]}},IN};
endmodule

