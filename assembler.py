import re
import sys

ex_codes = {
	'EQ':	int('00000000001000', 2),
	'LT':	int('00000000001001', 2),
	'LE':	int('00000000001010', 2),
	'ADD':	int('00000000001011', 2),
	'AND':	int('00000000001000', 2),
	'OR':	int('00000000100000', 2),
	'XOR':	int('00000000000000', 2),
	'SUB':	int('00000000000000', 2),
	'NAND':	int('00000000000000', 2),
	'NOR':	int('00000000000000', 2),
	'NXOR':	int('00000000000000', 2),
	'RSHF':	int('00000000000000', 2),
	'LSHF':	int('00000000000000', 2)
}

op_codes = {
	'BEQ':	int('001000', 2),
	'BLT':	int('001001', 2),
	'BLE':	int('001010', 2),
	'BNE':	int('001011', 2),
	'JAL':	int('001100', 2),
	'LW':	int('010010', 2),
	'SW':	int('011010', 2),
	'ADDI':	int('100000', 2),
	'ANDI':	int('100100', 2),
	'ORI':	int('100101', 2),
	'XORI':	int('100110', 2)
}

ps_codes = {
	'NOT':	'NAND',
	'RET':	'JAL',
	'BGT':	'BLT',
	'BR':	'BEQ',
	'GE':	'LE',
	'CALL': 'JAL',
	'JMP':	'JAL',
	'BGE':	'BLE',
	'GT':	'LT',
	'SUBI':	'ADDI'
}

asm_regs = {
	'R0':	0,
	'R1':	1,
	'R2':	2,
	'R3':	3,
	'R4':	4,
	'R5':	5,
	'R0':	6,
	'R0':	7,
	'R0':	8,
	'R0':	9,
	'R10':	10,
	'R11':	11,
	'R12':	12,
	'R13':	13,
	'R14':	14,
	'R15':	15,
	'ZERO':	0, # TODO should this be lowercase or uppercase
	'A0':	1,
	'A1':	2,
	'A2':	3,
	'A3':	4,
	'RV':	4,
	'T0':	5,
	'T1':	6,
	'S0':	7,
	'S1':	8,
	'S2':	9,
	'FP':	13,
	'SP':	14,
	'RA':	15
}

# Function definitions.

def parse_line(line, l_type):
	l_list = line.split(';', 1) # get rid of in-line comment text
	line = l_list if type(l_list) == type(str()) else l_list[0]
	line = line.strip()

	ret_list = []

	if l_type == 'ORIG':
		orig, val = line.split(maxsplit=1) # orig is discarded text '.ORIG'
		ret_list = [val]
	elif l_type == 'NAME':
		name, assign = line.split(maxsplit=1) # name is discarded text 'NAME'
		label, val = assign.split('=')
		ret_list = [label, val]
	elif l_type == 'OP':
		op, args = line.split(maxsplit=1)
		rs, rt, imm = args.split(',') # note the position of imm
		ret_list = [op, imm, rs, rt] # note the switched position of imm
	elif l_type == 'OP2':
		op2, args = line.split(maxsplit=1)
		rd, rs, rt = args.split(',')
		ret_list = [op2, rd, rs, rt]
	elif l_type == 'MEM':
		opm, args = line.split(maxsplit=1)
		rt, imm_rs = args.split(',')
		imm, rs = imm_rs.split('(')
		rs = rs[:-1] # strip trailing ')'
		ret_list = [opm, imm, rt, rs] # imm could also be a label
	else: # line is a label
		label = line[:-1] # strip trailing ':'
		ret_list = [label]

	for i in range(len(ret_list)):
		ret_list[i] = ret_list[i].strip()
		ret_list[i] = ret_list[i].upper()

	return ret_list

def parse_pseudo(line):
	l_list = line.split(maxsplit=1)
	first_word = l_list if type(l_list) == type(str()) else l_list[0]
	first_word = first_word.upper()

	l_list = line.split(';', 1) # get rid of in-line comment text
	line = l_list if type(l_list) == type(str()) else l_list[0]
	line = line.strip()

	args, i_type = [], '' 

	if first_word == 'NOT':
		i_type = 'OP2'
		op, o_args = line.split(maxsplit=1)
		ri, rj = o_args.split(',')
		args = ['NAND', ri, rj, rj]
	elif first_word == 'RET':
		i_type = 'MEM'
		args = ['JAL', '0', 'RA', 'R10']
	elif first_word == 'BGT':
		i_type = 'OP'
		op, o_args = line.split(maxsplit=1)
		ry, rx, label = o_args.split(',')
		args = ['BLT', label, rx, ry]
	elif first_word == 'BR':
		i_type = 'OP'
		op, label = line.split()
		args = ['BEQ', label, 'Zero', 'Zero']
	elif first_word == 'GE':
		i_type = 'OP2'
		op, o_args = line.split(maxsplit=1)
		rz, ry, rx = o_args.split(',')
		args = ['LE', rz, rx, ry]
	elif first_word == 'CALL':
		i_type = 'MEM'
		op, imm_ri = line.split()
		imm, ri = imm_ri.split('(')
		ri = ri[:-1] # strip trailing ')'
		args = ['JAL', imm, ri, 'RA'] # imm could also be a label
	elif first_word == 'JMP':
		i_type = 'MEM'
		op, imm_ri = line.split()
		imm, ri = imm_ri.split('(')
		ri = ri[:-1] # strip trailing ')'
		args = ['JAL', imm, ri, 'R10'] # imm could also be a label
	elif first_word == 'BGE':
		i_type = 'OP'
		op, o_args = line.split()
		ry, rx, label = o_args.split(',')
		args = ['BLE', label, rx, ry]
	elif first_word == 'GT':
		i_type = 'OP2'
		op, o_args = line.split()
		rz, ry, rx = o_args.split(',')
		args = ['LT', rz, rx, ry]
	else: # instruction is SUBI
		i_type = 'OP'
		op, o_args = line.split()
		ry, rx, imm = o_args.split(',')
		args = ['ADDI', str(-num(imm)), ry, rx] # return str to match others

	for i in range(len(args)):
		args[i] = args[i].strip()
		args[i] = args[i].upper()

	return args, i_type

def trans_instr(args, i_type, labels):
	instr = 0

	if i_type == 'OP':
		op, imm, rs, rt = args[0], args[1], args[2], args[3]
		imm = labels[imm] if imm in labels else num(imm)
		op, rs, rt = op_codes[op], asm_regs[rs], asm_regs[rt]
		instr = (op<<26) + (imm<<8) + (rs<<4) + rt
	elif i_type == 'OP2':
		op2, rd, rs, rt = args[0], args[1], args[2], args[3]
		op2, rd, rs, rt = ex_codes[op2], asm_regs[rd], asm_regs[rs], asm_regs[rt]
		instr = (op2<<18) + (rd<<8) + (rs<<4) + (rt)
	else: # instuction is memory instruction
		opm, imm, rs, rt = args[0], args[1], args[2], args[3]
		imm = labels[imm] if imm in labels else num(imm)
		opm, rs, rt = op_codes[opm], asm_regs[rs], asm_regs[rt]
		instr = (opm<<26) + (imm<<8) + (rs<<4) + rt

	return instr

def line_type(line):
	wsem = re.compile('^\s*$') # 'white space empty matcher'
	if (wsem.match(line)):
		return 'COMMENT'

	l_list = line.split(maxsplit=1)
	first_word = l_list if type(l_list) == type(str()) else l_list[0]
	first_word = first_word.upper()

	if first_word == '.ORIG':
		return 'ORIG'
	elif first_word == '.NAME':
		return 'NAME'
	elif first_word in op_codes:
		if first_word in ['JAL', 'LW', 'SW']:
			return 'MEM'
		else:
			return 'OP'
	elif first_word in ex_codes:
		return 'OP2'
	elif first_word in ps_codes:
		return 'PSEUDO'
	elif first_word[0] in [';', '']:
		return 'COMMENT'
	else:
		return 'LBL'

def num(number_str):
	hm = re.compile('0(x|X)([0-9]|[a-f]|[A-F])*') # 'hex matcher'
	if hm.match(number_str):
		return int(number_str, 16)
	else: 
		return int(number_str)
#
# MAIN FLOW OF EXECUTION-------------------------------------------------------
#

# check for correct number of arguments
if len(sys.argv) != 2:
	print('The amount of arguments is incorrect, assembler is exiting...')
	sys.exit()

# read from the input file.
asm = ''
with open(sys.argv[1], 'r') as f:
	asm = f.read()

asm = asm.split('\n')

# variables
orig_addr = 0
orig_offset = 0
labels = {}

# initially populate labels
for line in asm:
	line = line.strip()
	l_type = line_type(line)

	curr_addr = str(hex(orig_addr + orig_offset)) # TODO zero padding ?

	if l_type == 'ORIG':
		val = parse_line(line, l_type)[0] # returned list has one value
		orig_addr = num(val)
		orig_offset = 0
		continue # don't want to increment orig_offset
	elif l_type == 'NAME':
		label, val = parse_line(line, l_type)
		labels[label] = num(val)
		continue
	elif l_type in ['OP', 'OP2', 'MEM', 'PSEUDO']:
		pass
	elif l_type == 'COMMENT':
		continue
	else: # line is a label
		label = parse_line(line, l_type)[0]
		labels[label] = orig_addr + orig_offset
		continue
	
	orig_offset = orig_offset + 1

# the text to be written to the output file
mif_txt = ''

# reset variables for second pass
orig_addr = 0
orig_offset = 0

# deal with individual lines
for line in asm:
	line = line.strip()
	l_type = line_type(line)

	curr_addr = str(hex(orig_addr + orig_offset)) # TODO zero padding ?

	if l_type == 'ORIG':
		val = parse_line(line, l_type)[0] # returned list has one value
		addr_diff = num(val) - orig_addr
		mif_txt = (mif_txt + '[' + curr_addr + '..' + str(hex(num(val)-1))
			+ '] : DEAD;\n')
		orig_addr = num(val)
		orig_offset = 0
		continue # don't want to increment orig_offset
	elif l_type == 'NAME':
		continue
	elif l_type == 'OP':
		op, imm, rs, rt = parse_line(line, l_type)
		val =  trans_instr([op, imm, rs, rt], l_type, labels)
		mif_txt = (mif_txt + '-- @ ' + curr_addr + ' :\t\t\t' + op + '\t\t'
			+ rs + ',' + rt + ',' + imm + '\n')
		mif_txt = (mif_txt + curr_addr + ' : ' + str(hex(val)) + ';\n')
	elif l_type == 'OP2':
		op2, rd, rs, rt = parse_line(line, l_type)
		val =  trans_instr([op2, rd, rs, rt], l_type, labels)
		mif_txt = (mif_txt + '-- @ ' + curr_addr + ' :\t\t\t' + op2 + '\t\t'
			+ rd + ',' + rs + ',' + rt + '\n')
		mif_txt = (mif_txt + curr_addr + ' : ' + str(hex(val)) + ';\n')
	elif l_type == 'MEM':
		opm, imm, rs, rt = parse_line(line, l_type)
		val =  trans_instr([opm, imm, rs, rt], l_type, labels)
		mif_txt = (mif_txt + '-- @ ' + curr_addr + ' :\t\t\t' + opm + '\t\t'
			+ rs + ',' + rt + ',' + imm + '\n')
		mif_txt = (mif_txt + curr_addr + ' : ' + str(hex(val)) + ';\n')
	elif l_type == 'PSEUDO':
		args, s_type  = parse_pseudo(line)

		if s_type == 'OP':
			op, imm, rs, rt = args
			val =  trans_instr([op, imm, rs, rt], s_type, labels)
			mif_txt = (mif_txt + '-- @ ' + curr_addr + ' :\t\t\t' + op + '\t\t'
				+ rs + ',' + rt + ',' + imm + '\n')
			mif_txt = (mif_txt + curr_addr + ' : ' + str(hex(val)) + ';\n')
		elif s_type == 'OP2':
			op2, rd, rs, rt = args
			val =  trans_instr([op2, rd, rs, rt], s_type, labels)
			mif_txt = (mif_txt + '-- @ ' + curr_addr + ' :\t\t\t' + op2 + '\t\t'
				+ rd + ',' + rs + ',' + rt + '\n')
			mif_txt = (mif_txt + curr_addr + ' : ' + str(hex(val)) + ';\n')
		elif s_type == 'MEM':
			opm, imm, rs, rt = args
			val =  trans_instr([opm, imm, rs, rt], s_type, labels)
			mif_txt = (mif_txt + '-- @ ' + curr_addr + ' :\t\t\t' + opm + '\t\t'
				+ rs + ',' + rt + ',' + imm + '\n')
			mif_txt = (mif_txt + curr_addr + ' : ' + str(hex(val)) + ';\n')
	elif l_type == 'COMMENT':
		continue
	else: # line is a label
		continue

	orig_offset = orig_offset + 1

# writing to the output file
mif_f = open('output.mif', 'w+')
mif_f.write(mif_txt)
mif_f.close()
