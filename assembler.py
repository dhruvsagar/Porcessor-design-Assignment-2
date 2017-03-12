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

asm_registers = {
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
	line, comment = line.split(';', 1) # get rid of in-line comment text
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
		op, rs, rt, imm = line.split(',') # note the position of imm
		ret_list = [op, imm, rs, rt] # note the switched position of imm
	elif l_type == 'OP2':
		op2, rd, rs, rt = line.split(',')
		ret_list = [op2, rd, rs, rt]
	elif l_type == 'MEM':
		opm, rt, imm_rs = line.split(',')
		imm, rs = imm_rs.split('(')
		rs = rs[:-1] # strip trailing ')'
		ret_list = [op, imm, rt, rs] # imm could also be a label
	else: # line is a label
		label = line[:-1] # strip trailing ':'
		ret_list = [label]

	for i in range(len(ret_list)):
		ret_list[i] = ret_list[i].strip()
		ret_list[i] = ret_list[i].upper()

	return ret_list

def parse_pseudo(line):
	first_word, rest_of_line = line.split(maxsplit=1)
	first_word = first_word.upper()

	line, comment = line.split(';', 1) # get rid of in-line comment text
	line = line.strip()

	args, i_type = [], '' 

	if first_word == 'NOT':
		i_type = 'OP2'
		op, ri, rj = line.split(',')
		args = ['NAND', ri, rj, rj]
	elif first_word == 'RET':
		i_type = 'MEM'
		args = ['JAL', '0', 'RA', 'R10']
	elif first_word == 'BGT':
		i_type = 'OP'
		op, ry, rx, label = line.split(',')
		args = ['BLT', label, rx, ry]
	elif first_word == 'BR':
		i_type = 'OP'
		op, label = line.split(',')
		args = ['BEQ', label, 'Zero', 'Zero']
	elif first_word == 'GE':
		i_type = 'OP2'
		op, rz, ry, rx = line.split(',')
		args = ['LE', rz, rx, ry]
	elif first_word == 'CALL':
		i_type = 'MEM'
		op, imm_ri = line.split(',')
		imm, ri = imm_ri.split('(')
		ri = ri[:-1] # strip trailing ')'
		args = ['JAL', imm, ri, 'R5'] # imm could also be a label
	elif first_word == 'JMP':
		i_type = 'MEM'
		op, imm_ri = line.split(',')
		imm, ri = imm_ri.split('(')
		ri = ri[:-1] # strip trailing ')'
		args = ['JAL', imm, ri, 'R10'] # imm could also be a label
	elif first_word == 'BGE':
		i_type = 'OP'
		op, ry, rx, label = line.split(',')
		args = ['BLE', label, rx, ry]
	elif first_word == 'GT':
		i_type = 'OP2'
		op, rz, ry, rx = line.split(',')
		args = ['LT', rz, rx, ry]
	else: # instruction is SUBI
		i_type = 'OP'
		op, ry, rx, imm = line.split(',')
		args = ['ADDI', str(-int(imm)), ry, rx] # str to match other args

	for i in range(len(args)):
		args[i] = args[i].strip()
		args[i] = args[i].upper()

	return args, i_type

def trans_instr(args, i_type, labels):
	instr = 0

	if i_type == 'OP':
		op, imm, rs, rt = args[0], args[1], args[2], args[3]
		op, imm, rs, rt = op_codes[op], int(imm), int(rs), int(rt)
		instr = (op<<26) + (imm<<8) + (rs<<4) + rt
	elif i_type == 'OP2':
		op2, rd, rs, rt = args[0], args[1], args[2], args[3]
		op2, rd, rs, rt = ex_codes[op2], int(imm), int(rs), int(rt)
		instr = (op2<<18) + (rd<<8) + (rs<<4) + (rt)
	else: # instuction is memory instruction
		opm, imm, rs, rt = args[0], args[1], args[2], args[3]
		imm = labels[imm] if imm in labels else int(imm)
		opm, rs, rt = op_codes[opm], int(rs), int(rt)
		instr = (opm<<26) + (imm<<8) + (rs<<4) + rt

	return instr

def line_type(line):
	first_word, rest_of_line = line.split(maxsplit=1)
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
	elif first_word[0] == ';'
		return 'COMMENT'
	else:
		return 'LBL'

#
# MAIN FLOW OF EXECUTION-------------------------------------------------------
#

# check for correct number of arguments
if len(sys.argv) != 2:
	print('The amount of arguments is incorrect, assembler is exiting...')
	sys.exit()

# read from the input file.
asm = ''
with open(sys.argv(1), 'r') as f:
	asm = f.read()

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
		orig_addr = int(val)
		orig_offset = 0
		continue # don't want to increment orig_offset
	elif l_type == 'NAME':
		label, val = parse_line(line, l_type)
		labels[label] = int(val)
		continue
	elif l_type in ['OP', 'OP2', 'MEM', 'PSEUDO']:
		pass
	elif l_type == 'COMMENT':
		continue
	else: # line is a label
		label = parse_line(line, l_type)[0]
		labels[label] = orig_addr + orig_offset
		continue
	
	orig_offset++

# open output file for writing
mif_f = open('output.mif', 'w+')

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
		addr_diff = val - orig_addr
		mif_f.write('[' + curr_addr + '..' + str(hex(int(val)-1))
			+ '] : DEAD;\n')
		orig_addr = int(val)
		orig_offset = 0
		continue # don't want to increment orig_offset
	elif l_type == 'NAME':
		continue
	elif l_type == 'OP':
		op, imm, rs, rt = parse_line(line, l_type)
		val =  trans_instr([op, imm, rs, rt], l_type, labels)
		mif_f.write('-- @ ' + curr_addr + ' :\t\t\t' + op + '\t\t'
			+ rs + ',' + rt + ',' + imm '\n')
		mif_f.write(curr_addr + ' : ' + str(hex(val)) + ';\n')
	elif l_type == 'OP2':
		op2, rd, rs, rt = parse_line(line, l_type)
		val =  trans_instr([op2, rd, rs, rt], l_type, labels)
		mif_f.write('-- @ ' + curr_addr + ' :\t\t\t' + op2 + '\t\t'
			+ rd + ',' + rs + ',' + rt '\n')
		mif_f.write(curr_addr + ' : ' + str(hex(val)) + ';\n')
	elif l_type == 'MEM':
		opm, imm, rs, rt = parse_line(line, l_type)
		val =  trans_instr([opm, imm, rs, rt], l_type, labels)
		mif_f.write('-- @ ' + curr_addr + ' :\t\t\t' + opm + '\t\t'
			+ rs + ',' + rt + ',' + imm '\n')
		mif_f.write(curr_addr + ' : ' + str(hex(val)) + ';\n')
	elif l_type == 'PSEUDO':
		args, s_type  = parse_pseudo(line)

		if s_type == 'OP':
			op, imm, rs, rt = args
			val =  trans_instr([op, imm, rs, rt], l_type, labels)
			mif_f.write('-- @ ' + curr_addr + ' :\t\t\t' + op + '\t\t'
				+ rs + ',' + rt + ',' + imm '\n')
			mif_f.write(curr_addr + ' : ' + str(hex(val)) + ';\n')
		elif s_type == 'OP2':
			op2, rd, rs, rt = args
			val =  trans_instr([op2, rd, rs, rt], l_type, labels)
			mif_f.write('-- @ ' + curr_addr + ' :\t\t\t' + op2 + '\t\t'
				+ rd + ',' + rs + ',' + rt '\n')
			mif_f.write(curr_addr + ' : ' + str(hex(val)) + ';\n')
		elif s_type == 'MEM':
			opm, imm, rs, rt = args
			val =  trans_instr([opm, imm, rs, rt], l_type, labels)
			mif_f.write('-- @ ' + curr_addr + ' :\t\t\t' + opm + '\t\t'
				+ rs + ',' + rt + ',' + imm '\n')
			mif_f.write(curr_addr + ' : ' + str(hex(val)) + ';\n')
	elif l_type == 'COMMENT':
		continue
	else: # line is a label
		continue

	orig_offset++

# close output file
mif_f.close()
