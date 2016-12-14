.text

li $t0, 0x2
sw $t0, 0x10040000

jal main

li $v0, 10
syscall

#recibe en v0 el tamano a reservar, retorna en v0 un apuntador al espacio reservado
#tener cuidado, destruye ra
#en valor recibido en v0 debe estar cuadrado a 4
_Fc_malloc: li $k1, 0x10040000
li $a0, 0x10000000

_Fc_malloc_fase1:
lw $k0, ($k1)
bne $k0, 0xDEADBEEF, _Fc_malloc_libre
move $a0, $k1
lw $k1, 8($k1)
j _Fc_malloc_fase1

_Fc_malloc_libre: beq $k0, 0x2, _Fc_malloc_isEnd
lw $k0, 4($k1)
bgtu $v0, $k0, _Fc_malloc_libre_insuficiente
addiu $v0, $k1, 16
li $k0, 0xDEADBEEF
sw $k0, ($k1)
j _Fc_malloc_fin

_Fc_malloc_libre_insuficiente:
move $a0, $k1
lw $k1, 8($k1)
j _Fc_malloc_fase1

_Fc_malloc_isEnd: li $k0, 0xDEADBEEF
sw $k0, ($k1)
sw $v0, 4($k1)
addiu $k0, $k1, 16
addu $k0, $k0, $v0
sw $k0, 8($k1)
li $v0, 0x2
sw $v0, ($k0)
sw $a0, 12($k1)
addiu $v0, $k1, 16

_Fc_malloc_fin: jr $ra


#en v0 debe estar la dir a memoria a liberar, retorna null si fallo
_Fc_free: lw $k0, -16($v0)
bne $k0, 0xDEADBEEF, _Fc_free_noValid
li $k0, 0x1
sw $k0, -16($v0)

#unificando con el anterior
lw $k0, -4($v0)
beq $k0, 0x10000000, _Fc_free_noAnt
lw $k1, ($k0)
bne $k1, 0x1, _Fc_free_noAnt
lw $k1, -12($v0)
addiu $k1,$k1,16
lw $a0, 4($k0)
addu $k1,$k1,$a0
sw $k1, 4($k0)
lw $k1, -8($v0)
sw $k1, 8($k0)
addiu $v0, $k0, 16

_Fc_free_noAnt:
#unificando con siguiente
lw $k0, -8($v0)
lw $k1, ($k0)
beq $k1, 0xDEADBEEF, _Fc_free_noNext
beq $k1, 0x1, _Fc_free_Nextfree
li $k0, 0x2
sw $k0, -16($v0)
j _Fc_free_fin
_Fc_free_Nextfree:
lw $k1, 4($k0)
addiu $k1, $k1, 16
lw $a0, -12($v0)
addu $k1, $k1, $a0
sw $k1, -12($v0)
lw $k1, 8($k0)
sw $k1, -8($v0)

_Fc_free_noNext:
j _Fc_free_fin
_Fc_free_noValid: li $v0, 0x0
_Fc_free_fin: jr $ra

# crea block2 y block3 de las funciones, en k0 debe estar el tam de las variables de la funcion alineadas a 4
# el valor de retorno siempre valdra 4 aunque sea void
# en k1 pasar la return adress actual
_Fc_func: sw $0, ($sp) #inicializa retorno en cero
subiu $sp, $sp, 4
sw $k1, ($sp)
subiu $sp, $sp, 4
sw $fp, ($sp)
move $fp, $sp
subiu $sp, $sp, 4
subu $sp, $sp, $k0
jr $ra
fc0:
fac:
li $k0,12
move $a2,$ra
jal _Fc_func
li $v0,76
jal _Fc_malloc
move $a3,$v0
move $ra,$a2
fc5:	addiu $t0,$fp,12
addiu $t1,$a3,0
sw $t0,($t1)
fc6:	addiu $t0,$a3,0
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,4
sw $t0,($t1)
fc7:	li $t0,1
addiu $t1,$a3,8
sw $t0,($t1)
fc8:	addiu $t0,$a3,4
lw $t0,($t0)
addiu $t1,$a3,8
lw $t1,($t1)
seq $t0,$t0,$t1
addiu $t1,$a3,12
sw $t0,($t1)
fc9:	addiu $t0,$a3,12
lw $t0,($t0)
bnez $t0, fc2
fc10:	j fc11
fc2:	li $t0,1
addiu $t1,$a3,16
sw $t0,($t1)
fc3:	addiu $t0,$a3,16
lw $t0,($t0)
sw $t0,8($fp)
j __fac
fc11:	addiu $t0,$fp,-8
addiu $t1,$a3,20
sw $t0,($t1)
fc12:	addiu $t0,$fp,12
addiu $t1,$a3,24
sw $t0,($t1)
fc13:	addiu $t0,$a3,24
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,28
sw $t0,($t1)
fc14:	li $t0,1
addiu $t1,$a3,32
sw $t0,($t1)
fc15:	addiu $t0,$a3,28
lw $t0,($t0)
addiu $t1,$a3,32
lw $t1,($t1)
sub $t0,$t0,$t1
addiu $t1,$a3,36
sw $t0,($t1)
fc16:	addiu $t0,$a3,36
lw $t0,($t0)
sw $t0, ($sp)
subiu $sp, $sp, 4
fc17:	move $k1, $ra
sw $a3, -4($fp)
jal fac
addiu $sp, $fp, 12
lw $t0, 8($fp)
lw $ra, 4($fp)
lw $fp, ($fp)
lw $a3, -4($fp)
addiu $t1,$a3,40
sw $t0,($t1)
fc19:	addiu $t0,$a3,40
lw $t0,($t0)
addiu $t1,$a3,20
lw $t1,($t1)
sw $t0,($t1)
fc20:	addiu $t0,$fp,-8
addiu $t1,$a3,44
sw $t0,($t1)
fc21:	addiu $t0,$fp,-8
addiu $t1,$a3,48
sw $t0,($t1)
fc22:	addiu $t0,$a3,48
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,52
sw $t0,($t1)
fc23:	addiu $t0,$fp,12
addiu $t1,$a3,56
sw $t0,($t1)
fc24:	addiu $t0,$a3,56
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,60
sw $t0,($t1)
fc25:	addiu $t0,$a3,52
lw $t0,($t0)
addiu $t1,$a3,60
lw $t1,($t1)
mul $t0,$t0,$t1
addiu $t1,$a3,64
sw $t0,($t1)
fc26:	addiu $t0,$a3,64
lw $t0,($t0)
addiu $t1,$a3,44
lw $t1,($t1)
sw $t0,($t1)
fc27:	addiu $t0,$fp,-8
addiu $t1,$a3,68
sw $t0,($t1)
fc28:	addiu $t0,$a3,68
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,72
sw $t0,($t1)
fc29:	addiu $t0,$a3,72
lw $t0,($t0)
sw $t0,8($fp)
j __fac
fc1:
__fac:
move $a2,$ra
move $v0,$a3
jal _Fc_free
jr $a2
fc30:
fac2:
li $k0,12
move $a2,$ra
jal _Fc_func
li $v0,76
jal _Fc_malloc
move $a3,$v0
move $ra,$a2
fc32:	addiu $t0,$fp,-8
addiu $t1,$a3,0
sw $t0,($t1)
fc33:	li $t0,1
addiu $t1,$a3,4
sw $t0,($t1)
fc34:	addiu $t0,$a3,4
lw $t0,($t0)
addiu $t1,$a3,0
lw $t1,($t1)
sw $t0,($t1)
fc50:	addiu $t0,$fp,12
addiu $t1,$a3,8
sw $t0,($t1)
fc51:	addiu $t0,$a3,8
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,12
sw $t0,($t1)
fc52:	li $t0,1
addiu $t1,$a3,16
sw $t0,($t1)
fc53:	addiu $t0,$a3,12
lw $t0,($t0)
addiu $t1,$a3,16
lw $t1,($t1)
sgt $t0,$t0,$t1
addiu $t1,$a3,20
sw $t0,($t1)
fc54:	addiu $t0,$a3,20
lw $t0,($t0)
bnez $t0, fc37
fc55:	j fc56
fc37:	addiu $t0,$fp,-8
addiu $t1,$a3,24
sw $t0,($t1)
fc38:	addiu $t0,$fp,-8
addiu $t1,$a3,28
sw $t0,($t1)
fc39:	addiu $t0,$a3,28
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,32
sw $t0,($t1)
fc40:	addiu $t0,$fp,12
addiu $t1,$a3,36
sw $t0,($t1)
fc41:	addiu $t0,$a3,36
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,40
sw $t0,($t1)
fc42:	addiu $t0,$a3,32
lw $t0,($t0)
addiu $t1,$a3,40
lw $t1,($t1)
mul $t0,$t0,$t1
addiu $t1,$a3,44
sw $t0,($t1)
fc43:	addiu $t0,$a3,44
lw $t0,($t0)
addiu $t1,$a3,24
lw $t1,($t1)
sw $t0,($t1)
fc44:	addiu $t0,$fp,12
addiu $t1,$a3,48
sw $t0,($t1)
fc45:	addiu $t0,$fp,12
addiu $t1,$a3,52
sw $t0,($t1)
fc46:	addiu $t0,$a3,52
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,56
sw $t0,($t1)
fc47:	li $t0,1
addiu $t1,$a3,60
sw $t0,($t1)
fc48:	addiu $t0,$a3,56
lw $t0,($t0)
addiu $t1,$a3,60
lw $t1,($t1)
sub $t0,$t0,$t1
addiu $t1,$a3,64
sw $t0,($t1)
fc49:	addiu $t0,$a3,64
lw $t0,($t0)
addiu $t1,$a3,48
lw $t1,($t1)
sw $t0,($t1)
fc36:	j fc50
fc56:	addiu $t0,$fp,-8
addiu $t1,$a3,68
sw $t0,($t1)
fc57:	addiu $t0,$a3,68
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,72
sw $t0,($t1)
fc58:	addiu $t0,$a3,72
lw $t0,($t0)
sw $t0,8($fp)
j __fac2
fc31:
__fac2:
move $a2,$ra
move $v0,$a3
jal _Fc_free
jr $a2
fc59:
main:
li $k0,16
move $a2,$ra
jal _Fc_func
li $v0,88
jal _Fc_malloc
move $a3,$v0
move $ra,$a2
fc61:	addiu $t0,$fp,-12
addiu $t1,$a3,0
sw $t0,($t1)
fc62:	addiu $t0,$a3,0
lw $t0,($t0)
sw $t0, ($sp)
subiu $sp, $sp, 4
fc63:	li $v0, 5
syscall
addiu $sp,$sp,4
lw $t0,($sp)
sw $v0,($t0)
fc106:	addiu $t0,$fp,-12
addiu $t1,$a3,4
sw $t0,($t1)
fc107:	addiu $t0,$a3,4
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,8
sw $t0,($t1)
fc108:	li $t0,1
addiu $t1,$a3,12
sw $t0,($t1)
fc109:	addiu $t0,$a3,8
lw $t0,($t0)
addiu $t1,$a3,12
lw $t1,($t1)
seq $t0,$t0,$t1
addiu $t1,$a3,16
sw $t0,($t1)
fc110:	addiu $t0,$a3,16
lw $t0,($t0)
bnez $t0, fc67
fc111:	j fc60
fc67:	addiu $t0,$fp,-12
addiu $t1,$a3,20
sw $t0,($t1)
fc68:	addiu $t0,$a3,20
lw $t0,($t0)
sw $t0, ($sp)
subiu $sp, $sp, 4
fc69:	li $v0, 5
syscall
addiu $sp,$sp,4
lw $t0,($sp)
sw $v0,($t0)
fc71:	addiu $t0,$fp,-8
addiu $t1,$a3,24
sw $t0,($t1)
fc72:	addiu $t0,$a3,24
lw $t0,($t0)
sw $t0, ($sp)
subiu $sp, $sp, 4
fc73:	li $v0, 5
syscall
addiu $sp,$sp,4
lw $t0,($sp)
sw $v0,($t0)
fc91:	addiu $t0,$fp,-12
addiu $t1,$a3,28
sw $t0,($t1)
fc92:	addiu $t0,$a3,28
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,32
sw $t0,($t1)
fc93:	li $t0,1
addiu $t1,$a3,36
sw $t0,($t1)
fc94:	addiu $t0,$a3,32
lw $t0,($t0)
addiu $t1,$a3,36
lw $t1,($t1)
seq $t0,$t0,$t1
addiu $t1,$a3,40
sw $t0,($t1)
fc95:	addiu $t0,$a3,40
lw $t0,($t0)
bnez $t0, fc75
fc96:	j fc82
fc75:	addiu $t0,$fp,12
addiu $t1,$a3,44
sw $t0,($t1)
fc76:	addiu $t0,$fp,-8
addiu $t1,$a3,48
sw $t0,($t1)
fc77:	addiu $t0,$a3,48
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,52
sw $t0,($t1)
fc78:	addiu $t0,$a3,52
lw $t0,($t0)
sw $t0, ($sp)
subiu $sp, $sp, 4
fc79:	move $k1, $ra
sw $a3, -4($fp)
jal fac
addiu $sp, $fp, 12
lw $t0, 8($fp)
lw $ra, 4($fp)
lw $fp, ($fp)
lw $a3, -4($fp)
addiu $t1,$a3,56
sw $t0,($t1)
fc81:	addiu $t0,$a3,56
lw $t0,($t0)
addiu $t1,$a3,44
lw $t1,($t1)
sw $t0,($t1)
fc90:	j fc97
fc82:	addiu $t0,$fp,12
addiu $t1,$a3,60
sw $t0,($t1)
fc83:	addiu $t0,$fp,-8
addiu $t1,$a3,64
sw $t0,($t1)
fc84:	addiu $t0,$a3,64
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,68
sw $t0,($t1)
fc85:	addiu $t0,$a3,68
lw $t0,($t0)
sw $t0, ($sp)
subiu $sp, $sp, 4
fc86:	move $k1, $ra
sw $a3, -4($fp)
jal fac2
addiu $sp, $fp, 12
lw $t0, 8($fp)
lw $ra, 4($fp)
lw $fp, ($fp)
lw $a3, -4($fp)
addiu $t1,$a3,72
sw $t0,($t1)
fc88:	addiu $t0,$a3,72
lw $t0,($t0)
addiu $t1,$a3,60
lw $t1,($t1)
sw $t0,($t1)
fc97:	addiu $t0,$fp,12
addiu $t1,$a3,76
sw $t0,($t1)
fc98:	addiu $t0,$a3,76
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,80
sw $t0,($t1)
fc99:	addiu $t0,$a3,80
lw $t0,($t0)
sw $t0, ($sp)
subiu $sp, $sp, 4
fc100:	addiu $sp,$sp,4
lw $a0,($sp)
li $v0, 1
syscall
fc102:	addiu $t0,$fp,-12
addiu $t1,$a3,84
sw $t0,($t1)
fc103:	addiu $t0,$a3,84
lw $t0,($t0)
sw $t0, ($sp)
subiu $sp, $sp, 4
fc104:	li $v0, 5
syscall
addiu $sp,$sp,4
lw $t0,($sp)
sw $v0,($t0)
fc66:	j fc106
fc60:
__main:
move $a2,$ra
move $v0,$a3
jal _Fc_free
jr $a2
