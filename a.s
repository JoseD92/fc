.data
fc79: .asciiz "\nIndique numero a calcular raiz: "
fc64: .asciiz "\nQuiere Continuar (si=1): "
.align 4
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
raiz:
sw $0, ($sp)
sw $ra, -4($sp)
sw $fp, -8($sp)
subiu $fp, $sp, 8
subiu $sp, $sp, 204
addiu $a3,$sp,4
fc2:	addiu $t0,$fp,-16
addiu $t1,$a3,0
sw $t0,($t1)
fc3:	addiu $t0,$fp,12
addiu $t1,$a3,4
sw $t0,($t1)
fc4:	addiu $t0,$a3,4
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,8
sw $t0,($t1)
fc5:	addiu $t0,$a3,8
lw $t0,($t0)
addiu $t1,$a3,0
lw $t1,($t1)
sw $t0,($t1)
fc6:	addiu $t0,$fp,-12
addiu $t1,$a3,12
sw $t0,($t1)
fc7:	addiu $t0,$fp,12
addiu $t1,$a3,16
sw $t0,($t1)
fc8:	addiu $t0,$a3,16
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,20
sw $t0,($t1)
fc9:	li $t0,1077936128 #float
addiu $t1,$a3,24
sw $t0,($t1)
fc10:	addiu $t0,$a3,20
lw $t0,($t0)
addiu $t1,$a3,24
lw $t1,($t1)
mtc1 $t0, $f0
mtc1 $t1, $f1
div.s $f0,$f0,$f1
mfc1 $t0, $f0
addiu $t1,$a3,28
sw $t0,($t1)
fc11:	addiu $t0,$a3,28
lw $t0,($t0)
addiu $t1,$a3,12
lw $t1,($t1)
sw $t0,($t1)
fc12:	addiu $t0,$fp,-8
addiu $t1,$a3,32
sw $t0,($t1)
fc13:	li $t0,100000
addiu $t1,$a3,36
sw $t0,($t1)
fc14:	addiu $t0,$a3,36
lw $t0,($t0)
addiu $t1,$a3,32
lw $t1,($t1)
sw $t0,($t1)
fc52:	addiu $t0,$fp,-8
addiu $t1,$a3,40
sw $t0,($t1)
fc53:	addiu $t0,$a3,40
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,44
sw $t0,($t1)
fc54:	li $t0,0
addiu $t1,$a3,48
sw $t0,($t1)
fc55:	addiu $t0,$a3,44
lw $t0,($t0)
addiu $t1,$a3,48
lw $t1,($t1)
sgt $t0,$t0,$t1
addiu $t1,$a3,52
sw $t0,($t1)
fc56:	addiu $t0,$a3,52
lw $t0,($t0)
bnez $t0, fc17
fc57:	j fc58
fc17:	addiu $t0,$fp,-12
addiu $t1,$a3,56
sw $t0,($t1)
fc18:	addiu $t0,$fp,-12
addiu $t1,$a3,60
sw $t0,($t1)
fc19:	addiu $t0,$a3,60
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,64
sw $t0,($t1)
fc20:	addiu $t0,$fp,12
addiu $t1,$a3,68
sw $t0,($t1)
fc21:	addiu $t0,$a3,68
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,72
sw $t0,($t1)
fc22:	addiu $t0,$fp,-12
addiu $t1,$a3,76
sw $t0,($t1)
fc23:	addiu $t0,$a3,76
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,80
sw $t0,($t1)
fc24:	addiu $t0,$a3,72
lw $t0,($t0)
addiu $t1,$a3,80
lw $t1,($t1)
mtc1 $t0, $f0
mtc1 $t1, $f1
div.s $f0,$f0,$f1
mfc1 $t0, $f0
addiu $t1,$a3,84
sw $t0,($t1)
fc25:	addiu $t0,$a3,64
lw $t0,($t0)
addiu $t1,$a3,84
lw $t1,($t1)
mtc1 $t0, $f0
mtc1 $t1, $f1
add.s $f0,$f0,$f1
mfc1 $t0, $f0
addiu $t1,$a3,88
sw $t0,($t1)
fc26:	li $t0,1073741824 #float
addiu $t1,$a3,92
sw $t0,($t1)
fc27:	addiu $t0,$a3,88
lw $t0,($t0)
addiu $t1,$a3,92
lw $t1,($t1)
mtc1 $t0, $f0
mtc1 $t1, $f1
div.s $f0,$f0,$f1
mfc1 $t0, $f0
addiu $t1,$a3,96
sw $t0,($t1)
fc28:	addiu $t0,$a3,96
lw $t0,($t0)
addiu $t1,$a3,56
lw $t1,($t1)
sw $t0,($t1)
fc29:	addiu $t0,$fp,-8
addiu $t1,$a3,100
sw $t0,($t1)
fc30:	addiu $t0,$fp,-8
addiu $t1,$a3,104
sw $t0,($t1)
fc31:	addiu $t0,$a3,104
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,108
sw $t0,($t1)
fc32:	li $t0,1
addiu $t1,$a3,112
sw $t0,($t1)
fc33:	addiu $t0,$a3,108
lw $t0,($t0)
addiu $t1,$a3,112
lw $t1,($t1)
sub $t0,$t0,$t1
addiu $t1,$a3,116
sw $t0,($t1)
fc34:	addiu $t0,$a3,116
lw $t0,($t0)
addiu $t1,$a3,100
lw $t1,($t1)
sw $t0,($t1)
fc39:	addiu $t0,$fp,-16
addiu $t1,$a3,120
sw $t0,($t1)
fc40:	addiu $t0,$a3,120
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,124
sw $t0,($t1)
fc41:	addiu $t0,$fp,-12
addiu $t1,$a3,128
sw $t0,($t1)
fc42:	addiu $t0,$a3,128
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,132
sw $t0,($t1)
fc43:	addiu $t0,$a3,124
lw $t0,($t0)
addiu $t1,$a3,132
lw $t1,($t1)
mtc1 $t0, $f0
mtc1 $t1, $f1
sub.s $f0,$f0,$f1
mfc1 $t0, $f0
addiu $t1,$a3,136
sw $t0,($t1)
fc44:	li $t0,897988541 #float
addiu $t1,$a3,140
sw $t0,($t1)
fc45:	addiu $t0,$a3,136
lw $t0,($t0)
addiu $t1,$a3,140
lw $t1,($t1)
slt $t0,$t0,$t1
addiu $t1,$a3,144
sw $t0,($t1)
fc46:	addiu $t0,$a3,144
lw $t0,($t0)
bnez $t0, fc35
fc47:	j fc48
fc35:	addiu $t0,$fp,-12
addiu $t1,$a3,148
sw $t0,($t1)
fc36:	addiu $t0,$a3,148
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,152
sw $t0,($t1)
fc37:	addiu $t0,$a3,152
lw $t0,($t0)
sw $t0,8($fp)
j __raiz
fc48:	addiu $t0,$fp,-16
addiu $t1,$a3,156
sw $t0,($t1)
fc49:	addiu $t0,$fp,-12
addiu $t1,$a3,160
sw $t0,($t1)
fc50:	addiu $t0,$a3,160
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,164
sw $t0,($t1)
fc51:	addiu $t0,$a3,164
lw $t0,($t0)
addiu $t1,$a3,156
lw $t1,($t1)
sw $t0,($t1)
fc16:	j fc52
fc58:	addiu $t0,$fp,-12
addiu $t1,$a3,168
sw $t0,($t1)
fc59:	addiu $t0,$a3,168
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,172
sw $t0,($t1)
fc60:	addiu $t0,$a3,172
lw $t0,($t0)
sw $t0,8($fp)
j __raiz
fc1:
__raiz:
lw $ra, 4($fp)
jr $ra
fc61:
main:
sw $0, ($sp)
sw $ra, -4($sp)
sw $fp, -8($sp)
subiu $fp, $sp, 8
subiu $sp, $sp, 124
addiu $a3,$sp,4
fc63:	addiu $t0,$fp,-8
addiu $t1,$a3,0
sw $t0,($t1)
fc65:	la $t0, fc64
addiu $t1,$a3,4
sw $t0,($t1)
fc66:	addiu $t0,$a3,4
lw $t0,($t0)
addiu $t1,$a3,0
lw $t1,($t1)
sw $t0,($t1)
fc67:	addiu $t0,$fp,-8
addiu $t1,$a3,8
sw $t0,($t1)
fc68:	addiu $t0,$a3,8
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,12
sw $t0,($t1)
fc69:	addiu $t0,$a3,12
lw $t0,($t0)
sw $t0, ($sp)
subiu $sp, $sp, 4
fc70:	addiu $sp,$sp,4
lw $a0,($sp)
li $v0, 4
syscall
fc72:	addiu $t0,$fp,12
addiu $t1,$a3,16
sw $t0,($t1)
fc73:	addiu $t0,$a3,16
lw $t0,($t0)
sw $t0, ($sp)
subiu $sp, $sp, 4
fc74:	li $v0, 5
syscall
addiu $sp,$sp,4
lw $t0,($sp)
sw $v0,($t0)
fc115:	addiu $t0,$fp,12
addiu $t1,$a3,20
sw $t0,($t1)
fc116:	addiu $t0,$a3,20
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,24
sw $t0,($t1)
fc117:	li $t0,1
addiu $t1,$a3,28
sw $t0,($t1)
fc118:	addiu $t0,$a3,24
lw $t0,($t0)
addiu $t1,$a3,28
lw $t1,($t1)
seq $t0,$t0,$t1
addiu $t1,$a3,32
sw $t0,($t1)
fc119:	addiu $t0,$a3,32
lw $t0,($t0)
bnez $t0, fc78
fc120:	j fc62
fc78:	addiu $t0,$fp,-8
addiu $t1,$a3,36
sw $t0,($t1)
fc80:	la $t0, fc79
addiu $t1,$a3,40
sw $t0,($t1)
fc81:	addiu $t0,$a3,40
lw $t0,($t0)
addiu $t1,$a3,36
lw $t1,($t1)
sw $t0,($t1)
fc82:	addiu $t0,$fp,-8
addiu $t1,$a3,44
sw $t0,($t1)
fc83:	addiu $t0,$a3,44
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,48
sw $t0,($t1)
fc84:	addiu $t0,$a3,48
lw $t0,($t0)
sw $t0, ($sp)
subiu $sp, $sp, 4
fc85:	addiu $sp,$sp,4
lw $a0,($sp)
li $v0, 4
syscall
fc87:	addiu $t0,$fp,-12
addiu $t1,$a3,52
sw $t0,($t1)
fc88:	addiu $t0,$a3,52
lw $t0,($t0)
sw $t0, ($sp)
subiu $sp, $sp, 4
fc89:	li $v0, 6
syscall
addiu $sp,$sp,4
lw $t0,($sp)
s.s $f0,($t0)
fc91:	addiu $t0,$fp,-12
addiu $t1,$a3,56
sw $t0,($t1)
fc92:	addiu $t0,$fp,-12
addiu $t1,$a3,60
sw $t0,($t1)
fc93:	addiu $t0,$a3,60
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,64
sw $t0,($t1)
fc94:	addiu $t0,$a3,64
lw $t0,($t0)
sw $t0, ($sp)
subiu $sp, $sp, 4
fc95:	sw $a3, -4($fp)
jal raiz
addiu $sp, $fp, 12
lw $t0, 8($fp)
lw $fp, ($fp)
lw $a3, -4($fp)
addiu $t1,$a3,68
sw $t0,($t1)
fc97:	addiu $t0,$a3,68
lw $t0,($t0)
addiu $t1,$a3,56
lw $t1,($t1)
sw $t0,($t1)
fc98:	addiu $t0,$fp,-12
addiu $t1,$a3,72
sw $t0,($t1)
fc99:	addiu $t0,$a3,72
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,76
sw $t0,($t1)
fc100:	addiu $t0,$a3,76
lw $t0,($t0)
sw $t0, ($sp)
subiu $sp, $sp, 4
fc101:	addiu $sp,$sp,4
l.s $f12,($sp)
li $v0, 2
syscall
fc103:	addiu $t0,$fp,-8
addiu $t1,$a3,80
sw $t0,($t1)
fc104:	la $t0, fc64
addiu $t1,$a3,84
sw $t0,($t1)
fc105:	addiu $t0,$a3,84
lw $t0,($t0)
addiu $t1,$a3,80
lw $t1,($t1)
sw $t0,($t1)
fc106:	addiu $t0,$fp,-8
addiu $t1,$a3,88
sw $t0,($t1)
fc107:	addiu $t0,$a3,88
lw $t0,($t0)
lw $t0,($t0)
addiu $t1,$a3,92
sw $t0,($t1)
fc108:	addiu $t0,$a3,92
lw $t0,($t0)
sw $t0, ($sp)
subiu $sp, $sp, 4
fc109:	addiu $sp,$sp,4
lw $a0,($sp)
li $v0, 4
syscall
fc111:	addiu $t0,$fp,12
addiu $t1,$a3,96
sw $t0,($t1)
fc112:	addiu $t0,$a3,96
lw $t0,($t0)
sw $t0, ($sp)
subiu $sp, $sp, 4
fc113:	li $v0, 5
syscall
addiu $sp,$sp,4
lw $t0,($sp)
sw $v0,($t0)
fc77:	j fc115
fc62:
__main:
lw $ra, 4($fp)
jr $ra
