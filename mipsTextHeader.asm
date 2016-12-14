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