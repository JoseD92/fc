move $fp, $sp
la $k1, _Fc_exit
li $k0, 0x0
jal _Fc_func
jal next

move $k1, $ra
li $k0, 0x0
j _Fc_finFunc

next:
move $k1, $ra
li $k0, 0x1
jal _Fc_push
li $k0, 0x2
jal _Fc_push
move $ra, $k1

move $k1, $ra
li $k0, 0x0
jal _Fc_func
jal next2

move $k1, $ra
li $k0, 0x8 #tam de los param, en este caso 8
j _Fc_finFunc

next2:
li $t0, 0xDEADBEAF
sw $t0, ($sp)
move $k1, $ra
li $k0, 0x0
j _Fc_finFunc

#todas las funciones de aqui tener cuidado, destruyen ra
# push: mete un valor a la pila, valor debe estar en $k0
_Fc_push: sw $k0, ($sp)
subiu $sp, $sp, 4
jr $ra

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

#retorna de una funcion y deja el valor retornado en k0
#k0 debe de tener la suma del tamano de los parametros con que se llamo a la funcion
#k1 debe tener la dir de retorno actual
#esta funcion llamar con un j incondicional, retornara de una vez a la funcion llamadora
_Fc_finFunc: addiu $sp, $fp, 8
addu $sp, $sp, $k0
lw $k0, 8($fp)
lw $ra, 4($fp)
lw $fp, ($fp)
jr $k1

_Fc_exit:
li $v0, 10
syscall