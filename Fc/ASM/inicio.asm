
# push: mete un valor a la pila, valor debe estar en $k0
_Fc_push: sw $k0, $sp
subiu $sp, $sp, 4

# crea block2 y block3 de las funciones, en k0 debe estar el tam de las variables de la funcion alineadas a 4
# el valor de retorno siempre valdra 4 aunque sea void
_Fc_func: sw $0, ($sp) #inicializa retorno en cero
subiu $sp, $sp, 4
sw $ra, ($sp)
subiu $sp, $sp, 4
sw $fp, ($sp)
move $sp, $fp
subiu $sp, $sp, 4
subu $sp, $sp, $k0