all: fcmain.exe

./Fc/Grammar.hs: ./Fc/Grammar.y ./Fc/Tabla.hs ./Fc/MyState.hs ./Fc/Datas.hs
	happy ./Fc/Grammar.y -o ./Fc/Grammar.hs --info

./Fc/Lexer.hs: ./Fc/Tokens.x
	alex ./Fc/Tokens.x -o ./Fc/Lexer.hs

fcmain.exe: fcmain.hs ./Fc/Lexer.hs ./Fc/Grammar.hs ./Fc/Tabla.hs ./Fc/MyState.hs ./Fc/Tac/Tac.hs ./Fc/Tac/TacOp.hs
	ghc fcmain.hs

winclean:
	del *.o
	del *.hi

clean:
	rm *.o
	rm *.hi
	rm ./Fc/*.hi
	rm ./Fc/*.hi
