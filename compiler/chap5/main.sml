structure Main = 
struct 
    fun compile file = 
        let 
        in 
            print (file ^ ":\n");
            let 
                val ast = Parse.parse file 
            in 
                print "The ast is: \n";
                PrintAbsyn.print(TextIO.stdOut, ast);
                Semant.transProg(ast)
            end 
        end 
end 