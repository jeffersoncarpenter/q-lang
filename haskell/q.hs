import System.IO

main = do
    source <- readFile "test.q"
    execute source

execute :: String -> IO ()
execute source = runContext context
	where context = execute (return ()) source


data StringMatcher = WordMatcher String
                   | WhitespaceMatcher
				   | SymbolMatcher
				   | ImportMatcher
				   

data Value = Type String
           | Function
				   
				   
type StdOut = String

data ExecutionContext a = ExecutionContext [Value] StringMatcher StdOut a

initExecutionContext :: a -> ExecutionContext a
initExecutionContext result = ExecutionContext initialValues initialStringMatcher result
	where
		initialValues = []
		initialStringMatcher = ImportMatcher

ignoreExecutionContext :: ExecutionContext a -> (a -> b) -> ExecutionContext b
ignoreExecutionContext (ExecutionContext values matcher result) func = ExecutionContext values matcher (func result)

appendExecutionContext :: ExecutionContext a -> (a -> ExecutionContext b) -> ExecutionContext b
appendExecutionContext (ExecutionContext values stringMatcher output result
                       = ExecutionContext (values `concat` resultValues) resultMatcher (output ++ resultOutput)
						    where
							    ExecutionContext resultValues resultMatcher resultOutput