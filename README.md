# Simple-Calculus-in-Haskell-with-typechecker-and-parser

<h3>How to use</h3>
<p>Load the Project1.hs file into ghci. My_Parser.hs is the Parser module and it must be contained in the same directory of Project1.hs</p>

<h3>Functionalities:</h3>
<ul>
	<li><b>Interpreter</b>: starts an interactive interpreter. It will ask you to insert a term.
	(e.g. if iszero zero then succ zero else succ succ zero)</li>
	<li><b>typeTestT</b>: this function returns True if all the expressions you pass as an argument
	are well typed</li>
	<li><b>typeTestF</b>: this function returns True if all the expressions you pass as an argument
	contain type errors</li>
</ul>

<h3>Text Files</h3>
<p>I provide three files that I used to test the functions</p>
<ul>
	<li><b>FalseTest</b>: a list of expressions to use with typeTestF</li>
	<li><b>TrueTest</b>: a list of expressions to use with typeTestT</li>
	<li><b>TestExp</b>: expressions that must be parsed (calling Interpreter) and the values to obtain</li>
</ul>
