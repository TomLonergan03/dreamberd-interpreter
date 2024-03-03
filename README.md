# DreamBerd Interpreter

A web-scale interpreter for the DreamBerd programming language. 
Find the DreamBerd spec [here](https://github.com/TodePond/DreamBerd/tree/main).

### Typechecking
We use state of the art typechecking, using AI inference to detect whether your program will compile!

### Dependencies
- `npm`
- `perl`
- `perl-json`

Required `npm` packages:
- `@mistralai/mistralai`
- `dotenv`

### Running
Launch the Powerful AST Representation Sythesis Extra-Rapid™ (P.A.R.S.E™) microservice with `./Main.pl path/to/code.db` while in `/parser`. Your AST will be produced in `ast.json`