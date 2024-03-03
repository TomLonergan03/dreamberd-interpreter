package main

import (
	"fmt"
	"io"
	"os"
	"os/exec"
	"runtime"
	"strings"

	"github.com/gdamore/tcell"
)

var cliName string = "dreamREPL"
var temp_filepath string = "../command.berd"
var script_filepath string = "../entry.sh"

var clear map[string]func()     //create a map for storing clear functions
var commands map[string]func()  //map for storing possible functions
var backspace map[string]func() //map for backspace functions

func init() {
	clear = make(map[string]func())
	clear["linux"] = func() {
		cmd := exec.Command("clear")
		cmd.Stdout = os.Stdout
		cmd.Run()
	}
	clear["windows"] = func() {
		cmd := exec.Command("cmd", "/c", "cls")
		cmd.Stdout = os.Stdout
		cmd.Run()
	}

	commands = make(map[string]func())
	commands["help"] = displayHelp
	commands["clear"] = clearScreen

	backspace = make(map[string]func())
	backspace["windows"] = func() {
		cmd := exec.Command("cmd", "/c", "cls")
		cmd.Stdout = os.Stdout
		cmd.Run()
	}
	backspace["linux"] = func() {
		// Move cursor back, print a space, move cursor back again
		fmt.Print("\b \b")
	}
}

func printPrompt() {
	fmt.Print(cliName, "> ")
}

func printUnknown(text string) {
	fmt.Println(text, ": command not found")
}

func displayHelp() {
	fmt.Println()
	fmt.Print("Welcome to ", cliName, "! These are the available commands: \r\n")
	fmt.Print("help    - Show available commands\r\n")
	fmt.Print("clear   - Clear the terminal screen\r\n")
	fmt.Print("exit    - Closes the terminal\r\n")
	fmt.Print("read(file_path) - Read the file at file_path for DreamBerd interpretation\r\n")
	fmt.Print("run(code_snippet) - Send code_snippet to the DreamBerd interpreter\r\n")
	fmt.Println()
	fmt.Print("Note that multi-line inputs can be achieved using CTRL+SPACE, rather than SHIFT+ENTER\r\n")
	fmt.Println()
}

func clearScreen() {
	clear_function, result := clear[runtime.GOOS]
	if result {
		clear_function()
		printPrompt()
	} else {
		panic("Unsupported OS!")
	}
}

func readFile(filePath string) string {
	file, err := os.Open(filePath)
	if err != nil {
		fmt.Print("Error: ", err, "\r\n")
		return ""
	}
	defer file.Close()
	content, err := io.ReadAll(file)
	if err != nil {
		fmt.Print("Error: ", err, "\r\n")
	}
	return string(content)
}

func run(snippet string) {
	file, err := os.OpenFile(temp_filepath, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0666)
	if err != nil {
		fmt.Println("Error opening the file:", err)
		return
	}
	defer file.Close()

	bytes := []byte(snippet)

	// Write content to the file
	_, err = file.Write(bytes)
	if err != nil {
		fmt.Println("Error writing to the file:", err)
		return
	}

	fmt.Print("Now running: ", script_filepath, "\r\n")

	// Arguments to be passed to the Bash script
	scriptArguments := []string{temp_filepath}

	cmd := exec.Command("bash", script_filepath)
	cmd.Args = append(cmd.Args, scriptArguments...)

	// Redirect standard output and error streams
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	// Run the command
	e := cmd.Run()
	if e != nil {
		fmt.Print("Error executing script:", e, "\r\n")
		return
	}
}

func handleInvalidCmd(text string) {
	defer printUnknown(text)
}

// Format input string for parsing
func cleanInput(text string) string {
	output := strings.TrimSpace(text)
	output = strings.ToLower(output)
	return output
}

func handleCommand(screen tcell.Screen, command string) {
	command = cleanInput(command)
	if value, exists := commands[command]; exists {
		value()
	} else if strings.EqualFold("exit", command) {
		screen.Fini()
		os.Exit(0)
	} else if strings.HasPrefix(command, "read(") {
		filePath := strings.TrimPrefix(command, "read(")
		filePath = strings.TrimSuffix(filePath, ")")
		run(readFile(filePath))
	} else if strings.HasPrefix(command, "run(") {
		snippet := strings.TrimPrefix(command, "run(")
		snippet = strings.TrimSuffix(snippet, ")")
		run(snippet)
	} else {
		// Pass the command to the parser
		handleInvalidCmd(command)
	}
}

func main() {
	screen, err := tcell.NewScreen()
	if err != nil {
		panic(err)
	}
	defer screen.Fini()

	if err := screen.Init(); err != nil {
		panic(err)
	}

	quit := make(chan struct{})

	var input string = ""

	for {
		go func() {
			printPrompt()
			for {
				event := screen.PollEvent()
				switch event := event.(type) {
				case *tcell.EventKey:
					if event.Key() == tcell.KeyEnter {
						fmt.Print("\r\n")
						handleCommand(screen, input)
						input = ""
						fmt.Print("\r")
						printPrompt()
						break
					} else if event.Key() == tcell.KeyCtrlSpace {
						// Detecting shift+enter not possible, so ctrl+space used instead
						input += "\r\n"
						fmt.Print("\r\n")
					} else if event.Key() == tcell.KeyBackspace || event.Key() == tcell.KeyBackspace2 {
						if len(input) > 0 {
							input = input[:len(input)-1]
							// Determine how to backspace based on OS
							backspace_function, result := backspace[runtime.GOOS]
							if result {
								backspace_function()
							} else {
								panic("Unsupported OS!")
							}
						}
					} else if event.Key() == tcell.KeyCtrlC {
						screen.Fini()
						os.Exit(0)
					} else if event.Key() == tcell.KeyCtrlL {
						clearScreen()
						fmt.Print(input)
					} else {
						input += string(event.Rune())
						fmt.Print(string(event.Rune()))
					}
				}
			}
		}()
		<-quit
	}
}
