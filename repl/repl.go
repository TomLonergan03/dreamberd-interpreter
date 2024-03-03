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

var clear map[string]func() //create a map for storing clear funcs

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
}

// printPrompt displays the repl prompt at the start of each loop
func printPrompt() {
	fmt.Print(cliName, "> ")
}

// printUnknown informs the user about invalid commands
func printUnknown(text string) {
	fmt.Println(text, ": command not found")
}

// displayHelp informs the user about our hardcoded functions
func displayHelp() {
	fmt.Print("Welcome to ", cliName, "! These are the available commands: \r\n")
	fmt.Print("help    - Show available commands\r\n")
	fmt.Print("clear   - Clear the terminal screen\r\n")
	fmt.Print("exit    - Closes the terminal\r\n")
	fmt.Print("read(file_path) - Read the file at file_path for DreamBerd interpretation\r\n")
	fmt.Print("run(code_snippet) - Send code_snippet to the DreamBerd interpreter\r\n")
}

func clearScreen() {
	clear_function, result := clear[runtime.GOOS]
	if result {
		clear_function()
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
	str := string(content)
	fmt.Println(str)
	return str
}

func run(snippet string) {
	// TODOLater - Implement running evaluator
	fmt.Println("Now running: ", snippet)
}

// handleInvalidCmd attempts to recover from a bad command
func handleInvalidCmd(text string) {
	defer printUnknown(text)
}

// handleCmd parses the given commands
func handleCmd(text string) {
	handleInvalidCmd(text)
}

// cleanInput preprocesses input to the db repl
func cleanInput(text string) string {
	output := strings.TrimSpace(text)
	output = strings.ToLower(output)
	return output
}

// TODOLater - Make a better function name
func handleCommand(screen tcell.Screen, command string) {
	text := cleanInput(command)
	if strings.EqualFold("help", text) {
		displayHelp()
	} else if strings.EqualFold("clear", text) {
		clearScreen()
		printPrompt()
	} else if strings.EqualFold("exit", text) {
		// Close the program on the exit command
		screen.Fini()
		os.Exit(0)
	} else if strings.HasPrefix(text, "read(") {
		filePath := strings.TrimPrefix(text, "read(")
		filePath = strings.TrimSuffix(filePath, ")")
		run(readFile(filePath))
	} else if strings.HasPrefix(text, "run(") {
		snippet := strings.TrimPrefix(text, "run(")
		snippet = strings.TrimSuffix(snippet, ")")
		run(snippet)
	} else {
		// Pass the command to the parser
		handleCmd(text)
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
						if event.Modifiers() == tcell.ModShift {
							input += "\n"
							fmt.Print("\r\n")
						} else {
							fmt.Print("\r\n")
							handleCommand(screen, input)
							input = ""
							fmt.Print("\r")
							printPrompt()
							break
						}
					} else if event.Key() == tcell.KeyBackspace || event.Key() == tcell.KeyBackspace2 {
						// TODOLater - Why does this not work?
						if len(input) > 0 {
							input = input[:len(input)-1]
							screen.Clear()

							// Set the default style
							style := tcell.StyleDefault.Foreground(tcell.ColorWhite).Background(tcell.ColorBlack)

							// Display the updated input string
							for i, char := range input {
								screen.SetContent(i+1, 1, char, nil, style)
							}
							screen.Show()
						}
					} else if event.Key() == tcell.KeyCtrlC {
						screen.Fini()
						os.Exit(0)
					} else if event.Key() == tcell.KeyCtrlL {
						clearScreen()
						printPrompt()
					} else {
						input += string(event.Rune())
						fmt.Print(string(event.Rune()))
					}
				}
			}
		}()

		<-quit

		screen.Show()
	}
}
