package main

import (
	"bytes"
	"io/ioutil"
	"log"
	"os"
	"path"
	"path/filepath"
	"strconv"
	"strings"
)

const USE_PACKGE_KEYWORD string = "use-package"
const LISP_RELATIVE_PATH = ".emacs.d/lisp"
const EXCLUDE_CUSTOM_NAME = "custom.el"

func main() {
	homePath := os.Getenv("HOME")
	absoluteLispDirPath := path.Join(homePath, LISP_RELATIVE_PATH)
	files, err := ioutil.ReadDir(absoluteLispDirPath)
	if err != nil {
		log.Fatal(err)
	}
	var buffer bytes.Buffer
	buffer.WriteString("package: comment \n")
	var index = 1
	for _, file := range files {
		if file.Name() == EXCLUDE_CUSTOM_NAME {
			continue
		}
		absoluteLispFilePath := filepath.Join(absoluteLispDirPath, file.Name())
		data, err := ioutil.ReadFile(absoluteLispFilePath)
		if err != nil {
			log.Fatal(err)
		}
		context := string(data)
		buffer.WriteString(file.Name() + "\n")
		temp := strings.Split(context, "\n")
		for lineNum, content := range temp {
			if strings.Contains(content, USE_PACKGE_KEYWORD) {
				content = strings.Replace(content, "("+USE_PACKGE_KEYWORD, "", -1)
				comment := strings.Replace(temp[lineNum-1], ";", "", -1)
				buffer.WriteString(strconv.Itoa(index))
				buffer.WriteString(". ")
				buffer.WriteString(strings.TrimSpace(content))
				buffer.WriteString(": ")
				if len(strings.TrimSpace(comment)) > 0 {
					buffer.WriteString(strings.TrimSpace(comment))
				} else {
					buffer.WriteString("None")
				}
				buffer.WriteString("\n")
				index += 1
			}
		}
	}
	buffer.WriteString("total package: " + strconv.Itoa(index))
	writeInfoToFile("package-info.org", buffer.Bytes())
}
func writeInfoToFile(filename string, info []byte) {
	err := ioutil.WriteFile(filename, info, 0644)
	if err != nil {
		log.Fatal(err)
	}

}
