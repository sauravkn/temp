package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"strconv"
	"strings"
)

func main() {
	f1n := os.Args[1]
	f2n := os.Args[2]

	fmt.Println("first file: ", f1n)
	fmt.Println("second file: ", f2n)

	f1, err := os.Open(f1n)
	if err != nil {
		log.Fatalf("failed to open file1")
	}

	f2, err := os.Open(f2n)
	if err != nil {
		log.Fatalf("failed to open file2")
	}
	defer f1.Close()
	defer f2.Close()

	var line1 string
	var line2 string

	scanner1 := bufio.NewReader(f1)
	scanner2 := bufio.NewReader(f2)

	// c1, err := ioutil.ReadFile(f1n)
	// if err != nil {
	// 	log.Fatalf("failed to open file1")
	// }
	// d1 := string(c1)
	// sd1 := strings.Split(d1, "\n")
	// fmt.Println("len1 : ", len(sd1))

	// c2, err := ioutil.ReadFile(f2n)
	// if err != nil {
	// 	log.Fatalf("failed to open file2")
	// }
	// d2 := string(c2)
	// sd2 := strings.Split(d2, "\n")
	// fmt.Println("len2 : ", len(sd2))

	docPresentOnlyInFirst := 0
	docPresentOnlyInSecond := 0
	tokenPresentOnlyInFirst := 0
	tokenPresentOnlyInSecond := 0
	tokenPresentInBothSameFreq := 0
	tokenPresentInBothDiffFreq := 0
	i, j, ind := 0, 0, 0
	inc1, inc2 := true, true
	for {
		if inc1 {
			line1, err = scanner1.ReadString('\n')
			if line1 == "" || (err != nil && err != io.EOF) {
				break
			}
			// As the line contains newline "\n" character at the end, we could remove it.
			line1 = line1[:len(line1)-1]
		}

		if inc2 {
			line2, err = scanner2.ReadString('\n')
			if line2 == "" || (err != nil && err != io.EOF) {
				break
			}
			// As the line contains newline "\n" character at the end, we could remove it.
			line2 = line2[:len(line2)-1]
		}

		Doc1, f1 := lineParser(line1)
		Doc2, f2 := lineParser(line2)

		if Doc1 > Doc2 {
			docPresentOnlyInFirst++
			j++
			inc1 = false
		} else if Doc1 < Doc2 {
			docPresentOnlyInSecond++
			i++
			inc2 = false
		} else {
			a, b, c, d := compareFreq(f1, f2)
			tokenPresentOnlyInFirst += a
			tokenPresentOnlyInSecond += b
			tokenPresentInBothSameFreq += c
			tokenPresentInBothDiffFreq += d
			i++
			j++
			inc1, inc2 = true, true
		}
		if ind%50000 == 0 {
			fmt.Println("currently processing ", i, Doc1, j, Doc2, ind)
		}
		ind++
	}
	fmt.Println("total documents processed ", i, j, ind)

	fmt.Println("docPresentOnlyInFirst: ", docPresentOnlyInFirst)
	fmt.Println("docPresentOnlyInSecond: ", docPresentOnlyInSecond)
	fmt.Println("tokenPresentOnlyInFirst: ", tokenPresentOnlyInFirst)
	fmt.Println("tokenPresentOnlyInSecond: ", tokenPresentOnlyInSecond)
	fmt.Println("tokenPresentInBothSameFreq: ", tokenPresentInBothSameFreq)
	fmt.Println("tokenPresentInBothDiffFreq: ", tokenPresentInBothDiffFreq)
}

func compareFreq(f1, f2 map[int]int) (int, int, int, int) {
	a, c, d := onlyFirst(f1, f2)
	b, _, _ := onlyFirst(f2, f1)
	return a, b, c, d
}

func onlyFirst(f1, f2 map[int]int) (int, int, int) {
	a, d, c := 0, 0, 0
	for k1, v1 := range f1 {
		if v2, ok := f2[k1]; !ok {
			a++
		} else {
			if v1 == v2 {
				c++
			} else {
				d++
			}
		}
	}
	return a, c, d
}

func SplitOnNonLetters(s string) []string {
	return strings.Fields(s)
}

func lineParser(line string) (int, map[int]int) {
	parts := SplitOnNonLetters(line)
	if len(parts) <= 0 {
		tmp := make(map[int]int)
		return 0, tmp
	}
	docId, err := strconv.Atoi(parts[0][6:])
	if err != nil {
		log.Fatalf("failed to parse dociId %v", docId)
	}
	// unigramCnt, _ := strconv.Atoi(parts[1])
	val := parts[2:]
	count := parseCommaSep(val)
	return docId, count
}

func parseCommaSep(inp []string) map[int]int {
	tmp := make(map[int]int)

	for _, pair := range inp {
		keyVal := strings.Split(pair, ",")
		key, err := strconv.Atoi(keyVal[0])
		if err != nil {
			log.Fatalf("failed to parse key %v", key)
		}
		val, err := strconv.Atoi(keyVal[1])
		if err != nil {
			log.Fatalf("failed to parse value %v", val)
		}
		tmp[key] = val
	}

	return tmp
}

