package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strconv"
	"strings"
	"sync"
)

func main() {
	f1n := os.Args[1]
	f2n := os.Args[2]

	fmt.Println("first file: ", f1n)
	fmt.Println("second file: ", f2n)

	// f1, err := os.Open(f1n)
	// if err != nil {
	// 	log.Fatalf("failed to open file1")
	// }

	// f2, err := os.Open(f2n)
	// if err != nil {
	// 	log.Fatalf("failed to open file2")
	// }

	// var text1 []string
	// var text2 []string

	// scanner1 := bufio.NewScanner(f1)
	// scanner2 := bufio.NewScanner(f2)

	// scanner1.Split(bufio.ScanLines)
	// scanner2.Split(bufio.ScanLines)

	// for scanner1.Scan() {
	// 	text1 = append(text1, scanner1.Text())
	// }
	// for scanner2.Scan() {
	// 	text2 = append(text2, scanner2.Text())
	// }

	// fmt.Println(text1)
	// fmt.Println(text2)

	c1, err := ioutil.ReadFile(f1n)
	if err != nil {
		log.Fatalf("failed to open file1")
	}
	d1 := string(c1)
	sd1 := strings.Split(d1, "\n")
	fmt.Println("len1 : ", len(sd1))

	c2, err := ioutil.ReadFile(f2n)
	if err != nil {
		log.Fatalf("failed to open file2")
	}
	d2 := string(c2)
	sd2 := strings.Split(d2, "\n")
	fmt.Println("len2 : ", len(sd2))

	docPresentOnlyInFirst := 0
	docPresentOnlyInSecond := 0
	tokenPresentOnlyInFirst := 0
	tokenPresentOnlyInSecond := 0
	tokenPresentInBothSameFreq := 0
	tokenPresentInBothDiffFreq := 0
	i, j, ind := 0, 0, 0
	for i < len(sd1) && j < len(sd2) {
		// Doc1, f1 := lineParser(text1[i])
		// Doc2, f2 := lineParser(text2[i])
		Doc1, f1 := lineParser(sd1[i])
		Doc2, f2 := lineParser(sd2[i])

		if Doc1 > Doc2 {
			docPresentOnlyInFirst++
			j++
		} else if Doc1 < Doc2 {
			docPresentOnlyInSecond++
			i++
		} else {
			a, b, c, d := compareFreq(f1, f2)
			tokenPresentOnlyInFirst += a
			tokenPresentOnlyInSecond += b
			tokenPresentInBothSameFreq += c
			tokenPresentInBothDiffFreq += d
			i++
			j++
		}
		if ind%50000 == 0 {
			fmt.Println("currently processing ", i, Doc1, j, Doc2, ind)
		}
		ind++
	}
	fmt.Println("total documents processed ", i, j, ind)

	// f1.Close()
	// f2.Close()

	fmt.Println("docPresentOnlyInFirst: ", docPresentOnlyInFirst)
	fmt.Println("docPresentOnlyInSecond: ", docPresentOnlyInSecond)
	fmt.Println("tokenPresentOnlyInFirst: ", tokenPresentOnlyInFirst)
	fmt.Println("tokenPresentOnlyInSecond: ", tokenPresentOnlyInSecond)
	fmt.Println("tokenPresentInBothSameFreq: ", tokenPresentInBothSameFreq)
	fmt.Println("tokenPresentInBothDiffFreq: ", tokenPresentInBothDiffFreq)
}

func compareFreq(f1, f2 map[int]int) (int, int, int, int) {
	var wg sync.WaitGroup
	wg.Add(2)
	res1 := make([]int, 3)
	res2 := make([]int, 3)
	go onlyFirst(f1, f2, &wg, res1)
	go onlyFirst(f2, f1, &wg, res2)
	// a, c, d := onlyFirst(f1, f2, &wg)
	// b, _, _ := onlyFirst(f2, f1, &wg)
	wg.Wait()
	a := res1[0]
	c := res1[1]
	d := res1[2]
	b := res2[0]
	return a, b, c, d
}

func onlyFirst(f1, f2 map[int]int, wg *sync.WaitGroup, out []int) (int, int, int) {
	defer wg.Done()
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
	out[0] = a
	out[1] = c
	out[2] = d
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

