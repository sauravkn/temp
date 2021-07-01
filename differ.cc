#include <iostream>
#include <vector>
#include <unordered_map>
#include <filesystem>
#include <fstream>


inline std::vector<std::pair<uint32_t, uint32_t>> split_at(std::string &inp, char sep) {
  uint32_t last, curr;
  std::vector<std::pair<uint32_t, uint32_t>> res;

  last = curr = 0;
  while(curr < inp.size()) {
    if(inp[curr] == sep) {
      res.push_back({last, curr});
      last = curr+1;
    }
    curr++;
  }
  res.push_back({last, curr}); // last element won't have tailing `sep`, so need to be explictly added
  return res;
}

struct VocabCount{
  long a, cnt, vocabCount;
};

inline bool operator==(const VocabCount& lhs, const VocabCount& rhs) {
  /* if(lhs.a == rhs.a && lhs.cnt == rhs.cnt && lhs.vocabCount == rhs.vocabCount) { */
  /* while reading file only cnt is used */
  if(lhs.cnt == rhs.cnt) {
    return true;
  }
  return false;
}

std::unordered_map<std::string, VocabCount> VocabCountFileParser(const std::string& filePath) {
  std::unordered_map<std::string, VocabCount> parsed;
  std::ifstream myfile(filePath);
  if (!myfile.is_open()) {
    std::cout << "WARNING: Unable to open file " << filePath << '\n';
    return {};
  }

  std::string line;
  while (getline(myfile,line)) {
    std::vector<std::pair<uint32_t, uint32_t>> split_line = split_at(line, '|');
    std::string token = line.substr( split_line[0].first
                              , split_line[0].second-split_line[0].first
                              );
    std::string cnt1_s = line.substr( split_line[1].first
                               , split_line[1].second-split_line[1].first
                               );
    long cnt1 = stol(cnt1_s);
    std::string cnt2_s = line.substr( split_line[2].first
                               , split_line[2].second-split_line[2].first
                               );
    long cnt2 = stol(cnt2_s);
    std::string cnt3_s = line.substr( split_line[3].first
                               , split_line[3].second-split_line[3].first
                               );
    long cnt3 = stol(cnt3_s);
    parsed[token] = VocabCount{cnt1, cnt2, cnt3};
  }
  return parsed;
}

struct CoocurrenceCount {
  long docs, cnt;
};

inline bool operator==(const CoocurrenceCount& lhs, const CoocurrenceCount& rhs) {
  if(lhs.docs == rhs.docs && lhs.cnt == rhs.cnt) {
    return true;
  }
  return false;
}

std::unordered_map<std::string, CoocurrenceCount> CoocurenceCountFileParser(const std::string& filePath) {
  std::unordered_map<std::string, CoocurrenceCount> parsed;
  std::ifstream myfile(filePath);
  if (!myfile.is_open()) {
    std::cout << "WARNING: Unable to open file " << filePath << '\n';
    return {};
  }

  std::string line;
  while (getline(myfile,line)) {
    std::vector<std::pair<u_int32_t, uint32_t>> split_line = split_at(line, '|');
    std::string tokens = line.substr( split_line[0].first
                               , split_line[1].second-split_line[1].first
                               );
    std::string docs_s = line.substr( split_line[2].first
                               , split_line[2].second-split_line[2].first
                               );
    long docs = stol(docs_s);
    std::string coocurrence_s = line.substr( split_line[3].first
                                      , split_line[3].second-split_line[3].first
                                      );
    long coocurrence = stol(coocurrence_s);
    parsed[tokens] = CoocurrenceCount{docs, coocurrence};
  }
  return parsed;
}

struct DiffResult {
  long diffOldNew; // those not present in new, but were in old.
  long oldTotal;
  long newTotal;
  // to get freshly added in new, newTotal - (oldTotal- diffOldNew)
};

DiffResult compareVocabCountFiles(const std::string& vocabDiffPath, const std::string& oldFile, const std::string& newFile) {
  /* return number of entries that are common in both, & differ */
  auto old = VocabCountFileParser(oldFile);
  auto curr = VocabCountFileParser(newFile);
  int diffResults = 0;
  std::ofstream vocabDiffFile;
  vocabDiffFile.open(vocabDiffPath);

  for(auto const& [key, val] : old) {
    if(curr.find(key) == curr.end()) {
      diffResults++;
      vocabDiffFile << key <<  '\t' << val.cnt <<  '\t' << 0 << '\n';
    } else {
      vocabDiffFile << key <<  '\t' << val.cnt <<  '\t' << curr[key].cnt << '\n';
      if (!(curr[key] == val)) {
        diffResults++;
      }
    }
  }

  for(auto const& [key, val] : curr) {
    if(old.find(key) == old.end()) {
      vocabDiffFile << key <<  '\t' << 0 <<  '\t' << val.cnt << '\n';
    }
  }

  vocabDiffFile.close();
  int oldTotal = old.size();
  int newTotal = curr.size();
  return {diffResults, oldTotal, newTotal};
}

DiffResult compareCoocurrenceCountFiles(const std::string& coocDiffPath, const std::string& oldFile, const std::string& newFile) {
  /* return number of entries that are common in both, & differ */
  auto old = CoocurenceCountFileParser(oldFile);
  auto curr = CoocurenceCountFileParser(newFile);
  int diffResults = 0;
  std::ofstream coocDiffFile;
  coocDiffFile.open(coocDiffPath);

  for(auto const& [key, val] : old) {
    if(curr.find(key) == curr.end()) {
      diffResults++;
      coocDiffFile << key <<  '\t' << val.docs <<  '\t' << 0 << '\n';
    } else {
      auto currVal = curr[key];
      coocDiffFile << key <<  '\t' << val.docs <<  '\t' << currVal.docs << '\n';
      if(!(currVal == val)) {
        diffResults++;
      }
    }
  }

  for(auto const& [key, val] : curr) {
    if(old.find(key) == old.end()) {
      coocDiffFile << key << '\t' <<  0 <<  '\t' << val.docs << '\n';
    }
  }

  coocDiffFile.close();
  int oldTotal = old.size();
  int newTotal = curr.size();
  return {diffResults, oldTotal, newTotal};
}


auto main(int argc, char* argv[]) -> int {
  /* -- ⚔⚔⚔ vocab & co occurence count result differ ⚔⚔⚔ --
   * expected arguments: previous_version files, current version files
   * returns int: -1 for failure, 0 for success
   */
  /* if(argc < 7) { */
  /*   std::cout << "ERROR: Missing required arguments"; */
  /*   return -1; */
  /* } else if(argc > 7) { */
  /*   std::cout << "WARNING: Extra argument provided"; */
  /*   return -1; */
  /* } */
  auto oldVocabFilePath = argv[1];
  auto currVocabFilePath = argv[2];
  auto oldCoocurenceFilePath = argv[3];
  auto currCoocurenceFilePath = argv[4];
  auto vfp = argv[5];
  auto cfp = argv[6];

  auto resVoc = compareVocabCountFiles(vfp, oldVocabFilePath, currVocabFilePath);
  auto resCooc = compareCoocurrenceCountFiles(cfp, oldCoocurenceFilePath, currCoocurenceFilePath);

  std::cout << resVoc.diffOldNew << '\t' << resVoc.oldTotal << '\t' << resVoc.newTotal<< std::endl;
  std::cout << resCooc.diffOldNew << '\t' << resCooc.oldTotal << '\t' << resCooc.newTotal<< std::endl;

  return 0;
}

/* Requirements
 * - cpp17 compiler
 */

/* ASSUMPTION
 * - files are properly formatted, and code won't hit parsing error
 */

/* POSSIBLE IMPROVEMENT
 * - run vocab and coocurrence differ in seperatet threads.
 */

